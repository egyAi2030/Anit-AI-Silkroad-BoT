using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace SilkroadAIBot.Core.Helpers
{
    public static class ClientLauncher
    {
        private static void KillExistingClients()
        {
            var processes = Process.GetProcessesByName("sro_client");
            if (processes.Length > 0)
            {
                foreach (var process in processes)
                {
                    try
                    {
                        process.Kill();
                    }
                    catch (Exception ex)
                    {
                        LogService.Error($"Failed to kill sro_client process (ID: {process.Id})", ex);
                    }
                }
                System.Threading.Thread.Sleep(500);
                LogService.Info("[Loader] Killed existing sro_client processes for a clean start.");
            }
        }

        public static bool Launch(string clientFolderPath, int locale = 22)
        {
            return LaunchRedirected(clientFolderPath, null, null, locale);
        }

        private static string ExtractEmbeddedDll()
        {
            try
            {
                var assembly = System.Reflection.Assembly.GetExecutingAssembly();
                var resourceName = System.Linq.Enumerable.FirstOrDefault(assembly.GetManifestResourceNames(), n => n.EndsWith("xBotLoader.dll"));
                
                if (resourceName == null)
                    throw new Exception("xBotLoader.dll not found in embedded resources.");

                string tempPath = Path.Combine(Path.GetTempPath(), "xBotLoader_temp.dll");
                
                using (Stream stream = assembly.GetManifestResourceStream(resourceName))
                using (FileStream fileStream = new FileStream(tempPath, FileMode.Create, FileAccess.Write))
                {
                    stream.CopyTo(fileStream);
                }
                return tempPath;
            }
            catch (Exception ex)
            {
                LogService.Error("[Loader] Failed to extract embedded xBotLoader.dll", ex);
                return null;
            }
        }

        public static bool LaunchRedirected(string clientFolderPath, string? originalIp, string? redirectIp, int locale = 22)
        {
            string clientPath = Path.Combine(clientFolderPath, "sro_client.exe");

            if (!File.Exists(clientPath))
            {
                LogService.Error($"SRO_Client.exe not found at: {clientPath}");
                return false;
            }

            KillExistingClients();

            if (string.IsNullOrEmpty(originalIp) || string.IsNullOrEmpty(redirectIp))
            {
                try
                {
                    ProcessStartInfo startInfo = new ProcessStartInfo
                    {
                        FileName = clientPath,
                        WorkingDirectory = clientFolderPath,
                        Arguments = $"/{locale} 0 0",
                        UseShellExecute = true,
                        Verb = "runas" 
                    };
                    LogService.Info($"Launching client (Standard): {clientPath}");
                    Process.Start(startInfo);
                    return true;
                }
                catch (Exception ex)
                {
                    LogService.Error("Failed to launch Silkroad client.", ex);
                    return false;
                }
            }
            else
            {
                // 1. Extract the Embedded DLL
                string dllPath = ExtractEmbeddedDll();
                if (dllPath == null) return false;

                // 2. Generate Config for the DLL
                string cfgPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "xBot");
                Directory.CreateDirectory(cfgPath);
                string iniFile = Path.Combine(cfgPath, "xBotLoader.ini");

                StringBuilder cfg = new StringBuilder();
                cfg.AppendLine("[Patches]");
                cfg.AppendLine("Multiclient=yes");
                cfg.AppendLine("Zoom_Hack=yes");
                cfg.AppendLine("Swear_Filter=yes");
                cfg.AppendLine("Redirect_Gateway=yes");
                cfg.AppendLine("Redirect_Agent=no");
                cfg.AppendLine("Gateway_Ip=127.0.0.1");
                cfg.AppendLine($"Gateway_Port={SilkroadAIBot.Core.Configuration.ConfigManager.Config.ProxyPort}");
                cfg.AppendLine("Agent_Ip=127.0.0.1");
                cfg.AppendLine("Agent_Port=0");
                File.WriteAllText(iniFile, cfg.ToString());

                // 3. Launch Suspended
                SmartLoader.STARTUPINFO si = new SmartLoader.STARTUPINFO();
                si.cb = Marshal.SizeOf(si);
                SmartLoader.PROCESS_INFORMATION pi = new SmartLoader.PROCESS_INFORMATION();

                string workDir = Path.GetDirectoryName(clientPath);
                string cmdLine = $"\"{clientPath}\" 0 /{locale} 0 0";

                LogService.Info("[Loader] Launching client for Native API Hooking (Embedded DLL)...");
                bool success = SmartLoader.CreateProcess(clientPath, cmdLine, IntPtr.Zero, IntPtr.Zero, false, 0x00000004, IntPtr.Zero, workDir, ref si, out pi);

                if (!success) return false;

                // 4. Inject DLL
                IntPtr loadLibraryAddr = Injector.GetProcAddress(Injector.GetModuleHandle("kernel32.dll"), "LoadLibraryA");
                IntPtr allocMemAddress = Injector.VirtualAllocEx(pi.hProcess, IntPtr.Zero, (uint)((dllPath.Length + 1)), 0x00001000 | 0x00002000, 0x04);

                byte[] bytes = Encoding.Default.GetBytes(dllPath);
                Injector.WriteProcessMemory(pi.hProcess, allocMemAddress, bytes, (uint)bytes.Length, out _);
                Injector.CreateRemoteThread(pi.hProcess, IntPtr.Zero, 0, loadLibraryAddr, allocMemAddress, 0, IntPtr.Zero);

                // 5. Resume and Cleanup
                System.Threading.Thread.Sleep(200); 
                SmartLoader.ResumeThread(pi.hThread);

                SmartLoader.CloseHandle(pi.hThread);
                SmartLoader.CloseHandle(pi.hProcess);

                // Optional: Delete temp DLL after a few seconds
                Task.Delay(3000).ContinueWith(_ => { try { File.Delete(dllPath); } catch { } });

                LogService.Info("[Loader] Success! Client natively hooked and redirected to Proxy.");
                return true;
            }
        }
    }
}
