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

        public static bool LaunchRedirected(string clientFolderPath, string? originalIp, string? redirectIp, int locale = 22)
        {
            string clientPath = Path.Combine(clientFolderPath, "sro_client.exe");
            if (!File.Exists(clientPath))
            {
                LogService.Error($"SRO_Client.exe not found at: {clientPath}");
                return false;
            }
            
            KillExistingClients();

            // Ensure we only pass the IP string without port to the memory patcher
            string hostOnly = redirectIp.Contains(":") ? redirectIp.Split(':')[0] : redirectIp;
            LogService.Info($"[Loader] Using Native Memory Patching (SmartLoader) to redirect to {hostOnly}...");

            // SmartLoader now handles the suspended launch and asynchronous memory scanning
            string actualOriginalIp = string.IsNullOrEmpty(originalIp) ? "192.168.100.9" : originalIp;
            bool success = SmartLoader.LaunchAndPatch(clientPath, actualOriginalIp, hostOnly, locale);

            if (!success)
            {
                LogService.Error("[Loader] Failed to launch client natively via SmartLoader.");
            }

            return success;
        }
    }
}
