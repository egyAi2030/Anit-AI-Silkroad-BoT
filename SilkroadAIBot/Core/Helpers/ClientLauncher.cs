using System;
using System.Diagnostics;
using System.IO;

namespace SilkroadAIBot.Core.Helpers
{
    public static class ClientLauncher
    {
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
                // Use DLL Injection for API Hooking
                LogService.Info($"Launching client via API Hook DLL Injection");
                string dllPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "Redirector.dll");

                // Note: since the user builds the DLL, we'll try to find it in the AppDomain or absolute path if missing.
                // Assuming it's copied to output directory
                return Injector.LaunchAndInject(clientPath, dllPath, locale);
            }
        }
    }
}
