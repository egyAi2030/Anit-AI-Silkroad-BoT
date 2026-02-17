using System;
using System.Diagnostics;
using System.IO;

namespace SilkroadAIBot.Core.Helpers
{
    public static class ClientLauncher
    {
        public static bool Launch(string clientFolderPath, int locale = 22)
        {
            string clientPath = Path.Combine(clientFolderPath, "sro_client.exe");

            if (!File.Exists(clientPath))
            {
                LogService.Error($"SRO_Client.exe not found at: {clientPath}");
                return false;
            }

            try
            {
                ProcessStartInfo startInfo = new ProcessStartInfo
                {
                    FileName = clientPath,
                    WorkingDirectory = clientFolderPath,
                    Arguments = $"/{locale} 0 0",
                    UseShellExecute = true,
                    Verb = "runas" // Request administrator privileges
                };

                LogService.Info($"Launching client (Admin): {clientPath} with args: {startInfo.Arguments}");
                Process.Start(startInfo);
                return true;
            }
            catch (Exception ex)
            {
                LogService.Error("Failed to launch Silkroad client.", ex);
                return false;
            }
        }
    }
}
