using System;
using System.IO;
using System.Runtime.CompilerServices;

namespace SilkroadAIBot.Core.Helpers
{
    public enum LogLevel
    {
        Info,
        Warning,
        Error,
        Debug
    }

    public static class LogService
    {
        private static readonly string LogFilePath = "bot_debug.log";
        private static readonly object _lock = new object();
        public static Action<string>? OnLog;

        static LogService()
        {
            try
            {
                // Clear old log on startup for fresh debugging
                File.WriteAllText(LogFilePath, $"--- Bot Debug Log Started at {DateTime.Now} ---\n");
            }
            catch { }
        }

        public static void Info(string message) => Log(LogLevel.Info, message);
        public static void Warning(string message) => Log(LogLevel.Warning, message);
        public static void Error(string message, Exception? ex = null) 
        {
            string fullMessage = message;
            if (ex != null)
            {
                fullMessage += $" | Exception: {ex.Message}\nStack Trace: {ex.StackTrace}";
            }
            Log(LogLevel.Error, fullMessage);
        }
        public static void Debug(string message) => Log(LogLevel.Debug, message);

        private static void Log(LogLevel level, string message)
        {
            string timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");
            string formattedMessage = $"[{timestamp}] [{level.ToString().ToUpper()}] {message}";

            lock (_lock)
            {
                try
                {
                    // Write to file
                    File.AppendAllText(LogFilePath, formattedMessage + Environment.NewLine);
                }
                catch { }

                // Write to Console/Debug
                System.Diagnostics.Debug.WriteLine(formattedMessage);
                Console.WriteLine(formattedMessage);

                // Notify UI
                OnLog?.Invoke(formattedMessage);
            }
        }
    }
}
