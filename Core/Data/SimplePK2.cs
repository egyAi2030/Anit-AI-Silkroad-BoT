using System;
using System.IO;

namespace SilkroadAIBot.Core.Data
{
    public class SimplePK2 : IDisposable
    {
        private string _path;
        private FileStream? _stream;

        public SimplePK2(string path)
        {
            _path = path;
            if (File.Exists(_path))
            {
                _stream = new FileStream(_path, FileMode.Open, FileAccess.Read, FileShare.Read);
            }
        }

        public bool FileExists(string fileName)
        {
            // Lightweight existence check for now
            return File.Exists(_path);
        }

        public byte[]? GetFile(string path)
        {
            // Stub: implement parsing later
            return null;
        }

        public System.Collections.Generic.IEnumerable<string> GetFileNames()
        {
            // Stub
            return Array.Empty<string>();
        }

        public void Close()
        {
            _stream?.Close();
        }

        public void Dispose()
        {
            Close();
            _stream?.Dispose();
        }
    }
}
