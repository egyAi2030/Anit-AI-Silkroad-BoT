using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;

namespace SilkroadAIBot.Core.Proxy
{
    public class ProxyListener
    {
        private TcpListener _listener;
        private bool _isRunning;
        private int _localPort;

        public event EventHandler<TcpClient> OnClientConnected;

        public ProxyListener(int port)
        {
            _localPort = port;
            _listener = new TcpListener(IPAddress.Any, _localPort);
        }

        public void Start()
        {
            if (_isRunning) return;
            _isRunning = true;
            _listener.Start();
            Console.WriteLine($"[Proxy] Listening on port {_localPort}...");
            _ = AcceptLoop();
        }

        public void Stop()
        {
            _isRunning = false;
            _listener.Stop();
        }

        private async Task AcceptLoop()
        {
            while (_isRunning)
            {
                try
                {
                    var client = await _listener.AcceptTcpClientAsync();
                    Console.WriteLine($"[Proxy] Client connected from {client.Client.RemoteEndPoint}");
                    OnClientConnected?.Invoke(this, client);
                }
                catch (Exception ex)
                {
                    if (_isRunning) Console.WriteLine($"[Proxy] Accept Error: {ex.Message}");
                }
            }
        }
    }
}
