using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace SilkroadAIBot.Core.Proxy
{
    public class ProxyManager
    {
        private ProxyListener _listener;
        private List<ProxyContext> _sessions;
        private string _targetGatewayIP;
        private int _targetGatewayPort;
        private SilkroadAIBot.Core.Data.DataManager _dataManager = null!;
        private SilkroadAIBot.Bot.WorldState _worldState;

        public ProxyManager(string targetIP, int targetPort, SilkroadAIBot.Bot.WorldState worldState)
        {
            _targetGatewayIP = targetIP;
            _targetGatewayPort = targetPort;
            _worldState = worldState;
            _sessions = new List<ProxyContext>();
            _listener = new ProxyListener(15779); // Listen on Gateway Port
            _listener.OnClientConnected += Listener_OnClientConnected;
        }

        public void SetDataManager(SilkroadAIBot.Core.Data.DataManager dataManager)
        {
            _dataManager = dataManager;
            // Apply to existing sessions if any
            foreach (var session in _sessions)
            {
                session.Initialize(dataManager);
            }
        }

        public void Start()
        {
            _listener.Start();
        }

        public void Stop()
        {
            _listener.Stop();
        }

        private async void Listener_OnClientConnected(object? sender, System.Net.Sockets.TcpClient client)
        {
            Console.WriteLine("[ProxyManager] New Client Connection.");
            var context = new ProxyContext(client, _worldState);
            if (_dataManager != null) context.Initialize(_dataManager);
            _sessions.Add(context);
            
            try
            {
                await context.StartProxyAsync(_targetGatewayIP, _targetGatewayPort);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[ProxyManager] Session Error: {ex.Message}");
                _sessions.Remove(context);
            }
        }
    }
}
