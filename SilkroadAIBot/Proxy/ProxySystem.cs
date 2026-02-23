using System;
using System.Net.Sockets;
using System.Threading.Tasks;
using SilkroadAIBot.Networking;
using SilkroadAIBot.Bot;
using SecurityAPI;
using System.Net;
using System.Collections.Generic;



#region ProxyContext
namespace SilkroadAIBot.Proxy
{
    public class ProxyContext
    {
        private TcpClient _gameClient;
        private ClientlessConnection _serverConnection; // Bot -> Server
        
        // Security for Client Connection (Bot acts as Server)
        private Security _clientSecurity;
        private WorldState _worldState;
        private PacketHandler? _packetHandler;
        private SilkroadAIBot.Data.DataManager? _dataManager;
        private static ProxyManager? _agentProxy;
        
        public ProxyContext(TcpClient gameClient, WorldState worldState, ClientlessConnection serverConnection)
        {
            _gameClient = gameClient;
            _worldState = worldState;
            _serverConnection = serverConnection;
            _clientSecurity = new Security();
            
            // Generate security with server behavior
            // We need to manually trigger the initial 0x5000 from the "Server" (Bot)
            Security.SecurityFlags flags = new Security.SecurityFlags();
            flags.blowfish = 1;
            flags.security_bytes = 1;
            flags.handshake = 1;
            flags.handshake_response = 0;
            _clientSecurity.GenerateSecurity(flags); // This should queue 0x5000 
        }
        
        public void Initialize(SilkroadAIBot.Data.DataManager dataManager)
        {
            _dataManager = dataManager;
            _packetHandler = new PacketHandler(_worldState, dataManager);
        }

        public async Task StartProxySessionAsync()
        {
             Console.WriteLine("[Proxy] Client joined. Starting Relay Session...");

             // 3. Handle Client Connection (Bot is Server)
             // SecurityAPI Generated 0x5000 in Constructor. Send it to Client.
             // We need to loop TransferOutgoing for the client Stream
             await RelayLoop(_gameClient.GetStream(), _serverConnection);
        }
        
        private async Task RelayLoop(NetworkStream clientStream, ClientlessConnection server)
        {
            var serverStream = server.GetStream(); // Need to expose stream or use provided methods
            
            // Bidirectional Loop
            var clientToServer = RelayClientToServer(clientStream, server);
            var serverToClient = RelayServerToClient(server, clientStream);
            
            await Task.WhenAny(clientToServer, serverToClient);
            Console.WriteLine("[Proxy] Session Ended.");
        }

        private async Task RelayClientToServer(NetworkStream clientStream, ClientlessConnection server)
        {
            try 
            {
                byte[] buffer = new byte[8192];
                
                // Flush initial handshake packet (0x5000) immediately
                while (_clientSecurity.HasPacketToSend())
                {
                     var (outBuffer, pkt) = _clientSecurity.GetPacketToSend();
                     await clientStream.WriteAsync(outBuffer.GetBuffer(), 0, outBuffer.Size);
                }

                while (true)
                {
                    int bytesRead = await clientStream.ReadAsync(buffer, 0, buffer.Length);
                    if (bytesRead == 0) break;
                    
                    
                    // Client -> [Decrypt] -> Bot -> [Encrypt] -> Server
                    _clientSecurity.Recv(buffer, 0, bytesRead);
                    var packets = _clientSecurity.TransferIncoming();
                    
                    if (packets != null)
                    {
                        foreach(var packet in packets)
                        {
                            // Consumes packets from the queue populated by ProcessAsync
                            // TRIGGER LOGGER FIRST!
                            SilkroadAIBot.Core.Helpers.PacketLogger.OnPacketLogged?.Invoke("C->S", packet, true);

                            bool isHandshake = packet.Opcode == 0x5000 || packet.Opcode == 0x5001 || packet.Opcode == 0x9000;

                            if (isHandshake) 
                            {
                                SilkroadAIBot.Core.Helpers.LogService.Debug($"[Proxy] Swallowed Security Opcode {packet.Opcode:X4} (C->S)");
                                continue; // Do NOT forward handshake packets
                            }
                            
                            // Forward to Server (Bot Re-encrypts automatically via ClientlessConnection)
                            server.SendPacket(packet);
                        }
                    }
                    
                    // Also send any handshake responses back to client if generated by TransferIncoming
                    // (e.g. 0x9000 Handshake OK)
                     while (_clientSecurity.HasPacketToSend())
                    {
                        var (outBuffer, pkt) = _clientSecurity.GetPacketToSend();
                        await clientStream.WriteAsync(outBuffer.GetBuffer(), 0, outBuffer.Size);
                    }
                }
            }
            catch (Exception ex) { Console.WriteLine($"[Proxy] C->S Error: {ex.Message}"); }
        }

        private async Task RelayServerToClient(ClientlessConnection server, NetworkStream clientStream)
        {
             try 
            {
                // Start the server packet processor in the background
                // This handles reading from socket -> decrypt -> queue
                _ = server.ProcessAsync(); 
                
                while (true)
                {
                    // Consumes packets from the queue populated by ProcessAsync
                    var packet = server.GetNextPacket();
                    
                    if (packet != null)
                    {
                        // TRIGGER LOGGER FIRST!
                        SilkroadAIBot.Core.Helpers.PacketLogger.OnPacketLogged?.Invoke("S->C", packet, false);
                        
                        bool isHandshake = packet.Opcode == 0x5000 || packet.Opcode == 0x5001 || packet.Opcode == 0x9000;

                        if (isHandshake) 
                        {
                            SilkroadAIBot.Core.Helpers.LogService.Debug($"[Proxy] Swallowed Security Opcode {packet.Opcode:X4} (S->C)");
                            continue; // Do NOT forward handshake packets
                        }

                        // MITM: Inspect/Modify
                        if (packet.Opcode == 0xA102)
                        {
                            packet.Lock();
                            // Create a copy to read safely
                            Packet reader = new Packet(packet.Opcode, packet.Encrypted, packet.Massive, packet.GetBytes());
                            reader.Lock();
                            
                            byte result = reader.ReadByte();
                            if (result == 0x01) // Login Success!
                            {
                                uint sessionId = reader.ReadUInt();
                                string realAgentIp = reader.ReadAscii();
                                ushort realAgentPort = reader.ReadUShort();
                                
                                int localAgentPort = 15778; // Our new Agent Proxy port
                                
                                SilkroadAIBot.Core.Helpers.LogService.Info($"[Gateway] Login Success! Intercepted real Agent Server: {realAgentIp}:{realAgentPort}");
                                SilkroadAIBot.Core.Helpers.LogService.Info($"[Gateway] Spawning local Agent Proxy on 127.0.0.1:{localAgentPort}");
                                
                                // Spawn the Agent Proxy on a new background task
                                Task.Run(() => {
                                    if (_agentProxy != null) _agentProxy.Stop();
                                    _agentProxy = new ProxyManager(realAgentIp, realAgentPort, _worldState, localAgentPort);
                                    if (_dataManager != null) _agentProxy.SetDataManager(_dataManager);
                                    _ = _agentProxy.StartAsync();
                                });
                                
                                // Build spoofed packet for the client
                                Packet spoofedPacket = new Packet(0xA102);
                                spoofedPacket.WriteByte(0x01);
                                spoofedPacket.WriteUInt(sessionId);
                                spoofedPacket.WriteAscii("127.0.0.1"); // Force client to connect back to Bot
                                spoofedPacket.WriteUShort((ushort)localAgentPort);
                                
                                // Send spoofed packet to client
                                _clientSecurity.Send(spoofedPacket);
                                SilkroadAIBot.Core.Helpers.LogService.Info("[Gateway] Spoofed 0xA102 sent to client. Waiting for Agent connection...");
                                continue; // IMPORTANT: Skip forwarding the original packet
                            }
                        }
                        
                        // Update Bot State
                        _packetHandler?.HandlePacket(packet);

                        // Re-encrypt for Client
                        _clientSecurity.Send(packet); 
                    }
                    else
                    {
                        await Task.Delay(1); // Prevent CPU spin if queue empty
                    }
                    
                    // Send pending packets to Client (from _clientSecurity outgoing queue)
                    while (_clientSecurity.HasPacketToSend())
                    {
                         var (outBuffer, pkt) = _clientSecurity.GetPacketToSend();
                         await clientStream.WriteAsync(outBuffer.GetBuffer(), 0, outBuffer.Size);
                    }
                }
            }
            catch (Exception ex) { Console.WriteLine($"[Proxy] S->C Error: {ex.Message}"); }
        }
    }
}
#endregion


#region ProxyHandshake
namespace SilkroadAIBot.Proxy
{
    public static class ProxyHandshake
    {
        // Helper to manually construct 0x5000 if SecurityAPI fails to do so for server mode
        // Currently relying on Security.GenerateSecurity()
    }
}
#endregion


#region ProxyListener
namespace SilkroadAIBot.Proxy
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
            _listener.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true);
            _listener.Start();
            SilkroadAIBot.Core.Helpers.LogService.Info($"[Proxy] Listening for local client on 127.0.0.1:{_localPort}...");
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
                    System.Diagnostics.Debug.WriteLine($"[Proxy] DEBUG: Client HIT the bridge from {client.Client.RemoteEndPoint}");
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
#endregion


#region ProxyManager
namespace SilkroadAIBot.Proxy
{
    public class ProxyManager
    {
        private ProxyListener _listener;
        private List<ProxyContext> _sessions;
        private string _targetGatewayIP;
        private int _targetGatewayPort;
        private SilkroadAIBot.Data.DataManager _dataManager = null!;
        private SilkroadAIBot.Bot.WorldState _worldState;

        public ProxyManager(string targetIP, int targetPort, SilkroadAIBot.Bot.WorldState worldState, int localPort = 15778)
        {
            _targetGatewayIP = targetIP;
            _targetGatewayPort = targetPort;
            _worldState = worldState;
            _sessions = new List<ProxyContext>();
            _listener = new ProxyListener(localPort); // Listen on configurable Proxy Port
            _listener.OnClientConnected += Listener_OnClientConnected;
        }

        public void SetDataManager(SilkroadAIBot.Data.DataManager dataManager)
        {
            _dataManager = dataManager;
            // Apply to existing sessions if any
            foreach (var session in _sessions)
            {
                session.Initialize(dataManager);
            }
        }

        public async Task<bool> StartAsync()
        {
            try
            {
                // ONLY start listening locally. Do not connect to real server yet!
                Console.WriteLine($"[ProxyManager] Listening on local port. Awaiting clients...");
                _listener.Start();
                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[ProxyManager] Failed to start listener: {ex.Message}");
                SilkroadAIBot.Core.Helpers.LogService.Error($"[ProxyManager] Failed to start listener: {ex.Message}");
                return false;
            }
        }

        public void Stop()
        {
            _listener.Stop();
        }

        private void Listener_OnClientConnected(object? sender, System.Net.Sockets.TcpClient client)
        {
            SilkroadAIBot.Core.Helpers.LogService.Info("[Proxy] A client has just knocked on the door! (Connected to Proxy)");
            Console.WriteLine("[ProxyManager] New Client Connection. Connecting to Real Server ON DEMAND...");
            
            // Connect to Real Server ON DEMAND ONLY AFTER the local client is accepted
            Task.Run(async () => 
            {
                try 
                {
                    SilkroadAIBot.Networking.ClientlessConnection serverConn = new SilkroadAIBot.Networking.ClientlessConnection();
                    await serverConn.ConnectAsync(_targetGatewayIP, _targetGatewayPort);
                    SilkroadAIBot.Core.Helpers.LogService.Info("[Proxy] Connected to Real Server. Bridging the connection...");
                    
                    // Pass BOTH the local client and the newly connected real server client to ProxyContext
                    var context = new ProxyContext(client, _worldState, serverConn);
                    if (_dataManager != null) context.Initialize(_dataManager);
                    _sessions.Add(context);
                    
                    // Start the proxy session loop using the existing connections
                    _ = context.StartProxySessionAsync();
                }
                catch (Exception ex)
                {
                    SilkroadAIBot.Core.Helpers.LogService.Error($"[ProxyManager] Failed to connect to real server strictly on-demand: {ex.Message}");
                }
            });
        }
    }
}
#endregion

