using System;
using System.Net.Sockets;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Networking;
using SilkroadAIBot.Bot;
using SecurityAPI;

namespace SilkroadAIBot.Core.Proxy
{
    public class ProxyContext
    {
        private TcpClient _gameClient;
        private ClientlessConnection _serverConnection; // Bot -> Server
        
        // Security for Client Connection (Bot acts as Server)
        private Security _clientSecurity;
        private WorldState _worldState;
        private PacketHandler? _packetHandler;
        
        public ProxyContext(TcpClient gameClient, WorldState worldState)
        {
            _gameClient = gameClient;
            _worldState = worldState;
            _clientSecurity = new Security();
            
            // Security for Client Connection (Bot acts as Server)
            _clientSecurity = new Security();
            
            // Generate security with server behavior
            // We need to manually trigger the initial 0x5000 from the "Server" (Bot)
            // But SecurityAPI might need adjustment to start as Server.
            // For xBot's SecurityAPI: 
            // GenerateSecurity(flags) creates the keys and adds the 0x5000 handshake packet to m_outgoing_packets.
            // GenerateSecurity(flags) creates the keys and adds the 0x5000 handshake packet to m_outgoing_packets.
            Security.SecurityFlags flags = new Security.SecurityFlags();
            flags.blowfish = 1;
            flags.security_bytes = 1;
            flags.handshake = 1;
            flags.handshake_response = 0;
            _clientSecurity.GenerateSecurity(flags); // This should queue 0x5000 
             // Initialize PacketHandler
             // We need DataManager now. Assuming ProxyContext has access or we create a dummy for now if missing.
             // Actually, ProxyContext is created in MainForm or similar.
             // We should pass DataManager to ProxyContext.
             // For now, let's assume valid instance or null if not ready.
             // _packetHandler = new PacketHandler(_worldState, null); // Placeholder until we wire it up
        }
        
        public void Initialize(SilkroadAIBot.Core.Data.DataManager dataManager)
        {
            _packetHandler = new PacketHandler(_worldState, dataManager);
        }

        public async Task StartProxyAsync(string targetIP, int targetPort)
        {
             Console.WriteLine($"[Proxy] Connecting to Real Server {targetIP}:{targetPort}...");
             
             // 1. Connect to Real Server
             _serverConnection = new ClientlessConnection();
             await _serverConnection.ConnectAsync(targetIP, targetPort);
             
             // 2. Perform Handshake with Real Server (Bot is Client)
             await _serverConnection.PerformHandshakeAsync();
             Console.WriteLine("[Proxy] Connected to Server.");

             // 3. Handle Client Connection (Bot is Server)
             // SecurityAPI Generated 0x5000 in Constructor. Send it to Client.
             // We need to loop TransferOutgoing for the client Stream
             _ = Task.Run(() => RelayLoop(_gameClient.GetStream(), _serverConnection));
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
                            // MITM: Inspect/Modify Packet Here
                            Console.WriteLine($"[C->S] {packet.Opcode:X4}");
                            
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
                        Console.WriteLine($"[S->C] {packet.Opcode:X4}");
                        
                        // MITM: Inspect/Modify
                        if (packet.Opcode == 0xA103) { /* Auth Response Hook */ }
                        
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
