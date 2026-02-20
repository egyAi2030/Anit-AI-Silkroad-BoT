using System;
using System.Net.Sockets;
using System.Threading.Tasks;
using System.Collections.Generic;
using SecurityAPI;

namespace SilkroadAIBot.Networking
{
    public class ClientlessConnection : IDisposable
    {
        private TcpClient _client = null!;
        private NetworkStream _stream = null!;
        private Security _security = null!;
        private Queue<Packet> _receivedPackets = null!;
        private bool _running;
        
        public event Action<Packet>? OnPacketReceived;
        public event Action? OnDisconnected;
        
        public bool IsConnected => _client != null && _client.Connected;

        public async Task ConnectAsync(string host, int port)
        {
            _client = new TcpClient();
            await _client.ConnectAsync(host, port);
            _stream = _client.GetStream();
            
            _security = new Security();
            // Initialize security for Client Mode (Server will send 0x5000)
            // Do NOT call GenerateSecurity() as that is for Server/Gateway role. 
            
            _receivedPackets = new Queue<Packet>();
            _running = true;
            
            Console.WriteLine($"[Connection] Connected to {host}:{port}");
        }

        public NetworkStream GetStream() { return _stream; }

        public async Task PerformHandshakeAsync()
        {
            // Wait for handshake to complete
            int timeout = 5000;
            while (!_security.HasHandshake && timeout > 0)
            {
                await Task.Delay(100);
                timeout -= 100;
            }

            if (!_security.HasHandshake)
                throw new TimeoutException("Handshake timed out.");

            Console.WriteLine("[Connection] Handshake Successful."); 
        }

        public async Task ProcessAsync()
        {
            byte[] buffer = new byte[8192];
            
            while (_running && IsConnected)
            {
                try
                {
                    // Read from network
                    if (_stream.DataAvailable)
                    {
                        int bytesRead = await _stream.ReadAsync(buffer, 0, buffer.Length);
                        
                        if (bytesRead > 0)
                        {
                            // Feed to security
                            _security.Recv(buffer, 0, bytesRead);
                            
                            // Get decrypted packets
                            var packets = _security.TransferIncoming();
                            if (packets != null)
                            {
                                foreach (var packet in packets)
                                {
                                    _receivedPackets.Enqueue(packet);
                                    OnPacketReceived?.Invoke(packet);
                                }
                            }
                        }
                        else
                        {
                            // 0 bytes read usually means disconnect
                            HandleDisconnect();
                            break;
                        }
                    }
                    
                    // Write to network
                    var outgoing = _security.TransferOutgoing();
                    if (outgoing != null)
                    {
                        foreach (var kvp in outgoing)
                        {
                            var transferBuffer = kvp.Key;
                            await _stream.WriteAsync(
                                transferBuffer.Buffer,
                                transferBuffer.Offset,
                                transferBuffer.Size
                            );
                        }
                    }
                    
                    await Task.Delay(10);
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"[Connection] Error: {ex.Message}");
                    HandleDisconnect();
                }
            }
        }
        
        public void SendPacket(Packet packet)
        {
            if (_security != null)
                _security.Send(packet);
        }

        public Packet? GetNextPacket()
        {
            return _receivedPackets.Count > 0 ? _receivedPackets.Dequeue() : null;
        }
        
        private void HandleDisconnect()
        {
            if (_running)
            {
                _running = false;
                Console.WriteLine("[Connection] Disconnected.");
                OnDisconnected?.Invoke();
            }
        }
        
        public void Disconnect()
        {
            HandleDisconnect();
            Dispose();
        }

        public void Dispose()
        {
            _running = false;
            _stream?.Dispose();
            _client?.Dispose();
        }
    }
}

