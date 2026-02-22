using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using SecurityAPI;
using SilkroadAIBot.Bot;
using SilkroadAIBot.Models;
using SilkroadAIBot.Core.Helpers;
using System.Net.Sockets;



#region Login System
namespace SilkroadAIBot.Networking
{
    public class LoginManager
    {
        private ClientlessConnection _connection;
        public ClientlessConnection Connection => _connection;
        
        private bool _isLoginSuccess;
        private string _username = null!;
        private string _password = null!; // Store for reuse if needed
        private ushort _targetServerId = 0;
        
        private WorldState _worldState;
        private PacketHandler _packetHandler;

        public LoginManager(WorldState worldState, SilkroadAIBot.Data.DataManager dataManager)
        {
            _worldState = worldState;
            _packetHandler = new PacketHandler(_worldState, dataManager);
            _connection = new ClientlessConnection();
            _connection.OnPacketReceived += HandlePacket;
        }

        public async Task<bool> LoginAsync(string gatewayIp, int port, string username, string password)
        {
            _username = username;
            _password = password;
            _isLoginSuccess = false;

            LogService.Info($"[Login] Connecting to Gateway {gatewayIp}:{port}...");
            await _connection.ConnectAsync(gatewayIp, port);

            // Startup packet processing
             _ = _connection.ProcessAsync();

            // Wait for full login cycle (Gateway -> Redirect -> Agent Ready)
            int timeout = 30000;  // Increased to 30 seconds
            int delay = 100;
            while (timeout > 0)
            {
                if (_isLoginSuccess) return true;
                await Task.Delay(delay);
                timeout -= delay;
            }

            LogService.Warning("[Login] Timeout waiting for authentication/redirection.");
            return false;
        }

        private void HandlePacket(Packet packet)
        {
            switch (packet.Opcode)
            {
                case Opcode.GLOBAL_IDENTIFICATION: // 0x2001
                    string service = packet.ReadAscii();
                    if (service == "GatewayServer")
                    {
                        LogService.Info("[Login] Connected to Gateway. Sending Version Check (0x6100)...");
                        SendVersionCheck();
                    }
                    else
                    {
                        LogService.Info($"[Login] Connected to unknown service: {service}");
                    }
                    break;

                case Opcode.SERVER_CAPTCHA_CHALLENGE: // 0x2322
                    LogService.Info("[Login] Captcha Challenge received. Sending Empty Response (0x6103)...");
                    SendCaptchaResponse();
                    break;

                case Opcode.SERVER_LOGIN_REDIRECT: // 0xA102
                    ParseLoginRedirect(packet);
                    break;

                case Opcode.SERVER_LOGIN_ERROR: // 0xA103
                    byte errorCode = packet.ReadByte();
                    LogService.Warning($"[Login] Authentication Failed. Error Code: {errorCode}");
                    break;
                    
                case 0xA100: // Version Check Response
                    byte versionResult = packet.ReadByte();
                    if (versionResult == 1)
                    {
                        LogService.Info("[Login] Version Accepted! Requesting Server List (0x6101)...");
                        _connection.SendPacket(new Packet(0x6101, true)); // Request Server List
                    }
                    else if (versionResult == 2)
                    {
                        LogService.Warning("[Login] Version Mismatch!");
                    }
                    break;
                    
                case 0xA101: // Server List Response
                    ParseServerList(packet);
                    break;
            }
        }

        private void SendVersionCheck()
        {
            Packet packet = new Packet(0x6100, true);
            packet.WriteByte(22); // Locale (VSRO)
            packet.WriteAscii("SR_Client"); // CRITICAL: Missing client identifier added here
            packet.WriteUInt(188); // Version as uint
            _connection.SendPacket(packet);
        }

        private void SendLoginRequest()
        {
            LogService.Info($"[Login] Attempting login for user: {_username}");
            Packet packet = new Packet(Opcode.CLIENT_LOGIN_REQUEST, true);
            packet.WriteByte(22); // Locale (VSRO)
            packet.WriteAscii(_username);
            packet.WriteAscii(_password);
            packet.WriteUShort(_targetServerId); // Dynamic Server ID
            _connection.SendPacket(packet);
        }

        private void ParseServerList(Packet packet)
        {
            try
            {
                while (packet.ReadByte() == 1) // Divisions
                {
                    packet.ReadAscii(); 
                    packet.ReadByte(); 
                }
                while (packet.ReadByte() == 1) // Servers
                {
                    ushort serverID = packet.ReadUShort();
                    string serverName = packet.ReadAscii();
                    ushort currentUsers = packet.ReadUShort();
                    ushort maxUsers = packet.ReadUShort();
                    byte status = packet.ReadByte();
                    
                    LogService.Info($"[Login] Discovered Server: {serverName} (ID: {serverID}) - Users: {currentUsers}/{maxUsers}");
                    if (_targetServerId == 0) _targetServerId = serverID; // Save the first valid ID
                }
                
                LogService.Info("[Login] Server List parsed. Sending Login Request (0x6102)...");
                SendLoginRequest();
            }
            catch (Exception ex)
            {
                LogService.Error($"[Login] Error parsing server list: {ex.Message}");
            }
        }

        private void SendCaptchaResponse()
        {
            // Empty string response as per requirement
            Packet packet = new Packet(Opcode.CLIENT_CAPTCHA_RESPONSE, true);
            packet.WriteAscii(""); 
            _connection.SendPacket(packet);
            
            // Hex Reference: 03 61 00 00
        }

        private async void ParseLoginRedirect(Packet packet)
        {
            byte result = packet.ReadByte();
            if (result == 1)
            {
                uint sessionID = packet.ReadUInt();
                string agentIP = packet.ReadAscii();
                ushort agentPort = packet.ReadUShort();

                LogService.Info($"[Login] Success! Redirecting to Agent -> {agentIP}:{agentPort} (SessionID: {sessionID})");
                
                await ConnectToAgentServer(agentIP, agentPort, sessionID);
            }
            else
            {
                byte errorCode = packet.ReadByte();
                LogService.Warning($"[Login] Redirect failed. Error: {errorCode}");
            }
        }

        public async Task ConnectToAgentServer(string ip, int port, uint sessionID)
        {
            LogService.Info($"[Login] Connecting to Agent Server: {ip}:{port}");
            _connection.Disconnect();
            
            _connection = new ClientlessConnection();
            _connection.OnPacketReceived += (p) => HandleAgentPacket(p, sessionID);
            await _connection.ConnectAsync(ip, port);
            _ = _connection.ProcessAsync();
        }

        private void HandleAgentPacket(Packet packet, uint sessionID)
        {
             // Delegate to PacketHandler for entity packets
             if (packet.Opcode == Opcode.SERVER_CHARACTER_DATA || 
                 packet.Opcode == Opcode.SERVER_ENTITY_SPAWN || 
                 packet.Opcode == Opcode.SERVER_ENTITY_GROUP_SPAWN || 
                 packet.Opcode == Opcode.SERVER_ENTITY_DESPAWN ||
                 packet.Opcode == Opcode.SERVER_ENTITY_UPDATE_STATUS)
             {
                 if (packet.Opcode == Opcode.SERVER_CHARACTER_DATA) _packetHandler.AppendCharacterData(packet);
                 else if (packet.Opcode == Opcode.SERVER_ENTITY_SPAWN || packet.Opcode == Opcode.SERVER_ENTITY_GROUP_SPAWN) _packetHandler.ParseSpawn(packet);
                 else if (packet.Opcode == Opcode.SERVER_ENTITY_DESPAWN) _packetHandler.ParseDespawn(packet);
                 else if (packet.Opcode == Opcode.SERVER_ENTITY_UPDATE_STATUS) _packetHandler.ParseUpdateStatus(packet);
                 return;
             }

             switch (packet.Opcode)
             {
                case Opcode.GLOBAL_IDENTIFICATION: // 0x2001
                    LogService.Info("[Agent] Connected. Sending Session Handover (0x6103)...");
                    SendAgentAuth(sessionID);
                    break;

                case Opcode.SERVER_CHARACTER_SELECTION_ACTION_RESPONSE: // 0xB007 (Char List)
                    LogService.Info("[Agent] Character List Received. Parsing...");
                    ParseCharacterList(packet);
                    break;

                case Opcode.SERVER_CHARACTER_DATA_BEGIN: // 0x34A5
                    LogService.Info("[Agent] Receiving Character Data (Chunked)...");
                    _packetHandler.ResetCharacterDataBuffer();
                    break;

                case Opcode.SERVER_CHARACTER_DATA_END: // 0x34A6
                    LogService.Info("[Agent] Character Data Complete. Finalizing Assembly...");
                    _packetHandler.FinalizeCharacterData();
                    LogService.Info("[Agent] Confirming Spawn (0x3012)...");
                    ConfirmSpawn();
                    _isLoginSuccess = true;
                    break;
             }
        }

        private void SendAgentAuth(uint sessionID)
        {
            Packet packet = new Packet(Opcode.CLIENT_AGENT_AUTH, true);
            packet.WriteUInt(sessionID);
            packet.WriteAscii(_username);
            packet.WriteAscii(_password);
            _connection.SendPacket(packet);
        }

        public void SelectCharacter(string charName)
        {
             LogService.Info($"[Agent] Selecting Character: {charName}");
             Packet packet = new Packet(Opcode.CLIENT_CHARACTER_SELECTION_JOIN_REQUEST, true);
             packet.WriteAscii(charName);
             _connection.SendPacket(packet);
        }
        
        public void ConfirmSpawn()
        {
            Packet packet = new Packet(Opcode.CLIENT_CHARACTER_CONFIRM_SPAWN);
            _connection.SendPacket(packet);
            LogService.Info("[Agent] Spawn Confirmed. Bot active in world.");
        }
        
        private void ParseCharacterList(Packet packet)
        {
            try
            {
                byte action = packet.ReadByte(); // 2 = List
                if (action != 2) return;
                byte success = packet.ReadByte(); // 1 = Success
                if (success != 1) return;

                byte charCount = packet.ReadByte();
                LogService.Info($"[Agent] Found {charCount} characters.");

                string? targetChar = null;
                string? firstChar = null;

                // Load config for auto-select
                var configChar = SilkroadAIBot.Core.Configuration.ConfigManager.Config.AutoCharName;

                for (int i = 0; i < charCount; i++)
                {
                    packet.ReadUInt(); // Model Ref
                    string name = packet.ReadAscii();
                    
                    if (firstChar == null) firstChar = name;
                    if (!string.IsNullOrEmpty(configChar) && name.Equals(configChar, StringComparison.OrdinalIgnoreCase))
                    {
                        targetChar = name;
                    }

                    // Skip remaining character data to read next
                    // Standard vSRO structure
                    packet.ReadByte(); // Scale
                    packet.ReadByte(); // Level
                    packet.ReadULong(); // Exp
                    packet.ReadUShort(); // STR
                    packet.ReadUShort(); // INT
                    packet.ReadUShort(); // StatPoints
                    packet.ReadUInt(); // HP
                    packet.ReadUInt(); // MP
                    
                    bool isDeleting = packet.ReadByte() == 1;
                    if (isDeleting) packet.ReadUInt(); // DeleteTime
                    
                    packet.ReadByte(); // GuildClass
                    packet.ReadByte(); // GuildRename
                    packet.ReadByte(); // Academy

                    byte itemCount = packet.ReadByte();
                    for (int j = 0; j < itemCount; j++)
                    {
                        packet.ReadUInt(); // RefID
                        packet.ReadByte(); // Plus
                    }

                    byte avatarCount = packet.ReadByte();
                    for (int j = 0; j < avatarCount; j++)
                    {
                        packet.ReadUInt(); // RefID
                        packet.ReadByte(); // Plus
                    }
                }

                if (targetChar != null)
                {
                    LogService.Info($"[Agent] Auto-Selecting configured character: {targetChar}");
                    SelectCharacter(targetChar);
                }
                else if (firstChar != null)
                {
                    LogService.Info($"[Agent] Configured character not found. Selecting first available: {firstChar}");
                    SelectCharacter(firstChar);
                }
                else
                {
                    LogService.Warning("[Agent] No characters found on this account.");
                }
            }
            catch (Exception ex)
            {
                LogService.Error($"[Agent] Error parsing character list: {ex.Message}");
            }
        }

        public void Disconnect()
        {
            _connection.Dispose();
        }
    }
}
#endregion


#region Clientless Connection
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
                            SilkroadAIBot.Core.Helpers.LogService.Info($"[Server -> Bot RAW] {BitConverter.ToString(buffer, 0, bytesRead)}");

                            try
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
                            catch (Exception secEx)
                            {
                                SilkroadAIBot.Core.Helpers.LogService.Error($"[Security] Error processing packet: {secEx.Message}", secEx);
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
#endregion


#region Packet Handlers
namespace SilkroadAIBot.Networking
{
    public class PacketHandler
    {
        private WorldState _worldState;
        private SilkroadAIBot.Data.DataManager _dataManager;
        private List<byte> _characterDataBuffer = new List<byte>();

        public PacketHandler(WorldState worldState, SilkroadAIBot.Data.DataManager dataManager)
        {
            _worldState = worldState;
            _dataManager = dataManager;
        }

        public void ResetCharacterDataBuffer()
        {
            _characterDataBuffer.Clear();
            Console.WriteLine("[PacketHandler] Character Data Buffer Reset.");
        }

        public static event Action<Packet>? OnPacketReceived;

        public void HandlePacket(Packet packet)
        {
            OnPacketReceived?.Invoke(packet); // Notify UI
            switch (packet.Opcode)
            {
                case Opcode.SERVER_CHARACTER_DATA:
                    ParseCharacterData(packet);
                    break;
                case Opcode.SERVER_ENTITY_SPAWN:
                case Opcode.SERVER_ENTITY_GROUP_SPAWN:
                    ParseSpawn(packet);
                    break;
                case Opcode.SERVER_ENTITY_DESPAWN:
                    ParseDespawn(packet);
                    break;
                case Opcode.SERVER_ENTITY_UPDATE_STATUS:
                    ParseUpdateStatus(packet);
                    break;
                case 0x30D2: // Movement
                    ParseMovement(packet);
                    break;
            }
        }

        public void AppendCharacterData(Packet packet)
        {
            byte[] data = packet.ReadByteArray(packet.RemainingRead());
            _characterDataBuffer.AddRange(data);
            Console.WriteLine($"[PacketHandler] Appended {data.Length} bytes to Character Data Buffer. Total: {_characterDataBuffer.Count}");
        }

        public void FinalizeCharacterData()
        {
            if (_characterDataBuffer.Count == 0)
            {
                Console.WriteLine("[PacketHandler] Warning: FinalizeCharacterData called with empty buffer.");
                return;
            }

            // Create a fake packet for parsing to reuse existing logic
            // 0x3013 is used here as a placeholder for the assembled data
            Packet assembledPacket = new Packet(Opcode.SERVER_CHARACTER_DATA, false, false, _characterDataBuffer.ToArray());
            assembledPacket.Lock();
            
            ParseCharacterData(assembledPacket);
            _characterDataBuffer.Clear();
        }

        // 0x3013
        public void ParseCharacterData(Packet packet)
        {
            try
            {
                // In standard 1.188, 0x3013 is chunked. 
                // The structure for the DATA PART (what we receive in the handler) is:
                
                // Note: We assume the packet is already the assembled data if we handle it this way,
                // or we are handling individual chunks. 
                // In our current ClientlessConnection, we don't have a chunk assembler yet.
                // However, RSBot's logic shows 0x3013 is often just the character info.
                
                uint serverTime = packet.ReadUInt();
                uint modelID = packet.ReadUInt();
                byte scale = packet.ReadByte();
                byte level = packet.ReadByte();
                byte maxLevel = packet.ReadByte();
                ulong exp = packet.ReadULong();
                uint spExp = packet.ReadUInt();
                ulong gold = packet.ReadULong();
                uint sp = packet.ReadUInt();
                ushort statPoints = packet.ReadUShort();
                byte zerkPoints = packet.ReadByte();
                uint expChunk = packet.ReadUInt(); // Experience chunk
                uint hp = packet.ReadUInt();
                uint mp = packet.ReadUInt();
                byte autoInvestExp = packet.ReadByte();
                byte dailyPk = packet.ReadByte();
                ushort totalPk = packet.ReadUShort();
                uint pkPenalty = packet.ReadUInt();
                byte hwanLevel = packet.ReadByte();
                byte freePvp = packet.ReadByte();

                if (_worldState.Character == null) _worldState.Character = new SRCharacter();
                
                var c = _worldState.Character;
                c.ModelID = modelID;
                c.Level = level;
                c.Exp = exp;
                c.Gold = gold;
                c.SP = sp;
                c.HP = hp;
                c.HPMax = hp; // Needs max update from bionic details later
                c.MP = mp;
                c.MPMax = mp;

                // Inventory
                byte invSize = packet.ReadByte();
                byte itemCount = packet.ReadByte();
                c.Inventory.Clear();
                for (int i = 0; i < itemCount; i++)
                {
                    var item = ParseItem(packet);
                    if (item != null) c.Inventory.Add(item);
                }

                // Avatar Inventory
                byte avatarInvSize = packet.ReadByte();
                byte avatarItemCount = packet.ReadByte();
                for (int i = 0; i < avatarItemCount; i++)
                {
                    ParseItem(packet); // Skip for now
                }

                // Job Inventory (v1.188+)
                packet.ReadByte(); // Size
                byte jobItemCount = packet.ReadByte();
                for (int i = 0; i < jobItemCount; i++)
                {
                    ParseItem(packet); // Skip
                }

                // Skills
                ParseKnownSkills(packet);

                Console.WriteLine($"[PacketHandler] CharacterData parsed. Level: {level}, Gold: {gold}, Items: {c.Inventory.Count}");
                _worldState.TriggerCharacterUpdate();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PacketHandler] Error parsing CharacterData: {ex.Message}");
            }
        }

        private SRItem ParseItem(Packet packet)
        {
            byte slot = packet.ReadByte();
            // Rentable check (Skip 4 bytes if rentable) - Simplified
            // packet.ReadUInt(); 
            
            uint modelID = packet.ReadUInt();
            var itemInfo = _dataManager.GetModelInfo(modelID);

            var srItem = new SRItem { ModelID = modelID, Slot = slot, TID = modelID };
            
            if (itemInfo != null)
            {
                srItem.Name = itemInfo.Value.Name;
                // Complex attribute/durability parsing based on item type
                // RSBot's InventoryItem.FromPacket is the reference.
                // For now, we skip the variable parts to keep stream aligned.
                
                if (itemInfo.Value.TypeID2 == 3) // Item
                {
                    if (itemInfo.Value.TypeID3 == 1) // Equipment
                    {
                        packet.ReadByte(); // Plus
                        packet.ReadULong(); // Stats
                        packet.ReadUInt(); // Durability
                        byte magicCount = packet.ReadByte();
                        for (int i = 0; i < magicCount; i++)
                        {
                            packet.ReadUInt(); packet.ReadUInt();
                        }
                    }
                    else if (itemInfo.Value.TypeID3 == 3) // Consumable/ETC
                    {
                        srItem.Amount = packet.ReadUShort();
                    }
                }
            }

            return srItem;
        }

        private void ParseKnownSkills(Packet packet)
        {
            try
            {
                packet.ReadByte(); // Mastery section indicator or separator
                while (packet.ReadByte() == 1) // Masteries loop
                {
                    packet.ReadUInt(); // Mastery ID
                    packet.ReadByte(); // Level
                }

                packet.ReadByte(); // Skill section indicator
                _worldState.Character.KnownSkills.Clear();
                while (packet.ReadByte() == 1) // Skills loop
                {
                    uint skillID = packet.ReadUInt();
                    bool enabled = packet.ReadByte() == 1;
                    _worldState.Character.KnownSkills[skillID] = enabled;
                    
                    var skill = _dataManager.GetSkill(skillID);
                    if (skill != null)
                        Console.WriteLine($"[PacketHandler] Known Skill: {skill.Name} (Enabled: {enabled})");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PacketHandler] Error parsing skills: {ex.Message}");
            }
        }

        // 0x3015 (Single) / 0x3019 (Group)
        public void ParseSpawn(Packet packet)
        {
             try
             {
                 if (packet.Opcode == Opcode.SERVER_ENTITY_GROUP_SPAWN)
                 {
                     // Group Spawn (0x3019)
                     byte count = packet.ReadByte();
                     for (int i = 0; i < count; i++)
                     {
                         ParseSingleSpawn(packet);
                     }
                 }
                 else
                 {
                     // Single Spawn (0x3015)
                     ParseSingleSpawn(packet);
                 }
             }
             catch (Exception ex)
             {
                 Console.WriteLine($"[PacketHandler] Error parsing Spawn: {ex.Message}");
             }
        }
        
        private void ParseSingleSpawn(Packet packet)
        {
            uint refObjID = packet.ReadUInt();
            
            if (refObjID == uint.MaxValue)
            {
                // Skip Spell Area / etc
                packet.ReadUInt(); packet.ReadUInt();
                packet.ReadUShort(); packet.ReadUShort(); packet.ReadUShort(); packet.ReadUShort();
                return;
            }

            var info = _dataManager.GetModelInfo(refObjID);
            if (info == null)
            {
                Console.WriteLine($"[PacketHandler] Unknown RefObjID: {refObjID}. Cannot parse spawn.");
                return;
            }

            var modelInfo = info.Value;

            if (modelInfo.TypeID1 == 1) // Bionic
            {
                if (modelInfo.TypeID2 == 1) // Player
                {
                    ParsePlayerSpawn(packet, refObjID);
                }
                else if (modelInfo.TypeID2 == 2)
                {
                    if (modelInfo.TypeID3 == 1) // Monster
                        ParseMonsterSpawn(packet, refObjID);
                    else
                        ParseNpcSpawn(packet, refObjID);
                }
            }
            else if (modelInfo.TypeID1 == 3) // Item
            {
                ParseItemSpawn(packet, refObjID);
            }
            else if (modelInfo.TypeID1 == 4) // Portal
            {
                ParsePortalSpawn(packet, refObjID);
            }
        }

        private void ParsePlayerSpawn(Packet packet, uint refObjID)
        {
            // Simplified Player Spawn Parser
            byte scale = packet.ReadByte();
            packet.ReadByte(); // HwanLevel?
            packet.ReadByte(); // PvpCape?
            packet.ReadByte(); // AutoInverstExp?
            
            // Skip Inventory (Complex)
            byte invSize = packet.ReadByte();
            byte itemCount = packet.ReadByte();
            for (int i = 0; i < itemCount; i++)
            {
                packet.ReadUInt(); // ItemID
                // packet.ReadByte(); // Only if equippable and we know it...
            }
            // ... (Avatar/Mask skips etc)
            
            // For now, we likely can't parse players without breaking the stream 
            // if we don't have full Item stats to know sizes.
            // I'll leave a warning/log.
            Console.WriteLine($"[PacketHandler] Player Spawn: {refObjID} (Parsing might be incomplete)");
        }

        private void ParseMonsterSpawn(Packet packet, uint refObjID)
        {
            var mob = new SRMob { ModelID = refObjID };
            ParseBionicDetails(packet, mob);
            
            // Skip Talk
            packet.ReadByte(); // Talk size
            
            // Rarity
            mob.MobType = (SRMob.MobTypeEnum)packet.ReadByte();
            
            _worldState.SpawnEntity(mob);
            Console.WriteLine($"[PacketHandler] Monster Spawn: {refObjID} (UID: {mob.UniqueID}, Type: {mob.MobType})");
        }

        private void ParseNpcSpawn(Packet packet, uint refObjID)
        {
            var npc = new SRNpc { ModelID = refObjID };
            ParseBionicDetails(packet, npc);
            _worldState.SpawnEntity(npc);
            Console.WriteLine($"[PacketHandler] NPC Spawn: {refObjID} (UID: {npc.UniqueID})");
        }

        private void ParseItemSpawn(Packet packet, uint refObjID)
        {
            try
            {
                uint uniqueID = packet.ReadUInt();
                
                ushort region = packet.ReadUShort();
                float x = packet.ReadFloat();
                float z = packet.ReadFloat(); // N/S
                float y = packet.ReadFloat(); // Height
                ushort angle = packet.ReadUShort();
                
                byte hasOwner = packet.ReadByte();
                if (hasOwner == 1) packet.ReadUInt(); // Owner ID
                
                byte rarity = packet.ReadByte(); // 0,1,2...

                // If refObjID indicates gold? 
                // We'll trust DataManager for name, etc.
                
                var item = new SRGroundItem
                {
                    UniqueID = uniqueID,
                    ModelID = refObjID,
                    Position = new SRCoord(region, x, z, y),
                    // Amount defaults to 1 unless parsing specific Gold structure
                };
                
                // Fetch info to set name
                var info = _dataManager.GetModelInfo(refObjID);
                if (info != null) item.Name = info.Value.Name;

                _worldState.AddEntity(item);
                Console.WriteLine($"[PacketHandler] Item Spawn: {item.Name} ({uniqueID})");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PacketHandler] Error parsing Item Spawn: {ex.Message}");
            }
        }

        private void ParsePortalSpawn(Packet packet, uint refObjID)
        {
             uint uniqueID = packet.ReadUInt();
             Console.WriteLine($"[PacketHandler] Portal Spawn: {refObjID} (UID: {uniqueID})");
        }

        public void ParseDespawn(Packet packet)
        {
            try
            {
                uint uniqueID = packet.ReadUInt();
                _worldState.DespawnEntity(uniqueID);
                Console.WriteLine($"[PacketHandler] Despawn: {uniqueID}");
            }
            catch (Exception ex)
            {
                 Console.WriteLine($"[PacketHandler] Error parsing Despawn: {ex.Message}");
            }
        }

        public void ParseUpdateStatus(Packet packet)
        {
            try
            {
                uint uniqueID = packet.ReadUInt();
                var entity = _worldState.GetEntity(uniqueID) as SRModel;
                if (entity == null) return;

                byte statusType = packet.ReadByte();
                if (statusType == 1) // HP Update
                {
                    uint currentHP = packet.ReadUInt();
                    entity.HP = currentHP;
                    
                    if (uniqueID == _worldState.Character.UniqueID)
                        _worldState.TriggerCharacterUpdate();
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PacketHandler] Error parsing UpdateStatus: {ex.Message}");
            }
        }

        public void ParseMovement(Packet packet)
        {
            try
            {
                uint uniqueID = packet.ReadUInt();
                var entity = _worldState.GetEntity(uniqueID) as SRModel;
                
                if (entity != null)
                {
                    // Movement Packet (0x3020)
                    byte type = packet.ReadByte(); // 1=Moving, 0=Stopped?
                    ushort region = packet.ReadUShort();
                    if (region < 128) { /* Dungeon? */ }
                    
                    float x = packet.ReadFloat();
                    float z = packet.ReadFloat(); // N/S
                    float y = packet.ReadFloat(); // Height
                    
                    // SRCoord expects (Region, X, Y, Z) where Y is North/South and Z is Height
                    entity.Position = new SRCoord(region, x, z, y);
                    
                    if (packet.Opcode == 0xB021) // Client Movement ACK
                    {
                        // Angle usually here
                        entity.Angle = packet.ReadUShort();
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[PacketHandler] Error parsing Movement: {ex.Message}");
            }
        }

        private void ParseBionicDetails(Packet packet, SREntity entity)
        {
            if (entity is SRModel model)
            {
                model.UniqueID = packet.ReadUInt();
                
                // Position
                ushort region = packet.ReadUShort();
                float x = packet.ReadFloat();
                float z = packet.ReadFloat(); // N/S
                float y = packet.ReadFloat(); // Height
                ushort angle = packet.ReadUShort();
                
                model.Position = new SRCoord(region, x, z, y);
                model.Angle = angle;
    
                // Movement
                byte hasDest = packet.ReadByte();
                if (hasDest == 1)
                {
                    ushort dRegion = packet.ReadUShort(); // Dest Region
                    float dX = packet.ReadFloat(); 
                    float dZ = packet.ReadFloat(); 
                    float dY = packet.ReadFloat(); // Dest X, Z, Y
                    
                    // Can store destination in model if needed
                    model.MovementPosition = new SRCoord(dRegion, dX, dZ, dY);
                }
                
                // LifeState
                byte lifeState = packet.ReadByte();
                model.LifeStateType = (SRModel.LifeState)lifeState;
                
                // MotionState
                packet.ReadByte(); // Unknown
                byte motionState = packet.ReadByte();
                model.MotionStateType = (SRModel.MotionState)motionState;
                
                // Speed
                packet.ReadFloat(); // Walk Speed
                packet.ReadFloat(); // Run Speed
                packet.ReadFloat(); // Berserk Speed
                
                // Buffs
                byte buffCount = packet.ReadByte();
                for (int i = 0; i < buffCount; i++)
                {
                    packet.ReadUInt(); // Buff ID
                    packet.ReadUInt(); // Duration?
                }
                
                // Name (if applicable, though typically sent separately or implied by ID)
                // For now, minimal parsing.
            }
        }

    }
}
#endregion

