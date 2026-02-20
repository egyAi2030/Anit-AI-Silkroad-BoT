using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using SecurityAPI;
using SilkroadAIBot.Bot;
using SilkroadAIBot.Models;
using SilkroadAIBot.Core.Helpers;

namespace SilkroadAIBot.Networking
{
    public class LoginManager
    {
        private ClientlessConnection _connection;
        public ClientlessConnection Connection => _connection;
        
        private bool _isLoginSuccess;
        private string _username = null!;
        private string _password = null!; // Store for reuse if needed
        
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
            int timeout = 15000; 
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
                        LogService.Info("[Login] Connected to Gateway. Sending Login Request (0x6102)...");
                        SendLoginRequest();
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
            }
        }

        private void SendLoginRequest()
        {
            Packet packet = new Packet(Opcode.CLIENT_LOGIN_REQUEST, true);
            packet.WriteByte(22); // Locale (VSRO)
            packet.WriteAscii(_username);
            packet.WriteAscii(_password);
            packet.WriteUShort(64); // Server ID (Usually ignored in vSRO)
            _connection.SendPacket(packet);
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


