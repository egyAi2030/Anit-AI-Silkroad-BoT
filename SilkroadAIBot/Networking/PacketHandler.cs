using System;
using SilkroadAIBot.Bot;
using SilkroadAIBot.Models;
using SecurityAPI;

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



