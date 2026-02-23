using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using SilkroadAIBot.Bot.Bundles;
using SilkroadAIBot.Networking;
using SecurityAPI;
using SilkroadAIBot.Models;
using System.Linq;
using SilkroadAIBot.Bot;
using System.Collections.Concurrent;
using SilkroadAIBot.Bot.Navigation;
using System.Numerics;



#region BotController
namespace SilkroadAIBot.Bot
{
    public class BotController
    {
        private WorldState _worldState;
        private ClientlessConnection _connection;
        private SilkroadAIBot.Data.DatabaseManager _db;
        private List<IBundle> _bundles;
        private bool _isRunning;
        private DateTime _lastSaveTime;

        public BotController(WorldState worldState, ClientlessConnection connection, SilkroadAIBot.Data.DatabaseManager db)
        {
            _worldState = worldState;
            _connection = connection;
            _db = db;
            _bundles = new List<IBundle>();
            _lastSaveTime = DateTime.Now;
        }

        public void AddBundle(IBundle bundle)
        {
            _bundles.Add(bundle);
            // Console.WriteLine($"[Bot] Added Bundle: {bundle.Name}");
        }

        public void Start()
        {
            if (_isRunning) return;
            
            _isRunning = true;
            Console.WriteLine("[Bot] Starting Bot Controller...");
            
            foreach (var bundle in _bundles)
            {
                bundle.Start();
            }

            // Start main loop in background
            _ = BotLoop();
        }

        public void Stop()
        {
            _isRunning = false;
            foreach (var bundle in _bundles)
            {
                bundle.Stop();
            }
            Console.WriteLine("[Bot] Bot Controller Stopped.");
        }

        private async Task BotLoop()
        {
            while (_isRunning)
            {
                try
                {
                    // Execute bundles sequentially for thread safety on WorldState if needed
                    // (Though WorldState is concurrent, logic order matters)
                    
                    foreach (var bundle in _bundles)
                    {
                        await bundle.UpdateAsync(_worldState);
                    }

                    // Auto-Save every 5 minutes
                    if ((DateTime.Now - _lastSaveTime).TotalMinutes >= 5)
                    {
                        await Task.Run(() => _db.UpdateCharacterState(_worldState.Character));
                        _lastSaveTime = DateTime.Now;
                        Console.WriteLine("[Bot] Auto-Save completed.");
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"[Bot] Error in BotLoop: {ex.Message}");
                }

                await Task.Delay(100); // 10 ticks per second
            }
        }
    }
}
#endregion


#region SkillController
namespace SilkroadAIBot.Bot
{
    public class SkillController
    {
        private readonly ClientlessConnection _connection;
        private readonly Dictionary<uint, DateTime> _cooldowns = new Dictionary<uint, DateTime>();

        public SkillController(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public bool CastSkill(SRSkill skill, uint targetId)
        {
            if (skill == null) return false;
            
            if (IsOnCooldown(skill.ID))
            {
                // Console.WriteLine($"[SkillController] Skill {skill.Name} is on cooldown.");
                return false;
            }

            // Construct 0x7074 packet
            var packet = new Packet(0x7074);
            packet.WriteByte(1); // Execute
            packet.WriteByte(4); // Use Skill
            packet.WriteUInt(skill.ID);
            packet.WriteByte(1); // TargetType: Entity
            packet.WriteUInt(targetId);

            _connection.SendPacket(packet);
            
            // Set cooldown
            _cooldowns[skill.ID] = DateTime.Now.AddMilliseconds(skill.Cooldown);
            
            Console.WriteLine($"[SkillController] Casting {skill.Name} on {targetId}");
            return true;
        }

        public bool IsOnCooldown(uint skillId)
        {
            if (_cooldowns.TryGetValue(skillId, out DateTime endTime))
            {
                return DateTime.Now < endTime;
            }
            return false;
        }
    }
}
#endregion


#region TargetingSystem
namespace SilkroadAIBot.Bot
{
    public class TargetingSystem
    {
        private readonly WorldState _worldState;
        private uint _currentTargetId;
        
        public TargetPriority Priority { get; set; } = TargetPriority.Closest;

        public TargetingSystem(WorldState worldState)
        {
            _worldState = worldState;
        }

        public uint? GetNextTarget()
        {
            var charPos = _worldState.Character?.Position;
            if (charPos == null) return null;

            // Filter for Mobs that are alive and not currently targeted by others (optional)
            var targets = _worldState.GetEntities<SRMob>()
                .Where(m => m.HP > 0 && _worldState.TrainingArea.IsInRange(m.Position)) 
                .Select(m => new 
                { 
                    Mob = m, 
                    Distance = charPos.Value.DistanceTo(m.Position) 
                })
                .OrderBy(x => x.Distance)
                .ToList();

            if (targets.Count > 0)
            {
                _currentTargetId = targets[0].Mob.UniqueID;
                return _currentTargetId;
            }

            return null;
        }

        public enum TargetPriority
        {
            Closest,
            LowestHP,
            HighestHP
        }
    }
}
#endregion


#region TrainingArea
namespace SilkroadAIBot.Bot
{
    public class TrainingArea
    {
        public SRCoord Center { get; set; }
        public int Radius { get; set; }
        public bool Enabled { get; set; }

        public TrainingArea()
        {
            Enabled = false;
            Radius = 50; // Default
        }

        /// <summary>
        /// Checks if a coordinate is within the training area.
        /// </summary>
        public bool IsInRange(SRCoord pos)
        {
            if (!Enabled) return true;
            return pos.DistanceTo(Center) <= Radius;
        }

        /// <summary>
        /// Gets the distance from the center.
        /// </summary>
        public double GetDistanceFromCenter(SRCoord pos)
        {
            return pos.DistanceTo(Center);
        }
    }
}
#endregion


#region WorldState
namespace SilkroadAIBot.Bot
{
    public class WorldState
    {
        // Thread-safe dictionary for entities: Key = UniqueID
        private ConcurrentDictionary<uint, SREntity> _entities = new ConcurrentDictionary<uint, SREntity>();
        
        // The bot's own character
        private SRCharacter _character = null!;
        public SRCharacter Character 
        { 
            get => _character; 
            set { _character = value; OnCharacterUpdated?.Invoke(); } 
        }
        
        public uint CurrentTargetID { get; set; }
        public TrainingArea TrainingArea { get; set; } = new TrainingArea();

        // UI Events
        public event Action? OnCharacterUpdated;
        public event Action? OnPositionUpdated;
        public event Action? OnStatsUpdated;

        // Session Stats
        private long _sessionXp;
        public long SessionXP { get => _sessionXp; set { _sessionXp = value; OnStatsUpdated?.Invoke(); } }
        
        private long _sessionGold;
        public long SessionGold { get => _sessionGold; set { _sessionGold = value; OnStatsUpdated?.Invoke(); } }

        private int _sessionKills;
        public int SessionKills { get => _sessionKills; set { _sessionKills = value; OnStatsUpdated?.Invoke(); } }

        private int _sessionLoot;
        public int SessionLoot { get => _sessionLoot; set { _sessionLoot = value; OnStatsUpdated?.Invoke(); } }

        public WorldState()
        {
            _character = new SRCharacter();
        }

        public void TriggerCharacterUpdate()
        {
            OnCharacterUpdated?.Invoke();
        }

        public void UpdateCharacterPosition(SRCoord pos)
        {
            if (_character != null)
            {
                _character.Position = pos;
                OnPositionUpdated?.Invoke();
            }
        }

        public void SpawnEntity(SREntity entity)
        {
            if (entity == null) return;
            _entities.AddOrUpdate(entity.UniqueID, entity, (key, oldValue) => entity);
            // Console.WriteLine($"[World] Spawned Entity: {entity.UniqueID}");
        }

        public void AddEntity(SREntity entity) => SpawnEntity(entity);

        public void DespawnEntity(uint uniqueID)
        {
            if (_entities.TryRemove(uniqueID, out var removed))
            {
                // Console.WriteLine($"[World] Despawned Entity: {uniqueID}");
            }
        }

        public void RemoveEntity(uint uniqueID) => DespawnEntity(uniqueID);

        public SREntity? GetEntity(uint uniqueID)
        {
            _entities.TryGetValue(uniqueID, out var entity);
            return entity;
        }

        public IEnumerable<T> GetEntities<T>() where T : SREntity
        {
            return _entities.Values.OfType<T>();
        }

        public void Clear()
        {
            _entities.Clear();
        }
    }
}
#endregion


#region AttackBundle
namespace SilkroadAIBot.Bot.Bundles
{
    public class AttackBundle : IBundle
    {
        public string Name => "AttackBundle";

        private ClientlessConnection _connection; // Need connection access

        public AttackBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public Task UpdateAsync(WorldState worldState)
        {
            if (worldState.CurrentTargetID == 0) return Task.CompletedTask;

            var target = worldState.GetEntity(worldState.CurrentTargetID) as SRMob;
            if (target == null) return Task.CompletedTask;

            // Simple Distance check before skill use
            var charPos = worldState.Character.Position;
            var dist = Math.Sqrt(Math.Pow(charPos.X - target.Position.X, 2) + Math.Pow(charPos.Y - target.Position.Y, 2));

            // Select a skill (For now, first available skill)
            var skill = worldState.Character.Skills.Values.FirstOrDefault(s => !s.IsSelfOnly);
            
            if (skill != null && dist <= skill.Range / 10.0 + 2.0) // /10 for visual coordinate scale, +2 buffer
            {
                var skillController = new SkillController(_connection);
                if (skillController.CastSkill(skill, worldState.CurrentTargetID))
                {
                    // Success
                }
            }
            else if (skill == null)
            {
                // No skills? Auto attack placeholder
            }

            return Task.CompletedTask;
        }
    }
}
#endregion


#region IBundle
namespace SilkroadAIBot.Bot.Bundles
{
    public interface IBundle
    {
        string Name { get; }
        
        // Main loop logic for this bundle
        Task UpdateAsync(WorldState worldState);
        
        // Initialization/Teardown
        void Start();
        void Stop();
    }
}
#endregion


#region LootBundle
namespace SilkroadAIBot.Bot.Bundles
{
    public class LootBundle : IBundle
    {
        public string Name => "LootBundle";
        private ClientlessConnection _connection;
        private long _lastLootTime = 0;
        private const int LOOT_DELAY = 500; // ms

        public LootBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public Task UpdateAsync(WorldState worldState)
        {
            if (worldState.Character == null) return Task.CompletedTask;
            
            long now = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
            if (now - _lastLootTime < LOOT_DELAY) return Task.CompletedTask;

            // Simple Priority: If we are not fighting/moving manually?
            // For now, just check nearest item.
            
            var drops = worldState.GetEntities<SRGroundItem>()
                .Where(i => worldState.TrainingArea.IsInRange(i.Position)) // Respect training area
                .Select(i => new { Item = i, Distance = worldState.Character.Position.DistanceTo(i.Position) })
                .Where(x => x.Distance < 50) // Max loot range
                .OrderBy(x => x.Distance)
                .ToList();

            if (drops.Count > 0)
            {
                var target = drops[0];
                
                // If in pick range (approx 5 game units)
                if (target.Distance <= 5)
                {
                    SendPickup(target.Item.UniqueID);
                     _lastLootTime = now;
                }
                else
                {
                    // Move closer
                    MoveTo(target.Item.Position);
                    _lastLootTime = now + 1000; // Wait for move
                }
            }

            return Task.CompletedTask;
        }

        private void SendPickup(uint uniqueId)
        {
             var packet = new Packet(0x7074);
             // Standard: UniqueID only
             packet.WriteUInt(uniqueId);
             _connection.SendPacket(packet);
             // LogService.Info($"[Loot] Picking up {uniqueId}");
        }
        
        private void MoveTo(SRCoord pos)
        {
             // 0x7021 Movement
             var packet = new Packet(0x7021);
             packet.WriteByte(1); // 1 = Walk/Run
             packet.WriteUShort(pos.Region);
             
             // Mapping: Packet X, Z(N/S), Y(Height)
             // SRCoord: X, Y(N/S), Z(Height)
             
             packet.WriteFloat(pos.X);     
             packet.WriteFloat(pos.Y); // N/S
             packet.WriteFloat(pos.Z); // Height
             
             _connection.SendPacket(packet);
        }
    }
}
#endregion


#region MovementBundle
namespace SilkroadAIBot.Bot.Bundles
{
    public class MovementBundle : IBundle
    {
        public string Name => "MovementBundle";

        private readonly ClientlessConnection _connection;
        private DateTime _nextMoveTime = DateTime.MinValue;
        private List<SRCoord> _currentPath = new List<SRCoord>();
        private int _pathIndex = 0;

        public MovementBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public async Task UpdateAsync(WorldState worldState)
        {
            if (DateTime.Now < _nextMoveTime) return;

            var character = worldState.Character;
            if (character == null) return;
            
            // If we are already moving, maybe wait? 
            // SRO servers usually expect a packet for every chunk of movement or key points.
            // For now, simple waypoint following.

            var currentPos = character.Position;
            var trainingArea = worldState.TrainingArea;

            // 1. Boundary Check: Are we outside the training area?
            if (trainingArea.Enabled && !trainingArea.IsInRange(currentPos))
            {
                if (_currentPath.Count == 0 || _pathIndex >= _currentPath.Count)
                {
                    Console.WriteLine($"[MovementBundle] Outside area (Dist: {trainingArea.GetDistanceFromCenter(currentPos):F1}). Generating return path...");
                    _currentPath = Pathfinder.FindPath(currentPos, trainingArea.Center);
                    _pathIndex = 0;
                }
                
                await FollowPath(worldState);
                return;
            }
            else
            {
                // We are inside. Clear path if it was a return path.
                if (_currentPath.Count > 0)
                {
                     _currentPath.Clear();
                     _pathIndex = 0;
                }
            }

            // 2. Idle Movement (Optional)
            // If strictly inside, maybe we just stop. 
            // If target is null and not moving, we are idle.
        }

        private async Task FollowPath(WorldState worldState)
        {
            if (_pathIndex >= _currentPath.Count) return;

            var target = _currentPath[_pathIndex];
            
            // If close enough to current target node, move to next
            if (worldState.Character.Position.DistanceTo(target) < 2) 
            {
                _pathIndex++;
                if (_pathIndex < _currentPath.Count)
                    target = _currentPath[_pathIndex];
                else
                    return; // Reached end
            }

            // Move to target
            SendMovementPacket(target, worldState.Character.Position.Region);
            
            // Update internal state
            worldState.Character.Destination = target;
            worldState.Character.IsMoving = true;
            
            _nextMoveTime = DateTime.Now.AddSeconds(1); // Throttle
        }

        private void SendMovementPacket(SRCoord target, ushort currentRegion)
        {
            var packet = new Packet(0x7021);
            packet.WriteByte(1); // Type: 1 = Walk
            
            // Standard SRO Movement Packet (Client expects)
            // Region (2) + X (2) + Z (2) + Y (2)
            // Coordinates are usually offsets in the sector or absolute?
            // SimplePK2 / SRO usually uses region-local offsets * 10.
            
            // Logic:
            // If target region != current region, we need to handle region change.
            // For now, assuming relatively local movement.
            
            packet.WriteUShort(target.Region);
            
            // Coordinates in packet are short (local offset * 10) if standard
            // But if using World Coordinates, logic is different.
            // Assuming simplified packet structure for now matching server expectation.
            
            // NOTE: X and Y are often swapped in packet vs standard 3D logic.
            // X (East/West), Z (Up/Down), Y (North/South)
            
            // Packet often: Region (2) -> X (2) -> Z (Height) (2) -> Y (2)
            // Let's rely on standard logic:
            
            // Need to convert float coord to short packet coord (often x10)
            short pX = (short)(target.X < 0 ? target.X : target.X); // Raw float? usually not.
            // Standard: (short)Coord 
            
            packet.WriteShort((short)target.X);
            packet.WriteShort((short)target.Z); // Height
            packet.WriteShort((short)target.Y);

            _connection.SendPacket(packet);
            // Console.WriteLine($"[Movement] Move to {target}");
        }
    }
}
#endregion


#region RecoveryBundle
namespace SilkroadAIBot.Bot.Bundles
{
    public class RecoveryBundle : IBundle
    {
        public string Name => "RecoveryBundle";
        private ClientlessConnection _connection;
        private DateTime _lastHpPotTime = DateTime.MinValue;
        private DateTime _lastMpPotTime = DateTime.MinValue;

        public float HpThreshold { get; set; } = 0.70f; // 70%
        public float MpThreshold { get; set; } = 0.50f; // 50%

        public RecoveryBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public Task UpdateAsync(WorldState worldState)
        {
            var character = worldState.Character;
            if (character == null) return Task.CompletedTask;

            // Check HP
            if (character.HPMax > 0 && (float)character.HP / character.HPMax < HpThreshold)
            {
                UsePotion(character, "HP");
            }

            // Check MP
            if (character.MPMax > 0 && (float)character.MP / character.MPMax < MpThreshold)
            {
                UsePotion(character, "MP");
            }

            return Task.CompletedTask;
        }

        private void UsePotion(SRCharacter character, string type)
        {
            // Cooldown check (default 1s for demo)
            if (type == "HP" && (DateTime.Now - _lastHpPotTime).TotalMilliseconds < 1000) return;
            if (type == "MP" && (DateTime.Now - _lastMpPotTime).TotalMilliseconds < 1000) return;

            // Find potion in inventory
            // Improved search: Look for Vigor first if both low, or specific type
            SRItem? pot = null;

            if (type == "HP")
            {
                 // Try Vigor first if crucial? Or simplify: Just find HP Recovery
                 pot = character.Inventory.FirstOrDefault(i => 
                    (i.Name.Contains("HP Recovery Potion") || i.Name.Contains("Vigor")) && i.Amount > 0);
            }
            else if (type == "MP")
            {
                 pot = character.Inventory.FirstOrDefault(i => 
                    (i.Name.Contains("MP Recovery Potion") || i.Name.Contains("Vigor")) && i.Amount > 0);
            }

            if (pot != null)
            {
                var packet = new Packet(0x704C);
                packet.WriteByte(pot.Slot);
                packet.WriteByte(1); // Count = 1
                packet.WriteUInt(pot.TID); // RefItemID

                _connection.SendPacket(packet);
                Console.WriteLine($"[RecoveryBundle] Used {type} Potion at slot {pot.Slot}");

                if (type == "HP") _lastHpPotTime = DateTime.Now;
                else _lastMpPotTime = DateTime.Now;
            }
        }
    }
}
#endregion


#region TargetBundle
namespace SilkroadAIBot.Bot.Bundles
{
    public class TargetBundle : IBundle
    {
        public string Name => "TargetBundle";
        private ClientlessConnection _connection;

        public TargetBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public Task UpdateAsync(WorldState worldState)
        {
            // If we already have a target, check if it's still valid (alive and exists)
            if (worldState.CurrentTargetID != 0)
            {
                var currentTarget = worldState.GetEntity(worldState.CurrentTargetID) as SRMob;
                if (currentTarget == null || currentTarget.LifeStateType == SRModel.LifeState.Dead)
                {
                    worldState.CurrentTargetID = 0; // Reset target
                    Console.WriteLine("[TargetBundle] Target lost or dead. Searching new...");
                }
                else
                {
                    // Target is still valid
                    return Task.CompletedTask;
                }
            }

            // Find new target using TargetingSystem logic
            var targeting = new TargetingSystem(worldState);
            var nextTarget = targeting.GetNextTarget();

            if (nextTarget.HasValue)
            {
                worldState.CurrentTargetID = nextTarget.Value;
                Console.WriteLine($"[TargetBundle] Acquired Target: {worldState.CurrentTargetID}");
                
                // Opcode 0x7045 (Select Target)
                var selectPacket = new Packet(0x7045);
                selectPacket.WriteUInt(worldState.CurrentTargetID);
                _connection.SendPacket(selectPacket);
            }

            return Task.CompletedTask;
        }
    }
}
#endregion


#region Pathfinder
namespace SilkroadAIBot.Bot.Navigation
{
    public class Pathfinder
    {
        /// <summary>
        /// Finds a path between start and end coordinates.
        /// </summary>
        public static List<SRCoord> FindPath(SRCoord startCoord, SRCoord endCoord)
        {
            // Use SimplePathfinder for now (Basic Direct / Split logic)
            // Can be swapped for full A* later without changing call sites.
            return SimplePathfinder.CalculatePath(startCoord, endCoord);
        }
    }
}
#endregion


#region SimplePathfinder
namespace SilkroadAIBot.Bot.Navigation
{
    public class SimplePathfinder
    {
        // Future: Load NavMesh or WalkMap here
        
        public static List<SRCoord> CalculatePath(SRCoord start, SRCoord end)
        {
            // Currently implements direct pathing.
            // In a real implementation, this would use A* on a grid or navmesh.
            
            var path = new List<SRCoord>();
            path.Add(start);
            
            // Simple direct line check (placeholder for collision detection)
            // If distance is too far, maybe split it? 
            // For now, Silkroad movement is often point-to-point within visibility.
            
            if (start.DistanceTo(end) > 100)
            {
                // Split long paths for safety if no mesh
                var midPoint = new SRCoord(
                    start.Region, 
                    (start.X + end.X) / 2, 
                    (start.Y + end.Y) / 2, 
                    (start.Z + end.Z) / 2
                );
                path.Add(midPoint);
            }
            
            path.Add(end);
            return path;
        }
    }
}
#endregion

