using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using SilkroadAIBot.Core.Models;

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
