using System;
using System.Linq;
using System.Threading.Tasks;
using SilkroadAIBot.Models;
using SilkroadAIBot.Networking;
using SecurityAPI;

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

