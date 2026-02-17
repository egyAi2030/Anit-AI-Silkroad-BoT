using System;
using System.Linq;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Models;
using SilkroadAIBot.Core.Networking;
using SecurityAPI;

namespace SilkroadAIBot.Bot.Bundles
{
    public class LootBundle : IBundle
    {
        public string Name => "LootBundle";
        private ClientlessConnection _connection;
        private long _lastLootTime = 0;

        public LootBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public Task UpdateAsync(WorldState worldState)
        {
            long now = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
            if (now - _lastLootTime < 500) return Task.CompletedTask;

            var charPos = worldState.Character?.Position;
            if (charPos == null) return Task.CompletedTask;

            // Use the non-nullable value for the rest of the method
            SRCoord currentPos = charPos.Value;

            var drops = worldState.GetEntities<SRDrop>()
                .Select(d => new { Drop = d, Distance = CalculateDistance(currentPos, d.Position) })
                .Where(x => x.Distance < 50) 
                .OrderBy(x => x.Distance)
                .ToList();

            if (drops.Count > 0)
            {
                var lootController = new LootController(_connection);
                lootController.Pickup(drops[0].Drop.UniqueID);
                _lastLootTime = now;
            }

            return Task.CompletedTask;
        }

        private double CalculateDistance(SRCoord pos1, SRCoord pos2)
        {
            return Math.Sqrt(Math.Pow(pos1.X - pos2.X, 2) + Math.Pow(pos1.Y - pos2.Y, 2));
        }
    }
}
