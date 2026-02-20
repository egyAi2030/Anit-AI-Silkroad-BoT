using System;
using System.Linq;
using System.Threading.Tasks;
using SilkroadAIBot.Models;
using SilkroadAIBot.Networking;
using SilkroadAIBot.Bot;
using SecurityAPI;

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

