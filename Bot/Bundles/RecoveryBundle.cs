using System;
using System.Linq;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Models;
using SilkroadAIBot.Core.Networking;
using SecurityAPI;

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
            // Simplified: look for items with HP/MP in CodeName or specific TypeID
            var pot = character.Inventory.FirstOrDefault(i => i.Name.Contains(type, StringComparison.OrdinalIgnoreCase) && i.Amount > 0);

            if (pot != null)
            {
                var packet = new Packet(0x704C);
                packet.WriteByte(pot.Slot);
                // TypeID (TID) usage depends on client version. Defaulting to uint for private servers.
                packet.WriteUInt(pot.TID); 

                _connection.SendPacket(packet);
                Console.WriteLine($"[RecoveryBundle] Used {type} Potion at slot {pot.Slot}");

                if (type == "HP") _lastHpPotTime = DateTime.Now;
                else _lastMpPotTime = DateTime.Now;
            }
        }
    }
}
