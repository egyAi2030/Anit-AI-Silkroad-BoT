using System;
using SilkroadAIBot.Core.Networking;
using SilkroadAIBot.Core.Models;
using SecurityAPI;

namespace SilkroadAIBot.Bot
{
    public class LootController
    {
        private readonly ClientlessConnection _connection;

        public LootController(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Pickup(uint itemUniqueId)
        {
            // Construct 0x7074 packet for pickup
            var packet = new Packet(0x7074);
            packet.WriteByte(1); // Execute
            packet.WriteByte(2); // Pickup
            packet.WriteByte(1); // Target: Entity
            packet.WriteUInt(itemUniqueId);

            _connection.SendPacket(packet);
            Console.WriteLine($"[LootController] Picking up item {itemUniqueId}");
        }
    }
}
