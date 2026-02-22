using System;
using SecurityAPI;

namespace SilkroadAIBot.Core.Helpers
{
    public static class PacketLogger
    {
        // (Direction, Packet, isClientToServer)
        public static Action<string, Packet, bool>? OnPacketLogged;

        public static void Log(string direction, Packet packet, bool isClientToServer)
        {
            OnPacketLogged?.Invoke(direction, packet, isClientToServer);
        }
    }
}
