using System;

namespace SilkroadAIBot.Models
{
    public class SREquipable : SRItem
    {
        public byte Plus { get; set; }
        public ulong Variance { get; set; }
        public uint Durability { get; set; }
        public byte DurabilityMax { get; set; } // Or uint?
        // Magic options and sockets can be added later
    }
}

