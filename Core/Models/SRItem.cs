using System;

namespace SilkroadAIBot.Core.Models
{
    public class SRItem
    {
        public uint ModelID { get; set; }
        public uint TID { get; set; }
        public byte Slot { get; set; }
        public string Name { get; set; }
        public ushort Amount { get; set; }
    }
}
