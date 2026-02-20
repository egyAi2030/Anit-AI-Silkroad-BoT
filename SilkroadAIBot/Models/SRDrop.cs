using System;

namespace SilkroadAIBot.Models
{
    public class SRDrop : SREntity
    {
        // ModelID is in SREntity
        public uint Amount { get; set; }
        public SRCoord Position { get; set; }
        public bool IsEquipment { get; set; }
        public byte Plus { get; set; }
        public string OwnerName { get; set; } // specific owner or empty
    }
}

