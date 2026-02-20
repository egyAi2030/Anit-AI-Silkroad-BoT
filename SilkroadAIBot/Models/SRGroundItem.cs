using System;

namespace SilkroadAIBot.Models
{
    public class SRGroundItem : SREntity
    {
        public SRCoord Position { get; set; }
        public ushort Amount { get; set; } = 1;
        // Optional: Source (Drop owner), Rarity, etc.
    }
}

