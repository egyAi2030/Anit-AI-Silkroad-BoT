using System;

namespace SilkroadAIBot.Models
{
    // Base entity
    public class SREntity
    {
        public uint UniqueID { get; set; }
        public uint ModelID { get; set; }
        public string Name { get; set; }
        
        // Placeholder for factory method if needed later
        // public static SREntity Create(uint modelID) { ... }
    }
}

