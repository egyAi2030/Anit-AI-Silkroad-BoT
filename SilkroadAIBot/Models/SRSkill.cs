using System;

namespace SilkroadAIBot.Models
{
    public class SRSkill
    {
        public uint ID { get; set; }
        public string CodeName { get; set; }
        public string Name { get; set; }
        
        public int CastTime { get; set; } // in ms
        public int Cooldown { get; set; } // in ms
        
        public short MPUsage { get; set; }
        public short Range { get; set; }
        
        public bool IsSelfOnly { get; set; }
        public byte Level { get; set; }
        
        public string IconPath { get; set; }
        
        public override string ToString()
        {
            return $"[{ID}] {Name} (Lv.{Level})";
        }
    }
}

