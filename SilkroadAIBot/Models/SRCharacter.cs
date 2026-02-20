using System;
using System.Collections.Generic;

namespace SilkroadAIBot.Models
{
    public class SRCharacter : SRPlayer
    {
        public ulong Exp { get; set; }
        public ulong ExpMax { get; set; }
        public uint SPExp { get; set; }
        public ulong Gold { get; set; }
        public uint SP { get; set; }
        public ushort StatPoints { get; set; }
        public ushort STR { get; set; }
        public ushort INT { get; set; }
        public SRCoord? Destination { get; set; }
        public bool IsMoving { get; set; }
        
        public Dictionary<uint, bool> KnownSkills { get; set; } = new Dictionary<uint, bool>();
        public List<SRItem> Inventory { get; set; } = new List<SRItem>();
        public Dictionary<uint, SRSkill> Skills { get; set; } = new Dictionary<uint, SRSkill>();
        public Dictionary<uint, SRQuest> Quests { get; set; } = new Dictionary<uint, SRQuest>();
        public Dictionary<uint, SRMastery> Masteries { get; set; } = new Dictionary<uint, SRMastery>();
    }
}

