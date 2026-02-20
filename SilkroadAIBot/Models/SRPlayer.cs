using System;

namespace SilkroadAIBot.Models
{
    public class SRPlayer : SRModel
    {
        // Name is in SREntity
        public byte Level { get; set; }
        public Job JobType { get; set; }
        public byte JobLevel { get; set; }
        public uint JobExp { get; set; }
        public bool InCombat { get; set; }
        public uint RidingUniqueID { get; set; }
        public string GuildName { get; set; }
        
        public enum Job { None = 0, Trader = 1, Thief = 2, Hunter = 3 }
    }
}

