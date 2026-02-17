using System;

namespace SilkroadAIBot.Core.Models
{
    public class SRMob : SRNpc
    {
        public MobTypeEnum MobType { get; set; }
        public byte Appearance { get; set; }
        
        public enum MobTypeEnum
        {
            General = 0,
            Champion = 1,
            Giant = 2,
            Titan = 3,
            Elite = 4,
            Unique = 5
        }
    }
}
