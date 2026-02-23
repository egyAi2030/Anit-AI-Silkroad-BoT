using System;
using System.Collections.Generic;



#region SRBuff
namespace SilkroadAIBot.Models
{
    public class SRBuff
    {
        public uint ID { get; set; }
        public uint UniqueID { get; set; }
        public uint GroupID { get; set; }
        public uint CasterUniqueID { get; set; }
        public uint TargetUniqueID { get; set; }
        // Additional properties can be added as needed
    }
}
#endregion


#region SRCharacter
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
#endregion


#region SRCoord
namespace SilkroadAIBot.Models
{
    public struct SRCoord
    {
        public ushort Region;
        public float X;
        public float Y;
        public float Z;

        public float WorldX
        {
            get
            {
                if (IsDungeon) return X; // Dungeons often use absolute coords or different mapping
                byte xSector = (byte)(Region & 0xFF);
                return (xSector - 135) * 192 + (X / 10);
            }
        }

        public float WorldY
        {
            get
            {
                if (IsDungeon) return Y;
                byte ySector = (byte)(Region >> 8);
                return (ySector - 92) * 192 + (Y / 10);
            }
        }

        public bool IsDungeon => Region > 32767;

        public SRCoord(ushort region, float x, float y, float z)
        {
            Region = region;
            X = x;
            Y = y;
            Z = z;
        }

        public double DistanceTo(SRCoord other)
        {
            if (IsDungeon != other.IsDungeon) return 100000; // Too far
            return Math.Sqrt(Math.Pow(WorldX - other.WorldX, 2) + Math.Pow(WorldY - other.WorldY, 2));
        }

        public override string ToString()
        {
            return $"[{Region}] ({X:F1}, {Y:F1}, {Z:F1}) | World({WorldX:F1}, {WorldY:F1})";
        }
    }
}
#endregion


#region SRDrop
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
#endregion


#region SREntity
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
#endregion


#region SREquipable
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
#endregion


#region SRGroundItem
namespace SilkroadAIBot.Models
{
    public class SRGroundItem : SREntity
    {
        public SRCoord Position { get; set; }
        public ushort Amount { get; set; } = 1;
        // Optional: Source (Drop owner), Rarity, etc.
    }
}
#endregion


#region SRItem
namespace SilkroadAIBot.Models
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
#endregion


#region SRMastery
namespace SilkroadAIBot.Models
{
    public class SRMastery
    {
        public uint ID { get; set; }
        public byte Level { get; set; }
    }
}
#endregion


#region SRMob
namespace SilkroadAIBot.Models
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
#endregion


#region SRModel
namespace SilkroadAIBot.Models
{
    public class SRModel : SREntity
    {
        public SRCoord Position { get; set; }
        public SRCoord MovementPosition { get; set; }
        public ushort Angle { get; set; }
        public float SpeedWalking { get; set; }
        public float SpeedRunning { get; set; }
        public float SpeedBerserk { get; set; }
        public uint HP { get; set; }
        public uint HPMax { get; set; }
        public uint MP { get; set; }
        public uint MPMax { get; set; }
        
        public LifeState LifeStateType { get; set; }
        public MotionState MotionStateType { get; set; }
        public GameState GameStateType { get; set; }
        
        // Dictionary for Buffs: Key = BuffID? Or UniqueID of buff instance?
        // Using Dictionary<uint, SRBuff> as per analysis, usually Key is BuffID
        public Dictionary<uint, SRBuff> Buffs { get; set; } = new Dictionary<uint, SRBuff>();

        public enum LifeState { Alive = 1, Dead = 2 }
        public enum MotionState { Standing = 0, Walking = 1, Running = 2 }
        public enum GameState { Normal = 0, InCombat = 1 }
    }
}
#endregion


#region SRNpc
namespace SilkroadAIBot.Models
{
    public class SRNpc : SRModel
    {
        // Add NPC specific properties here if needed later
        // e.g., Talk flags, shop type, etc.
    }
}
#endregion


#region SRPlayer
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
#endregion


#region SRQuest
namespace SilkroadAIBot.Models
{
    public class SRQuest
    {
        public uint ID { get; set; }
        // Add status, progress, etc.
    }
}
#endregion


#region SRSkill
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
#endregion

