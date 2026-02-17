using System;
using System.Collections.Generic;

namespace SilkroadAIBot.Core.Models
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
