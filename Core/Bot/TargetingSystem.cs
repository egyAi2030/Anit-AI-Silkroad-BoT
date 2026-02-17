using System;
using System.Linq;
using SilkroadAIBot.Bot;
using SilkroadAIBot.Core.Models;

namespace SilkroadAIBot.Core.Bot
{
    public class TargetingSystem
    {
        private readonly WorldState _worldState;
        private uint _currentTargetId;
        
        public TargetPriority Priority { get; set; } = TargetPriority.Closest;

        public TargetingSystem(WorldState worldState)
        {
            _worldState = worldState;
        }

        public uint? GetNextTarget()
        {
            var charPos = _worldState.Character?.Position;
            if (charPos == null) return null;

            // Filter for Mobs that are alive and not currently targeted by others (optional)
            var targets = _worldState.GetEntities<SRMob>()
                .Where(m => m.HP > 0 && _worldState.TrainingArea.IsInRange(m.Position)) 
                .Select(m => new 
                { 
                    Mob = m, 
                    Distance = CalculateDistance(charPos.Value, m.Position) 
                })
                .OrderBy(x => x.Distance)
                .ToList();

            if (targets.Count > 0)
            {
                _currentTargetId = targets[0].Mob.UniqueID;
                return _currentTargetId;
            }

            return null;
        }

        private double CalculateDistance(SRCoord pos1, SRCoord pos2)
        {
            // Simple 2D distance for now
            return Math.Sqrt(Math.Pow(pos1.X - pos2.X, 2) + Math.Pow(pos1.Y - pos2.Y, 2));
        }

        public enum TargetPriority
        {
            Closest,
            LowestHP,
            HighestHP
        }
    }
}
