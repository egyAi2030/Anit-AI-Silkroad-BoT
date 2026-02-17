using System;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Models;
using SilkroadAIBot.Core.Networking;
using SecurityAPI;

namespace SilkroadAIBot.Bot.Bundles
{
    public class AttackBundle : IBundle
    {
        public string Name => "AttackBundle";

        private long _lastAttackTime = 0;
        private ClientlessConnection _connection; // Need connection access

        public AttackBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public Task UpdateAsync(WorldState worldState)
        {
            if (worldState.CurrentTargetID == 0) return Task.CompletedTask;

            var target = worldState.GetEntity(worldState.CurrentTargetID) as SRMob;
            if (target == null) return Task.CompletedTask;

            // Simple Distance check before skill use
            var charPos = worldState.Character.Position;
            var dist = Math.Sqrt(Math.Pow(charPos.X - target.Position.X, 2) + Math.Pow(charPos.Y - target.Position.Y, 2));

            // Select a skill (For now, first available skill)
            var skill = worldState.Character.Skills.Values.FirstOrDefault(s => !s.IsSelfOnly);
            
            if (skill != null && dist <= skill.Range / 10.0 + 2.0) // /10 for visual coordinate scale, +2 buffer
            {
                var skillController = new SkillController(_connection);
                if (skillController.CastSkill(skill, worldState.CurrentTargetID))
                {
                    // Success
                }
            }
            else if (skill == null)
            {
                // No skills? Auto attack placeholder
            }

            return Task.CompletedTask;
        }
    }
}
