using System;
using System.Collections.Generic;
using SilkroadAIBot.Networking;
using SecurityAPI;
using SilkroadAIBot.Models;

namespace SilkroadAIBot.Bot
{
    public class SkillController
    {
        private readonly ClientlessConnection _connection;
        private readonly Dictionary<uint, DateTime> _cooldowns = new Dictionary<uint, DateTime>();

        public SkillController(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public bool CastSkill(SRSkill skill, uint targetId)
        {
            if (skill == null) return false;
            
            if (IsOnCooldown(skill.ID))
            {
                // Console.WriteLine($"[SkillController] Skill {skill.Name} is on cooldown.");
                return false;
            }

            // Construct 0x7074 packet
            var packet = new Packet(0x7074);
            packet.WriteByte(1); // Execute
            packet.WriteByte(4); // Use Skill
            packet.WriteUInt(skill.ID);
            packet.WriteByte(1); // TargetType: Entity
            packet.WriteUInt(targetId);

            _connection.SendPacket(packet);
            
            // Set cooldown
            _cooldowns[skill.ID] = DateTime.Now.AddMilliseconds(skill.Cooldown);
            
            Console.WriteLine($"[SkillController] Casting {skill.Name} on {targetId}");
            return true;
        }

        public bool IsOnCooldown(uint skillId)
        {
            if (_cooldowns.TryGetValue(skillId, out DateTime endTime))
            {
                return DateTime.Now < endTime;
            }
            return false;
        }
    }
}

