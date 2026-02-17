using System;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Models;
using SilkroadAIBot.Core.Navigation;
using SilkroadAIBot.Core.Networking;
using SecurityAPI;

namespace SilkroadAIBot.Bot.Bundles
{
    public class MovementBundle : IBundle
    {
        public string Name => "MovementBundle";

        private readonly ClientlessConnection _connection;
        private DateTime _nextMoveTime = DateTime.MinValue;

        public MovementBundle(ClientlessConnection connection)
        {
            _connection = connection;
        }

        public void Start() { }
        public void Stop() { }

        public async Task UpdateAsync(WorldState worldState)
        {
            if (DateTime.Now < _nextMoveTime) return;

            var character = worldState.Character;
            if (character == null) return;

            var currentPos = character.Position;
            var trainingArea = worldState.TrainingArea;

            // 1. Boundary Check: Are we outside the training area?
            if (trainingArea.Enabled && !trainingArea.IsInRange(currentPos))
            {
                Console.WriteLine("[MovementBundle] Outside training area! Returning to center...");
                await MoveToCoord(currentPos, trainingArea.Center, worldState);
                _nextMoveTime = DateTime.Now.AddSeconds(2); // Wait a bit before next decision
                return;
            }

            // 2. Idle Movement: If we are not moving and have no target, maybe random walk?
            if (worldState.CurrentTargetID == 0 && !character.IsMoving)
            {
                // Optional: Random walk within training area (similar to xBot)
                // For now, let's just ensure we stay at center if idle and out of spot
                if (trainingArea.Enabled && trainingArea.GetDistanceFromCenter(currentPos) > 10)
                {
                     await MoveToCoord(currentPos, trainingArea.Center, worldState);
                     _nextMoveTime = DateTime.Now.AddSeconds(5);
                }
            }
        }

        private async Task MoveToCoord(SRCoord start, SRCoord end, WorldState worldState)
        {
            var path = Pathfinder.FindPath(start, end);
            if (path.Count > 0)
            {
                // For now, move to the first relevant waypoint or the end if close
                var target = path.Count > 1 ? path[1] : path[0];
                SendMovementPacket(target);
                worldState.Character.Destination = target;
                worldState.Character.IsMoving = true;
            }
        }

        private void SendMovementPacket(SRCoord target)
        {
            var packet = new Packet(0x7021);
            packet.WriteByte(1); // Type: Move
            
            if (target.IsDungeon)
            {
                packet.WriteUInt(target.Region);
                packet.WriteFloat(target.X);
                packet.WriteFloat(target.Z);
                packet.WriteFloat(target.Y);
            }
            else
            {
                packet.WriteByte(1); // Local move? or usually just region/coords
                packet.WriteUInt(target.Region);
                packet.WriteUShort((ushort)target.X);
                packet.WriteUShort((ushort)target.Z);
                packet.WriteUShort((ushort)target.Y);
            }

            _connection.SendPacket(packet);
        }
    }
}
