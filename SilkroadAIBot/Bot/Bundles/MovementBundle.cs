using System;
using System.Threading.Tasks;
using SilkroadAIBot.Models;
using SilkroadAIBot.Bot.Navigation;
using SilkroadAIBot.Networking;
using SecurityAPI;

namespace SilkroadAIBot.Bot.Bundles
{
    public class MovementBundle : IBundle
    {
        public string Name => "MovementBundle";

        private readonly ClientlessConnection _connection;
        private DateTime _nextMoveTime = DateTime.MinValue;
        private List<SRCoord> _currentPath = new List<SRCoord>();
        private int _pathIndex = 0;

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
            
            // If we are already moving, maybe wait? 
            // SRO servers usually expect a packet for every chunk of movement or key points.
            // For now, simple waypoint following.

            var currentPos = character.Position;
            var trainingArea = worldState.TrainingArea;

            // 1. Boundary Check: Are we outside the training area?
            if (trainingArea.Enabled && !trainingArea.IsInRange(currentPos))
            {
                if (_currentPath.Count == 0 || _pathIndex >= _currentPath.Count)
                {
                    Console.WriteLine($"[MovementBundle] Outside area (Dist: {trainingArea.GetDistanceFromCenter(currentPos):F1}). Generating return path...");
                    _currentPath = Pathfinder.FindPath(currentPos, trainingArea.Center);
                    _pathIndex = 0;
                }
                
                await FollowPath(worldState);
                return;
            }
            else
            {
                // We are inside. Clear path if it was a return path.
                if (_currentPath.Count > 0)
                {
                     _currentPath.Clear();
                     _pathIndex = 0;
                }
            }

            // 2. Idle Movement (Optional)
            // If strictly inside, maybe we just stop. 
            // If target is null and not moving, we are idle.
        }

        private async Task FollowPath(WorldState worldState)
        {
            if (_pathIndex >= _currentPath.Count) return;

            var target = _currentPath[_pathIndex];
            
            // If close enough to current target node, move to next
            if (worldState.Character.Position.DistanceTo(target) < 2) 
            {
                _pathIndex++;
                if (_pathIndex < _currentPath.Count)
                    target = _currentPath[_pathIndex];
                else
                    return; // Reached end
            }

            // Move to target
            SendMovementPacket(target, worldState.Character.Position.Region);
            
            // Update internal state
            worldState.Character.Destination = target;
            worldState.Character.IsMoving = true;
            
            _nextMoveTime = DateTime.Now.AddSeconds(1); // Throttle
        }

        private void SendMovementPacket(SRCoord target, ushort currentRegion)
        {
            var packet = new Packet(0x7021);
            packet.WriteByte(1); // Type: 1 = Walk
            
            // Standard SRO Movement Packet (Client expects)
            // Region (2) + X (2) + Z (2) + Y (2)
            // Coordinates are usually offsets in the sector or absolute?
            // SimplePK2 / SRO usually uses region-local offsets * 10.
            
            // Logic:
            // If target region != current region, we need to handle region change.
            // For now, assuming relatively local movement.
            
            packet.WriteUShort(target.Region);
            
            // Coordinates in packet are short (local offset * 10) if standard
            // But if using World Coordinates, logic is different.
            // Assuming simplified packet structure for now matching server expectation.
            
            // NOTE: X and Y are often swapped in packet vs standard 3D logic.
            // X (East/West), Z (Up/Down), Y (North/South)
            
            // Packet often: Region (2) -> X (2) -> Z (Height) (2) -> Y (2)
            // Let's rely on standard logic:
            
            // Need to convert float coord to short packet coord (often x10)
            short pX = (short)(target.X < 0 ? target.X : target.X); // Raw float? usually not.
            // Standard: (short)Coord 
            
            packet.WriteShort((short)target.X);
            packet.WriteShort((short)target.Z); // Height
            packet.WriteShort((short)target.Y);

            _connection.SendPacket(packet);
            // Console.WriteLine($"[Movement] Move to {target}");
        }
    }
}

