using System;
using SilkroadAIBot.Core.Models;

namespace SilkroadAIBot.Bot
{
    public class TrainingArea
    {
        public SRCoord Center { get; set; }
        public int Radius { get; set; }
        public bool Enabled { get; set; }

        public TrainingArea()
        {
            Enabled = false;
            Radius = 50; // Default
        }

        /// <summary>
        /// Checks if a coordinate is within the training area.
        /// </summary>
        public bool IsInRange(SRCoord pos)
        {
            if (!Enabled) return true;
            return pos.DistanceTo(Center) <= Radius;
        }

        /// <summary>
        /// Gets the distance from the center.
        /// </summary>
        public double GetDistanceFromCenter(SRCoord pos)
        {
            return pos.DistanceTo(Center);
        }
    }
}
