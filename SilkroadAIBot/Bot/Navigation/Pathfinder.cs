using System;
using System.Collections.Generic;
using System.Numerics;
using SilkroadAIBot.Models;
using System.Linq;

namespace SilkroadAIBot.Bot.Navigation
{
    public class Pathfinder
    {
        /// <summary>
        /// Finds a path between start and end coordinates.
        /// </summary>
        public static List<SRCoord> FindPath(SRCoord startCoord, SRCoord endCoord)
        {
            // Use SimplePathfinder for now (Basic Direct / Split logic)
            // Can be swapped for full A* later without changing call sites.
            return SimplePathfinder.CalculatePath(startCoord, endCoord);
        }
    }
}

