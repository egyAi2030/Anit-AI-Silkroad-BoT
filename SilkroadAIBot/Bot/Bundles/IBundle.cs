using System;
using System.Threading.Tasks;

namespace SilkroadAIBot.Bot.Bundles
{
    public interface IBundle
    {
        string Name { get; }
        
        // Main loop logic for this bundle
        Task UpdateAsync(WorldState worldState);
        
        // Initialization/Teardown
        void Start();
        void Stop();
    }
}
