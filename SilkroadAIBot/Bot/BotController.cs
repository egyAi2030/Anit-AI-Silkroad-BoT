using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using SilkroadAIBot.Bot.Bundles;
using SilkroadAIBot.Networking;

namespace SilkroadAIBot.Bot
{
    public class BotController
    {
        private WorldState _worldState;
        private ClientlessConnection _connection;
        private SilkroadAIBot.Data.DatabaseManager _db;
        private List<IBundle> _bundles;
        private bool _isRunning;
        private DateTime _lastSaveTime;

        public BotController(WorldState worldState, ClientlessConnection connection, SilkroadAIBot.Data.DatabaseManager db)
        {
            _worldState = worldState;
            _connection = connection;
            _db = db;
            _bundles = new List<IBundle>();
            _lastSaveTime = DateTime.Now;
        }

        public void AddBundle(IBundle bundle)
        {
            _bundles.Add(bundle);
            // Console.WriteLine($"[Bot] Added Bundle: {bundle.Name}");
        }

        public void Start()
        {
            if (_isRunning) return;
            
            _isRunning = true;
            Console.WriteLine("[Bot] Starting Bot Controller...");
            
            foreach (var bundle in _bundles)
            {
                bundle.Start();
            }

            // Start main loop in background
            _ = BotLoop();
        }

        public void Stop()
        {
            _isRunning = false;
            foreach (var bundle in _bundles)
            {
                bundle.Stop();
            }
            Console.WriteLine("[Bot] Bot Controller Stopped.");
        }

        private async Task BotLoop()
        {
            while (_isRunning)
            {
                try
                {
                    // Execute bundles sequentially for thread safety on WorldState if needed
                    // (Though WorldState is concurrent, logic order matters)
                    
                    foreach (var bundle in _bundles)
                    {
                        await bundle.UpdateAsync(_worldState);
                    }

                    // Auto-Save every 5 minutes
                    if ((DateTime.Now - _lastSaveTime).TotalMinutes >= 5)
                    {
                        await Task.Run(() => _db.UpdateCharacterState(_worldState.Character));
                        _lastSaveTime = DateTime.Now;
                        Console.WriteLine("[Bot] Auto-Save completed.");
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"[Bot] Error in BotLoop: {ex.Message}");
                }

                await Task.Delay(100); // 10 ticks per second
            }
        }
    }
}


