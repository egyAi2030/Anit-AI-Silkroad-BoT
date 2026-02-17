using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using SilkroadAIBot.Core.Helpers;
using SilkroadAIBot.Core.Models;

namespace SilkroadAIBot.Core.Data
{
    public class DataManager
    {
        private SimplePK2? _mediaPk2;
        private SimplePK2? _dataPk2;
        private DatabaseManager _db = null!;
        
        // Caches
        private Dictionary<uint, SRModelInfo> _modelInfo = new Dictionary<uint, SRModelInfo>();
        private Dictionary<string, string> _codeNameToName = new Dictionary<string, string>();
        private Dictionary<uint, string> _modelNames = new Dictionary<uint, string>();
        private Dictionary<uint, SRSkill> _skills = new Dictionary<uint, SRSkill>();
        
        public struct SRModelInfo
        {
            public uint ID;
            public string CodeName;
            public string Name;
            public byte TypeID1;
            public byte TypeID2;
            public byte TypeID3;
            public byte TypeID4;
        }

        public void Initialize(string sroPath, DatabaseManager db)
        {
            _db = db;
            string mediaPath = Path.Combine(sroPath, "Media.pk2");
            if (!File.Exists(mediaPath))
            {
                throw new FileNotFoundException("Media.pk2 not found in: " + sroPath);
            }

            _mediaPk2 = new SimplePK2(mediaPath);
            
            // Temporary Stub: Just log that we found it. 
            // We will implement full parsing later.
            LogService.Info($"[System] Game Data linked successfully at: {sroPath}");
        }

        public bool AutoDiscoverServerConfig(out string ip, out int port)
        {
            ip = "127.0.0.1";
            port = 15779;

            if (_mediaPk2 == null) return false;

            try
            {
                // 1. Division Info
                var divData = _mediaPk2.GetFile("divisioninfo.txt");
                if (divData != null)
                {
                    using (var reader = new BinaryReader(new MemoryStream(divData)))
                    {
                        byte count = reader.ReadByte();
                        for (int i = 0; i < count; i++)
                        {
                            string divName = Encoding.ASCII.GetString(reader.ReadBytes(reader.ReadInt32()));
                            byte divCount = reader.ReadByte();
                            for (int j = 0; j < divCount; j++)
                            {
                                ip = Encoding.ASCII.GetString(reader.ReadBytes(reader.ReadInt32()));
                            }
                        }
                    }
                }

                // 2. Gateway Port
                var portData = _mediaPk2.GetFile("gateport.txt");
                if (portData != null)
                {
                    string portStr = Encoding.ASCII.GetString(portData).Trim();
                    if (int.TryParse(portStr, out int p)) port = p;
                }

                Console.WriteLine($"[DataManager] Discovered Server: {ip}:{port}");
                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[DataManager] Error during server discovery: {ex.Message}");
                return false;
            }
        }

        private void ExtractDataToDatabase()
        {
            // Only extract if we haven't already or if explicitly requested
            // For now, extract and cache in memory, but also save to DB
            LoadTextData();
            LoadCharacterData();
            LoadSkillData();

            if (_db != null)
            {
                Console.WriteLine("[DataManager] Saving extracted data to database...");
                _db.SaveModelsBatch(_modelInfo.Values);
                _db.SaveSkillsBatch(_skills.Values);
                Console.WriteLine("[DataManager] Data extraction to database complete.");
            }
            
            BuildModelNameCache();
        }
        
        public string GetModelName(uint modelID)
        {
            if (_modelNames.TryGetValue(modelID, out var name))
                return name;
            return $"Unknown[{modelID}]";
        }

        public SRModelInfo? GetModelInfo(uint modelID)
        {
            if (_modelInfo.TryGetValue(modelID, out var info))
                return info;
            return null;
        }
        
        private void LoadTextData()
        {
            // Try common paths
            var fileData = _mediaPk2.GetFile("server_dep\\silkroad\\textdata\\textdata_object.txt");
            if (fileData == null) fileData = _mediaPk2.GetFile("server_dep/silkroad/textdata/textdata_object.txt");
            
            if (fileData != null)
            {
                using (var reader = new StreamReader(new MemoryStream(fileData), Encoding.Unicode)) 
                {
                    string line;
                    while ((line = reader.ReadLine()) != null)
                    {
                        var parts = line.Split('\t');
                        if (parts.Length >= 3)
                        {
                            string codeName = parts[1];
                            string realName = parts[2];
                            _codeNameToName[codeName] = realName;
                        }
                    }
                }
                Console.WriteLine($"[DataManager] Loaded {_codeNameToName.Count} text entries.");
            }
        }
        
        private void LoadCharacterData()
        {
            if (_dataPk2 == null) return;

            foreach (var filePath in _dataPk2.GetFileNames())
            {
                string fileName = Path.GetFileName(filePath);
                if (fileName.StartsWith("characterdata_") && fileName.EndsWith(".txt"))
                {
                    byte[] data = _dataPk2.GetFile(filePath);
                    if (data != null)
                    {
                        using (var reader = new StreamReader(new MemoryStream(data), Encoding.Unicode))
                        {
                            string line;
                            while ((line = reader.ReadLine()) != null)
                            {
                                var parts = line.Split('\t');
                                if (parts.Length >= 13) // We need at least up to TypeID4
                                {
                                    if (uint.TryParse(parts[0], out uint id))
                                    {
                                        var info = new SRModelInfo
                                        {
                                            ID = id,
                                            CodeName = parts[1],
                                            TypeID1 = byte.Parse(parts[9]),
                                            TypeID2 = byte.Parse(parts[10]),
                                            TypeID3 = byte.Parse(parts[11]),
                                            TypeID4 = byte.Parse(parts[12])
                                        };
                                        _modelInfo[id] = info;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Console.WriteLine($"[DataManager] Loaded {_modelInfo.Count} character data entries.");
        }
        
        private void BuildModelNameCache()
        {
            foreach (var kvp in _modelInfo)
            {
                var info = kvp.Value;
                if (_codeNameToName.TryGetValue(info.CodeName, out var realName))
                {
                    info.Name = realName;
                    _modelNames[kvp.Key] = realName;
                }
                else
                {
                    info.Name = info.CodeName;
                    _modelNames[kvp.Key] = info.CodeName; // Fallback to CodeName
                }
                _modelInfo[kvp.Key] = info;
            }
        }
        
        public void Dispose()
        {
            _mediaPk2?.Close();
            _dataPk2?.Close();
        }

        private void LoadSkillData()
        {
            if (_dataPk2 == null) return;

            foreach (var filePath in _dataPk2.GetFileNames())
            {
                string fileName = Path.GetFileName(filePath);
                if (fileName.StartsWith("skilldata_") && fileName.EndsWith(".txt"))
                {
                    byte[] data = _dataPk2.GetFile(filePath);
                    if (data != null)
                    {
                        using (var reader = new StreamReader(new MemoryStream(data), Encoding.Unicode))
                        {
                            string line;
                            while ((line = reader.ReadLine()) != null)
                            {
                                if (string.IsNullOrEmpty(line) || line.StartsWith("//")) continue;

                                var parts = line.Split('\t');
                                if (parts.Length < 63) continue;

                                if (byte.TryParse(parts[0], out byte service) && service == 1)
                                {
                                    if (uint.TryParse(parts[1], out uint id))
                                    {
                                        var skill = new SRSkill
                                        {
                                            ID = id,
                                            CodeName = parts[3],
                                            Level = byte.Parse(parts[7]),
                                            CastTime = int.Parse(parts[12]),
                                            Cooldown = int.Parse(parts[14]),
                                            Range = short.Parse(parts[21]),
                                            IsSelfOnly = parts[26] == "1",
                                            MPUsage = short.Parse(parts[53]),
                                            IconPath = parts[61]
                                        };

                                        // Lookup localized name
                                        if (_codeNameToName.TryGetValue(parts[62], out string realName))
                                            skill.Name = realName;
                                        else
                                            skill.Name = skill.CodeName;

                                        _skills[id] = skill;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        public SRSkill GetSkill(uint id)
        {
            if (_skills.TryGetValue(id, out var skill))
                return skill;
            return null;
        }
    }
}
