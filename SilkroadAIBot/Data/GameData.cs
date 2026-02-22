using System;
using Microsoft.Data.Sqlite;
using System.IO;
using SilkroadAIBot.Models;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Helpers;
using SRO.PK2API;
using SilkroadAIBot.Core.Configuration;
using SRO.PK2API.Security;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Linq;
using SRO.PK2API.Utility;
using System.Security.Authentication;



#region DatabaseManager
namespace SilkroadAIBot.Data
{
    public class DatabaseManager : IDisposable
    {
        private SqliteConnection _connection;
        private string _dbPath = "bot_data.db";

        public DatabaseManager()
        {
            Initialize();
        }

        private void Initialize()
        {
            // Microsoft.Data.Sqlite handles file creation automatically in the connection string if not exists
            _connection = new SqliteConnection($"Data Source={_dbPath}");
            _connection.Open();

            // Create tables if not exist
            string createConfigTable = @"
                CREATE TABLE IF NOT EXISTS Configuration (
                    Key TEXT PRIMARY KEY,
                    Value TEXT
                )";
            
            using (var command = new SqliteCommand(createConfigTable, _connection))
            {
                command.ExecuteNonQuery();
            }

            string createCharactersTable = @"
                CREATE TABLE IF NOT EXISTS Characters (
                    Id INTEGER PRIMARY KEY AUTOINCREMENT,
                    Name TEXT UNIQUE,
                    ModelID INTEGER,
                    Level INTEGER,
                    Exp INTEGER,
                    Gold INTEGER,
                    HP INTEGER,
                    MP INTEGER,
                    X REAL,
                    Y REAL,
                    Z REAL,
                    Region INTEGER,
                    LastUpdated DATETIME DEFAULT CURRENT_TIMESTAMP
                )";

            using (var command = new SqliteCommand(createCharactersTable, _connection))
            {
                command.ExecuteNonQuery();
            }

            string createInventoryTable = @"
                CREATE TABLE IF NOT EXISTS Inventory (
                    Id INTEGER PRIMARY KEY AUTOINCREMENT,
                    CharId INTEGER,
                    ModelID INTEGER,
                    Slot INTEGER,
                    Amount INTEGER,
                    FOREIGN KEY(CharId) REFERENCES Characters(Id)
                )";

            using (var command = new SqliteCommand(createInventoryTable, _connection))
            {
                command.ExecuteNonQuery();
            }

            string createModelsTable = @"
                CREATE TABLE IF NOT EXISTS Models (
                    ID INTEGER PRIMARY KEY,
                    CodeName TEXT,
                    Name TEXT,
                    TypeID1 INTEGER,
                    TypeID2 INTEGER,
                    TypeID3 INTEGER,
                    TypeID4 INTEGER
                )";

            using (var command = new SqliteCommand(createModelsTable, _connection))
            {
                command.ExecuteNonQuery();
            }

            string createSkillsTable = @"
                CREATE TABLE IF NOT EXISTS Skills (
                    ID INTEGER PRIMARY KEY,
                    CodeName TEXT,
                    Name TEXT,
                    Level INTEGER,
                    CastTime INTEGER,
                    Cooldown INTEGER,
                    Range INTEGER,
                    IsSelfOnly INTEGER,
                    MPUsage INTEGER,
                    IconPath TEXT
                )";

            using (var command = new SqliteCommand(createSkillsTable, _connection))
            {
                command.ExecuteNonQuery();
            }

            Console.WriteLine("[Database] Initialized.");
        }

        public void SaveModelsBatch(IEnumerable<DataManager.SRModelInfo> models)
        {
            using (var transaction = _connection.BeginTransaction())
            {
                string sql = @"
                    INSERT OR REPLACE INTO Models (ID, CodeName, Name, TypeID1, TypeID2, TypeID3, TypeID4)
                    VALUES (@id, @codeName, @name, @t1, @t2, @t3, @t4)";

                using (var command = new SqliteCommand(sql, _connection, transaction))
                {
                    var pId = command.Parameters.Add("@id", SqliteType.Integer);
                    var pCode = command.Parameters.Add("@codeName", SqliteType.Text);
                    var pName = command.Parameters.Add("@name", SqliteType.Text);
                    var pT1 = command.Parameters.Add("@t1", SqliteType.Integer);
                    var pT2 = command.Parameters.Add("@t2", SqliteType.Integer);
                    var pT3 = command.Parameters.Add("@t3", SqliteType.Integer);
                    var pT4 = command.Parameters.Add("@t4", SqliteType.Integer);

                    foreach (var m in models)
                    {
                        pId.Value = m.ID;
                        pCode.Value = m.CodeName ?? (object)DBNull.Value;
                        pName.Value = m.Name ?? (object)DBNull.Value;
                        pT1.Value = m.TypeID1;
                        pT2.Value = m.TypeID2;
                        pT3.Value = m.TypeID3;
                        pT4.Value = m.TypeID4;
                        command.ExecuteNonQuery();
                    }
                }
                transaction.Commit();
            }
        }

        public void SaveSkillsBatch(IEnumerable<SRSkill> skills)
        {
            using (var transaction = _connection.BeginTransaction())
            {
                string sql = @"
                    INSERT OR REPLACE INTO Skills (ID, CodeName, Name, Level, CastTime, Cooldown, Range, IsSelfOnly, MPUsage, IconPath)
                    VALUES (@id, @codeName, @name, @lvl, @cast, @cd, @range, @self, @mp, @icon)";

                using (var command = new SqliteCommand(sql, _connection, transaction))
                {
                    var pId = command.Parameters.Add("@id", SqliteType.Integer);
                    var pCode = command.Parameters.Add("@codeName", SqliteType.Text);
                    var pName = command.Parameters.Add("@name", SqliteType.Text);
                    var pLvl = command.Parameters.Add("@lvl", SqliteType.Integer);
                    var pCast = command.Parameters.Add("@cast", SqliteType.Integer);
                    var pCd = command.Parameters.Add("@cd", SqliteType.Integer);
                    var pRange = command.Parameters.Add("@range", SqliteType.Integer);
                    var pSelf = command.Parameters.Add("@self", SqliteType.Integer);
                    var pMp = command.Parameters.Add("@mp", SqliteType.Integer);
                    var pIcon = command.Parameters.Add("@icon", SqliteType.Text);

                    foreach (var s in skills)
                    {
                        pId.Value = s.ID;
                        pCode.Value = s.CodeName ?? (object)DBNull.Value;
                        pName.Value = s.Name ?? (object)DBNull.Value;
                        pLvl.Value = s.Level;
                        pCast.Value = s.CastTime;
                        pCd.Value = s.Cooldown;
                        pRange.Value = s.Range;
                        pSelf.Value = s.IsSelfOnly ? 1 : 0;
                        pMp.Value = s.MPUsage;
                        pIcon.Value = s.IconPath ?? (object)DBNull.Value;
                        command.ExecuteNonQuery();
                    }
                }
                transaction.Commit();
            }
        }

        public void SaveConfig(string key, string value)
        {
            string sql = "INSERT OR REPLACE INTO Configuration (Key, Value) VALUES (@key, @value)";
            using (var command = new SqliteCommand(sql, _connection))
            {
                command.Parameters.AddWithValue("@key", key);
                command.Parameters.AddWithValue("@value", value);
                command.ExecuteNonQuery();
            }
        }

        public string GetConfig(string key)
        {
            string sql = "SELECT Value FROM Configuration WHERE Key = @key";
            using (var command = new SqliteCommand(sql, _connection))
            {
                command.Parameters.AddWithValue("@key", key);
                var result = command.ExecuteScalar();
                return result?.ToString();
            }
        }

        public void UpdateCharacterState(SRCharacter character)
        {
            if (character == null) return;

            string sql = @"
                INSERT OR REPLACE INTO Characters (Name, ModelID, Level, Exp, Gold, HP, MP, X, Y, Z, Region, LastUpdated)
                VALUES (@name, @modelId, @level, @exp, @gold, @hp, @mp, @x, @y, @z, @region, CURRENT_TIMESTAMP)";

            using (var command = new SqliteCommand(sql, _connection))
            {
                command.Parameters.AddWithValue("@name", character.Name ?? "Unknown");
                command.Parameters.AddWithValue("@modelId", character.ModelID);
                command.Parameters.AddWithValue("@level", character.Level);
                command.Parameters.AddWithValue("@exp", (long)character.Exp);
                command.Parameters.AddWithValue("@gold", (long)character.Gold);
                command.Parameters.AddWithValue("@hp", character.HP);
                command.Parameters.AddWithValue("@mp", character.MP);
                command.Parameters.AddWithValue("@x", character.Position.X);
                command.Parameters.AddWithValue("@y", character.Position.Y);
                command.Parameters.AddWithValue("@z", character.Position.Z);
                command.Parameters.AddWithValue("@region", character.Position.Region);
                command.ExecuteNonQuery();
            }

            // Get the ID of the character for inventory saving
            long charId = GetCharacterId(character.Name);
            if (charId != -1)
            {
                SaveInventory(charId, character.Inventory);
            }
        }

        public long GetCharacterId(string name)
        {
            string sql = "SELECT Id FROM Characters WHERE Name = @name";
            using (var command = new SqliteCommand(sql, _connection))
            {
                command.Parameters.AddWithValue("@name", name);
                var result = command.ExecuteScalar();
                return result != null ? (long)result : -1;
            }
        }

        public void SaveInventory(long charId, List<SRItem> items)
        {
            if (items == null) return;

            // Delete old inventory for this character
            string deleteSql = "DELETE FROM Inventory WHERE CharId = @charId";
            using (var command = new SqliteCommand(deleteSql, _connection))
            {
                command.Parameters.AddWithValue("@charId", charId);
                command.ExecuteNonQuery();
            }

            // Insert current inventory
            string insertSql = "INSERT INTO Inventory (CharId, ModelID, Slot, Amount) VALUES (@charId, @modelId, @slot, @amount)";
            foreach (var item in items)
            {
                using (var command = new SqliteCommand(insertSql, _connection))
                {
                    command.Parameters.AddWithValue("@charId", charId);
                    command.Parameters.AddWithValue("@modelId", item.ModelID);
                    command.Parameters.AddWithValue("@slot", item.Slot);
                    command.Parameters.AddWithValue("@amount", item.Amount);
                    command.ExecuteNonQuery();
                }
            }
        }

        public void Dispose()
        {
            _connection?.Close();
            _connection?.Dispose();
        }
    }
}
#endregion


#region DataManager
namespace SilkroadAIBot.Data
{
    public class DataManager
    {
        private Pk2Stream? _mediaPk2;
        private Pk2Stream? _dataPk2;
        private Pk2Stream? _mapPk2;
        
        public NavmeshReader? Navmesh { get; private set; }
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

        public bool Initialize(string sroPath)
        {
            string mediaPath = Path.Combine(sroPath, "Media.pk2");
            string mapPath = Path.Combine(sroPath, "Map.pk2");
            string dataPath = Path.Combine(sroPath, "Data.pk2");

            try 
            {
                if (!File.Exists(mediaPath))
                {
                    LogService.Error($"Media.pk2 not found at: {mediaPath}");
                    return false;
                }

                _mediaPk2 = new Pk2Stream(mediaPath, "169841", true);
                LogService.Info($"[DataManager] Media.pk2 loaded and indexed.");

                if (File.Exists(dataPath))
                {
                    _dataPk2 = new Pk2Stream(dataPath, "169841", true);
                    LogService.Info($"[DataManager] Data.pk2 loaded and indexed.");
                }
                else
                {
                    LogService.Warning($"[DataManager] Data.pk2 not found. Object/Skill data might be missing.");
                }

                if (File.Exists(mapPath))
                {
                    _mapPk2 = new Pk2Stream(mapPath, "169841", true);
                    Navmesh = new NavmeshReader(_mapPk2);
                    LogService.Info($"[DataManager] Map.pk2 loaded and NavmeshReader initialized.");
                }
                else
                {
                    LogService.Warning($"[DataManager] Map.pk2 not found. Navmesh features disabled.");
                }

                return true;
            }
            catch (Exception ex)
            {
                LogService.Error($"PK2 Load Error: {ex.ToString()}"); // Log full stack trace
                return false; 
            }
        }

        public bool AutoDiscoverServerConfig(out string ip, out int port)
        {
            ip = "";
            port = 15779;

            if (_mediaPk2 == null) return false;

            try
            {
                // 1. Division Info
                var divData = _mediaPk2.GetFile("divisioninfo.txt")?.GetContent();
                if (divData != null)
                {
                    using (var reader = new BinaryReader(new MemoryStream(divData)))
                    {
                        reader.ReadByte(); // Unknown byte (often 0x16 or similar)
                        byte count = reader.ReadByte();
                        for (int i = 0; i < count; i++)
                        {
                            int nameLen = reader.ReadInt32();
                            string divName = Encoding.ASCII.GetString(reader.ReadBytes(nameLen));
                            reader.ReadByte(); // Null terminator
                            
                            byte divCount = reader.ReadByte();
                            for (int j = 0; j < divCount; j++)
                            {
                                int len = reader.ReadInt32();
                                if (len > 0)
                                {
                                    string fetchedIp = Encoding.ASCII.GetString(reader.ReadBytes(len));
                                    reader.ReadByte(); // Null terminator
                                    
                                    // Initialize with the first found IP
                                    if (string.IsNullOrEmpty(ip) && !string.IsNullOrEmpty(fetchedIp))
                                    {
                                        ip = fetchedIp.TrimEnd('\0');
                                    }
                                }
                            }
                        }
                    }
                }

                // 2. Gateway Port
                var portData = _mediaPk2.GetFile("gateport.txt")?.GetContent();
                if (portData != null)
                {
                    string portStr = Encoding.ASCII.GetString(portData).Trim();
                    if (int.TryParse(portStr, out int p)) port = p;
                }

                if (!string.IsNullOrEmpty(ip))
                {
                    LogService.Info($"[DataManager] Discovered Server IP: {ip}:{port}");
                    LogService.Info($"[DataManager] EXACT EXTRACTED IP: '{ip}'");
                    
                    // Update global config with the discovered IP and Port
                    ConfigManager.Config.OriginalServerIp = ip;
                    ConfigManager.Config.LastServerPort = port;
                    ConfigManager.Save();
                    
                    return true;
                }
                
                return false;
            }
            catch (Exception ex)
            {
                LogService.Error($"[DataManager] Error during server discovery: {ex.Message}");
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
            var fileData = _mediaPk2.GetFile("server_dep\\silkroad\\textdata\\textdata_object.txt")?.GetContent();
            if (fileData == null) fileData = _mediaPk2.GetFile("server_dep/silkroad/textdata/textdata_object.txt")?.GetContent();
            
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
                    byte[]? data = _dataPk2.GetFile(filePath)?.GetContent();
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
            _mediaPk2?.Dispose();
            _dataPk2?.Dispose();
            _mapPk2?.Dispose();
        }

        private void LoadSkillData()
        {
            if (_dataPk2 == null) return;

            foreach (var filePath in _dataPk2.GetFileNames())
            {
                string fileName = Path.GetFileName(filePath);
                if (fileName.StartsWith("skilldata_") && fileName.EndsWith(".txt"))
                {
                    byte[]? data = _dataPk2.GetFile(filePath)?.GetContent();
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
#endregion


#region NavmeshReader
namespace SilkroadAIBot.Data
{
    public class NavmeshReader
    {
        private Pk2Stream _mapPk2;
        private Dictionary<ushort, float[,]> _heightCache = new Dictionary<ushort, float[,]>();
        
        // SRO Sector Constants
        private const int SECTOR_SIZE = 1920; // 192.0 units per sector? Or 100? Varies.
                                              // Actually standard is 256x256 units but split into blocks? 
                                              // Let's stick to reading the BMS file structure.
                                              
        public NavmeshReader(Pk2Stream mapPk2)
        {
            _mapPk2 = mapPk2;
        }

        public float GetHeight(ushort region, float x, float y)
        {
            // 1. Check cache
            if (!_heightCache.TryGetValue(region, out var heightMap))
            {
                heightMap = LoadSector(region);
                if (heightMap != null)
                {
                    _heightCache[region] = heightMap;
                }
            }

            if (heightMap == null) return 0.0f; // Default ground

            // SRO standard region size is 1920.0 units.
            // SRO heightmap grid is usually 96x96 tiles (starting at 0,0).
            // This means there are 97 vertices in each dimension.
            // Grid cell size = 1920 / 96 = 20.0 units.

            // x and y within the region [0, 1920).
            // In SRO, "y" in 2D is often the North/South axis, which maps to array Z or Y?
            // Usually array[x, y].
            
            float cellX = x / 20.0f;
            float cellY = y / 20.0f;
            
            int x1 = (int)cellX;
            int y1 = (int)cellY;
            
            // Clamp to valid grid range [0, 96]
            // Note: If x is 1920, x1=96. We need vertices x1 and x1+1.
            // If we only have 97 vertices (0..96), max index is 96.
            
            if (x1 < 0) x1 = 0; if (x1 > 95) x1 = 95;
            if (y1 < 0) y1 = 0; if (y1 > 95) y1 = 95;

            // Simple nearest/floor for now
            return heightMap[x1, y1];
            
            // TODO: Implement Bilinear Interpolation for smoother movement
            // float dx = cellX - x1;
            // float dy = cellY - y1;
            // ...
        }

        private float[,]? LoadSector(ushort region)
        {
            try 
            {
                byte xSector = (byte)(region & 0xFF);
                byte ySector = (byte)((region >> 8) & 0xFF);
                
                string path = $"Map/{xSector}/{ySector}.bms";
                var data = _mapPk2.GetFile(path)?.GetContent();
                
                if (data == null) 
                {
                    // Attempt "Data/Map" prefix just in case structure differs
                    path = $"Data/Map/{xSector}/{ySector}.bms";
                    data = _mapPk2.GetFile(path)?.GetContent();
                    if (data == null) return null;
                }

                using (var reader = new BinaryReader(new MemoryStream(data)))
                {
                    // Basic BMS Header Parsing
                    // 0x00: Version (4)
                    // 0x04: Map Type/Attr (4)
                    // 0x08: Unknown (4)
                    // 0x0C: Unknown (4)
                    // 0x10: Start of Height Data (97x97 floats) or different based on version
                    
                    int version = reader.ReadInt32();
                    reader.ReadInt32(); 
                    reader.ReadInt32(); 
                    reader.ReadInt32();

                    // Read 97x97 floats
                    // 97 * 97 * 4 bytes = 37636 bytes. 
                    // File size check?
                    
                    float[,] map = new float[97, 97];
                    for (int x = 0; x < 97; x++)
                    {
                         for (int y = 0; y < 97; y++)
                         {
                             map[x, y] = reader.ReadSingle(); 
                         }
                    }
                    return map;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[NavmeshReader] Error loading sector {region}: {ex.Message}");
                return null;
            }
        }
        
        private string GetBmsPath(ushort region)
        {
             // Not used directly logic moved to LoadSector
             return "";
        }
    }
}


// https://www.elitepvpers.com/forum/sro-coding-corner/1063078-c-silkroadsecurity.html
#endregion


#region Blowfish
namespace SilkroadAIBot.Data.PK2API.Security
{
    public class Blowfish
	{
		private static uint[] bf_P =
		{
			0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344,
			0xa4093822, 0x299f31d0, 0x082efa98, 0xec4e6c89,
			0x452821e6, 0x38d01377, 0xbe5466cf, 0x34e90c6c,
			0xc0ac29b7, 0xc97c50dd, 0x3f84d5b5, 0xb5470917,
			0x9216d5d9, 0x8979fb1b,
		};

		private static uint[,] bf_S = 
		{
			{
				0xd1310ba6, 0x98dfb5ac, 0x2ffd72db, 0xd01adfb7, 0xb8e1afed, 0x6a267e96, 0xba7c9045, 0xf12c7f99,
				0x24a19947, 0xb3916cf7, 0x0801f2e2, 0x858efc16, 0x636920d8, 0x71574e69, 0xa458fea3, 0xf4933d7e,
				0x0d95748f, 0x728eb658, 0x718bcd58, 0x82154aee, 0x7b54a41d, 0xc25a59b5, 0x9c30d539, 0x2af26013,
				0xc5d1b023, 0x286085f0, 0xca417918, 0xb8db38ef, 0x8e79dcb0, 0x603a180e, 0x6c9e0e8b, 0xb01e8a3e,
				0xd71577c1, 0xbd314b27, 0x78af2fda, 0x55605c60, 0xe65525f3, 0xaa55ab94, 0x57489862, 0x63e81440,
				0x55ca396a, 0x2aab10b6, 0xb4cc5c34, 0x1141e8ce, 0xa15486af, 0x7c72e993, 0xb3ee1411, 0x636fbc2a,
				0x2ba9c55d, 0x741831f6, 0xce5c3e16, 0x9b87931e, 0xafd6ba33, 0x6c24cf5c, 0x7a325381, 0x28958677,
				0x3b8f4898, 0x6b4bb9af, 0xc4bfe81b, 0x66282193, 0x61d809cc, 0xfb21a991, 0x487cac60, 0x5dec8032,
				0xef845d5d, 0xe98575b1, 0xdc262302, 0xeb651b88, 0x23893e81, 0xd396acc5, 0x0f6d6ff3, 0x83f44239,
				0x2e0b4482, 0xa4842004, 0x69c8f04a, 0x9e1f9b5e, 0x21c66842, 0xf6e96c9a, 0x670c9c61, 0xabd388f0,
				0x6a51a0d2, 0xd8542f68, 0x960fa728, 0xab5133a3, 0x6eef0b6c, 0x137a3be4, 0xba3bf050, 0x7efb2a98,
				0xa1f1651d, 0x39af0176, 0x66ca593e, 0x82430e88, 0x8cee8619, 0x456f9fb4, 0x7d84a5c3, 0x3b8b5ebe,
				0xe06f75d8, 0x85c12073, 0x401a449f, 0x56c16aa6, 0x4ed3aa62, 0x363f7706, 0x1bfedf72, 0x429b023d,
				0x37d0d724, 0xd00a1248, 0xdb0fead3, 0x49f1c09b, 0x075372c9, 0x80991b7b, 0x25d479d8, 0xf6e8def7,
				0xe3fe501a, 0xb6794c3b, 0x976ce0bd, 0x04c006ba, 0xc1a94fb6, 0x409f60c4, 0x5e5c9ec2, 0x196a2463,
				0x68fb6faf, 0x3e6c53b5, 0x1339b2eb, 0x3b52ec6f, 0x6dfc511f, 0x9b30952c, 0xcc814544, 0xaf5ebd09,
				0xbee3d004, 0xde334afd, 0x660f2807, 0x192e4bb3, 0xc0cba857, 0x45c8740f, 0xd20b5f39, 0xb9d3fbdb,
				0x5579c0bd, 0x1a60320a, 0xd6a100c6, 0x402c7279, 0x679f25fe, 0xfb1fa3cc, 0x8ea5e9f8, 0xdb3222f8,
				0x3c7516df, 0xfd616b15, 0x2f501ec8, 0xad0552ab, 0x323db5fa, 0xfd238760, 0x53317b48, 0x3e00df82,
				0x9e5c57bb, 0xca6f8ca0, 0x1a87562e, 0xdf1769db, 0xd542a8f6, 0x287effc3, 0xac6732c6, 0x8c4f5573,
				0x695b27b0, 0xbbca58c8, 0xe1ffa35d, 0xb8f011a0, 0x10fa3d98, 0xfd2183b8, 0x4afcb56c, 0x2dd1d35b,
				0x9a53e479, 0xb6f84565, 0xd28e49bc, 0x4bfb9790, 0xe1ddf2da, 0xa4cb7e33, 0x62fb1341, 0xcee4c6e8,
				0xef20cada, 0x36774c01, 0xd07e9efe, 0x2bf11fb4, 0x95dbda4d, 0xae909198, 0xeaad8e71, 0x6b93d5a0,
				0xd08ed1d0, 0xafc725e0, 0x8e3c5b2f, 0x8e7594b7, 0x8ff6e2fb, 0xf2122b64, 0x8888b812, 0x900df01c,
				0x4fad5ea0, 0x688fc31c, 0xd1cff191, 0xb3a8c1ad, 0x2f2f2218, 0xbe0e1777, 0xea752dfe, 0x8b021fa1,
				0xe5a0cc0f, 0xb56f74e8, 0x18acf3d6, 0xce89e299, 0xb4a84fe0, 0xfd13e0b7, 0x7cc43b81, 0xd2ada8d9,
				0x165fa266, 0x80957705, 0x93cc7314, 0x211a1477, 0xe6ad2065, 0x77b5fa86, 0xc75442f5, 0xfb9d35cf,
				0xebcdaf0c, 0x7b3e89a0, 0xd6411bd3, 0xae1e7e49, 0x00250e2d, 0x2071b35e, 0x226800bb, 0x57b8e0af,
				0x2464369b, 0xf009b91e, 0x5563911d, 0x59dfa6aa, 0x78c14389, 0xd95a537f, 0x207d5ba2, 0x02e5b9c5,
				0x83260376, 0x6295cfa9, 0x11c81968, 0x4e734a41, 0xb3472dca, 0x7b14a94a, 0x1b510052, 0x9a532915,
				0xd60f573f, 0xbc9bc6e4, 0x2b60a476, 0x81e67400, 0x08ba6fb5, 0x571be91f, 0xf296ec6b, 0x2a0dd915,
				0xb6636521, 0xe7b9f9b6, 0xff34052e, 0xc5855664, 0x53b02d5d, 0xa99f8fa1, 0x08ba4799, 0x6e85076a
			},

			{
				0x4b7a70e9, 0xb5b32944, 0xdb75092e, 0xc4192623, 0xad6ea6b0, 0x49a7df7d, 0x9cee60b8, 0x8fedb266,
				0xecaa8c71, 0x699a17ff, 0x5664526c, 0xc2b19ee1, 0x193602a5, 0x75094c29, 0xa0591340, 0xe4183a3e,
				0x3f54989a, 0x5b429d65, 0x6b8fe4d6, 0x99f73fd6, 0xa1d29c07, 0xefe830f5, 0x4d2d38e6, 0xf0255dc1,
				0x4cdd2086, 0x8470eb26, 0x6382e9c6, 0x021ecc5e, 0x09686b3f, 0x3ebaefc9, 0x3c971814, 0x6b6a70a1,
				0x687f3584, 0x52a0e286, 0xb79c5305, 0xaa500737, 0x3e07841c, 0x7fdeae5c, 0x8e7d44ec, 0x5716f2b8,
				0xb03ada37, 0xf0500c0d, 0xf01c1f04, 0x0200b3ff, 0xae0cf51a, 0x3cb574b2, 0x25837a58, 0xdc0921bd,
				0xd19113f9, 0x7ca92ff6, 0x94324773, 0x22f54701, 0x3ae5e581, 0x37c2dadc, 0xc8b57634, 0x9af3dda7,
				0xa9446146, 0x0fd0030e, 0xecc8c73e, 0xa4751e41, 0xe238cd99, 0x3bea0e2f, 0x3280bba1, 0x183eb331,
				0x4e548b38, 0x4f6db908, 0x6f420d03, 0xf60a04bf, 0x2cb81290, 0x24977c79, 0x5679b072, 0xbcaf89af,
				0xde9a771f, 0xd9930810, 0xb38bae12, 0xdccf3f2e, 0x5512721f, 0x2e6b7124, 0x501adde6, 0x9f84cd87,
				0x7a584718, 0x7408da17, 0xbc9f9abc, 0xe94b7d8c, 0xec7aec3a, 0xdb851dfa, 0x63094366, 0xc464c3d2,
				0xef1c1847, 0x3215d908, 0xdd433b37, 0x24c2ba16, 0x12a14d43, 0x2a65c451, 0x50940002, 0x133ae4dd,
				0x71dff89e, 0x10314e55, 0x81ac77d6, 0x5f11199b, 0x043556f1, 0xd7a3c76b, 0x3c11183b, 0x5924a509,
				0xf28fe6ed, 0x97f1fbfa, 0x9ebabf2c, 0x1e153c6e, 0x86e34570, 0xeae96fb1, 0x860e5e0a, 0x5a3e2ab3,
				0x771fe71c, 0x4e3d06fa, 0x2965dcb9, 0x99e71d0f, 0x803e89d6, 0x5266c825, 0x2e4cc978, 0x9c10b36a,
				0xc6150eba, 0x94e2ea78, 0xa5fc3c53, 0x1e0a2df4, 0xf2f74ea7, 0x361d2b3d, 0x1939260f, 0x19c27960,
				0x5223a708, 0xf71312b6, 0xebadfe6e, 0xeac31f66, 0xe3bc4595, 0xa67bc883, 0xb17f37d1, 0x018cff28,
				0xc332ddef, 0xbe6c5aa5, 0x65582185, 0x68ab9802, 0xeecea50f, 0xdb2f953b, 0x2aef7dad, 0x5b6e2f84,
				0x1521b628, 0x29076170, 0xecdd4775, 0x619f1510, 0x13cca830, 0xeb61bd96, 0x0334fe1e, 0xaa0363cf,
				0xb5735c90, 0x4c70a239, 0xd59e9e0b, 0xcbaade14, 0xeecc86bc, 0x60622ca7, 0x9cab5cab, 0xb2f3846e,
				0x648b1eaf, 0x19bdf0ca, 0xa02369b9, 0x655abb50, 0x40685a32, 0x3c2ab4b3, 0x319ee9d5, 0xc021b8f7,
				0x9b540b19, 0x875fa099, 0x95f7997e, 0x623d7da8, 0xf837889a, 0x97e32d77, 0x11ed935f, 0x16681281,
				0x0e358829, 0xc7e61fd6, 0x96dedfa1, 0x7858ba99, 0x57f584a5, 0x1b227263, 0x9b83c3ff, 0x1ac24696,
				0xcdb30aeb, 0x532e3054, 0x8fd948e4, 0x6dbc3128, 0x58ebf2ef, 0x34c6ffea, 0xfe28ed61, 0xee7c3c73,
				0x5d4a14d9, 0xe864b7e3, 0x42105d14, 0x203e13e0, 0x45eee2b6, 0xa3aaabea, 0xdb6c4f15, 0xfacb4fd0,
				0xc742f442, 0xef6abbb5, 0x654f3b1d, 0x41cd2105, 0xd81e799e, 0x86854dc7, 0xe44b476a, 0x3d816250,
				0xcf62a1f2, 0x5b8d2646, 0xfc8883a0, 0xc1c7b6a3, 0x7f1524c3, 0x69cb7492, 0x47848a0b, 0x5692b285,
				0x095bbf00, 0xad19489d, 0x1462b174, 0x23820e00, 0x58428d2a, 0x0c55f5ea, 0x1dadf43e, 0x233f7061,
				0x3372f092, 0x8d937e41, 0xd65fecf1, 0x6c223bdb, 0x7cde3759, 0xcbee7460, 0x4085f2a7, 0xce77326e,
				0xa6078084, 0x19f8509e, 0xe8efd855, 0x61d99735, 0xa969a7aa, 0xc50c06c2, 0x5a04abfc, 0x800bcadc,
				0x9e447a2e, 0xc3453484, 0xfdd56705, 0x0e1e9ec9, 0xdb73dbd3, 0x105588cd, 0x675fda79, 0xe3674340,
				0xc5c43465, 0x713e38d8, 0x3d28f89e, 0xf16dff20, 0x153e21e7, 0x8fb03d4a, 0xe6e39f2b, 0xdb83adf7
			},

			{
				0xe93d5a68, 0x948140f7, 0xf64c261c, 0x94692934, 0x411520f7, 0x7602d4f7, 0xbcf46b2e, 0xd4a20068,
				0xd4082471, 0x3320f46a, 0x43b7d4b7, 0x500061af, 0x1e39f62e, 0x97244546, 0x14214f74, 0xbf8b8840,
				0x4d95fc1d, 0x96b591af, 0x70f4ddd3, 0x66a02f45, 0xbfbc09ec, 0x03bd9785, 0x7fac6dd0, 0x31cb8504,
				0x96eb27b3, 0x55fd3941, 0xda2547e6, 0xabca0a9a, 0x28507825, 0x530429f4, 0x0a2c86da, 0xe9b66dfb,
				0x68dc1462, 0xd7486900, 0x680ec0a4, 0x27a18dee, 0x4f3ffea2, 0xe887ad8c, 0xb58ce006, 0x7af4d6b6,
				0xaace1e7c, 0xd3375fec, 0xce78a399, 0x406b2a42, 0x20fe9e35, 0xd9f385b9, 0xee39d7ab, 0x3b124e8b,
				0x1dc9faf7, 0x4b6d1856, 0x26a36631, 0xeae397b2, 0x3a6efa74, 0xdd5b4332, 0x6841e7f7, 0xca7820fb,
				0xfb0af54e, 0xd8feb397, 0x454056ac, 0xba489527, 0x55533a3a, 0x20838d87, 0xfe6ba9b7, 0xd096954b,
				0x55a867bc, 0xa1159a58, 0xcca92963, 0x99e1db33, 0xa62a4a56, 0x3f3125f9, 0x5ef47e1c, 0x9029317c,
				0xfdf8e802, 0x04272f70, 0x80bb155c, 0x05282ce3, 0x95c11548, 0xe4c66d22, 0x48c1133f, 0xc70f86dc,
				0x07f9c9ee, 0x41041f0f, 0x404779a4, 0x5d886e17, 0x325f51eb, 0xd59bc0d1, 0xf2bcc18f, 0x41113564,
				0x257b7834, 0x602a9c60, 0xdff8e8a3, 0x1f636c1b, 0x0e12b4c2, 0x02e1329e, 0xaf664fd1, 0xcad18115,
				0x6b2395e0, 0x333e92e1, 0x3b240b62, 0xeebeb922, 0x85b2a20e, 0xe6ba0d99, 0xde720c8c, 0x2da2f728,
				0xd0127845, 0x95b794fd, 0x647d0862, 0xe7ccf5f0, 0x5449a36f, 0x877d48fa, 0xc39dfd27, 0xf33e8d1e,
				0x0a476341, 0x992eff74, 0x3a6f6eab, 0xf4f8fd37, 0xa812dc60, 0xa1ebddf8, 0x991be14c, 0xdb6e6b0d,
				0xc67b5510, 0x6d672c37, 0x2765d43b, 0xdcd0e804, 0xf1290dc7, 0xcc00ffa3, 0xb5390f92, 0x690fed0b,
				0x667b9ffb, 0xcedb7d9c, 0xa091cf0b, 0xd9155ea3, 0xbb132f88, 0x515bad24, 0x7b9479bf, 0x763bd6eb,
				0x37392eb3, 0xcc115979, 0x8026e297, 0xf42e312d, 0x6842ada7, 0xc66a2b3b, 0x12754ccc, 0x782ef11c,
				0x6a124237, 0xb79251e7, 0x06a1bbe6, 0x4bfb6350, 0x1a6b1018, 0x11caedfa, 0x3d25bdd8, 0xe2e1c3c9,
				0x44421659, 0x0a121386, 0xd90cec6e, 0xd5abea2a, 0x64af674e, 0xda86a85f, 0xbebfe988, 0x64e4c3fe,
				0x9dbc8057, 0xf0f7c086, 0x60787bf8, 0x6003604d, 0xd1fd8346, 0xf6381fb0, 0x7745ae04, 0xd736fccc,
				0x83426b33, 0xf01eab71, 0xb0804187, 0x3c005e5f, 0x77a057be, 0xbde8ae24, 0x55464299, 0xbf582e61,
				0x4e58f48f, 0xf2ddfda2, 0xf474ef38, 0x8789bdc2, 0x5366f9c3, 0xc8b38e74, 0xb475f255, 0x46fcd9b9,
				0x7aeb2661, 0x8b1ddf84, 0x846a0e79, 0x915f95e2, 0x466e598e, 0x20b45770, 0x8cd55591, 0xc902de4c,
				0xb90bace1, 0xbb8205d0, 0x11a86248, 0x7574a99e, 0xb77f19b6, 0xe0a9dc09, 0x662d09a1, 0xc4324633,
				0xe85a1f02, 0x09f0be8c, 0x4a99a025, 0x1d6efe10, 0x1ab93d1d, 0x0ba5a4df, 0xa186f20f, 0x2868f169,
				0xdcb7da83, 0x573906fe, 0xa1e2ce9b, 0x4fcd7f52, 0x50115e01, 0xa70683fa, 0xa002b5c4, 0x0de6d027,
				0x9af88c27, 0x773f8641, 0xc3604c06, 0x61a806b5, 0xf0177a28, 0xc0f586e0, 0x006058aa, 0x30dc7d62,
				0x11e69ed7, 0x2338ea63, 0x53c2dd94, 0xc2c21634, 0xbbcbee56, 0x90bcb6de, 0xebfc7da1, 0xce591d76,
				0x6f05e409, 0x4b7c0188, 0x39720a3d, 0x7c927c24, 0x86e3725f, 0x724d9db9, 0x1ac15bb4, 0xd39eb8fc,
				0xed545578, 0x08fca5b5, 0xd83d7cd3, 0x4dad0fc4, 0x1e50ef5e, 0xb161e6f8, 0xa28514d9, 0x6c51133c,
				0x6fd5c7e7, 0x56e14ec4, 0x362abfce, 0xddc6c837, 0xd79a3234, 0x92638212, 0x670efa8e, 0x406000e0
			},

			{
				0x3a39ce37, 0xd3faf5cf, 0xabc27737, 0x5ac52d1b, 0x5cb0679e, 0x4fa33742, 0xd3822740, 0x99bc9bbe,
				0xd5118e9d, 0xbf0f7315, 0xd62d1c7e, 0xc700c47b, 0xb78c1b6b, 0x21a19045, 0xb26eb1be, 0x6a366eb4,
				0x5748ab2f, 0xbc946e79, 0xc6a376d2, 0x6549c2c8, 0x530ff8ee, 0x468dde7d, 0xd5730a1d, 0x4cd04dc6,
				0x2939bbdb, 0xa9ba4650, 0xac9526e8, 0xbe5ee304, 0xa1fad5f0, 0x6a2d519a, 0x63ef8ce2, 0x9a86ee22,
				0xc089c2b8, 0x43242ef6, 0xa51e03aa, 0x9cf2d0a4, 0x83c061ba, 0x9be96a4d, 0x8fe51550, 0xba645bd6,
				0x2826a2f9, 0xa73a3ae1, 0x4ba99586, 0xef5562e9, 0xc72fefd3, 0xf752f7da, 0x3f046f69, 0x77fa0a59,
				0x80e4a915, 0x87b08601, 0x9b09e6ad, 0x3b3ee593, 0xe990fd5a, 0x9e34d797, 0x2cf0b7d9, 0x022b8b51,
				0x96d5ac3a, 0x017da67d, 0xd1cf3ed6, 0x7c7d2d28, 0x1f9f25cf, 0xadf2b89b, 0x5ad6b472, 0x5a88f54c,
				0xe029ac71, 0xe019a5e6, 0x47b0acfd, 0xed93fa9b, 0xe8d3c48d, 0x283b57cc, 0xf8d56629, 0x79132e28,
				0x785f0191, 0xed756055, 0xf7960e44, 0xe3d35e8c, 0x15056dd4, 0x88f46dba, 0x03a16125, 0x0564f0bd,
				0xc3eb9e15, 0x3c9057a2, 0x97271aec, 0xa93a072a, 0x1b3f6d9b, 0x1e6321f5, 0xf59c66fb, 0x26dcf319,
				0x7533d928, 0xb155fdf5, 0x03563482, 0x8aba3cbb, 0x28517711, 0xc20ad9f8, 0xabcc5167, 0xccad925f,
				0x4de81751, 0x3830dc8e, 0x379d5862, 0x9320f991, 0xea7a90c2, 0xfb3e7bce, 0x5121ce64, 0x774fbe32,
				0xa8b6e37e, 0xc3293d46, 0x48de5369, 0x6413e680, 0xa2ae0810, 0xdd6db224, 0x69852dfd, 0x09072166,
				0xb39a460a, 0x6445c0dd, 0x586cdecf, 0x1c20c8ae, 0x5bbef7dd, 0x1b588d40, 0xccd2017f, 0x6bb4e3bb,
				0xdda26a7e, 0x3a59ff45, 0x3e350a44, 0xbcb4cdd5, 0x72eacea8, 0xfa6484bb, 0x8d6612ae, 0xbf3c6f47,
				0xd29be463, 0x542f5d9e, 0xaec2771b, 0xf64e6370, 0x740e0d8d, 0xe75b1357, 0xf8721671, 0xaf537d5d,
				0x4040cb08, 0x4eb4e2cc, 0x34d2466a, 0x0115af84, 0xe1b00428, 0x95983a1d, 0x06b89fb4, 0xce6ea048,
				0x6f3f3b82, 0x3520ab82, 0x011a1d4b, 0x277227f8, 0x611560b1, 0xe7933fdc, 0xbb3a792b, 0x344525bd,
				0xa08839e1, 0x51ce794b, 0x2f32c9b7, 0xa01fbac9, 0xe01cc87e, 0xbcc7d1f6, 0xcf0111c3, 0xa1e8aac7,
				0x1a908749, 0xd44fbd9a, 0xd0dadecb, 0xd50ada38, 0x0339c32a, 0xc6913667, 0x8df9317c, 0xe0b12b4f,
				0xf79e59b7, 0x43f5bb3a, 0xf2d519ff, 0x27d9459c, 0xbf97222c, 0x15e6fc2a, 0x0f91fc71, 0x9b941525,
				0xfae59361, 0xceb69ceb, 0xc2a86459, 0x12baa8d1, 0xb6c1075e, 0xe3056a0c, 0x10d25065, 0xcb03a442,
				0xe0ec6e0e, 0x1698db3b, 0x4c98a0be, 0x3278e964, 0x9f1f9532, 0xe0d392df, 0xd3a0342b, 0x8971f21e,
				0x1b0a7441, 0x4ba3348c, 0xc5be7120, 0xc37632d8, 0xdf359f8d, 0x9b992f2e, 0xe60b6f47, 0x0fe3f11d,
				0xe54cda54, 0x1edad891, 0xce6279cf, 0xcd3e7e6f, 0x1618b166, 0xfd2c1d05, 0x848fd2c5, 0xf6fb2299,
				0xf523f357, 0xa6327623, 0x93a83531, 0x56cccd02, 0xacf08162, 0x5a75ebb5, 0x6e163697, 0x88d273cc,
				0xde966292, 0x81b949d0, 0x4c50901b, 0x71c65614, 0xe6c6c7bd, 0x327a140a, 0x45e1d006, 0xc3f27b9a,
				0xc9aa53fd, 0x62a80f00, 0xbb25bfe2, 0x35bdd2f6, 0x71126905, 0xb2040222, 0xb6cbcf7c, 0xcd769c2b,
				0x53113ec0, 0x1640e3d3, 0x38abbd60, 0x2547adf0, 0xba38209c, 0xf746ce76, 0x77afa1c5, 0x20756060,
				0x85cbfe4e, 0x8ae88dd8, 0x7aaaf9b0, 0x4cf9aa7e, 0x1948c25c, 0x02fb8a8c, 0x01c36ae4, 0xd6ebe1f9,
				0x90d4f869, 0xa65cdea0, 0x3f09252d, 0xc208e69f, 0xb74e6132, 0xce77e25b, 0x578fdfe3, 0x3ac372e6
			}
		};

		uint[] PArray;
		uint[,] SBoxes;

		public Blowfish()
		{
			PArray = new uint[18];
			SBoxes = new uint[4, 256];
		}

		private uint S(uint x, int i)
		{
			if (i < 0 || i > 3)
			{
				throw (new Exception(string.Format("[Blowfish::S] Invalid i index of [{0}].", i)));
			}

			x >>= (24 - (8 * i));
			x &= 0xFF;

			return SBoxes[i, x];
		}

        private uint bf_F(uint x)
		{
			return (((S(x, 0) + S(x, 1)) ^ S(x, 2)) + S(x, 3));
		}

        private void ROUND(ref uint a, uint b, int n)
		{
			a ^= (bf_F(b) ^ PArray[n]);
		}

        private void Blowfish_encipher(ref uint xl, ref uint xr)
		{
			uint Xl = xl;
			uint Xr = xr;

			Xl ^= PArray[0];
			ROUND(ref Xr, Xl, 1); ROUND(ref Xl, Xr, 2);
			ROUND(ref Xr, Xl, 3); ROUND(ref Xl, Xr, 4);
			ROUND(ref Xr, Xl, 5); ROUND(ref Xl, Xr, 6);
			ROUND(ref Xr, Xl, 7); ROUND(ref Xl, Xr, 8);
			ROUND(ref Xr, Xl, 9); ROUND(ref Xl, Xr, 10);
			ROUND(ref Xr, Xl, 11); ROUND(ref Xl, Xr, 12);
			ROUND(ref Xr, Xl, 13); ROUND(ref Xl, Xr, 14);
			ROUND(ref Xr, Xl, 15); ROUND(ref Xl, Xr, 16);
			Xr ^= PArray[17];

			xr = Xl;
			xl = Xr;
		}

        private void Blowfish_decipher(ref uint xl, ref uint xr)
		{
			uint Xl = xl;
			uint Xr = xr;

			Xl ^= PArray[17];
			ROUND(ref Xr, Xl, 16); ROUND(ref Xl, Xr, 15);
			ROUND(ref Xr, Xl, 14); ROUND(ref Xl, Xr, 13);
			ROUND(ref Xr, Xl, 12); ROUND(ref Xl, Xr, 11);
			ROUND(ref Xr, Xl, 10); ROUND(ref Xl, Xr, 9);
			ROUND(ref Xr, Xl, 8); ROUND(ref Xl, Xr, 7);
			ROUND(ref Xr, Xl, 6); ROUND(ref Xl, Xr, 5);
			ROUND(ref Xr, Xl, 4); ROUND(ref Xl, Xr, 3);
			ROUND(ref Xr, Xl, 2); ROUND(ref Xl, Xr, 1);
			Xr ^= PArray[0];

			xl = Xr;
			xr = Xl;
		}
        /// <summary>
        /// Sets up the blowfish object with this specific key.
        /// </summary>
        public void Initialize(byte[] key_ptr)
        {
            Initialize(key_ptr, 0, key_ptr.Length);
        }
        /// <summary>
        /// Sets up the blowfish object with this specific key.
        /// </summary>
        public void Initialize(byte[] key_ptr, int offset, int length)
		{
			uint i, j;
			uint data, datal, datar;

			for (i = 0; i < 18; ++i)
			{
				PArray[i] = bf_P[i];
			}

			for (i = 0; i < 4; ++i)
			{
				for (j = 0; j < 256; ++j)
				{
					SBoxes[i, j] = bf_S[i, j];
				}
			}

			byte[] temp = new byte[4];
			j = 0;
			for (i = 0; i < 16 + 2; ++i)
			{
				temp[3] = key_ptr[j];
				temp[2] = key_ptr[(j + 1) % length];
				temp[1] = key_ptr[(j + 2) % length];
				temp[0] = key_ptr[(j + 3) % length];
				data = BitConverter.ToUInt32(temp, 0);
				PArray[i] ^= data;
				j = (j + 4) % (uint)length;
			}

			datal = 0;
			datar = 0;

			for (i = 0; i < 16 + 2; i += 2)
			{
				Blowfish_encipher(ref datal, ref datar);
				PArray[i] = datal;
				PArray[i + 1] = datar;
			}

			for (i = 0; i < 4; ++i)
			{
				for (j = 0; j < 256; j += 2)
				{
					Blowfish_encipher(ref datal, ref datar);
					SBoxes[i, j] = datal;
					SBoxes[i, j + 1] = datar;
				}
			}
		}
        /// <summary>
        /// Returns the output length based on the size. This can be used to 
        /// determine how many bytes of output space is needed for data that
        /// is about to be encoded or decoded.
        /// </summary>
        public int GetOutputLength(int length)
		{
			return (length % 8) == 0 ? length : length + (8 - (length % 8));
		}
        /// <summary>
        /// Encodes a stream of data and returns a new array of the encoded data.
        /// Returns null if length is 0.
        /// </summary>
        public byte[] Encode(byte[] stream)
		{
			return Encode(stream, 0, stream.Length);
		}
        /// <summary>
        /// Encodes a stream of data and returns a new array of the encoded data.
        /// Returns null if length is 0.
        /// </summary>
        public byte[] Encode(byte[] stream, int offset, int length)
		{
			if (length == 0)
			{
				return null;
			}

			byte[] workspace = new byte[GetOutputLength(length)];

			Buffer.BlockCopy(stream, offset, workspace, 0, length);
			for (int x = length; x < workspace.Length; ++x)
			{
				workspace[x] = 0;
			}

			for (int x = 0; x < workspace.Length; x += 8)
			{
				uint l = BitConverter.ToUInt32(workspace, x + 0);
				uint r = BitConverter.ToUInt32(workspace, x + 4);
				Blowfish_encipher(ref l, ref r);
				Buffer.BlockCopy(BitConverter.GetBytes(l), 0, workspace, x + 0, 4);
				Buffer.BlockCopy(BitConverter.GetBytes(r), 0, workspace, x + 4, 4);
			}

			return workspace;
		}
        /// <summary>
        /// Decodes a stream of data and returns an array of the decoded data.
        /// Returns null if length is not % 8.
        /// </summary>

        public byte[] Decode(byte[] stream)
		{
			return Decode(stream, 0, stream.Length);
		}
        /// <summary>
        /// Decodes a stream of data and returns an array of the decoded data.
        /// Returns null if length is not % 8.
        /// </summary>
        public byte[] Decode(byte[] stream, int offset, int length)
		{
			if (length % 8 != 0 || length == 0)
			{
				return null;
			}

			byte[] workspace = new byte[length];
			Buffer.BlockCopy(stream, offset, workspace, 0, length);

			for (int x = 0; x < workspace.Length; x += 8)
			{
				uint l = BitConverter.ToUInt32(workspace, x + 0);
				uint r = BitConverter.ToUInt32(workspace, x + 4);
				Blowfish_decipher(ref l, ref r);
				Buffer.BlockCopy(BitConverter.GetBytes(l), 0, workspace, x + 0, 4);
				Buffer.BlockCopy(BitConverter.GetBytes(r), 0, workspace, x + 4, 4);
			}

			return workspace;
		}
	}
}
#endregion


#region BlowfishHelpers
namespace SilkroadAIBot.Data.PK2API.Utility
{
    public static class BlowfishHelpers
    {
        public static void Initialize(this Blowfish Blowfish, string ascii_keyy)
        {
            // Using the default base key from Silkroad Online
            Blowfish.Initialize(ascii_keyy, new byte[] { 0x03, 0xF8, 0xE4, 0x44, 0x88, 0x99, 0x3F, 0x64, 0xFE, 0x35 });
        }
        public static void Initialize(this Blowfish Blowfish, string ascii_key, byte[] base_key)
        {
            byte ascii_key_length = (byte)ascii_key.Length;

            // Max count of 56 key bytes
            if (ascii_key_length > 56)
            {
                ascii_key_length = 56;
            }

            // Get bytes from ascii
            byte[] a_key = Encoding.ASCII.GetBytes(ascii_key);

            // This is the Silkroad base key used in all versions
            byte[] b_key = new byte[56];

            // Copy key to array to keep the b_key at 56 bytes. b_key has to be bigger than a_key
            // to be able to xor every index of a_key.
            Array.ConstrainedCopy(base_key, 0, b_key, 0, base_key.Length);

            // Their key modification algorithm for the final blowfish key
            byte[] bf_key = new byte[ascii_key_length];
            for (byte x = 0; x < ascii_key_length; ++x)
            {
                bf_key[x] = (byte)(a_key[x] ^ b_key[x]);
            }

            Blowfish.Initialize(bf_key);
        }
    }
}
#endregion


#region ByteArrayHelpers
namespace SilkroadAIBot.Data.PK2API.Utility
{
    public static class ByteArrayHelpers
    {
        /// <summary>
        /// Compare values from byte arrays and check if each value seems to be same.
        /// </summary>
        public static bool IsSame(this byte[] objA, byte[] objB)
        {
            // Check if some is null and other is not
            if(objA == null && objB != null || objB == null && objA != null)
                return false;
            // If one is null, both are
            if (objA == null)
                return true;
            // Check lengths
            if (objA.Length != objB.Length)
                return false;
            // Check each value
            for (int i = 0; i < objA.Length; i++)
            {
                if (objA[i] != objB[i])
                    return false;
            }
            return true;
        }
        /// <summary>
        /// Converts a sequence of bytes into a structure type.
        /// </summary>
        public static T FromByteArray<T>(byte[] bytes)
        {
            GCHandle handle = GCHandle.Alloc(bytes, GCHandleType.Pinned);
            T structure = (T)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof(T));
            handle.Free();
            return structure;
        }
        /// <summary>
        /// Converts a structure type into bytes.
        /// </summary>
        public static byte[] ToByteArray<T>(T value)
        {
            int size = Marshal.SizeOf(typeof(T));
            byte[] bytes = new byte[size];
            GCHandle handle = GCHandle.Alloc(bytes, GCHandleType.Pinned);
            Marshal.StructureToPtr(value, handle.AddrOfPinnedObject(), false);
            handle.Free();
            return bytes;
        }

        public static bool Equal(byte[] bytes)
        {
            return true;
        }
    }
}
#endregion


#region FileStreamHelpers
namespace SilkroadAIBot.Data.PK2API.Utility
{
    public static class FileStreamHelpers
    {
        /// <summary>
        /// Reads and convert a sequence from stream into a structure type.
        /// </summary>
        public static T Read<T>(this FileStream stream)
        {
            byte[] bytes = new byte[Marshal.SizeOf(typeof(T))];
            stream.Read(bytes, 0, bytes.Length);
            return ByteArrayHelpers.FromByteArray<T>(bytes);
        }
        /// <summary>
        /// Convert the structure and writes the sequence on stream.
        /// </summary>
        public static void Write<T>(this FileStream stream, T value)
        {
            byte[] bytes = ByteArrayHelpers.ToByteArray(value);
            stream.Write(bytes, 0, bytes.Length);
        }
    }
}

// https://github.com/DummkopfOfHachtenduden/SilkroadDoc/wiki/JMXPACK
#endregion


#region PackFileHeader
namespace SilkroadAIBot.Data.PK2API
{
    [StructLayout(LayoutKind.Sequential, Size = 256)]
    public struct PackFileHeader
    {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 30)]
        public string Signature; //JoyMax File Manager!\n
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
        private byte[] Version; //2.0.0.1
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)]
        private byte[] _Encrypted;
        /// <summary>
        /// Indicates if the file is encrypted.
        /// </summary>
        public bool IsEncrypted
        {
            get => BitConverter.ToBoolean(_Encrypted, 0);
            set => _Encrypted = BitConverter.GetBytes(value);
        }
        /// <summary>
        /// Used to test the blowfish key.
        /// </summary>
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16)]
        public byte[] Checksum;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 205)]
        public byte[] Reserved;
        /// <summary>
        /// Creates a header with default values.
        /// </summary>
        public static PackFileHeader GetDefault()
        {
            var result = new PackFileHeader()
            {
                Signature = "JoyMax File Manager!\n",
                Version = new byte[4] { 2, 0, 0, 1 },
                IsEncrypted = true,
                Checksum = new byte[16],
                Reserved = new byte[205],
            };
            return result;
        }
    }
    [StructLayout(LayoutKind.Sequential, Size = 2560)]
    public struct PackFileBlock
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 20)]
        public PackFileEntry[] Entries;
    }
    [StructLayout(LayoutKind.Sequential, Size = 128)]
    public struct PackFileEntry
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)]
        private byte[] _Type;
        public PackFileEntryType Type
        {
            get => (PackFileEntryType)_Type[0];
            set => _Type = new byte[] { (byte)value };
        }
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 89)]
        public string Name;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
        private byte[] _CreationTime;
        public DateTime CreationTime
        {
            get => DateTime.FromFileTime(BitConverter.ToInt64(_CreationTime.ToArray(), 0));
            set => _CreationTime = BitConverter.GetBytes(value.ToFileTime());
        }
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
        private byte[] _ModificationTime;
        public DateTime ModificationTime
        {
            get => DateTime.FromFileTime(BitConverter.ToInt64(_ModificationTime.ToArray(), 0));
            set => _ModificationTime = BitConverter.GetBytes(value.ToFileTime());
        }
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
        private byte[] _Offset;
        /// <summary>
        /// Depends on <see cref="Type"/>.
        /// <para>If <see cref="PackFileEntryType.Folder"/> : Offset to the directory block.</para>
        /// <para>If <see cref="PackFileEntryType.File"/> : Offset to the file data.</para>
        /// </summary>
        public long Offset
        {
            get => BitConverter.ToInt64(_Offset.ToArray(), 0);
            set => _Offset = BitConverter.GetBytes(value);
        }
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
        private byte[] _Size;
        /// <summary>
        /// File size.
        /// </summary>
        public uint Size
        {
            get => BitConverter.ToUInt32(_Size.ToArray(), 0);
            set => _Size = BitConverter.GetBytes(value);
        }
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
        private byte[] _NextBlock;
        /// <summary>
        /// Offset pointing to the next block from chain.
        /// </summary>
        public long NextBlock
        {
            get => BitConverter.ToInt64(_NextBlock.ToArray(), 0);
            set => _NextBlock = BitConverter.GetBytes(value).ToArray();
        }
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)]
        public byte[] Padding;
        /// <summary>
        /// Creates an entry with default values.
        /// </summary>
        public static PackFileEntry GetDefault()
        {
            return new PackFileEntry()
            {
                Type = PackFileEntryType.Empty,
                Name = string.Empty,
                CreationTime = DateTime.FromFileTime(0),
                ModificationTime = DateTime.FromFileTime(0),
                Offset = 0,
                Size = 0,
                NextBlock = 0,
                Padding = new byte[2]
            };
        }
    }
    public enum PackFileEntryType : byte
    {
        Empty = 0,
        Folder,
        File,
    }
}
#endregion


#region Pk2File
namespace SilkroadAIBot.Data.PK2API
{
    public class Pk2File
    {
        #region Private Members & Public Properties
        private FileStream mFileStream;
        /// <summary>
        /// File name.
        /// </summary>
        public string Name { get; }
        /// <summary>
        /// The folder which contains this file.
        /// </summary>
        public Pk2Folder Parent { get; }
        /// <summary>
        /// Offset from file data on stream.
        /// </summary>
        public long Offset { get; }
        /// <summary>
        /// File data size.
        /// </summary>
        public uint Size { get; }
        #endregion

        #region Constructor
        public Pk2File(string name, Pk2Folder parent, FileStream filestream, long offset, uint size)
        {
            Name = name;
            Parent = parent;
            mFileStream = filestream;
            Offset = offset;
            Size = size;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Get the full path to this file.
        /// </summary>
        public string GetFullPath()
        {
            if (Parent != null)
                return Path.Combine(Parent.GetFullPath(), Name.ToLowerInvariant());
            return Name.ToLowerInvariant();
        }
        /// <summary>
        /// Gets the content in bytes from the file.
        /// </summary>
        public byte[] GetContent()
        {
            mFileStream.Seek(Offset, SeekOrigin.Begin);
            var bytes = new byte[Size];
            mFileStream.Read(bytes, 0, bytes.Length);
            return bytes;
        }
        #endregion
    }
}
#endregion


#region Pk2Folder
namespace SilkroadAIBot.Data.PK2API
{
    public class Pk2Folder
    {
        #region Private Members & Public Properties
        /// <summary>
        /// The folder which contains this folder.
        /// </summary>
        public Pk2Folder Parent;
        /// <summary>
        /// Folder name.
        /// </summary>
        public string Name { get; }
        /// <summary>
        /// Offset from the <see cref="PackFileBlock"/> chain.
        /// </summary>
        public long Offset { get; }
        /// <summary>
        /// All files this folder contains.
        /// </summary>
        public Dictionary<string, Pk2File> Files { get; } = new Dictionary<string, Pk2File>();
        /// <summary>
        /// All subfolders this folder contains.
        /// </summary>
        public Dictionary<string, Pk2Folder> Folders { get; } = new Dictionary<string, Pk2Folder>();
        #endregion

        #region Constructor
        public Pk2Folder(string name, Pk2Folder parent, long offset)
        {
            Name = name;
            Parent = parent;
            Offset = offset;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Get the full path to this folder.
        /// </summary>
        public string GetFullPath()
        {
            if (Parent != null)
                return Path.Combine(Parent.GetFullPath(), Name.ToLowerInvariant());
            return Name.ToLowerInvariant();
        }
        #endregion
    }
}
#endregion


#region Pk2Stream
namespace SilkroadAIBot.Data.PK2API
{
    public class Pk2Stream : IDisposable
    {
        #region Private Members & Public Properties
        private Blowfish mBlowfish = new Blowfish();
        private FileStream mFileStream;
        private PackFileHeader mHeader;
        private Dictionary<string, Pk2Folder> mFolders = new Dictionary<string, Pk2Folder>();
        private Dictionary<string, Pk2File> mFiles = new Dictionary<string, Pk2File>();
        private SortedDictionary<long, long> mDiskAllocations = new SortedDictionary<long, long>();
        #endregion

        #region Constructor
        /// <summary>
        /// Initialize a PK2 file stream which handles all the data required to work with it.
        /// </summary>
        /// <param name="path">Path to the PK2 file.</param>
        /// <param name="key">Blowfish key used for encryption.</param>
        /// <param name="isReadOnly">Check if the stream is gonna be used for read only. Otherwise the file will be created in case does not exists.</param>
        /// <exception cref="AuthenticationException"/>
        /// <exception cref="IOException"/>
        public Pk2Stream(string path, string key, bool isReadOnly = false)
        {
            // Set up blowfish
            mBlowfish.Initialize(key);

            // Check file existence first
            var fileExists = File.Exists(path);
            // Check file setup
            mFileStream = new FileStream(path, isReadOnly ? FileMode.Open : FileMode.OpenOrCreate, isReadOnly ? FileAccess.Read : FileAccess.ReadWrite);
            if (!fileExists)
                CreateBaseStream();

            // Prepare to read the file
            mFileStream.Seek(0, SeekOrigin.Begin);
            mHeader = mFileStream.Read<PackFileHeader>();
            // Track header allocation
            mDiskAllocations[0] = Marshal.SizeOf(typeof(PackFileHeader));

            // Check if the key provided is the right one by comparing the checksum
            var checksum = mBlowfish.Encode(Encoding.UTF8.GetBytes("Joymax Pack File"));
            var comparer = new byte[mHeader.Checksum.Length];
            Array.Copy(checksum, comparer, 3); // Compare first three values generated from encoding
            if (!mHeader.Checksum.IsSame(comparer))
                throw new AuthenticationException("Error on key authentication access to stream");

            // Set root folder
            var root = new Pk2Folder("", null, mFileStream.Position);
            mFolders.Add(root.Name, root);
            // Try to read all the blocks
            try
            {
                // Track default block
                mDiskAllocations[root.Offset] = Marshal.SizeOf(typeof(PackFileBlock));
                InitializeStreamBlock(root.Offset, root);
            }
            catch (Exception ex)
            {
                // The only reason to fail is a wrong blowfish key or differents Pk2 structures
                throw new IOException("Error reading PK2 file!", ex);
            }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Get folder by path.
        /// Returns null if folder is not found.
        /// </summary>
        public Pk2Folder GetFolder(string path)
        {
            if (path == null)
                throw new ArgumentException();
            // Normalize path
            path = path.ToLowerInvariant().Replace("/", Path.DirectorySeparatorChar.ToString());
            // Try to get folder
            if (mFolders.TryGetValue(path, out var folder))
                return folder;
            return null;
        }
        /// <summary>
        /// Get file by path.
        /// Returns null if file is not found.
        /// </summary>
        public Pk2File GetFile(string path)
        {
            if (string.IsNullOrEmpty(path))
                throw new ArgumentException();
            // Normalize path
            path = path.ToLowerInvariant().Replace("/", Path.DirectorySeparatorChar.ToString());
            // Try to get file
            if (mFiles.TryGetValue(path, out var file))
                return file;
            return null;
        }
        /// <summary>
        /// Add a folder path.
        /// Returns false if cannot be created or the folder already exists.
        /// </summary>
        public bool AddFolder(string path)
        {
            // Check if folder already exists
            if (GetFolder(path) != null)
                return false;
            // Make sure the same path doesn't exists as a file
            if (GetFile(path) != null)
                return false;
            // Normalize path
            path = path.ToLowerInvariant().Replace("/", Path.DirectorySeparatorChar.ToString());

            // Proceed to create the new folder
            // Try to find the closer existing parent folder
            int i;
            Pk2Folder parent = null;
            var paths = path.Split(Path.DirectorySeparatorChar).ToList();
            for (i = 0; i < paths.Count; i++)
            {
                var nearPath = string.Join(Path.DirectorySeparatorChar.ToString(), paths.Take(paths.Count - i));
                if (mFolders.TryGetValue(nearPath, out parent))
                    break;
            }
            // Set root if an existing parent folder is not found
            if (parent == null)
                parent = mFolders[""];

            // Check paths required to fulfill
            var newPaths = paths.Skip(paths.Count - i).ToList();
            CreateFolderBlock(parent.Offset, parent, newPaths);
            return true;
        }
        /// <summary>
        /// Add a file on specified path. If the path does not exists, it is created.
        /// Return success.
        /// </summary>
        /// <exception cref="ArgumentException"/>
        public bool AddFile(string path, byte[] bytes)
        {
            // Make sure the same path doesn't exists as a folder
            if (GetFolder(path) != null)
                return false;
            // Normalize path
            path = path.ToLowerInvariant().Replace("/", Path.DirectorySeparatorChar.ToString());

            // Check if the file already exists to update/overwrite info with the block entry
            var file = GetFile(path);
            if (file != null)
            {
                long fileOffset;
                // Find and update size allocation
                if (bytes.Length <= file.Size)
                {
                    fileOffset = file.Offset;
                }
                else
                {
                    mDiskAllocations.Remove(file.Offset);
                    fileOffset = AllocateSpace(bytes.Length);
                }
                // Find and update block entry
                long blockOffset = file.Parent.Offset;
                while (blockOffset != 0)
                {
                    var block = LoadPackFileBlock(blockOffset);
                    for (int i = 0; i < block.Entries.Length; i++)
                    {
                        if (block.Entries[i].Type == PackFileEntryType.File
                            && block.Entries[i].Offset == file.Offset) // Easier find offset than normalize names
                        {
                            var newFile = new Pk2File(file.Name, file.Parent, mFileStream, fileOffset, (uint)bytes.Length);
                            // Write/Overwrite file data & track it
                            mFileStream.Seek(newFile.Offset, SeekOrigin.Begin);
                            mFileStream.Write(bytes, 0, bytes.Length);
                            mFileStream.Flush();
                            mDiskAllocations[newFile.Offset] = newFile.Size;
                            // Update entry
                            var timeNow = DateTime.Now;
                            block.Entries[i].ModificationTime = timeNow;
                            block.Entries[i].Size = newFile.Size;
                            block.Entries[i].Offset = newFile.Offset;
                            UpdatePackFileBlock(blockOffset, block);
                            // File added success
                            file.Parent.Files[newFile.Name] = newFile;
                            mFiles[path] = newFile;
                            return true;
                        }
                    }
                    // Continue reading chain until find it
                    blockOffset = block.Entries.Last().NextBlock;
                }
                // Entry was not able to be found
                return false;
            }
            else
            {
                // Create folders required to allocate the file
                var folderName = Path.GetDirectoryName(path);
                if (folderName != "")
                    AddFolder(folderName);
                var folder = GetFolder(folderName);
                // Load and update block with the new file
                long blockOffset = folder.Offset;
                while (blockOffset != 0)
                {
                    var block = LoadPackFileBlock(blockOffset);
                    for (int i = 0; i < block.Entries.Length; i++)
                    {
                        if (block.Entries[i].Type == PackFileEntryType.Empty)
                        {
                            var newFile = new Pk2File(Path.GetFileName(path), folder, mFileStream, AllocateSpace(bytes.Length), (uint)bytes.Length);
                            // Write file data & track it
                            mFileStream.Seek(newFile.Offset, SeekOrigin.Begin);
                            mFileStream.Write(bytes, 0, bytes.Length);
                            mFileStream.Flush();
                            mDiskAllocations[newFile.Offset] = newFile.Size;
                            // Update entry
                            block.Entries[i].Type = PackFileEntryType.File;
                            block.Entries[i].Name = newFile.Name;
                            var timeNow = DateTime.Now;
                            block.Entries[i].CreationTime = timeNow;
                            block.Entries[i].ModificationTime = timeNow;
                            block.Entries[i].Size = newFile.Size;
                            block.Entries[i].Offset = newFile.Offset;
                            UpdatePackFileBlock(blockOffset, block);
                            // File added success
                            folder.Files[newFile.Name] = newFile;
                            mFiles[path] = newFile;
                            return true;
                        }
                    }
                    // Continue reading next block or expand it
                    var nextBlock = block.Entries.Last().NextBlock;
                    // Expand chain if necessary
                    blockOffset = nextBlock == 0 ? ExpandPackFileBlock(blockOffset, block) : nextBlock;
                }
                // Entry was not able to be found
                return false;
            }
        }
        /// <summary>
        /// Removes a folder.
        /// Returns success.
        /// </summary>
        /// <exception cref="ArgumentException"/>
        public bool RemoveFolder(string path)
        {
            if (path == "")
                throw new ArgumentException("Root folder cannot be removed!");
            // Folder has to exists
            var folder = GetFolder(path);
            if (folder == null)
                return false;
            // Remove all links to the folders and files
            RemoveFolderLinks(folder);
            // Remove entry from parent block only so it breaks all the sub blocks
            var blockOffset = folder.Parent.Offset;
            while (blockOffset != 0)
            {
                var block = LoadPackFileBlock(blockOffset);
                for (int i = 0; i < block.Entries.Length; i++)
                {
                    if (block.Entries[i].Type == PackFileEntryType.Folder
                        && block.Entries[i].Offset == folder.Offset) // Easier find offset than normalize names
                    {
                        // Reset entry
                        block.Entries[i] = PackFileEntry.GetDefault();
                        // Update block
                        UpdatePackFileBlock(blockOffset, block);
                        // Remove from parent
                        folder.Parent.Folders.Remove(folder.Name);
                        return true;
                    }
                }
                // Continue reading chain until find it
                blockOffset = block.Entries.Last().NextBlock;
            }
            // Entry was not able to be found
            return false;
        }
        /// <summary>
        /// Removes a file.
        /// Returns success.
        /// </summary
        public bool RemoveFile(string path)
        {
            // File has to exists
            var file = GetFile(path);
            if (file == null)
                return false;
            // Normalize path
            path = path.ToLowerInvariant().Replace("/", Path.DirectorySeparatorChar.ToString());

            // Load and update block which contains the entry to this file
            var blockOffset = file.Parent.Offset;
            while (blockOffset != 0)
            {
                var block = LoadPackFileBlock(blockOffset);
                for (int i = 0; i < block.Entries.Length; i++)
                {
                    if (block.Entries[i].Type == PackFileEntryType.File
                        && block.Entries[i].Offset == file.Offset) // Easier find offset than normalize names
                    {
                        // Reset entry
                        block.Entries[i] = PackFileEntry.GetDefault();
                        // Update block
                        UpdatePackFileBlock(blockOffset, block);
                        // Remove from parent folder
                        file.Parent.Files.Remove(file.Name);
                        // Remove from dictionary
                        mFiles.Remove(path);
                        return true;
                    }
                }
                // Continue reading chain until find it
                blockOffset = block.Entries.Last().NextBlock;
            }
            // Entry was not able to be found
            return false;
        }
        /// <summary>
        /// Close the stream and free the resources.
        /// </summary>
        public void Dispose() => mFileStream?.Dispose();
        #endregion

        #region Private Helpers
        /// <summary>
        /// Creates the base from Pk2 file to work.
        /// </summary>
        private void CreateBaseStream()
        {
            // Header
            var header = PackFileHeader.GetDefault();
            var checksum = mBlowfish.Encode(Encoding.UTF8.GetBytes("Joymax Pack File"));
            Array.Copy(checksum, header.Checksum, 3);
            mFileStream.Write(header);
            mFileStream.Flush();
            // Setup block chain from root folder
            var offset = Marshal.SizeOf(typeof(PackFileHeader));
            PackFileBlock block = new PackFileBlock { Entries = new PackFileEntry[20] };
            for (int i = 0; i < block.Entries.Length; i++)
            {
                block.Entries[i] = PackFileEntry.GetDefault();
                if (i == 0)
                {
                    // Initialize root pointer
                    block.Entries[i].Type = PackFileEntryType.Folder;
                    block.Entries[i].Name = ".";
                    var timeNow = DateTime.Now;
                    block.Entries[i].CreationTime = timeNow;
                    block.Entries[i].ModificationTime = timeNow;
                    block.Entries[i].Offset = offset;
                }
            }
            UpdatePackFileBlock(offset, block);
            // Fill left bytes to match chunk size (4096)
            var bytes = new byte[4096 - mFileStream.Length];
            mFileStream.Write(bytes, 0, bytes.Length);
            mFileStream.Flush();
        }
        /// <summary>
        /// Initialize the block chains reading the minimal information to use the stream.
        /// </summary>
        private void InitializeStreamBlock(long offset, Pk2Folder parent)
        {
            var block = LoadPackFileBlock(offset);
            // Check all entries
            foreach (var entry in block.Entries)
            {
                switch (entry.Type)
                {
                    case PackFileEntryType.Empty:
                        break;
                    case PackFileEntryType.Folder:
                        // Ignore reading pointers to the block itself or parent block
                        if (entry.Name == "." || entry.Name == "..")
                            break;
                        // Add new folder to parent
                        var newFolder = new Pk2Folder(entry.Name, parent, entry.Offset);
                        parent.Folders.Add(entry.Name.ToLowerInvariant(), newFolder);
                        // Keep full path for a quick search
                        mFolders.Add(newFolder.GetFullPath(), newFolder);
                        // Track folder allocation
                        mDiskAllocations[entry.Offset] = Marshal.SizeOf(typeof(PackFileBlock));
                        // Continue reading folder
                        InitializeStreamBlock(entry.Offset, newFolder);
                        break;
                    case PackFileEntryType.File:
                        // Add new file to parent
                        var newFile = new Pk2File(entry.Name, parent, mFileStream, entry.Offset, entry.Size);
                        parent.Files.Add(entry.Name.ToLowerInvariant(), newFile);
                        // Keep full path for a quick search
                        mFiles.Add(newFile.GetFullPath(), newFile);
                        // Track file allocation
                        mDiskAllocations[entry.Offset] = entry.Size;
                        break;
                }
            }
            // Read next block from chain
            var nextBlock = block.Entries.Last().NextBlock;
            if (nextBlock != 0)
            {
                InitializeStreamBlock(nextBlock, parent);
                // Track folder chain allocation
                mDiskAllocations[nextBlock] = Marshal.SizeOf(typeof(PackFileBlock));
            }
        }
        /// <summary>
        /// Load and decode the PackFileBlock from offset
        /// </summary>
        private PackFileBlock LoadPackFileBlock(long offset)
        {
            // Set cursor position
            mFileStream.Seek(offset, SeekOrigin.Begin);
            // Read and decode block
            var bytes = new byte[Marshal.SizeOf(typeof(PackFileBlock))];
            mFileStream.Read(bytes, 0, bytes.Length);
            return ByteArrayHelpers.FromByteArray<PackFileBlock>(mBlowfish.Decode(bytes, 0, bytes.Length));
        }
        /// <summary>
        /// Encode and write a PackFileBlock at offset
        /// </summary>
        private void UpdatePackFileBlock(long offset, PackFileBlock block)
        {
            var bytes = ByteArrayHelpers.ToByteArray(block);
            bytes = mBlowfish.Encode(bytes);
            mFileStream.Seek(offset, SeekOrigin.Begin);
            mFileStream.Write(bytes, 0, bytes.Length);
            mFileStream.Flush();
        }
        /// <summary>
        /// Creates a new block on the block chain given.
        /// Returns the offset to the new block.
        /// </summary>
        private long ExpandPackFileBlock(long offset, PackFileBlock block)
        {
            var newBlockOffset = AllocateSpace(Marshal.SizeOf(typeof(PackFileBlock)));
            // Set up new block on chain
            PackFileBlock newBlock = new PackFileBlock { Entries = new PackFileEntry[block.Entries.Length] };
            for (int j = 0; j < newBlock.Entries.Length; j++)
                newBlock.Entries[j] = PackFileEntry.GetDefault();
            UpdatePackFileBlock(newBlockOffset, newBlock);
            // Track it
            mDiskAllocations[newBlockOffset] = Marshal.SizeOf(typeof(PackFileBlock));
            // Join the new block to the block chain
            block.Entries[block.Entries.Length - 1].NextBlock = newBlockOffset;
            UpdatePackFileBlock(offset, block);
            return newBlockOffset;
        }
        /// <summary>
        /// Search all the data available to allocate space in between otherwise create space at EOF.
        /// Returns the offset which contains the space.
        /// </summary>
        private long AllocateSpace(long Size)
        {
            // Sort all offsets
            var offsets = mDiskAllocations.Keys.ToList();
            for (int i = 0; i < offsets.Count; i++)
            {
                var allocationSize = mDiskAllocations[offsets[i]];
                var nextAllocation = offsets[i] + allocationSize;
                if (!mDiskAllocations.TryGetValue(nextAllocation, out _))
                {
                    // Check space available between this allocation and the next one
                    long availableSize = i + 1 == offsets.Count ? mFileStream.Length - nextAllocation : offsets[i + 1] - nextAllocation;
                    if (availableSize >= Size)
                        return nextAllocation;
                }
            }

            // Creates space at the end if no available space is found
            long allignedDiskSpace = (long)Math.Ceiling(Size / 4096f) * 4096L;
            byte[] bytes = new byte[allignedDiskSpace];
            mFileStream.Seek(0, SeekOrigin.End);
            var pos = mFileStream.Length;
            mFileStream.Write(bytes, 0, bytes.Length);
            mFileStream.Flush();
            return pos;
        }
        private void CreateFolderBlock(long offset, Pk2Folder parentFolder, List<string> paths)
        {
            var block = LoadPackFileBlock(offset);
            // Find an entry available on this block
            for (int i = 0; i < block.Entries.Length; i++)
            {
                if (block.Entries[i].Type == PackFileEntryType.Empty)
                {
                    var newFolder = new Pk2Folder(paths[0], parentFolder, AllocateSpace(Marshal.SizeOf(typeof(PackFileBlock))));
                    // Setup new block from folder
                    var timeNow = DateTime.Now;
                    PackFileBlock newBlock = new PackFileBlock { Entries = new PackFileEntry[block.Entries.Length] };
                    for (int j = 0; j < newBlock.Entries.Length; j++)
                    {
                        newBlock.Entries[j] = PackFileEntry.GetDefault();
                        if (j < 2)
                        {
                            // Initialize the pointers to itself and parent
                            newBlock.Entries[j].Type = PackFileEntryType.Folder;
                            newBlock.Entries[j].Name = j == 0 ? "." : "..";
                            newBlock.Entries[j].CreationTime = timeNow;
                            newBlock.Entries[j].ModificationTime = timeNow;
                            newBlock.Entries[j].Offset = j == 0 ? newFolder.Offset : parentFolder.Offset;
                        }
                    }
                    // Write block data & track it
                    UpdatePackFileBlock(newFolder.Offset, newBlock);
                    mDiskAllocations[newFolder.Offset] = Marshal.SizeOf(typeof(PackFileBlock));
                    // Update entry
                    block.Entries[i].Type = PackFileEntryType.Folder;
                    block.Entries[i].Name = newFolder.Name;
                    block.Entries[i].CreationTime = timeNow;
                    block.Entries[i].ModificationTime = timeNow;
                    block.Entries[i].Offset = newFolder.Offset;
                    UpdatePackFileBlock(offset, block);
                    // Folder added
                    parentFolder.Folders.Add(newFolder.Name, newFolder);
                    mFolders.Add(newFolder.GetFullPath(), newFolder);

                    // Continue creating folder if necessary
                    paths.RemoveAt(0);
                    if (paths.Count > 0)
                        CreateFolderBlock(newFolder.Offset, newFolder, paths);
                    return;
                }
            }

            // Continue reading next block or expand it
            var nextBlock = block.Entries.Last().NextBlock;
            // Expand chain if necessary
            offset = nextBlock == 0 ? ExpandPackFileBlock(offset, block) : nextBlock;
            // Continue reading next block
            CreateFolderBlock(offset, parentFolder, paths);
        }
        /// <summary>
        /// Removes a folder and subfolders with all their links to the stream.
        /// </summary>
        private void RemoveFolderLinks(Pk2Folder folder)
        {
            // Remove subfolders
            foreach (var kv in folder.Folders)
            {
                // Repeat cicle for sub folders
                RemoveFolderLinks(kv.Value);
            }
            // Remove all files
            foreach (var kv in folder.Files)
            {
                // Remove from dictionary
                mFiles.Remove(kv.Value.GetFullPath());
                // Remove space allocation
                mDiskAllocations.Remove(kv.Value.Offset);
            }
            // Remove from dictionary
            mFolders.Remove(folder.GetFullPath());
            // Remove the block space allocation
            mDiskAllocations.Remove(folder.Offset);
        }
        #endregion
    }
}
#endregion

