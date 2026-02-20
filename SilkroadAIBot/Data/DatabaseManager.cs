using System;
using Microsoft.Data.Sqlite;
using System.IO;
using SilkroadAIBot.Models;
using System.Collections.Generic;

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

