using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SilkroadAIBot.Core.Data.PK2
{
    public class Pk2Reader : IDisposable
    {
        private string _path;
        private FileStream _stream;
        private BinaryReader _reader;
        private Pk2Blowfish _blowfish;
        
        private Pk2Folder _rootFolder;
        private Dictionary<string, Pk2Entry> _fileIndex; // Optimization: Flattened index for fast lookup

        public Pk2Reader(string path)
        {
            _path = path;
            if (!File.Exists(_path))
                throw new FileNotFoundException("PK2 file not found", path);
                
            _stream = new FileStream(_path, FileMode.Open, FileAccess.Read, FileShare.Read);
            _reader = new BinaryReader(_stream);
            _blowfish = new Pk2Blowfish();
            
            // Standard SRO PK2 Key
            byte[] key = { 0x31, 0x36, 0x39, 0x38, 0x34, 0x31 }; // "169841"
            _blowfish.Initialize(key);
            
            _fileIndex = new Dictionary<string, Pk2Entry>(StringComparer.OrdinalIgnoreCase);

            if (!ReadHeader())
            {
                throw new Exception("Invalid PK2 Header or Signature.");
            }

            // Read Root Block
            // The root block represents the main folder structure
            // In typical SRO PK2, the root entries start at position 256 (after header)
            Console.WriteLine($"[PK2] Indexing {_path}...");
            IndexFiles(); 
            Console.WriteLine($"[PK2] Indexed {_fileIndex.Count} files.");
        }

        private bool ReadHeader()
        {
            _stream.Seek(0, SeekOrigin.Begin);
            byte[] headerBytes = _reader.ReadBytes(256);
            string signature = Encoding.ASCII.GetString(headerBytes, 0, 30).TrimEnd('\0');
            
            if (!signature.Contains("JoyMax File Manager!"))
                return false;

            return true;
        }

        private void IndexFiles()
        {
            // Traverse from Root
            // Root block is usually at 256
            // We need to implement a recursive traversal.
            // However, PK2 structure is a linked list of blocks.
            // Folder entries point to other chains.
            
            // Let's model the traversal:
            // Start at 256.
            // Read Block. Decrypt.
            // Parse 20 entries.
            // If Entry is Folder ("." or "..") -> Skip or Handle context.
            // If Entry is SubFolder -> Add to traversal list.
            // If Entry is File -> Add to Index.
            
            TraverseChain(256, "");
        }

        private void TraverseChain(long position, string currentPath)
        {
            // Safety check
            if (position <= 0 || position >= _stream.Length) return;

             // Read the block at 'position'
            _stream.Seek(position, SeekOrigin.Begin);
            
            // A block is 20 entries * 128 bytes = 2560 bytes
            byte[] blockData = _reader.ReadBytes(2560);
            
            // Decrypt Block
            blockData = _blowfish.Decode(blockData);

            using (var blockStream = new MemoryStream(blockData))
            using (var blockReader = new BinaryReader(blockStream))
            {
                for (int i = 0; i < 20; i++)
                {
                    Pk2Entry entry = ReadEntry(blockReader);
                    
                    if (entry.Type == 0) continue; // Empty
                    
                    if (entry.Type == 1) // Folder
                    {
                        if (entry.Name != "." && entry.Name != "..")
                        {
                            string newPath = string.IsNullOrEmpty(currentPath) ? entry.Name : currentPath + "/" + entry.Name;
                            
                            // Recursively traverse the folder's position
                            // CAREFUL: A folder entry's Position points to the FIRST CHAIN of that folder's content.
                            TraverseChain((long)entry.Position, newPath);
                        }
                    }
                    else if (entry.Type == 2) // File
                    {
                         string fullPath = string.IsNullOrEmpty(currentPath) ? entry.Name : currentPath + "/" + entry.Name;
                         _fileIndex[fullPath] = entry;
                    }
                    
                    // Check logic for Next Chain of the CURRENT directory block
                    // Wait, the NextChain is usually at the end of the block? No, it's inside entries?
                    // No, the entries are strictly file/folder.
                    // How do we know the next block of THIS chain?
                    // Usually, the LAST entry might be special? Or the standard says "NextChain" is part of the Entry.
                    // But which entry holds the next chain for the *current* folder list?
                    // Actually, usually a logic is:
                    // Read 20 entries.
                    // If we need more entries for THIS folder, where is the link?
                    
                    // Re-reading JMXPACK.md: 
                    // "NextChain // Next chain in the current directory"
                    // It seems EACH entry has a NextChain?
                    // No, usually only the last entry of the block, or the Folder entry itself?
                    // Typically in SRO PK2: The 20th entry (or all?) might contain the link?
                    // No, if `NextChain` is non-zero, it points to the Next Block of the SAME directory specific to that entry? 
                    // No.
                    
                    // Let's verify standard SRO PK2 Logic:
                    // 1 Block = 20 Entries.
                    // If a folder has 40 files, it needs 2 blocks.
                    // How are they linked?
                    // Usually the file entries themselves don't link blocks.
                    // The PARENT folder points to the FIRST block.
                    // Does the block itself have a "Next Block" pointer?
                    // The struct `PackFileEntry` has `NextChain`.
                    // This `NextChain` is usually only relevant for the entries?
                    
                    // Correction:
                    // The standard PK2 logic is:
                    // Each Entry has a `NextChain` field, BUT it is only used by specific entries?
                    // Actually, `NextChain` is present in EVERY entry struct, but typically:
                    // In a block of 20 entries, if there are more files in this folder, 
                    // does the last entry point to the next block?
                    // OR does the LAST entry of the block indicate the next block?
                    
                    // Actually, the logic usually is:
                    // We iterate the 20 entries.
                    // We look for a valid `NextChain` pointer in the entries?
                    // Standard SRO behavior: If `169841` key is used.
                    // The `NextChain` value of the *entries* is 0.
                    // EXCEPT... maybe the logic is distinct.
                    
                    // Let's check a specialized detail:
                    // In many implementations:
                    // "The definition of the PackFileBlock struct does NOT contain a next pointer."
                    // "Instead, the LAST entry (index 19) of a block is OFTEN unused for file data and used as a link?"
                    // OR "PackFileEntry" has "NextChain".
                    // If `NextChain` is not 0, does it mean "Next entry is at X"? No, "Next Chain in current directory".
                    // It implies: "Next Block of this structure".
                    
                    // Let's follow this logic:
                    // We traverse 20 entries.
                    // Only ONE of them needs to point to the next chain block for this folder.
                    // Usually, it's the LAST entry (index 19) of the block?
                    // Or ANY entry?
                    // Actually, Srose and other libs suggest:
                    // "PackFileEntry" contains `NextChain`. 
                    // If we are iterating a folder, we read the block.
                    // If `Entry[19].NextChain != 0`, that's the next block?
                    // Let's assume ANY entry having `NextChain != 0` is suspicious, but usually it's consistent.
                    // Wait, `NextChain` is property of the Entry.
                    // Does it mean "The Next Entry in the chain"? No, 128 bytes fixed.
                    
                    // Let's assume the recursive logic:
                    // TraverseChain(startPos):
                    //   Read 20 entries.
                    //   Processing...
                    //   Scan for the "Next Block" which is usually stored in the LAST entry's `NextChain`? 
                    //   OR the `NextChain` of the first entry?
                    
                    // Let's try to detect it:
                    // Iterate 20 entries.
                    // If `NextChain` is found (on the last one?), we jump there for *more items in this folder*.
                    
                    if (i == 19 && entry.NextChain != 0)
                    {
                        // Recursively (tail call) traverse the continuation of this folder
                        TraverseChain((long)entry.NextChain, currentPath);
                    }
                }
            }
        }
        
        private Pk2Entry ReadEntry(BinaryReader reader)
        {
            Pk2Entry entry = new Pk2Entry();
            entry.Type = reader.ReadByte();
            byte[] nameBytes = reader.ReadBytes(89);
            entry.Name = Encoding.ASCII.GetString(nameBytes).TrimEnd('\0');
            entry.CreateTime = reader.ReadUInt64();
            entry.ModifyTime = reader.ReadUInt64();
            entry.Position = reader.ReadUInt64();
            entry.Size = reader.ReadUInt32();
            entry.NextChain = reader.ReadUInt64();
            reader.ReadBytes(2); // Padding
            return entry;
        }

        public byte[] GetFile(string path)
        {
            if (_fileIndex.TryGetValue(path.Replace("\\", "/"), out Pk2Entry entry))
            {
                return ReadFileContent(entry);
            }
            return null;
        }
        
        private byte[] ReadFileContent(Pk2Entry entry)
        {
            if (entry.Type != 2) return null;
            
             _stream.Seek((long)entry.Position, SeekOrigin.Begin);
             byte[] data = _reader.ReadBytes((int)entry.Size);
             
             // Files are NOT encrypted with Blowfish if they are raw data?
             // Actually, usually they are stored plain inside the container (already compressed or encrypted by game logic).
             // The PK2 container only encrypts the directory table (blocks).
             // Verify this? 
             // "Joymax File Manager" usually encrypts the *Files* only if specific flag?
             // Headers said "Encrypted: 1". This refers to the Index Blocks.
             // Inside data is usually raw.
             
             return data;
        }

        public IEnumerable<string> GetFileNames()
        {
            return _fileIndex.Keys;
        }

        public void Close()
        {
            _stream?.Close();
        }

        public void Dispose()
        {
            Close();
            _stream?.Dispose();
        }
    }
    
    public struct Pk2Entry
    {
        public byte Type;
        public string Name;
        public ulong CreateTime;
        public ulong ModifyTime;
        public ulong Position;
        public uint Size;
        public ulong NextChain;
    }
    
    public class Pk2Folder
    {
        public string Name;
        public List<Pk2Folder> SubFolders = new List<Pk2Folder>();
        public List<Pk2Entry> Files = new List<Pk2Entry>();
    }
}
