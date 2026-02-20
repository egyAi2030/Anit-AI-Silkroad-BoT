using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using SilkroadAIBot.Core.Configuration;

namespace SilkroadAIBot.Core.Helpers
{
    public static class SmartLoader
    {
        // Windows API constants and structs
        private const uint CREATE_SUSPENDED = 0x00000004;
        private const int MEM_COMMIT = 0x00001000;
        private const int PAGE_READWRITE = 0x04;

        [StructLayout(LayoutKind.Sequential)]
        public struct STARTUPINFO
        {
            public int cb;
            public string lpReserved;
            public string lpDesktop;
            public string lpTitle;
            public int dwX;
            public int dwY;
            public int dwXSize;
            public int dwYSize;
            public int dwXCountChars;
            public int dwYCountChars;
            public int dwFillAttribute;
            public int dwFlags;
            public short wShowWindow;
            public short cbReserved2;
            public IntPtr lpReserved2;
            public IntPtr hStdInput;
            public IntPtr hStdOutput;
            public IntPtr hStdError;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct PROCESS_INFORMATION
        {
            public IntPtr hProcess;
            public IntPtr hThread;
            public int dwProcessId;
            public int dwThreadId;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct MEMORY_BASIC_INFORMATION
        {
            public IntPtr BaseAddress;
            public IntPtr AllocationBase;
            public uint AllocationProtect;
            public IntPtr RegionSize;
            public uint State;
            public uint Protect;
            public uint Type;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct SYSTEM_INFO
        {
            public uint dwOemId;
            public uint dwPageSize;
            public IntPtr minimumApplicationAddress;
            public IntPtr maximumApplicationAddress;
            public IntPtr dwActiveProcessorMask;
            public uint dwNumberOfProcessors;
            public uint dwProcessorType;
            public uint dwAllocationGranularity;
            public ushort wProcessorLevel;
            public ushort wProcessorRevision;
        }

        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto)]
        static extern bool CreateProcess(
            string lpApplicationName,
            string lpCommandLine,
            IntPtr lpProcessAttributes,
            IntPtr lpThreadAttributes,
            bool bInheritHandles,
            uint dwCreationFlags,
            IntPtr lpEnvironment,
            string lpCurrentDirectory,
            ref STARTUPINFO lpStartupInfo,
            out PROCESS_INFORMATION lpProcessInformation);

        [DllImport("kernel32.dll")]
        static extern uint ResumeThread(IntPtr hThread);

        [DllImport("kernel32.dll")]
        static extern bool CloseHandle(IntPtr hObject);

        [DllImport("kernel32.dll")]
        static extern bool ReadProcessMemory(IntPtr hProcess, IntPtr lpBaseAddress, [Out] byte[] lpBuffer, int dwSize, out int lpNumberOfBytesRead);

        [DllImport("kernel32.dll")]
        static extern bool WriteProcessMemory(IntPtr hProcess, IntPtr lpBaseAddress, byte[] lpBuffer, int dwSize, out int lpNumberOfBytesWritten);

        [DllImport("kernel32.dll")]
        static extern int VirtualQueryEx(IntPtr hProcess, IntPtr lpAddress, out MEMORY_BASIC_INFORMATION lpBuffer, uint dwLength);

        [DllImport("kernel32.dll")]
        static extern void GetSystemInfo(ref SYSTEM_INFO lpSystemInfo);

        [DllImport("kernel32.dll")]
        static extern bool VirtualProtectEx(IntPtr hProcess, IntPtr lpAddress, UIntPtr dwSize, uint flNewProtect, out uint lpflOldProtect);


        public static bool LaunchAndPatch(string clientPath, string originalIp, string redirectUrl, int locale = 22)
        {
            STARTUPINFO si = new STARTUPINFO();
            PROCESS_INFORMATION pi = new PROCESS_INFORMATION();
            si.cb = Marshal.SizeOf(si);

            string commandLine = $"\"{clientPath}\" /{locale} 0 0";
            string workingDir = System.IO.Path.GetDirectoryName(clientPath);

            LogService.Info($"[Loader] Creating process suspended: {clientPath}");

            if (CreateProcess(null, commandLine, IntPtr.Zero, IntPtr.Zero, false, CREATE_SUSPENDED, IntPtr.Zero, workingDir, ref si, out pi))
            {
                try
                {
                    LogService.Info($"[Loader] Process created (PID: {pi.dwProcessId}). Initiating Smart Memory Patcher...");

                    UniversalPatch(pi, originalIp, redirectUrl);

                    LogService.Info("[Loader] Calling ResumeThread. Client booting...");
                    ResumeThread(pi.hThread);
                    return true;
                }
                catch (Exception ex)
                {
                    LogService.Error($"[Loader] Error during patching: {ex.Message}");
                    return false;
                }
                finally
                {
                    CloseHandle(pi.hProcess);
                    CloseHandle(pi.hThread);
                }
            }
            else
            {
                LogService.Error($"[Loader] Failed to create process. Error: {Marshal.GetLastWin32Error()}");
                return false;
            }
        }

        private static void UniversalPatch(PROCESS_INFORMATION pi, string originalIp, string redirectUrl)
        {
            IntPtr hProcess = pi.hProcess;
            string targetIp = redirectUrl;
            ushort targetPort = 0;
            ushort originalPort = (ushort)ConfigManager.Config.LastServerPort;
            
            if (redirectUrl.Contains(":"))
            {
                var parts = redirectUrl.Split(':');
                targetIp = parts[0];
                if (ushort.TryParse(parts[1], out ushort p))
                    targetPort = p;
            }

            byte[] targetBytes = Encoding.ASCII.GetBytes(targetIp);
            
            // Append a null terminator so the client reads it properly
            byte[] patchBytes = new byte[targetBytes.Length + 1];
            Array.Copy(targetBytes, patchBytes, targetBytes.Length);
            patchBytes[patchBytes.Length - 1] = 0x00;

            // Wipe Cache as requested 
            ConfigManager.Config.CachedIpOffsets?.Clear();
            ConfigManager.Save();
            
            List<long> cachedOffsets = ConfigManager.Config.CachedIpOffsets ?? new List<long>();
            bool fastPathSuccess = false;

            // Step A & B: Try Fast Path
            if (cachedOffsets.Count > 0)
            {
                LogService.Info("[Scanner] Checking cached offsets (Fast Path)...");
                int successCount = 0;

                foreach (long relativeOffset in cachedOffsets)
                {
                    IntPtr targetAddress = (IntPtr)relativeOffset;
                    
                    // Verify if it contains an IP pattern
                    byte[] checkBuf = new byte[30];
                    if (ReadProcessMemory(hProcess, targetAddress, checkBuf, 30, out int bytesRead))
                    {
                        string strVal = Encoding.ASCII.GetString(checkBuf);
                        int nullIdx = strVal.IndexOf('\0');
                        if (nullIdx != -1) strVal = strVal.Substring(0, nullIdx);
                        
                        if (strVal == "255.255.255.0") continue;

                        if (strVal == originalIp)
                        {
                            LogService.Info($"[Scanner] Found TARGET IP at 0x{relativeOffset:X}");
                            WritePatch(hProcess, targetAddress, patchBytes, strVal.Length, false);
                            successCount++;
                            PatchPortNearAddress(hProcess, targetAddress, originalPort, targetPort);
                        }
                        else
                        {
                            // Could fail if memory is scrambled or if it was Unicode. Try simple Unicode check.
                            string uniVal = Encoding.Unicode.GetString(checkBuf);
                            int uniNullIdx = uniVal.IndexOf('\0');
                            if (uniNullIdx != -1) uniVal = uniVal.Substring(0, uniNullIdx);
                            
                            if (uniVal == "255.255.255.0") continue;

                            if (uniVal == originalIp)
                            {
                                LogService.Info($"[Scanner] Found TARGET IP at 0x{relativeOffset:X}");
                                WritePatch(hProcess, targetAddress, Encoding.Unicode.GetBytes(targetIp + "\0"), uniVal.Length * 2, true);
                                successCount++;
                                PatchPortNearAddress(hProcess, targetAddress, originalPort, targetPort);
                            }
                        }
                    }
                }

                if (successCount > 0)
                {
                    fastPathSuccess = true;
                }
            }

            // Step C: Full Scan
            if (!fastPathSuccess)
            {
                LogService.Info("[Scanner] Scanning memory for IP patterns...");
                List<long> newOffsets = new List<long>();
                
                SYSTEM_INFO sysInfo = new SYSTEM_INFO();
                GetSystemInfo(ref sysInfo);

                long currentAddress = (long)sysInfo.minimumApplicationAddress;
                long endAddress = (long)sysInfo.maximumApplicationAddress;

                MEMORY_BASIC_INFORMATION memInfo;
                Regex ipRegex = new Regex(@"^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$", RegexOptions.Compiled);

                while (currentAddress < endAddress)
                {
                    VirtualQueryEx(hProcess, (IntPtr)currentAddress, out memInfo, (uint)Marshal.SizeOf(typeof(MEMORY_BASIC_INFORMATION)));

                    bool isScannable = memInfo.State == MEM_COMMIT && 
                                       (memInfo.Protect == PAGE_READWRITE || 
                                        memInfo.Protect == 0x40 /* PAGE_EXECUTE_READWRITE */ ||
                                        memInfo.Protect == 0x02 /* PAGE_READONLY */ ||
                                        memInfo.Protect == 0x20 /* PAGE_EXECUTE_READ */);

                    if (isScannable)
                    {
                        byte[] buffer = new byte[(int)memInfo.RegionSize];
                        if (ReadProcessMemory(hProcess, memInfo.BaseAddress, buffer, buffer.Length, out int bytesRead))
                        {
                            // Optimized scan for ASCII and Unicode strings looking for IPs
                            for (int i = 0; i < bytesRead - 14; i++) // -14 to leave room for minimum length IPs
                            {
                                // Check ASCII
                                if (buffer[i] >= '0' && buffer[i] <= '9')
                                {
                                    int len = 0;
                                    while ((i + len) < bytesRead && len < 16 && 
                                           ((buffer[i+len] >= '0' && buffer[i+len] <= '9') || buffer[i+len] == '.'))
                                    {
                                        len++;
                                    }

                                    if (len >= 7) // min length for x.x.x.x
                                    {
                                        // check if null terminated after the IP string
                                        if ((i + len) < bytesRead && buffer[i + len] == '\0')
                                        {
                                            string candidate = Encoding.ASCII.GetString(buffer, i, len);
                                            
                                            if (candidate == "255.255.255.0") { i += len; continue; }

                                            if (candidate == originalIp) // Strict Match
                                            {
                                                long offset = (long)memInfo.BaseAddress + i;
                                                LogService.Info($"[Scanner] Found TARGET IP at 0x{offset:X}");
                                                WritePatch(hProcess, (IntPtr)offset, patchBytes, candidate.Length, false);
                                                PatchPortNearAddress(hProcess, (IntPtr)offset, originalPort, targetPort);
                                                newOffsets.Add(offset);
                                                i += len; // skip past this string
                                                continue;
                                            }
                                        }
                                    }
                                }

                                // Check Unicode (UTF-16 LE)
                                if (buffer[i] >= '0' && buffer[i] <= '9' && buffer[i+1] == '\0')
                                {
                                    int len = 0; // Length in bytes
                                    while ((i + len + 1) < bytesRead && len < 32 && 
                                           ((buffer[i+len] >= '0' && buffer[i+len] <= '9') || buffer[i+len] == '.') && 
                                           buffer[i+len+1] == '\0')
                                    {
                                        len += 2;
                                    }

                                    if (len >= 14) // min length for x.x.x.x in unicode (7 chars * 2)
                                    {
                                        // check if null terminated (2 bytes) after the IP string
                                        if ((i + len + 1) < bytesRead && buffer[i + len] == '\0' && buffer[i + len + 1] == '\0')
                                        {
                                            string candidate = Encoding.Unicode.GetString(buffer, i, len);
                                            
                                            if (candidate == "255.255.255.0") { i += len; continue; }

                                            if (candidate == originalIp) // Strict Match
                                            {
                                                long offset = (long)memInfo.BaseAddress + i;
                                                LogService.Info($"[Scanner] Found TARGET IP at 0x{offset:X}");
                                                byte[] unicodePatch = Encoding.Unicode.GetBytes(targetIp + "\0");
                                                WritePatch(hProcess, (IntPtr)offset, unicodePatch, len, true);
                                                PatchPortNearAddress(hProcess, (IntPtr)offset, originalPort, targetPort);
                                                newOffsets.Add(offset);
                                                i += len; // skip past this string
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    long nextAddress = (long)memInfo.BaseAddress + (long)memInfo.RegionSize;
                    if (nextAddress <= currentAddress) break; 
                    currentAddress = nextAddress;
                }

                // Step D: Save Offsets
                if (newOffsets.Count > 0)
                {
                    ConfigManager.Config.CachedIpOffsets = newOffsets;
                    ConfigManager.Save();
                    LogService.Info($"[Config] Saved {newOffsets.Count} new Offset(s) to settings for future use.");
                }
                else
                {
                    LogService.Error("[Error] Scanner could not find any IP in memory!");
                    LogService.Warning("[Scanner] The client may be obfuscated or IP strings are extracted later.");
                }
            }
        }

        private static void WritePatch(IntPtr hProcess, IntPtr address, byte[] patchData, int originalByteLen, bool isUnicode = false)
        {
            uint oldProtect = 0;
            // Unprotect
            VirtualProtectEx(hProcess, address, (UIntPtr)originalByteLen, PAGE_READWRITE, out oldProtect);

            int totalBytesToWrite = Math.Max(patchData.Length, originalByteLen + (isUnicode ? 2 : 1));
            byte[] finalPatch = new byte[totalBytesToWrite]; // Init to 0x00
            Array.Copy(patchData, finalPatch, patchData.Length);
            
            WriteProcessMemory(hProcess, address, finalPatch, finalPatch.Length, out int bytesWritten);

            // Verification Module Requested by User
            byte[] verifyBuf = new byte[patchData.Length];
            if (ReadProcessMemory(hProcess, address, verifyBuf, verifyBuf.Length, out int bytesRead))
            {
                bool match = true;
                for (int i = 0; i < patchData.Length; i++)
                {
                    if (verifyBuf[i] != patchData[i])
                    {
                        match = false;
                        break;
                    }
                }
                
                if (match)
                {
                    LogService.Info($"[Patcher] Successfully redirected to 127.0.0.1!");
                }
                else
                {
                    LogService.Error($"[Error] WriteProcessMemory FAILED at 0x{(long)address:X}. Data mismatch (Possible Permission Issue).");
                }
            }
            else
            {
                LogService.Error($"[Error] WriteProcessMemory FAILED at 0x{(long)address:X}. Read verification failed (Possible Permission Issue).");
            }

            // Restore protection
            VirtualProtectEx(hProcess, address, (UIntPtr)originalByteLen, oldProtect, out oldProtect);
        }

        private static void PatchPortNearAddress(IntPtr hProcess, IntPtr ipAddress, ushort originalPort, ushort targetPort)
        {
            if (originalPort == 0 || targetPort == 0 || originalPort == targetPort) return;

            // Scan 64 bytes before and 64 bytes after the IP address string
            long searchStart = (long)ipAddress - 64;
            int searchSize = 128 + 32; // 64 before + ip len ~32 + 64 after
            
            byte[] buffer = new byte[searchSize];
            if (ReadProcessMemory(hProcess, (IntPtr)searchStart, buffer, searchSize, out int bytesRead))
            {
                byte[] origPortBytes = BitConverter.GetBytes(originalPort); // Little Endian ushort
                
                for (int i = 0; i < bytesRead - 1; i++)
                {
                    if (buffer[i] == origPortBytes[0] && buffer[i+1] == origPortBytes[1])
                    {
                        IntPtr portAddress = (IntPtr)(searchStart + i);
                        
                        byte[] newPortBytes = BitConverter.GetBytes(targetPort);
                        
                        uint oldProtect = 0;
                        VirtualProtectEx(hProcess, portAddress, (UIntPtr)2, PAGE_READWRITE, out oldProtect);
                        WriteProcessMemory(hProcess, portAddress, newPortBytes, 2, out _);
                        VirtualProtectEx(hProcess, portAddress, (UIntPtr)2, oldProtect, out oldProtect);
                        
                        LogService.Info($"[Patcher] Successfully patched Port {originalPort} -> {targetPort} at 0x{(long)portAddress:X}");
                        // Continue checking just in case it appears multiple times in the struct
                    }
                }
            }
        }
        public static void HardPatchIP(IntPtr hProcess)
        {
            LogService.Info("[Patcher] Initiating Hard Memory Patch (Raw Byte Scanning)...");

            string targetIpStr = "192.168.100.9";
            string redirectIpStr = "127.0.0.1";
            ushort targetPort = 15779;
            ushort redirectPort = 15884;

            byte[] asciiTarget = Encoding.ASCII.GetBytes(targetIpStr);
            byte[] asciiRedirect = Encoding.ASCII.GetBytes(redirectIpStr + "\0"); // Null terminate for safety

            byte[] unicodeTarget = Encoding.Unicode.GetBytes(targetIpStr);
            byte[] unicodeRedirect = Encoding.Unicode.GetBytes(redirectIpStr + "\0");

            byte[] hexIpTarget = new byte[] { 192, 168, 100, 9 };
            byte[] hexIpRedirect = new byte[] { 127, 0, 0, 1 };

            byte[] hexPortTarget = BitConverter.GetBytes(targetPort);
            byte[] hexPortRedirect = BitConverter.GetBytes(redirectPort);

            SYSTEM_INFO sysInfo = new SYSTEM_INFO();
            GetSystemInfo(ref sysInfo);

            long currentAddress = (long)sysInfo.minimumApplicationAddress;
            long endAddress = (long)sysInfo.maximumApplicationAddress;

            MEMORY_BASIC_INFORMATION memInfo;
            int patchCount = 0;

            while (currentAddress < endAddress)
            {
                if (VirtualQueryEx(hProcess, (IntPtr)currentAddress, out memInfo, (uint)Marshal.SizeOf(typeof(MEMORY_BASIC_INFORMATION))) == 0)
                    break;

                bool isScannable = memInfo.State == MEM_COMMIT &&
                                   (memInfo.Protect == PAGE_READWRITE ||
                                    memInfo.Protect == 0x40 ||
                                    memInfo.Protect == 0x02 ||
                                    memInfo.Protect == 0x20);

                if (isScannable)
                {
                    byte[] buffer = new byte[(int)memInfo.RegionSize];
                    if (ReadProcessMemory(hProcess, memInfo.BaseAddress, buffer, buffer.Length, out int bytesRead))
                    {
                        // 1. ASCII Scan
                        patchCount += ReplaceInPool(hProcess, memInfo.BaseAddress, buffer, asciiTarget, asciiRedirect, "ASCII IP");
                        
                        // 2. Unicode Scan
                        patchCount += ReplaceInPool(hProcess, memInfo.BaseAddress, buffer, unicodeTarget, unicodeRedirect, "Unicode IP");

                        // 3. Hex IP Scan
                        patchCount += ReplaceInPool(hProcess, memInfo.BaseAddress, buffer, hexIpTarget, hexIpRedirect, "Hex IP");

                        // 4. Hex Port Scan
                        patchCount += ReplaceInPool(hProcess, memInfo.BaseAddress, buffer, hexPortTarget, hexPortRedirect, "Hex Port");
                    }
                }
                currentAddress += (long)memInfo.RegionSize;
            }

            LogService.Info($"[Patcher] Hard Patching completed. Total replacements: {patchCount}");
        }

        private static int ReplaceInPool(IntPtr hProcess, IntPtr baseAddress, byte[] buffer, byte[] target, byte[] replacement, string label)
        {
            int foundCount = 0;
            if (target.Length == 0) return 0;

            for (int i = 0; i <= buffer.Length - target.Length; i++)
            {
                bool match = true;
                for (int j = 0; j < target.Length; j++)
                {
                    if (buffer[i + j] != target[j])
                    {
                        match = false;
                        break;
                    }
                }

                if (match)
                {
                    IntPtr targetAddress = (IntPtr)((long)baseAddress + i);
                    
                    // If replacement is shorter than target, we want to clear the original area completely
                    // to prevent "127.0.0.1.100.9" style artifacts if the client reads by length.
                    byte[] thoroughReplacement = replacement;
                    if (replacement.Length < target.Length)
                    {
                        thoroughReplacement = new byte[target.Length];
                        Array.Copy(replacement, thoroughReplacement, replacement.Length);
                        // Rest of thoroughReplacement is already 0x00 by default (null bytes)
                    }

                    uint oldProtect;
                    VirtualProtectEx(hProcess, targetAddress, (UIntPtr)thoroughReplacement.Length, PAGE_READWRITE, out oldProtect);
                    
                    if (WriteProcessMemory(hProcess, targetAddress, thoroughReplacement, (int)thoroughReplacement.Length, out _))
                    {
                        LogService.Info($"[Patcher] Hard Patched {label} at 0x{(long)targetAddress:X} (Erased full {target.Length} bytes)");
                        foundCount++;
                    }
                    
                    VirtualProtectEx(hProcess, targetAddress, (UIntPtr)thoroughReplacement.Length, oldProtect, out oldProtect);
                    i += target.Length - 1;
                }
            }
            return foundCount;
        }
    }
}
