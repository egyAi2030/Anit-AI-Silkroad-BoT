# Silkroad AI Bot - Progress & Roadmap

## Completed Features

- [x] **UI Foundation** (MainForm, Sidebar, ControlWriter for Logs).
- [x] **Database Initialization** (SQLite `bot_data.db`).
- [x] **PK2 Data Extraction** (Media, Data, Map) and reading the official Gateway IP.
- [x] **Proxy System Base** (Listening on local port, connecting to real server).
- [x] **Security & Blowfish implementation** for packet handling.
- [x] **Client Launcher** (100% Native).
- [x] **Smart Asynchronous Memory Patching** (Bypassing UAC/Defender, reading obfuscated IP, and successfully redirecting the client to `127.0.0.1`).

## Pending/Upcoming Features (Roadmap)

- [x] **Fix Packet Relay Loop** (Swallowing `0x5000`/`0x9000` Handshake packets to prevent C9 during login).
- [ ] **Login Flow & Captcha Handling**.
- [x] **Agent Server Redirection** (Intercepting the Agent IP upon login success and spawning an Agent Proxy on a new port).
- [ ] **Character Selection & Spawning packet parsing**.
- [ ] **World State Parsing** (Players, Monsters, Items, HP/MP updates).
- [ ] **Bot AI Logic** (Auto-potion, Auto-attack, Auto-target).
- [ ] **Navigation & NavMesh Pathfinding**.
