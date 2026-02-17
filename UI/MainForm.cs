using System;
using System.Windows.Forms;
using System.Threading.Tasks;
using SilkroadAIBot.Core.Networking;
using SilkroadAIBot.Bot;
using SilkroadAIBot.Core.Data;
using SilkroadAIBot.Core.Helpers;
using System.IO;
using SilkroadAIBot.UI.Controls;

namespace SilkroadAIBot.UI
{
    public partial class MainForm : Form
    {
        private TabControl _tabs = null!;
        private RichTextBox _rtbLogs = null!;
        private Label _lblHp = null!, _lblMp = null!;
        private ProgressBar _pbHp = null!, _pbMp = null!;
        private MapVisualizer _map = null!;
        private Label _lblSessionXp = null!, _lblSessionGold = null!, _lblSessionKills = null!, _lblSessionLoot = null!;
        
        // Configuration Controls
        private TextBox _txtSroPath = null!;
        private Button _btnSelectPath = null!;
        private TextBox _txtServerIp = null!, _txtServerPort = null!;
        private Button _btnSaveConfig = null!;

        // Login & Character Controls
        private TextBox _txtUsername = null!, _txtPassword = null!;
        private CheckBox _chkAutoLogin = null!, _chkProxyMode = null!;
        private ComboBox _cmbServers = null!, _cmbCharacters = null!;
        private Button _btnStartBot = null!;
        private ListView _lvPacketMonitor = null!;
        
        private ClientlessConnection? _agent;
        private BotController? _bot;
        private WorldState _worldState = null!;
        private DatabaseManager _db = null!;
        private DataManager _dataManager = null!;
        private SilkroadAIBot.Core.Proxy.ProxyManager? _proxy;

        // Settings Tab Controls
        private Button _btnLaunchClient = null!;
        private CheckBox _chkClientMode = null!;

        public MainForm()
        {
            this.DoubleBuffered = true;
            InitializeComponent();
            ApplyDarkTheme(this);
            InitializeBotComponents();
            RedirectConsole();
        }

        private void InitializeComponent()
        {
            this.Text = "Silkroad AI Bot - Ultimate Dashboard";
            this.Size = new Size(1000, 750);
            this.Font = new Font("Segoe UI", 9F);
            this.BackColor = UIPalette.BackDark;
            this.ForeColor = UIPalette.TextMain;

            // 1. Top Panel: Stats & Basic Status
            Panel topPanel = new Panel { Dock = DockStyle.Top, Height = 80, BackColor = UIPalette.BackLight };
            this.Controls.Add(topPanel);

            _pbHp = new ProgressBar { Location = new Point(20, 30), Size = new Size(200, 15), Style = ProgressBarStyle.Continuous };
            _lblHp = new Label { Text = "HP: 100/100", Location = new Point(20, 10), AutoSize = true, ForeColor = UIPalette.HP };
            
            _pbMp = new ProgressBar { Location = new Point(240, 30), Size = new Size(200, 15), Style = ProgressBarStyle.Continuous };
            _lblMp = new Label { Text = "MP: 50/50", Location = new Point(240, 10), AutoSize = true, ForeColor = UIPalette.MP };

            // Start button moved to Dashboard tab as _btnStartGame
            // We'll keep a reference or use null check if needed.

            topPanel.Controls.AddRange(new Control[] { _pbHp, _lblHp, _pbMp, _lblMp });

            // 2. Main Tabs
            _tabs = new TabControl { Dock = DockStyle.Fill, Appearance = TabAppearance.Normal };
            this.Controls.Add(_tabs);
            
            _tabs.TabPages.Add("LoginConfig", "Login & Config");
            _tabs.TabPages.Add("Dashboard", "Dashboard");
            _tabs.TabPages.Add("Combat", "Combat/Skills");
            _tabs.TabPages.Add("Area", "Area Management");
            _tabs.TabPages.Add("Maintenance", "Inventory/City");
            _tabs.TabPages.Add("Settings", "Settings");

            SetupLoginTab(_tabs.TabPages[0]);
            SetupDashboardTab(_tabs.TabPages[1]);
            SetupCombatTab(_tabs.TabPages[2]);
            SetupAreaTab(_tabs.TabPages[3]);
            SetupMaintenanceTab(_tabs.TabPages[4]);
            SetupSettingsTab(_tabs.TabPages[5]);

            // 3. Log (Bottom)
            _rtbLogs = new RichTextBox 
            { 
                Name = "rtbLogs",
                Dock = DockStyle.Bottom, 
                Height = (int)(this.Height * 0.3), 
                BackColor = UIPalette.BackInput, 
                ForeColor = UIPalette.TextMain, 
                BorderStyle = BorderStyle.None,
                ReadOnly = true,
                Font = new Font("Consolas", 9F)
            };
            this.Controls.Add(_rtbLogs);
            _tabs.SelectedIndex = 0;
        }

        private void SetupLoginTab(TabPage page)
        {
            // 1. Game Directory Selector
            GroupBox gbPath = new GroupBox { Text = "GAME DIRECTORY", Location = new Point(20, 20), Size = new Size(600, 80), ForeColor = UIPalette.TextMain };
            page.Controls.Add(gbPath);

            _txtSroPath = new TextBox { Name = "txtSroPath", Location = new Point(15, 35), Width = 450, ReadOnly = true, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle };
            _btnSelectPath = CreateFlatButton("Select SRO Folder", new Point(475, 32), new Size(110, 30), UIPalette.AccentBlue);
            _btnSelectPath.Click += (s, e) => BrowseForSroPath();
            gbPath.Controls.AddRange(new Control[] { _txtSroPath, _btnSelectPath });

            // 2. Authentication & Server
            GroupBox gbLogin = new GroupBox { Text = "LOGIN SETTINGS", Location = new Point(20, 110), Size = new Size(300, 320), ForeColor = UIPalette.TextMain };
            page.Controls.Add(gbLogin);

            int y = 30;
            gbLogin.Controls.Add(new Label { Text = "Username:", Location = new Point(15, y), AutoSize = true });
            _txtUsername = new TextBox { Name = "txtUsername", Location = new Point(100, y - 3), Width = 170, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle };
            gbLogin.Controls.Add(_txtUsername);

            y += 35;
            gbLogin.Controls.Add(new Label { Text = "Password:", Location = new Point(15, y), AutoSize = true });
            _txtPassword = new TextBox { Name = "txtPassword", Location = new Point(100, y - 3), Width = 170, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle, PasswordChar = '*' };
            gbLogin.Controls.Add(_txtPassword);

            y += 35;
            gbLogin.Controls.Add(new Label { Text = "Server IP:", Location = new Point(15, y), AutoSize = true });
            _txtServerIp = new TextBox { Name = "txtServerIp", Text = "127.0.0.1", Location = new Point(100, y - 3), Width = 170, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle };
            gbLogin.Controls.Add(_txtServerIp);

            y += 35;
            gbLogin.Controls.Add(new Label { Text = "Port:", Location = new Point(15, y), AutoSize = true });
            _txtServerPort = new TextBox { Name = "txtServerPort", Text = "15779", Location = new Point(100, y - 3), Width = 170, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle };
            gbLogin.Controls.Add(_txtServerPort);

            y += 35;
            _chkAutoLogin = new CheckBox { Name = "chkAutoLogin", Text = "Auto Login", Location = new Point(100, y), AutoSize = true };
            _chkProxyMode = new CheckBox { Name = "chkProxyMode", Text = "Proxy Mode", Location = new Point(100, y + 25), AutoSize = true };
            gbLogin.Controls.AddRange(new Control[] { _chkAutoLogin, _chkProxyMode });

            y += 60;
            _btnSaveConfig = CreateFlatButton("SAVE CONFIG", new Point(15, y), new Size(270, 30), UIPalette.BackLight);
            _btnSaveConfig.Click += (s, e) => SaveClientConfig();
            gbLogin.Controls.Add(_btnSaveConfig);

            // 3. Start Button
            _btnStartBot = CreateFlatButton("CONNECT / START BOT", new Point(340, 120), new Size(280, 60), UIPalette.AccentOrange);
            _btnStartBot.Font = new Font(this.Font.FontFamily, 12, FontStyle.Bold);
            _btnStartBot.Click += async (s, e) => await StartSequence();
            page.Controls.Add(_btnStartBot);
        }

        private void SetupDashboardTab(TabPage page)
        {
            // 1. Session Summary
            GroupBox gbSession = new GroupBox { Text = "SESSION SUMMARY", Location = new Point(20, 20), Size = new Size(300, 150), ForeColor = UIPalette.TextMain };
            page.Controls.Add(gbSession);

            _lblSessionXp = new Label { Text = "XP Gained: 0", Location = new Point(20, 30), AutoSize = true };
            _lblSessionGold = new Label { Text = "Gold Collected: 0", Location = new Point(20, 55), AutoSize = true };
            _lblSessionKills = new Label { Text = "Kills Total: 0", Location = new Point(20, 80), AutoSize = true };
            _lblSessionLoot = new Label { Text = "Loot Count: 0", Location = new Point(20, 105), AutoSize = true };
            gbSession.Controls.AddRange(new Control[] { _lblSessionXp, _lblSessionGold, _lblSessionKills, _lblSessionLoot });

            // 2. Packet Monitor
            Label lblMonitor = new Label { Text = "PACKET MONITOR", Location = new Point(20, 180), Font = new Font(this.Font, FontStyle.Bold), AutoSize = true };
            page.Controls.Add(lblMonitor);

            _lvPacketMonitor = new ListView 
            { 
                Name = "lvPacketMonitor",
                Location = new Point(20, 210), 
                Size = new Size(940, 250), 
                View = View.Details, 
                FullRowSelect = true, 
                BackColor = UIPalette.BackInput, 
                ForeColor = UIPalette.TextMain, 
                BorderStyle = BorderStyle.None 
            };
            _lvPacketMonitor.Columns.Add("Time", 100);
            _lvPacketMonitor.Columns.Add("Opcode", 100);
            _lvPacketMonitor.Columns.Add("Size", 80);
            _lvPacketMonitor.Columns.Add("Data", 640);
            page.Controls.Add(_lvPacketMonitor);
        }

        private void SetupCombatTab(TabPage page)
        {
            Panel sidePanel = new Panel { Dock = DockStyle.Left, Width = 300, BackColor = UIPalette.BackLight, Padding = new Padding(10) };
            page.Controls.Add(sidePanel);

            sidePanel.Controls.Add(new Label { Text = "SKILL CONFIGURATION", Location = new Point(10, 10), Font = new Font(this.Font, FontStyle.Bold), AutoSize = true });
            
            ListView lvSkills = new ListView { Location = new Point(10, 40), Size = new Size(280, 400), View = View.Details, FullRowSelect = true, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.None };
            lvSkills.Columns.Add("Skill Name", 150);
            lvSkills.Columns.Add("Enabled", 80);
            sidePanel.Controls.Add(lvSkills);

            Button btnRefresh = CreateFlatButton("REFRESH SKILLS", new Point(10, 450), new Size(280, 30), UIPalette.AccentBlue);
            sidePanel.Controls.Add(btnRefresh);
        }

        private void SetupMaintenanceTab(TabPage page)
        {
            Label lblInv = new Label { Text = "INVENTORY MANAGEMENT", Location = new Point(20, 20), Font = new Font(this.Font, FontStyle.Bold), AutoSize = true };
            page.Controls.Add(lblInv);

            string[] options = { "Auto-Sell Gray Items", "Auto-Store SOS/Alchemy", "Repair at 10% Durability", "Auto-Refill Potions" };
            for (int i = 0; i < options.Length; i++)
            {
                CheckBox chk = new CheckBox { Text = options[i], Location = new Point(20, 60 + (i * 30)), AutoSize = true };
                page.Controls.Add(chk);
            }
        }

        private void SetupAreaTab(TabPage page)
        {
            Panel sidePanel = new Panel { Dock = DockStyle.Left, Width = 250, BackColor = UIPalette.BackLight, Padding = new Padding(10) };
            page.Controls.Add(sidePanel);

            sidePanel.Controls.Add(new Label { Text = "TRAINING SETTINGS", Location = new Point(10, 10), Font = new Font(this.Font, FontStyle.Bold), AutoSize = true });
            
            CheckBox chkEnabled = new CheckBox { Text = "Enable Training Area", Location = new Point(10, 40), Checked = true, AutoSize = true };
            chkEnabled.CheckedChanged += (s, e) => _worldState.TrainingArea.Enabled = chkEnabled.Checked;
            sidePanel.Controls.Add(chkEnabled);

            sidePanel.Controls.Add(new Label { Text = "Radius:", Location = new Point(10, 70), AutoSize = true });
            NumericUpDown numRadius = new NumericUpDown { Location = new Point(10, 90), Width = 100, Maximum = 1000, Value = 50, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain };
            numRadius.ValueChanged += (s, e) => { _worldState.TrainingArea.Radius = (int)numRadius.Value; _map.TrainingRadius = (int)numRadius.Value; _map.Invalidate(); };
            sidePanel.Controls.Add(numRadius);

            Button btnSetCenter = CreateFlatButton("SET CURRENT AS CENTER", new Point(10, 130), new Size(200, 30), UIPalette.AccentBlue);
            btnSetCenter.Click += (s, e) => 
            {
                 _worldState.TrainingArea.Center = _worldState.Character.Position;
                 _map.TrainingCenter = _worldState.Character.Position;
                 _map.Invalidate();
                 LogService.Info($"Training center set to: {_worldState.Character.Position}");
            };
            sidePanel.Controls.Add(btnSetCenter);

            _map = new MapVisualizer { Dock = DockStyle.Fill };
            page.Controls.Add(_map);
        }

        private void SetupSettingsTab(TabPage page)
        {
            _btnLaunchClient = CreateFlatButton("LAUNCH SRO", new Point(120, 20), new Size(150, 30), UIPalette.AccentOrange);
            _btnLaunchClient.Click += (s, e) => LaunchClient();

            _chkClientMode = new CheckBox { Text = "Proxy Mode (Connect via actual client)", Location = new Point(120, 70), AutoSize = true };

            page.Controls.AddRange(new Control[] { _btnLaunchClient, _chkClientMode });
        }

        private Button CreateFlatButton(string text, Point pos, Size size, Color accent)
        {
            return new Button
            {
                Text = text,
                Location = pos,
                Size = size,
                FlatStyle = FlatStyle.Flat,
                BackColor = accent,
                ForeColor = Color.White,
                Cursor = Cursors.Hand
            };
        }

        private void ApplyDarkTheme(Control container)
        {
            foreach (Control c in container.Controls)
            {
                if (c is Panel || c is TabPage) 
                {
                    c.BackColor = UIPalette.BackDark;
                    c.ForeColor = UIPalette.TextMain;
                }
                if (c is Label lbl) lbl.ForeColor = UIPalette.TextMain;
                if (c is CheckBox chk) chk.ForeColor = UIPalette.TextMain;
                if (c.HasChildren) ApplyDarkTheme(c);
            }
        }

        private void RedirectConsole()
        {
            var writer = new ControlWriter(_rtbLogs);
            Console.SetOut(writer);
            LogService.OnLog += (msg) => SafeInvoke(() => writer.Write(msg + Environment.NewLine));
        }

        private async Task StartSequence()
        {
            if (string.IsNullOrEmpty(_txtSroPath.Text)) 
            { 
                LogService.Error("SRO Path is required. Please select your Silkroad folder."); 
                _tabs.SelectedIndex = 0;
                return; 
            }
            if (string.IsNullOrEmpty(_txtUsername.Text) || string.IsNullOrEmpty(_txtPassword.Text)) 
            { 
                LogService.Error("Username and Password are required."); 
                _tabs.SelectedIndex = 0;
                return; 
            }

            if (_chkProxyMode.Checked)
                await StartProxySequence();
            else
                await StartBotSequence();
        }

        private async Task StartProxySequence()
        {
             _btnStartBot.Enabled = false;
             LogService.Info("Starting Proxy Mode...");
             
             _proxy = new SilkroadAIBot.Core.Proxy.ProxyManager(_txtServerIp.Text, int.Parse(_txtServerPort.Text), _worldState);
             _proxy.SetDataManager(_dataManager);
             _proxy.Start();
             
             LogService.Info("Proxy Listener Started. Waiting for client...");
             await Task.CompletedTask;
        }

        private void InitializeBotComponents()
        {
            _db = new DatabaseManager();
            _worldState = new WorldState();
            
            // Wire up UI Events
            _worldState.OnCharacterUpdated += () => SafeInvoke(UpdateCharacterUI);
            _worldState.OnPositionUpdated += () => SafeInvoke(UpdatePositionUI);
            _worldState.OnStatsUpdated += () => SafeInvoke(UpdateStatsUI);

            _dataManager = new DataManager();

            string savedPath = _db.GetConfig("SroPath");
            if (string.IsNullOrEmpty(savedPath))
            {
                LogService.Warning("Silkroad Path not set. Please select SRO folder in Login & Config tab.");
                _tabs.SelectedIndex = 0; 
                _btnStartBot.Enabled = false;
            }
            else
            {
                _txtSroPath.Text = savedPath;
                InitializeDataManager(savedPath);
            }

            _txtUsername.Text = _db.GetConfig("LastUsername");
            _txtServerIp.Text = _db.GetConfig("LastServerIp") ?? "127.0.0.1";
            _txtServerPort.Text = _db.GetConfig("LastServerPort") ?? "15779";
        }

        private async void InitializeDataManager(string path)
        {
            try 
            {
                if (!ValidateSroFolder(path)) return;

                LogService.Info("Initializing Game Data (PK2)...");
                _btnStartBot.Enabled = false;
                await Task.Run(() => _dataManager.Initialize(path, _db));
                LogService.Info("Game Data Loaded Successfully.");
                
                // Server Discovery
                if (_dataManager.AutoDiscoverServerConfig(out string ip, out int port))
                {
                    SafeInvoke(() => {
                        _txtServerIp.Text = ip;
                        _txtServerPort.Text = port.ToString();
                        LogService.Info($"Auto-Discovered Server: {ip}:{port}");
                    });
                }

                _btnStartBot.Enabled = true;
            }
            catch (Exception ex)
            {
                LogService.Error("Failed to initialize game data. Check your SRO Path.", ex);
                _tabs.SelectedIndex = 0;
            }
        }

        private void SafeInvoke(Action action)
        {
            try
            {
                if (this.IsDisposed || !this.IsHandleCreated) return;
                if (this.InvokeRequired) this.BeginInvoke(action);
                else action();
            }
            catch { }
        }

        private void UpdateCharacterUI()
        {
            var chara = _worldState.Character;
            if (chara == null) return;

            _pbHp.Maximum = chara.HPMax > 0 ? (int)chara.HPMax : 100;
            _pbHp.Value = Math.Clamp((int)chara.HP, 0, _pbHp.Maximum);
            _lblHp.Text = $"HP: {chara.HP}/{chara.HPMax}";

            _pbMp.Maximum = chara.MPMax > 0 ? (int)chara.MPMax : 100;
            _pbMp.Value = Math.Clamp((int)chara.MP, 0, _pbMp.Maximum);
            _lblMp.Text = $"MP: {chara.MP}/{chara.MPMax}";
        }

        private void UpdatePositionUI()
        {
            if (_map != null)
            {
                _map.BotPos = _worldState.Character.Position;
                _map.Invalidate();
            }
        }

        private void UpdateStatsUI()
        {
            _lblSessionXp.Text = $"XP Gained: {_worldState.SessionXP}";
            _lblSessionGold.Text = $"Gold Collected: {_worldState.SessionGold}";
            _lblSessionKills.Text = $"Kills Total: {_worldState.SessionKills}";
            _lblSessionLoot.Text = $"Loot Count: {_worldState.SessionLoot}";
        }

        private void BrowseForSroPath()
        {
            using (var fbd = new FolderBrowserDialog())
            {
                if (fbd.ShowDialog() == DialogResult.OK)
                {
                    if (ValidateSroFolder(fbd.SelectedPath))
                    {
                        _txtSroPath.Text = fbd.SelectedPath;
                        _db.SaveConfig("SroPath", fbd.SelectedPath);
                        LogService.Info($"SRO Path updated: {fbd.SelectedPath}");
                        InitializeDataManager(fbd.SelectedPath);
                    }
                }
            }
        }

        private bool ValidateSroFolder(string path)
        {
            bool hasClient = File.Exists(Path.Combine(path, "sro_client.exe"));
            bool hasMedia = File.Exists(Path.Combine(path, "Media.pk2"));

            if (!hasClient || !hasMedia)
            {
                LogService.Error("Invalid SRO Folder! Missing sro_client.exe or Media.pk2.");
                MessageBox.Show("Invalid SRO Folder!\nPlease ensure sro_client.exe and Media.pk2 exist in the selected directory.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return false;
            }
            return true;
        }

        private void SaveClientConfig()
        {
            _db.SaveConfig("LastUsername", _txtUsername.Text);
            _db.SaveConfig("LastServerIp", _txtServerIp.Text);
            _db.SaveConfig("LastServerPort", _txtServerPort.Text);
            LogService.Info("Login configuration saved.");
        }

        private void LaunchClient()
        {
            if (string.IsNullOrEmpty(_txtSroPath.Text)) return;
            ClientLauncher.Launch(_txtSroPath.Text);
        }


        private async Task StartBotSequence()
        {
            _btnStartBot.Enabled = false;
            LogService.Info("Starting Bot Sequence (Clientless)...");
            
            try
            {
                var loginManager = new LoginManager(_worldState, _dataManager);
                bool success = await loginManager.LoginAsync(_txtServerIp.Text, int.Parse(_txtServerPort.Text), _txtUsername.Text, _txtPassword.Text);
                
                if (!success) { _btnStartBot.Enabled = true; return; }
                
                _agent = loginManager.Connection;
                
                _bot = new BotController(_worldState, _agent, _db);
                _bot.AddBundle(new SilkroadAIBot.Bot.Bundles.TargetBundle(_agent));
                _bot.AddBundle(new SilkroadAIBot.Bot.Bundles.MovementBundle(_agent));
                _bot.AddBundle(new SilkroadAIBot.Bot.Bundles.AttackBundle(_agent));
                _bot.AddBundle(new SilkroadAIBot.Bot.Bundles.LootBundle(_agent));
                _bot.AddBundle(new SilkroadAIBot.Bot.Bundles.RecoveryBundle(_agent));
                
                _bot.Start();
                LogService.Info("Bot Running.");
            }
            catch (Exception ex)
            {
                LogService.Error("Bot Sequence Error", ex);
                _btnStartBot.Enabled = true;
            }
        }
    }

    public class ControlWriter : TextWriter
    {
        private RichTextBox _log;
        public ControlWriter(RichTextBox log) { _log = log; }

        public override void Write(string? value)
        {
            if (value == null) return;
            if (_log.InvokeRequired)
            {
                _log.Invoke(new Action<string?>(Write), value);
            }
            else
            {
                AppendColored(value);
            }
        }

        private void AppendColored(string text)
        {
            _log.SelectionStart = _log.TextLength;
            _log.SelectionLength = 0;

            if (text.Contains("[ERROR]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = UIPalette.Error;
            else if (text.Contains("[WARNING]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = UIPalette.Warning;
            else if (text.Contains("[INFO]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = UIPalette.Info;
            else if (text.Contains("[LOOT]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = Color.LightGreen;
            else _log.SelectionColor = UIPalette.TextMain;

            _log.AppendText(text);
            _log.ScrollToCaret();
        }

        public override System.Text.Encoding Encoding => System.Text.Encoding.UTF8;
    }
}
