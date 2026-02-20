using System;
using System.Windows.Forms;
using System.Threading.Tasks;
using SilkroadAIBot.Networking;
using SilkroadAIBot.Bot;
using SilkroadAIBot.Data;
using SilkroadAIBot.Core.Helpers;
using System.IO;
using SilkroadAIBot.UI.Controls;
using SilkroadAIBot.Core.Configuration;
using System.Collections.Generic;
using System.Drawing;

namespace SilkroadAIBot.UI
{
    public partial class MainForm : Form
    {
        private Panel _sidebar = null!, _contentPanel = null!;
        private Dictionary<string, Panel> _views = new Dictionary<string, Panel>();
        private Dictionary<string, Button> _navButtons = new Dictionary<string, Button>();
        
        // Views
        private Panel _viewSetup = null!, _viewDashboard = null!, _viewConfig = null!, _viewLogs = null!;
        private RichTextBox _rtbLogs = null!;

        // -----------------------------------------
        // View 1: Setup & Login Controls
        // -----------------------------------------
        private TextBox _txtSroPath = null!;
        private Button _btnSelectPath = null!, _btnLoadData = null!;
        private Label _lblDiscoveredServer = null!;
        private CheckBox _chkProxyMode = null!;
        private Button _btnStartGame = null!;
        // (Optional credentials for Clientless if they want it later)
        private TextBox _txtUsername = null!, _txtPassword = null!;

        // -----------------------------------------
        // View 2: Dashboard Controls
        // -----------------------------------------
        private ProgressBar _pbHp = null!, _pbMp = null!, _pbExp = null!;
        private Label _lblHp = null!, _lblMp = null!, _lblExp = null!;
        private MapVisualizer _map = null!;
        private Label _lblSessionXp = null!, _lblSessionGold = null!, _lblState = null!;

        // -----------------------------------------
        // View 3: Bot Config Controls
        // -----------------------------------------
        private TabControl _tcBotConfig = null!;
        private TabPage _tpCombat = null!, _tpArea = null!, _tpLoot = null!;
        private NumericUpDown _numRadius = null!;

        // -----------------------------------------
        // Logic
        // -----------------------------------------
        private WorldState _worldState = null!;
        private DatabaseManager _db = null!;
        private DataManager _dataManager = null!;
        private SilkroadAIBot.Proxy.ProxyManager? _proxy;
        private BotController? _bot;

        public MainForm()
        {
            this.DoubleBuffered = true;
            this.Text = "Silkroad AI Bot";
            this.Size = new Size(1100, 750);
            this.Font = new Font("Segoe UI", 9F);
            this.BackColor = UIPalette.BackDark;
            this.ForeColor = UIPalette.TextMain;

            InitializeLayout();
            ApplyDarkTheme(this);
            InitializeBotComponents();
        }

        private void InitializeLayout()
        {
            // 1. Sidebar (Left Navigation)
            _sidebar = new Panel { Dock = DockStyle.Left, Width = 200, BackColor = UIPalette.BackLight, Padding = new Padding(5) };
            this.Controls.Add(_sidebar);

            // 2. Content Panel
            _contentPanel = new Panel { Dock = DockStyle.Fill, BackColor = UIPalette.BackDark, Padding = new Padding(10) };
            this.Controls.Add(_contentPanel);
            _contentPanel.BringToFront();

            // Create Views
            _viewSetup = CreateViewPanel("Setup & Login");
            _viewDashboard = CreateViewPanel("Dashboard");
            _viewConfig = CreateViewPanel("Bot Config");
            _viewLogs = CreateViewPanel("Logs");

            // Setup Individual Views
            SetupView1_SetupAndLogin(_viewSetup);
            SetupView2_Dashboard(_viewDashboard);
            SetupView3_BotConfig(_viewConfig);
            SetupView4_Logs(_viewLogs);

            // Create Navigation Buttons
            AddNavButton("Setup & Login", _viewSetup);
            AddNavButton("Dashboard", _viewDashboard);
            AddNavButton("Bot Config", _viewConfig);
            AddNavButton("Logs", _viewLogs);

            // Default
            ShowView(_viewSetup);
        }

        private Panel CreateViewPanel(string name)
        {
            var p = new Panel { Dock = DockStyle.Fill, Visible = false, BackColor = UIPalette.BackDark, Name = name };
            _contentPanel.Controls.Add(p);
            _views[name] = p;
            return p;
        }

        private void AddNavButton(string text, Panel view)
        {
            Button btn = new Button
            {
                Text = "  " + text,
                Tag = view,
                Dock = DockStyle.Top,
                Height = 60,
                FlatStyle = FlatStyle.Flat,
                BackColor = UIPalette.BackLight, 
                ForeColor = UIPalette.TextMain,
                TextAlign = ContentAlignment.MiddleLeft,
                Padding = new Padding(10, 0, 0, 0),
                Cursor = Cursors.Hand,
                Font = new Font("Segoe UI", 11F, FontStyle.Bold)
            };
            btn.FlatAppearance.BorderSize = 0;
            btn.Click += (s, e) => ShowView(view);
            
            _sidebar.Controls.Add(btn);
            _sidebar.Controls.SetChildIndex(btn, 0); // stack at top
            _navButtons[text] = btn;
        }

        private void ShowView(Panel view)
        {
            foreach (var v in _views.Values) v.Visible = false;
            view.Visible = true;
            view.BringToFront();
            
            foreach (var btn in _navButtons.Values)
            {
                if (btn.Tag == view)
                {
                    btn.BackColor = UIPalette.AccentBlue; 
                    btn.ForeColor = Color.White;
                }
                else
                {
                    btn.BackColor = UIPalette.BackLight; 
                    btn.ForeColor = UIPalette.TextMain;
                }
            }
        }

        // ==========================================
        // VIEW 1: SETUP & LOGIN
        // ==========================================
        private void SetupView1_SetupAndLogin(Panel page)
        {
            Label lblTitle = new Label { Text = "GAME SETUP", Font = new Font("Segoe UI", 16, FontStyle.Bold), Location = new Point(20, 20), AutoSize = true, ForeColor = UIPalette.AccentBlue };
            page.Controls.Add(lblTitle);

            // SRO Path Configuration
            GroupBox gbPath = new GroupBox { Text = "1. Game Directory", Location = new Point(20, 70), Size = new Size(600, 100), ForeColor = UIPalette.TextMain };
            page.Controls.Add(gbPath);

            _txtSroPath = new TextBox { Location = new Point(20, 30), Width = 430, ReadOnly = true, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle };
            _btnSelectPath = CreateFlatButton("Select Folder", new Point(460, 28), new Size(120, 30), UIPalette.AccentBlue);
            _btnSelectPath.Click += (s, e) => BrowseForSroPath();
            
            _btnLoadData = CreateFlatButton("LOAD DATA", new Point(20, 60), new Size(560, 30), UIPalette.AccentOrange);
            _btnLoadData.Click += async (s, e) => await LoadGameData();

            gbPath.Controls.AddRange(new Control[] { _txtSroPath, _btnSelectPath, _btnLoadData });

            // Launch Options
            GroupBox gbLaunch = new GroupBox { Text = "2. Launch Settings", Location = new Point(20, 190), Size = new Size(600, 200), ForeColor = UIPalette.TextMain };
            page.Controls.Add(gbLaunch);

            _lblDiscoveredServer = new Label { Text = "Discovered Server: Not Loaded", Location = new Point(20, 30), AutoSize = true, Font = new Font("Segoe UI", 9, FontStyle.Italic) };
            
            _chkProxyMode = new CheckBox { Text = "Proxy Mode (SRO Client)", Location = new Point(20, 60), Checked = true, AutoSize = true, Font = new Font("Segoe UI", 10, FontStyle.Bold) };
            
            // Clientless info (hidden unless they uncheck proxy)
            Label lblWarning = new Label { Text = "Clientless requires Username/Password:", Location = new Point(20, 90), AutoSize = true };
            _txtUsername = new TextBox { Location = new Point(20, 110), Width = 200, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle };
            _txtPassword = new TextBox { Location = new Point(230, 110), Width = 200, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain, BorderStyle = BorderStyle.FixedSingle, PasswordChar = '*' };

            _btnStartGame = CreateFlatButton("START GAME", new Point(20, 150), new Size(560, 40), UIPalette.AccentGreen);
            _btnStartGame.Font = new Font("Segoe UI", 12, FontStyle.Bold);
            _btnStartGame.Click += async (s, e) => await StartGameSequence();

            gbLaunch.Controls.AddRange(new Control[] { _lblDiscoveredServer, _chkProxyMode, lblWarning, _txtUsername, _txtPassword, _btnStartGame });
        }

        // ==========================================
        // VIEW 2: DASHBOARD
        // ==========================================
        private void SetupView2_Dashboard(Panel page)
        {
            // Top Stats Panel
            Panel pnlStats = new Panel { Dock = DockStyle.Top, Height = 80, BackColor = UIPalette.BackLight, Padding = new Padding(10) };
            page.Controls.Add(pnlStats);

            _lblHp = new Label { Text = "HP", Location = new Point(10, 10), ForeColor = UIPalette.HP, Font = new Font("Segoe UI", 9, FontStyle.Bold), AutoSize = true };
            _pbHp = new ProgressBar { Location = new Point(10, 30), Size = new Size(200, 20), Style = ProgressBarStyle.Continuous };
            
            _lblMp = new Label { Text = "MP", Location = new Point(230, 10), ForeColor = UIPalette.MP, Font = new Font("Segoe UI", 9, FontStyle.Bold), AutoSize = true };
            _pbMp = new ProgressBar { Location = new Point(230, 30), Size = new Size(200, 20), Style = ProgressBarStyle.Continuous };

            _lblExp = new Label { Text = "EXP", Location = new Point(450, 10), ForeColor = UIPalette.AccentBlue, Font = new Font("Segoe UI", 9, FontStyle.Bold), AutoSize = true };
            _pbExp = new ProgressBar { Location = new Point(450, 30), Size = new Size(300, 20), Style = ProgressBarStyle.Continuous };

            pnlStats.Controls.AddRange(new Control[] { _lblHp, _pbHp, _lblMp, _pbMp, _lblExp, _pbExp });

            // Center: MapVisualizer
            _map = new MapVisualizer { Location = new Point(10, 90), Size = new Size(600, 600), Anchor = AnchorStyles.Top | AnchorStyles.Bottom | AnchorStyles.Left };
            page.Controls.Add(_map);

            // Right: Information
            GroupBox gbInfo = new GroupBox { Text = "SESSION INFO", Location = new Point(630, 90), Size = new Size(250, 200), ForeColor = UIPalette.TextMain, Anchor = AnchorStyles.Top | AnchorStyles.Right };
            page.Controls.Add(gbInfo);

            _lblState = new Label { Text = "State: IDLE", Location = new Point(10, 30), AutoSize = true, Font = new Font("Segoe UI", 10, FontStyle.Bold), ForeColor = UIPalette.AccentOrange };
            _lblSessionXp = new Label { Text = "XP Gained: 0", Location = new Point(10, 60), AutoSize = true };
            _lblSessionGold = new Label { Text = "Gold Collected: 0", Location = new Point(10, 90), AutoSize = true };
            
            Button btnStartBot = CreateFlatButton("Start Bot", new Point(10, 130), new Size(110, 30), UIPalette.AccentGreen);
            btnStartBot.Click += (s, e) => {
                if (_bot == null) {
                   _bot = new BotController(_worldState, null!, _db); // Pass null connection for now
                }
                _bot.Start();
                _lblState.Text = "State: BOT RUNNING";
                _lblState.ForeColor = UIPalette.AccentGreen;
            };

            Button btnStopBot = CreateFlatButton("Stop Bot", new Point(130, 130), new Size(110, 30), UIPalette.Error);
            btnStopBot.Click += (s, e) => {
                if (_bot != null) _bot.Stop();
                _lblState.Text = "State: BOT STOPPED";
                _lblState.ForeColor = UIPalette.Error;
            };

            gbInfo.Controls.AddRange(new Control[] { _lblState, _lblSessionXp, _lblSessionGold, btnStartBot, btnStopBot });
        }

        // ==========================================
        // VIEW 3: BOT CONFIG
        // ==========================================
        private void SetupView3_BotConfig(Panel page)
        {
            _tcBotConfig = new TabControl { Dock = DockStyle.Fill };
            page.Controls.Add(_tcBotConfig);

            _tpCombat = new TabPage("Combat");
            _tpArea = new TabPage("Training Area");
            _tpLoot = new TabPage("Loot");

            _tcBotConfig.TabPages.Add(_tpCombat);
            _tcBotConfig.TabPages.Add(_tpArea);
            _tcBotConfig.TabPages.Add(_tpLoot);

            // --- Combat ---
            _tpCombat.Controls.Add(new Label { Text = "Skill List (Coming Soon)", Location = new Point(20, 20), AutoSize = true });

            // --- Area ---
            _numRadius = new NumericUpDown { Location = new Point(20, 50), Width = 100, Maximum = 1000, Value = 50, BackColor = UIPalette.BackInput, ForeColor = UIPalette.TextMain };
            _numRadius.ValueChanged += (s, e) => { if (_worldState != null) _worldState.TrainingArea.Radius = (int)_numRadius.Value; };
            _tpArea.Controls.Add(new Label { Text = "Training Radius (X,Y):", Location = new Point(20, 30), AutoSize = true });
            _tpArea.Controls.Add(_numRadius);

            Button btnSetCenter = CreateFlatButton("Set Current Position as Center", new Point(20, 90), new Size(200, 30), UIPalette.AccentBlue);
            btnSetCenter.Click += (s, e) => { if (_worldState != null) _worldState.TrainingArea.Center = _worldState.Character.Position; };
            _tpArea.Controls.Add(btnSetCenter);

            // --- Loot ---
            _tpLoot.Controls.Add(new CheckBox { Text = "Auto Pickup Gold", Location = new Point(20, 20), Checked = true, AutoSize = true });
            _tpLoot.Controls.Add(new CheckBox { Text = "Auto Pickup Items", Location = new Point(20, 50), Checked = true, AutoSize = true });
        }

        // ==========================================
        // VIEW 4: LOGS
        // ==========================================
        private void SetupView4_Logs(Panel page)
        {
            _rtbLogs = new RichTextBox
            {
                Dock = DockStyle.Fill,
                BackColor = UIPalette.BackInput,
                ForeColor = UIPalette.TextMain,
                BorderStyle = BorderStyle.None,
                ReadOnly = true,
                Font = new Font("Consolas", 10F)
            };
            page.Controls.Add(_rtbLogs);
        }

        // ==========================================
        // HELPERS & LOGIC
        // ==========================================
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
                Cursor = Cursors.Hand,
                FlatAppearance = { BorderSize = 0 }
            };
        }

        private void ApplyDarkTheme(Control container)
        {
            foreach (Control c in container.Controls)
            {
                if (c is Panel && c != _sidebar && c != _contentPanel) c.BackColor = UIPalette.BackDark;
                if (c is CheckBox chk) chk.ForeColor = UIPalette.TextMain;
                if (c is GroupBox gb) gb.ForeColor = UIPalette.TextMain;
                if (c is Label lbl) lbl.ForeColor = UIPalette.TextMain;
                if (c.HasChildren) ApplyDarkTheme(c);
            }
        }

        private void RedirectConsole()
        {
            var writer = new ControlWriter(_rtbLogs);
            Console.SetOut(writer);
            LogService.OnLog += (msg) => SafeInvoke(() => writer.Write(msg + Environment.NewLine));
        }

        private void InitializeBotComponents()
        {
            RedirectConsole();
            
            _db = new DatabaseManager();
            _worldState = new WorldState();
            _dataManager = new DataManager();

            ConfigManager.Load();
            _txtSroPath.Text = ConfigManager.Config.SroPath;
            _txtUsername.Text = ConfigManager.Config.Username;
            _txtPassword.Text = ConfigManager.Config.Password;

            if (!string.IsNullOrEmpty(_txtSroPath.Text) && ValidateSroFolder(_txtSroPath.Text))
            {
                // Auto load on startup if path exists
                _ = LoadGameData();
            }
            else
            {
                LogService.Warning("Please select your Silkroad folder to begin.");
            }
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
                        ConfigManager.Config.SroPath = fbd.SelectedPath;
                        ConfigManager.Save();
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
                MessageBox.Show("Invalid Folder! Missing sro_client.exe or Media.pk2", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return false;
            }
            return true;
        }

        private async Task LoadGameData()
        {
            if (!ValidateSroFolder(_txtSroPath.Text)) return;

            LogService.Info("Loading PK2 Data...");
            _btnLoadData.Enabled = false;

            bool success = await Task.Run(() => _dataManager.Initialize(_txtSroPath.Text));
            if (success)
            {
                if (_dataManager.AutoDiscoverServerConfig(out string ip, out int port))
                {
                    SafeInvoke(() => _lblDiscoveredServer.Text = $"Discovered Server: {ip}:{port}");
                }
                else
                {
                    LogService.Warning("Could not discover server IP from Media.pk2.");
                }
            }
            else
            {
                LogService.Error("Failed to initialize game data.");
            }

            _btnLoadData.Enabled = true;
        }

        private async Task StartGameSequence()
        {
            if (string.IsNullOrEmpty(_txtSroPath.Text) || !ValidateSroFolder(_txtSroPath.Text)) return;

            ConfigManager.Config.Username = _txtUsername.Text;
            ConfigManager.Config.Password = _txtPassword.Text;
            ConfigManager.Save();

            _btnStartGame.Enabled = false;

            if (_chkProxyMode.Checked)
            {
                LogService.Info("Starting Proxy Mode...");
                string targetIp = ConfigManager.Config.OriginalServerIp;
                int targetPort = ConfigManager.Config.LastServerPort;

                if (string.IsNullOrEmpty(targetIp))
                {
                    LogService.Error("Original Server IP is unknown! Please Load Data first.");
                    _btnStartGame.Enabled = true;
                    return;
                }

                try
                {
                    if (_proxy != null)
                    {
                        _proxy.Stop();
                        _proxy = null;
                        await Task.Delay(500); // Give the socket OS-level time to unbind
                    }

                    int localProxyPort = 15884; // Forced to bypass reserved range
                    _proxy = new SilkroadAIBot.Proxy.ProxyManager(targetIp, targetPort, _worldState, localProxyPort);
                    _proxy.SetDataManager(_dataManager);
                    _proxy.Start();

                    LogService.Info($"[Proxy] Listening for client on port {localProxyPort}...");
                    LogService.Info($"Proxy started on 127.0.0.1:{localProxyPort}. Launching client diverted to local proxy...");
                    ClientLauncher.LaunchRedirected(_txtSroPath.Text, targetIp, $"127.0.0.1:{localProxyPort}");
                    
                    // ClientLauncher is launching it, keep button disabled to avoid spam,
                    // or re-enable it if you want them to be able to launch multiple (not recommended with same proxy).
                }
                catch (Exception ex)
                {
                    LogService.Error($"Failed to start proxy: {ex.Message}");
                    _btnStartGame.Enabled = true;
                }
            }
            else
            {
                LogService.Info("Starting Clientless Mode (Connecting directly)...");
                var loginManager = new LoginManager(_worldState, _dataManager);
                
                string targetIp = ConfigManager.Config.OriginalServerIp;
                int targetPort = ConfigManager.Config.LastServerPort;
                bool success = await loginManager.LoginAsync(targetIp, targetPort, _txtUsername.Text, _txtPassword.Text);
                
                if (success)
                {
                    LogService.Info("Clientless connected.");
                    ShowView(_viewDashboard);
                }
                else
                {
                    LogService.Error("Clientless login failed.");
                    _btnStartGame.Enabled = true;
                }
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
    }

    public class ControlWriter : TextWriter
    {
        private RichTextBox _log;
        public ControlWriter(RichTextBox log) { _log = log; }
        public override void Write(string? value)
        {
            if (value == null) return;
            if (_log.InvokeRequired) _log.Invoke(new Action<string?>(Write), value);
            else AppendColored(value);
        }
        private void AppendColored(string text)
        {
            _log.SelectionStart = _log.TextLength;
            _log.SelectionLength = 0;
            if (text.Contains("[ERROR]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = UIPalette.Error;
            else if (text.Contains("[WARNING]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = UIPalette.Warning;
            else if (text.Contains("[INFO]", StringComparison.OrdinalIgnoreCase)) _log.SelectionColor = UIPalette.Info;
            else _log.SelectionColor = UIPalette.TextMain;
            _log.AppendText(text);
            _log.ScrollToCaret();
        }
        public override System.Text.Encoding Encoding => System.Text.Encoding.UTF8;
    }
}



