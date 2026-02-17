using System.Drawing;

namespace SilkroadAIBot.Core.Helpers
{
    public static class UIPalette
    {
        // Backgrounds
        public static readonly Color BackDark = Color.FromArgb(30, 30, 30);
        public static readonly Color BackLight = Color.FromArgb(45, 45, 48);
        public static readonly Color BackInput = Color.FromArgb(37, 37, 38);

        // Accents
        public static readonly Color AccentBlue = Color.FromArgb(0, 122, 204);
        public static readonly Color AccentOrange = Color.FromArgb(202, 81, 0);
        
        // Status
        public static readonly Color Success = Color.FromArgb(86, 156, 214); // Cyan-ish Blue
        public static readonly Color Warning = Color.FromArgb(220, 220, 170); // Yellow-ish
        public static readonly Color Error = Color.FromArgb(244, 71, 71); // Soft Red
        public static readonly Color Info = Color.FromArgb(180, 180, 180); // Gray

        // Text
        public static readonly Color TextMain = Color.FromArgb(220, 220, 220);
        public static readonly Color TextDim = Color.FromArgb(150, 150, 150);

        // Progress Bars
        public static readonly Color HP = Color.FromArgb(192, 57, 43);
        public static readonly Color MP = Color.FromArgb(41, 128, 185);
        public static readonly Color XP = Color.FromArgb(39, 174, 96);
    }
}
