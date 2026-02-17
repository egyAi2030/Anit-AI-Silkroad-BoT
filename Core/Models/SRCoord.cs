using System;

namespace SilkroadAIBot.Core.Models
{
    public struct SRCoord
    {
        public ushort Region;
        public float X;
        public float Y;
        public float Z;

        public float WorldX
        {
            get
            {
                if (IsDungeon) return X; // Dungeons often use absolute coords or different mapping
                byte xSector = (byte)(Region & 0xFF);
                return (xSector - 135) * 192 + (X / 10);
            }
        }

        public float WorldY
        {
            get
            {
                if (IsDungeon) return Y;
                byte ySector = (byte)(Region >> 8);
                return (ySector - 92) * 192 + (Y / 10);
            }
        }

        public bool IsDungeon => Region > 32767;

        public SRCoord(ushort region, float x, float y, float z)
        {
            Region = region;
            X = x;
            Y = y;
            Z = z;
        }

        public double DistanceTo(SRCoord other)
        {
            if (IsDungeon != other.IsDungeon) return 100000; // Too far
            return Math.Sqrt(Math.Pow(WorldX - other.WorldX, 2) + Math.Pow(WorldY - other.WorldY, 2));
        }

        public override string ToString()
        {
            return $"[{Region}] ({X:F1}, {Y:F1}, {Z:F1}) | World({WorldX:F1}, {WorldY:F1})";
        }
    }
}
