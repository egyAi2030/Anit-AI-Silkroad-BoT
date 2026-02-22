namespace SilkroadAIBot.Networking
{
    public static class Opcode
    {
        // Identity & Handshake
        public const ushort GLOBAL_IDENTIFICATION = 0x2001;
        public const ushort HANDSHAKE = 0x5000;
        public const ushort HANDSHAKE_ACCEPT = 0x9000;

        // Login (vSRO / Private Server Flow)
        public const ushort CLIENT_LOGIN_REQUEST = 0x6102;
        public const ushort SERVER_CAPTCHA_CHALLENGE = 0x2322;
        public const ushort CLIENT_CAPTCHA_RESPONSE = 0x6103;
        public const ushort SERVER_LOGIN_REDIRECT = 0xA102;
        public const ushort SERVER_LOGIN_ERROR = 0xA103;
        public const ushort CLIENT_AGENT_AUTH = 0x6103; // Same as captcha response opcode

        // Character Selection
        public const ushort CLIENT_CHARACTER_SELECTION_JOIN_REQUEST = 0x7001;
        public const ushort SERVER_CHARACTER_SELECTION_JOIN_RESPONSE = 0xB001;
        public const ushort SERVER_CHARACTER_SELECTION_ACTION_RESPONSE = 0xB007;

        // Game Data Loading
        public const ushort SERVER_CHARACTER_DATA_BEGIN = 0x34A5;
        public const ushort SERVER_CHARACTER_DATA = 0x3013;
        public const ushort SERVER_CHARACTER_DATA_END = 0x34A6;
        public const ushort CLIENT_CHARACTER_CONFIRM_SPAWN = 0x3012;

        // Entity Spawn/Despawn
        public const ushort SERVER_ENTITY_SPAWN = 0x3015;
        public const ushort SERVER_ENTITY_GROUP_SPAWN = 0x3019;
        public const ushort SERVER_ENTITY_DESPAWN = 0x3016;
        public const ushort SERVER_ENTITY_UPDATE_STATUS = 0x3057;

        // Movement & Action
        public const ushort CLIENT_CHARACTER_MOVEMENT = 0x7021;
        public const ushort CLIENT_CHARACTER_ACTION_REQUEST = 0x7074;
        public const ushort SERVER_CHARACTER_ACTION_RESPONSE = 0xB074;

        // Inventory
        public const ushort CLIENT_INVENTORY_ITEM_USE = 0x704C;
        public const ushort SERVER_INVENTORY_ITEM_USE = 0xB04C;
        public const ushort CLIENT_INVENTORY_ITEM_MOVEMENT = 0x7034;
        public const ushort SERVER_INVENTORY_ITEM_MOVEMENT = 0xB034;
    }
}

