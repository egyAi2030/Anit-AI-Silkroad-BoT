using System;
using System.Text;
using System.IO;
using System.Collections.Generic;
using System.Runtime.InteropServices;


#region Blowfish
namespace SecurityAPI
{
	public class Blowfish
	{
		private static uint[] bf_P ={
			0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344,
			0xa4093822, 0x299f31d0, 0x082efa98, 0xec4e6c89,
			0x452821e6, 0x38d01377, 0xbe5466cf, 0x34e90c6c,
			0xc0ac29b7, 0xc97c50dd, 0x3f84d5b5, 0xb5470917,
			0x9216d5d9, 0x8979fb1b,
		};
		private static uint[,] bf_S ={
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
			},{
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
			},{
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
			},{
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


#region Packet
namespace SecurityAPI
{
	public class Packet
	{
		private ushort m_opcode;
		private PacketWriter m_writer;
		private PacketReader m_reader;
		private bool m_encrypted;
		private bool m_massive;
		private bool m_locked;
		byte[] m_reader_bytes;
		object m_lock;

		public ushort Opcode
		{
			get { return m_opcode; }
		}
		public bool Encrypted
		{
			get { return m_encrypted; }
		}
		public bool Massive
		{
			get { return m_massive; }
		}
		public int Size
		{
			get 
			{
				lock (m_lock)
				{
					if (m_locked) return m_reader_bytes != null ? m_reader_bytes.Length : 0;
					return m_writer != null ? (int)m_writer.BaseStream.Length : 0;
				}
			}
		}

		public Packet(Packet rhs)
		{
			lock (rhs.m_lock)
			{
				m_lock = new object();

				m_opcode = rhs.m_opcode;
				m_encrypted = rhs.m_encrypted;
				m_massive = rhs.m_massive;

				m_locked = rhs.m_locked;
				if (!m_locked)
				{
					m_writer = new PacketWriter();
					m_reader = null;
					m_reader_bytes = null;
					m_writer.Write(rhs.m_writer.GetBytes());
				}
				else
				{
					m_writer = null;
					m_reader_bytes = rhs.m_reader_bytes;
					m_reader = new PacketReader(m_reader_bytes);
				}
			}
		}
		public Packet(ushort opcode)
		{
			m_lock = new object();
			m_opcode = opcode;
			m_encrypted = false;
			m_massive = false;
			m_writer = new PacketWriter();
			m_reader = null;
			m_reader_bytes = null;
		}
		public Packet(ushort opcode, bool encrypted)
		{
			m_lock = new object();
			m_opcode = opcode;
			m_encrypted = encrypted;
			m_massive = false;
			m_writer = new PacketWriter();
			m_reader = null;
			m_reader_bytes = null;
		}
		public Packet(ushort opcode, bool encrypted, bool massive)
		{
			if (encrypted && massive)
			{
				throw new Exception("[Packet::Packet] Packets cannot both be massive and encrypted!");
			}
			m_lock = new object();
			m_opcode = opcode;
			m_encrypted = encrypted;
			m_massive = massive;
			m_writer = new PacketWriter();
			m_reader = null;
			m_reader_bytes = null;
		}
		public Packet(ushort opcode, bool encrypted, bool massive, byte[] bytes)
		{
			if (encrypted && massive)
			{
				throw new Exception("[Packet::Packet] Packets cannot both be massive and encrypted!");
			}
			m_lock = new object();
			m_opcode = opcode;
			m_encrypted = encrypted;
			m_massive = massive;
			m_writer = new PacketWriter();
			m_writer.Write(bytes);
			m_reader = null;
			m_reader_bytes = null;
		}
		public Packet(ushort opcode, bool encrypted, bool massive, byte[] bytes, int offset, int length)
		{
			if (encrypted && massive)
			{
				throw new Exception("[Packet::Packet] Packets cannot both be massive and encrypted!");
			}
			m_lock = new object();
			m_opcode = opcode;
			m_encrypted = encrypted;
			m_massive = massive;
			m_writer = new PacketWriter();
			m_writer.Write(bytes, offset, length);
			m_reader = null;
			m_reader_bytes = null;
		}

		public byte[] GetBytes()
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					return m_reader_bytes;
				}
				return m_writer.GetBytes();
			}
		}

		public void Lock()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					m_reader_bytes = m_writer.GetBytes();
					m_reader = new PacketReader(m_reader_bytes);
					m_writer.Close();
					m_writer = null;
					m_locked = true;
				}
			}
		}

		public long SeekRead(long offset, SeekOrigin orgin)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot SeekRead on an unlocked Packet.");
				}
				return m_reader.BaseStream.Seek(offset, orgin);
			}
		}

		public int RemainingRead()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot SeekRead on an unlocked Packet.");
				}
				return (int)(m_reader.BaseStream.Length - m_reader.BaseStream.Position);
			}
		}
		public bool ReadBool()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadByte() == 1;
			}
		}

		public byte ReadByte()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadByte();
			}
		}
		public sbyte ReadSByte()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadSByte();
			}
		}
		public ushort ReadUShort()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadUInt16();
			}
		}
		public short ReadShort()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadInt16();
			}
		}
		public uint ReadUInt()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadUInt32();
			}
		}
		public int ReadInt()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadInt32();
			}
		}
		public ulong ReadULong()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadUInt64();
			}
		}
		public long ReadLong()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadInt64();
			}
		}
		public float ReadFloat()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadSingle();
			}
		}
		public double ReadDouble()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				return m_reader.ReadDouble();
			}
		}
		public string ReadString(int length, int codepage = 949)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}

				byte[] bytes = m_reader.ReadBytes(length);
				return Encoding.GetEncoding(codepage).GetString(bytes);
			}
		}
		public string ReadAscii(int codepage = 949)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}

				ushort length = m_reader.ReadUInt16();
				byte[] bytes = m_reader.ReadBytes(length);

				return Encoding.GetEncoding(codepage).GetString(bytes);
			}
		}
		public string ReadUnicode()
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}

				ushort length = m_reader.ReadUInt16();
				byte[] bytes = m_reader.ReadBytes(length * 2);

				return Encoding.Unicode.GetString(bytes);
			}
		}
		public byte[] ReadByteArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				byte[] values = new byte[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadByte();
				}
				return values;
			}
		}
		public sbyte[] ReadSByteArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				sbyte[] values = new sbyte[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadSByte();
				}
				return values;
			}
		}
		public ushort[] ReadUShortArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				ushort[] values = new ushort[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadUInt16();
				}
				return values;
			}
		}
		public short[] ReadShortArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				short[] values = new short[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadInt16();
				}
				return values;
			}
		}
		public uint[] ReadUIntArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				uint[] values = new uint[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadUInt32();
				}
				return values;
			}
		}
		public int[] ReadIntArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				int[] values = new int[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadInt32();
				}
				return values;
			}
		}
		public ulong[] ReadULongArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				ulong[] values = new ulong[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadUInt64();
				}
				return values;
			}
		}
		public long[] ReadLongArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				long[] values = new long[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadInt64();
				}
				return values;
			}
		}
		public float[] ReadFloatArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				float[] values = new float[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadSingle();
				}
				return values;
			}
		}
		public double[] ReadDoubleArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				double[] values = new double[count];
				for (int x = 0; x < count; ++x)
				{
					values[x] = m_reader.ReadDouble();
				}
				return values;
			}
		}
		public string[] ReadAsciiArray(int count)
		{
			return ReadAsciiArray(1252);
		}
		public string[] ReadAsciiArray(int codepage, int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				string[] values = new string[count];
				for (int x = 0; x < count; ++x)
				{
					ushort length = m_reader.ReadUInt16();
					byte[] bytes = m_reader.ReadBytes(length);
					values[x] = Encoding.GetEncoding(codepage).GetString(bytes);
				}
				return values;
			}
		}
		public string[] ReadUnicodeArray(int count)
		{
			lock (m_lock)
			{
				if (!m_locked)
				{
					throw new Exception("Cannot Read from an unlocked Packet.");
				}
				string[] values = new string[count];
				for (int x = 0; x < count; ++x)
				{
					ushort length = m_reader.ReadUInt16();
					byte[] bytes = m_reader.ReadBytes(length * 2);
					values[x] = Encoding.Unicode.GetString(bytes);
				}
				return values;
			}
		}
		public long SeekWrite(long offset, SeekOrigin orgin)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot SeekWrite on a locked Packet.");
				}
				return m_writer.BaseStream.Seek(offset, orgin);
			}
		}
		public void WriteBool(bool value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value ? 1 : 0);
			}
		}
		public void WriteByte(byte value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteSByte(sbyte value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteUShort(ushort value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteShort(short value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteUInt(uint value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteInt(int value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteULong(ulong value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteLong(long value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteSingle(float value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteFloat(float value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteDouble(double value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(value);
			}
		}
		public void WriteAscii(string value, int codepage = 949)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}

				byte[] bytes = Encoding.GetEncoding(codepage).GetBytes(value);

				m_writer.Write((ushort)bytes.Length);
				m_writer.Write(bytes);
			}
		}
		public void WriteUnicode(string value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}

				byte[] bytes = Encoding.Unicode.GetBytes(value);

				m_writer.Write((ushort)value.ToString().Length);
				m_writer.Write(bytes);
			}
		}
		public void WriteByte(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write((byte)(Convert.ToUInt64(value) & 0xFF));
			}
		}
		public void WriteSByte(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write((sbyte)(Convert.ToInt64(value) & 0xFF));
			}
		}
		public void WriteUShort(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write((ushort)(Convert.ToUInt64(value) & 0xFFFF));
			}
		}
		public void WriteShort(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write((short)(Convert.ToInt64(value) & 0xFFFF));
			}
		}
		public void WriteUInt(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write((uint)(Convert.ToUInt64(value) & 0xFFFFFFFF));
			}
		}
		public void WriteInt(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write((int)(Convert.ToInt64(value) & 0xFFFFFFFF));
			}
		}
		public void WriteULong(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(Convert.ToUInt64(value));
			}
		}
		public void WriteLong(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(Convert.ToInt64(value));
			}
		}
		public void WriteSingle(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(Convert.ToSingle(value));
			}
		}
		public void WriteDouble(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				m_writer.Write(Convert.ToDouble(value));
			}
		}
		public void WriteAscii(object value)
		{
			WriteAscii(value, 1252);
		}
		public void WriteAscii(object value, int code_page)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}

				byte[] bytes = Encoding.GetEncoding(code_page).GetBytes(value.ToString() ?? "");

				m_writer.Write((ushort)bytes.Length);
				m_writer.Write(bytes);
			}
		}
		public void WriteUnicode(object value)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}

				byte[] bytes = Encoding.Unicode.GetBytes(value.ToString());

				m_writer.Write((ushort)value.ToString().Length);
				m_writer.Write(bytes);
			}
		}

		public void WriteByteArray(byte[] values)
		{
			if (m_locked)
			{
				throw new Exception("Cannot Write to a locked Packet.");
			}
			m_writer.Write(values);
		}
		public void WriteUInt8Array(byte[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteUInt16Array(UInt16[] values)
		{
			WriteUInt16Array(values, 0, values.Length);
		}
		public void WriteUInt16Array(UInt16[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteInt16Array(Int16[] values)
		{
			WriteInt16Array(values, 0, values.Length);
		}
		public void WriteInt16Array(Int16[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteUInt32Array(UInt32[] values)
		{
			WriteUInt32Array(values, 0, values.Length);
		}
		public void WriteUInt32Array(UInt32[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteInt32Array(Int32[] values)
		{
			WriteInt32Array(values, 0, values.Length);
		}
		public void WriteInt32Array(Int32[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteUInt64Array(UInt64[] values)
		{
			WriteUInt64Array(values, 0, values.Length);
		}
		public void WriteUInt64Array(UInt64[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteInt64Array(long[] values)
		{
			WriteInt64Array(values, 0, values.Length);
		}
		public void WriteInt64Array(long[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteSingleArray(float[] values)
		{
			WriteSingleArray(values, 0, values.Length);
		}
		public void WriteSingleArray(float[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteDoubleArray(double[] values)
		{
			WriteDoubleArray(values, 0, values.Length);
		}
		public void WriteDoubleArray(double[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					m_writer.Write(values[x]);
				}
			}
		}
		public void WriteAsciiArray(String[] values, int codepage)
		{
			WriteAsciiArray(values, 0, values.Length, codepage);
		}
		public void WriteAsciiArray(String[] values, int index, int count, int codepage)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteAscii(values[x], codepage);
				}
			}
		}
		public void WriteAsciiArray(String[] values)
		{
			WriteAsciiArray(values, 0, values.Length, 1252);
		}
		public void WriteAsciiArray(String[] values, int index, int count)
		{
			WriteAsciiArray(values, index, count, 1252);
		}
		public void WriteUnicodeArray(String[] values)
		{
			WriteUnicodeArray(values, 0, values.Length);
		}
		public void WriteUnicodeArray(String[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteUnicode(values[x]);
				}
			}
		}

		public void WriteUInt8Array(object[] values)
		{
			WriteUInt8Array(values, 0, values.Length);
		}
		public void WriteUInt8Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteByte(values[x]);
				}
			}
		}
		public void WriteInt8Array(object[] values)
		{
			WriteInt8Array(values, 0, values.Length);
		}
		public void WriteInt8Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteSByte(values[x]);
				}
			}
		}
		public void WriteUInt16Array(object[] values)
		{
			WriteUInt16Array(values, 0, values.Length);
		}
		public void WriteUInt16Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteUShort(values[x]);
				}
			}
		}
		public void WriteInt16Array(object[] values)
		{
			WriteInt16Array(values, 0, values.Length);
		}
		public void WriteInt16Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteShort(values[x]);
				}
			}
		}
		public void WriteUInt32Array(object[] values)
		{
			WriteUInt32Array(values, 0, values.Length);
		}
		public void WriteUInt32Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteUInt(values[x]);
				}
			}
		}
		public void WriteInt32Array(object[] values)
		{
			WriteInt32Array(values, 0, values.Length);
		}
		public void WriteInt32Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteInt(values[x]);
				}
			}
		}
		public void WriteUInt64Array(object[] values)
		{
			WriteUInt64Array(values, 0, values.Length);
		}
		public void WriteUInt64Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteULong(values[x]);
				}
			}
		}
		public void WriteInt64Array(object[] values)
		{
			WriteInt64Array(values, 0, values.Length);
		}
		public void WriteInt64Array(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteLong(values[x]);
				}
			}
		}
		public void WriteSingleArray(object[] values)
		{
			WriteSingleArray(values, 0, values.Length);
		}
		public void WriteSingleArray(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteSingle(values[x]);
				}
			}
		}
		public void WriteDoubleArray(object[] values)
		{
			WriteDoubleArray(values, 0, values.Length);
		}
		public void WriteDoubleArray(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteDouble(values[x]);
				}
			}
		}
		public void WriteAsciiArray(object[] values, int codepage)
		{
			WriteAsciiArray(values, 0, values.Length, codepage);
		}
		public void WriteAsciiArray(object[] values, int index, int count, int codepage)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteAscii(values[x].ToString(), codepage);
				}
			}
		}
		public void WriteAsciiArray(object[] values)
		{
			WriteAsciiArray(values, 0, values.Length, 1252);
		}
		public void WriteAsciiArray(object[] values, int index, int count)
		{
			WriteAsciiArray(values, index, count, 1252);
		}
		public void WriteUnicodeArray(object[] values)
		{
			WriteUnicodeArray(values, 0, values.Length);
		}
		public void WriteUnicodeArray(object[] values, int index, int count)
		{
			lock (m_lock)
			{
				if (m_locked)
				{
					throw new Exception("Cannot Write to a locked Packet.");
				}
				for (int x = index; x < index + count; ++x)
				{
					WriteUnicode(values[x].ToString());
				}
			}
		}
		public static string ToStringHexadecimal(byte[] bytes)
		{
			if (bytes.Length == 0)
				return "";
			StringBuilder hexData = new StringBuilder();
			foreach (byte b in bytes)
				hexData.Append(b.ToString("X2") + " ");
			// remove the last empty space
			return hexData.Remove(hexData.Length - 1, 1).ToString();
		}
		public override string ToString()
		{
			return "[" + Opcode.ToString("X4") + "][" + ToStringHexadecimal(GetBytes()) + "]";
    }
	}
}
#endregion


#region PacketReader
namespace SecurityAPI
{
    internal class PacketReader : BinaryReader
    {
        byte[] m_input;

        public PacketReader(byte[] input)
            : base(new MemoryStream(input, false))
        {
            m_input = input;
        }

        public PacketReader(byte[] input, int index, int count)
            : base(new MemoryStream(input, index, count, false))
        {
            m_input = input;
        }
    }
}
#endregion


#region PacketWriter
namespace SecurityAPI
{
    internal class PacketWriter : BinaryWriter
    {
        MemoryStream m_ms;

        public PacketWriter()
        {
            m_ms = new MemoryStream();
            this.OutStream = m_ms;
        }

        public byte[] GetBytes()
        {
            return m_ms.ToArray();
        }
    }
}
#endregion


#region Security
namespace SecurityAPI
{
    public class Security
    {
        #region SecurityFlags
        // Security flags container
        [StructLayout(LayoutKind.Explicit, Size = 8, CharSet = CharSet.Ansi)]
        public class SecurityFlags
        {
            [FieldOffset(0)]
            public byte none;
            [FieldOffset(1)]
            public byte blowfish;
            [FieldOffset(2)]
            public byte security_bytes;
            [FieldOffset(3)]
            public byte handshake;
            [FieldOffset(4)]
            public byte handshake_response;
            [FieldOffset(5)]
            public byte _6;
            [FieldOffset(6)]
            public byte _7;
            [FieldOffset(7)]
            public byte _8;
        }

        static SecurityFlags CopySecurityFlags(SecurityFlags flags)
        {
            SecurityFlags copy = new SecurityFlags();
            copy.none = flags.none;
            copy.blowfish = flags.blowfish;
            copy.security_bytes = flags.security_bytes;
            copy.handshake = flags.handshake;
            copy.handshake_response = flags.handshake_response;
            copy._6 = flags._6;
            copy._7 = flags._7;
            copy._8 = flags._8;
            return copy;
        }

        // Returns a byte from a SecurityFlags object.
        static byte FromSecurityFlags(SecurityFlags flags)
        {
            return (byte)(flags.none | flags.blowfish << 1 | flags.security_bytes << 2 | flags.handshake << 3 | flags.handshake_response << 4 | flags._6 << 5 | flags._7 << 6 | flags._8 << 7);
        }

        // Returns a SecurityFlags object from a byte.
        static SecurityFlags ToSecurityFlags(byte value)
        {
            SecurityFlags flags = new SecurityFlags();
            flags.none = (byte)(value & 1);
            value >>= 1;
            flags.blowfish = (byte)(value & 1);
            value >>= 1;
            flags.security_bytes = (byte)(value & 1);
            value >>= 1;
            flags.handshake = (byte)(value & 1);
            value >>= 1;
            flags.handshake_response = (byte)(value & 1);
            value >>= 1;
            flags._6 = (byte)(value & 1);
            value >>= 1;
            flags._7 = (byte)(value & 1);
            value >>= 1;
            flags._8 = (byte)(value & 1);
            value >>= 1;
            return flags;
        }
        #endregion

        #region SecurityTable
        // Generates the crc bytes lookup table
        static uint[] GenerateSecurityTable()
        {
            uint[] security_table = new uint[0x10000];
            byte[] base_security_table = 
            {
                0xB1, 0xD6, 0x8B, 0x96, 0x96, 0x30, 0x07, 0x77, 0x2C, 0x61, 0x0E, 0xEE, 0xBA, 0x51, 0x09, 0x99, 
                0x19, 0xC4, 0x6D, 0x07, 0x8F, 0xF4, 0x6A, 0x70, 0x35, 0xA5, 0x63, 0xE9, 0xA3, 0x95, 0x64, 0x9E,
                0x32, 0x88, 0xDB, 0x0E, 0xA4, 0xB8, 0xDC, 0x79, 0x1E, 0xE9, 0xD5, 0xE0, 0x88, 0xD9, 0xD2, 0x97, 
                0x2B, 0x4C, 0xB6, 0x09, 0xBD, 0x7C, 0xB1, 0x7E, 0x07, 0x2D, 0xB8, 0xE7, 0x91, 0x1D, 0xBF, 0x90,
                0x64, 0x10, 0xB7, 0x1D, 0xF2, 0x20, 0xB0, 0x6A, 0x48, 0x71, 0xB1, 0xF3, 0xDE, 0x41, 0xBE, 0x8C, 
                0x7D, 0xD4, 0xDA, 0x1A, 0xEB, 0xE4, 0xDD, 0x6D, 0x51, 0xB5, 0xD4, 0xF4, 0xC7, 0x85, 0xD3, 0x83,
                0x56, 0x98, 0x6C, 0x13, 0xC0, 0xA8, 0x6B, 0x64, 0x7A, 0xF9, 0x62, 0xFD, 0xEC, 0xC9, 0x65, 0x8A, 
                0x4F, 0x5C, 0x01, 0x14, 0xD9, 0x6C, 0x06, 0x63, 0x63, 0x3D, 0x0F, 0xFA, 0xF5, 0x0D, 0x08, 0x8D,
                0xC8, 0x20, 0x6E, 0x3B, 0x5E, 0x10, 0x69, 0x4C, 0xE4, 0x41, 0x60, 0xD5, 0x72, 0x71, 0x67, 0xA2, 
                0xD1, 0xE4, 0x03, 0x3C, 0x47, 0xD4, 0x04, 0x4B, 0xFD, 0x85, 0x0D, 0xD2, 0x6B, 0xB5, 0x0A, 0xA5,
                0xFA, 0xA8, 0xB5, 0x35, 0x6C, 0x98, 0xB2, 0x42, 0xD6, 0xC9, 0xBB, 0xDB, 0x40, 0xF9, 0xBC, 0xAC, 
                0xE3, 0x6C, 0xD8, 0x32, 0x75, 0x5C, 0xDF, 0x45, 0xCF, 0x0D, 0xD6, 0xDC, 0x59, 0x3D, 0xD1, 0xAB,
                0xAC, 0x30, 0xD9, 0x26, 0x3A, 0x00, 0xDE, 0x51, 0x80, 0x51, 0xD7, 0xC8, 0x16, 0x61, 0xD0, 0xBF, 
                0xB5, 0xF4, 0xB4, 0x21, 0x23, 0xC4, 0xB3, 0x56, 0x99, 0x95, 0xBA, 0xCF, 0x0F, 0xA5, 0xB7, 0xB8,
                0x9E, 0xB8, 0x02, 0x28, 0x08, 0x88, 0x05, 0x5F, 0xB2, 0xD9, 0xEC, 0xC6, 0x24, 0xE9, 0x0B, 0xB1, 
                0x87, 0x7C, 0x6F, 0x2F, 0x11, 0x4C, 0x68, 0x58, 0xAB, 0x1D, 0x61, 0xC1, 0x3D, 0x2D, 0x66, 0xB6,
                0x90, 0x41, 0xDC, 0x76, 0x06, 0x71, 0xDB, 0x01, 0xBC, 0x20, 0xD2, 0x98, 0x2A, 0x10, 0xD5, 0xEF, 
                0x89, 0x85, 0xB1, 0x71, 0x1F, 0xB5, 0xB6, 0x06, 0xA5, 0xE4, 0xBF, 0x9F, 0x33, 0xD4, 0xB8, 0xE8,
                0xA2, 0xC9, 0x07, 0x78, 0x34, 0xF9, 0xA0, 0x0F, 0x8E, 0xA8, 0x09, 0x96, 0x18, 0x98, 0x0E, 0xE1, 
                0xBB, 0x0D, 0x6A, 0x7F, 0x2D, 0x3D, 0x6D, 0x08, 0x97, 0x6C, 0x64, 0x91, 0x01, 0x5C, 0x63, 0xE6,
                0xF4, 0x51, 0x6B, 0x6B, 0x62, 0x61, 0x6C, 0x1C, 0xD8, 0x30, 0x65, 0x85, 0x4E, 0x00, 0x62, 0xF2, 
                0xED, 0x95, 0x06, 0x6C, 0x7B, 0xA5, 0x01, 0x1B, 0xC1, 0xF4, 0x08, 0x82, 0x57, 0xC4, 0x0F, 0xF5,
                0xC6, 0xD9, 0xB0, 0x63, 0x50, 0xE9, 0xB7, 0x12, 0xEA, 0xB8, 0xBE, 0x8B, 0x7C, 0x88, 0xB9, 0xFC, 
                0xDF, 0x1D, 0xDD, 0x62, 0x49, 0x2D, 0xDA, 0x15, 0xF3, 0x7C, 0xD3, 0x8C, 0x65, 0x4C, 0xD4, 0xFB,
                0x58, 0x61, 0xB2, 0x4D, 0xCE, 0x51, 0xB5, 0x3A, 0x74, 0x00, 0xBC, 0xA3, 0xE2, 0x30, 0xBB, 0xD4, 
                0x41, 0xA5, 0xDF, 0x4A, 0xD7, 0x95, 0xD8, 0x3D, 0x6D, 0xC4, 0xD1, 0xA4, 0xFB, 0xF4, 0xD6, 0xD3,
                0x6A, 0xE9, 0x69, 0x43, 0xFC, 0xD9, 0x6E, 0x34, 0x46, 0x88, 0x67, 0xAD, 0xD0, 0xB8, 0x60, 0xDA, 
                0x73, 0x2D, 0x04, 0x44, 0xE5, 0x1D, 0x03, 0x33, 0x5F, 0x4C, 0x0A, 0xAA, 0xC9, 0x7C, 0x0D, 0xDD,
                0x3C, 0x71, 0x05, 0x50, 0xAA, 0x41, 0x02, 0x27, 0x10, 0x10, 0x0B, 0xBE, 0x86, 0x20, 0x0C, 0xC9, 
                0x25, 0xB5, 0x68, 0x57, 0xB3, 0x85, 0x6F, 0x20, 0x09, 0xD4, 0x66, 0xB9, 0x9F, 0xE4, 0x61, 0xCE,
                0x0E, 0xF9, 0xDE, 0x5E, 0x08, 0xC9, 0xD9, 0x29, 0x22, 0x98, 0xD0, 0xB0, 0xB4, 0xA8, 0x57, 0xC7, 
                0x17, 0x3D, 0xB3, 0x59, 0x81, 0x0D, 0xB4, 0x3E, 0x3B, 0x5C, 0xBD, 0xB7, 0xAD, 0x6C, 0xBA, 0xC0,
                0x20, 0x83, 0xB8, 0xED, 0xB6, 0xB3, 0xBF, 0x9A, 0x0C, 0xE2, 0xB6, 0x03, 0x9A, 0xD2, 0xB1, 0x74, 
                0x39, 0x47, 0xD5, 0xEA, 0xAF, 0x77, 0xD2, 0x9D, 0x15, 0x26, 0xDB, 0x04, 0x83, 0x16, 0xDC, 0x73,
                0x12, 0x0B, 0x63, 0xE3, 0x84, 0x3B, 0x64, 0x94, 0x3E, 0x6A, 0x6D, 0x0D, 0xA8, 0x5A, 0x6A, 0x7A, 
                0x0B, 0xCF, 0x0E, 0xE4, 0x9D, 0xFF, 0x09, 0x93, 0x27, 0xAE, 0x00, 0x0A, 0xB1, 0x9E, 0x07, 0x7D,
                0x44, 0x93, 0x0F, 0xF0, 0xD2, 0xA2, 0x08, 0x87, 0x68, 0xF2, 0x01, 0x1E, 0xFE, 0xC2, 0x06, 0x69, 
                0x5D, 0x57, 0x62, 0xF7, 0xCB, 0x67, 0x65, 0x80, 0x71, 0x36, 0x6C, 0x19, 0xE7, 0x06, 0x6B, 0x6E,
                0x76, 0x1B, 0xD4, 0xFE, 0xE0, 0x2B, 0xD3, 0x89, 0x5A, 0x7A, 0xDA, 0x10, 0xCC, 0x4A, 0xDD, 0x67, 
                0x6F, 0xDF, 0xB9, 0xF9, 0xF9, 0xEF, 0xBE, 0x8E, 0x43, 0xBE, 0xB7, 0x17, 0xD5, 0x8E, 0xB0, 0x60,
                0xE8, 0xA3, 0xD6, 0xD6, 0x7E, 0x93, 0xD1, 0xA1, 0xC4, 0xC2, 0xD8, 0x38, 0x52, 0xF2, 0xDF, 0x4F, 
                0xF1, 0x67, 0xBB, 0xD1, 0x67, 0x57, 0xBC, 0xA6, 0xDD, 0x06, 0xB5, 0x3F, 0x4B, 0x36, 0xB2, 0x48,
                0xDA, 0x2B, 0x0D, 0xD8, 0x4C, 0x1B, 0x0A, 0xAF, 0xF6, 0x4A, 0x03, 0x36, 0x60, 0x7A, 0x04, 0x41, 
                0xC3, 0xEF, 0x60, 0xDF, 0x55, 0xDF, 0x67, 0xA8, 0xEF, 0x8E, 0x6E, 0x31, 0x79, 0x0E, 0x69, 0x46,
                0x8C, 0xB3, 0x51, 0xCB, 0x1A, 0x83, 0x63, 0xBC, 0xA0, 0xD2, 0x6F, 0x25, 0x36, 0xE2, 0x68, 0x52, 
                0x95, 0x77, 0x0C, 0xCC, 0x03, 0x47, 0x0B, 0xBB, 0xB9, 0x14, 0x02, 0x22, 0x2F, 0x26, 0x05, 0x55,
                0xBE, 0x3B, 0xB6, 0xC5, 0x28, 0x0B, 0xBD, 0xB2, 0x92, 0x5A, 0xB4, 0x2B, 0x04, 0x6A, 0xB3, 0x5C, 
                0xA7, 0xFF, 0xD7, 0xC2, 0x31, 0xCF, 0xD0, 0xB5, 0x8B, 0x9E, 0xD9, 0x2C, 0x1D, 0xAE, 0xDE, 0x5B,
                0xB0, 0x72, 0x64, 0x9B, 0x26, 0xF2, 0xE3, 0xEC, 0x9C, 0xA3, 0x6A, 0x75, 0x0A, 0x93, 0x6D, 0x02, 
                0xA9, 0x06, 0x09, 0x9C, 0x3F, 0x36, 0x0E, 0xEB, 0x85, 0x68, 0x07, 0x72, 0x13, 0x07, 0x00, 0x05,
                0x82, 0x48, 0xBF, 0x95, 0x14, 0x7A, 0xB8, 0xE2, 0xAE, 0x2B, 0xB1, 0x7B, 0x38, 0x1B, 0xB6, 0x0C, 
                0x9B, 0x8E, 0xD2, 0x92, 0x0D, 0xBE, 0xD5, 0xE5, 0xB7, 0xEF, 0xDC, 0x7C, 0x21, 0xDF, 0xDB, 0x0B,
                0x94, 0xD2, 0xD3, 0x86, 0x42, 0xE2, 0xD4, 0xF1, 0xF8, 0xB3, 0xDD, 0x68, 0x6E, 0x83, 0xDA, 0x1F, 
                0xCD, 0x16, 0xBE, 0x81, 0x5B, 0x26, 0xB9, 0xF6, 0xE1, 0x77, 0xB0, 0x6F, 0x77, 0x47, 0xB7, 0x18,
                0xE0, 0x5A, 0x08, 0x88, 0x70, 0x6A, 0x0F, 0xF1, 0xCA, 0x3B, 0x06, 0x66, 0x5C, 0x0B, 0x01, 0x11, 
                0xFF, 0x9E, 0x65, 0x8F, 0x69, 0xAE, 0x62, 0xF8, 0xD3, 0xFF, 0x6B, 0x61, 0x45, 0xCF, 0x6C, 0x16,
                0x78, 0xE2, 0x0A, 0xA0, 0xEE, 0xD2, 0x0D, 0xD7, 0x54, 0x83, 0x04, 0x4E, 0xC2, 0xB3, 0x03, 0x39, 
                0x61, 0x26, 0x67, 0xA7, 0xF7, 0x16, 0x60, 0xD0, 0x4D, 0x47, 0x69, 0x49, 0xDB, 0x77, 0x6E, 0x3E,
                0x4A, 0x6A, 0xD1, 0xAE, 0xDC, 0x5A, 0xD6, 0xD9, 0x66, 0x0B, 0xDF, 0x40, 0xF0, 0x3B, 0xD8, 0x37, 
                0x53, 0xAE, 0xBC, 0xA9, 0xC5, 0x9E, 0xBB, 0xDE, 0x7F, 0xCF, 0xB2, 0x47, 0xE9, 0xFF, 0xB5, 0x30,
                0x1C, 0xF9, 0xBD, 0xBD, 0x8A, 0xCD, 0xBA, 0xCA, 0x30, 0x9E, 0xB3, 0x53, 0xA6, 0xA3, 0xBC, 0x24, 
                0x05, 0x3B, 0xD0, 0xBA, 0xA3, 0x06, 0xD7, 0xCD, 0xE9, 0x57, 0xDE, 0x54, 0xBF, 0x67, 0xD9, 0x23,
                0x2E, 0x72, 0x66, 0xB3, 0xB8, 0x4A, 0x61, 0xC4, 0x02, 0x1B, 0x38, 0x5D, 0x94, 0x2B, 0x6F, 0x2B, 
                0x37, 0xBE, 0xCB, 0xB4, 0xA1, 0x8E, 0xCC, 0xC3, 0x1B, 0xDF, 0x0D, 0x5A, 0x8D, 0xED, 0x02, 0x2D,
            };

            using (MemoryStream in_memory_stream = new MemoryStream(base_security_table, false))
            {
                using (BinaryReader reader = new BinaryReader(in_memory_stream))
                {
                    int index = 0;
                    for (int edi = 0; edi < 1024; edi += 4)
                    {
                        uint edx = reader.ReadUInt32();
                        for (uint ecx = 0; ecx < 256; ++ecx)
                        {
                            uint eax = ecx >> 1;
                            if ((ecx & 1) != 0)
                            {
                                eax ^= edx;
                            }
                            for (int bit = 0; bit < 7; ++bit)
                            {
                                if ((eax & 1) != 0)
                                {
                                    eax >>= 1;
                                    eax ^= edx;
                                }
                                else
                                {
                                    eax >>= 1;
                                }
                            }
                            security_table[index++] = eax;
                        }
                    }
                }
            }

            return security_table;
        }

        // Use one security table for all objects.
        static uint[] global_security_table = GenerateSecurityTable();
        #endregion

        #region WIN32_Helper_Functions
        static ulong MAKELONGLONG_(uint a, uint b)
        {
            ulong a_ = a;
            ulong b_ = b;
            return (ulong)((b_ << 32) | a_);
        }

        static uint MAKELONG_(ushort a, ushort b)
        {
            uint a_ = a;
            uint b_ = b;
            return (uint)((b_ << 16) | a_);
        }

        static ushort MAKEWORD_(byte a, byte b)
        {
            ushort a_ = a;
            ushort b_ = b;
            return (ushort)((b_ << 8) | a_);
        }

        static ushort LOWORD_(uint a)
        {
            return (ushort)(a & 0xFFFF);
        }

        static ushort HIWORD_(uint a)
        {
            return (ushort)((a >> 16) & 0xFFFF);
        }

        static byte LOBYTE_(ushort a)
        {
            return (byte)(a & 0xFF);
        }

        static byte HIBYTE_(ushort a)
        {
            return (byte)((a >> 8) & 0xFF);
        }
        #endregion

        #region Random
        static Random random = new Random();

        static UInt64 NextUInt64()
        {
            var buffer = new byte[sizeof(UInt64)];
            random.NextBytes(buffer);
            return BitConverter.ToUInt64(buffer, 0);
        }

        static UInt32 NextUInt32()
        {
            var buffer = new byte[sizeof(UInt32)];
            random.NextBytes(buffer);
            return BitConverter.ToUInt32(buffer, 0);
        }

        static UInt16 NextUInt16()
        {
            var buffer = new byte[sizeof(UInt16)];
            random.NextBytes(buffer);
            return BitConverter.ToUInt16(buffer, 0);
        }

        static byte NextUInt8()
        {
            return (byte)(NextUInt16() & 0xFF);
        }
        #endregion

        uint m_value_x;
        uint m_value_g;
        uint m_value_p;
        uint m_value_A;
        uint m_value_B;
        uint m_value_K;
        uint m_seed_count;
        uint m_crc_seed;
        ulong m_initial_blowfish_key;
        ulong m_handshake_blowfish_key;
        byte[] m_count_byte_seeds;
        ulong m_client_key;
        ulong m_challenge_key;

        bool m_client_security;
        byte m_security_flag;
        SecurityFlags m_security_flags;
        bool m_accepted_handshake;
        public bool HasHandshake => m_accepted_handshake;
        bool m_started_handshake;
        byte m_identity_flag;
        String m_identity_name;

        List<Packet> m_incoming_packets;
        List<Packet> m_outgoing_packets;

        List<ushort> m_enc_opcodes;

        Blowfish m_blowfish;

        TransferBuffer m_recv_buffer;
        TransferBuffer m_current_buffer;

        ushort m_massive_count;
        Packet m_massive_packet;

        object m_class_lock;

        #region CoreSecurityFunction
        // This function's logic was written by jMerlin as part of the article "How to generate the security bytes for SRO"
        uint GenerateValue(ref uint val)
        {
            for (int i = 0; i < 32; ++i)
            {
                val = (((((((((((val >> 2) ^ val) >> 2) ^ val) >> 1) ^ val) >> 1) ^ val) >> 1) ^ val) & 1) | ((((val & 1) << 31) | (val >> 1)) & 0xFFFFFFFE);
            }
            return val;
        }

        // Sets up the count bytes
        // This function's logic was written by jMerlin as part of the article "How to generate the security bytes for SRO"
        void SetupCountByte(uint seed)
        {
            if (seed == 0) seed = 0x9ABFB3B6;
            uint mut = seed;
            uint mut1 = GenerateValue(ref mut);
            uint mut2 = GenerateValue(ref mut);
            uint mut3 = GenerateValue(ref mut);
            GenerateValue(ref mut);
            byte byte1 = (byte)((mut & 0xFF) ^ (mut3 & 0xFF));
            byte byte2 = (byte)((mut1 & 0xFF) ^ (mut2 & 0xFF));
            if (byte1 == 0) byte1 = 1;
            if (byte2 == 0) byte2 = 1;
            m_count_byte_seeds[0] = (byte)(byte1 ^ byte2);
            m_count_byte_seeds[1] = byte2;
            m_count_byte_seeds[2] = byte1;
        }

        // Helper function used in the handshake, X may be a or b, this clean version of the function is from jMerlin (Func_X_4)
        uint G_pow_X_mod_P(uint P, uint X, uint G)
        {
            long result = 1;
            long mult = G;
            if (X == 0)
            {
                return 1;
            }
            while (X != 0)
            {
                if ((X & 1) > 0)
                {
                    result = (mult * result) % P;
                }
                X = X >> 1;
                mult = (mult * mult) % P;
            }
            return (uint)result;
        }

        // Helper function used in the handshake (Func_X_2)
        void KeyTransformValue(ref ulong val, uint key, byte key_byte)
        {
            byte[] stream = BitConverter.GetBytes(val);
            stream[0] ^= (byte)(stream[0] + LOBYTE_(LOWORD_(key)) + key_byte);
            stream[1] ^= (byte)(stream[1] + HIBYTE_(LOWORD_(key)) + key_byte);
            stream[2] ^= (byte)(stream[2] + LOBYTE_(HIWORD_(key)) + key_byte);
            stream[3] ^= (byte)(stream[3] + HIBYTE_(HIWORD_(key)) + key_byte);
            stream[4] ^= (byte)(stream[4] + LOBYTE_(LOWORD_(key)) + key_byte);
            stream[5] ^= (byte)(stream[5] + HIBYTE_(LOWORD_(key)) + key_byte);
            stream[6] ^= (byte)(stream[6] + LOBYTE_(HIWORD_(key)) + key_byte);
            stream[7] ^= (byte)(stream[7] + HIBYTE_(HIWORD_(key)) + key_byte);
            val = BitConverter.ToUInt64(stream, 0);
        }

        // Function called to generate a count byte
        // This function's logic was written by jMerlin as part of the article "How to generate the security bytes for SRO"
        byte GenerateCountByte(bool update)
        {
            byte result = (byte)(m_count_byte_seeds[2] * (~m_count_byte_seeds[0] + m_count_byte_seeds[1]));
            result = (byte)(result ^ (result >> 4));
            if (update)
            {
                m_count_byte_seeds[0] = result;
            }
            return result;
        }

        // Function called to generate the crc byte
        // This function's logic was written by jMerlin as part of the article "How to generate the security bytes for SRO"
        byte GenerateCheckByte(byte[] stream, int offset, int length)
        {
            uint checksum = 0xFFFFFFFF;
            uint moddedseed = m_crc_seed << 8;
            for (int x = offset; x < offset + length; ++x)
            {
                checksum = (checksum >> 8) ^ global_security_table[moddedseed + (((uint)stream[x] ^ checksum) & 0xFF)];
            }
            return (byte)(((checksum >> 24) & 0xFF) + ((checksum >> 8) & 0xFF) + ((checksum >> 16) & 0xFF) + (checksum & 0xFF));
        }

        byte GenerateCheckByte(byte[] stream)
        {
            return GenerateCheckByte(stream, 0, stream.Length);
        }
        #endregion

        #region ExtendedSecurityFunctions
        public void GenerateSecurity(SecurityFlags flags)
        {
            m_security_flag = FromSecurityFlags(flags);
            m_security_flags = flags;
            m_client_security = true;

            Packet response = new Packet(0x5000);

            response.WriteByte(m_security_flag);

            if (m_security_flags.blowfish == 1)
            {
                m_initial_blowfish_key = NextUInt64();

                m_blowfish.Initialize(BitConverter.GetBytes(m_initial_blowfish_key));

                response.WriteULong(m_initial_blowfish_key);
            }
            if (m_security_flags.security_bytes == 1)
            {
                m_seed_count = NextUInt8();
                SetupCountByte(m_seed_count);
                m_crc_seed = NextUInt8();

                response.WriteUInt(m_seed_count);
                response.WriteUInt(m_crc_seed);
            }
            if (m_security_flags.handshake == 1)
            {
                m_handshake_blowfish_key = NextUInt64();
                m_value_x = NextUInt32() & 0x7FFFFFFF;
                m_value_g = NextUInt32() & 0x7FFFFFFF;
                m_value_p = NextUInt32() & 0x7FFFFFFF;
                m_value_A = G_pow_X_mod_P(m_value_p, m_value_x, m_value_g);

                response.WriteULong(m_handshake_blowfish_key);
                response.WriteUInt(m_value_g);
                response.WriteUInt(m_value_p);
                response.WriteUInt(m_value_A);
            }

            m_outgoing_packets.Add(response);
        }

        void Handshake(ushort packet_opcode, PacketReader packet_data, bool packet_encrypted)
        {
            if (packet_encrypted)
            {
                throw (new Exception("[SecurityAPI::Handshake] Received an illogical (encrypted) handshake packet."));
            }
            if (m_client_security)
            {
                // If this object does not need a handshake
                if (m_security_flags.handshake == 0)
                {
                    // Client should only accept it then
                    if (packet_opcode == 0x9000)
                    {
                        if (m_accepted_handshake)
                        {
                            throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (duplicate 0x9000)."));
                        }
                        m_accepted_handshake = true; // Otherwise, all good here
                        return;
                    }
                    // Client should not send any 0x5000s!
                    else if (packet_opcode == 0x5000)
                    {
                        throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (0x5000 with no handshake)."));
                    }
                    // Programmer made a mistake in calling this function
                    else
                    {
                        throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (programmer error)."));
                    }
                }
                else
                {
                    // Client accepts the handshake
                    if (packet_opcode == 0x9000)
                    {
                        // Can't accept it before it's started!
                        if (!m_started_handshake)
                        {
                            throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (out of order 0x9000)."));
                        }
                        if (m_accepted_handshake) // Client error
                        {
                            throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (duplicate 0x9000)."));
                        }
                        // Otherwise, all good here
                        m_accepted_handshake = true;
                        return;
                    }
                    // Client sends a handshake response
                    else if (packet_opcode == 0x5000)
                    {
                        if (m_started_handshake) // Client error
                        {
                            throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (duplicate 0x5000)."));
                        }
                        m_started_handshake = true;
                    }
                    // Programmer made a mistake in calling this function
                    else
                    {
                        throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (programmer error)."));
                    }
                }

                ulong key_array = 0;
                byte[] tmp_bytes;

                m_value_B = packet_data.ReadUInt32();
                m_client_key = packet_data.ReadUInt64();

                m_value_K = G_pow_X_mod_P(m_value_p, m_value_x, m_value_B);

                key_array = MAKELONGLONG_(m_value_A, m_value_B);
                KeyTransformValue(ref key_array, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_K)) & 0x03));
                m_blowfish.Initialize(BitConverter.GetBytes(key_array));

                tmp_bytes = m_blowfish.Decode(BitConverter.GetBytes(m_client_key));
                m_client_key = BitConverter.ToUInt64(tmp_bytes, 0);

                key_array = MAKELONGLONG_(m_value_B, m_value_A);
                KeyTransformValue(ref key_array, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_B)) & 0x07));
                if (m_client_key != key_array)
                {
                    throw (new Exception("[SecurityAPI::Handshake] Client signature error."));
                }

                key_array = MAKELONGLONG_(m_value_A, m_value_B);
                KeyTransformValue(ref key_array, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_K)) & 0x03));
                m_blowfish.Initialize(BitConverter.GetBytes(key_array));

                m_challenge_key = MAKELONGLONG_(m_value_A, m_value_B);
                KeyTransformValue(ref m_challenge_key, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_A)) & 0x07));
                tmp_bytes = m_blowfish.Encode(BitConverter.GetBytes(m_challenge_key));
                m_challenge_key = BitConverter.ToUInt64(tmp_bytes, 0);

                KeyTransformValue(ref m_handshake_blowfish_key, m_value_K, 0x3);
                m_blowfish.Initialize(BitConverter.GetBytes(m_handshake_blowfish_key));

                SecurityFlags tmp_flags = new SecurityFlags();
                tmp_flags.handshake_response = 1;
                byte tmp_flag = FromSecurityFlags(tmp_flags);

                Packet response = new Packet(0x5000);
                response.WriteByte(tmp_flag);
                response.WriteULong(m_challenge_key);
                m_outgoing_packets.Add(response);
            }
            else
            {
                if (packet_opcode != 0x5000)
                {
                    throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (programmer error)."));
                }

                byte flag = packet_data.ReadByte();

                SecurityFlags flags = ToSecurityFlags(flag);

                if (m_security_flag == 0)
                {
                    m_security_flag = flag;
                    m_security_flags = flags;
                }

                if (flags.blowfish == 1)
                {
                    m_initial_blowfish_key = packet_data.ReadUInt64();
                    m_blowfish.Initialize(BitConverter.GetBytes(m_initial_blowfish_key));
                }

                if (flags.security_bytes == 1)
                {
                    m_seed_count = packet_data.ReadUInt32();
                    m_crc_seed = packet_data.ReadUInt32();
                    SetupCountByte(m_seed_count);
                }

                if (flags.handshake == 1)
                {
                    m_handshake_blowfish_key = packet_data.ReadUInt64();
                    m_value_g = packet_data.ReadUInt32();
                    m_value_p = packet_data.ReadUInt32();
                    m_value_A = packet_data.ReadUInt32();

                    m_value_x = NextUInt32() & 0x7FFFFFFF;

                    m_value_B = G_pow_X_mod_P(m_value_p, m_value_x, m_value_g);
                    m_value_K = G_pow_X_mod_P(m_value_p, m_value_x, m_value_A);

                    ulong key_array = MAKELONGLONG_(m_value_A, m_value_B);
                    KeyTransformValue(ref key_array, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_K)) & 0x03));
                    m_blowfish.Initialize(BitConverter.GetBytes(key_array));

                    m_client_key = MAKELONGLONG_(m_value_B, m_value_A);
                    KeyTransformValue(ref m_client_key, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_B)) & 0x07));
                    byte[] tmp_bytes = m_blowfish.Encode(BitConverter.GetBytes(m_client_key));
                    m_client_key = BitConverter.ToUInt64(tmp_bytes, 0);
                }

                if (flags.handshake_response == 1)
                {
                    m_challenge_key = packet_data.ReadUInt64();

                    ulong expected_challenge_key = MAKELONGLONG_(m_value_A, m_value_B);
                    KeyTransformValue(ref expected_challenge_key, m_value_K, (byte)(LOBYTE_(LOWORD_(m_value_A)) & 0x07));
                    byte[] tmp_bytes = m_blowfish.Encode(BitConverter.GetBytes(expected_challenge_key));
                    expected_challenge_key = BitConverter.ToUInt64(tmp_bytes, 0);

                    if (m_challenge_key != expected_challenge_key)
                    {
                        throw (new Exception("[SecurityAPI::Handshake] Server signature error."));
                    }

                    KeyTransformValue(ref m_handshake_blowfish_key, m_value_K, 0x3);
                    m_blowfish.Initialize(BitConverter.GetBytes(m_handshake_blowfish_key));
                }

                // Generate the outgoing packet now
                if (flags.handshake == 1 && flags.handshake_response == 0)
                {
                    // Check to see if we already started a handshake
                    if (m_started_handshake || m_accepted_handshake)
                    {
                        throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (duplicate 0x5000)."));
                    }

                    // Handshake challenge
                    Packet response = new Packet(0x5000);
                    response.WriteUInt(m_value_B);
                    response.WriteULong(m_client_key);
                    m_outgoing_packets.Insert(0, response);

                    // The handshake has started
                    m_started_handshake = true;
                }
                else
                {
                    // Check to see if we already accepted a handshake
                    if (m_accepted_handshake)
                    {
                        throw (new Exception("[SecurityAPI::Handshake] Received an illogical handshake packet (duplicate 0x5000)."));
                    }

                    // Handshake accepted
                    Packet response1 = new Packet(0x9000);

                    // Identify
                    Packet response2 = new Packet(0x2001, true, false);
                    response2.WriteAscii(m_identity_name);
                    response2.WriteByte(m_identity_flag);

                    // Insert at the front, we want 0x9000 first, then 0x2001
                    m_outgoing_packets.Insert(0, response2);
                    m_outgoing_packets.Insert(0, response1);

                    // Mark the handshake as accepted now
                    m_started_handshake = true;
                    m_accepted_handshake = true;
                }
            }
        }

        public byte[] FormatPacket(ushort opcode, byte[] data, bool encrypted)
        {
            // Sanity check
            if (data.Length >= 0x8000)
            {
                throw (new Exception("[SecurityAPI::FormatPacket] Payload is too large!"));
            }

            ushort data_length = (ushort)data.Length;

            // Add the packet header to the start of the data
            PacketWriter writer = new PacketWriter();
            writer.Write(data_length); // packet size
            writer.Write(opcode); // packet opcode
            writer.Write((ushort)0); // packet security bytes
            writer.Write(data);
            writer.Flush();

            // Determine if we need to mark the packet size as encrypted
            if (encrypted && (m_security_flags.blowfish == 1 || (m_security_flags.security_bytes == 1 && m_security_flags.blowfish == 0)))
            {
                long seek_index = writer.BaseStream.Seek(0, SeekOrigin.Current);

                ushort packet_size = (ushort)(data_length | 0x8000);

                writer.BaseStream.Seek(0, SeekOrigin.Begin);
                writer.Write((ushort)packet_size);
                writer.Flush();

                writer.BaseStream.Seek(seek_index, SeekOrigin.Begin);
            }

            // Only need to stamp bytes if this is a clientless object
            if (m_client_security == false && m_security_flags.security_bytes == 1)
            {
                long seek_index = writer.BaseStream.Seek(0, SeekOrigin.Current);

                byte sb1 = GenerateCountByte(true);
                writer.BaseStream.Seek(4, SeekOrigin.Begin);
                writer.Write(sb1);
                writer.Flush();

                byte sb2 = GenerateCheckByte(writer.GetBytes());
                writer.BaseStream.Seek(5, SeekOrigin.Begin);
                writer.Write(sb2);
                writer.Flush();

                writer.BaseStream.Seek(seek_index, SeekOrigin.Begin);
            }

            // If the packet should be physically encrypted, return an encrypted version of it
            if (encrypted && m_security_flags.blowfish == 1)
            {
                byte[] raw_data = writer.GetBytes();
                byte[] encrypted_data = m_blowfish.Encode(raw_data, 2, raw_data.Length - 2);

                writer.BaseStream.Seek(2, SeekOrigin.Begin);

                writer.Write(encrypted_data);
                writer.Flush();
            }
            else
            {
                // Determine if we need to unmark the packet size from being encrypted but not physically encrypted
                if (encrypted && (m_security_flags.security_bytes == 1 && m_security_flags.blowfish == 0))
                {
                    long seek_index = writer.BaseStream.Seek(0, SeekOrigin.Current);

                    writer.BaseStream.Seek(0, SeekOrigin.Begin);
                    writer.Write((ushort)data_length);
                    writer.Flush();

                    writer.BaseStream.Seek(seek_index, SeekOrigin.Begin);
                }
            }

            // Return the final data
            return writer.GetBytes();
        }

        public bool HasPacketToSend()
        {
            // No packets, easy case
            if (m_outgoing_packets.Count == 0)
            {
                return false;
            }

            // If we have packets and have accepted the handshake, we can send whenever,
            // so return true.
            if (m_accepted_handshake)
            {
                return true;
            }

            // Otherwise, check to see if we have pending handshake packets to send
            Packet packet = m_outgoing_packets[0];
            if (packet.Opcode == 0x5000 || packet.Opcode == 0x9000)
            {
                return true;
            }

            // If we get here, we have out of order packets that cannot be sent yet.
            return false;
        }

        public KeyValuePair<TransferBuffer, Packet> GetPacketToSend()
        {
            if (m_outgoing_packets.Count == 0)
            {               
                throw (new Exception("[SecurityAPI::GetPacketToSend] No packets are avaliable to send."));
            }

            Packet packet = m_outgoing_packets[0];
            m_outgoing_packets.RemoveAt(0);

            if (packet.Massive)
            {
                ushort parts = 0;

                PacketWriter final = new PacketWriter();
                PacketWriter final_data = new PacketWriter();

                byte[] input_data = packet.GetBytes();
                PacketReader input_reader = new PacketReader(input_data);

                TransferBuffer workspace = new TransferBuffer(4089, 0, (int)input_data.Length);

                while (workspace.Size > 0)
                {
                    PacketWriter part_data = new PacketWriter();

                    int cur_size = workspace.Size > 4089 ? 4089 : workspace.Size; // Max buffer size is 4kb for the client

                    part_data.Write((byte)0); // Data flag

                    part_data.Write(input_data, workspace.Offset, cur_size);

                    workspace.Offset += cur_size;
                    workspace.Size -= cur_size; // Update the size

                    final_data.Write(FormatPacket(0x600D, part_data.GetBytes(), false));

                    ++parts; // Track how many parts there are
                }

                // Write the final header packet to the front of the packet
                PacketWriter final_header = new PacketWriter();
                final_header.Write((byte)1); // Header flag
                final_header.Write((short)parts);
                final_header.Write(packet.Opcode);
                final.Write(FormatPacket(0x600D, final_header.GetBytes(), false));

                // Finish the large packet of all the data
                final.Write(final_data.GetBytes());

                // Return the collated data
                byte[] raw_bytes = final.GetBytes();
                packet.Lock();
                return new KeyValuePair<TransferBuffer, Packet>(new TransferBuffer(raw_bytes, 0, raw_bytes.Length, true), packet);
            }
            else
            {
                bool encrypted = packet.Encrypted;
                if (!m_client_security)
                {
                    if (m_enc_opcodes.Contains(packet.Opcode))
                    {
                        encrypted = true;
                    }
                }
                byte[] raw_bytes = FormatPacket(packet.Opcode, packet.GetBytes(), encrypted);
                packet.Lock();
                return new KeyValuePair<TransferBuffer, Packet>(new TransferBuffer(raw_bytes, 0, raw_bytes.Length, true), packet);
            }
        }
        #endregion

        // Default constructor
        public Security()
        {
            m_value_x = 0;
            m_value_g = 0;
            m_value_p = 0;
            m_value_A = 0;
            m_value_B = 0;
            m_value_K = 0;
            m_seed_count = 0;
            m_crc_seed = 0;
            m_initial_blowfish_key = 0;
            m_handshake_blowfish_key = 0;
            m_count_byte_seeds = new byte[3];
            m_count_byte_seeds[0] = 0;
            m_count_byte_seeds[1] = 0;
            m_count_byte_seeds[2] = 0;
            m_client_key = 0;
            m_challenge_key = 0;

            m_client_security = false;
            m_security_flag = 0;
            m_security_flags = new SecurityFlags();
            m_accepted_handshake = false;
            m_started_handshake = false;
            m_identity_flag = 0;
            m_identity_name = "SR_Client";

            m_outgoing_packets = new List<Packet>();
            m_incoming_packets = new List<Packet>();

            m_enc_opcodes = new List<ushort>();
            m_enc_opcodes.Add(0x2001);
            m_enc_opcodes.Add(0x6100);
            m_enc_opcodes.Add(0x6101);
            m_enc_opcodes.Add(0x6102);
            m_enc_opcodes.Add(0x6103);
            m_enc_opcodes.Add(0x6107);

            m_blowfish = new Blowfish();

            m_recv_buffer = new TransferBuffer(8192); // must be at minimal 2 bytes!
            m_current_buffer = null;

            m_massive_count = 0;
            m_massive_packet = null;

            m_class_lock = new object();
        }

        // Changes the 0x2001 identify packet data that will be sent out by
        // this security object.
        public void ChangeIdentity(string name, byte flag)
        {
            lock (m_class_lock)
            {
                m_identity_name = name;
                m_identity_flag = flag;
            }
        }

        // Generates the security settings. This should only be called if the security object
        // is being used to process an incoming connection's data (server).
        public void GenerateSecurity(bool blowfish, bool security_bytes, bool handshake)
        {
            lock (m_class_lock)
            {
                SecurityFlags flags = new SecurityFlags();
                if (blowfish)
                {
                    flags.none = 0;
                    flags.blowfish = 1;
                }
                if (security_bytes)
                {
                    flags.none = 0;
                    flags.security_bytes = 1;
                }
                if (handshake)
                {
                    flags.none = 0;
                    flags.handshake = 1;
                }
                if (!blowfish && !security_bytes && !handshake)
                {
                    flags.none = 1;
                }
                GenerateSecurity(flags);
            }
        }

        // Adds an encrypted opcode to the API. Any opcodes registered here will automatically
        // be encrypted as needed. Users should add the current Item Use opcode if they will be
        // mixing security modes.
        public void AddEncryptedOpcode(ushort opcode)
        {
            lock (m_class_lock)
            {
                if (m_enc_opcodes.Contains(opcode) == false)
                {
                    m_enc_opcodes.Add(opcode);
                }
            }
        }

        // Queues a packet for processing to be sent. The data is simply formatted and processed during
        // the next call to TransferOutgoing.
        public void Send(Packet packet)
        {
            if (packet.Opcode == 0x5000 || packet.Opcode == 0x9000)
            {
                throw (new Exception("[SecurityAPI::Send] Handshake packets cannot be sent through this function."));
            }
            lock (m_class_lock)
            {
                m_outgoing_packets.Add(packet);
            }
        }

        // Transfers raw incoming data into the security object. Call TransferIncoming to
        // obtain a list of ready to process packets.
        public void Recv(byte[] buffer, int offset, int length)
        {
            Recv(new TransferBuffer(buffer, offset, length, true));
        }

        // Transfers raw incoming data into the security object. Call TransferIncoming to
        // obtain a list of ready to process packets.
        public void Recv(TransferBuffer raw_buffer)
        {
            List<TransferBuffer> incoming_buffers_tmp = new List<TransferBuffer>();
            lock (m_class_lock)
            {
                int length = raw_buffer.Size - raw_buffer.Offset;
                int index = 0;
                while (length > 0)
                {
                    int max_length = length;
                    int calc_length = m_recv_buffer.Buffer.Length - m_recv_buffer.Size;

                    if (max_length > calc_length)
                    {
                        max_length = calc_length;
                    }
                    length -= max_length;

                    Buffer.BlockCopy(raw_buffer.Buffer, raw_buffer.Offset + index, m_recv_buffer.Buffer, m_recv_buffer.Size, max_length);

                    m_recv_buffer.Size += max_length;
                    index += max_length;

                    // Loop while we have data to process
                    while (m_recv_buffer.Size > 0)
                    {
                        // If we do not have a current packet object, try to allocate one.
                        if (m_current_buffer == null)
                        {
                            // We need at least two bytes to allocate a packet.
                            if (m_recv_buffer.Size < 2)
                            {
                                break;
                            }

                            // Calculate the packet size.
                            int packet_size = m_recv_buffer.Buffer[1] << 8 | m_recv_buffer.Buffer[0];

                            // Check to see if this packet is encrypted.
                            if ((packet_size & 0x8000) > 0)
                            {
                                // If so, calculate the total payload size.
                                packet_size &= 0x7FFF; // Mask off the encryption.
                                if (m_security_flags.blowfish == 1)
                                {
                                    packet_size = 2 + m_blowfish.GetOutputLength(packet_size + 4);
                                }
                                else
                                {
                                    packet_size += 6;
                                }
                            }
                            else
                            {
                                // The packet is unencrypted. The final size is simply
                                // header size + payload size.
                                packet_size += 6;
                            }

                            // Allocate the final buffer the packet will be written to
                            m_current_buffer = new TransferBuffer(packet_size, 0, packet_size);
                        }

                        // Calculate how many bytes are left to receive in the packet.
                        int max_copy_count = m_current_buffer.Size - m_current_buffer.Offset;

                        // If we need more bytes than we currently have, update the size.
                        if (max_copy_count > m_recv_buffer.Size)
                        {
                            max_copy_count = m_recv_buffer.Size;
                        }

                        // Copy the buffer data to the packet buffer
                        Buffer.BlockCopy(m_recv_buffer.Buffer, 0, m_current_buffer.Buffer, m_current_buffer.Offset, max_copy_count);

                        // Update how many bytes we now have
                        m_current_buffer.Offset += max_copy_count;
                        m_recv_buffer.Size -= max_copy_count;

                        // If there is data remaining in the buffer, copy it over the data
                        // we just removed (sliding buffer).
                        if (m_recv_buffer.Size > 0)
                        {
                            Buffer.BlockCopy(m_recv_buffer.Buffer, max_copy_count, m_recv_buffer.Buffer, 0, m_recv_buffer.Size);
                        }

                        // Check to see if the current packet is now complete.
                        if (m_current_buffer.Size == m_current_buffer.Offset)
                        {
                            // If so, dispatch it to the manager class for processing by the system.
                            m_current_buffer.Offset = 0;
                            incoming_buffers_tmp.Add(m_current_buffer);

                            // Set the current packet to null so we can process the next packet
                            // in the stream.
                            m_current_buffer = null;
                        }
                        else
                        {
                            // Otherwise, we are done with this loop, since we need more
                            // data for the current packet.
                            break;
                        }
                    }
                }

                if (incoming_buffers_tmp.Count > 0)
                {
                    foreach (TransferBuffer buffer in incoming_buffers_tmp)
                    {
                        bool packet_encrypted = false;

                        int packet_size = buffer.Buffer[1] << 8 | buffer.Buffer[0];
                        if ((packet_size & 0x8000) > 0)
                        {
                            if (m_security_flags.blowfish == 1)
                            {
                                packet_size &= 0x7FFF;
                                packet_encrypted = true;
                            }
                            else
                            {
                                packet_size &= 0x7FFF;
                            }
                        }

                        if (packet_encrypted)
                        {
                            byte[] decrypted = m_blowfish.Decode(buffer.Buffer, 2, buffer.Size - 2);
                            byte[] new_buffer = new byte[6 + packet_size];
                            Buffer.BlockCopy(BitConverter.GetBytes((ushort)packet_size), 0, new_buffer, 0, 2);
                            Buffer.BlockCopy(decrypted, 0, new_buffer, 2, 4 + packet_size);
                            buffer.Buffer = null;
                            buffer.Buffer = new_buffer;
                        }

                        PacketReader packet_data = new PacketReader(buffer.Buffer);
                        packet_size = packet_data.ReadUInt16();
                        ushort packet_opcode = packet_data.ReadUInt16();
                        byte packet_security_count = packet_data.ReadByte();
                        byte packet_security_crc = packet_data.ReadByte();

                        // Client object whose bytes the server might need to verify
                        if (m_client_security)
                        {
                            if (m_security_flags.security_bytes == 1)
                            {
                                byte expected_count = GenerateCountByte(true);
                                if (packet_security_count != expected_count)
                                {
                                    throw (new Exception("[SecurityAPI::Recv] Count byte mismatch."));
                                }

                                if (packet_encrypted || (m_security_flags.security_bytes == 1 && m_security_flags.blowfish == 0))
                                {
                                    if (packet_encrypted || m_enc_opcodes.Contains(packet_opcode))
                                    {
                                        packet_size |= 0x8000;
                                        Buffer.BlockCopy(BitConverter.GetBytes((ushort)packet_size), 0, buffer.Buffer, 0, 2);
                                    }
                                }

                                buffer.Buffer[5] = 0;

                                byte expected_crc = GenerateCheckByte(buffer.Buffer);
                                if (packet_security_crc != expected_crc)
                                {
                                    throw (new Exception("[SecurityAPI::Recv] CRC byte mismatch."));
                                }

                                buffer.Buffer[4] = 0;

                                if (packet_encrypted || (m_security_flags.security_bytes == 1 && m_security_flags.blowfish == 0))
                                {
                                    if (packet_encrypted || m_enc_opcodes.Contains(packet_opcode))
                                    {
                                        packet_size &= 0x7FFF;
                                        Buffer.BlockCopy(BitConverter.GetBytes((ushort)packet_size), 0, buffer.Buffer, 0, 2);
                                    }
                                }
                            }
                        }

                        if (packet_opcode == 0x5000 || packet_opcode == 0x9000) // New logic processing!
                        {
                            Handshake(packet_opcode, packet_data, packet_encrypted);

                            // Pass the handshake packets to the user so they can at least see them.
                            // They do not need to actually do anything with them. This was added to
                            // help debugging and make output logs complete.

                            Packet packet = new Packet(packet_opcode, packet_encrypted, false, buffer.Buffer, 6, packet_size);
                            packet.Lock();
                            m_incoming_packets.Add(packet);
                        }
                        else
                        {
                            if (m_client_security)
                            {
                                // Make sure the client accepted the security system first
                                if (!m_accepted_handshake)
                                {
                                    throw (new Exception("[SecurityAPI::Recv] The client has not accepted the handshake."));
                                }
                            }
                            if (packet_opcode == 0x600D) // Auto process massive messages for the user
                            {
                                byte mode = packet_data.ReadByte();
                                if (mode == 1)
                                {
                                    m_massive_count = packet_data.ReadUInt16();
                                    ushort contained_packet_opcode = packet_data.ReadUInt16();
                                    m_massive_packet = new Packet(contained_packet_opcode, packet_encrypted, true);
                                }
                                else
                                {
                                    if (m_massive_packet == null)
                                    {
                                        throw (new Exception("[SecurityAPI::Recv] A malformed 0x600D packet was received."));
                                    }
                                    m_massive_packet.WriteByteArray(packet_data.ReadBytes(packet_size - 1));
                                    m_massive_count--;
                                    if (m_massive_count == 0)
                                    {
                                        m_massive_packet.Lock();
                                        m_incoming_packets.Add(m_massive_packet);
                                        m_massive_packet = null;
                                    }
                                }
                            }
                            else
                            {
                                Packet packet = new Packet(packet_opcode, packet_encrypted, false, buffer.Buffer, 6, packet_size);
                                packet.Lock();
                                m_incoming_packets.Add(packet);
                            }
                        }
                    }
                }
            }
        }

        // Returns a list of buffers that is ready to be sent. These buffers must be sent in order.
        // If no buffers are available for sending, null is returned.
        public List<KeyValuePair<TransferBuffer, Packet>>? TransferOutgoing()
        {
            List<KeyValuePair<TransferBuffer, Packet>> buffers = null;
            lock (m_class_lock)
            {
                if (HasPacketToSend())
                {
                    buffers = new List<KeyValuePair<TransferBuffer, Packet>>();
                    while (HasPacketToSend())
                    {
                        buffers.Add(GetPacketToSend());
                    }
                }
            }
            return buffers;
        }

        // Returns a list of all packets that are ready for processing. If no packets are available,
        // null is returned.
        public List<Packet>? TransferIncoming()
        {
            List<Packet> packets = null;
            lock (m_class_lock)
            {
                if (m_incoming_packets.Count > 0)
                {
                    packets = m_incoming_packets;
                    m_incoming_packets = new List<Packet>();
                }
            }
            return packets;
        }
    }
}
#endregion


#region TransferBuffer
namespace SecurityAPI
{
	public class TransferBuffer
	{
		byte[] m_buffer;
		int m_offset;
		int m_size;
		object m_lock;

		public byte[] Buffer
		{
			get { return m_buffer; }
			set { lock (m_lock) { m_buffer = value; } }
		}

		public int Offset
		{
			get { return m_offset; }
			set { lock (m_lock) { m_offset = value; } }
		}

		public int Size
		{
			get { return m_size; }
			set { lock (m_lock) { m_size = value; } }
		}

        public byte[] GetBuffer()
        {
            return m_buffer;
        }

		public TransferBuffer(TransferBuffer rhs)
		{
			lock (rhs.m_lock)
			{
				m_buffer = new byte[rhs.m_buffer.Length];
				System.Buffer.BlockCopy(rhs.m_buffer, 0, m_buffer, 0, m_buffer.Length);
				m_offset = rhs.m_offset;
				m_size = rhs.m_size;
				m_lock = new object();
			}
		}

		public TransferBuffer()
		{
			m_buffer = null;
			m_offset = 0;
			m_size = 0;
			m_lock = new object();
		}

		public TransferBuffer(int length, int offset, int size)
		{
			m_buffer = new byte[length];
			m_offset = offset;
			m_size = size;
			m_lock = new object();
		}

		public TransferBuffer(int length)
		{
			m_buffer = new byte[length];
			m_offset = 0;
			m_size = 0;
			m_lock = new object();
		}

		public TransferBuffer(byte[] buffer, int offset, int size, bool assign)
		{
			if (assign)
			{
				m_buffer = buffer;
			}
			else
			{
				m_buffer = new byte[buffer.Length];
				System.Buffer.BlockCopy(buffer, 0, m_buffer, 0, buffer.Length);
			}
			m_offset = offset;
			m_size = size;
			m_lock = new object();
		}
	}
}
#endregion


#region Utility
namespace SecurityAPI
{
	public class Utility
	{
		public static string HexDump(byte[] buffer)
		{
			return HexDump(buffer, 0, buffer.Length);
		}

		public static string HexDump(byte[] buffer, int offset, int count)
		{
			const int bytesPerLine = 16;
			StringBuilder output = new StringBuilder();
			StringBuilder ascii_output = new StringBuilder();
			int length = count;
			if (length % bytesPerLine != 0)
			{
				length += bytesPerLine - length % bytesPerLine;
			}
			for (int x = 0; x <= length; ++x)
			{
				if (x % bytesPerLine == 0)
				{
					if (x > 0)
					{
						output.AppendFormat("  {0}{1}", ascii_output.ToString(), Environment.NewLine);
						ascii_output.Clear();
					}
					if (x != length)
					{
						output.AppendFormat("{0:d10}   ", x);
					}
				}
				if (x < count)
				{
					output.AppendFormat("{0:X2} ", buffer[offset + x]);
					char ch = (char)buffer[offset + x];
					if (!Char.IsControl(ch))
					{
						ascii_output.AppendFormat("{0}", ch);
					}
					else
					{
						ascii_output.Append(".");
					}
				}
				else
				{
					output.Append("   ");
					ascii_output.Append(".");
				}
			}
			return output.ToString();
		}
	}
}
#endregion

