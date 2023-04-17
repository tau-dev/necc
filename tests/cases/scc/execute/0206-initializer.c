//!necc-dbg @ -run
static int alpha3[][2] = {
	{ 0x00D6, 0x00D8 },
	{ 0x00F6, 0x00F8 },
	{ 0x02EC, 0x02EE },
	{ 0x0374, 0x0376 },
	{ 0x037D, 0x037F },
	{ 0x0386, 0x0388 },
	{ 0x038A, 0x038E },
	{ 0x03A1, 0x03A3 },
	{ 0x03F5, 0x03F7 },
	{ 0x052F, 0x0531 },
	{ 0x066F, 0x0671 },
	{ 0x06D3, 0x06D5 },
	{ 0x0710, 0x0712 },
	{ 0x09A8, 0x09AA },
	{ 0x09B0, 0x09B2 },
	{ 0x09DD, 0x09DF },
	{ 0x0A28, 0x0A2A },
	{ 0x0A30, 0x0A32 },
	{ 0x0A33, 0x0A35 },
	{ 0x0A36, 0x0A38 },
	{ 0x0A5C, 0x0A5E },
	{ 0x0A8D, 0x0A8F },
	{ 0x0A91, 0x0A93 },
	{ 0x0AA8, 0x0AAA },
	{ 0x0AB0, 0x0AB2 },
	{ 0x0AB3, 0x0AB5 },
	{ 0x0B28, 0x0B2A },
	{ 0x0B30, 0x0B32 },
	{ 0x0B33, 0x0B35 },
	{ 0x0B5D, 0x0B5F },
	{ 0x0B83, 0x0B85 },
	{ 0x0B90, 0x0B92 },
	{ 0x0B9A, 0x0B9E },
	{ 0x0C0C, 0x0C0E },
	{ 0x0C10, 0x0C12 },
	{ 0x0C28, 0x0C2A },
	{ 0x0C8C, 0x0C8E },
	{ 0x0C90, 0x0C92 },
	{ 0x0CA8, 0x0CAA },
	{ 0x0CB3, 0x0CB5 },
	{ 0x0CDE, 0x0CE0 },
	{ 0x0D0C, 0x0D0E },
	{ 0x0D10, 0x0D12 },
	{ 0x0DB1, 0x0DB3 },
	{ 0x0DBB, 0x0DBD },
	{ 0x0E30, 0x0E32 },
	{ 0x0E82, 0x0E84 },
	{ 0x0E88, 0x0E8A },
	{ 0x0E97, 0x0E99 },
	{ 0x0E9F, 0x0EA1 },
	{ 0x0EA3, 0x0EA7 },
	{ 0x0EAB, 0x0EAD },
	{ 0x0EB0, 0x0EB2 },
	{ 0x0EC4, 0x0EC6 },
	{ 0x0F47, 0x0F49 },
	{ 0x10C5, 0x10C7 },
	{ 0x10FA, 0x10FC },
	{ 0x1248, 0x124A },
	{ 0x1256, 0x125A },
	{ 0x1288, 0x128A },
	{ 0x12B0, 0x12B2 },
	{ 0x12BE, 0x12C2 },
	{ 0x12D6, 0x12D8 },
	{ 0x1310, 0x1312 },
	{ 0x167F, 0x1681 },
	{ 0x170C, 0x170E },
	{ 0x176C, 0x176E },
	{ 0x18A8, 0x18AA },
	{ 0x1CEC, 0x1CEE },
	{ 0x1F57, 0x1F5F },
	{ 0x1FB4, 0x1FB6 },
	{ 0x1FBC, 0x1FBE },
	{ 0x1FC4, 0x1FC6 },
	{ 0x1FF4, 0x1FF6 },
	{ 0x2113, 0x2115 },
	{ 0x2124, 0x212A },
	{ 0x212D, 0x212F },
	{ 0x2C2E, 0x2C30 },
	{ 0x2C5E, 0x2C60 },
	{ 0x2D25, 0x2D27 },
	{ 0x2DA6, 0x2DA8 },
	{ 0x2DAE, 0x2DB0 },
	{ 0x2DB6, 0x2DB8 },
	{ 0x2DBE, 0x2DC0 },
	{ 0x2DC6, 0x2DC8 },
	{ 0x2DCE, 0x2DD0 },
	{ 0x2DD6, 0x2DD8 },
	{ 0x309F, 0x30A1 },
	{ 0x30FA, 0x30FC },
	{ 0xA78E, 0xA790 },
	{ 0xA801, 0xA803 },
	{ 0xA805, 0xA807 },
	{ 0xA80A, 0xA80C },
	{ 0xA9E4, 0xA9E6 },
	{ 0xA9FE, 0xAA00 },
	{ 0xAA42, 0xAA44 },
	{ 0xAAAF, 0xAAB1 },
	{ 0xAAC0, 0xAAC2 },
	{ 0xAB26, 0xAB28 },
	{ 0xAB2E, 0xAB30 },
	{ 0xAB5A, 0xAB5C },
	{ 0xFB1D, 0xFB1F },
	{ 0xFB28, 0xFB2A },
	{ 0xFB36, 0xFB38 },
	{ 0xFB3C, 0xFB40 },
	{ 0xFB41, 0xFB43 },
	{ 0xFB44, 0xFB46 },
	{ 0xFE74, 0xFE76 },
	{ 0x1000B, 0x1000D },
	{ 0x10026, 0x10028 },
	{ 0x1003A, 0x1003C },
	{ 0x1003D, 0x1003F },
	{ 0x10340, 0x10342 },
	{ 0x10808, 0x1080A },
	{ 0x10835, 0x10837 },
	{ 0x10A13, 0x10A15 },
	{ 0x10A17, 0x10A19 },
	{ 0x10AC7, 0x10AC9 },
	{ 0x11211, 0x11213 },
	{ 0x11328, 0x1132A },
	{ 0x11330, 0x11332 },
	{ 0x11333, 0x11335 },
	{ 0x114C5, 0x114C7 },
	{ 0x1D454, 0x1D456 },
	{ 0x1D49C, 0x1D49E },
	{ 0x1D4AC, 0x1D4AE },
	{ 0x1D4B9, 0x1D4BD },
	{ 0x1D4C3, 0x1D4C5 },
	{ 0x1D505, 0x1D507 },
	{ 0x1D514, 0x1D516 },
	{ 0x1D51C, 0x1D51E },
	{ 0x1D539, 0x1D53B },
	{ 0x1D53E, 0x1D540 },
	{ 0x1D544, 0x1D546 },
	{ 0x1D550, 0x1D552 },
	{ 0x1D6C0, 0x1D6C2 },
	{ 0x1D6DA, 0x1D6DC },
	{ 0x1D6FA, 0x1D6FC },
	{ 0x1D714, 0x1D716 },
	{ 0x1D734, 0x1D736 },
	{ 0x1D74E, 0x1D750 },
	{ 0x1D76E, 0x1D770 },
	{ 0x1D788, 0x1D78A },
	{ 0x1D7A8, 0x1D7AA },
	{ 0x1D7C2, 0x1D7C4 },
	{ 0x1EE03, 0x1EE05 },
	{ 0x1EE1F, 0x1EE21 },
	{ 0x1EE22, 0x1EE24 },
	{ 0x1EE27, 0x1EE29 },
	{ 0x1EE32, 0x1EE34 },
	{ 0x1EE37, 0x1EE3B },
	{ 0x1EE47, 0x1EE4D },
	{ 0x1EE4F, 0x1EE51 },
	{ 0x1EE52, 0x1EE54 },
	{ 0x1EE57, 0x1EE61 },
	{ 0x1EE62, 0x1EE64 },
	{ 0x1EE6A, 0x1EE6C },
	{ 0x1EE72, 0x1EE74 },
	{ 0x1EE77, 0x1EE79 },
	{ 0x1EE7C, 0x1EE80 },
	{ 0x1EE89, 0x1EE8B },
	{ 0x1EEA3, 0x1EEA5 },
	{ 0x1EEA9, 0x1EEAB },
};

int
main()
{
	return alpha3 == 0;
}
