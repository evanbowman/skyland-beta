
@{{BLOCK(background_sailing)

@=======================================================================
@
@	background_sailing, 48x8@4, 
@	Transparent color : FF,00,FF
@	+ palette 16 entries, not compressed
@	+ 6 tiles lz77 compressed
@	Total size: 32 + 44 = 76
@
@	Time-stamp: 2026-07-14, 18:08:04
@	Exported by Cearn's GBA Image Transmogrifier, v0.9.2
@	( http://www.coranac.com/projects/#grit )
@
@=======================================================================

	.section .rodata
	.align	2
	.global background_sailingTiles		@ 44 unsigned chars
	.hidden background_sailingTiles
background_sailingTiles:
	.word 0x0000C010,0xF000003F,0xF001F001,0xF001F001,0x9501F001,0x111101F0,0x10150100,0x0A205105
	.word 0x400460CC,0xF022220E,0x00059001

	.section .rodata
	.align	2
	.global background_sailingPal		@ 32 unsigned chars
	.hidden background_sailingPal
background_sailingPal:
	.hword 0x7C1F,0x6666,0x77DE,0x7774,0x76AB,0x6A0B,0x0000,0x0000
	.hword 0x0421,0x0421,0x0421,0x0421,0x0421,0x0421,0x0421,0x0421

@}}BLOCK(background_sailing)
