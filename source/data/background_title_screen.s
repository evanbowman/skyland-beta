
@{{BLOCK(background_title_screen)

@=======================================================================
@
@	background_title_screen, 1024x8@4, 
@	Transparent color : FF,00,FF
@	+ palette 256 entries, not compressed
@	+ 128 tiles not compressed
@	Total size: 512 + 4096 = 4608
@
@	Time-stamp: 2022-04-20, 15:04:48
@	Exported by Cearn's GBA Image Transmogrifier, v0.8.16
@	( http://www.coranac.com/projects/#grit )
@
@=======================================================================

	.section .rodata
	.align	2
	.global background_title_screenTiles		@ 4096 unsigned chars
	.hidden background_title_screenTiles
background_title_screenTiles:
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333

	.word 0x33333333,0x43333333,0x22333333,0x22333333,0x22333333,0x22233333,0x22233333,0x22222333
	.word 0x33333333,0x44444444,0x44444442,0x44422222,0x44222222,0x44222222,0x44222222,0x42222222
	.word 0x22222233,0x22222223,0x22222222,0x22222222,0x22222222,0x22222224,0x22222224,0x22222444
	.word 0x42222222,0x42222222,0x42222222,0x22222222,0x22222222,0x22222222,0x22222224,0x22222222
	.word 0x33333333,0x33332244,0x33444444,0x44444444,0x44444444,0x44444444,0x44444444,0x44444444
	.word 0x33333333,0x33333333,0x33333333,0x33333333,0x33333334,0x33333344,0x33333344,0x33333444
	.word 0x44444444,0x44444444,0x44444444,0x44444444,0x44444442,0x22222422,0x22222222,0x22222222
	.word 0x33333444,0x33334444,0x44344444,0x44444444,0x44422444,0x44422244,0x44442222,0x44422222

	.word 0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x44443333,0x44422333,0x44422333
	.word 0x33333333,0x44333333,0x44422333,0x42222233,0x42222233,0x22222233,0x22222234,0x22222224
	.word 0x44422233,0x44442233,0x44442223,0x44442222,0x44442222,0x22442222,0x22222222,0x22222242
	.word 0x22222224,0x22222224,0x22222224,0x22222224,0x22244424,0x22444444,0x22444442,0x24444222
	.word 0x33333333,0x44444444,0x44444444,0x44444444,0x44444444,0x44444444,0x44444444,0x44444442
	.word 0x33333333,0x33333324,0x33333444,0x33334444,0x33344444,0x33344444,0x33344444,0x33344444
	.word 0x44444442,0x44444442,0x44444422,0x44444222,0x44422222,0x44222222,0x22222222,0x22222222
	.word 0x33344444,0x33444444,0x33444444,0x34444444,0x34444444,0x44444444,0x44444444,0x44444442

	.word 0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333
	.word 0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333
	.word 0x33333333,0x44333333,0x42223333,0x22222233,0x22222223,0x22222224,0x22222224,0x22222244
	.word 0x33333444,0x33444444,0x34444444,0x44444444,0x44444442,0x22222242,0x22222242,0x22222442
	.word 0x33333333,0x33333333,0x33333333,0x33333333,0x44433333,0x44443333,0x44442333,0x44422233
	.word 0x33333333,0x33333333,0x33333333,0x33443333,0x34444344,0x34444444,0x44444444,0x44444444
	.word 0x44422233,0x44422233,0x44422223,0x44422224,0x44422224,0x44422222,0x44422222,0x22222222
	.word 0x44444444,0x44444444,0x44444444,0x44444444,0x44444444,0x44444444,0x44444444,0x44444222

	.word 0x11111111,0x11111111,0x31111111,0x33111111,0x33111111,0x33111111,0x33111111,0x33111111
	.word 0x11111111,0x55555511,0x55555533,0x55555533,0x55555333,0x55553333,0x55553333,0x53333333
	.word 0x33111115,0x33111555,0x33115553,0x31155533,0x33353333,0x33333333,0x33333333,0x33333333
	.word 0x53333333,0x53333333,0x53333333,0x53333333,0x33333333,0x33333333,0x33333333,0x33333333
	.word 0x11111111,0x11111115,0x11111155,0x11111555,0x11111555,0x31111555,0x33115555,0x33115555
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11115555,0x11115555,0x11155555,0x11555555
	.word 0x33335555,0x33335555,0x33355555,0x33355555,0x33555333,0x35533333,0x33333333,0x33333333
	.word 0x15555553,0x15555533,0x55553333,0x35533333,0x35533333,0x33333333,0x33333333,0x33333333

	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x51111111,0x55333111,0x33333111,0x33333311
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x55555555,0x55555555,0x55555555,0x55555555
	.word 0x33333331,0x33333331,0x33333331,0x33333333,0x33333333,0x33333333,0x33333333,0x33333333
	.word 0x55553333,0x55553333,0x55553333,0x55553333,0x55533333,0x33333333,0x33333333,0x33333333
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111115,0x11111555,0x11115555,0x11115555
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x11115555,0x11115555,0x11155555,0x11155555,0x11155555,0x11555555,0x33555555,0x33333333
	.word 0x55511111,0x53311111,0x33333111,0x33333311,0x33333311,0x33333311,0x33333333,0x33333333

	.word 0x22244444,0x22444444,0x22444444,0x24444442,0x44422222,0x44222222,0x44222222,0x42222222
	.word 0x22222222,0x22244222,0x24442222,0x44422222,0x44222222,0x42222222,0x42222222,0x22222222
	.word 0x42222222,0x42222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222224,0x22222224,0x22222224,0x22222244
	.word 0x24222222,0x42222222,0x42222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222

	.word 0x22222222,0x22222222,0x22222244,0x22222242,0x22222242,0x22222422,0x22222222,0x22242222
	.word 0x22442222,0x44444222,0x44224422,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22242222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222224,0x22222444,0x22224444,0x22224444,0x22242222,0x22422222,0x22422222
	.word 0x44444442,0x44444422,0x44444222,0x44442222,0x44442222,0x24222222,0x42222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x24222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222

	.word 0x22222444,0x22224444,0x22224444,0x24444444,0x42222224,0x22222222,0x22222222,0x22222224
	.word 0x22224222,0x22222222,0x22422222,0x22222222,0x22222222,0x22222224,0x22222222,0x22222422
	.word 0x22222224,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222422,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x44442222,0x44422222,0x24422222,0x24222222,0x24222222,0x44222222,0x42222222,0x42222222
	.word 0x22222222,0x22222224,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222
	.word 0x22222222,0x42222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222,0x22222222

	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111,0x11111111
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000
	.word 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000

	.section .rodata
	.align	2
	.global background_title_screenPal		@ 512 unsigned chars
	.hidden background_title_screenPal
background_title_screenPal:
	.hword 0x7C1F,0x72CC,0x7BBA,0x7B52,0x77DE,0x7774,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000

	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000

	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000

	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000

@}}BLOCK(background_title_screen)