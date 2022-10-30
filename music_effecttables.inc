; vibrato sinus tables

mus_vibrato_sinus_0: ; depth 0.12 semi-tones
	dw $0000,$0002,$0004,$0004,$0006,$0008,$0008,$000a ; 0-7
	dw $000c,$000c,$000e,$000e,$000e,$0010,$0010,$0010 ; 8-15
	dw $0010,$0010,$0010,$0010,$000e,$000e,$000e,$000c ; 16-23
	dw $000c,$000a,$0008,$0008,$0006,$0004,$0004,$0002 ; 24-31
	dw $0000,$0000,$fffe,$fffe,$fffc,$fffa,$fffa,$fff8 ; 32-39
	dw $fff6,$fff6,$fff4,$fff4,$fff4,$fff2,$fff2,$fff2 ; 40-47
	dw $fff2,$fff2,$fff2,$fff2,$fff4,$fff4,$fff4,$fff6 ; 48-55
	dw $fff6,$fff8,$fffa,$fffa,$fffc,$fffe,$fffe,$0000 ; 56-63

mus_vibrato_sinus_1: ; depth 0.25 semi-tones
	dw $0000,$0004,$0006,$000a,$000c,$0010,$0012,$0014 ; 0-7
	dw $0016,$0018,$001a,$001c,$001e,$001e,$0020,$0020 ; 8-15
	dw $0020,$0020,$0020,$001e,$001e,$001c,$001a,$0018 ; 16-23
	dw $0016,$0014,$0012,$0010,$000c,$000a,$0006,$0004 ; 24-31
	dw $0000,$fffe,$fffc,$fff8,$fff6,$fff2,$fff0,$ffee ; 32-39
	dw $ffec,$ffea,$ffe8,$ffe6,$ffe4,$ffe4,$ffe2,$ffe2 ; 40-47
	dw $ffe2,$ffe2,$ffe2,$ffe4,$ffe4,$ffe6,$ffe8,$ffea ; 48-55
	dw $ffec,$ffee,$fff0,$fff2,$fff6,$fff8,$fffc,$fffe ; 56-63

mus_vibrato_sinus_2: ; depth 0.37 semi-tones
	dw $0000,$0004,$000a,$000e,$0012,$0016,$001a,$001e ; 0-7
	dw $0022,$0026,$0028,$002a,$002c,$002e,$0030,$0030 ; 8-15
	dw $0030,$0030,$0030,$002e,$002c,$002a,$0028,$0026 ; 16-23
	dw $0022,$001e,$001a,$0016,$0012,$000e,$000a,$0004 ; 24-31
	dw $0000,$fffe,$fff8,$fff4,$fff0,$ffec,$ffe8,$ffe4 ; 32-39
	dw $ffe0,$ffdc,$ffda,$ffd8,$ffd6,$ffd4,$ffd2,$ffd2 ; 40-47
	dw $ffd2,$ffd2,$ffd2,$ffd4,$ffd6,$ffd8,$ffda,$ffdc ; 48-55
	dw $ffe0,$ffe4,$ffe8,$ffec,$fff0,$fff4,$fff8,$fffe ; 56-63

mus_vibrato_sinus_3: ; depth 0.50 semi-tones
	dw $0000,$0006,$000c,$0012,$0018,$001e,$0024,$0028 ; 0-7
	dw $002e,$0032,$0036,$0038,$003c,$003e,$003e,$0040 ; 8-15
	dw $0040,$0040,$003e,$003e,$003c,$0038,$0036,$0032 ; 16-23
	dw $002e,$0028,$0024,$001e,$0018,$0012,$000c,$0006 ; 24-31
	dw $0000,$fffc,$fff6,$fff0,$ffea,$ffe4,$ffde,$ffda ; 32-39
	dw $ffd4,$ffd0,$ffcc,$ffca,$ffc6,$ffc4,$ffc4,$ffc2 ; 40-47
	dw $ffc2,$ffc2,$ffc4,$ffc4,$ffc6,$ffca,$ffcc,$ffd0 ; 48-55
	dw $ffd4,$ffda,$ffde,$ffe4,$ffea,$fff0,$fff6,$fffc ; 56-63

mus_vibrato_sinus_4: ; depth 0.62 semi-tones
	dw $0000,$0008,$0010,$0018,$001e,$0026,$002c,$0032 ; 0-7
	dw $0038,$003e,$0042,$0046,$004a,$004c,$004e,$0050 ; 8-15
	dw $0050,$0050,$004e,$004c,$004a,$0046,$0042,$003e ; 16-23
	dw $0038,$0032,$002c,$0026,$001e,$0018,$0010,$0008 ; 24-31
	dw $0000,$fffa,$fff2,$ffea,$ffe4,$ffdc,$ffd6,$ffd0 ; 32-39
	dw $ffca,$ffc4,$ffc0,$ffbc,$ffb8,$ffb6,$ffb4,$ffb2 ; 40-47
	dw $ffb2,$ffb2,$ffb4,$ffb6,$ffb8,$ffbc,$ffc0,$ffc4 ; 48-55
	dw $ffca,$ffd0,$ffd6,$ffdc,$ffe4,$ffea,$fff2,$fffa ; 56-63

mus_vibrato_sinus_5: ; depth 0.75 semi-tones
	dw $0000,$000a,$0012,$001c,$0024,$002e,$0036,$003c ; 0-7
	dw $0044,$004a,$0050,$0054,$0058,$005c,$005e,$0060 ; 8-15
	dw $0060,$0060,$005e,$005c,$0058,$0054,$0050,$004a ; 16-23
	dw $0044,$003c,$0036,$002e,$0024,$001c,$0012,$000a ; 24-31
	dw $0000,$fff8,$fff0,$ffe6,$ffde,$ffd4,$ffcc,$ffc6 ; 32-39
	dw $ffbe,$ffb8,$ffb2,$ffae,$ffaa,$ffa6,$ffa4,$ffa2 ; 40-47
	dw $ffa2,$ffa2,$ffa4,$ffa6,$ffaa,$ffae,$ffb2,$ffb8 ; 48-55
	dw $ffbe,$ffc6,$ffcc,$ffd4,$ffde,$ffe6,$fff0,$fff8 ; 56-63

mus_vibrato_sinus_6: ; depth 0.87 semi-tones
	dw $0000,$000a,$0016,$0020,$002a,$0034,$003e,$0048 ; 0-7
	dw $0050,$0056,$005e,$0062,$0068,$006c,$006e,$0070 ; 8-15
	dw $0070,$0070,$006e,$006c,$0068,$0062,$005e,$0056 ; 16-23
	dw $0050,$0048,$003e,$0034,$002a,$0020,$0016,$000a ; 24-31
	dw $0000,$fff8,$ffec,$ffe2,$ffd8,$ffce,$ffc4,$ffba ; 32-39
	dw $ffb2,$ffac,$ffa4,$ffa0,$ff9a,$ff96,$ff94,$ff92 ; 40-47
	dw $ff92,$ff92,$ff94,$ff96,$ff9a,$ffa0,$ffa4,$ffac ; 48-55
	dw $ffb2,$ffba,$ffc4,$ffce,$ffd8,$ffe2,$ffec,$fff8 ; 56-63

mus_vibrato_sinus_7: ; depth 1.00 semi-tones
	dw $0000,$000c,$0018,$0026,$0030,$003c,$0048,$0052 ; 0-7
	dw $005a,$0062,$006a,$0070,$0076,$007a,$007e,$0080 ; 8-15
	dw $0080,$0080,$007e,$007a,$0076,$0070,$006a,$0062 ; 16-23
	dw $005a,$0052,$0048,$003c,$0030,$0026,$0018,$000c ; 24-31
	dw $0000,$fff6,$ffea,$ffdc,$ffd2,$ffc6,$ffba,$ffb0 ; 32-39
	dw $ffa8,$ffa0,$ff98,$ff92,$ff8c,$ff88,$ff84,$ff82 ; 40-47
	dw $ff82,$ff82,$ff84,$ff88,$ff8c,$ff92,$ff98,$ffa0 ; 48-55
	dw $ffa8,$ffb0,$ffba,$ffc6,$ffd2,$ffdc,$ffea,$fff6 ; 56-63

mus_vibrato_sinus_8: ; depth 1.25 semi-tones
	dw $0000,$0010,$0020,$002e,$003e,$004c,$0058,$0066 ; 0-7
	dw $0072,$007c,$0086,$008e,$0094,$009a,$009c,$00a0 ; 8-15
	dw $00a0,$00a0,$009c,$009a,$0094,$008e,$0086,$007c ; 16-23
	dw $0072,$0066,$0058,$004c,$003e,$002e,$0020,$0010 ; 24-31
	dw $0000,$fff2,$ffe2,$ffd4,$ffc4,$ffb6,$ffaa,$ff9c ; 32-39
	dw $ff90,$ff86,$ff7c,$ff74,$ff6e,$ff68,$ff66,$ff62 ; 40-47
	dw $ff62,$ff62,$ff66,$ff68,$ff6e,$ff74,$ff7c,$ff86 ; 48-55
	dw $ff90,$ff9c,$ffaa,$ffb6,$ffc4,$ffd4,$ffe2,$fff2 ; 56-63

mus_vibrato_sinus_9: ; depth 1.50 semi-tones
	dw $0000,$0012,$0026,$0038,$004a,$005a,$006a,$007a ; 0-7
	dw $0088,$0094,$00a0,$00aa,$00b2,$00b8,$00bc,$00c0 ; 8-15
	dw $00c0,$00c0,$00bc,$00b8,$00b2,$00aa,$00a0,$0094 ; 16-23
	dw $0088,$007a,$006a,$005a,$004a,$0038,$0026,$0012 ; 24-31
	dw $0000,$fff0,$ffdc,$ffca,$ffb8,$ffa8,$ff98,$ff88 ; 32-39
	dw $ff7a,$ff6e,$ff62,$ff58,$ff50,$ff4a,$ff46,$ff42 ; 40-47
	dw $ff42,$ff42,$ff46,$ff4a,$ff50,$ff58,$ff62,$ff6e ; 48-55
	dw $ff7a,$ff88,$ff98,$ffa8,$ffb8,$ffca,$ffdc,$fff0 ; 56-63

mus_vibrato_sinus_10: ; depth 1.75 semi-tones
	dw $0000,$0016,$002c,$0042,$0056,$006a,$007c,$008e ; 0-7
	dw $009e,$00ae,$00ba,$00c6,$00ce,$00d6,$00dc,$00de ; 8-15
	dw $00e0,$00de,$00dc,$00d6,$00ce,$00c6,$00ba,$00ae ; 16-23
	dw $009e,$008e,$007c,$006a,$0056,$0042,$002c,$0016 ; 24-31
	dw $0000,$ffec,$ffd6,$ffc0,$ffac,$ff98,$ff86,$ff74 ; 32-39
	dw $ff64,$ff54,$ff48,$ff3c,$ff34,$ff2c,$ff26,$ff24 ; 40-47
	dw $ff22,$ff24,$ff26,$ff2c,$ff34,$ff3c,$ff48,$ff54 ; 48-55
	dw $ff64,$ff74,$ff86,$ff98,$ffac,$ffc0,$ffd6,$ffec ; 56-63

mus_vibrato_sinus_11: ; depth 2.00 semi-tones
	dw $0000,$001a,$0032,$004a,$0062,$0078,$008e,$00a2 ; 0-7
	dw $00b6,$00c6,$00d4,$00e2,$00ec,$00f4,$00fc,$00fe ; 8-15
	dw $0100,$00fe,$00fc,$00f4,$00ec,$00e2,$00d4,$00c6 ; 16-23
	dw $00b6,$00a2,$008e,$0078,$0062,$004a,$0032,$001a ; 24-31
	dw $0000,$ffe8,$ffd0,$ffb8,$ffa0,$ff8a,$ff74,$ff60 ; 32-39
	dw $ff4c,$ff3c,$ff2e,$ff20,$ff16,$ff0e,$ff06,$ff04 ; 40-47
	dw $ff02,$ff04,$ff06,$ff0e,$ff16,$ff20,$ff2e,$ff3c ; 48-55
	dw $ff4c,$ff60,$ff74,$ff8a,$ffa0,$ffb8,$ffd0,$ffe8 ; 56-63

mus_vibrato_sinus_12: ; depth 2.25 semi-tones
	dw $0000,$001c,$0038,$0054,$006e,$0088,$00a0,$00b6 ; 0-7
	dw $00cc,$00de,$00f0,$00fe,$010a,$0114,$011a,$011e ; 8-15
	dw $0120,$011e,$011a,$0114,$010a,$00fe,$00f0,$00de ; 16-23
	dw $00cc,$00b6,$00a0,$0088,$006e,$0054,$0038,$001c ; 24-31
	dw $0000,$ffe6,$ffca,$ffae,$ff94,$ff7a,$ff62,$ff4c ; 32-39
	dw $ff36,$ff24,$ff12,$ff04,$fef8,$feee,$fee8,$fee4 ; 40-47
	dw $fee2,$fee4,$fee8,$feee,$fef8,$ff04,$ff12,$ff24 ; 48-55
	dw $ff36,$ff4c,$ff62,$ff7a,$ff94,$ffae,$ffca,$ffe6 ; 56-63

mus_vibrato_sinus_13: ; depth 2.50 semi-tones
	dw $0000,$0020,$003e,$005c,$007a,$0096,$00b2,$00cc ; 0-7
	dw $00e2,$00f8,$010a,$011a,$0128,$0132,$013a,$013e ; 8-15
	dw $0140,$013e,$013a,$0132,$0128,$011a,$010a,$00f8 ; 16-23
	dw $00e2,$00cc,$00b2,$0096,$007a,$005c,$003e,$0020 ; 24-31
	dw $0000,$ffe2,$ffc4,$ffa6,$ff88,$ff6c,$ff50,$ff36 ; 32-39
	dw $ff20,$ff0a,$fef8,$fee8,$feda,$fed0,$fec8,$fec4 ; 40-47
	dw $fec2,$fec4,$fec8,$fed0,$feda,$fee8,$fef8,$ff0a ; 48-55
	dw $ff20,$ff36,$ff50,$ff6c,$ff88,$ffa6,$ffc4,$ffe2 ; 56-63

mus_vibrato_sinus_14: ; depth 2.75 semi-tones
	dw $0000,$0022,$0044,$0066,$0086,$00a6,$00c4,$00e0 ; 0-7
	dw $00f8,$0110,$0124,$0136,$0146,$0150,$015a,$015e ; 8-15
	dw $0160,$015e,$015a,$0150,$0146,$0136,$0124,$0110 ; 16-23
	dw $00f8,$00e0,$00c4,$00a6,$0086,$0066,$0044,$0022 ; 24-31
	dw $0000,$ffe0,$ffbe,$ff9c,$ff7c,$ff5c,$ff3e,$ff22 ; 32-39
	dw $ff0a,$fef2,$fede,$fecc,$febc,$feb2,$fea8,$fea4 ; 40-47
	dw $fea2,$fea4,$fea8,$feb2,$febc,$fecc,$fede,$fef2 ; 48-55
	dw $ff0a,$ff22,$ff3e,$ff5c,$ff7c,$ff9c,$ffbe,$ffe0 ; 56-63

mus_vibrato_sinus_15: ; depth 3.00 semi-tones
	dw $0000,$0026,$004a,$0070,$0092,$00b6,$00d6,$00f4 ; 0-7
	dw $0110,$0128,$0140,$0152,$0162,$0170,$0178,$017e ; 8-15
	dw $0180,$017e,$0178,$0170,$0162,$0152,$0140,$0128 ; 16-23
	dw $0110,$00f4,$00d6,$00b6,$0092,$0070,$004a,$0026 ; 24-31
	dw $0000,$ffdc,$ffb8,$ff92,$ff70,$ff4c,$ff2c,$ff0e ; 32-39
	dw $fef2,$feda,$fec2,$feb0,$fea0,$fe92,$fe8a,$fe84 ; 40-47
	dw $fe82,$fe84,$fe8a,$fe92,$fea0,$feb0,$fec2,$feda ; 48-55
	dw $fef2,$ff0e,$ff2c,$ff4c,$ff70,$ff92,$ffb8,$ffdc ; 56-63

mus_vibrato_sinus_16: ; depth 3.50 semi-tones
	dw $0000,$002c,$0058,$0082,$00ac,$00d4,$00f8,$011c ; 0-7
	dw $013c,$015a,$0174,$018c,$019e,$01ac,$01b8,$01be ; 8-15
	dw $01c0,$01be,$01b8,$01ac,$019e,$018c,$0174,$015a ; 16-23
	dw $013c,$011c,$00f8,$00d4,$00ac,$0082,$0058,$002c ; 24-31
	dw $0000,$ffd6,$ffaa,$ff80,$ff56,$ff2e,$ff0a,$fee6 ; 32-39
	dw $fec6,$fea8,$fe8e,$fe76,$fe64,$fe56,$fe4a,$fe44 ; 40-47
	dw $fe42,$fe44,$fe4a,$fe56,$fe64,$fe76,$fe8e,$fea8 ; 48-55
	dw $fec6,$fee6,$ff0a,$ff2e,$ff56,$ff80,$ffaa,$ffd6 ; 56-63

mus_vibrato_sinus_17: ; depth 4.00 semi-tones
	dw $0000,$0032,$0064,$0094,$00c4,$00f2,$011c,$0144 ; 0-7
	dw $016a,$018c,$01aa,$01c4,$01da,$01ea,$01f6,$01fe ; 8-15
	dw $0200,$01fe,$01f6,$01ea,$01da,$01c4,$01aa,$018c ; 16-23
	dw $016a,$0144,$011c,$00f2,$00c4,$0094,$0064,$0032 ; 24-31
	dw $0000,$ffd0,$ff9e,$ff6e,$ff3e,$ff10,$fee6,$febe ; 32-39
	dw $fe98,$fe76,$fe58,$fe3e,$fe28,$fe18,$fe0c,$fe04 ; 40-47
	dw $fe02,$fe04,$fe0c,$fe18,$fe28,$fe3e,$fe58,$fe76 ; 48-55
	dw $fe98,$febe,$fee6,$ff10,$ff3e,$ff6e,$ff9e,$ffd0 ; 56-63

mus_vibrato_sinus_18: ; depth 4.50 semi-tones
	dw $0000,$0038,$0070,$00a8,$00dc,$0110,$0140,$016e ; 0-7
	dw $0198,$01be,$01de,$01fc,$0214,$0228,$0234,$023e ; 8-15
	dw $0240,$023e,$0234,$0228,$0214,$01fc,$01de,$01be ; 16-23
	dw $0198,$016e,$0140,$0110,$00dc,$00a8,$0070,$0038 ; 24-31
	dw $0000,$ffca,$ff92,$ff5a,$ff26,$fef2,$fec2,$fe94 ; 32-39
	dw $fe6a,$fe44,$fe24,$fe06,$fdee,$fdda,$fdce,$fdc4 ; 40-47
	dw $fdc2,$fdc4,$fdce,$fdda,$fdee,$fe06,$fe24,$fe44 ; 48-55
	dw $fe6a,$fe94,$fec2,$fef2,$ff26,$ff5a,$ff92,$ffca ; 56-63

mus_vibrato_sinus_19: ; depth 5.00 semi-tones
	dw $0000,$003e,$007c,$00ba,$00f4,$012e,$0164,$0196 ; 0-7
	dw $01c4,$01ee,$0214,$0234,$0250,$0264,$0274,$027c ; 8-15
	dw $0280,$027c,$0274,$0264,$0250,$0234,$0214,$01ee ; 16-23
	dw $01c4,$0196,$0164,$012e,$00f4,$00ba,$007c,$003e ; 24-31
	dw $0000,$ffc4,$ff86,$ff48,$ff0e,$fed4,$fe9e,$fe6c ; 32-39
	dw $fe3e,$fe14,$fdee,$fdce,$fdb2,$fd9e,$fd8e,$fd86 ; 40-47
	dw $fd82,$fd86,$fd8e,$fd9e,$fdb2,$fdce,$fdee,$fe14 ; 48-55
	dw $fe3e,$fe6c,$fe9e,$fed4,$ff0e,$ff48,$ff86,$ffc4 ; 56-63

mus_vibrato_sinus_20: ; depth 5.50 semi-tones
	dw $0000,$0046,$008a,$00cc,$010e,$014c,$0188,$01be ; 0-7
	dw $01f2,$0220,$024a,$026c,$028a,$02a2,$02b2,$02bc ; 8-15
	dw $02c0,$02bc,$02b2,$02a2,$028a,$026c,$024a,$0220 ; 16-23
	dw $01f2,$01be,$0188,$014c,$010e,$00cc,$008a,$0046 ; 24-31
	dw $0000,$ffbc,$ff78,$ff36,$fef4,$feb6,$fe7a,$fe44 ; 32-39
	dw $fe10,$fde2,$fdb8,$fd96,$fd78,$fd60,$fd50,$fd46 ; 40-47
	dw $fd42,$fd46,$fd50,$fd60,$fd78,$fd96,$fdb8,$fde2 ; 48-55
	dw $fe10,$fe44,$fe7a,$feb6,$fef4,$ff36,$ff78,$ffbc ; 56-63

mus_vibrato_sinus_21: ; depth 6.00 semi-tones
	dw $0000,$004c,$0096,$00de,$0126,$016a,$01aa,$01e8 ; 0-7
	dw $0220,$0252,$027e,$02a6,$02c6,$02de,$02f2,$02fc ; 8-15
	dw $0300,$02fc,$02f2,$02de,$02c6,$02a6,$027e,$0252 ; 16-23
	dw $0220,$01e8,$01aa,$016a,$0126,$00de,$0096,$004c ; 24-31
	dw $0000,$ffb6,$ff6c,$ff24,$fedc,$fe98,$fe58,$fe1a ; 32-39
	dw $fde2,$fdb0,$fd84,$fd5c,$fd3c,$fd24,$fd10,$fd06 ; 40-47
	dw $fd02,$fd06,$fd10,$fd24,$fd3c,$fd5c,$fd84,$fdb0 ; 48-55
	dw $fde2,$fe1a,$fe58,$fe98,$fedc,$ff24,$ff6c,$ffb6 ; 56-63

mus_vibrato_sinus_22: ; depth 6.50 semi-tones
	dw $0000,$0052,$00a2,$00f2,$013e,$0188,$01ce,$0210 ; 0-7
	dw $024c,$0284,$02b4,$02de,$0300,$031c,$0330,$033c ; 8-15
	dw $0340,$033c,$0330,$031c,$0300,$02de,$02b4,$0284 ; 16-23
	dw $024c,$0210,$01ce,$0188,$013e,$00f2,$00a2,$0052 ; 24-31
	dw $0000,$ffb0,$ff60,$ff10,$fec4,$fe7a,$fe34,$fdf2 ; 32-39
	dw $fdb6,$fd7e,$fd4e,$fd24,$fd02,$fce6,$fcd2,$fcc6 ; 40-47
	dw $fcc2,$fcc6,$fcd2,$fce6,$fd02,$fd24,$fd4e,$fd7e ; 48-55
	dw $fdb6,$fdf2,$fe34,$fe7a,$fec4,$ff10,$ff60,$ffb0 ; 56-63

mus_vibrato_sinus_23: ; depth 7.00 semi-tones
	dw $0000,$0058,$00ae,$0104,$0156,$01a6,$01f2,$0238 ; 0-7
	dw $027a,$02b4,$02e8,$0316,$033c,$035a,$036e,$037c ; 8-15
	dw $0380,$037c,$036e,$035a,$033c,$0316,$02e8,$02b4 ; 16-23
	dw $027a,$0238,$01f2,$01a6,$0156,$0104,$00ae,$0058 ; 24-31
	dw $0000,$ffaa,$ff54,$fefe,$feac,$fe5c,$fe10,$fdca ; 32-39
	dw $fd88,$fd4e,$fd1a,$fcec,$fcc6,$fca8,$fc94,$fc86 ; 40-47
	dw $fc82,$fc86,$fc94,$fca8,$fcc6,$fcec,$fd1a,$fd4e ; 48-55
	dw $fd88,$fdca,$fe10,$fe5c,$feac,$fefe,$ff54,$ffaa ; 56-63

mus_vibrato_sinus_24: ; depth 8.00 semi-tones
	dw $0000,$0064,$00c8,$012a,$0188,$01e2,$0238,$028a ; 0-7
	dw $02d4,$0318,$0354,$0388,$03b2,$03d4,$03ec,$03fc ; 8-15
	dw $0400,$03fc,$03ec,$03d4,$03b2,$0388,$0354,$0318 ; 16-23
	dw $02d4,$028a,$0238,$01e2,$0188,$012a,$00c8,$0064 ; 24-31
	dw $0000,$ff9e,$ff3a,$fed8,$fe7a,$fe20,$fdca,$fd78 ; 32-39
	dw $fd2e,$fcea,$fcae,$fc7a,$fc50,$fc2e,$fc16,$fc06 ; 40-47
	dw $fc02,$fc06,$fc16,$fc2e,$fc50,$fc7a,$fcae,$fcea ; 48-55
	dw $fd2e,$fd78,$fdca,$fe20,$fe7a,$fed8,$ff3a,$ff9e ; 56-63

mus_vibrato_sinus_25: ; depth 9.00 semi-tones
	dw $0000,$0070,$00e0,$014e,$01b8,$0220,$0280,$02da ; 0-7
	dw $032e,$037a,$03be,$03f8,$0428,$044e,$046a,$047a ; 8-15
	dw $0480,$047a,$046a,$044e,$0428,$03f8,$03be,$037a ; 16-23
	dw $032e,$02da,$0280,$0220,$01b8,$014e,$00e0,$0070 ; 24-31
	dw $0000,$ff92,$ff22,$feb4,$fe4a,$fde2,$fd82,$fd28 ; 32-39
	dw $fcd4,$fc88,$fc44,$fc0a,$fbda,$fbb4,$fb98,$fb88 ; 40-47
	dw $fb82,$fb88,$fb98,$fbb4,$fbda,$fc0a,$fc44,$fc88 ; 48-55
	dw $fcd4,$fd28,$fd82,$fde2,$fe4a,$feb4,$ff22,$ff92 ; 56-63

mus_vibrato_sinus_26: ; depth 10.00 semi-tones
	dw $0000,$007e,$00fa,$0174,$01ea,$025c,$02c8,$032c ; 0-7
	dw $038a,$03de,$0428,$0468,$049e,$04c8,$04e8,$04fa ; 8-15
	dw $0500,$04fa,$04e8,$04c8,$049e,$0468,$0428,$03de ; 16-23
	dw $038a,$032c,$02c8,$025c,$01ea,$0174,$00fa,$007e ; 24-31
	dw $0000,$ff84,$ff08,$fe8e,$fe18,$fda6,$fd3a,$fcd6 ; 32-39
	dw $fc78,$fc24,$fbda,$fb9a,$fb64,$fb3a,$fb1a,$fb08 ; 40-47
	dw $fb02,$fb08,$fb1a,$fb3a,$fb64,$fb9a,$fbda,$fc24 ; 48-55
	dw $fc78,$fcd6,$fd3a,$fda6,$fe18,$fe8e,$ff08,$ff84 ; 56-63

mus_vibrato_sinus_27: ; depth 11.00 semi-tones
	dw $0000,$008a,$0112,$0198,$021a,$0298,$030e,$037e ; 0-7
	dw $03e4,$0440,$0492,$04da,$0514,$0544,$0564,$057a ; 8-15
	dw $0580,$057a,$0564,$0544,$0514,$04da,$0492,$0440 ; 16-23
	dw $03e4,$037e,$030e,$0298,$021a,$0198,$0112,$008a ; 24-31
	dw $0000,$ff78,$fef0,$fe6a,$fde8,$fd6a,$fcf4,$fc84 ; 32-39
	dw $fc1e,$fbc2,$fb70,$fb28,$faee,$fabe,$fa9e,$fa88 ; 40-47
	dw $fa82,$fa88,$fa9e,$fabe,$faee,$fb28,$fb70,$fbc2 ; 48-55
	dw $fc1e,$fc84,$fcf4,$fd6a,$fde8,$fe6a,$fef0,$ff78 ; 56-63

mus_vibrato_sinus_28: ; depth 12.00 semi-tones
	dw $0000,$0096,$012c,$01be,$024c,$02d4,$0356,$03ce ; 0-7
	dw $043e,$04a4,$04fe,$054a,$058c,$05be,$05e2,$05f8 ; 8-15
	dw $0600,$05f8,$05e2,$05be,$058c,$054a,$04fe,$04a4 ; 16-23
	dw $043e,$03ce,$0356,$02d4,$024c,$01be,$012c,$0096 ; 24-31
	dw $0000,$ff6c,$fed6,$fe44,$fdb6,$fd2e,$fcac,$fc34 ; 32-39
	dw $fbc4,$fb5e,$fb04,$fab8,$fa76,$fa44,$fa20,$fa0a ; 40-47
	dw $fa02,$fa0a,$fa20,$fa44,$fa76,$fab8,$fb04,$fb5e ; 48-55
	dw $fbc4,$fc34,$fcac,$fd2e,$fdb6,$fe44,$fed6,$ff6c ; 56-63

mus_vibrato_sinus_29: ; depth 15.33 semi-tones
	dw $0000,$00c0,$017e,$023a,$02f0,$039e,$0442,$04de ; 0-7
	dw $056c,$05ee,$0660,$06c2,$0716,$0756,$0784,$07a2 ; 8-15
	dw $07aa,$07a2,$0784,$0756,$0716,$06c2,$0660,$05ee ; 16-23
	dw $056c,$04de,$0442,$039e,$02f0,$023a,$017e,$00c0 ; 24-31
	dw $0000,$ff42,$fe84,$fdc8,$fd12,$fc64,$fbc0,$fb24 ; 32-39
	dw $fa96,$fa14,$f9a2,$f940,$f8ec,$f8ac,$f87e,$f860 ; 40-47
	dw $f858,$f860,$f87e,$f8ac,$f8ec,$f940,$f9a2,$fa14 ; 48-55
	dw $fa96,$fb24,$fbc0,$fc64,$fd12,$fdc8,$fe84,$ff42 ; 56-63

mus_vibrato_sinus_30: ; depth 18.66 semi-tones
	dw $0000,$00ea,$01d2,$02b6,$0392,$0466,$0530,$05ec ; 0-7
	dw $069a,$0736,$07c2,$083c,$08a0,$08ee,$0928,$094a ; 8-15
	dw $0956,$094a,$0928,$08ee,$08a0,$083c,$07c2,$0736 ; 16-23
	dw $069a,$05ec,$0530,$0466,$0392,$02b6,$01d2,$00ea ; 24-31
	dw $0000,$ff18,$fe30,$fd4c,$fc70,$fb9c,$fad2,$fa16 ; 32-39
	dw $f968,$f8cc,$f840,$f7c6,$f762,$f714,$f6da,$f6b8 ; 40-47
	dw $f6ac,$f6b8,$f6da,$f714,$f762,$f7c6,$f840,$f8cc ; 48-55
	dw $f968,$fa16,$fad2,$fb9c,$fc70,$fd4c,$fe30,$ff18 ; 56-63

mus_vibrato_sinus_31: ; depth 24.00 semi-tones
	dw $0000,$012e,$0258,$037c,$0498,$05a8,$06aa,$079c ; 0-7
	dw $087c,$0946,$09fa,$0a96,$0b16,$0b7c,$0bc4,$0bf2 ; 8-15
	dw $0c00,$0bf2,$0bc4,$0b7c,$0b16,$0a96,$09fa,$0946 ; 16-23
	dw $087c,$079c,$06aa,$05a8,$0498,$037c,$0258,$012e ; 24-31
	dw $0000,$fed4,$fdaa,$fc86,$fb6a,$fa5a,$f958,$f866 ; 32-39
	dw $f786,$f6bc,$f608,$f56c,$f4ec,$f486,$f43e,$f410 ; 40-47
	dw $f402,$f410,$f43e,$f486,$f4ec,$f56c,$f608,$f6bc ; 48-55
	dw $f786,$f866,$f958,$fa5a,$fb6a,$fc86,$fdaa,$fed4 ; 56-63


mus_noisefreq_table:

	; for mode 1, add $40 to the number, ie, $40-$7f

	db %1111_0_111 ; $00 off
	db %1111_0_111 ; $01 off
	db %1111_0_111 ; $02 off
	db %1111_0_111 ; $03 off
  	db %1101_0_111 ; $04      4.6 hz (d-z)      4.6hz
  	db %1101_0_110 ; $05      5.3 hz (f-z)      5.5hz
  	db %1101_0_101 ; $06      6.4 hz (g#z)      6.5hz
  	db %1101_0_100 ; $07      8.0 hz (c-y)      8.1hz
  	db %1100_0_111 ; $08      9.1 hz (d-y)      9.2hz
  	db %1101_0_011 ; $09     10.6 hz (f-y)     10.9hz
  	db %1100_0_101 ; $0a     12.8 hz (g#y)     13.0hz
  	db %1100_0_100 ; $0b     16.0 hz (c-x)     16.4hz
  	db %1011_0_111 ; $0c     18.3 hz (d-x)     18.4hz
  	db %1100_0_011 ; $0d     21.3 hz (f-x)     21.8hz
  	db %1011_0_101 ; $0e     25.6 hz (g#x)     26.0hz
  	db %1011_0_100 ; $0f     32.0 hz (c-0)     32.7hz
  	db %1010_0_111 ; $10     36.6 hz (d-0)     36.7hz
  	db %1011_0_011 ; $11     42.6 hz (f-0)     43.6hz
  	db %1010_0_101 ; $12     51.2 hz (g#0)     51.9hz
  	db %1101_0_000 ; $13     64.0 hz (c-1)     65.4hz
  	db %1001_0_111 ; $14     73.1 hz (d-1)     73.4hz
  	db %1010_0_011 ; $15     85.3 hz (f-1)     87.3hz
  	db %1001_0_101 ; $16    102.4 hz (g#1)    103.8hz
  	db %1100_0_000 ; $17    128.0 hz (c-2)    130.8hz
  	db %1000_0_111 ; $18    146.3 hz (d-2)    146.8hz
  	db %1001_0_011 ; $19    170.6 hz (f-2)    174.6hz
  	db %1000_0_101 ; $1a    204.8 hz (g#2)    207.7hz
  	db %1011_0_000 ; $1b    256.0 hz (c-3)    261.6hz
  	db %0111_0_111 ; $1c    292.6 hz (d-3)    293.7hz
  	db %1000_0_011 ; $1d    341.3 hz (f-3)    349.2hz
  	db %0111_0_101 ; $1e    409.6 hz (g#3)    415.3hz
  	db %1010_0_000 ; $1f    512.0 hz (c-4)    523.3hz
  	db %0110_0_111 ; $20    585.1 hz (d-4)    587.3hz
  	db %0111_0_011 ; $21    682.6 hz (f-4)    698.5hz
  	db %0110_0_101 ; $22    819.2 hz (g#4)    830.6hz
  	db %1001_0_000 ; $23   1024.0 hz (c-5)   1046.5hz
  	db %0101_0_111 ; $24   1170.3 hz (d-5)   1174.7hz
  	db %0110_0_011 ; $25   1365.3 hz (f-5)   1396.9hz
  	db %0101_0_101 ; $26   1638.4 hz (g#5)   1661.2hz
  	db %1000_0_000 ; $27   2048.0 hz (c-6)   2093.0hz
  	db %0100_0_111 ; $28   2340.6 hz (d-6)   2349.3hz
  	db %0101_0_011 ; $29   2730.6 hz (f-6)   2793.8hz
  	db %0100_0_101 ; $2a   3276.8 hz (g#6)   3322.4hz
  	db %0111_0_000 ; $2b   4096.0 hz (c-7)   4186.0hz
  	db %0011_0_111 ; $2c   4681.1 hz (d-7)   4698.6hz
  	db %0100_0_011 ; $2d   5461.3 hz (f-7)   5587.7hz
  	db %0011_0_101 ; $2e   6553.6 hz (g#7)   6644.9hz
  	db %0110_0_000 ; $2f   8192.0 hz (c-8)   8372.0hz
  	db %0010_0_111 ; $30   9362.3 hz (d-8)   9397.3hz
  	db %0011_0_011 ; $31  10922.6 hz (f-8)  11175.3hz
  	db %0010_0_101 ; $32  13107.2 hz (g#8)  13289.8hz
  	db %0101_0_000 ; $33  16384.0 hz (c-9)  16744.0hz
  	db %0001_0_111 ; $34  18724.6 hz (d-9)  18794.5hz
  	db %0010_0_011 ; $35  21845.3 hz (f-9)  22350.6hz
  	db %0001_0_101 ; $36  26214.4 hz (g#9)  26579.5hz
  	db %0100_0_000 ; $37  32768.0 hz (c-a)  33488.1hz
  	db %0000_0_111 ; $38  37449.1 hz (d-a)  37589.1hz
  	db %0001_0_011 ; $39  43690.6 hz (f-a)  44701.2hz
  	db %0000_0_101 ; $3a  52428.8 hz (g#a)  53159.0hz
  	db %0011_0_000 ; $3b  65536.0 hz (c-b)  66976.1hz
  	db %0000_0_011 ; $3c  87381.3 hz (f-b)  89402.4hz
  	db %0010_0_000 ; $3d 131072.0 hz (c-c) 133952.3hz
  	db %0001_0_000 ; $3e 262144.0 hz (c-d) 267904.6hz
  	db %0000_0_000 ; $3f 524288.0 hz (c-e) 535809.2hz

; shuffle sinus tables

mus_shuffle_sinus_0: ; depth 32,32
	db $20,$20,$20,$20,$20,$20,$20,$20 ; 0-7
	db $20,$20,$20,$20,$20,$20,$20,$20 ; 8-15

mus_shuffle_sinus_1: ; depth 30,34
	db $22,$21,$21,$20,$1f,$1f,$1e,$1e ; 0-7
	db $1e,$1e,$1e,$1f,$20,$20,$21,$21 ; 8-15

mus_shuffle_sinus_2: ; depth 28,36
	db $24,$23,$22,$21,$1f,$1e,$1d,$1c ; 0-7
	db $1c,$1c,$1d,$1e,$20,$21,$22,$23 ; 8-15

mus_shuffle_sinus_3: ; depth 26,38
	db $26,$25,$24,$22,$1f,$1d,$1b,$1a ; 0-7
	db $1a,$1a,$1b,$1d,$20,$22,$24,$25 ; 8-15

mus_shuffle_sinus_4: ; depth 24,40
	db $28,$27,$25,$23,$1f,$1c,$1a,$18 ; 0-7
	db $18,$18,$1a,$1c,$20,$23,$25,$27 ; 8-15

mus_shuffle_sinus_5: ; depth 22,42
	db $2a,$29,$27,$23,$1f,$1c,$18,$16 ; 0-7
	db $16,$16,$18,$1c,$20,$23,$27,$29 ; 8-15

mus_shuffle_sinus_6: ; depth 20,44
	db $2c,$2b,$28,$24,$1f,$1b,$17,$14 ; 0-7
	db $14,$14,$17,$1b,$20,$24,$28,$2b ; 8-15

mus_shuffle_sinus_7: ; depth 18,46
	db $2e,$2c,$29,$25,$1f,$1a,$16,$13 ; 0-7
	db $12,$13,$16,$1a,$20,$25,$29,$2c ; 8-15

mus_shuffle_sinus_8: ; depth 16,48
	db $30,$2e,$2b,$26,$1f,$19,$14,$11 ; 0-7
	db $10,$11,$14,$19,$20,$26,$2b,$2e ; 8-15

mus_shuffle_sinus_9: ; depth 14,50
	db $32,$30,$2c,$26,$1f,$19,$13,$0f ; 0-7
	db $0e,$0f,$13,$19,$20,$26,$2c,$30 ; 8-15

mus_shuffle_sinus_10: ; depth 12,52
	db $34,$32,$2e,$27,$1f,$18,$11,$0d ; 0-7
	db $0c,$0d,$11,$18,$20,$27,$2e,$32 ; 8-15

mus_shuffle_sinus_11: ; depth 10,54
	db $36,$34,$2f,$28,$1f,$17,$10,$0b ; 0-7
	db $0a,$0b,$10,$17,$20,$28,$2f,$34 ; 8-15

mus_shuffle_sinus_12: ; depth 8,56
	db $38,$36,$30,$29,$1f,$16,$0f,$09 ; 0-7
	db $08,$09,$0f,$16,$20,$29,$30,$36 ; 8-15

mus_shuffle_sinus_13: ; depth 6,58
	db $3a,$38,$32,$29,$1f,$16,$0d,$07 ; 0-7
	db $06,$07,$0d,$16,$20,$29,$32,$38 ; 8-15

mus_shuffle_sinus_14: ; depth 4,60
	db $3c,$39,$33,$2a,$1f,$15,$0c,$06 ; 0-7
	db $04,$06,$0c,$15,$20,$2a,$33,$39 ; 8-15

mus_shuffle_sinus_15: ; depth 2,62
	db $3e,$3b,$35,$2b,$1f,$14,$0a,$04 ; 0-7
	db $02,$04,$0a,$14,$20,$2b,$35,$3b ; 8-15

mus_shuffle_rates:
	dw $0000,$0800,$0556,$0400
	dw $0334,$02ab,$024a,$0200
	dw $01c8,$019a,$0175,$0156
	dw $013c,$0125,$0112,$0100


mus_gain_matrix:

; $00
; nothing to normal, using smooth log matrix

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88 ;  3
	db $66,$66,$67,$77,$88,$89,$99,$99 ;  5
	db $55,$56,$66,$77,$88,$99,$9a,$aa ;  7
	db $44,$55,$66,$77,$88,$99,$aa,$bb ;  9
	db $33,$45,$56,$67,$89,$9a,$ab,$cc ; 11
	db $23,$34,$56,$67,$89,$9a,$bc,$cd ; 13
	db $12,$34,$45,$67,$89,$ab,$bc,$de ; 15

; $08
; normal to +6db using smooth log matrix

	db $01,$23,$45,$67,$89,$ab,$cd,$ef
	db $00,$12,$45,$67,$89,$ab,$de,$ff ; 17
	db $00,$12,$35,$67,$89,$bc,$de,$ff ; 19
	db $00,$01,$34,$67,$89,$bc,$ef,$ff ; 21
	db $00,$01,$24,$57,$8a,$bd,$ef,$ff ; 23
	db $00,$00,$24,$57,$8a,$bd,$ff,$ff ; 25
	db $00,$00,$23,$57,$8a,$cd,$ff,$ff ; 27
	db $00,$00,$13,$57,$8a,$ce,$ff,$ff ; 29

; $10
; nothing to normal, using dirty triangular matrix

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88
	db $67,$77,$77,$77,$88,$88,$88,$89
	db $56,$77,$77,$77,$88,$88,$88,$9a
	db $45,$67,$77,$77,$88,$88,$89,$ab
	db $34,$56,$77,$77,$88,$88,$9a,$bc
	db $23,$45,$67,$77,$88,$89,$ab,$cd
	db $12,$34,$56,$77,$88,$9a,$bc,$de

; $18
; normal to fully clipping square using dirty triangular matrix

	db $01,$23,$45,$67,$89,$ab,$cd,$ef
	db $00,$12,$34,$56,$9a,$bc,$de,$ff
	db $00,$01,$23,$45,$ab,$cd,$ef,$ff
	db $00,$00,$12,$34,$bc,$de,$ff,$ff
	db $00,$00,$01,$23,$cd,$ef,$ff,$ff
	db $00,$00,$00,$12,$de,$ff,$ff,$ff
	db $00,$00,$00,$01,$ef,$ff,$ff,$ff
	db $00,$00,$00,$00,$ff,$ff,$ff,$ff

; $20
; nothing to normal, using smooth log+fuzz matrix

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88 ;  3
	db $66,$66,$67,$78,$78,$89,$99,$99 ;  5
	db $55,$56,$66,$78,$78,$99,$9a,$aa ;  7
	db $44,$55,$66,$79,$69,$99,$aa,$bb ;  9
	db $33,$45,$56,$69,$69,$9a,$ab,$cc ; 11
	db $23,$34,$56,$6a,$5a,$9a,$bc,$cd ; 13

; $28
; normal to +6db using smooth log+fuzz matrix

	db $12,$34,$45,$6a,$59,$ab,$bc,$de ; 15
	db $00,$12,$45,$6b,$49,$ab,$de,$ff ; 17
	db $00,$12,$35,$6b,$49,$bc,$de,$ff ; 19
	db $00,$01,$34,$6b,$49,$bc,$ef,$ff ; 21
	db $00,$01,$24,$5c,$3a,$bd,$ef,$ff ; 23
	db $00,$00,$24,$5c,$3a,$bd,$ff,$ff ; 25
	db $00,$00,$23,$5c,$3a,$cd,$ff,$ff ; 27
	db $00,$00,$13,$5d,$2a,$ce,$ff,$ff ; 29

; $30
; nothing to square wave, using square matrix

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88
	db $66,$66,$77,$77,$88,$88,$99,$99
	db $55,$55,$67,$77,$88,$89,$aa,$aa
	db $44,$44,$67,$77,$88,$89,$bb,$bb
	db $33,$33,$57,$77,$88,$8a,$cc,$cc
	db $22,$22,$57,$77,$88,$8a,$dd,$dd
	db $11,$11,$47,$77,$88,$8b,$ee,$ee

; $38
; square to more square, using square matrix

	db $00,$00,$47,$77,$88,$8b,$ff,$ff
	db $00,$00,$04,$77,$88,$bf,$ff,$ff
	db $00,$00,$00,$47,$8b,$ff,$ff,$ff
	db $00,$00,$00,$04,$bf,$ff,$ff,$ff
	db $00,$00,$00,$00,$ff,$ff,$ff,$ff
	db $00,$04,$77,$77,$88,$88,$bf,$ff
	db $00,$47,$77,$77,$88,$88,$8b,$ff
	db $04,$77,$77,$77,$88,$88,$88,$bf

; $40
; sonic decimator type square waving

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88
	db $66,$66,$77,$77,$88,$88,$99,$99
	db $55,$55,$67,$77,$88,$89,$aa,$aa
	db $44,$46,$66,$77,$88,$99,$9b,$bb
	db $33,$35,$55,$77,$88,$aa,$ac,$cc
	db $22,$25,$55,$77,$88,$aa,$ad,$dd
	db $11,$33,$55,$77,$88,$aa,$cc,$ee

; $48
; sonic decimator type square waving to square

	db $00,$22,$44,$66,$99,$bb,$dd,$ff
	db $00,$03,$33,$66,$99,$cc,$cf,$ff
	db $00,$00,$44,$44,$bb,$bb,$ff,$ff
	db $00,$00,$07,$77,$88,$8f,$ff,$ff
	db $00,$00,$00,$77,$88,$ff,$ff,$ff
	db $00,$00,$00,$07,$8f,$ff,$ff,$ff
	db $00,$00,$00,$00,$ff,$ff,$ff,$ff
	db $00,$00,$00,$00,$ff,$ff,$ff,$ff

; $50
; nothing to gated, using smooth log matrix

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88 ;  1
	db $77,$77,$77,$77,$88,$88,$88,$88 ;  3
	db $66,$66,$77,$77,$88,$88,$99,$99 ;  5
	db $55,$56,$77,$77,$88,$88,$9a,$aa ;  7
	db $44,$55,$77,$77,$88,$88,$aa,$bb ;  9
	db $33,$45,$77,$77,$88,$88,$ab,$cc ; 11
	db $23,$34,$77,$77,$88,$88,$bc,$cd ; 13

; $58
; gated to compressed

	db $01,$23,$77,$77,$88,$88,$cd,$ef
	db $01,$23,$66,$77,$88,$99,$cd,$ef
	db $01,$23,$56,$67,$89,$9a,$cd,$ef
	db $01,$23,$55,$66,$99,$aa,$cd,$ef
	db $01,$23,$45,$56,$9a,$ab,$cd,$ef
	db $01,$23,$44,$55,$aa,$bb,$cd,$ef
	db $01,$23,$34,$44,$bb,$bc,$cd,$ef
	db $01,$23,$33,$33,$cc,$cc,$cd,$ef

; $60
; nothing to tube, using smooth log matrix

	db $77,$77,$77,$77,$77,$77,$77,$77 ;  1
	db $88,$88,$77,$77,$88,$88,$77,$77 ;  3
	db $99,$99,$77,$77,$88,$88,$66,$66 ;  5
	db $aa,$aa,$77,$77,$88,$98,$65,$55 ;  7
	db $bb,$ba,$77,$77,$88,$98,$55,$44 ;  9
	db $cc,$cb,$76,$67,$89,$99,$54,$33 ; 11
	db $dd,$cc,$76,$67,$89,$99,$43,$32 ; 13
	db $ee,$dc,$76,$67,$89,$a9,$43,$21 ; 15

; $68
; tube to +6db tube using smooth log matrix

	db $ff,$ed,$76,$67,$89,$a9,$32,$10
	db $ff,$fe,$76,$67,$89,$a9,$21,$00 ; 17
	db $ff,$fe,$76,$67,$89,$b9,$21,$00 ; 19
	db $ff,$ff,$76,$67,$89,$b9,$10,$00 ; 21
	db $ff,$ff,$75,$57,$8a,$ba,$10,$00 ; 23
	db $ff,$ff,$75,$57,$8a,$ba,$00,$00 ; 25
	db $ff,$ff,$75,$57,$8a,$ca,$00,$00 ; 27
	db $ff,$ff,$75,$57,$8a,$ca,$00,$00 ; 29

; $70
; nothing to noise distorted, using noise matrix

	db $77,$77,$77,$77,$88,$88,$88,$88
	db $76,$77,$77,$77,$88,$88,$88,$98
	db $65,$77,$77,$77,$88,$88,$88,$a9
	db $54,$76,$77,$77,$88,$88,$98,$ba
	db $43,$65,$77,$77,$88,$88,$a9,$cb
	db $32,$54,$76,$77,$88,$98,$ba,$dc
	db $21,$43,$65,$77,$88,$a9,$cb,$ed
	db $10,$32,$54,$76,$98,$ba,$dc,$fe

; $78
; noise distorted to various noise distortions, using noise matrix
	
	db $31,$02,$54,$76,$98,$ba,$df,$ce
	db $e1,$20,$54,$76,$98,$ba,$fd,$c3
	db $1e,$25,$04,$76,$98,$bf,$ad,$3c
	db $12,$e5,$40,$76,$98,$fb,$a3,$dc
	db $12,$5e,$47,$06,$9f,$8b,$3a,$dc
	db $12,$54,$e7,$60,$f9,$83,$ba,$dc
	db $15,$24,$7e,$6f,$09,$38,$bd,$ac
	db $45,$21,$7e,$f6,$90,$38,$cd,$ab


mus_default_waveform:

sine_wave equ $00

	db $76,$53,$21,$10,$00,$11,$23,$56,$89,$ac,$de,$ef,$ff,$ee,$dc,$a9 ; $00 sine
	db $65,$42,$10,$01,$11,$00,$12,$45,$9a,$bd,$ef,$fe,$ee,$ff,$ed,$ba ; $01 sine feedback
	db $54,$31,$01,$12,$22,$11,$01,$34,$ab,$ce,$fe,$ed,$dd,$ee,$fe,$cb ; $02 sine feedback
	db $43,$20,$12,$23,$33,$22,$10,$23,$bc,$df,$ed,$dc,$cc,$dd,$ef,$dc ; $03 sine feedback
	db $32,$11,$23,$34,$44,$33,$21,$12,$cd,$ee,$dc,$cb,$bb,$cc,$de,$ed ; $04 sine feedback
	db $21,$02,$34,$45,$55,$44,$32,$01,$de,$fd,$cb,$ba,$aa,$bb,$cd,$fe ; $05 sine feedback
	db $10,$13,$45,$56,$66,$55,$43,$10,$ef,$ec,$ba,$a9,$99,$aa,$bc,$ef ; $06 sine feedback
	db $01,$24,$56,$67,$77,$66,$54,$21,$fe,$db,$a9,$98,$88,$99,$ab,$de ; $07 sine feedback
	db $12,$35,$67,$78,$88,$77,$65,$32,$ed,$ca,$98,$87,$77,$88,$9a,$cd ; $08 sine feedback
	db $23,$46,$78,$87,$77,$88,$76,$43,$dc,$b9,$87,$78,$88,$77,$89,$bc ; $09 sine feedback
	db $34,$57,$87,$76,$66,$77,$87,$54,$cb,$a8,$78,$89,$99,$88,$78,$ab ; $0a sine feedback
	db $45,$68,$76,$65,$55,$66,$78,$65,$ba,$97,$89,$9a,$aa,$99,$87,$9a ; $0b sine feedback
	db $56,$77,$65,$54,$44,$55,$67,$76,$a9,$88,$9a,$ab,$bb,$aa,$98,$89 ; $0c sine feedback
	db $67,$86,$54,$43,$33,$44,$56,$87,$98,$99,$ab,$bc,$cc,$bb,$a9,$78 ; $0d sine feedback
	db $78,$75,$43,$32,$22,$33,$45,$78,$87,$9a,$bc,$cd,$dd,$cc,$ba,$87 ; $0e sine feedback
	db $87,$64,$32,$21,$11,$22,$34,$67,$98,$9b,$cd,$de,$ee,$dd,$cb,$98 ; $0f sine feedback to sine

	db $76,$64,$32,$21,$11,$22,$34,$67,$98,$9b,$cd,$de,$ee,$dd,$cb,$98 ; $10 sine amplify to square
	db $65,$53,$21,$10,$00,$11,$23,$56,$a9,$ac,$de,$ef,$ff,$ee,$dc,$a9 ; $11 sine amplify to square
	db $54,$42,$10,$00,$00,$00,$12,$45,$ba,$bd,$ef,$ff,$ff,$ff,$ed,$ba ; $12 sine amplify to square
	db $43,$31,$00,$00,$00,$00,$01,$34,$cb,$ce,$ff,$ff,$ff,$ff,$fe,$cb ; $13 sine amplify to square
	db $32,$20,$00,$00,$00,$00,$00,$23,$dc,$df,$ff,$ff,$ff,$ff,$ff,$dc ; $14 sine amplify to square
	db $21,$10,$00,$00,$00,$00,$00,$12,$ed,$ef,$ff,$ff,$ff,$ff,$ff,$ed ; $15 sine amplify to square
	db $10,$00,$00,$00,$00,$00,$00,$01,$fe,$ff,$ff,$ff,$ff,$ff,$ff,$fe ; $16 sine amplify to square
	db $00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $17 sine amplify to square

square_wave	equ $18

	db $00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $18 square
	db $50,$00,$00,$00,$00,$00,$00,$05,$af,$ff,$ff,$ff,$ff,$ff,$ff,$fa ; $19 square to triangle
	db $63,$00,$00,$00,$00,$00,$00,$36,$9c,$ff,$ff,$ff,$ff,$ff,$ff,$c9 ; $1a square to triangle
	db $64,$20,$00,$00,$00,$00,$02,$46,$9b,$df,$ff,$ff,$ff,$ff,$fd,$b9 ; $1b square to triangle
	db $75,$32,$00,$00,$00,$00,$23,$57,$8a,$cd,$ff,$ff,$ff,$ff,$dc,$a8 ; $1c square to triangle
	db $75,$43,$10,$00,$00,$01,$34,$57,$8a,$bc,$ef,$ff,$ff,$fe,$cb,$a8 ; $1d square to triangle
	db $76,$53,$21,$00,$00,$12,$35,$67,$89,$ac,$de,$ff,$ff,$ed,$ca,$98 ; $1e square to triangle
	db $76,$54,$32,$10,$01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98 ; $1f square to triangle

triangle_wave	equ $20

	db $76,$54,$32,$10,$01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98 ; $20 triangle
	db $76,$53,$21,$00,$12,$23,$45,$66,$89,$ac,$de,$ff,$ed,$dc,$ba,$99 ; $21 triangle to flipping saw
	db $75,$43,$10,$01,$12,$34,$45,$66,$8a,$bc,$ef,$fe,$ed,$cb,$ba,$99 ; $22 triangle to flipping saw
	db $75,$32,$00,$11,$23,$34,$55,$67,$8a,$cd,$ff,$ee,$dc,$cb,$aa,$98 ; $23 triangle to flipping saw
	db $64,$20,$01,$12,$23,$44,$55,$67,$9b,$df,$fe,$ed,$dc,$bb,$aa,$98 ; $24 triangle to flipping saw
	db $63,$00,$11,$22,$33,$44,$56,$67,$9c,$ff,$ee,$dd,$cc,$bb,$a9,$98 ; $25 triangle to flipping saw
	db $50,$01,$12,$23,$34,$45,$56,$67,$af,$fe,$ed,$dc,$cb,$ba,$a9,$98 ; $26 triangle to flipping saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$ff,$fe,$ed,$dc,$cb,$ba,$a9,$98 ; $27 triangle to flipping saw

flipping_saw_wave	equ $28

	db $00,$01,$12,$23,$34,$45,$56,$67,$ff,$fe,$ed,$dc,$cb,$ba,$a9,$98 ; $28 flipping saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$ef,$fe,$ed,$dc,$cb,$ba,$a9,$99 ; $29 flipping saw to saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$de,$ee,$ed,$dc,$cb,$ba,$aa,$aa ; $2a flipping saw to saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$cd,$de,$ed,$dc,$cb,$ba,$ab,$bb ; $2b flipping saw to saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$bc,$cd,$dd,$dc,$cb,$bb,$bc,$cc ; $2c flipping saw to saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$ab,$bc,$cd,$dc,$cb,$bc,$cd,$dd ; $2d flipping saw to saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$9a,$ab,$bc,$cc,$cc,$cd,$de,$ee ; $2e flipping saw to saw
	db $00,$01,$12,$23,$34,$45,$56,$67,$89,$9a,$ab,$bc,$cd,$de,$ef,$ff ; $2f flipping saw to saw

saw_wave		equ $30

	db $00,$01,$12,$23,$34,$45,$56,$67,$89,$9a,$ab,$bc,$cd,$de,$ef,$ff ; $30 saw
	db $00,$01,$12,$23,$33,$34,$46,$67,$89,$9a,$ab,$bc,$cd,$de,$ef,$ff ; $31 saw to distorted saw
	db $00,$01,$12,$23,$32,$24,$45,$67,$89,$9a,$ab,$bc,$cd,$de,$ef,$ff ; $32 saw to distorted saw
	db $00,$01,$12,$23,$31,$13,$35,$57,$89,$9a,$ab,$bc,$cd,$de,$ef,$ff ; $33 saw to distorted saw
	db $00,$01,$12,$23,$30,$02,$24,$56,$89,$9a,$ab,$bb,$bc,$ce,$ef,$ff ; $34 saw to distorted saw
	db $00,$01,$12,$23,$30,$02,$24,$56,$89,$9a,$ab,$ba,$ab,$be,$ef,$ff ; $35 saw to distorted saw
	db $00,$01,$12,$23,$30,$01,$13,$46,$89,$9a,$ab,$b9,$8a,$ad,$de,$ff ; $36 saw to distorted saw
	db $00,$01,$12,$23,$30,$00,$13,$46,$89,$9a,$ab,$b8,$89,$ab,$cd,$ef ; $37 saw to distorted saw

distorted_saw_wave	equ $38

	db $00,$01,$12,$23,$30,$00,$13,$46,$89,$9a,$ab,$b8,$89,$ab,$cd,$ef ; $38 distorted saw
	db $00,$01,$22,$34,$52,$21,$23,$45,$79,$8a,$ab,$b9,$9a,$ab,$bd,$cd ; $39 distorted saw to octave triangle
	db $00,$12,$33,$55,$74,$42,$34,$44,$69,$7a,$9b,$ba,$aa,$ab,$ad,$bb ; $3a distorted saw to octave triangle
	db $01,$23,$44,$66,$96,$54,$44,$44,$59,$6a,$9b,$bb,$bb,$aa,$9d,$a9 ; $3b distorted saw to octave triangle
	db $01,$23,$57,$97,$a8,$76,$44,$34,$39,$6a,$9a,$cb,$cb,$ba,$9d,$97 ; $3c distorted saw to octave triangle
	db $01,$34,$68,$a8,$ca,$87,$55,$33,$29,$5a,$8a,$cc,$dc,$ba,$8d,$75 ; $3d distorted saw to octave triangle
	db $02,$45,$79,$bd,$ec,$a8,$65,$32,$19,$4a,$8a,$cd,$ec,$b9,$8d,$53 ; $3e distorted saw to octave triangle
	db $02,$46,$8a,$ce,$fd,$b9,$75,$31,$02,$46,$8a,$ce,$fd,$b9,$75,$31 ; $3f distorted saw to octave triangle

octave_triangle_wave	equ $40

	db $02,$46,$8a,$ce,$fd,$b9,$75,$31,$02,$46,$8a,$ce,$fd,$b9,$75,$31 ; $40 octave triangle
	db $02,$56,$9a,$be,$dd,$b9,$86,$41,$02,$56,$9a,$be,$dd,$b9,$86,$41 ; $41 octave triangle to 2 octave triangle
	db $03,$56,$aa,$be,$bd,$a9,$97,$42,$03,$56,$aa,$be,$bd,$a9,$97,$42 ; $42 octave triangle to 2 octave triangle
	db $03,$66,$ba,$ae,$9d,$a9,$a8,$52,$03,$66,$ba,$ae,$9d,$a9,$a8,$52 ; $43 octave triangle to 2 octave triangle
	db $03,$66,$cb,$9e,$6d,$99,$c8,$52,$03,$66,$cb,$9e,$6d,$99,$c8,$52 ; $44 octave triangle to 2 octave triangle
	db $04,$76,$db,$8e,$4d,$99,$d9,$63,$04,$76,$db,$8e,$4d,$99,$d9,$63 ; $45 octave triangle to 2 octave triangle
	db $04,$76,$eb,$8e,$2d,$89,$ea,$63,$04,$76,$eb,$8e,$2d,$89,$ea,$63 ; $46 octave triangle to 2 octave triangle
	db $04,$8c,$fb,$73,$04,$8c,$fb,$73,$04,$8c,$fb,$73,$04,$8c,$fb,$73 ; $47 octave triangle to 2 octave triangle

octave_2_triangle_wave	equ $48

	db $04,$8c,$fb,$73,$04,$8c,$fb,$73,$04,$8c,$fb,$73,$04,$8c,$fb,$73 ; $48 2 octave triangle
	db $03,$69,$cb,$fb,$c9,$63,$03,$69,$cb,$fb,$c9,$63,$03,$69,$cb,$fb ; $49 2 octave triangle tune spread saw
	db $02,$46,$8a,$ce,$fd,$b9,$75,$10,$24,$68,$ac,$ef,$db,$97,$51,$02 ; $4a 2 octave triangle tune spread saw
	db $01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98,$76,$54,$32,$10 ; $4b 2 octave triangle tune spread saw
	db $00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff ; $4c tune spread saw
	db $00,$88,$22,$88,$33,$88,$55,$88,$66,$88,$77,$88,$88,$dd,$77,$ff ; $4d tune spread saw to feedback
	db $00,$cc,$22,$cc,$22,$cc,$33,$cc,$33,$cc,$33,$cc,$44,$dd,$33,$ff ; $4e tune spread saw to feedback
	db $00,$dd,$22,$ff,$00,$dd,$22,$ff,$00,$dd,$22,$ff,$00,$dd,$22,$ff ; $4f tune spread saw to feedback

feedback_wave	equ $50

	db $00,$bc,$22,$cf,$20,$bc,$22,$cc,$22,$dd,$34,$fd,$02,$dc,$33,$ff ; $50 feedback
	db $00,$9a,$32,$ae,$40,$ab,$32,$aa,$44,$dd,$46,$fb,$14,$dc,$45,$ff ; $51 feedback to ringing square
	db $00,$79,$31,$8e,$60,$8a,$32,$88,$66,$dc,$58,$f9,$16,$eb,$66,$ff ; $52 feedback to ringing square
	db $00,$88,$41,$6d,$70,$69,$32,$66,$88,$dc,$6a,$f8,$28,$eb,$77,$ff ; $53 feedback to ringing square
	db $00,$67,$41,$4d,$80,$48,$42,$44,$aa,$dc,$7c,$f7,$2a,$ea,$99,$ff ; $54 feedback to ringing square
	db $00,$45,$50,$2c,$a0,$27,$42,$22,$cd,$db,$8e,$f5,$3c,$fa,$ac,$ff ; $55 feedback to ringing square
	db $00,$24,$60,$0c,$c0,$06,$42,$00,$ff,$db,$9f,$f3,$3f,$f9,$bd,$ff ; $56 feedback to ringing square
	db $11,$24,$60,$0a,$a0,$06,$42,$00,$ee,$db,$9f,$f5,$5f,$f9,$bd,$ff ; $57 feedback to ringing square

	db $22,$24,$50,$08,$80,$06,$42,$00,$dd,$db,$af,$f7,$7f,$f9,$bd,$ff ; $58 ringing square
	db $33,$33,$40,$06,$60,$06,$42,$00,$cc,$ca,$bf,$f9,$9f,$f9,$bd,$ff ; $59 ringing square to sine phase
	db $44,$33,$30,$04,$40,$06,$42,$00,$bb,$ca,$cf,$fb,$bf,$f9,$bd,$ff ; $5a ringing square to sine phase
	db $55,$42,$20,$02,$20,$06,$42,$00,$aa,$b9,$df,$fd,$df,$f9,$bd,$ff ; $5b ringing square to sine phase
	db $66,$42,$20,$01,$10,$07,$42,$00,$99,$b9,$df,$fe,$ef,$f8,$bd,$ff ; $5c ringing square to sine phase
	db $76,$42,$10,$00,$00,$08,$52,$00,$89,$b9,$ef,$ff,$ff,$f7,$ad,$ff ; $5d ringing square to sine phase
	db $76,$42,$10,$00,$00,$09,$63,$00,$89,$b9,$ef,$ff,$ff,$f6,$9c,$ff ; $5e ringing square to sine phase
	db $76,$42,$10,$00,$00,$0a,$74,$00,$89,$b9,$ef,$ff,$ff,$f5,$8b,$ff ; $5f ringing square to sine phase

sine_phase_wave	equ $60

	db $f6,$42,$10,$00,$00,$01,$a7,$40,$09,$b9,$ef,$ff,$ff,$fe,$58,$bf ; $60 sine phase sweep
	db $ff,$42,$10,$00,$00,$01,$2a,$74,$00,$b9,$ef,$ff,$ff,$fe,$d5,$8b ; $61 sine phase sweep
	db $bf,$f2,$10,$00,$00,$01,$24,$a7,$40,$09,$ef,$ff,$ff,$fe,$db,$58 ; $62 sine phase sweep
	db $8b,$ff,$10,$00,$00,$01,$24,$6a,$74,$00,$ef,$ff,$ff,$fe,$db,$a5 ; $63 sine phase sweep
	db $58,$bf,$f0,$00,$00,$01,$24,$67,$a7,$40,$0f,$ff,$ff,$fe,$db,$a9 ; $64 sine phase sweep
	db $75,$8b,$ff,$00,$00,$01,$24,$67,$9a,$74,$00,$ff,$ff,$fe,$db,$a9 ; $65 sine phase sweep
	db $76,$58,$bf,$f0,$00,$01,$24,$67,$9a,$a7,$40,$0f,$ff,$fe,$db,$a9 ; $66 sine phase sweep
	db $76,$45,$8b,$ff,$00,$01,$24,$67,$9a,$ba,$74,$00,$ff,$fe,$db,$a9 ; $67 sine phase sweep
	db $76,$42,$58,$bf,$f0,$01,$24,$67,$9a,$bd,$a7,$40,$0f,$fe,$db,$a9 ; $68 sine phase sweep
	db $76,$42,$15,$8b,$ff,$01,$24,$67,$9a,$bd,$ea,$74,$00,$fe,$db,$a9 ; $69 sine phase sweep
	db $76,$42,$10,$58,$bf,$f1,$24,$67,$9a,$bd,$ef,$a7,$40,$0e,$db,$a9 ; $6a sine phase sweep
	db $76,$42,$10,$05,$8b,$ff,$24,$67,$9a,$bd,$ef,$fa,$74,$00,$db,$a9 ; $6b sine phase sweep
	db $76,$42,$10,$00,$58,$bf,$f4,$67,$9a,$bd,$ef,$ff,$a7,$40,$0b,$a9 ; $6c sine phase sweep
	db $76,$42,$10,$00,$05,$8b,$ff,$67,$9a,$bd,$ef,$ff,$fa,$74,$00,$a9 ; $6d sine phase sweep
	db $76,$42,$10,$00,$00,$58,$bf,$f7,$9a,$bd,$ef,$ff,$ff,$a7,$40,$09 ; $6e sine phase sweep
	db $76,$42,$10,$00,$00,$05,$8b,$ff,$9a,$bd,$ef,$ff,$ff,$fa,$74,$00 ; $6f sine phase sweep
	db $06,$42,$10,$00,$00,$01,$58,$bf,$fa,$bd,$ef,$ff,$ff,$fe,$a7,$40 ; $70 sine phase sweep
	db $00,$42,$10,$00,$00,$01,$25,$8b,$ff,$bd,$ef,$ff,$ff,$fe,$da,$74 ; $71 sine phase sweep
	db $40,$02,$10,$00,$00,$01,$24,$58,$bf,$fd,$ef,$ff,$ff,$fe,$db,$a7 ; $72 sine phase sweep
	db $74,$00,$10,$00,$00,$01,$24,$65,$8b,$ff,$ef,$ff,$ff,$fe,$db,$aa ; $73 sine phase sweep
	db $a7,$40,$00,$00,$00,$01,$24,$67,$58,$bf,$ff,$ff,$ff,$fe,$db,$a9 ; $74 sine phase sweep
	db $7a,$74,$00,$00,$00,$01,$24,$67,$95,$8b,$ff,$ff,$ff,$fe,$db,$a9 ; $75 sine phase sweep
	db $76,$a7,$40,$00,$00,$01,$24,$67,$9a,$58,$bf,$ff,$ff,$fe,$db,$a9 ; $76 sine phase sweep
	db $76,$4a,$74,$00,$00,$01,$24,$67,$9a,$b5,$8b,$ff,$ff,$fe,$db,$a9 ; $77 sine phase sweep
	db $76,$42,$a7,$40,$00,$01,$24,$67,$9a,$bd,$58,$bf,$ff,$fe,$db,$a9 ; $78 sine phase sweep
	db $76,$42,$1a,$74,$00,$01,$24,$67,$9a,$bd,$e5,$8b,$ff,$fe,$db,$a9 ; $79 sine phase sweep
	db $76,$42,$10,$a7,$40,$01,$24,$67,$9a,$bd,$ef,$58,$bf,$fe,$db,$a9 ; $7a sine phase sweep
	db $76,$42,$10,$0a,$74,$00,$24,$67,$9a,$bd,$ef,$f5,$8b,$ff,$db,$a9 ; $7b sine phase sweep
	db $76,$42,$10,$00,$a7,$40,$04,$67,$9a,$bd,$ef,$ff,$58,$bf,$fb,$a9 ; $7c sine phase sweep
	db $76,$42,$10,$00,$0a,$74,$00,$67,$9a,$bd,$ef,$ff,$f5,$8b,$ff,$a9 ; $7d sine phase sweep
	db $76,$42,$10,$00,$00,$a7,$40,$07,$9a,$bd,$ef,$ff,$ff,$58,$bf,$f9 ; $7e sine phase sweep
	db $76,$42,$10,$00,$00,$0a,$74,$00,$9a,$bd,$ef,$ff,$ff,$f5,$8b,$ff ; $7f sine phase sweep

distorted_sine_wave	equ $80

	db $76,$42,$10,$00,$00,$08,$64,$22,$9a,$bd,$ef,$ff,$ff,$f7,$9b,$ee ; $80 distorted sine
	db $76,$42,$10,$00,$00,$06,$54,$43,$9a,$bd,$ef,$ff,$ff,$f9,$9b,$dd ; $81 distorted sine
	db $76,$33,$01,$01,$10,$14,$53,$54,$a9,$cc,$fe,$fe,$ef,$eb,$ac,$bc ; $82 distorted sine
	db $67,$33,$01,$01,$10,$12,$43,$65,$a9,$cc,$fe,$fe,$ef,$ed,$ac,$ab ; $83 distorted sine
	db $67,$24,$01,$01,$10,$10,$42,$76,$a9,$db,$fe,$fe,$ef,$ef,$bd,$9a ; $84 distorted sine
	db $58,$35,$10,$10,$01,$01,$33,$67,$9a,$ea,$fd,$df,$df,$df,$ae,$8b ; $85 distorted sine
	db $49,$46,$20,$20,$02,$02,$24,$58,$8b,$f9,$fc,$cf,$cf,$cf,$9f,$7c ; $86 distorted sine
	db $3a,$57,$30,$30,$03,$03,$15,$49,$7c,$f8,$fb,$bf,$bf,$bf,$8f,$6d ; $87 distorted sine
	db $3a,$47,$40,$40,$04,$04,$06,$39,$6d,$f7,$fa,$af,$af,$af,$7f,$5d ; $88 distorted sine
	db $2b,$38,$50,$50,$05,$05,$07,$2a,$5e,$f6,$f9,$9f,$9f,$9f,$6f,$4e ; $89 distorted sine
	db $1c,$29,$60,$60,$06,$06,$08,$1b,$4f,$f5,$f8,$8f,$8f,$8f,$5f,$3f ; $8a distorted sine
	db $0d,$1a,$70,$70,$07,$07,$09,$0c,$3f,$f4,$f7,$7f,$7f,$7f,$4f,$2f ; $8b distorted sine
	db $0e,$0b,$80,$80,$08,$08,$0a,$0d,$2f,$f3,$f6,$6f,$6f,$6f,$3f,$1f ; $8c distorted sine
	db $0f,$0c,$90,$90,$09,$09,$0b,$0e,$1f,$f2,$f5,$5f,$5f,$5f,$2f,$0f ; $8d distorted sine
	db $0f,$0d,$a0,$a0,$0a,$0a,$0c,$0f,$0f,$f1,$f4,$4f,$4f,$4f,$1f,$0f ; $8e distorted sine
	db $0f,$0e,$b0,$b0,$0b,$0b,$0d,$0f,$0f,$f0,$f3,$3f,$3f,$3f,$0f,$0f ; $8f distorted sine

	; incomplete from here on - don't use these yet

	db $0f,$0f,$c0,$c0,$0c,$0c,$0e,$0f,$0f,$f0,$f3,$3f,$3f,$3f,$0f,$0f ; $90 pulse width fun
	db $0f,$0f,$d0,$d0,$0d,$0d,$0f,$0f,$0f,$f0,$f2,$2f,$2f,$2f,$0f,$0f ; $91 pulse width fun
	db $0f,$0f,$e0,$e0,$0e,$0e,$0f,$0f,$0f,$f0,$f1,$1f,$1f,$1f,$0f,$0f ; $92 pulse width fun
	db $0f,$0f,$f0,$f0,$0f,$0f,$0f,$0f,$0f,$f0,$f0,$0f,$0f,$0f,$0f,$0f ; $93 pulse width fun
	db $2c,$0f,$f4,$f4,$4f,$4f,$4f,$4f,$4f,$f4,$f4,$4f,$4f,$4f,$4f,$4f ; $94 pulse width fun
	db $48,$0e,$f8,$f8,$8f,$8f,$8f,$8f,$8f,$f8,$f8,$8f,$8f,$8f,$8f,$8d ; $95 pulse width fun
	db $54,$0c,$fc,$fc,$cf,$cf,$cf,$cf,$cf,$fc,$fc,$cf,$cf,$cf,$cf,$ca ; $96 pulse width fun
	db $70,$08,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f8 ; $97 pulse width fun

	db $70,$00,$5a,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f8 ; $98 pulse width fun
	db $70,$00,$00,$48,$cf,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f8 ; $99 pulse width fun
	db $70,$00,$00,$00,$36,$9c,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f8 ; $9a pulse width fun
	db $70,$00,$00,$00,$00,$24,$68,$ac,$ef,$ff,$ff,$ff,$ff,$ff,$ff,$f8 ; $9b pulse width fun
	db $70,$00,$00,$00,$00,$00,$12,$34,$56,$78,$9a,$bc,$de,$ff,$ff,$f8 ; $9c pulse width fun
	db $70,$00,$02,$22,$00,$00,$00,$12,$34,$56,$78,$9a,$bc,$de,$ff,$f8 ; $9d pulse width fun
	db $70,$00,$24,$44,$62,$00,$00,$00,$12,$34,$56,$78,$9a,$bc,$df,$f8 ; $9e pulse width fun
	db $70,$02,$48,$cc,$86,$42,$00,$00,$00,$12,$34,$56,$78,$9a,$bf,$f8 ; $9f pulse width fun
	
	; $a0

	db $70,$04,$6c,$ff,$c8,$64,$21,$00,$00,$00,$12,$34,$56,$78,$9f,$f8 ; $a0 bad speakers
	db $70,$06,$8f,$ff,$fc,$86,$43,$21,$00,$00,$00,$12,$34,$56,$7f,$f8 ; $a1 bad speakers
	db $70,$0a,$ff,$ff,$ff,$c8,$65,$43,$21,$00,$00,$00,$12,$34,$5f,$f8 ; $a2 bad speakers
	db $70,$0f,$ff,$ff,$ff,$fc,$a8,$65,$43,$21,$00,$00,$00,$12,$3f,$f8 ; $a3 bad speakers
	db $70,$0f,$ff,$ff,$ff,$ff,$fc,$87,$65,$43,$21,$00,$00,$00,$3f,$f8 ; $a4 bad speakers
	db $70,$0f,$ff,$ff,$ff,$ff,$ff,$fc,$a8,$65,$43,$21,$00,$00,$0f,$f8 ; $a5 bad speakers
	db $70,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fc,$86,$43,$21,$00,$0f,$f8 ; $a6 bad speakers
	db $70,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fc,$86,$30,$0f,$f8 ; $a7 bad speakers

	db $70,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f0,$0f,$f8 ; $a8 bad speakers
	db $70,$0f,$ff,$ff,$dc,$ff,$ff,$ff,$dc,$ff,$ff,$ff,$dc,$f0,$0f,$f8 ; $a9 bad speakers
	db $70,$0f,$dc,$ff,$98,$ff,$dc,$ff,$98,$ff,$dc,$ff,$98,$f0,$0f,$f8 ; $aa bad speakers
	db $70,$0c,$98,$ff,$54,$dc,$98,$ff,$54,$dc,$98,$ff,$54,$d0,$0f,$f8 ; $ab bad speakers
	db $70,$08,$54,$dc,$10,$98,$54,$dc,$10,$98,$54,$dc,$10,$90,$0f,$f8 ; $ac bad speakers
	db $70,$04,$10,$98,$00,$54,$10,$98,$00,$54,$10,$98,$00,$50,$0f,$f8 ; $ad bad speakers
	db $70,$00,$00,$54,$00,$10,$00,$54,$00,$10,$00,$54,$00,$10,$0f,$f8 ; $ae bad speakers
	db $70,$00,$00,$10,$00,$00,$00,$10,$00,$00,$00,$10,$00,$00,$0f,$f8 ; $af bad speakers

	; $b0

	db $70,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$f8 ; $b0 bad speakers
	db $60,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$ff,$c8 ; $b1 bad speakers
	db $50,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$ff,$ff,$fc,$88 ; $b2 bad speakers
	db $40,$00,$00,$00,$00,$00,$00,$00,$00,$08,$ff,$ff,$ff,$fc,$88,$88 ; $b3 bad speakers
	db $40,$00,$00,$00,$00,$00,$00,$08,$ff,$ff,$ff,$ff,$fc,$88,$88,$88 ; $b4 bad speakers
	db $40,$00,$00,$00,$00,$08,$ff,$ff,$ff,$ff,$ff,$c8,$88,$88,$88,$88 ; $b5 bad speakers
	db $40,$00,$00,$08,$ff,$ff,$ff,$ff,$ff,$fc,$88,$88,$88,$88,$88,$88 ; $b6 bad speakers
	db $40,$08,$ff,$ff,$ff,$ff,$ff,$ff,$c8,$88,$88,$88,$88,$88,$88,$88 ; $b7 bad speakers

	db $40,$08,$ff,$ff,$ff,$ff,$fc,$88,$88,$88,$88,$88,$88,$88,$88,$88 ; $b8 bad speakers
	db $40,$08,$ff,$ff,$ff,$c8,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88 ; $b9 bad speakers
	db $40,$08,$ff,$fc,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88 ; $ba bad speakers
	db $40,$08,$ff,$c8,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88 ; $bb bad speakers
	db $40,$08,$ff,$c8,$88,$88,$88,$88,$98,$88,$88,$78,$88,$88,$88,$88 ; $bc bad speakers
	db $40,$08,$ff,$c8,$88,$88,$88,$99,$a9,$98,$76,$57,$78,$88,$88,$88 ; $bd bad speakers
	db $40,$08,$ff,$c8,$88,$88,$99,$ab,$cb,$a8,$64,$34,$57,$78,$88,$88 ; $be bad speakers
	db $40,$08,$ff,$c8,$88,$99,$aa,$bc,$ec,$b8,$53,$12,$34,$56,$77,$88 ; $bf bad speakers

	; $c0

	db $40,$08,$ff,$c8,$89,$aa,$bc,$de,$fd,$c8,$42,$01,$23,$45,$67,$78 ; $c0 bad speakers
	db $51,$19,$ee,$b7,$9a,$bb,$cd,$ef,$ec,$b7,$31,$00,$34,$56,$56,$67 ; $c1 bad speakers
	db $62,$2a,$dd,$a6,$ab,$cc,$de,$ff,$db,$a6,$20,$00,$45,$67,$45,$56 ; $c2 bad speakers
	db $73,$3b,$cc,$95,$bc,$dd,$ef,$ff,$ca,$95,$10,$00,$56,$78,$34,$45 ; $c3 bad speakers
	db $84,$4c,$bb,$84,$cd,$ee,$ff,$ff,$b9,$84,$00,$00,$67,$89,$23,$34 ; $c4 bad speakers
	db $95,$5d,$aa,$73,$de,$ff,$ff,$ff,$a8,$73,$00,$00,$78,$9a,$12,$23 ; $c5 bad speakers
	db $a6,$6e,$99,$62,$ef,$ff,$ff,$ff,$97,$62,$00,$00,$89,$ab,$01,$12 ; $c6 bad speakers
	db $b7,$7f,$88,$51,$ff,$ff,$ff,$ff,$86,$51,$00,$00,$9a,$bc,$00,$01 ; $c7 bad speakers
	
	db $c8,$8f,$77,$40,$ff,$ff,$ff,$ff,$75,$40,$00,$00,$ab,$cd,$00,$00 ; $c8 bad speakers
	db $d9,$9f,$66,$30,$ff,$ff,$ff,$ff,$64,$30,$00,$00,$bc,$de,$00,$00 ; $c9 bad speakers
	db $ea,$af,$55,$20,$ff,$ff,$ff,$ff,$53,$20,$00,$00,$cd,$ef,$00,$00 ; $ca bad speakers
	db $fb,$bf,$44,$10,$ff,$ff,$ff,$ff,$42,$10,$00,$00,$de,$ff,$00,$00 ; $cb bad speakers
	db $fc,$cf,$33,$00,$ff,$ff,$ff,$ff,$31,$00,$00,$00,$ef,$ff,$00,$00 ; $cc bad speakers
	db $fd,$df,$22,$00,$ff,$ff,$ff,$ff,$20,$00,$00,$00,$ff,$ff,$00,$00 ; $cd bad speakers
	db $fe,$ef,$11,$00,$ff,$ff,$ff,$ff,$10,$00,$00,$00,$ff,$ff,$00,$00 ; $ce bad speakers
	db $ff,$ff,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$00,$00 ; $cf bad speakers

	; $d0

	db $ff,$ff,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$0f,$ff,$f0,$00,$00 ; $d0 bad speakers
	db $ff,$ff,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$ff,$ff,$00,$00,$00 ; $d1 bad speakers
	db $ff,$ff,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$0f,$ff,$f0,$00,$00,$00 ; $d2 bad speakers
	db $ff,$ff,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$ff,$ff,$00,$00,$00,$00 ; $d3 bad speakers

	db $eb,$cb,$22,$22,$ee,$ee,$ef,$ff,$33,$33,$ff,$ff,$33,$22,$22,$22 ; $d4 bad speakers
	db $d8,$98,$43,$44,$dd,$ee,$ef,$ff,$66,$66,$ff,$ff,$66,$55,$44,$44 ; $d5 bad speakers
	db $b6,$66,$54,$56,$dd,$de,$ee,$ef,$99,$99,$ff,$ee,$99,$77,$66,$55 ; $d6 bad speakers
	db $94,$34,$66,$78,$cc,$de,$ee,$ef,$cc,$cc,$ff,$ee,$bb,$99,$98,$76 ; $d7 bad speakers

	db $82,$02,$88,$9a,$bb,$cd,$de,$ef,$ff,$ff,$ff,$ee,$dd,$cc,$ba,$98 ; $d8 sine width fun
	db $83,$00,$38,$89,$ab,$cc,$de,$ee,$ff,$ff,$ff,$ee,$ed,$cc,$ba,$98 ; $d9 sine width fun
	db $84,$10,$14,$88,$9a,$bc,$cd,$ee,$ff,$ff,$ff,$fe,$ed,$dc,$ba,$98 ; $da sine width fun
	db $84,$20,$02,$48,$88,$9a,$bc,$dd,$ee,$ff,$ff,$fe,$ed,$dc,$ba,$98 ; $db sine width fun
	db $85,$21,$01,$25,$88,$9a,$bc,$dd,$ee,$ff,$ff,$fe,$ed,$dc,$ba,$99 ; $dc sine width fun
	db $85,$31,$00,$13,$58,$9a,$ab,$cd,$ee,$ff,$ff,$ff,$ee,$dc,$bb,$a9 ; $dd sine width fun
	db $85,$31,$00,$01,$35,$89,$ab,$cc,$de,$ef,$ff,$ff,$ee,$dc,$cb,$a9 ; $de sine width fun
	db $84,$42,$10,$01,$23,$58,$9a,$bc,$dd,$ee,$ff,$ff,$ee,$dd,$cb,$a9 ; $df sine width fun

	; $e0

	db $86,$42,$10,$00,$12,$45,$89,$ab,$cd,$ee,$ff,$ff,$fe,$ed,$cb,$a9 ; $e0 sine width fun
	db $86,$43,$11,$00,$01,$24,$68,$9a,$bc,$de,$ef,$ff,$fe,$ed,$cb,$a9 ; $e1 sine width fun
	db $86,$43,$21,$00,$01,$23,$46,$89,$ab,$cd,$ef,$ff,$ff,$ed,$cb,$a9 ; $e2 sine width fun
	db $86,$53,$21,$00,$00,$12,$34,$68,$9a,$bd,$de,$ff,$ff,$ee,$db,$a9 ; $e3 sine width fun

	db $86,$53,$21,$10,$00,$11,$23,$56,$89,$ac,$de,$ef,$ff,$ee,$dc,$a9 ; $e4 sine width fun
	db $96,$53,$21,$10,$00,$11,$23,$56,$9a,$bd,$ef,$ff,$ff,$ff,$ed,$ba ; $e5 sine distortion
	db $a7,$53,$21,$10,$00,$11,$23,$57,$ab,$ce,$ff,$ff,$ff,$ff,$fe,$cb ; $e6 sine distortion
	db $b7,$63,$21,$10,$00,$11,$23,$67,$bc,$df,$ff,$ff,$ff,$ff,$ff,$dc ; $e7 sine distortion

	db $c8,$64,$21,$10,$00,$11,$24,$68,$cd,$ef,$ff,$ff,$ff,$ff,$ff,$ed ; $e8 sine distortion
	db $c8,$64,$21,$10,$00,$11,$24,$68,$cd,$ef,$ff,$ff,$ff,$ff,$ff,$ed ; $e9 sine distortion
	db $c8,$64,$21,$10,$00,$11,$24,$68,$cd,$ef,$ff,$fe,$ee,$ef,$ff,$ed ; $ea sine distortion
	db $c8,$64,$21,$10,$00,$11,$24,$68,$cd,$ef,$fe,$ed,$dd,$de,$ef,$ed ; $eb sine distortion

	;   	0123456789abcdef0123456789abcdef
	;	 +-+ -+--+- +-+  -+- +-++-+ -+- 
	;   0+  -+  0-  +-  -+  -0  +-  +0  0-  +-  0+  -+  +-  +0  -+  -0 
	db $c9,$55,$20,$20,$01,$01,$33,$78,$cd,$ef,$fe,$ed,$dd,$de,$ef,$ed ; $ec sine distortion
	db $c9,$55,$20,$20,$01,$01,$33,$78,$cc,$fe,$ff,$de,$ec,$ee,$df,$dd ; $ed sine distortion
	db $c9,$55,$20,$20,$01,$01,$33,$78,$cb,$fd,$ff,$cf,$fb,$fe,$cf,$cd ; $ee sine distortion
	db $c9,$55,$20,$20,$01,$01,$33,$78,$ca,$fc,$ff,$bf,$fa,$fe,$bf,$bd ; $ef sine distortion

	; $f0

	;   0+  -+  0-  +-  -+  -0  +-  +0  0-  +-  0+  -+  +-  +0  -+  -0 
	db $ca,$46,$20,$30,$02,$01,$42,$88,$cb,$fb,$ff,$af,$f9,$fd,$af,$ad ; $f0 sine distortion
	db $ca,$46,$20,$30,$02,$01,$42,$88,$cc,$fa,$ff,$9f,$f8,$fd,$9f,$9d ; $f1 sine distortion
	db $ca,$46,$20,$30,$02,$01,$42,$88,$cd,$f9,$ff,$8f,$f7,$fd,$8f,$8d ; $f2 sine distortion
	db $ca,$46,$20,$30,$02,$01,$42,$88,$ce,$f8,$ff,$7f,$f6,$fd,$7f,$7d ; $f3 sine distortion
	db $cb,$37,$20,$40,$03,$01,$51,$98,$cf,$f7,$ff,$6f,$f5,$fd,$6f,$6d ; $f4 sine distortion
	db $cb,$37,$20,$40,$03,$01,$51,$98,$cf,$f6,$ff,$5f,$f4,$fd,$5f,$5d ; $f5 sine distortion
	db $cb,$37,$20,$40,$03,$01,$51,$98,$cf,$f5,$ff,$4f,$f3,$fd,$4f,$4d ; $f6 sine distortion
	db $cb,$37,$20,$40,$03,$01,$51,$98,$cf,$f4,$ff,$3f,$f2,$fd,$3f,$3d ; $f7 sine distortion
	db $cc,$28,$20,$50,$04,$01,$60,$a8,$cf,$f3,$ff,$2f,$f1,$fd,$2f,$2d ; $f8 sine distortion
	db $cc,$28,$20,$50,$04,$01,$60,$a8,$cf,$f2,$ff,$1f,$f0,$fd,$1f,$1d ; $f9 sine distortion
	db $cc,$28,$20,$50,$04,$01,$60,$a8,$cf,$f1,$ff,$0f,$f0,$fd,$0f,$0d ; $fa sine distortion
	db $cc,$28,$20,$50,$04,$01,$60,$a8,$cf,$f0,$ff,$0f,$f0,$fd,$0f,$0d ; $fb sine distortion

	db $bb,$37,$20,$40,$03,$01,$51,$97,$bd,$e3,$ef,$4f,$f4,$fd,$3c,$3c ; $fc sine distortion to original sine
	db $a9,$36,$20,$30,$02,$01,$42,$87,$ac,$d6,$ef,$8f,$f8,$fd,$6c,$5b ; $fd sine distortion to original sine
	db $87,$44,$21,$20,$01,$11,$32,$66,$9b,$b9,$de,$cf,$fc,$ee,$ac,$8a ; $fe sine distortion to original sine
	db $76,$53,$21,$10,$00,$11,$23,$56,$89,$ac,$de,$ef,$ff,$ee,$dc,$a9 ; $ff sine distortion to original sine



; redundant buffer for tonefunmixing


	db $76,$53,$21,$10,$00,$11,$23,$56,$89,$ac,$de,$ef,$ff,$ee,$dc,$a9 ; $00 sine
	db $65,$42,$10,$01,$11,$00,$12,$45,$9a,$bd,$ef,$fe,$ee,$ff,$ed,$ba ; $01 sine feedback
	db $54,$31,$01,$12,$22,$11,$01,$34,$ab,$ce,$fe,$ed,$dd,$ee,$fe,$cb ; $02 sine feedback
	db $43,$20,$12,$23,$33,$22,$10,$23,$bc,$df,$ed,$dc,$cc,$dd,$ef,$dc ; $03 sine feedback
	db $32,$11,$23,$34,$44,$33,$21,$12,$cd,$ee,$dc,$cb,$bb,$cc,$de,$ed ; $04 sine feedback
	db $21,$02,$34,$45,$55,$44,$32,$01,$de,$fd,$cb,$ba,$aa,$bb,$cd,$fe ; $05 sine feedback
	db $10,$13,$45,$56,$66,$55,$43,$10,$ef,$ec,$ba,$a9,$99,$aa,$bc,$ef ; $06 sine feedback
	db $01,$24,$56,$67,$77,$66,$54,$21,$fe,$db,$a9,$98,$88,$99,$ab,$de ; $07 sine feedback
	db $12,$35,$67,$78,$88,$77,$65,$32,$ed,$ca,$98,$87,$77,$88,$9a,$cd ; $08 sine feedback
	db $23,$46,$78,$87,$77,$88,$76,$43,$dc,$b9,$87,$78,$88,$77,$89,$bc ; $09 sine feedback
	db $34,$57,$87,$76,$66,$77,$87,$54,$cb,$a8,$78,$89,$99,$88,$78,$ab ; $0a sine feedback
	db $45,$68,$76,$65,$55,$66,$78,$65,$ba,$97,$89,$9a,$aa,$99,$87,$9a ; $0b sine feedback
	db $56,$77,$65,$54,$44,$55,$67,$76,$a9,$88,$9a,$ab,$bb,$aa,$98,$89 ; $0c sine feedback
	db $67,$86,$54,$43,$33,$44,$56,$87,$98,$99,$ab,$bc,$cc,$bb,$a9,$78 ; $0d sine feedback
	db $78,$75,$43,$32,$22,$33,$45,$78,$87,$9a,$bc,$cd,$dd,$cc,$ba,$87 ; $0e sine feedback
	db $87,$64,$32,$21,$11,$22,$34,$67,$98,$9b,$cd,$de,$ee,$dd,$cb,$98 ; $0f sine feedback to sine

	db $76,$64,$32,$21,$11,$22,$34,$67,$98,$9b,$cd,$de,$ee,$dd,$cb,$98 ; $10 sine amplify to square
	db $65,$53,$21,$10,$00,$11,$23,$56,$a9,$ac,$de,$ef,$ff,$ee,$dc,$a9 ; $11 sine amplify to square
	db $54,$42,$10,$00,$00,$00,$12,$45,$ba,$bd,$ef,$ff,$ff,$ff,$ed,$ba ; $12 sine amplify to square
	db $43,$31,$00,$00,$00,$00,$01,$34,$cb,$ce,$ff,$ff,$ff,$ff,$fe,$cb ; $13 sine amplify to square
	db $32,$20,$00,$00,$00,$00,$00,$23,$dc,$df,$ff,$ff,$ff,$ff,$ff,$dc ; $14 sine amplify to square
	db $21,$10,$00,$00,$00,$00,$00,$12,$ed,$ef,$ff,$ff,$ff,$ff,$ff,$ed ; $15 sine amplify to square
	db $10,$00,$00,$00,$00,$00,$00,$01,$fe,$ff,$ff,$ff,$ff,$ff,$ff,$fe ; $16 sine amplify to square
	db $00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $17 sine amplify to square

	db $00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; $18 square
	db $50,$00,$00,$00,$00,$00,$00,$05,$af,$ff,$ff,$ff,$ff,$ff,$ff,$fa ; $19 square to triangle
	db $63,$00,$00,$00,$00,$00,$00,$36,$9c,$ff,$ff,$ff,$ff,$ff,$ff,$c9 ; $1a square to triangle
	db $64,$20,$00,$00,$00,$00,$02,$46,$9b,$df,$ff,$ff,$ff,$ff,$fd,$b9 ; $1b square to triangle
	db $75,$32,$00,$00,$00,$00,$23,$57,$8a,$cd,$ff,$ff,$ff,$ff,$dc,$a8 ; $1c square to triangle
	db $75,$43,$10,$00,$00,$01,$34,$57,$8a,$bc,$ef,$ff,$ff,$fe,$cb,$a8 ; $1d square to triangle
	db $76,$53,$21,$00,$00,$12,$35,$67,$89,$ac,$de,$ff,$ff,$ed,$ca,$98 ; $1e square to triangle
	db $76,$54,$32,$10,$01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98 ; $1f square to triangle

	end


mus_default_waveform_2:

	db $75,$21,$00,$01,$34,$67,$89,$88,$76,$65,$67,$8a,$bd,$ef,$ed,$c9 ; sine +-12
	db $85,$31,$00,$01,$35,$68,$99,$98,$76,$55,$56,$89,$bd,$ef,$ed,$b9 ; sine +-11
	db $96,$31,$00,$01,$35,$78,$99,$98,$76,$55,$56,$79,$bd,$ef,$ed,$b8 ; sine +-10
	db $a7,$42,$00,$01,$35,$78,$99,$98,$65,$44,$45,$79,$bd,$ef,$ed,$b8 ; sine +-9
	db $a7,$42,$00,$01,$34,$67,$88,$87,$65,$44,$45,$68,$ac,$ef,$ed,$b9 ; sine +-8
	db $a7,$42,$00,$01,$24,$57,$77,$76,$54,$43,$45,$68,$ac,$ee,$fe,$ca ; sine +-7
	db $96,$31,$00,$00,$23,$46,$66,$65,$54,$33,$45,$68,$ac,$de,$fe,$db ; sine +-6
	db $85,$31,$00,$00,$13,$45,$56,$55,$44,$44,$45,$78,$ac,$de,$fe,$dc ; sine +-5
	db $75,$31,$00,$00,$12,$34,$55,$55,$44,$44,$56,$79,$bc,$de,$fe,$dc ; sine +-4
	db $75,$31,$00,$00,$12,$34,$55,$55,$54,$45,$56,$89,$bd,$ee,$fe,$dc ; sine +-3
	db $75,$31,$00,$00,$12,$34,$55,$55,$55,$56,$67,$9a,$cd,$ef,$ee,$db ; sine +-2
	db $75,$31,$00,$00,$12,$34,$56,$66,$66,$66,$78,$9b,$cd,$ef,$ed,$ca ; sine +-1
	db $75,$31,$00,$00,$12,$45,$66,$77,$77,$78,$89,$ac,$de,$ef,$ed,$b9 ; sine +0
	db $75,$31,$00,$00,$13,$46,$77,$88,$88,$89,$9a,$bc,$de,$fe,$ec,$a8 ; sine +1
	db $86,$31,$00,$00,$23,$57,$89,$9a,$aa,$aa,$ab,$cd,$ee,$fe,$dc,$a8 ; sine +2
	db $96,$41,$00,$01,$24,$68,$9b,$bc,$cb,$bb,$cc,$dd,$ee,$fe,$dc,$a8 ; sine +3
	db $a7,$42,$00,$01,$35,$79,$bd,$de,$dd,$dc,$cc,$dd,$ee,$fe,$dc,$a9 ; sine +4
	db $a7,$42,$00,$01,$36,$8a,$ce,$ef,$ee,$dc,$cc,$cc,$dd,$ee,$dc,$ba ; sine +5
	db $a7,$41,$00,$01,$46,$8b,$de,$fe,$ed,$cb,$aa,$ab,$bc,$dd,$dd,$cb ; sine +6
	db $a6,$41,$00,$02,$46,$9b,$de,$fe,$dc,$ba,$99,$99,$ab,$cd,$ee,$dd ; sine +7
	db $96,$31,$00,$02,$46,$9b,$de,$ed,$cb,$98,$77,$78,$9b,$cd,$ef,$ed ; sine +8
	db $85,$31,$00,$02,$46,$8a,$cc,$cc,$a9,$76,$65,$67,$8a,$cd,$ef,$ed ; sine +9
	db $85,$21,$00,$02,$46,$8a,$bc,$ba,$98,$65,$44,$57,$8a,$ce,$ef,$ec ; sine +10
	db $74,$20,$00,$02,$46,$8a,$bb,$a9,$86,$54,$44,$57,$9b,$de,$fe,$db ; sine +11
	db $74,$20,$00,$12,$46,$8a,$bb,$a9,$75,$43,$34,$68,$ac,$de,$fe,$ca ; sine +12
	db $74,$20,$00,$12,$57,$9a,$aa,$98,$65,$43,$45,$69,$bd,$ef,$ed,$b8 ; sine +13
	db $74,$20,$00,$13,$57,$9a,$ba,$97,$64,$33,$46,$8a,$ce,$ef,$ec,$a7 ; sine +14
	db $74,$20,$00,$13,$58,$9a,$aa,$87,$54,$34,$57,$9b,$de,$fe,$db,$96 ; sine +15
	db $85,$20,$00,$24,$68,$ab,$ba,$87,$54,$45,$68,$ac,$ef,$ee,$ca,$86 ; sine +16
	db $85,$20,$00,$24,$79,$bb,$ba,$86,$54,$56,$79,$bd,$ef,$ed,$ba,$87 ; sine +17
	db $95,$20,$00,$25,$8a,$bc,$ba,$86,$55,$67,$9b,$de,$fe,$ec,$ba,$99 ; sine +18
	db $95,$20,$01,$36,$9b,$cc,$ba,$86,$66,$79,$bc,$ee,$fe,$dd,$cc,$cc ; sine +19
	db $a6,$20,$01,$36,$9c,$dc,$b9,$87,$67,$9b,$ce,$ef,$ee,$dd,$de,$ee ; sine +20
	db $95,$20,$01,$36,$9a,$ba,$98,$66,$67,$9a,$bc,$cc,$bb,$bc,$de,$fe ; sine +21
	db $84,$10,$01,$36,$89,$99,$86,$66,$67,$9a,$aa,$a9,$99,$ac,$de,$fd ; sine +22
	db $74,$10,$01,$35,$78,$88,$66,$56,$78,$99,$99,$87,$89,$ac,$ef,$ec ; sine +23
	db $74,$10,$01,$35,$78,$87,$65,$56,$78,$99,$87,$66,$79,$bd,$ef,$da ; sine +24
	db $73,$10,$01,$35,$77,$76,$55,$56,$78,$98,$76,$56,$79,$ce,$fe,$c9 ; sine +25
	db $73,$10,$01,$35,$77,$65,$55,$67,$89,$98,$65,$55,$7a,$de,$fd,$a7 ; sine +26
	db $73,$10,$02,$45,$66,$65,$55,$67,$99,$87,$54,$46,$8b,$df,$ec,$96 ; sine +27
	db $73,$10,$02,$45,$66,$55,$55,$78,$99,$86,$44,$47,$ac,$ef,$db,$85 ; sine +28
	db $73,$00,$02,$46,$66,$54,$56,$89,$a9,$75,$43,$58,$bd,$fe,$c9,$76 ; sine +29
	db $73,$00,$02,$46,$65,$44,$57,$8a,$a9,$64,$34,$69,$ce,$fd,$b9,$77 ; sine +30
	db $73,$00,$03,$56,$65,$44,$57,$9b,$a8,$64,$34,$7a,$df,$ec,$a9,$88 ; sine +31
	db $73,$00,$13,$56,$65,$44,$68,$ab,$a8,$53,$35,$9c,$ef,$db,$a9,$aa ; sine +32
	db $83,$00,$13,$56,$54,$45,$79,$cc,$a7,$43,$46,$ad,$fe,$cb,$ab,$cc ; sine +33
	db $83,$00,$14,$66,$54,$45,$8b,$dc,$a6,$43,$58,$ce,$fd,$cb,$bd,$ee ; sine +34
	db $73,$00,$13,$55,$43,$35,$8b,$cb,$85,$33,$59,$cd,$cb,$aa,$ce,$fd ; sine +35
	db $73,$00,$13,$55,$43,$35,$8b,$ca,$74,$23,$69,$bb,$a9,$9b,$df,$eb ; sine +36
	db $72,$00,$13,$44,$32,$35,$8a,$b9,$63,$24,$79,$aa,$98,$9b,$ef,$d9 ; sine +37
	db $62,$00,$13,$44,$22,$35,$9b,$a8,$53,$35,$79,$a8,$78,$ac,$fe,$c8 ; sine +38
	db $62,$00,$13,$43,$21,$36,$9a,$a7,$42,$35,$89,$87,$67,$ad,$fd,$a6 ; sine +39
	db $62,$00,$13,$43,$11,$36,$9a,$96,$32,$46,$88,$76,$68,$ce,$fc,$85 ; sine +40
	db $62,$00,$23,$42,$11,$37,$9a,$85,$33,$57,$88,$65,$69,$df,$eb,$75 ; sine +41
	db $62,$00,$23,$32,$11,$47,$aa,$85,$33,$68,$87,$55,$7b,$ef,$d9,$65 ; sine +42
	db $62,$00,$24,$32,$01,$48,$aa,$74,$34,$78,$86,$45,$8c,$fe,$b8,$77 ; sine +43
	db $62,$00,$24,$31,$01,$58,$a9,$64,$35,$78,$75,$45,$9d,$fd,$a8,$78 ; sine +44
	db $62,$00,$34,$31,$01,$59,$b9,$64,$46,$88,$64,$47,$be,$fc,$98,$9a ; sine +45
	db $62,$00,$34,$31,$02,$6a,$b9,$54,$57,$98,$53,$48,$cf,$eb,$99,$bc ; sine +46
	db $72,$01,$34,$30,$02,$7a,$b8,$54,$68,$97,$43,$5a,$ef,$da,$9b,$dc ; sine +47
	db $72,$01,$34,$20,$03,$8b,$b8,$55,$79,$96,$33,$6b,$ee,$ca,$bd,$fc ; sine +48
	db $72,$01,$34,$20,$03,$8b,$a7,$45,$8a,$85,$23,$8c,$ec,$aa,$cf,$eb ; sine +49
	db $72,$01,$34,$20,$04,$8a,$96,$46,$89,$74,$24,$8c,$ca,$9a,$df,$d8 ; sine +50
	db $62,$02,$44,$20,$04,$8a,$85,$46,$99,$63,$25,$9c,$b9,$9b,$ef,$c7 ; sine +51
	db $62,$02,$44,$10,$15,$89,$75,$57,$99,$53,$36,$ab,$a8,$9c,$fe,$a6 ; sine +52
	db $62,$12,$44,$10,$15,$99,$75,$58,$98,$52,$37,$aa,$87,$ad,$fc,$85 ; sine +53
	db $72,$12,$44,$10,$16,$99,$65,$69,$a7,$42,$48,$a9,$78,$be,$fb,$76 ; sine +54
	db $62,$13,$43,$10,$26,$98,$65,$69,$97,$33,$59,$a8,$78,$cf,$da,$77 ; sine +55
	db $62,$13,$43,$10,$26,$98,$55,$79,$96,$33,$69,$97,$69,$df,$c9,$78 ; sine +56
	db $72,$13,$43,$00,$27,$97,$55,$8a,$95,$34,$89,$86,$7b,$ef,$b8,$8a ; sine +57
	db $72,$13,$43,$00,$37,$97,$55,$9a,$84,$35,$89,$76,$7c,$fd,$a8,$9b ; sine +58
	db $72,$13,$43,$00,$37,$86,$56,$9a,$74,$36,$98,$65,$9d,$fc,$99,$bb ; sine +59
	db $72,$13,$53,$00,$48,$96,$57,$ab,$73,$47,$98,$56,$ae,$fb,$9b,$dc ; sine +60
	db $72,$14,$53,$00,$48,$86,$58,$ba,$63,$58,$97,$57,$bf,$da,$ac,$eb ; sine +61
	db $73,$24,$52,$00,$59,$85,$59,$ba,$64,$69,$96,$58,$df,$ca,$be,$ea ; sine +62
	db $73,$24,$52,$01,$59,$75,$69,$b9,$54,$79,$85,$59,$de,$ba,$cf,$d8 ; sine +63
	db $73,$24,$52,$01,$68,$75,$6a,$b8,$54,$79,$74,$5a,$dc,$aa,$df,$c7 ; sine +64
	db $73,$25,$52,$01,$68,$75,$6a,$b8,$45,$89,$64,$6b,$db,$9b,$ef,$a6 ; sine +65
	db $73,$25,$52,$02,$68,$64,$7a,$a7,$46,$88,$54,$7b,$ca,$9c,$fd,$96 ; sine +66
	db $73,$25,$51,$02,$67,$54,$7a,$a6,$46,$97,$44,$8b,$b9,$9d,$fc,$77 ; sine +67
	db $73,$35,$51,$02,$77,$55,$8b,$96,$57,$97,$45,$9c,$a8,$ae,$fb,$78 ; sine +68
	db $73,$35,$41,$03,$77,$55,$8b,$95,$58,$96,$35,$ab,$98,$bf,$d9,$79 ; sine +69
	db $72,$35,$41,$03,$76,$45,$9b,$85,$58,$85,$36,$aa,$88,$cf,$c8,$8a ; sine +70
	db $72,$35,$41,$03,$76,$45,$9b,$75,$69,$84,$47,$a9,$79,$ef,$b8,$ab ; sine +71
	db $72,$35,$40,$04,$76,$46,$ab,$75,$79,$73,$48,$a8,$7a,$fe,$a9,$bc ; sine +72
	db $72,$35,$40,$04,$75,$36,$aa,$65,$89,$63,$59,$a7,$7b,$fc,$9a,$cb ; sine +73
	db $72,$35,$40,$04,$75,$36,$ba,$65,$89,$53,$6a,$96,$8d,$fb,$9b,$da ; sine +74
	db $72,$36,$40,$05,$75,$48,$ba,$66,$a9,$53,$7a,$86,$9e,$fb,$ad,$e9 ; sine +75
	db $72,$36,$40,$05,$74,$48,$b9,$57,$a9,$44,$8a,$76,$af,$da,$be,$d7 ; sine +76
	db $73,$36,$30,$05,$74,$48,$b8,$57,$a8,$44,$99,$66,$be,$c9,$cf,$b6 ; sine +77
	db $73,$36,$30,$16,$64,$49,$b8,$58,$a7,$35,$98,$57,$ce,$aa,$df,$a6 ; sine +78
	db $73,$46,$30,$16,$63,$59,$b7,$69,$a6,$36,$97,$58,$dd,$ab,$fe,$97 ; sine +79
	db $73,$46,$30,$16,$63,$59,$a6,$69,$95,$47,$96,$59,$db,$9b,$fc,$87 ; sine +80
	db $73,$46,$30,$26,$53,$5a,$96,$6a,$95,$48,$95,$5a,$ca,$9d,$fb,$79 ; sine +81
	db $73,$46,$30,$26,$53,$6a,$96,$7a,$94,$59,$85,$6b,$c9,$ae,$fa,$8b ; sine +82
	db $73,$46,$30,$26,$53,$6a,$85,$7a,$84,$59,$74,$7b,$b8,$bf,$d9,$9b ; sine +83
	db $73,$46,$20,$36,$43,$6a,$85,$8a,$74,$69,$64,$8b,$a8,$bf,$c8,$ab ; sine +84
	db $73,$46,$20,$36,$43,$7a,$75,$9a,$64,$79,$54,$9b,$98,$df,$b9,$bb ; sine +85
	db $83,$56,$20,$36,$43,$8b,$76,$ab,$64,$89,$55,$ab,$8a,$ef,$aa,$db ; sine +86
	db $73,$46,$20,$36,$33,$8a,$66,$aa,$55,$88,$45,$aa,$7a,$fd,$9b,$d9 ; sine +87
	db $73,$46,$20,$46,$33,$89,$66,$a9,$55,$97,$46,$b9,$7b,$fc,$9c,$d7 ; sine +88
	db $73,$56,$10,$46,$33,$99,$67,$b9,$46,$96,$47,$b8,$8d,$fb,$ae,$c7 ; sine +89
	db $73,$56,$10,$46,$24,$99,$58,$b8,$47,$95,$48,$a7,$8e,$ea,$bf,$b6 ; sine +90
	db $73,$56,$10,$45,$24,$98,$58,$b7,$48,$94,$49,$a6,$9e,$d9,$cf,$a6 ; sine +91
	db $72,$56,$10,$45,$24,$a8,$59,$b7,$59,$84,$5a,$96,$af,$ba,$ee,$87 ; sine +92
	db $72,$55,$00,$44,$14,$97,$59,$b6,$59,$73,$5a,$76,$be,$aa,$fd,$78 ; sine +93
	db $72,$45,$00,$44,$15,$96,$59,$a5,$59,$63,$69,$66,$cc,$9b,$fb,$79 ; sine +94
	db $72,$45,$00,$43,$15,$96,$5a,$94,$69,$53,$79,$57,$cb,$8c,$fa,$8b ; sine +95
	db $72,$55,$00,$53,$16,$96,$6b,$95,$79,$53,$88,$58,$db,$9e,$f9,$9c ; sine +96
	db $72,$55,$00,$53,$16,$95,$6b,$84,$89,$44,$97,$49,$c9,$af,$d8,$ab ; sine +97
	db $72,$55,$00,$53,$16,$85,$6b,$74,$88,$35,$96,$5a,$c8,$af,$b8,$ba ; sine +98
	db $72,$55,$01,$52,$17,$84,$7b,$75,$98,$36,$95,$5b,$b8,$cf,$a9,$c9 ; sine +99
	db $73,$55,$01,$52,$27,$85,$8b,$76,$a7,$37,$95,$6c,$a8,$ef,$ab,$d8 ; sine +100
	db $73,$65,$01,$52,$28,$85,$9b,$66,$a7,$38,$84,$7c,$99,$fd,$9c,$d7 ; sine +101
	db $73,$55,$02,$52,$28,$74,$9a,$67,$a6,$48,$74,$8b,$8a,$fc,$9d,$c6 ; sine +102
	db $73,$65,$02,$52,$28,$75,$9a,$57,$a5,$49,$64,$9b,$7b,$fb,$ae,$a6 ; sine +103
	db $73,$65,$02,$51,$38,$65,$aa,$58,$a5,$59,$65,$aa,$7c,$ea,$cf,$97 ; sine +104
	db $83,$65,$03,$51,$39,$65,$b9,$69,$a4,$69,$55,$b9,$8e,$ea,$df,$88 ; sine +105
	db $83,$65,$03,$51,$49,$66,$b9,$6a,$94,$79,$46,$b8,$8e,$da,$fe,$8a ; sine +106
	db $73,$64,$03,$51,$48,$56,$b8,$6a,$84,$88,$47,$a7,$9e,$ba,$fc,$8b ; sine +107
	db $73,$64,$03,$41,$48,$46,$b7,$6a,$74,$87,$38,$a6,$ad,$ab,$fa,$8b ; sine +108
	db $73,$64,$03,$41,$58,$47,$b7,$7a,$64,$96,$49,$96,$bd,$9c,$f9,$9c ; sine +109
	db $83,$74,$04,$41,$58,$48,$b6,$8b,$65,$a6,$4a,$87,$dd,$9e,$f9,$bc ; sine +110
	db $73,$64,$04,$40,$57,$48,$a6,$8a,$56,$95,$5a,$77,$db,$9f,$d9,$ca ; sine +111
	db $73,$63,$04,$30,$67,$48,$a5,$8a,$56,$94,$59,$67,$c9,$af,$b9,$c8 ; sine +112
	db $73,$63,$04,$30,$66,$49,$95,$99,$47,$83,$69,$58,$c9,$bf,$aa,$d7 ; sine +113
	db $73,$63,$04,$30,$66,$49,$95,$a9,$48,$83,$79,$5a,$c8,$cf,$9c,$d6 ; sine +114
	db $83,$73,$05,$31,$76,$4a,$96,$b9,$59,$83,$98,$5b,$c9,$fe,$ae,$c6 ; sine +115
	db $73,$63,$04,$21,$75,$4a,$86,$b8,$59,$63,$97,$5b,$a9,$fc,$ae,$a6 ; sine +116
	db $73,$62,$04,$21,$75,$4a,$76,$b7,$59,$54,$96,$6b,$99,$fb,$ae,$97 ; sine +117
	db $73,$62,$04,$11,$74,$4a,$76,$b6,$59,$45,$95,$7c,$8a,$fa,$ce,$88 ; sine +118
	db $73,$72,$05,$11,$74,$5b,$67,$b6,$6a,$46,$94,$8c,$7c,$fa,$de,$7a ; sine +119
	db $73,$62,$05,$11,$73,$5a,$68,$b5,$79,$36,$84,$9b,$7d,$d9,$fc,$8b ; sine +120
	db $72,$61,$04,$01,$73,$5a,$58,$a4,$78,$37,$73,$99,$7d,$b9,$fa,$8b ; sine +121
	db $62,$61,$04,$01,$62,$69,$58,$94,$87,$38,$64,$a8,$7d,$aa,$f9,$9b ; sine +122
	db $72,$61,$04,$02,$62,$69,$49,$94,$96,$38,$54,$a7,$9e,$9c,$f8,$ba ; sine +123
	db $73,$71,$05,$02,$72,$7a,$5a,$95,$a6,$49,$56,$b7,$be,$9e,$f9,$da ; sine +124
	db $73,$71,$04,$03,$62,$79,$5a,$85,$a5,$49,$46,$a6,$bd,$9f,$c9,$d8 ; sine +125
	db $73,$61,$14,$03,$62,$88,$5a,$75,$a4,$58,$37,$95,$cb,$9f,$aa,$d6 ; sine +126
	db $73,$61,$14,$03,$62,$87,$5a,$66,$94,$68,$38,$86,$ca,$af,$9b,$c6 ; sine +127
	db $73,$71,$14,$04,$52,$97,$6b,$67,$94,$77,$39,$77,$d9,$bf,$9d,$b6 ; sine +128
	db $73,$71,$25,$05,$63,$a7,$7c,$68,$a4,$87,$4a,$78,$d9,$de,$af,$b7 ; sine +129
	db $73,$71,$24,$05,$53,$96,$7b,$69,$94,$96,$5a,$69,$c8,$ed,$af,$98 ; sine +130
	db $73,$71,$24,$05,$53,$96,$7b,$59,$84,$95,$59,$5a,$b8,$fb,$be,$8a ; sine +131
	db $73,$71,$24,$05,$43,$95,$8a,$5a,$74,$94,$69,$5b,$a9,$fa,$cd,$7b ; sine +132
	db $73,$71,$24,$05,$44,$95,$9a,$5a,$65,$93,$78,$5c,$9a,$f9,$dc,$8c ; sine +133
	db $73,$71,$34,$06,$34,$95,$99,$5b,$66,$93,$87,$6c,$8b,$e9,$fb,$9c ; sine +134
	db $73,$70,$33,$06,$34,$94,$98,$6a,$56,$83,$86,$6b,$7c,$c9,$f9,$ab ; sine +135
	db $73,$70,$33,$06,$35,$84,$a8,$6a,$47,$73,$95,$7b,$7d,$ba,$f9,$b9 ; sine +136
	db $73,$70,$33,$06,$25,$84,$a7,$7a,$48,$64,$94,$8a,$7e,$ac,$f9,$d8 ; sine +137
	db $73,$70,$33,$06,$26,$84,$b7,$8a,$49,$65,$94,$a9,$8f,$ae,$e9,$e7 ; sine +138
	db $73,$70,$32,$06,$16,$84,$b6,$89,$49,$45,$84,$a8,$9e,$9f,$ca,$d6 ; sine +139
	db $73,$60,$32,$05,$16,$75,$a5,$88,$49,$46,$74,$a7,$ac,$9f,$ab,$c5 ; sine +140
	db $73,$60,$32,$05,$16,$65,$a5,$97,$49,$37,$64,$a6,$bb,$9f,$9c,$b6 ; sine +141
	db $73,$60,$31,$05,$17,$65,$a5,$a7,$59,$38,$55,$a6,$ca,$af,$9d,$97 ; sine +142
	db $73,$70,$41,$16,$17,$66,$a5,$b7,$69,$39,$56,$a6,$da,$ce,$9f,$89 ; sine +143
	db $74,$60,$41,$15,$18,$57,$a5,$b6,$78,$39,$47,$96,$d9,$dd,$af,$8a ; sine +144
	db $74,$70,$41,$15,$18,$57,$a5,$b5,$88,$49,$38,$87,$d8,$fc,$be,$7c ; sine +145
	db $74,$60,$41,$15,$18,$48,$96,$b5,$86,$48,$39,$78,$c8,$fa,$cc,$8c ; sine +146
	db $74,$60,$40,$25,$18,$48,$86,$b4,$96,$58,$39,$69,$b8,$f9,$db,$9b ; sine +147
	db $74,$60,$40,$24,$18,$49,$86,$a4,$95,$67,$3a,$5a,$a9,$e9,$fa,$aa ; sine +148
	db $74,$60,$40,$24,$18,$39,$77,$a4,$94,$76,$49,$4b,$8a,$d9,$f9,$b8 ; sine +149
	db $74,$60,$40,$34,$28,$39,$78,$95,$a4,$75,$59,$5b,$8c,$ca,$f8,$c7 ; sine +150
	db $74,$60,$50,$33,$28,$3a,$69,$95,$a4,$95,$68,$5c,$7d,$cc,$f9,$e6 ; sine +151
	db $74,$60,$50,$33,$38,$4b,$6a,$96,$a4,$94,$78,$6c,$7f,$be,$ea,$e6 ; sine +152
	db $74,$60,$50,$43,$38,$4b,$6a,$87,$94,$94,$87,$7b,$8f,$ae,$cb,$d6 ; sine +153
	db $74,$60,$50,$43,$37,$4a,$5a,$77,$84,$93,$85,$8a,$8e,$9f,$ac,$b7 ; sine +154
	db $74,$60,$50,$42,$47,$4a,$5a,$68,$85,$83,$95,$98,$9d,$9f,$9d,$98 ; sine +155
	db $74,$60,$50,$42,$46,$4a,$5b,$69,$75,$83,$94,$a8,$ac,$af,$9e,$8a ; sine +156
	db $75,$60,$50,$52,$56,$5a,$5b,$6a,$76,$84,$94,$b7,$bb,$be,$af,$8b ; sine +157
	db $85,$61,$50,$52,$56,$6a,$6c,$6a,$68,$75,$94,$b7,$da,$de,$bf,$8c ; sine +158
	db $85,$71,$60,$62,$66,$6a,$6c,$6b,$69,$76,$95,$c7,$ea,$fd,$de,$9d ; sine +159
	db $75,$61,$50,$51,$65,$69,$6b,$6b,$59,$57,$76,$a6,$d9,$fb,$dc,$ab ; sine +160
	db $75,$61,$50,$51,$65,$78,$6a,$5b,$59,$57,$66,$97,$d8,$fa,$ea,$b9 ; sine +161
	db $75,$61,$50,$51,$64,$78,$7a,$6a,$49,$48,$57,$88,$c9,$e9,$f9,$c8 ; sine +162
	db $75,$51,$50,$51,$64,$77,$79,$6a,$49,$48,$48,$78,$b9,$e9,$f8,$c7 ; sine +163
	db $75,$61,$50,$51,$74,$87,$89,$7a,$59,$39,$49,$6a,$aa,$da,$f9,$d6 ; sine +164
	db $85,$61,$50,$61,$73,$97,$99,$8a,$6a,$4a,$4a,$6b,$ac,$dc,$fa,$e6 ; sine +165
	db $86,$61,$50,$60,$83,$a7,$a9,$9a,$7a,$4a,$4b,$6d,$9f,$de,$ec,$e8 ; sine +166
	db $85,$51,$50,$50,$83,$96,$a8,$98,$78,$59,$4a,$5c,$8e,$af,$cc,$b8 ; sine +167
	db $75,$51,$40,$50,$72,$95,$a6,$97,$77,$58,$49,$5c,$7e,$9f,$ad,$99 ; sine +168
	db $75,$51,$40,$50,$72,$94,$a6,$96,$76,$66,$58,$5b,$7d,$9f,$9d,$8a ; sine +169
	db $75,$41,$40,$40,$72,$94,$a5,$a5,$85,$66,$57,$6a,$8d,$9f,$9e,$7b ; sine +170
	db $75,$41,$30,$40,$72,$94,$b5,$b5,$95,$75,$76,$79,$9d,$af,$ae,$7c ; sine +171
	db $75,$51,$30,$40,$72,$a4,$c5,$c5,$a4,$94,$86,$99,$bc,$cf,$be,$9c ; sine +172
	db $85,$51,$30,$40,$73,$a5,$d6,$d5,$b4,$a4,$a5,$b8,$dc,$fe,$ee,$bb ; sine +173
	db $75,$41,$30,$30,$62,$94,$b5,$c4,$a3,$93,$94,$b6,$da,$fb,$eb,$b9 ; sine +174
	db $64,$31,$20,$30,$52,$84,$a5,$a4,$a3,$92,$93,$a5,$d8,$fa,$e9,$c6 ; sine +175
	db $64,$31,$10,$20,$42,$74,$95,$a4,$93,$82,$83,$a5,$c7,$e9,$f8,$c5 ; sine +176
	db $64,$31,$10,$20,$43,$75,$95,$94,$83,$72,$83,$95,$c7,$e8,$f7,$d5 ; sine +177
	db $64,$31,$10,$11,$33,$65,$86,$95,$84,$73,$74,$96,$c8,$e9,$f8,$d5 ; sine +178
	db $64,$21,$00,$11,$34,$66,$87,$86,$75,$64,$75,$97,$ca,$eb,$fa,$c7 ; sine +179
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-12
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-11
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-10
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-9
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ee ; sine +-8
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-7
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-6
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-5
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-4
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-3
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-2
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +-1
	db $00,$01,$12,$23,$34,$45,$56,$67,$78,$89,$9a,$ab,$bc,$cd,$de,$ef ; sine +0
	db $00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ef,$78 ; sine +1
	db $00,$11,$22,$33,$45,$56,$67,$78,$89,$aa,$bb,$cc,$dd,$ef,$77,$89 ; sine +2
	db $00,$11,$23,$34,$45,$66,$77,$89,$9a,$ab,$cc,$dd,$ef,$77,$88,$9a ; sine +3
	db $00,$11,$23,$34,$45,$66,$78,$89,$9a,$bb,$cd,$de,$f7,$78,$89,$aa ; sine +4
	db $00,$12,$23,$44,$56,$67,$88,$9a,$ab,$cc,$de,$f6,$78,$89,$aa,$bc ; sine +5
	db $00,$12,$23,$45,$56,$77,$89,$aa,$bc,$cd,$ef,$67,$88,$9a,$ab,$cd ; sine +6
	db $00,$12,$23,$45,$56,$78,$99,$ab,$bc,$de,$f6,$77,$89,$aa,$bc,$dd ; sine +7
	db $00,$12,$33,$45,$67,$78,$9a,$bb,$cd,$ef,$67,$78,$9a,$bb,$cd,$ef ; sine +8
	db $00,$12,$33,$45,$66,$78,$9a,$ab,$cd,$d5,$67,$88,$9a,$bb,$cd,$ef ; sine +9
	db $00,$12,$33,$45,$66,$78,$99,$ab,$cc,$55,$67,$88,$9a,$bb,$cd,$ef ; sine +10
	db $00,$12,$23,$45,$56,$78,$89,$ab,$b4,$56,$67,$89,$9a,$bc,$cd,$ef ; sine +11
	db $00,$12,$23,$45,$56,$78,$89,$ab,$b4,$56,$67,$89,$9a,$bc,$cd,$ef ; sine +12
	db $00,$12,$33,$45,$66,$78,$99,$ab,$45,$56,$78,$89,$ab,$bc,$de,$f7 ; sine +13
	db $00,$12,$33,$45,$67,$78,$9a,$b3,$45,$67,$78,$9a,$bb,$cd,$ef,$78 ; sine +14
	db $00,$12,$34,$45,$67,$89,$9a,$b4,$55,$67,$89,$aa,$bc,$de,$f7,$89 ; sine +15
	db $00,$12,$34,$56,$67,$89,$ab,$34,$56,$78,$89,$ab,$cd,$ef,$78,$9a ; sine +16
	db $00,$12,$34,$56,$78,$99,$ab,$45,$56,$78,$9a,$bc,$de,$f7,$89,$ab ; sine +17
	db $00,$12,$34,$56,$78,$9a,$b3,$45,$67,$89,$ab,$cd,$ef,$78,$9a,$bc ; sine +18
	db $00,$12,$34,$56,$78,$9a,$b4,$56,$78,$9a,$bc,$de,$f7,$89,$ab,$cd ; sine +19
	db $00,$12,$34,$56,$78,$9a,$b4,$56,$78,$9a,$bc,$de,$f7,$89,$ab,$cd ; sine +20
	db $01,$23,$45,$67,$89,$ab,$34,$56,$78,$9a,$bc,$df,$78,$9a,$bc,$de ; sine +21
	db $01,$23,$45,$67,$89,$ab,$34,$56,$89,$ab,$cd,$e6,$78,$9a,$bc,$df ; sine +22
	db $01,$23,$45,$67,$89,$a2,$34,$56,$89,$ab,$cd,$56,$78,$9a,$bc,$df ; sine +23
	db $01,$23,$45,$67,$89,$a3,$45,$67,$89,$ab,$cd,$67,$89,$ab,$cd,$ef ; sine +24
	db $00,$12,$34,$56,$78,$93,$45,$67,$78,$9a,$b5,$67,$89,$ab,$cd,$ef ; sine +25
	db $01,$23,$45,$67,$89,$a3,$45,$67,$89,$ab,$c5,$67,$89,$ab,$cd,$f8 ; sine +26
	db $01,$23,$45,$67,$89,$23,$45,$68,$9a,$bc,$56,$78,$9a,$bc,$df,$79 ; sine +27
	db $01,$23,$45,$67,$9a,$24,$56,$78,$9a,$cd,$57,$89,$ab,$cd,$f7,$8a ; sine +28
	db $01,$23,$45,$67,$89,$34,$56,$78,$9a,$b5,$67,$89,$ab,$cd,$f8,$9a ; sine +29
	db $01,$23,$45,$78,$9a,$34,$56,$89,$ab,$c5,$67,$9a,$bc,$df,$79,$ab ; sine +30
	db $01,$23,$46,$78,$92,$35,$67,$89,$bc,$56,$78,$ab,$cd,$f7,$8a,$bc ; sine +31
	db $01,$23,$46,$78,$92,$35,$67,$89,$bc,$56,$78,$ab,$cd,$f7,$9a,$bc ; sine +32
	db $01,$23,$56,$79,$a2,$45,$68,$9a,$cd,$57,$89,$bc,$df,$78,$ab,$ce ; sine +33
	db $01,$23,$56,$78,$a3,$45,$68,$9a,$b4,$67,$89,$bc,$df,$79,$ab,$ce ; sine +34
	db $01,$23,$56,$79,$a3,$45,$78,$9b,$c5,$67,$9a,$bd,$e7,$89,$bc,$df ; sine +35
	db $01,$23,$56,$78,$a3,$45,$78,$9a,$c5,$67,$9a,$bc,$e7,$89,$bc,$df ; sine +36
	db $01,$23,$46,$78,$23,$45,$78,$9a,$45,$67,$9a,$bc,$67,$8a,$bc,$df ; sine +37
	db $01,$23,$56,$79,$23,$56,$78,$ab,$46,$78,$ab,$cd,$78,$9b,$cd,$f8 ; sine +38
	db $01,$23,$56,$78,$23,$56,$78,$ab,$46,$78,$ab,$c6,$78,$9b,$cd,$f8 ; sine +39
	db $01,$24,$56,$89,$24,$56,$89,$a4,$56,$89,$ac,$d6,$89,$ac,$df,$89 ; sine +40
	db $01,$24,$57,$8a,$34,$57,$8a,$b4,$67,$9a,$bd,$67,$9a,$cd,$f7,$9a ; sine +41
	db $01,$24,$57,$89,$34,$57,$8a,$b4,$67,$9a,$bd,$67,$9a,$cd,$f8,$9a ; sine +42
	db $01,$34,$67,$91,$35,$68,$9b,$35,$68,$ab,$d5,$78,$ab,$df,$79,$ac ; sine +43
	db $01,$24,$57,$82,$34,$67,$9a,$45,$78,$9b,$c6,$79,$ac,$df,$89,$bc ; sine +44
	db $01,$34,$68,$92,$35,$78,$ab,$46,$79,$bc,$e6,$8a,$bd,$f7,$9a,$ce ; sine +45
	db $01,$34,$67,$92,$35,$78,$ab,$46,$79,$bc,$57,$8a,$bd,$f7,$9b,$ce ; sine +46
	db $01,$34,$67,$92,$35,$78,$ab,$46,$79,$bc,$57,$8a,$bd,$f8,$9b,$ce ; sine +47
	db $01,$34,$67,$92,$45,$78,$a3,$56,$89,$bc,$67,$9a,$cd,$78,$ab,$df ; sine +48
	db $01,$34,$67,$92,$45,$78,$a3,$56,$89,$b4,$67,$9a,$cd,$78,$ab,$df ; sine +49
	db $01,$34,$68,$92,$46,$79,$b4,$57,$9a,$c5,$78,$ac,$d6,$8a,$bd,$f8 ; sine +50
	db $01,$34,$67,$92,$46,$79,$a4,$57,$9a,$c5,$78,$ac,$d7,$8a,$bd,$f8 ; sine +51

mus_multiply_table:
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
	db $00,$02,$04,$06,$08,$0a,$0c,$0e,$10,$12,$14,$16,$18,$1a,$1c,$1e
	db $00,$03,$06,$09,$0c,$0f,$12,$15,$18,$1b,$1e,$21,$24,$27,$2a,$2d
	db $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c
	db $00,$05,$0a,$0f,$14,$19,$1e,$23,$28,$2d,$32,$37,$3c,$41,$46,$4b
	db $00,$06,$0c,$12,$18,$1e,$24,$2a,$30,$36,$3c,$42,$48,$4e,$54,$5a
	db $00,$07,$0e,$15,$1c,$23,$2a,$31,$38,$3f,$46,$4d,$54,$5b,$62,$69
	db $00,$08,$10,$18,$20,$28,$30,$38,$40,$48,$50,$58,$60,$68,$70,$78
	db $00,$09,$12,$1b,$24,$2d,$36,$3f,$48,$51,$5a,$63,$6c,$75,$7e,$87
	db $00,$0a,$14,$1e,$28,$32,$3c,$46,$50,$5a,$64,$6e,$78,$82,$8c,$96
	db $00,$0b,$16,$21,$2c,$37,$42,$4d,$58,$63,$6e,$79,$84,$8f,$9a,$a5
	db $00,$0c,$18,$24,$30,$3c,$48,$54,$60,$6c,$78,$84,$90,$9c,$a8,$b4
	db $00,$0d,$1a,$27,$34,$41,$4e,$5b,$68,$75,$82,$8f,$9c,$a9,$b6,$c3
	db $00,$0e,$1c,$2a,$38,$46,$54,$62,$70,$7e,$8c,$9a,$a8,$b6,$c4,$d2
	db $00,$0f,$1e,$2d,$3c,$4b,$5a,$69,$78,$87,$96,$a5,$b4,$c3,$c4,$e1



	end

; old vibrato tables

mus_vibrato_sinus_0: ; depth 0.25 semi-tones 
	dw $0000,$0004,$0006,$000a,$000c,$0010,$0012,$0014 ; 0-7
	dw $0016,$0018,$001a,$001c,$001e,$001e,$0020,$0020 ; 8-15
	dw $0020,$0020,$0020,$001e,$001e,$001c,$001a,$0018 ; 16-23
	dw $0016,$0014,$0012,$0010,$000c,$000a,$0006,$0004 ; 24-31
	dw $0000,$fffe,$fffc,$fff8,$fff6,$fff2,$fff0,$ffee ; 32-39
	dw $ffec,$ffea,$ffe8,$ffe6,$ffe4,$ffe4,$ffe2,$ffe2 ; 40-47
	dw $ffe2,$ffe2,$ffe2,$ffe4,$ffe4,$ffe6,$ffe8,$ffea ; 48-55
	dw $ffec,$ffee,$fff0,$fff2,$fff6,$fff8,$fffc,$fffe ; 56-63

mus_vibrato_sinus_1: ; depth 0.50 semi-tones 
	dw $0000,$0006,$000c,$0012,$0018,$001e,$0024,$0028 ; 0-7
	dw $002e,$0032,$0036,$0038,$003c,$003e,$003e,$0040 ; 8-15
	dw $0040,$0040,$003e,$003e,$003c,$0038,$0036,$0032 ; 16-23
	dw $002e,$0028,$0024,$001e,$0018,$0012,$000c,$0006 ; 24-31
	dw $0000,$fffc,$fff6,$fff0,$ffea,$ffe4,$ffde,$ffda ; 32-39
	dw $ffd4,$ffd0,$ffcc,$ffca,$ffc6,$ffc4,$ffc4,$ffc2 ; 40-47
	dw $ffc2,$ffc2,$ffc4,$ffc4,$ffc6,$ffca,$ffcc,$ffd0 ; 48-55
	dw $ffd4,$ffda,$ffde,$ffe4,$ffea,$fff0,$fff6,$fffc ; 56-63

mus_vibrato_sinus_2: ; depth 0.75 semi-tones 
	dw $0000,$000a,$0012,$001c,$0024,$002e,$0036,$003c ; 0-7
	dw $0044,$004a,$0050,$0054,$0058,$005c,$005e,$0060 ; 8-15
	dw $0060,$0060,$005e,$005c,$0058,$0054,$0050,$004a ; 16-23
	dw $0044,$003c,$0036,$002e,$0024,$001c,$0012,$000a ; 24-31
	dw $0000,$fff8,$fff0,$ffe6,$ffde,$ffd4,$ffcc,$ffc6 ; 32-39
	dw $ffbe,$ffb8,$ffb2,$ffae,$ffaa,$ffa6,$ffa4,$ffa2 ; 40-47
	dw $ffa2,$ffa2,$ffa4,$ffa6,$ffaa,$ffae,$ffb2,$ffb8 ; 48-55
	dw $ffbe,$ffc6,$ffcc,$ffd4,$ffde,$ffe6,$fff0,$fff8 ; 56-63

mus_vibrato_sinus_3: ; depth 1.00 semi-tones 
	dw $0000,$000c,$0018,$0026,$0030,$003c,$0048,$0052 ; 0-7
	dw $005a,$0062,$006a,$0070,$0076,$007a,$007e,$0080 ; 8-15
	dw $0080,$0080,$007e,$007a,$0076,$0070,$006a,$0062 ; 16-23
	dw $005a,$0052,$0048,$003c,$0030,$0026,$0018,$000c ; 24-31
	dw $0000,$fff6,$ffea,$ffdc,$ffd2,$ffc6,$ffba,$ffb0 ; 32-39
	dw $ffa8,$ffa0,$ff98,$ff92,$ff8c,$ff88,$ff84,$ff82 ; 40-47
	dw $ff82,$ff82,$ff84,$ff88,$ff8c,$ff92,$ff98,$ffa0 ; 48-55
	dw $ffa8,$ffb0,$ffba,$ffc6,$ffd2,$ffdc,$ffea,$fff6 ; 56-63

mus_vibrato_sinus_4: ; depth 1.50 semi-tones 
	dw $0000,$0012,$0026,$0038,$004a,$005a,$006a,$007a ; 0-7
	dw $0088,$0094,$00a0,$00aa,$00b2,$00b8,$00bc,$00c0 ; 8-15
	dw $00c0,$00c0,$00bc,$00b8,$00b2,$00aa,$00a0,$0094 ; 16-23
	dw $0088,$007a,$006a,$005a,$004a,$0038,$0026,$0012 ; 24-31
	dw $0000,$fff0,$ffdc,$ffca,$ffb8,$ffa8,$ff98,$ff88 ; 32-39
	dw $ff7a,$ff6e,$ff62,$ff58,$ff50,$ff4a,$ff46,$ff42 ; 40-47
	dw $ff42,$ff42,$ff46,$ff4a,$ff50,$ff58,$ff62,$ff6e ; 48-55
	dw $ff7a,$ff88,$ff98,$ffa8,$ffb8,$ffca,$ffdc,$fff0 ; 56-63

mus_vibrato_sinus_5: ; depth 2.00 semi-tones 
	dw $0000,$001a,$0032,$004a,$0062,$0078,$008e,$00a2 ; 0-7
	dw $00b6,$00c6,$00d4,$00e2,$00ec,$00f4,$00fc,$00fe ; 8-15
	dw $0100,$00fe,$00fc,$00f4,$00ec,$00e2,$00d4,$00c6 ; 16-23
	dw $00b6,$00a2,$008e,$0078,$0062,$004a,$0032,$001a ; 24-31
	dw $0000,$ffe8,$ffd0,$ffb8,$ffa0,$ff8a,$ff74,$ff60 ; 32-39
	dw $ff4c,$ff3c,$ff2e,$ff20,$ff16,$ff0e,$ff06,$ff04 ; 40-47
	dw $ff02,$ff04,$ff06,$ff0e,$ff16,$ff20,$ff2e,$ff3c ; 48-55
	dw $ff4c,$ff60,$ff74,$ff8a,$ffa0,$ffb8,$ffd0,$ffe8 ; 56-63

mus_vibrato_sinus_6: ; depth 2.50 semi-tones 
	dw $0000,$0020,$003e,$005c,$007a,$0096,$00b2,$00cc ; 0-7
	dw $00e2,$00f8,$010a,$011a,$0128,$0132,$013a,$013e ; 8-15
	dw $0140,$013e,$013a,$0132,$0128,$011a,$010a,$00f8 ; 16-23
	dw $00e2,$00cc,$00b2,$0096,$007a,$005c,$003e,$0020 ; 24-31
	dw $0000,$ffe2,$ffc4,$ffa6,$ff88,$ff6c,$ff50,$ff36 ; 32-39
	dw $ff20,$ff0a,$fef8,$fee8,$feda,$fed0,$fec8,$fec4 ; 40-47
	dw $fec2,$fec4,$fec8,$fed0,$feda,$fee8,$fef8,$ff0a ; 48-55
	dw $ff20,$ff36,$ff50,$ff6c,$ff88,$ffa6,$ffc4,$ffe2 ; 56-63

mus_vibrato_sinus_7: ; depth 3.00 semi-tones 
	dw $0000,$0026,$004a,$0070,$0092,$00b6,$00d6,$00f4 ; 0-7
	dw $0110,$0128,$0140,$0152,$0162,$0170,$0178,$017e ; 8-15
	dw $0180,$017e,$0178,$0170,$0162,$0152,$0140,$0128 ; 16-23
	dw $0110,$00f4,$00d6,$00b6,$0092,$0070,$004a,$0026 ; 24-31
	dw $0000,$ffdc,$ffb8,$ff92,$ff70,$ff4c,$ff2c,$ff0e ; 32-39
	dw $fef2,$feda,$fec2,$feb0,$fea0,$fe92,$fe8a,$fe84 ; 40-47
	dw $fe82,$fe84,$fe8a,$fe92,$fea0,$feb0,$fec2,$feda ; 48-55
	dw $fef2,$ff0e,$ff2c,$ff4c,$ff70,$ff92,$ffb8,$ffdc ; 56-63

mus_vibrato_sinus_8: ; depth 4.00 semi-tones 
	dw $0000,$0032,$0064,$0094,$00c4,$00f2,$011c,$0144 ; 0-7
	dw $016a,$018c,$01aa,$01c4,$01da,$01ea,$01f6,$01fe ; 8-15
	dw $0200,$01fe,$01f6,$01ea,$01da,$01c4,$01aa,$018c ; 16-23
	dw $016a,$0144,$011c,$00f2,$00c4,$0094,$0064,$0032 ; 24-31
	dw $0000,$ffd0,$ff9e,$ff6e,$ff3e,$ff10,$fee6,$febe ; 32-39
	dw $fe98,$fe76,$fe58,$fe3e,$fe28,$fe18,$fe0c,$fe04 ; 40-47
	dw $fe02,$fe04,$fe0c,$fe18,$fe28,$fe3e,$fe58,$fe76 ; 48-55
	dw $fe98,$febe,$fee6,$ff10,$ff3e,$ff6e,$ff9e,$ffd0 ; 56-63

mus_vibrato_sinus_9: ; depth 5.00 semi-tones 
	dw $0000,$003e,$007c,$00ba,$00f4,$012e,$0164,$0196 ; 0-7
	dw $01c4,$01ee,$0214,$0234,$0250,$0264,$0274,$027c ; 8-15
	dw $0280,$027c,$0274,$0264,$0250,$0234,$0214,$01ee ; 16-23
	dw $01c4,$0196,$0164,$012e,$00f4,$00ba,$007c,$003e ; 24-31
	dw $0000,$ffc4,$ff86,$ff48,$ff0e,$fed4,$fe9e,$fe6c ; 32-39
	dw $fe3e,$fe14,$fdee,$fdce,$fdb2,$fd9e,$fd8e,$fd86 ; 40-47
	dw $fd82,$fd86,$fd8e,$fd9e,$fdb2,$fdce,$fdee,$fe14 ; 48-55
	dw $fe3e,$fe6c,$fe9e,$fed4,$ff0e,$ff48,$ff86,$ffc4 ; 56-63

mus_vibrato_sinus_10: ; depth 6.00 semi-tones 
	dw $0000,$004c,$0096,$00de,$0126,$016a,$01aa,$01e8 ; 0-7
	dw $0220,$0252,$027e,$02a6,$02c6,$02de,$02f2,$02fc ; 8-15
	dw $0300,$02fc,$02f2,$02de,$02c6,$02a6,$027e,$0252 ; 16-23
	dw $0220,$01e8,$01aa,$016a,$0126,$00de,$0096,$004c ; 24-31
	dw $0000,$ffb6,$ff6c,$ff24,$fedc,$fe98,$fe58,$fe1a ; 32-39
	dw $fde2,$fdb0,$fd84,$fd5c,$fd3c,$fd24,$fd10,$fd06 ; 40-47
	dw $fd02,$fd06,$fd10,$fd24,$fd3c,$fd5c,$fd84,$fdb0 ; 48-55
	dw $fde2,$fe1a,$fe58,$fe98,$fedc,$ff24,$ff6c,$ffb6 ; 56-63

mus_vibrato_sinus_11: ; depth 7.00 semi-tones 
	dw $0000,$0058,$00ae,$0104,$0156,$01a6,$01f2,$0238 ; 0-7
	dw $027a,$02b4,$02e8,$0316,$033c,$035a,$036e,$037c ; 8-15
	dw $0380,$037c,$036e,$035a,$033c,$0316,$02e8,$02b4 ; 16-23
	dw $027a,$0238,$01f2,$01a6,$0156,$0104,$00ae,$0058 ; 24-31
	dw $0000,$ffaa,$ff54,$fefe,$feac,$fe5c,$fe10,$fdca ; 32-39
	dw $fd88,$fd4e,$fd1a,$fcec,$fcc6,$fca8,$fc94,$fc86 ; 40-47
	dw $fc82,$fc86,$fc94,$fca8,$fcc6,$fcec,$fd1a,$fd4e ; 48-55
	dw $fd88,$fdca,$fe10,$fe5c,$feac,$fefe,$ff54,$ffaa ; 56-63

mus_vibrato_sinus_12: ; depth 8.00 semi-tones 
	dw $0000,$0064,$00c8,$012a,$0188,$01e2,$0238,$028a ; 0-7
	dw $02d4,$0318,$0354,$0388,$03b2,$03d4,$03ec,$03fc ; 8-15
	dw $0400,$03fc,$03ec,$03d4,$03b2,$0388,$0354,$0318 ; 16-23
	dw $02d4,$028a,$0238,$01e2,$0188,$012a,$00c8,$0064 ; 24-31
	dw $0000,$ff9e,$ff3a,$fed8,$fe7a,$fe20,$fdca,$fd78 ; 32-39
	dw $fd2e,$fcea,$fcae,$fc7a,$fc50,$fc2e,$fc16,$fc06 ; 40-47
	dw $fc02,$fc06,$fc16,$fc2e,$fc50,$fc7a,$fcae,$fcea ; 48-55
	dw $fd2e,$fd78,$fdca,$fe20,$fe7a,$fed8,$ff3a,$ff9e ; 56-63

mus_vibrato_sinus_13: ; depth 9.00 semi-tones 
	dw $0000,$0070,$00e0,$014e,$01b8,$0220,$0280,$02da ; 0-7
	dw $032e,$037a,$03be,$03f8,$0428,$044e,$046a,$047a ; 8-15
	dw $0480,$047a,$046a,$044e,$0428,$03f8,$03be,$037a ; 16-23
	dw $032e,$02da,$0280,$0220,$01b8,$014e,$00e0,$0070 ; 24-31
	dw $0000,$ff92,$ff22,$feb4,$fe4a,$fde2,$fd82,$fd28 ; 32-39
	dw $fcd4,$fc88,$fc44,$fc0a,$fbda,$fbb4,$fb98,$fb88 ; 40-47
	dw $fb82,$fb88,$fb98,$fbb4,$fbda,$fc0a,$fc44,$fc88 ; 48-55
	dw $fcd4,$fd28,$fd82,$fde2,$fe4a,$feb4,$ff22,$ff92 ; 56-63

mus_vibrato_sinus_14: ; depth 10.00 semi-tones 
	dw $0000,$007e,$00fa,$0174,$01ea,$025c,$02c8,$032c ; 0-7
	dw $038a,$03de,$0428,$0468,$049e,$04c8,$04e8,$04fa ; 8-15
	dw $0500,$04fa,$04e8,$04c8,$049e,$0468,$0428,$03de ; 16-23
	dw $038a,$032c,$02c8,$025c,$01ea,$0174,$00fa,$007e ; 24-31
	dw $0000,$ff84,$ff08,$fe8e,$fe18,$fda6,$fd3a,$fcd6 ; 32-39
	dw $fc78,$fc24,$fbda,$fb9a,$fb64,$fb3a,$fb1a,$fb08 ; 40-47
	dw $fb02,$fb08,$fb1a,$fb3a,$fb64,$fb9a,$fbda,$fc24 ; 48-55
	dw $fc78,$fcd6,$fd3a,$fda6,$fe18,$fe8e,$ff08,$ff84 ; 56-63

mus_vibrato_sinus_15: ; depth 11.00 semi-tones 
	dw $0000,$008a,$0112,$0198,$021a,$0298,$030e,$037e ; 0-7
	dw $03e4,$0440,$0492,$04da,$0514,$0544,$0564,$057a ; 8-15
	dw $0580,$057a,$0564,$0544,$0514,$04da,$0492,$0440 ; 16-23
	dw $03e4,$037e,$030e,$0298,$021a,$0198,$0112,$008a ; 24-31
	dw $0000,$ff78,$fef0,$fe6a,$fde8,$fd6a,$fcf4,$fc84 ; 32-39
	dw $fc1e,$fbc2,$fb70,$fb28,$faee,$fabe,$fa9e,$fa88 ; 40-47
	dw $fa82,$fa88,$fa9e,$fabe,$faee,$fb28,$fb70,$fbc2 ; 48-55
	dw $fc1e,$fc84,$fcf4,$fd6a,$fde8,$fe6a,$fef0,$ff78 ; 56-63

mus_vibrato_sinus_16: ; depth 12.00 semi-tones 
	dw $0000,$0096,$012c,$01be,$024c,$02d4,$0356,$03ce ; 0-7
	dw $043e,$04a4,$04fe,$054a,$058c,$05be,$05e2,$05f8 ; 8-15
	dw $0600,$05f8,$05e2,$05be,$058c,$054a,$04fe,$04a4 ; 16-23
	dw $043e,$03ce,$0356,$02d4,$024c,$01be,$012c,$0096 ; 24-31
	dw $0000,$ff6c,$fed6,$fe44,$fdb6,$fd2e,$fcac,$fc34 ; 32-39
	dw $fbc4,$fb5e,$fb04,$fab8,$fa76,$fa44,$fa20,$fa0a ; 40-47
	dw $fa02,$fa0a,$fa20,$fa44,$fa76,$fab8,$fb04,$fb5e ; 48-55
	dw $fbc4,$fc34,$fcac,$fd2e,$fdb6,$fe44,$fed6,$ff6c ; 56-63

mus_vibrato_sinus_17: ; depth 13.00 semi-tones 
	dw $0000,$00a4,$0144,$01e4,$027c,$0310,$039c,$0420 ; 0-7
	dw $0498,$0506,$0568,$05bc,$0602,$0638,$0660,$0678 ; 8-15
	dw $0680,$0678,$0660,$0638,$0602,$05bc,$0568,$0506 ; 16-23
	dw $0498,$0420,$039c,$0310,$027c,$01e4,$0144,$00a4 ; 24-31
	dw $0000,$ff5e,$febe,$fe1e,$fd86,$fcf2,$fc66,$fbe2 ; 32-39
	dw $fb6a,$fafc,$fa9a,$fa46,$fa00,$f9ca,$f9a2,$f98a ; 40-47
	dw $f982,$f98a,$f9a2,$f9ca,$fa00,$fa46,$fa9a,$fafc ; 48-55
	dw $fb6a,$fbe2,$fc66,$fcf2,$fd86,$fe1e,$febe,$ff5e ; 56-63

mus_vibrato_sinus_18: ; depth 14.00 semi-tones 
	dw $0000,$00b0,$015e,$0208,$02ae,$034c,$03e4,$0470 ; 0-7
	dw $04f4,$056a,$05d2,$062c,$0678,$06b2,$06de,$06f8 ; 8-15
	dw $0700,$06f8,$06de,$06b2,$0678,$062c,$05d2,$056a ; 16-23
	dw $04f4,$0470,$03e4,$034c,$02ae,$0208,$015e,$00b0 ; 24-31
	dw $0000,$ff52,$fea4,$fdfa,$fd54,$fcb6,$fc1e,$fb92 ; 32-39
	dw $fb0e,$fa98,$fa30,$f9d6,$f98a,$f950,$f924,$f90a ; 40-47
	dw $f902,$f90a,$f924,$f950,$f98a,$f9d6,$fa30,$fa98 ; 48-55
	dw $fb0e,$fb92,$fc1e,$fcb6,$fd54,$fdfa,$fea4,$ff52 ; 56-63

mus_vibrato_sinus_19: ; depth 15.00 semi-tones 
	dw $0000,$00bc,$0176,$022e,$02de,$038a,$042a,$04c2 ; 0-7
	dw $054e,$05cc,$063c,$069e,$06ee,$072e,$075c,$0776 ; 8-15
	dw $0780,$0776,$075c,$072e,$06ee,$069e,$063c,$05cc ; 16-23
	dw $054e,$04c2,$042a,$038a,$02de,$022e,$0176,$00bc ; 24-31
	dw $0000,$ff46,$fe8c,$fdd4,$fd24,$fc78,$fbd8,$fb40 ; 32-39
	dw $fab4,$fa36,$f9c6,$f964,$f914,$f8d4,$f8a6,$f88c ; 40-47
	dw $f882,$f88c,$f8a6,$f8d4,$f914,$f964,$f9c6,$fa36 ; 48-55
	dw $fab4,$fb40,$fbd8,$fc78,$fd24,$fdd4,$fe8c,$ff46 ; 56-63

mus_vibrato_sinus_20: ; depth 16.00 semi-tones 
	dw $0000,$00c8,$0190,$0252,$0310,$03c6,$0472,$0514 ; 0-7
	dw $05a8,$0630,$06a6,$070e,$0764,$07a8,$07d8,$07f6 ; 8-15
	dw $0800,$07f6,$07d8,$07a8,$0764,$070e,$06a6,$0630 ; 16-23
	dw $05a8,$0514,$0472,$03c6,$0310,$0252,$0190,$00c8 ; 24-31
	dw $0000,$ff3a,$fe72,$fdb0,$fcf2,$fc3c,$fb90,$faee ; 32-39
	dw $fa5a,$f9d2,$f95c,$f8f4,$f89e,$f85a,$f82a,$f80c ; 40-47
	dw $f802,$f80c,$f82a,$f85a,$f89e,$f8f4,$f95c,$f9d2 ; 48-55
	dw $fa5a,$faee,$fb90,$fc3c,$fcf2,$fdb0,$fe72,$ff3a ; 56-63

mus_vibrato_sinus_21: ; depth 17.00 semi-tones 
	dw $0000,$00d6,$01a8,$0278,$0340,$0402,$04b8,$0564 ; 0-7
	dw $0602,$0692,$0712,$0780,$07da,$0822,$0856,$0876 ; 8-15
	dw $0880,$0876,$0856,$0822,$07da,$0780,$0712,$0692 ; 16-23
	dw $0602,$0564,$04b8,$0402,$0340,$0278,$01a8,$00d6 ; 24-31
	dw $0000,$ff2c,$fe5a,$fd8a,$fcc2,$fc00,$fb4a,$fa9e ; 32-39
	dw $fa00,$f970,$f8f0,$f882,$f828,$f7e0,$f7ac,$f78c ; 40-47
	dw $f782,$f78c,$f7ac,$f7e0,$f828,$f882,$f8f0,$f970 ; 48-55
	dw $fa00,$fa9e,$fb4a,$fc00,$fcc2,$fd8a,$fe5a,$ff2c ; 56-63

mus_vibrato_sinus_22: ; depth 18.00 semi-tones 
	dw $0000,$00e2,$01c2,$029c,$0372,$043e,$0500,$05b6 ; 0-7
	dw $065e,$06f6,$077c,$07f0,$0850,$089c,$08d4,$08f4 ; 8-15
	dw $0900,$08f4,$08d4,$089c,$0850,$07f0,$077c,$06f6 ; 16-23
	dw $065e,$05b6,$0500,$043e,$0372,$029c,$01c2,$00e2 ; 24-31
	dw $0000,$ff20,$fe40,$fd66,$fc90,$fbc4,$fb02,$fa4c ; 32-39
	dw $f9a4,$f90c,$f886,$f812,$f7b2,$f766,$f72e,$f70e ; 40-47
	dw $f702,$f70e,$f72e,$f766,$f7b2,$f812,$f886,$f90c ; 48-55
	dw $f9a4,$fa4c,$fb02,$fbc4,$fc90,$fd66,$fe40,$ff20 ; 56-63

mus_vibrato_sinus_23: ; depth 19.00 semi-tones 
	dw $0000,$00ee,$01da,$02c2,$03a2,$047a,$0548,$0606 ; 0-7
	dw $06b8,$0758,$07e6,$0860,$08c6,$0918,$0952,$0974 ; 8-15
	dw $0980,$0974,$0952,$0918,$08c6,$0860,$07e6,$0758 ; 16-23
	dw $06b8,$0606,$0548,$047a,$03a2,$02c2,$01da,$00ee ; 24-31
	dw $0000,$ff14,$fe28,$fd40,$fc60,$fb88,$faba,$f9fc ; 32-39
	dw $f94a,$f8aa,$f81c,$f7a2,$f73c,$f6ea,$f6b0,$f68e ; 40-47
	dw $f682,$f68e,$f6b0,$f6ea,$f73c,$f7a2,$f81c,$f8aa ; 48-55
	dw $f94a,$f9fc,$faba,$fb88,$fc60,$fd40,$fe28,$ff14 ; 56-63

mus_vibrato_sinus_24: ; depth 20.00 semi-tones 
	dw $0000,$00fa,$01f4,$02e8,$03d4,$04b6,$058e,$0658 ; 0-7
	dw $0712,$07ba,$0850,$08d2,$093e,$0992,$09ce,$09f4 ; 8-15
	dw $0a00,$09f4,$09ce,$0992,$093e,$08d2,$0850,$07ba ; 16-23
	dw $0712,$0658,$058e,$04b6,$03d4,$02e8,$01f4,$00fa ; 24-31
	dw $0000,$ff08,$fe0e,$fd1a,$fc2e,$fb4c,$fa74,$f9aa ; 32-39
	dw $f8f0,$f848,$f7b2,$f730,$f6c4,$f670,$f634,$f60e ; 40-47
	dw $f602,$f60e,$f634,$f670,$f6c4,$f730,$f7b2,$f848 ; 48-55
	dw $f8f0,$f9aa,$fa74,$fb4c,$fc2e,$fd1a,$fe0e,$ff08 ; 56-63

mus_vibrato_sinus_25: ; depth 21.00 semi-tones 
	dw $0000,$0108,$020c,$030c,$0404,$04f4,$05d6,$06aa ; 0-7
	dw $076c,$081e,$08ba,$0942,$09b4,$0a0c,$0a4c,$0a74 ; 8-15
	dw $0a80,$0a74,$0a4c,$0a0c,$09b4,$0942,$08ba,$081e ; 16-23
	dw $076c,$06aa,$05d6,$04f4,$0404,$030c,$020c,$0108 ; 24-31
	dw $0000,$fefa,$fdf6,$fcf6,$fbfe,$fb0e,$fa2c,$f958 ; 32-39
	dw $f896,$f7e4,$f748,$f6c0,$f64e,$f5f6,$f5b6,$f58e ; 40-47
	dw $f582,$f58e,$f5b6,$f5f6,$f64e,$f6c0,$f748,$f7e4 ; 48-55
	dw $f896,$f958,$fa2c,$fb0e,$fbfe,$fcf6,$fdf6,$fefa ; 56-63

mus_vibrato_sinus_26: ; depth 22.00 semi-tones 
	dw $0000,$0114,$0226,$0332,$0436,$0530,$061c,$06fa ; 0-7
	dw $07c8,$0880,$0926,$09b4,$0a2a,$0a86,$0aca,$0af2 ; 8-15
	dw $0b00,$0af2,$0aca,$0a86,$0a2a,$09b4,$0926,$0880 ; 16-23
	dw $07c8,$06fa,$061c,$0530,$0436,$0332,$0226,$0114 ; 24-31
	dw $0000,$feee,$fddc,$fcd0,$fbcc,$fad2,$f9e6,$f908 ; 32-39
	dw $f83a,$f782,$f6dc,$f64e,$f5d8,$f57c,$f538,$f510 ; 40-47
	dw $f502,$f510,$f538,$f57c,$f5d8,$f64e,$f6dc,$f782 ; 48-55
	dw $f83a,$f908,$f9e6,$fad2,$fbcc,$fcd0,$fddc,$feee ; 56-63

mus_vibrato_sinus_27: ; depth 23.00 semi-tones 
	dw $0000,$0120,$023e,$0356,$0466,$056c,$0664,$074c ; 0-7
	dw $0822,$08e4,$0990,$0a24,$0aa0,$0b02,$0b48,$0b72 ; 8-15
	dw $0b80,$0b72,$0b48,$0b02,$0aa0,$0a24,$0990,$08e4 ; 16-23
	dw $0822,$074c,$0664,$056c,$0466,$0356,$023e,$0120 ; 24-31
	dw $0000,$fee2,$fdc4,$fcac,$fb9c,$fa96,$f99e,$f8b6 ; 32-39
	dw $f7e0,$f71e,$f672,$f5de,$f562,$f500,$f4ba,$f490 ; 40-47
	dw $f482,$f490,$f4ba,$f500,$f562,$f5de,$f672,$f71e ; 48-55
	dw $f7e0,$f8b6,$f99e,$fa96,$fb9c,$fcac,$fdc4,$fee2 ; 56-63

mus_vibrato_sinus_28: ; depth 24.00 semi-tones 
	dw $0000,$012e,$0258,$037c,$0498,$05a8,$06aa,$079c ; 0-7
	dw $087c,$0946,$09fa,$0a96,$0b16,$0b7c,$0bc4,$0bf2 ; 8-15
	dw $0c00,$0bf2,$0bc4,$0b7c,$0b16,$0a96,$09fa,$0946 ; 16-23
	dw $087c,$079c,$06aa,$05a8,$0498,$037c,$0258,$012e ; 24-31
	dw $0000,$fed4,$fdaa,$fc86,$fb6a,$fa5a,$f958,$f866 ; 32-39
	dw $f786,$f6bc,$f608,$f56c,$f4ec,$f486,$f43e,$f410 ; 40-47
	dw $f402,$f410,$f43e,$f486,$f4ec,$f56c,$f608,$f6bc ; 48-55
	dw $f786,$f866,$f958,$fa5a,$fb6a,$fc86,$fdaa,$fed4 ; 56-63

mus_vibrato_sinus_29: ; depth 28.00 semi-tones 
	dw $0000,$0160,$02bc,$0410,$055c,$069a,$07c8,$08e2 ; 0-7
	dw $09e6,$0ad2,$0ba4,$0c58,$0cf0,$0d66,$0dbc,$0dee ; 8-15
	dw $0e00,$0dee,$0dbc,$0d66,$0cf0,$0c58,$0ba4,$0ad2 ; 16-23
	dw $09e6,$08e2,$07c8,$069a,$055c,$0410,$02bc,$0160 ; 24-31
	dw $0000,$fea2,$fd46,$fbf2,$faa6,$f968,$f83a,$f720 ; 32-39
	dw $f61c,$f530,$f45e,$f3aa,$f312,$f29c,$f246,$f214 ; 40-47
	dw $f202,$f214,$f246,$f29c,$f312,$f3aa,$f45e,$f530 ; 48-55
	dw $f61c,$f720,$f83a,$f968,$faa6,$fbf2,$fd46,$fea2 ; 56-63

mus_vibrato_sinus_30: ; depth 32.00 semi-tones 
	dw $0000,$0192,$0320,$04a6,$0620,$078a,$08e4,$0a26 ; 0-7
	dw $0b50,$0c5e,$0d4e,$0e1c,$0ec8,$0f50,$0fb2,$0fec ; 8-15
	dw $1000,$0fec,$0fb2,$0f50,$0ec8,$0e1c,$0d4e,$0c5e ; 16-23
	dw $0b50,$0a26,$08e4,$078a,$0620,$04a6,$0320,$0192 ; 24-31
	dw $0000,$fe70,$fce2,$fb5c,$f9e2,$f878,$f71e,$f5dc ; 32-39
	dw $f4b2,$f3a4,$f2b4,$f1e6,$f13a,$f0b2,$f050,$f016 ; 40-47
	dw $f002,$f016,$f050,$f0b2,$f13a,$f1e6,$f2b4,$f3a4 ; 48-55
	dw $f4b2,$f5dc,$f71e,$f878,$f9e2,$fb5c,$fce2,$fe70 ; 56-63

mus_vibrato_sinus_31: ; depth 36.00 semi-tones 
	dw $0000,$01c4,$0382,$053a,$06e4,$087c,$0a00,$0b6c ; 0-7
	dw $0cba,$0dea,$0ef8,$0fe0,$10a2,$113a,$11a8,$11ea ; 8-15
	dw $1200,$11ea,$11a8,$113a,$10a2,$0fe0,$0ef8,$0dea ; 16-23
	dw $0cba,$0b6c,$0a00,$087c,$06e4,$053a,$0382,$01c4 ; 24-31
	dw $0000,$fe3e,$fc80,$fac8,$f91e,$f786,$f602,$f496 ; 32-39
	dw $f348,$f218,$f10a,$f022,$ef60,$eec8,$ee5a,$ee18 ; 40-47
	dw $ee02,$ee18,$ee5a,$eec8,$ef60,$f022,$f10a,$f218 ; 48-55
	dw $f348,$f496,$f602,$f786,$f91e,$fac8,$fc80,$fe3e ; 56-63
