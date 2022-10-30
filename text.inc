
;***************************************************************************
textbank00	equ	$07
text00		group	textbank00		; Set group bank
		org 	textbank00:$4000
this_bank	=	textbank00
;***************************************************************************

small_cset_tileset:
	libbin graphics\charsets\proportional\5x5_cset_tileset.bin
	libbin graphics\charsets\proportional\bold_5x5_cset_tileset.bin
	libbin graphics\charsets\proportional\grady_5x5_cset_tileset.bin
	libbin graphics\charsets\proportional\grady_5x5_cset3(italic)_tileset.bin
	libbin graphics\charsets\proportional\grady_5x5_cset2(bold)_tileset.bin
	libbin graphics\charsets\proportional\norm_cset_tileset.bin
	libbin graphics\charsets\proportional\slink_cset_tileset.bin

small_cset_tileset_widths:

	include graphics\charsets\proportional\5x5_cset_widths.z80
	include graphics\charsets\proportional\bold_5x5_cset_widths.z80
	include graphics\charsets\proportional\grady_5x5_cset_widths.z80
	include graphics\charsets\proportional\grady_5x5_cset3(italic)_widths.z80
	include graphics\charsets\proportional\grady_5x5_cset2(bold)_widths.z80
	include graphics\charsets\proportional\norm_cset_widths.z80
	include graphics\charsets\proportional\slink_cset_widths.z80

;***************************************************************************

text_eof		equ $ff
text_cr			equ $fe
text_set_font		equ $fd
text_set_y_size		equ $fc

;***************************************************************************
; introduction help text
;***************************************************************************

	db $ff
introduction_help_text_table:
	dw introduction_help_text00
	dw $ffff

introduction_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "INTRODUCTION",text_set_font,$00," - 01",text_cr

	db text_set_y_size,$06
	db "DATA ENTRY SHORTCUTS:",text_cr
	db "A + SELECT: SAVE",text_cr
	db "B + SELECT: EXIT",text_cr

	db text_eof

;***************************************************************************
; pattern editor help text
;***************************************************************************

	db $ff
pattern_editor_help_text_table:
	dw pattern_editor_help_text00
	dw pattern_editor_help_text01
	dw $ffff

pattern_editor_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "PATTERN EDITOR",text_set_font,$00," - 01",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "CONTROLS:",text_cr

	db text_set_font,$00,text_set_y_size,$06
	DB "U/D/L/R - MOVE CURSOR",text_cr
	DB "A + L/R - +/- TONE",text_cr
	DB "A + U/D - +/- OCTAVE",text_cr
	DB "B + U - DELETE EVENT",text_cr
	DB "B + D - INSERT EVENT",text_cr
	DB "SELECT - MAIN MENU",text_cr
	DB "A + SELECT - SEQUENCE EDITOR",text_cr
	DB "B + SELECT - EFFECTS EDITOR",text_cr
	DB "START - PLAY",text_cr
	DB "A + START - PLAY FROM START",text_cr
	db text_eof

pattern_editor_help_text01:
	db text_set_font,$06,text_set_y_size,$0c
	db "PATTERN EDITOR",text_set_font,$00," - 02",text_cr

	db text_set_font,$00,text_set_y_size,$06
	DB "B + START - PLAY LOOPING",text_cr
	DB "A + B + START - RESET EFFECTS",text_cr
	DB "A + B + U/D - MOVE SEQUENCE",text_cr
	db text_set_y_size,$09
	DB "A + B + L/R - ALTER SEQUENCE",text_cr

	db text_eof

;***************************************************************************
; effects editor help text
;***************************************************************************

	db $ff
effects_editor_help_text_table:
	dw effects_editor_help_text00
	dw effects_editor_help_text01
	dw effects_editor_help_text02
	dw effects_editor_help_text03
	dw effects_editor_help_text04
	dw effects_editor_help_text05
	dw effects_editor_help_text06
	dw effects_editor_help_text07
	dw effects_editor_help_text08
	dw effects_editor_help_text09
	dw effects_editor_help_text0a
	dw effects_editor_help_text0b
	dw effects_editor_help_text0c
	dw effects_editor_help_text0d
	dw effects_editor_help_text0e
	dw effects_editor_help_text0f
	dw effects_editor_help_text10
	dw effects_editor_help_text11
	dw effects_editor_help_text12
	dw effects_editor_help_text13
	dw effects_editor_help_text14
	dw $ffff

effects_editor_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 01",text_cr

	db text_set_font,$05,text_set_y_size,$07
	db "PULS:",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "ONLY TRACKS 1&2 CAN USE",text_cr
	db text_set_y_size,$08
	db "PULSE-WIDTH COMMANDS.",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "PULSE-WIDTH:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "0=12.5%,1=25%,2=50%,3=75%",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "AUTO-PWM ENTRY 1-4:",text_cr
	db text_set_font,$00
	db "AUTO-PWM MODULATES THE",text_cr
	db "PULSE-WIDTH THROUGH",text_cr
	db text_set_y_size,$08
	db "THESE 4 VALUES.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "AUTO-PWM RATE:",text_cr
	db text_set_font,$00
	db "HIGHER=SLOWER."

	db text_eof
	
effects_editor_help_text01:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 02",text_cr

	db text_set_font,$05,text_set_y_size,$07
	db "WAVE:",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "ONLY TRACK 3 CAN USE",text_cr
	db text_set_y_size,$08
	db "WAVEFORM COMMANDS.",text_cr

	db text_set_font,$01
	db text_set_y_size,$08
	db "WAVEFORM PRESET B:",text_cr
	db text_set_y_size,$06
	db "WAVEFORM PRESET #:",text_cr
	db text_set_font,$00
	db "LOADS A WAVEFORM PRESET.",text_cr
	db "00-0F=SINE FEEDBACK",text_cr
	db "10-17=SINE TO SQUARE",text_cr
	db "18-20=SQUARE TO TRIANGLE",text_cr
	db "20-27=SQUARE TO FL. SAW",text_cr
	db "28-2F=FL.SAW TO SAW"
	db text_eof

effects_editor_help_text02:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 03",text_cr

	db text_set_y_size,$06
	db "30-37=SAW TO DIST. SAW",text_cr
	db "38-40=DIST. SAW TO OCT. TRI",text_cr
	db "40-47=OCT. TRI TO 2 OCT. TRI",text_cr
	db "48-4B=2 OCT. TRI TO TUNE SPR.",text_cr
	db "4C-4F=TUNE SPR. SAW FEEDBK",text_cr
	db "50-57=FEEDBK TO RINGMD SQR",text_cr
	db "58-5F=RINGMD SQUARE TO SINE",text_cr
	db "60-7F=SINE PHASE SWEEP",text_cr
	db "80-8F=DISTORTED SINE",text_cr
	db "90-9F=PULSE WIDTH FUN",text_cr
	db "A0-D7=BAD SPEAKERS",text_cr
	db "D8-E3=SINE WIDTH FUN"
	db text_eof

effects_editor_help_text03:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 04",text_cr

	db text_set_y_size,$06
	db "E4-FB=MORE SINE DISTORTION",text_cr
	db text_set_y_size,$08
	db "FC-FF=BACK TO ORIGINAL SINE",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "WAVEFORM DIST TYPE:",text_cr
	db text_set_font,$00
	db "APPLIES A DISTORTION MATRIX",text_cr
	db "TO THE CURRENT WAVEFORM.",text_cr
	db "0=SMOOTH LOG (MAGNETO)",text_cr
	db "1=DIRTY TRIANGULAR (SF DIRT)"
	db "2=SMOOTH LOG+FUZZ (SF FUZZ)",text_cr
	db "3=SQUARE (SF HARD CLIP)",text_cr
	db "4=DECIMATOR SQUARE (LOFI)",text_cr
	db "5=GATED/COMPRESSED (HKN)"
	db text_eof

effects_editor_help_text04:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 05",text_cr

	db text_set_y_size,$06
	db "6=TUBE (WAVEWARM)",text_cr
	db text_set_y_size,$08
	db "7=NOISE (MANGLE)",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "WAVEFORM GAIN:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "0=SILENT,8=SAME,F=MAX.",text_cr

	db text_set_font,$01
	db text_set_y_size,$08
	db "WAVEFORM MIX PRE B:",text_cr
	db text_set_y_size,$06
	db "WAVEFORM MIX PRE #:",text_cr
	db text_set_font,$00
	db "MIXES THE CURRENT WAVEFORM",text_cr
	db text_set_y_size,$08
	db "WITH A PRESET.",text_cr
	db text_set_font,$01
	db text_set_y_size,$06
	db "WAVEFORM MIX RATE:",text_cr
	db text_set_font,$00
	db "00=-8 OCTAVES, TRUNCATED"
	db text_eof

effects_editor_help_text05:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 06",text_cr

	db text_set_y_size,$06
	db "01=-4 OCTAVES, TRUNCATED",text_cr
	db "02=-3 OCTAVES, TRUNCATED",text_cr
	db "04=-2 OCTAVES, TRUNCATED",text_cr
	db "08=-1 OCTAVES, TRUNCATED",text_cr
	db "10=SLIGHT DITHER",text_cr
	db "20=+1 OCTAVES",text_cr
	db "40=+2 OCTAVES",text_cr
	db "80=+3 OCTAVES",text_cr
	db text_set_y_size,$08
	db "FF=+4 OCTAVES (OR CLOSE)",text_cr


	db text_set_font,$05,text_set_y_size,$07
	db "VOLU:",text_cr
	db text_set_font,$00
	db text_set_y_size,$06
	db "SETTINGS ARE DIFFERENT FOR"
	db text_eof

effects_editor_help_text06:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 07",text_cr

	db text_set_y_size,$08
	db "TRACK 3!",text_cr
	db text_set_font,$01
	db text_set_y_size,$06
	db "INITIAL VOLUME:",text_cr
	db text_set_font,$00
	db "FOR TRACKS 1,2&4: 0-F, F=MAX.",text_cr
	db "FOR TRACK 3: 0=OFF, 2=MAX,",text_cr
	db text_set_y_size,$08
	db "4=1/2, 6=1/4.",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "RESTART ENVELOPE:",text_cr
	db text_set_font,$00
	db "RESTARTS THE ENVELOPE AND",text_cr
	db "DURATION. USE DURING",text_cr
	db "PORTAMENTOS, AFTER LOADING",text_cr
	db "A WAVEFORM, OR TO RETRIGGER",text_cr
	db "CURRENT SOUND."

	db text_eof

effects_editor_help_text07:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 08",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "ENVELOPE DIRECTON:",text_cr
	db text_set_font,$00
	db "FOR TRACKS 1,2&4 ONLY:",text_cr
	db text_set_y_size,$08
	db "UP=INCREASE, DN=DECREASE.",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "ENVELOPE RATE:",text_cr
	db text_set_font,$00
	db "FOR TRACKS 1,2&4 ONLY:",text_cr
	db text_set_y_size,$08
	db "HIGHER=SLOWER.",text_cr

	db text_set_font,$05,text_set_y_size,$07
	db "DURA:",text_cr
	db text_set_font,$00
	db text_set_y_size,$06
	db "SETTINGS ARE DIFFERENT FOR",text_cr
	db text_set_y_size,$08
	db "TRACK 3!",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "DURATION:",text_cr
	db text_set_font,$00
	db "FOR TRACKS 1,2&4: 0-3F.",text_cr

effects_editor_help_text08:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 09",text_cr

	db text_set_y_size,$06
	db "FOR TRACK 3: 0-FF.",text_cr
	db text_set_y_size,$08
	db "HIGHER=SHORTER.",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "INFINITE DURATION:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "IGNORES DURATION SETTING.",text_cr


	db text_set_font,$05,text_set_y_size,$07+2
	db "PANN:",text_cr
	db text_set_font,$01
	db text_set_y_size,$06
	db "TRACK 1-4 PANNING:",text_cr
	db text_set_font,$00
	db "SETS THE PANNING FOR THAT",text_cr
	db "TRACK. ONLY EFFECTS",text_cr
	db "CURRENT TRACK (EXCEPT WHEN",text_cr
	db "USED IN SEQUENCE).",text_cr
	db "(OFF, RIGHT, LEFT, CENTER)."

	db text_eof

effects_editor_help_text09:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 10",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "AUTO-PAN ENTRY 1-4:",text_cr
	db text_set_font,$00
	db "AUTO-PAN MODULATES THE",text_cr
	db "PANNING OF THIS TRACK",text_cr
	db text_set_y_size,$08
	db "THROUGH THESE 4 VALUES.",text_cr

	db text_set_y_size,$06
	db text_set_font,$01
	db "AUTO-PAN RATE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=SLOWER.",text_cr

	db text_set_font,$05,text_set_y_size,$07+2
	db "VIBR:",text_cr
	db text_set_font,$01,text_set_y_size,$06
	db "VIBRATO DEPTH:",text_cr
	db text_set_font,$00
	db "00=0.12 SEMI-TONES",text_cr
	db "01=0.25 SEMI-TONES",text_cr
	db "02=0.37 SEMI-TONES"
	db text_eof

effects_editor_help_text0a:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 11",text_cr

	db text_set_y_size,$06
	db "03=0.50 SEMI-TONES",text_cr
	db "04=0.62 SEMI-TONES",text_cr
	db "05=0.75 SEMI-TONES",text_cr
	db "06=0.87 SEMI-TONES",text_cr
	db "07=1.00 SEMI-TONES",text_cr
	db "08=1.25 SEMI-TONES",text_cr
	db "09=1.50 SEMI-TONES",text_cr
	db "0A=1.75 SEMI-TONES",text_cr
	db "0B=2.00 SEMI-TONES",text_cr
	db "0C=2.25 SEMI-TONES",text_cr
	db "0D=2.50 SEMI-TONES",text_cr
	db "0E=2.75 SEMI-TONES"
	db text_eof

effects_editor_help_text0b:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 12",text_cr

	db text_set_y_size,$06
	db "0F=3.00 SEMI-TONES",text_cr
	db "10=3.50 SEMI-TONES",text_cr
	db "11=4.00 SEMI-TONES",text_cr
	db "12=4.50 SEMI-TONES",text_cr
	db "13=5.00 SEMI-TONES",text_cr
	db "14=5.50 SEMI-TONES",text_cr
	db "15=6.00 SEMI-TONES",text_cr
	db "16=6.50 SEMI-TONES",text_cr
	db "17=7.00 SEMI-TONES",text_cr
	db "18=8.00 SEMI-TONES",text_cr
	db "19=9.00 SEMI-TONES",text_cr
	db "1A=10.00 SEMI-TONES"
	db text_eof

effects_editor_help_text0c:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 13",text_cr

	db text_set_y_size,$06
	db "1B=11.00 SEMI-TONES",text_cr
	db "1C=12.00 SEMI-TONES/1 OCTAVE",text_cr
	db "1D=15.33 SEMI-TONES",text_cr
	db "1E=18.66 SEMI-TONES",text_cr
	db "1F=24.00 SEMI-TONES/2",text_cr
	db text_set_y_size,$08
	db "OCTAVES",text_cr


	db text_set_font,$01,text_set_y_size,$06
	db "VIBRATO RATE:",text_cr
	db text_set_font,$00
	db "40=FASTEST, >40 MOVES",text_cr
	db "BACKWARDS SO THAT 01 & 7F",text_cr
	db text_set_y_size,$08
	db "ARE SLOWEST.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "RESET VIBRATO:"
	db text_eof

effects_editor_help_text0d:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 14",text_cr

	db text_set_y_size,$06
	db "SETS THE VIBRATO PHASE TO",text_cr
	db "0, YOU SHOULD DO THIS WHEN",text_cr
	db "YOU TURN OFF A VIBRATO IF",text_cr
	db text_set_y_size,$08
	db "YOU WANT TO STAY IN TUNE.",text_cr


	db text_set_font,$05,text_set_y_size,$09
	db "PORT:",text_cr
	db text_set_font,$01,text_set_y_size,$06
	db "PORTAMENTO SOURCE:",text_cr
	db text_set_font,$00
	db "SETS THE NOTE TO SLIDE",text_cr
	db "FROM. OTHERWISE, CURRENT",text_cr
	db text_set_y_size,$08
	db "TONE WILL BE USED.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "PORTAMENTO RATE:",text_cr
	db text_set_font,$00
	db "HIGHER=SLOWER."
	db text_eof

effects_editor_help_text0e:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 15",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "GLISSANDO MODE:",text_cr
	db text_set_font,$00
	db "GLISSANDO MODE AFFECTS",text_cr
	db "VIBRATOS AS WELL-WHAT IT",text_cr
	db "DOES IS SKIP OUT-OF-TUNE",text_cr
	db text_set_y_size,$08
	db "FREQUENCIES.",text_cr


	db text_set_font,$05,text_set_y_size,$09
	db "ARPE:",text_cr
	db text_set_font,$01,text_set_y_size,$06
	db "ARPEGGIO ENTRY 1-4:",text_cr
	db text_set_font,$00
	db "ARPEGGIO MODULATES THE",text_cr
	db "TUNE OF THIS TRACK",text_cr
	db "THROUGH THESE 4 VALUES.",text_cr
	db "EACH VALUE IS A POSITIVE"
	db text_eof

effects_editor_help_text0f:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 16",text_cr

	db text_set_y_size,$06
	db "NUMBER OF HALF STEPS TO",text_cr
	db "TRANSPOSE. TRACK AND SONG",text_cr
	db "TRANSPOSE ARE ALSO TAKEN",text_cr
	db text_set_y_size,$08
	db "INTO ACCOUNT.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "ARPEGGIO RATE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=SLOWER.",text_cr

	db text_set_font,$05,text_set_y_size,$07
	db "SWEE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$06
	db "TRACK 1 ONLY! IT'S JUST A",text_cr
	db text_set_y_size,$08
	db "LINEAR SLIDE EFFECT ANYWAY.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "SWEEP DIRECTION:",text_cr
	db text_set_font,$00
	db "UP=INCREASE, DN=DECREASE."
	db text_eof

effects_editor_help_text10:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 17",text_cr

	db text_set_y_size,$06
	db text_set_font,$01
	db "SWEEP DEPTH:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=LESS",text_cr

	db text_set_y_size,$06
	db text_set_font,$01
	db "SWEEP RATE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=SLOWER",text_cr

	db text_set_font,$05,text_set_y_size,$09
	db "TRIG:",text_cr
	db text_set_font,$01,text_set_y_size,$06
	db "TRIGGER DELAY:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=SLOWER",text_cr
	db text_set_font,$01,text_set_y_size,$06
	db "RETRIGGER DELAY:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=SLOWER",text_cr

	db text_set_font,$05
	db "SPEE:"
	db text_eof

effects_editor_help_text11:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 18",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "SONG SPEED:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "HIGHER=FASTER",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "SONG SHUFFLE PHASE:",text_cr
	db text_set_font,$00
	db "SETS THE PHASE OF THE",text_cr
	db "DRAG IN THE SHUFFLE.",text_cr
	db text_set_y_size,$08
	db "00 IS GENERALLY MIDDLE.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "SONG SHUFFLE RATE:",text_cr
	db text_set_font,$00
	db "NUMBER OF LINES-1 FOR",text_cr
	db "THE SHUFFLE TO LOOP.",text_cr
	db text_set_y_size,$08
	db "IE, 7 REPEATS IN 8 LINES.",text_cr
	
	db text_set_font,$01,text_set_y_size,$06
	db "SONG SHUFFLE DEPTH:"
	db text_eof

effects_editor_help_text12:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 19",text_cr

	db text_set_y_size,$08
	db "HIGHER=DEEPER",text_cr

	db text_set_font,$05,text_set_y_size,$09
	db "TRAN:",text_cr
	db text_set_font,$01,text_set_y_size,$06
	db "TRANSPOSE:",text_cr
	db text_set_font,$00
	db "NUMBER OF SEMITONES TO",text_cr
	db "TRANSPOSE.  NOTE THAT",text_cr
	db "TRACK TRANSPOSE, SEQ",text_cr
	db "TRANSPOSE AND ARPEGGIO",text_cr
	db text_set_y_size,$08
	db "ARE ACCUMULATIVE.",text_cr

	db text_set_font,$01,text_set_y_size,$06
	db "FINE-TUNE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "40=ONE SEMITONE."
	db text_eof

effects_editor_help_text13:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 20",text_cr

	db text_set_font,$05,text_set_y_size,$07
	db "SAMP:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "CHANNEL 3 ONLY!",text_cr
	db text_set_font,$01
	db "PLAY SAMPLE BANK:",text_cr
	db text_set_y_size,$06
	db "PLAY SAMPLE NUMBER:",text_cr
	db text_set_font,$00
	db "00-0F=BASS DRUMS",text_cr
	db "10-1F=SNARE DRUMS",text_cr
	db "20-2F=HIHATS",text_cr
	db "30-3F=RIDE CYMBALS",text_cr
	db "40-4F=CRASH CYMBALS",text_cr
	db "50-5F=TOMS",text_cr
	db "60-6F=SCRATCHES",text_cr
	db text_eof

effects_editor_help_text14:
	db text_set_font,$06,text_set_y_size,$0c
	db "EFFECTS EDITOR",text_set_font,$00," - 21",text_cr

	db text_set_y_size,$08
	db "70-FF=UNUSED CURRENTLY",text_cr

	db text_set_font,$01
	db text_set_y_size,$06
	db "STOP SAMPLE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$08
	db "STOPS THE CURRENT SAMPLE.",text_cr

	db text_set_font,$05,text_set_y_size,$09
	db "MUTATE:",text_cr
	db text_set_font,$00
	db text_set_y_size,$06
	db "RANDOMLY ALTERS ALL",text_cr
	db "SETTINGS SET ON THIS EVENT."
	db text_eof
	
;***************************************************************************

;***************************************************************************
; sequence editor help text
;***************************************************************************

	db $ff
sequence_editor_help_text_table:
	dw sequence_editor_help_text00
	dw sequence_editor_help_text01
	dw $ffff

sequence_editor_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "SEQUENCE EDITOR",text_set_font,$00," - 01",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "CONTROLS:",text_cr

	db text_set_font,$00,text_set_y_size,$06
	DB "U/D/L/R - MOVE CURSOR",text_cr
	DB "A + L/R - +/- 1 PATTERN",text_cr
	DB "A + U/D - +/- 4 PATTERMS",text_cr
	DB "B + U - DELETE EVENT",text_cr
	DB "B + D - INSERT EVENT",text_cr
	DB "SELECT - MAIN MENU",text_cr
	DB "A + SELECT - PATTERN EDITOR",text_cr
	DB "B + SELECT - EFFECTS EDITOR",text_cr
	DB "START - PLAY",text_cr
	DB "A + START - PLAY FROM START",text_cr
	db text_eof

sequence_editor_help_text01:
	db text_set_font,$06,text_set_y_size,$0c
	db "SEQUENCE EDITOR",text_set_font,$00," - 02",text_cr

	db text_set_font,$00,text_set_y_size,$06
	DB "B + START - PLAY LOOPING",text_cr
	DB "A + B + START - RESET EFFECTS",text_cr
	DB "A + B + U/D/L/R - MOVE PATTERN",text_cr

	db text_eof

;***************************************************************************
; known bugs help text
;***************************************************************************

	db $ff
known_bugs_help_text_table:
	dw known_bugs_help_text00
	dw $ffff

known_bugs_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "KNOWN BUGS",text_set_font,$00," - 01",text_cr

	db text_set_font,$03,text_set_y_size,$06
	db "-BW GB HAS ONLY 1 $D000 BANK,",text_cr
	db " SO NO EDITING/PLAYBACK.",text_cr
	db "-SONGS WITH MACROS ARE BAD.",text_cr
	db "-PALETTES BAD SOMETIMES.",text_cr
	db "-IF YOU POWER OFF DURING",text_cr
	db " SAVERAM WRITE, IT FORMATS.",text_cr

	db text_eof

;***************************************************************************
; revision history help text
;***************************************************************************

	db $ff
revision_history_help_text_table:
	dw revision_history_help_text00
	dw revision_history_help_text01
	dw revision_history_help_text02
	dw revision_history_help_text03
	dw revision_history_help_text04
	dw revision_history_help_text05
	dw revision_history_help_text06
	dw revision_history_help_text07
	dw revision_history_help_text08
	dw $ffff

revision_history_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 01",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.14 - MEGA BUG FIXES",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        LOTS OF CLEANUP, ALSO",text_cr
	db "        ADDED RESET EFFECTS",text_cr
	db text_set_y_size,$09
	db "        AND MORE HELP INFO.",text_cr

	db text_eof

revision_history_help_text01:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 02",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.13 - REDO",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        FIXED SEVERAL MORE",text_cr
	db "        BUGS AND ADDED REDO,",text_cr
	db "        HOWEVER, SONG SAVE",text_cr
	db text_set_y_size,$09
	db "        FORMAT WAS ALTERED.",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.12 - MEGA BUG FIXES",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        LOTS OF CLEANUP, ALSO",text_cr
	db "        ADDED PATTERN COPY",text_cr
	db text_set_y_size,$09
	db "        INSERT.",text_cr

	db text_eof

revision_history_help_text02:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 03",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.11 - TIME STAMP",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        TIME STAMP, EFFECTS",text_cr
	db "        COPY/PASTE/MUTATE,",text_cr
	db "        COPY/PASTE FROM/TO",text_cr
	db "        CURSOR, RASTER CURSOR",text_cr
	db "        MULTICOLOUR CHARSETS,",text_cr
	db "        REORGANIZED SAMPLES,",text_cr
	db "        AND PROBABLY SOME MORE",text_cr
	db "        STUFF ADDED, PLUS LOTS",text_cr
	db text_set_y_size,$09
	db "        OF TWEAKS AND BUGFIXES.",text_cr

	db text_eof

revision_history_help_text03:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 04",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.10 - FULL FUNCTIONS",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        EFFECTS EDITING AND",text_cr
	db "        SONG NAME ENTRY ADDED.",text_cr
	db text_set_y_size,$09
	db "        SOME BUGFIXES, TOO.",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.09 - BASIC EDITOR!",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        REMAPPED KEYS AND",text_cr
	db "        ADDED NOTE ENTRY/SEQ.",text_cr
	db "        EDITOR FUNCTIONS, ALSO",text_cr
	db "        INSERT AND DELETE.",text_cr
	db text_set_y_size,$09
	db "        AND NEW PATTERN INS.",text_cr

	db text_eof

revision_history_help_text04:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 05",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.08 - COPY & PASTE",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        TRACK COPY, TRACK PASTE",text_cr
	db "        & UNDO FUNCTIONS ADDED.",text_cr
	db "        UNDOS ARE ALSO SAVED",text_cr
	db text_set_y_size,$09
	db "        AND LOADED PROPERLY.",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.07 - OPTIMIZER",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        OPTIMIZE FUNCTION ADDED.",text_cr
	db "        COULD GET CRAZY IF I ADD",text_cr
	db "        THE CONVERT PARTS TO",text_cr
	db text_set_y_size,$09
	db "        MACROS METHOD.",text_cr

	db text_eof

revision_history_help_text05:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 06",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.06 - FILE ACCESS",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        LOAD, SAVE, AND DELETE",text_cr
	db "        TO/FROM SAVERAM ARE NOW",text_cr
	db text_set_y_size,$09
	db "        WORKING PROPERLY.",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.05 - WRAM PLAYER",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        MADE IT LOAD SONGS TO",text_cr
	db "        WRAM AND PLAY FROM THERE"
	db "        SONGS CAN CURRENTLY BE",text_cr
	db "        UP TO $5FFF BYTES LONG!",text_cr
	db "        AND ALSO REORGANISED",text_cr
	db text_set_y_size,$09
	db "        SAVERAM, BUGFIXES, ETC.",text_cr

	db text_eof

revision_history_help_text06:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 07",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.04 - NEW COMMANDS",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        ADDED 2 NEW COMMANDS-",text_cr
	db "        MACROS AND MULTI-WAIT.",text_cr
	db text_set_y_size,$09
	db "        ADDED FONT SEL. SAVE.",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.03 - CLEANED UP",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        FIXED PLASMA UP.",text_cr
	db "        WORKED ON SAVERAM, NOW",text_cr
	db "        IT SHOULD AUTODETECT",text_cr
	db "        THE MAXIMUM SAVERAM",text_cr
	db "        SIZE AND USE THAT (FOR",text_cr
	db "        BUNG CARTS, ETC.)",text_cr
	db "        ALSO WORKED ON DATA",text_cr
	db text_set_font,$00,text_set_y_size,$09
	db "        ENTRY SYSTEM.",text_cr

	db text_eof

revision_history_help_text07:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 08",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.02 - AFTER HD CRASH",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        ALTERED PALETTE SYSTEM",text_cr
	db "        SO THAT COLOURS CAN BE",text_cr
	db "        CONFIGURABLE. PLASMA",text_cr
	db "        PALETTES ARE NOW BAD",text_cr
	db "        AND PATTERNS ARE GONE.",text_cr
	db "        FADE IN AND OUT RASTER",text_cr
	db "        BARS ARE ALSO GONE.",text_cr
	db "        ADDED CONFIG MENU AND",text_cr
	db text_set_y_size,$09
	db "        DID SAVERAM FORMAT.",text_cr

	db text_eof

revision_history_help_text08:
	db text_set_font,$06,text_set_y_size,$0c
	db "REVISION HISTORY",text_set_font,$00," - 09",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "V0.01 - INITIAL VERSION",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "        BASIC PLAYER, NO EDITING",text_cr
	db "        FUNCTIONS AVAILIABLE,",text_cr
	db "        BASIC INTERFACE DESIGN,",text_cr
	db "        MENUING SYSTEM AND TEXT",text_cr
	db text_set_y_size,$09
	db "        SYSTEM IMPLEMENTED.",text_cr

;	db text_set_font,$05,text_set_y_size,$0a
;	db "DO NOT SPREAD THIS",text_cr
;	db text_set_font,$00,text_set_y_size,$08
;	db "IT HAS BEEN FINGERPRINTED",text_cr

	db text_eof
	
;***************************************************************************

;***************************************************************************
; contact_info_help_text
;***************************************************************************

	db $ff
contact_info_help_text_table:
	dw contact_info_help_text00
	dw $ffff

contact_info_help_text00:
	db text_set_font,$06,text_set_y_size,$0c
	db "CONTACT INFO/CREDS",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "CODE/ART/MUSIC:",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "JEREMY EVERS",text_cr
	db text_set_y_size,$09
	db "NEGSPECT@RUNANDGUN.COM",text_cr

	db text_set_font,$01,text_set_y_size,$08
	db "ADITIONAL MUSIC:",text_cr
	db text_set_font,$00,text_set_y_size,$06
	db "RANDY WILSON",text_cr
	db text_set_y_size,$09
	db "DECIBEL@RUNANDGUN.COM",text_cr

;	db text_set_font,$05,text_set_y_size,$0a
;	db "DO NOT SPREAD THIS",text_cr
;	db text_set_font,$00,text_set_y_size,$08
;	db "IT HAS BEEN FINGERPRINTED",text_cr

	db text_eof
	
;***************************************************************************

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
	
;***************************************************************************

example_text:
	db text_set_font,$04,text_set_y_size,$08
;	DB "FONT ONE - 5X5, 6 Y"
	db "@ABCDEFGHIJKLMNO"
	db "PQRSTUVWXYZ[\]^_"
	db " !",$22,"#$%&'()*+,-./"
	db "0123456789:;<=>?"
	DB text_cr
	db text_set_font,$05,text_set_y_size,$08
;	DB "FONT TWO - BOLD, 6 Y"
	db "@ABCDEFGHIJKLMNO"
	db "PQRSTUVWXYZ[\]^_"
	db " !",$22,"#$%&'()*+,-./"
	db "0123456789:;<=>?"
	DB text_cr
;	DB text_cr
;	db text_set_font,$02,text_set_y_size,$06
;	DB "FONT THREE - GRADY MONSTER, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$03,text_set_y_size,$06
;	DB "FONT FOUR - GRADY ITALIC, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$04,text_set_y_size,$06
;	DB "FONT FIVE - GRADY BOLD, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$05,text_set_y_size,$07
;	DB "FONT SIX - NORMAL, 7 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$06,text_set_y_size,$08
;	DB "FONT SEVEN - SLINK, 8 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;
;	db text_set_font,$00,text_set_y_size,$06
;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	db text_set_font,$01,text_set_y_size,$06
;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$02,text_set_y_size,$06
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$03,text_set_y_size,$06
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$04,text_set_y_size,$06
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$05,text_set_y_size,$07
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$06,text_set_y_size,$08
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"

	db text_eof

example_text_2:
	db text_set_font,$00,text_set_y_size,$0a
	DB "FONT ONE - 5X5, 6 Y"
	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	DB text_cr
	db text_set_font,$01,text_set_y_size,$06
	DB "FONT TWO - BOLD, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	DB text_cr
	db text_set_font,$02,text_set_y_size,$06
;	DB "FONT THREE - GRADY MONSTER, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$03,text_set_y_size,$06
;	DB "FONT FOUR - GRADY ITALIC, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$04,text_set_y_size,$06
;	DB "FONT FIVE - GRADY BOLD, 6 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$05,text_set_y_size,$07
;	DB "FONT SIX - NORMAL, 7 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	DB text_cr
;	db text_set_font,$06,text_set_y_size,$08
;	DB "FONT SEVEN - SLINK, 8 Y"
;;	DB "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
;;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;
;	db text_set_font,$00,text_set_y_size,$06
;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
;	db text_set_font,$01,text_set_y_size,$06
;	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$02,text_set_y_size,$06
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$03,text_set_y_size,$06
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$04,text_set_y_size,$06
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$05,text_set_y_size,$07
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"
	db text_set_font,$06,text_set_y_size,$08
	db "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]_ !#$%^&'()*+,-./0123456789,:;<=>?!\^"

	db text_eof

;***************************************************************************

	end
