;***************************************************************
;*							       *
;*		 Game Pak Registration Data    		       *
;*		    as of June 25, 1998		  	       *
;* 		        				       *
;***************************************************************

_type	equ $80 ; $80 = gbc $00 = gb

;***************************************************************************

;---------------------------------------------------------------------------
; There will be many changes in early programs, so 
; please re-confirm comments and code written below.
;---------------------------------------------------------------------------

	org	$000:$0100  		; Locates in address $0100 bank $00

;------------------------- STARTING VECTOR ---------------------
	nop			; $00
	jp	code_start	; $C3,$50,$01
				; USER PROGRAM START ADDRESS


;------------------ NINTENDO LOGO CHARACTER DATA ---------------

;104H~
;	db	$fe,$fe,$f0,$e0	; fatass logo
;	db	$00,$7e,$00,$ee
;	db	$ee,$fe,$00,$c0
;	db	$00,$7e,$00,$ee
;	db	$00,$7e,$00,$c0
;	db	$00,$7e,$00,$c0
;	db	$ee,$ee,$00,$00
;	db	$ee,$f7,$ee,$ee
;	db	$ee,$f7,$00,$ee
;	db	$ee,$f7,$ee,$ee
;	db	$70,$ff,$ce,$ec
;	db	$70,$ff,$ce,$ec

	db	$CE,$ED,$66,$66
	db	$CC,$0D,$00,$0B
	db	$03,$73,$00,$83
	db	$00,$0C,$00,$0D
	db	$00,$08,$11,$1F
	db	$88,$89,$00,$0E
	db	$DC,$CC,$6E,$E6
	db	$DD,$DD,$D9,$99
	db	$BB,$BB,$67,$63
	db	$6E,$0E,$EC,$CC
	db	$DD,$DC,$99,$9F
	db	$BB,$B9,$33,$3E


;       Title and Game registration area

;--------------  GAME TITLE (15 BYTES TOTAL) -------------------
; Note:  If letters are surrounded by ', then the will become 
;	 ASCII Characters
;134H~
	db 'Fatass Tracker!'	; 15 bytes

;---------------------=  CGB CODE ------------------------------
; The game code used to be 16 Bytes, but this has changed for 
; CGB.  Prior games contained 00H or ASCII code.  The CGB code 
; is 80H.
;143H
	db _type		; 80: CGB, 00(or other ASCII): Not CGB

;----------------- Maker Code (ASCII CODE) ---------------------
;144H~
	db	$00,$00		; 0,0: null(These are ASCII CODE,
				; and '0','0' doesn't actually exsist)

;----------------  SUPER GAMEBOY FUNCTION ----------------------
;146H
	db	$00		; 3: Uses SGB functions
				; 0:GAMEBOY(SUPER GAMEBOYè„Ç≈Ç‡ìÆçÏ)

;---------------------- GAME PAK TYPE --------------------------
;147H (Caution:  You must use MBC5 with 2x speed mode - 06/01/98)
	db	$1b	; 0:ONLY ROM.1:ROM+MBC1,2:ROM+MBC1+SRAM
			; 3:ROM+MBC1+SRAM(Battery),5:ROM+MBC2
			; 6:ROM+MBC2(Battery),8:ROM+SRAM,9:ROM+SRAM(Battery)
			; B:ROM+MMM01,C:ROM+MMM01+SRAM,D:ROM+MMM01+SRAM(Battery)
			; F:ROM+MBC3(Timer,Battery),10:ROM+MBC3(Timer)+SRAM(Battery)
			; 11:ROM+MBC3,12:ROM+MBC3+SRAM,13:ROM+MBC3+SRAM(Battery)
			; 15:ROM+MBC4,16:ROM+MBC4+SRAM,17:ROM+MBC4+SRAM(Battery)
			; 19:ROM+MBC5,1A:ROM+MBC5+SRAM,1B:ROM+MBC5+SRAM(Battery)
			; 1C:ROM+MBC5(Rumble),1D:ROM+MBC5(Rumble)+SRAM,1E:ROM+MBC5(Rumble)+SRAM(Battery)
			; Don't care about parts structure listed below
			; FC: Pocket Camera, FD: Bandai TAMA5
			; FE: Hudson HuC-3, FF: Hudson HuC-1

;------------------------ ROM SIZE -----------------------------
;148H
	db	$04			; 0: 256Kb, 1: 512Kb, 2: 1Mb
					; 3: 2Mb, 4: 4Mb, 5: 8Mb, 6: 16Mb
					; 7: 32Mb
					; 52: 9Mb, 53: 10Mb, 54: 12Mb
;------------------------ RAM SIZE -----------------------------
;149H
	db	$04			; 0: None or MBC2, 2: 64Kb, 3: 256Kb
					; 4: 1Mb (planned in future)

;-------------------- DESTINATION CODE -------------------------
;14AH
	db	$01			; 0: Japan, 1: Other Countries

;---------------------- FIXED VALUE ----------------------------
;14BH
	db	$33			; Fixed value

;--------------------- VERSION NUMBER --------------------------
;14CH
	db	$00

;-------------------- COMPLIMENT CHECK -------------------------
;14DH
;					; Sum registers (134H-14CH)
					; add + $19
					; invert ($ff-x)
					; add + $1 
					; E7H is used for games in development

	db $21+_type

;----------------------- CHECK SUM -----------------------------
;14EH~
	db	$00,$00			; ROM summation for entire program
					; These Bytes must be 00 when summing,
					; then enter final Check Sum.

;***************************************************************************

	end
