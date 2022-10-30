;---------------------------------------------------------------------------
; core routines for palettes
;
; palette_setup_bkg: 		sets up the default palettes - vbl or screen off
; palette_fade_to_white_bkg:	brings the bkg palette one step closer to white
; palette_morph_bkg:		brings the bkg palette one step closer to the one pointed to by current
; palette_blank_bkg:		blanks the bkg palette to white - vbl or screenoff
; palette_copy_from_buffer_bkg:	copies the current bkg palette buffer to vram - call during vbl
;---------------------------------------------------------------------------
; notes: same shit for _obj but for sprite palettes
; notes: for both fades & morphs, it takes a maximum of 32 calls to get there.
;---------------------------------------------------------------------------

;***************************************************************************
; variables

colour_table		equ pal_ram+$00	; $1c bytes

;***************************************************************************
;---------------------------------------------------------------------------
palette_ram_load:

	ld hl,colour_table_save_ram
	ld de,colour_table

palette_ram_work:

	xor a
	ld ($4666),a
	ld a,$0a
	ld ($1666),a

	ld c,$1c

_pal_ram_init_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_pal_ram_init_loop

	xor a
	ld ($1666),a

	ret

;---------------------------------------------------------------------------

palette_ram_save:

	ld hl,colour_table
	ld de,colour_table_save_ram

	jr palette_ram_work

;---------------------------------------------------------------------------

palette_fix_last_entry:

	ld de,(29*2*256)+2
	ld bc,colour_index+29
	jr setup_pal_loop

;---------------------------------------------------------------------------

palette_setup_main:

	ld de,2
	ld bc,colour_index

setup_pal_loop:

	ld a,(bc)
	add a,<colour_table
	ld l,a
	ld a,>colour_table
	adc a,0
	ld h,a

	inc bc

	ld a,d				; start at beginning, auto increment
	inc d
	ld ($ff68),a
	ld ($ff6a),a

_scr_loop_1:
	ld a,($ff41)
	and e
	jr nz,_scr_loop_1
	
	ld a,(hl)			; get colour byte
	ld ($ff69),a			; set colour byte
	ld ($ff6b),a			; set colour byte

	ld a,($ff41)
	and e
	jr nz,_scr_loop_1

	inc hl

	ld a,d				; start at beginning, auto increment
	inc d
	ld ($ff68),a
	ld ($ff6a),a

_scr_loop_2:
	ld a,($ff41)
	and e
	jr nz,_scr_loop_2
	
	ld a,(hl)			; get colour byte
	ld ($ff69),a			; set colour byte
	ld ($ff6b),a			; set colour byte

	ld a,($ff41)
	and e
	jr nz,_scr_loop_2

	ld a,d
	and $40
	jr z,setup_pal_loop		; loop

	ld a,$02
	ld ($ff6a),a
	ld hl,colour_table+$02

_scr_loop_3:
	ld a,($ff41)
	and e
	jr nz,_scr_loop_3

	ld a,(hl)	
	ld ($ff6b),a

	ld a,($ff41)
	and e
	jr nz,_scr_loop_3

	ld a,$03
	ld ($ff6a),a
	inc hl

_scr_loop_4:
	ld a,($ff41)
	and e
	jr nz,_scr_loop_4

	ld a,(hl)	
	ld ($ff6b),a

	ld a,($ff41)
	and e
	jr nz,_scr_loop_4

	ret

;---------------------------------------------------------------------------

colour_index:
	db $06,$0c,$0a,$00
	db $08,$0c,$0a,$00
	db $0e,$12,$10,$04
	db $00,$08,$0c,$0a
	db $04,$0e,$12,$10
	db $00,$14,$16,$0c
	db $08,$14,$16,$00
	db $04,$18,$1a,$12

;---------------------------------------------------------------------------

colour_table_presets:

	; frankenstein

	dw $0000 ; background 		- black
	dw $7fff ; main foreground	- white
	dw $0000 ; background 		- black
	dw $7fff ; main foreground	- white
	dw $7fe5 ; top highlight	- lt green
	dw $2980 ; top main		- md green
	dw $14c0 ; top shadow		- dk green
	dw $7d5b ; bottom highlight	- lt purple
	dw $300a ; bottom main		- md purple
	dw $1805 ; bottom shadow	- dk purple
	dw $43dd ; scope/top text 1	- yellow
	dw $3bce ; top text 2		- green
	dw $3333 ; bottom text 1	- yellow
	dw $6666 ; bottom text 2	- blue

	; tan scheme

	dw $0825 ; $00 background
	dw $7fb8 ; $02 main foreground
	dw $0825 ; $00 background
	dw $7fb8 ; $02 main foreground
	dw $49bf ; $04 top highlight
	dw $2db0 ; $06 top main
	dw $08ca ; $08 top shadow
	dw $7d40 ; $0a bottom highlight
	dw $3000 ; $0c bottom main
	dw $1800 ; $0e bottom shadow
	dw $3f7a ; $10 top text 1/scope
	dw $52ff ; $12 top text 2
	dw $3333 ; $14 bottom text 1
	dw $6666 ; $16 bottom text 2

	; BROWN & BLUE


	dw $04ed ; $00 background
	dw $7fb8 ; $02 main foreground
	dw $20a1 ; $00 background
	dw $7fb8 ; $02 main foreground
	dw $31d3 ; $04 top highlight
	dw $2db0 ; $06 top main
	dw $08ca ; $08 top shadow
	dw $48c6 ; $0a bottom highlight
	dw $38a5 ; $0c bottom main
	dw $20a5 ; $0e bottom shadow
	dw $7fec ; $10 top text 1/scope
	dw $52ff ; $12 top text 2
	dw $333c ; $14 bottom text 1
	dw $7f2c ; $16 bottom text 2


;; b&w scheme
;
;	dw $0000 ; $00 background
;	dw $7fff ; $02 main foreground
;	dw $0000 ; $00 background
;	dw $7fff ; $02 main foreground
;	dw $2108 ; $04 top highlight
;	dw $1084 ; $06 top main
;	dw $0842 ; $08 top shadow
;	dw $4210 ; $0a bottom highlight
;	dw $318c ; $0c bottom main
;	dw $0884 ; $0e bottom shadow
;	dw $4210 ; $10 top text 1/scope
;	dw $2108 ; $12 top text 2
;	dw $7fff ; $14 bottom text 1
;	dw $2108 ; $16 bottom text 2

	; frankenstein

	dw $0000 ; background 		- black
	dw $7fff ; main foreground	- white
	dw $0000 ; background 		- black
	dw $7fff ; main foreground	- white
	dw $7d5b ; bottom highlight	- lt purple
	dw $300a ; bottom main		- md purple
	dw $1805 ; bottom shadow	- dk purple
	dw $7fe5 ; top highlight	- lt green
	dw $2980 ; top main		- md green
	dw $14c0 ; top shadow		- dk green
	dw $3333 ; bottom text 1	- yellow
	dw $6666 ; bottom text 2	- blue
	dw $43dd ; scope/top text 1	- yellow
	dw $3bce ; top text 2		- green

	; tan scheme

	dw $0825 ; $00 background
	dw $7fb8 ; $02 main foreground
	dw $0825 ; $00 background
	dw $7fb8 ; $02 main foreground
	dw $7d40 ; $0a bottom highlight
	dw $3000 ; $0c bottom main
	dw $1800 ; $0e bottom shadow
	dw $49bf ; $04 top highlight
	dw $2db0 ; $06 top main
	dw $08ca ; $08 top shadow
	dw $3333 ; $14 bottom text 1
	dw $6666 ; $16 bottom text 2
	dw $3f7a ; $10 top text 1/scope
	dw $52ff ; $12 top text 2

	; b&w scheme
	; poop and moss scheme

	dw $0060
	dw $7fff
	dw $0005
	dw $7fff
	dw $2d80
	dw $1d20
	dw $10a0
	dw $00ce
	dw $008a
	dw $0045
	dw $13ff
	dw $0145
	dw $0e13
	dw $0054

;	dw $0000 ; $00 background
;	dw $7fff ; $02 main foreground
;	dw $0000 ; $00 background
;	dw $7fff ; $02 main foreground
;	dw $4210 ; $0a bottom highlight
;	dw $318c ; $0c bottom main
;	dw $0884 ; $0e bottom shadow
;	dw $2108 ; $04 top highlight
;	dw $1084 ; $06 top main
;	dw $0842 ; $08 top shadow
;	dw $7fff ; $14 bottom text 1
;	dw $2108 ; $16 bottom text 2
;	dw $4210 ; $10 top text 1/scope
;	dw $2108 ; $12 top text 2

	end

