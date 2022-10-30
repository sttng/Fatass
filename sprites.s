cursor_colour		equ sprite_ram+$00	; $03 bytes
cursor_colour_buffer	equ sprite_ram+$03	; $10 bytes

;---------------------------------------------------------------------------
; sprite routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; sequence cursor
;---------------------------------------------------------------------------
;***************************************************************************

seq_cursor:

	ld hl,seq_cursor_pos
	ld de,$fe00+(36*4)
	ld c,$10

_sprite_loop:
	ld a,(hli)
	ld (de),a
	ld a,e
	inc a
	ld e,a
	dec c
	jr nz,_sprite_loop

	jr buffer_cursor_colours

;***************************************************************************
;---------------------------------------------------------------------------
; editor cursor
;---------------------------------------------------------------------------
;***************************************************************************

patt_cursor:

	ld a,(editor_track)
	swap a
	add a,<track_cursor_pos
	ld l,a
	ld a,>track_cursor_pos
	adc a,0
	ld h,a

	ld de,($fe00+(36*4))
	ld c,$10

_sprite_loop:
	ld a,(hli)
	ld (de),a
	ld a,e
	inc a
	ld e,a
	dec c
	jr nz,_sprite_loop
	
;***************************************************************************
;---------------------------------------------------------------------------
; buffer_cursor_colours
;---------------------------------------------------------------------------
;***************************************************************************

buffer_cursor_colours:

	ld a,($ff49)
	add a,7
	ld ($ff49),a

	ld a,palettebank00
	ld (current_rom_bank),a
	ld ($2666),a

	ld b,0

	ld a,(cursor_colour)
	inc a
	ld (cursor_colour),a

	ld c,a
	ld hl,plasma_sinus_2
;	ld hl,plasma_sinus
	add hl,bc
	ld a,(hl)

;	sla a
	ld e,a

	ld a,(cursor_colour+2)
	add a,$78
	ld (cursor_colour+2),a

	ld a,(cursor_colour+1)
	jr nc,_dont_inc
	dec a
	ld (cursor_colour+1),a

_dont_inc:

;	ld c,a
;	ld hl,plasma_sinus
;	add hl,bc
;	ld a,(hl)

	add a,e
	and $fe
	ld c,a

;l	ld a,(menu_cur_sel)
;l	sla a
;l	sla a
;l	sla a
;l	add a,c
;l	and $FE
;l	ld c,a

	ld hl,plasma_palette		; set bkg palette
	add hl,bc

	ld a,%10111110
	ld ($ff6a),a

	ld a,(hli)
	ld ($ff6b),a
	ld a,(hld)
	ld ($ff6b),a

	ld de,cursor_colour_buffer
	ld c,$10
_buffer_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_buffer_loop

	ret


;***************************************************************************
;---------------------------------------------------------------------------
; clear cursor
;---------------------------------------------------------------------------
;***************************************************************************

clear_cursor:

	ld bc,$e002

_z_0:
	ld a,($ff41)
	and c
	jr nz,_z_0

	ld a,b
	ld ($fe00+(36*4)),a

	ld a,($ff41)
	and c
	jr nz,_z_0

_z_1:
	ld a,($ff41)
	and c
	jr nz,_z_1

	ld a,b
	ld ($fe00+(37*4)),a

	ld a,($ff41)
	and c
	jr nz,_z_1

_z_2:
	ld a,($ff41)
	and c
	jr nz,_z_2

	ld a,b
	ld ($fe00+(38*4)),a

	ld a,($ff41)
	and c
	jr nz,_z_2

_z_3:
	ld a,($ff41)
	and c
	jr nz,_z_3

	ld a,b
	ld ($fe00+(39*4)),a

	ld a,($ff41)
	and c
	jr nz,_z_3

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; reset patt bar
;---------------------------------------------------------------------------
;***************************************************************************

reset_pat_bar:
	ld de,sprite_bars
	jr set_bar

;***************************************************************************
;---------------------------------------------------------------------------
; set menu bar
;---------------------------------------------------------------------------
;***************************************************************************

set_menu_bar:
	ld de,menu_cursor_pos
set_bar:
	ld hl,$fe00+(30*4)
	ld bc,$2802

_z_0:
	ld a,($ff41)
	and c
	jr nz,_z_0

	ld a,(de)
	ld (hl),a

	ld a,($ff41)
	and c
	jr nz,_z_0

	inc l
	inc de

	dec b
	jr nz,_z_0

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; sprite setup
;---------------------------------------------------------------------------
;***************************************************************************

sprite_setup:

	ld hl,sprite_data
sprite_setup_2:
	ld de,$fe00
	ld c,40*4
_sprite_loop:
	ld a,(hli)
	ld (de),a
	inc e
	dec c
	jr nz,_sprite_loop
	ret

;***************************************************************************
;---------------------------------------------------------------------------
; copy hl de sprite ram
;---------------------------------------------------------------------------
;***************************************************************************

copy_hl_de_sprite_ram:

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	inc l
	ld a,e
	add a,4
	ld e,a

_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_1

	inc l
	ld a,e
	add a,4
	ld e,a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; clear de sprite ram
;---------------------------------------------------------------------------
;***************************************************************************

clear_de_sprite_ram:
_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,$20
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,e
	add a,4
	ld e,a

_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,$20
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,e
	add a,4
	ld e,a

_z_2:
	ld a,($ff41)
	and 2
	jr nz,_z_2

	ld a,$20
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_2

	ld a,e
	add a,4
	ld e,a

_z_3:
	ld a,($ff41)
	and 2
	jr nz,_z_3

	ld a,$20
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_3

	ld a,e
	add a,4
	ld e,a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; copy b number dw sprite ram
;---------------------------------------------------------------------------
;***************************************************************************

copy_b_number_de_sprite_ram:

	push hl

	ld h,>numbers
	ld a,b
	swap a
	and $0f
	add a,<numbers
	ld l,a

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,e
	add a,4
	ld e,a

	ld a,b
	and $0f
	add a,<numbers
	ld l,a

_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_1
	
	ld a,e
	add a,4
	ld e,a

	pop hl

	ret

;***************************************************************************

sprite_data:
	db $13,$10,$20,$05 ; sequence 0-19
	db $13,$18,$20,$05
	db $13,$28,$20,$05
	db $13,$30,$20,$05
	db $1b,$10,$20,$05
	db $1b,$18,$20,$05
	db $1b,$28,$20,$05
	db $1b,$30,$20,$05
	db $23,$10,$20,$05
	db $23,$18,$20,$05
	db $23,$28,$20,$05
	db $23,$30,$20,$05
	db $2b,$10,$20,$05
	db $2b,$18,$20,$05
	db $2b,$28,$20,$05
	db $2b,$30,$20,$05
	db $33,$10,$20,$05
	db $33,$18,$20,$05
	db $33,$28,$20,$05
	db $33,$30,$20,$05

	db $11,$a0,$cc,$01 ; graphics 20-29
	db $19,$a0,$cc,$01
	db $21,$a0,$cc,$01

	db $11,$a0,222,$00

	db $e1,$51,$d3,$93 ; graphics, but sprites are not needed
	db $e1,$91,$d3,$93
	db $e1,$a1,$d3,$93

;	db $13,$06,$cc,$01 
;	db $1b,$06,$cc,$01
;	db $13,$1e,$cc,$01
;	db $1b,$1e,$cc,$01

	db $11,$38,$d0,$03
	db $19,$38,$d1,$03
	db $21,$38,$d2,$03

;	db $21,$50,$cf,$03 ; graphics, but sprites are needed
;	db $21,$68,$cf,$03
;	db $21,$90,$cf,$03
;	db $21,$a0,$cf,$03

sprite_bars:

	db $6b,$08,$c4,$04 ; bars 30-35
	db $6b,$20,$c5,$04
	db $6b,$40,$c5,$04
	db $6b,$60,$c5,$04
	db $6b,$80,$c5,$04
	db $6b,$a0,$c6,$04

track_cursor_pos:

	db $e0,$e0,$d3,$97 ; blank 36
	db $6b,$28,$d3,$97 ; cursor 37-39
	db $6b,$30,$d3,$97
	db $6b,$38,$d3,$97

	db $e0,$e0,$d3,$97 ; blank 36
	db $6b,$48,$d3,$97
	db $6b,$50,$d3,$97
	db $6b,$58,$d3,$97

	db $e0,$e0,$d3,$97 ; blank 36
	db $6b,$68,$d3,$97
	db $6b,$70,$d3,$97
	db $6b,$78,$d3,$97

	db $e0,$e0,$d3,$97 ; blank 36
	db $6b,$88,$d3,$97
	db $6b,$90,$d3,$97
	db $6b,$98,$d3,$97

seq_cursor_pos:

	db $e0,$e0,$d3,$17 ; blank 36
	db $e0,$e0,$d3,$17
	db $23,$28,$d3,$17
	db $23,$30,$d3,$17

menu_cursor_pos:

	db $e0,$10,$d3,$97
	db $e0,$18,$d3,$97
	db $e0,$20,$d3,$97
	db $e0,$28,$d3,$97
	db $e0,$30,$d3,$97
	db $e0,$38,$d3,$97
	db $e0,$40,$d3,$97
	db $e0,$48,$d3,$97
	db $e0,$50,$d3,$97
	db $e0,$58,$d3,$97

;***************************************************************************

	end

	; this method halves the brightness

	inc hl
	ld b,0
	ld a,(hld)
	sra a
	rr b
	ld a,(hli)
	sra a
	and %111_01111
	or b
	ld ($ff6b),a
	ld a,(hld)
	sra a
	and %0_01111_01
	ld ($ff6b),a

	ld de,cursor_colour_buffer+1
	ld c,$08
_buffer_loop:
	LD B,B
	inc hl
	ld b,0
	ld a,(hld)
	sra a
	rr b
	and %0_01111_01
	ld (de),a
	dec de
	ld a,(hli)
	sra a
	and %111_01111
	or b
	ld (de),a
	INC HL
	inc de
	inc de
	inc de
	dec c
	jr nz,_buffer_loop

	ret







	; this method sucks

	ld a,(cursor_colour+2)
	inc a
	ld (cursor_colour+2),a

	ld d,a

	ld a,(cursor_colour+1)
	add a,2
	ld (cursor_colour+1),a

	ld e,a
	ld b,4

_bounce_loop:
	ld a,d
	add a,1
	ld d,a

	ld h,>cursor_palette
	add a,<cursor_palette
	ld l,a

	ld a,e
	add a,$10
	ld e,a

	push de
	push hl

	ld h,>plasma_sinus
	add a,<plasma_sinus
	ld l,a
	
	ld a,(hl)
	sra a
	sra a
	and $0e

	ld h,>cursor_colour_buffer
	add a,<cursor_colour_buffer
	ld l,a

	pop de
	
	ld a,(de)
	ld (hli),a
	inc de
	ld a,(de)
	ld (hl),a

	pop de

	dec b
	jr nz,_bounce_loop


	ret
