plasma_ff40 	equ plasma_ram+$00	; 1 byte
plasma_vram	equ plasma_ram+$02	; 1 byte
plasma_update	equ plasma_ram+$03	; 1 byte

plasma_sinpos 	equ plasma_ram+$04	; 6 bytes

plasma_zoompos	equ plasma_ram+$0a	; 1 byte
plasma_zoom	equ plasma_ram+$0b	; 1 byte
plasma_zoomfin	equ plasma_ram+$0c	; 1 byte

plasma_frac	equ plasma_ram+$0d	; 1 byte
plasma_x_carry  equ plasma_ram+$0e	; 1 byte
plasma_y_carry  equ plasma_ram+$0f	; 1 byte

plasma_x_buffer equ plasma_ram+$10	; 21 bytes

plasma_pal_pos	equ plasma_ram+$26	; 1 byte
plasma_pal_upd	equ plasma_ram+$27	; 1 byte

plasma_cset	equ plasma_ram+$28	; 1 byte
plasma_col_rat	equ plasma_ram+$29	; 1 byte
plasma_play_mus equ plasma_ram+$2a	; 1 byte

pal_bkg_buffer	equ plasma_ram+$2b	; $40 bytes
pal_src_buffer	equ plasma_ram+$6b	; $40 bytes
pal_red		equ plasma_ram+$ab	; $01 bytes
pal_green_low	equ plasma_ram+$ac	; $01 bytes
pal_green_high	equ plasma_ram+$ad	; $01 bytes
pal_blue	equ plasma_ram+$ae	; $01 bytes
pal_count	equ plasma_ram+$af	; $01 bytes

;**********************************************************************

setup_plasma:

	; reset critical variables

	ld a,%10001001
	ld (plasma_ff40),a
	ld a,$98
	ld (plasma_vram),a
	xor a
	ld (plasma_sinpos),a
	ld (plasma_update),a

	call music_reset

	; set plasma mode

	ld a,plasma_mode
	ld (editor_mode),a

	; set colours for bw bg

	ld a,%11100100
	ld ($ff47),a

	; set bank for color gb

	ld a,1
	ld ($ff4f),a

	; clear color data

	ld hl,$9800
	ld c,2
_z_0:
	ld a,($ff41)
	and c
	jr nz,_z_0

	xor a
	ld (hl),a

	ld a,($ff41)
	and c
	jr nz,_z_0

	inc hl
	ld a,h
	cp $a0
	jr nz,_z_0

	; reset variables

	xor a
	ld ($ff4f),a
	ld (fast_forward),a
	ld (editor_looping_mode),a
	ld ($ff42),a
	ld ($ff43),a
	inc a
	ld (plasma_pal_upd),a

	; load tileset; toggle which one to use	

	ld de,plasma_2_tileset

	ld a,(plasma_cset)
	and a
	jr nz,_gotitsucka

	ld de,plasma_tileset
_gotitsucka:
	ld hl,$9000
	ld bc,$2020
	ld a,plasmabank00
	call scr_copy_to_vram_dmg

	; do palettes

	ld a,(gbc)
	and a
	ret z

	call plasma_scroll_palettes
	ld b,$40
	ld hl,pal_src_buffer
	ld de,pal_bkg_buffer

_copy_palettes:
	ld a,(hli)
	ld (de),a
	inc de
	dec b
	jr nz,_copy_palettes

	ret

;**********************************************************************

plasma_vbl_irq:
	; epilepsy jitter to smooth it out

	ld a,($ff42)
	xor 4
	ld ($ff42),a
	ld ($ff43),a

	; check to see if it is ready to display

	ld a,(plasma_update)
	and a
	jp nz,_done_plasma_vbl

	; update palettes

	ld a,(gbc)
	and a
	call nz,palette_copy_from_buffer_bkg

	ld a,(plasma_ff40)
	ld ($ff40),a
	xor a
	inc a
	ld (plasma_update),a

_done_plasma_vbl:

	ld a,(plasma_play_mus)
	and a
	jp z,done_vbl

;	ld a,(poopeor)
;	and $01
;	xor $01
;	ld (poopeor),a
;	jr z,_poopeop_poop

	call music_player
;_poopeop_poop:
	jp done_vbl


;**********************************************************************

plasma_poop_loop:
	call joy_update
	ld a,(joy_pressed)
	and joy_b+joy_a+joy_start+joy_select
	jr z,_dont_exit

	ld a,%11011011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld (plasma_ff40),a
	ld ($ff40),a

	di
	call wait_vbl
	xor a
	ld ($ff40),a

	ld a,(gbc)
	and a
	call nz,screen_setup_gbc

	ld a,(gbc)
	and a
	call z,screen_setup_dmg

	call sprite_setup
	call redraw_song_info
	call setup_edit_patt

	xor a
	ld ($ff42),a
	ld ($ff43),a

	call wait_vbl
	ld a,%10001001
	ld ($ff40),a
	ei

	jp poop

_dont_exit:

; ------------- real plasma code

	ld a,plasmabank00
	ld (current_rom_bank),a
	ld ($2666),a

	xor a
	ld ($ff4f),a

; plasma zoomer update

	ld h,>plasma_zoomsin
	ld a,(plasma_zoomfin)
	add a,$40
	ld (plasma_zoomfin),a
	ld a,(plasma_zoompos)
	jr nc,_no_zoompos_carry
	inc a
	ld (plasma_zoompos),a
_no_zoompos_carry:
	ld l,a
	ld a,(hl)
	swap a
	ld l,a
	and $f0
	ld (plasma_frac),a
	ld (plasma_x_carry),a
	ld (plasma_y_carry),a
	ld a,l
	and $0f
	ld (plasma_zoom),a

	dec h
	dec h

; buffer x sinus stuff

	ld a,(plasma_sinpos+2)
	add a,3
	ld (plasma_sinpos+2),a
	ld l,a

	ld a,(plasma_sinpos+3)
	dec a		; update x sin 1
	ld (plasma_sinpos+3),a
	ld b,0
	ld c,a
	sla c
	rl b

	ld a,b
	add a,h
	ld b,a

	ld de,plasma_x_buffer

_buffer_loop:

	ld a,(bc)		; add 2 sinus values together
	add a,(hl)
	ld (de),a

	ld a,(plasma_zoom)	; take zoom into account when updating sinus positions
	ld d,a
	add a,c
	ld c,a
	jr nc,_no_carry_x
	ld a,b
	xor 1
	ld b,a
_no_carry_x:
	ld a,d
	add a,l
	ld l,a

	ld a,(plasma_frac)	; take fractional zoom into account
	ld d,a
	ld a,(plasma_x_carry)
	add a,d
	ld (plasma_x_carry),a
	jr nc,_no_x_carry
	inc l
	inc c
	jr nz,_no_x_carry
	ld a,b
	xor 1
	ld b,a
_no_x_carry:

	ld d,>plasma_x_buffer	; update destination and loop
	inc e
	ld a,e
	cp <plasma_x_buffer+21
	jr nz,_buffer_loop

; set up y sinus

	ld a,(plasma_sinpos+0)
	inc a		; update y sin 0
	ld (plasma_sinpos+0),a

	ld l,a

	ld (plasma_sinpos+4),a
	ld a,(hl)
	ld c,a

	ld a,(plasma_sinpos+1)
	add a,-5		; update y sin 1
	ld (plasma_sinpos+1),a

	ld l,a

	ld (plasma_sinpos+5),a
	ld a,(hl)
	add a,c
	ld c,a			; c is our first value

	ld a,(plasma_vram)
	ld d,a
	ld e,0			; our dest address
	ld b,19			; our y counter

	ld hl,plasma_x_buffer

; make sure it is not displaying this page before we draw it

_wait:
	ld a,(plasma_update)
	and a
	jr z,_wait

	ld a,c				; remember to get our start value

	jr _entry_point

_plasma_y_loop:

	ld a,(plasma_zoom)		; take zoom and fractional zoom into account
	ld l,a
	ld a,(plasma_frac)
	ld h,a
	ld a,(plasma_y_carry)
	add a,h
	ld (plasma_y_carry),a
	jr nc,_no_y_carry
	inc l
_no_y_carry:

	ld a,(plasma_sinpos+5)		; update our sinus positions
	add a,l
	ld (plasma_sinpos+5),a

	ld h,>plasma_sinus
	ld a,(plasma_sinpos+4)
	add a,l
	ld (plasma_sinpos+4),a
	ld l,a
	ld a,(hl)
	ld c,a				; get first y value

	ld a,(plasma_sinpos+5)
	ld l,a
	ld a,(hl)
	add a,c				; add it to second y value
	ld c,a				; store it for safe keeping

	ld hl,plasma_x_buffer		; address of already calculated x values

_entry_point:
_plasma_x_loop:

	push bc				; store bc for safekeeping so we can use it for math
					
	add a,(hl)			; add our y value to our x value
	ld b,a

	ld a,(gbc)			; check for gbc plasma or dmg plasma
	and a
	jr nz,_gbc

	ld a,b				; do dmg plasma
	srl a				; 0-128
	and 63				; now we have 2 cycles of tile numbers 0-63
	ld b,a
	jr _dmg				; put it onscreen

_gbc:
	ld a,b				; do our palette number
	swap a
	rra				; /32
	and $07				; 0-7
	ld c,a
	ld a,b				; and then get our tile number
	and 31
	ld b,a

	ld a,1				; put palette onscreen
	ld ($ff4f),a

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,c
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	xor a				; put tile onscreen
	ld ($ff4f),a

_dmg:
_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,b
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_1

	pop bc				; get our counter and y value back

	inc l				; increment x calced sinus table position
	inc e				; increment screen position
	ld a,l
	cp <plasma_x_buffer+21		; check for done line
	ld a,c				; get our c value
	jr nz,_plasma_x_loop		; loop

	ld hl,32-21			; add rest of line to screen position
	add hl,de
	ld e,l
	ld d,h
	dec b				; decrement our line counter
	jp nz,_plasma_y_loop		; loop

;-------- scroll palettes

	ld a,(gbc)
	and a
	call nz,plasma_scroll_palettes

	ld a,(plasma_ff40)		; toggle onscreen page
	xor %00001000
	ld (plasma_ff40),a
	ld a,(plasma_vram)		; toggle destination page
	xor %00000100
	ld (plasma_vram),a
	xor a				; set update flag
	ld (plasma_update),a

	jp poop

;**********************************************************************

palette_copy_from_buffer_bkg:

	ld hl,pal_bkg_buffer
	ld a,$80
	ld ($ff68),a
	ld c,$40
	ld de,$ff69

_pal_set:
	ld a,(hli)
	ld (de),a
	dec c
	jr nz,_pal_set

	ret

;**********************************************************************

plasma_scroll_palettes:

	ld a,(plasma_pal_upd)
	dec a
	ld (plasma_pal_upd),a

	jr nz,palette_morph_bkg
	ld a,(plasma_col_rat)
	ld (plasma_pal_upd),a

	ld a,(plasma_pal_pos)
	inc a
	ld (plasma_pal_pos),a

	and $7f

	ld c,a
	ld b,0
	sla c
	rl b

	ld hl,plasma_palette		; set bkg palette
	add hl,bc

	ld a,palettebank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld de,pal_src_buffer

	ld c,8
_pal_loop:
	ld a,(hli)	; 0-0-l
	ld (de),a
	inc de
	ld a,(hli)	; 0-0-h
	ld (de),a
	inc de
	ld a,(hli)	; 0-1-l
	ld (de),a
	inc de
	ld a,(hli)	; 0-1-h
	ld (de),a
	inc de
	ld a,(hli)	; 0-2-l
	ld (de),a
	inc de
	ld a,(hli)	; 0-2-h
	ld (de),a
	inc de
	ld a,(hli)	; 0-3-l
	ld (de),a
	inc de
	ld a,(hld)	; 0-3-h -
	ld (de),a
	inc de
	dec c
	jr nz,_pal_loop

;**********************************************************************

palette_morph_bkg:

	ld de,pal_src_buffer
	ld hl,pal_bkg_buffer		; source & destination buffer in de

	ld a,$20
	ld (pal_count),a

;---------------------------------------------------------------------------
palette_morph_loop:

	ld a,(de)			; get red from palette
	inc de
	ld c,a
	and %000_11111
	ld (pal_red),a

	ld a,c				; get green low from palette
	and %111_00000
	ld (pal_green_low),a

	ld a,(de)			; get green hi from palette
	inc de
	ld c,a
	and %0_00000_11
	ld (pal_green_high),a

	ld a,c				; get blue from palette
	and %0_11111_00
	ld (pal_blue),a

	ld a,(hli)			; get lowbyte into b
	ld b,a
	ld a,(hld)			; get highbyte into d
	ld d,a

;---------------------------------------------------------------------------
; red
	; check red

	ld a,b
	and %000_11111			; get red
	ld c,a				; put it in c
	ld a,(pal_red)			; compare it to palette
	cp c
	jr z,_red_done			; if it's equal, do nothing
	jr nc,_red_smaller		; check for smaller

	dec c				; do bigger
	ld a,c
	ld (pal_red),a			; store it
	jr _red_done

_red_smaller:
	inc c				; do smaller
	ld a,c
	ld (pal_red),a			; store it
_red_done:

;---------------------------------------------------------------------------
; green
	; check green hi

	ld a,d
	and %0_00000_11			; get green hi
	ld c,a				; put it in c
	ld a,(pal_green_high)		; compare it to palette
	cp c
	jr z,_green_hi_done		; if it's equal, do nothing
	jr nc,_green_hi_smaller		; check for smaller
	
	ld a,b				; decrease lowbyte
	and %111_00000
	sub %001_00000
	ld (pal_green_low),a
	jr nc,_no_green_low_carry	; check for carry
	dec c				; decrease hibyte

_no_green_low_carry:
	ld a,c
	ld (pal_green_high),a		; store it
	jr _green_done
	
_green_hi_smaller:

	ld a,b				; increase lowbyte
	and %111_00000
	add a,%001_00000
	ld (pal_green_low),a
	jr nc,_no_green_low_carry	; check for carry

	inc c				; increase hibyte
	ld a,c
	ld (pal_green_high),a		; store it
	jr _green_done

_green_hi_done:

	ld a,b				; check lowbyte
	and %111_00000			; get green low
	ld c,a				; put it in c
	ld a,(pal_green_low)		; compare it to palette
	cp c
	jr z,_green_done		; if it's equal, do nothing
	jr nc,_green_low_smaller	; check for smaller

	ld a,c				; decrease lowbyte
	sub %001_00000
	ld (pal_green_low),a		; store it
	jr _green_done
	
_green_low_smaller:
	ld a,c				; increase hibyte
	add a,%001_00000
	ld (pal_green_low),a		; store it

_green_done:

;---------------------------------------------------------------------------
; blue
	; check blue

	ld a,d
	and %0_11111_00			; get blue
	ld c,a				; put it in c
	ld a,(pal_blue)			; compare it to palette
	cp c
	jr z,_blue_done			; if it's equal, do nothing
	jr nc,_blue_smaller		; check for smaller

	ld a,c
	sub %0_00001_00			; do bigger
	ld (pal_blue),a			; store it
	jr _blue_done
_blue_smaller:
	ld a,c
	add a,%0_00001_00		; do smaller
	ld (pal_blue),a			; store it
_blue_done:

;---------------------------------------------------------------------------
; put them in bkg palette buffer

	ld d,>pal_bkg_buffer		; *****! note that bkg and obj must be in same
					; 256 byte block for this to work universally

	ld a,(pal_red)			; mix red and green
	ld c,a
	ld a,(pal_green_low)
	or c
	ld (hli),a			; store them

	ld a,(pal_blue)			; mix blue and green
	ld c,a
	ld a,(pal_green_high)
	or c
	ld (hli),a			; store them

;---------------------------------------------------------------------------
; loop
	ld a,(pal_count)
	dec a
	ld (pal_count),a

	jp nz,palette_morph_loop	; loop

;***************************************************************************
; plasma load
;***************************************************************************
 
plasma_load:

	xor a
	ld ($4666),a
	ld a,$0a
	ld ($1666),a

	ld hl,plasma_save_ram

	ld a,(hli)
	ld (plasma_cset),a
	ld a,(hli)
	ld (plasma_col_rat),a
	ld a,(hl)
	ld (plasma_play_mus),a

	xor a
	ld ($1666),a
	ret

;***************************************************************************
; plasma save
;***************************************************************************
 
plasma_save:

	xor a
	ld ($4666),a
	ld a,$0a
	ld ($1666),a

	ld hl,plasma_save_ram

	ld a,(plasma_cset)
	ld (hli),a
	ld a,(plasma_col_rat)
	ld (hli),a
	ld a,(plasma_play_mus)
	ld (hl),a

	xor a
	ld ($1666),a
	ret

;***************************************************************************

	ret
