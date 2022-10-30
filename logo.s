;**********************************************************************

setup_logo:

	; reset critical variables

	ld a,%10001111
	ld (plasma_ff40),a
	ld a,$98
	ld (plasma_vram),a
	xor a
	ld (plasma_sinpos),a
	ld (plasma_update),a

	call music_reset

	ld a,logo_mode
	ld (editor_mode),a

	; set colours for bw bg

	ld a,%11100100
	ld ($ff47),a

	; set bank for color gb

	xor a
	ld ($ff4f),a

	ld de,logo_map_dmg
	ld hl,$9800
	ld bc,$2020
	ld a,logobank00
	call scr_copy_to_vram_dmg

	ld de,logo_map_dmg
	ld hl,$9c00
	ld bc,$2020
	ld a,logobank00
	call scr_copy_to_vram_dmg

	ld de,logo_tileset
	ld hl,$9000
	ld bc,$4020
	ld a,logobank00
	call scr_copy_to_vram_dmg

	ld de,logo_tileset+$800
	ld hl,$8800
	ld bc,$3020
	ld a,logobank00
	call scr_copy_to_vram_dmg

	ld de,logo_sprites
	ld hl,$8000
	ld bc,$3020
	ld a,logobank00
	call scr_copy_to_vram_dmg

	; do palettes

	ld c,8

_z_0aa:
	ld a,c
	ld ($ff6a),a

_z_0a:
	ld a,($ff41)
	and 2
	jr nz,_z_0a
	
	ld a,$66
	sub c
	ld ($ff6b),a

	ld a,($ff41)
	and 2
	jr nz,_z_0a

	dec c
	jr nz,_z_0aa


	ld c,$00
	ld hl,logo_palette
	ld a,c
_pal_loop:
	ld ($ff68),a
_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,(hl)
	ld ($ff69),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	inc hl
	inc c
	ld a,c
	cp $40
	jr nz,_pal_loop	

	; do sprites

	ld hl,logo_sprite_table
	ld de,$fe00
	ld c,40*4

_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,(hl)
 	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_1

	inc hl
	inc e
	dec c
	jr nz,_z_1

	ret

;**********************************************************************

logo_vbl_irq:
	; epilepsy jitter to smooth it out

	ld a,(plasma_sinpos+0)
	ld l,a
	ld a,plasmabank00
	ld (current_rom_bank),a
	ld ($2666),a

	ld a,1
	ld ($ff4f),a

	ld h,>plasma_sinus_2
	ld a,(hl)
	add a,16
	ld ($ff42),a

	dec h
	ld a,(plasma_sinpos+3)
	ld l,a
	ld a,(hl)
	add a,18
	ld ($ff43),a

	; check to see if it is ready to display

	ld a,(plasma_update)
	and a
	jp nz,_done_logo_vbl

	ld a,(plasma_ff40)
	ld ($ff40),a
	xor a
	inc a
	ld (plasma_update),a

_done_logo_vbl:

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

logo_poop_loop:
	call joy_update
	ld a,(joy_pressed)
	and joy_b+joy_a+joy_start+joy_select
	jr z,_dont_exit

	ld a,%11011111	; LCD Controller = On
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

	xor a
	ld ($ff4f),a

	call cset_load

	ld hl,$8800
	ld de,main_tileset
	ld bc,$4020
	ld a,tilebank00 
	call scr_copy_to_vram_dmg

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
	ld a,%10001011
	ld ($ff40),a
	ei

	jp poop

_dont_exit:

; ------------- real plasma code

	ld a,plasmabank00
	ld (current_rom_bank),a
	ld ($2666),a

	ld a,1
	ld ($ff4f),a

; buffer x sinus stuff

	ld h,>plasma_sinus
	ld b,>plasma_sinus_2

	ld a,(plasma_sinpos+2)
	add a,3
	ld (plasma_sinpos+2),a
	ld l,a

	ld a,(plasma_sinpos+3)
	sub 2
	ld (plasma_sinpos+3),a
	ld c,a

	ld de,data_entry_work_ram

_buffer_loop:

	ld a,(bc)		; add 2 sinus values together
	add a,(hl)
	ld (de),a

	inc c
	inc c
	inc c
	inc l
	inc l
	inc l
	inc l
	inc de

	ld a,e
	cp <data_entry_work_ram+$20
	jr nz,_buffer_loop

; set up y sinus

	ld a,(plasma_sinpos+0)
	add a,5
	ld (plasma_sinpos+0),a

	ld l,a

	ld (plasma_sinpos+4),a
	ld a,(hl)
	ld c,a

	ld a,(plasma_sinpos+1)
	sub 3		; update y sin 1
	ld (plasma_sinpos+1),a

	ld l,a

	ld (plasma_sinpos+5),a
	ld a,(hl)
	add a,c
	ld c,a			; c is our first value

	ld a,(plasma_vram)
	ld d,a
	ld e,0			; our dest address
	ld b,32			; our y counter

	ld hl,data_entry_work_ram

; make sure it is not displaying this page before we draw it

_wait:
	ld a,(plasma_update)
	and a
	jr z,_wait

	ld a,c				; remember to get our start value

	jr _entry_point

_plasma_y_loop:

	ld a,(plasma_sinpos+5)		; update our sinus positions
	add a,4
	ld (plasma_sinpos+5),a

	ld a,(plasma_sinpos+4)
	sub 2
	ld (plasma_sinpos+4),a

	ld h,>plasma_sinus
	ld l,a
	ld a,(hl)
	ld c,a				; get first y value

	ld a,(plasma_sinpos+5)
	ld l,a
	inc h
	ld a,(hl)
	add a,c				; add it to second y value
	ld c,a				; store it for safe keeping

	ld hl,data_entry_work_ram	; address of already calculated x values

_entry_point:
_plasma_x_loop:

	push bc				; store bc for safekeeping so we can use it for math
					
	add a,(hl)			; add our y value to our x value

	swap a
	rra				; /32
	and $07				; 0-7
	ld c,a


_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,c
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	pop bc				; get our counter and y value back

	inc l				; increment x calced sinus table position
	inc de				; increment screen position
	ld a,l
	cp <data_entry_work_ram+$20 	; check for done line
	ld a,c				; get our c value
	jr nz,_plasma_x_loop		; loop

	dec b				; decrement our line counter
	jp nz,_plasma_y_loop		; loop

;-------- scroll palettes

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
