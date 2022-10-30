text_table_pos		equ text_ram+$00	; $02 bytes
text_font		equ text_ram+$02	; $02 bytes
text_position		equ text_ram+$04	; $02 bytes
text_plot_x		equ text_ram+$06	; $02 bytes
text_plot_y		equ text_ram+$08	; $02 bytes
text_plot_y_t		equ text_ram+$0a	; $02 bytes
text_temp		equ text_ram+$0c	; $01 bytes
text_y_size		equ text_ram+$0d	; $01 bytes

;***************************************************************************
;---------------------------------------------------------------------------
; setup text
; call with address of text in DE
; and address to call on A/START in HL
; and address to call on B in BC
;---------------------------------------------------------------------------
;***************************************************************************

setup_text:

	ld a,text_display_mode
	ld (editor_mode),a

	ld a,l
	ld (text_table_pos),a
	ld a,h
	ld (text_table_pos+1),a

	ld a,textbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,(hli)
	ld h,(hl)
	ld l,a

	ld a,h
	cp $ff
	jp z,setup_help_menu

	push hl

	xor a
	ld ($ff4f),a
	ld de,$9800
	ld hl,$9000
	call wipe_vram

	ld a,%11001011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8800
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

	call palette_fix_last_entry
	call set_menu_bar

	ld a,(gbc)
	and a
	jr z,_dmg_setup

	ld hl,$9d21
	ld de,text_screen_gbc_map
	ld bc,$0f12
	ld a,mapbank00
	call scr_copy_to_vram

	ld hl,$9cc0
	ld de,menu_header_gbc_map
	ld bc,$0214
	ld a,mapbank00
	call scr_copy_to_vram

	pop de
	jr render_text

_dmg_setup:

	ld hl,$9d21
	ld de,text_screen_dmg_map
	ld bc,$0f12
	ld a,mapbank00
	call scr_copy_to_vram_dmg

	ld hl,$9cc0
	ld de,menu_header_dmg_map
	ld bc,$0214
	ld a,mapbank00
	call scr_copy_to_vram_dmg

	pop de

;***************************************************************************
;---------------------------------------------------------------------------
; text renderer routines
;---------------------------------------------------------------------------
;***************************************************************************

render_text:

	ld a,textbank00

;draw_text_from_de:

	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,e
	ld (text_position),a
	ld a,d
	ld (text_position+1),a

	xor a			; clear variables
	ld (text_plot_x),a	; x pixel column
	ld (text_plot_x+1),a	; x tile column
	ld (text_plot_y),a	; y pixel row
	ld (text_plot_y+1),a	; y tile row
	ld (text_font),a	; font
	ld (text_font+1),a	; font
	ld ($ff4f),a		; set vram bank 0

	ld a,6
	ld (text_y_size),a

_draw_text_loop:

	ld a,(text_plot_y)
	ld (text_plot_y_t),a	; y pixel row
	ld a,(text_plot_y+1)
	ld (text_plot_y_t+1),a	; y tile row

	; ok, it is now time to generate the destination address

	ld a,(text_position)		; update text position
	ld l,a
	ld a,(text_position+1)
	ld h,a
_get_next_char:
	ld a,(hli)			; get current character
	cp text_eof			; check for end of file
	jr z,_do_eof
	cp text_cr			; check for carriage return
	jr z,_do_cr
	cp text_set_font		; check for font change
	jr z,_do_font
	cp text_set_y_size		; check for font change
	jr z,_do_y_size
	jr _not_special

_do_y_size:

	ld a,(hli)
	ld (text_y_size),a
	jr _inc_tp

_do_font:
	ld a,(hli)
	ld d,a
	ld e,0
	srl d
	rr e
	srl d
	rr e
	ld a,e
	ld (text_font),a		; store font index
	ld a,d
	ld (text_font+1),a

_inc_tp:
	ld a,l
	ld (text_position),a
	ld a,h
	ld (text_position+1),a

	jr _get_next_char
_do_cr:
	ld a,$e0
	jr _store_eof
_do_eof:
	ld a,$60
_store_eof:
	ld (text_temp),a
	ld de,small_cset_tileset+$100
	jr _cr_not_done	; in which case we keep drawing untill done

_not_special:

	cp $20
	jr z,_space_master
	and $3f
	ld e,a				; store it for later use
	ld a,(text_font)
	or e
_space_master:	
	ld e,a				; do bold toggle
	ld (text_temp),a		; yessiree

	ld a,l
	ld (text_position),a
	ld a,h
	ld (text_position+1),a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


_not_done_drawing_text:

	ld a,(text_font+1)
	ld d,a

	sla e			; *8 so that it is an index to cset
	rl d
	sla e
	rl d
	sla e
	rl d

	ld hl,small_cset_tileset
	add hl,de
	ld e,l
	ld d,h

_cr_not_done:

	ld a,(text_y_size)	; get the size of the text
	cp 9
	jr c,_use_it
	ld a,8
_use_it:
	ld c,a
	ld a,(text_plot_x)	; get the x pixel column
	cp 1
	jr z,_one_alligned
	cp 2
	jr z,_two_alligned
	cp 3
	jr z,_three_alligned
	cp 4
	jr z,_four_alligned
	cp 5
	jp z,_five_alligned
	cp 6
	jp z,_six_alligned
	cp 7
	jp z,_seven_alligned

	; then it must be 0

;000000000000000000000000000000000000
_zero_alligned:

	call get_text_dest_addr
	call text_inc_y_position
	jr nz,_zero_alligned
	jp _draw_text_loop_inc_pos

;1111111111111111111111111111111111111111
_one_alligned:

	call get_text_dest_addr
	srl a

	call text_inc_x_position
	rrca
	and %10000000

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_one_alligned
	jp _draw_text_loop_inc_pos

;22222222222222222222222222222222222222222
_two_alligned:

	call get_text_dest_addr
	srl a
	srl a

	call text_inc_x_position
	rrca
	rrca
	and %11000000

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_two_alligned
	jp _draw_text_loop_inc_pos

;3333333333333333333333333333333333333333
_three_alligned:

	call get_text_dest_addr
	srl a
	srl a
	srl a

	call text_inc_x_position
	rrca
	rrca
	rrca
	and %11100000

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_three_alligned
	jr _draw_text_loop_inc_pos

;444444444444444444444444444444444	
_four_alligned:

	call get_text_dest_addr
	swap a
	and %00001111

	call text_inc_x_position
	swap a
	and %11110000

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_four_alligned
	jr _draw_text_loop_inc_pos

;5555555555555555555555555555555555555
_five_alligned:

	call get_text_dest_addr
	rlca
	rlca
	rlca
	and %00000111

	call text_inc_x_position
	sla a
	sla a
	sla a

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_five_alligned
	jr _draw_text_loop_inc_pos

;666666666666666666666666666666666
_six_alligned:

	call get_text_dest_addr
	rlca
	rlca
	and %00000011

	call text_inc_x_position
	sla a
	sla a

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_six_alligned
	jr _draw_text_loop_inc_pos

;7777777777777777777777777777777777
_seven_alligned:

	call get_text_dest_addr
	rlca
	and %00000001

	call text_inc_x_position
	sla a

	push bc
	ld bc,$0010
	add hl,bc
	pop bc
	call text_inc_y_position
	jr nz,_seven_alligned
;	jr _draw_text_loop_inc_pos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_draw_text_loop_inc_pos:

	; first clear any unused area

;	ld a,(text_y_size)	; get the size of the text
;	sub 8
;	jr c,_dont_clear
;	jr z,_dont_clear
;	ld c,a
;_clear_loop:
;	call get_text_dest_addr
;	xor a
;	call text_inc_y_position
;	jr nz,_clear_loop
;_dont_clear:

	ld a,(text_temp)
	ld e,a
	ld b,e				; store for cr check
	ld a,(text_font+1)
	ld d,a
	ld hl,small_cset_tileset_widths
	add hl,de

	ld a,(text_plot_x)	; get the x pixel column
	add a,(hl)		; add in the width
	ld (text_plot_x),a
_xpos_update_loop:
	sub 8			; update for >8 carry
	jr c,_draw_text_next_2	; check for >8 carry

	ld (text_plot_x),a
	ld a,(text_plot_x+1)	; update tile column
	inc a
	ld (text_plot_x+1),a
	ld a,(text_plot_x)
	jr _xpos_update_loop

_draw_text_next_2:

	ld a,(text_position)		; update text position
	ld l,a
	ld a,(text_position+1)
	ld h,a

	ld a,(text_font+1)
	ld d,a
	ld a,(text_font)
	ld e,a

_loop:
	ld a,(hli)
	cp text_eof
	jr z,_do_eof_and_cr_check
	cp text_cr
	jr nz,_dont_test_this_line_yet
_do_eof_and_cr_check:
	ld a,(text_plot_x+1)
	jr _test_this_line
_dont_test_this_line_yet:
	cp text_set_font		; check for font change
	jr nz,_dont_do_font
	ld a,(hli)
	ld d,a
	ld e,0
	srl d
	rr e
	srl d
	rr e
	jr _loop
_dont_do_font:
	cp text_set_y_size
	jr nz,_done_loop
	inc hl
	jr _loop

_done_loop:
	and $3f
	or e
	ld e,a
	ld hl,small_cset_tileset_widths
	add hl,de

	ld a,(text_plot_x+1)
	ld e,a

	ld a,(text_plot_x)	; get the x pixel column
	add a,(hl)		; add in the width
;	dec a
;	sub $8			; update for >8 carry
	sub $a
	jp c,_draw_text_looper	; check for >8 carry

_check_loop:
	inc e
	sub 8
	jr nc,_check_loop

_draw_text_looper:

	ld a,e
_test_this_line:
	cp 18
	jp c,_draw_text_loop

_force_next_line:

	; wrap
	
	xor a			; clear x tile column
	ld (text_plot_x+1),a
	ld (text_plot_x),a

	ld a,(text_plot_y)	; increment y pixel column
	ld l,a
	ld a,(text_y_size)
	add a,l
	ld (text_plot_y),a	; increment y pixel column
_y_size_loop:
	sub 8
	jr c,_fine_y_is_fine
	ld (text_plot_y),a	; increment y pixel column

	ld a,(text_plot_y+1)
	cp 9
	ret nc ; if it is 10, we are done.  That's 11 lines

	inc a
	ld (text_plot_y+1),a
	ld a,(text_plot_y)	; increment y pixel column
	jr _y_size_loop

_fine_y_is_fine:

	ld a,b
	cp $e0				; check for carriage return
	jp nz,_draw_text_loop

	ld a,(text_position)		; update text position
	ld c,a
	ld a,(text_position+1)
	ld b,a
	inc bc
	ld a,c
	ld (text_position),a		; update text position
	ld a,b
	ld (text_position+1),a

	jp _draw_text_loop

	ret

;***************************************************************************

text_inc_y_position:

	ld b,a

_t_2:
	ld a,($ff41)
	and 2
	jr nz,_t_2

	ld a,b
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_t_2

	ld a,(text_plot_y_t)
	inc a
	cp 8
	jr nz,_no_coarse_inc
	ld a,(text_plot_y_t+1)
	inc a
	ld (text_plot_y_t+1),a
	xor a
_no_coarse_inc:
	ld (text_plot_y_t),a
	inc de
	dec c
	ret

;***************************************************************************

text_inc_x_position:

	ld b,a

_f_1:
	ld a,($ff41)
	and 2
	jr nz,_f_1

	ld a,(hl)
	or b
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_f_1

	ld a,(de)
	ret

;***************************************************************************

get_text_dest_addr:

	push bc
	ld h,$90
	ld bc,$0140

	; fine y

	ld a,(text_plot_y_t)
	sla a
	ld l,a

	; coarse y

	ld a,(text_plot_y_t+1)
	cp 6
	jr c,_no_carry_y_pos
	inc l
	sub 6
_no_carry_y_pos:
	and a
	jr z,_done_carry_y_pos
_no_carry_y_pos_loop:
	add hl,bc
	dec a
	jr nz,_no_carry_y_pos_loop
_done_carry_y_pos:

	; coarse x

	ld bc,$0010
	ld a,(text_plot_x+1)
	and a
	jr z,_done_x_pos
_no_carry_x_pos_loop:
	add hl,bc
	dec a
	jr nz,_no_carry_x_pos_loop

_done_x_pos:
	pop bc
	ld a,(de)
	ret

;***************************************************************************
;---------------------------------------------------------------------------
; text vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

text_vbl_irq:

	ld a,$fe
	ld ($ff68),a
	ld hl,colour_table+$06
	ld a,(hli)
	ld ($ff69),a
	ld a,(hl)
	ld ($ff69),a

	ei

	call editor_calc_waves
	jp done_vbl

;***************************************************************************
;---------------------------------------------------------------------------
; text poop loop
;---------------------------------------------------------------------------
;***************************************************************************

text_poop_loop:

	call update_waveforms
	call joy_update

	ld hl,poop
	push hl

	ld a,(joy_pressed)
	and joy_a+joy_start+joy_b+joy_left+joy_right
	ret z

	and joy_b+joy_left

	ld a,(text_table_pos)
	ld l,a
	ld a,(text_table_pos+1)
	ld h,a

	jr z,_came_from

	dec hl
	dec hl

	jp setup_text

_came_from:

	inc hl
	inc hl

	jp setup_text

;---------------------------------------------------------------------------
;***************************************************************************

;***************************************************************************
;---------------------------------------------------------------------------
; text mode lcdc irq
;---------------------------------------------------------------------------
;***************************************************************************

;---------------------------------------------------------------------------
; main lcdc irq controller

text_lcdc_irq:

	ld a,($ff44)
	cp $8d
	jp nc,c_wait_6
	cp $74
	jp nc,c_wait_5
	cp $40
	jr nc,_wait_4
	cp $32
	jr nc,_wait_3
	cp $24
	jr nc,_wait_2
	cp $02
	jp c,c_wait_1
	jp logo_lcdc_irq

;---------------------------------------------------------------------------
; switch from top window to main edit and fade in edit text

_wait_2:
	ld a,($ff44)
	cp $2f
	jr c,_wait_2

	ld a,2
	ld ($ff42),a

	ld a,$35-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_3:
	push bc
	push de
	ld hl,colour_table+$04
	ld a,(hli)
	ld c,a
	ld a,(hli)
	ld b,a
	ld a,(hli)
	ld e,a
	ld a,(hl)
	ld d,a

_wait_03:
	ld a,($ff44)
	cp $35
	jr c,_wait_03

	ld a,$fa
	ld ($ff68),a
	ld a,e
	ld ($ff69),a
	ld a,d
	ld ($ff69),a
	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a

	ld a,$1c-1
	ld ($ff42),a

	ld a,$65-1
	ld ($ff45),a

	pop de
	pop bc
	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_4:

	push bc
	push de
	ld hl,colour_table+$04
	ld a,(hli)
	ld c,a
	ld a,(hli)
	ld b,a
	ld a,(hli)
	ld e,a
	ld a,(hl)
	ld d,a

_wait_04:
	ld a,($ff44)
	cp $65
	jr c,_wait_04

	ld a,$fa
	ld ($ff68),a
	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a
	ld a,e
	ld ($ff69),a
	ld a,d
	ld ($ff69),a

	ld a,$1c+4+4-1
	ld ($ff42),a

	ld a,$8b-1
	ld ($ff45),a

	pop de
	pop bc
	pop hl
	pop af
	reti

;***************************************************************************
;---------------------------------------------------------------------------
; text mode lcdc dmg
;---------------------------------------------------------------------------
;***************************************************************************

;---------------------------------------------------------------------------
; main lcdc irq controller

text_lcdc_dmg:

	ld a,($ff44)
	cp $8d
	jp nc,d_wait_6
	cp $74
	jp nc,d_wait_5
	cp $40
	jr nc,_wait_4
	cp $34
	jr nc,_wait_3
	cp $33-1
	jr nc,_wait_2q
	cp $24
	jr nc,_wait_2
	cp $02
	jp c,d_wait_1
	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; switch from top window to main edit and fade in edit text

_wait_2:
	ld a,($ff44)
	cp $30
	jr c,_wait_2

	ld a,2
	ld ($ff42),a

	ld a,$33-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_2q:
	ld a,($ff44)
	cp $33
	jr c,_wait_2q

	ld a,2
	ld ($ff42),a

	ld a,$35-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_3:
	ld a,($ff44)
	cp $35
	jr c,_wait_3

;	ld a,%00000101			;"normal" colours
	ld a,%00110011			;"normal" colours
	ld ($ff47),a			; dmg bg palette

	ld a,$1c-1
	ld ($ff42),a

	ld a,$65-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_4:
	ld a,($ff44)
	cp $65
	jr c,_wait_4

	ld a,%00001111			;"normal" colours
;	ld a,%00010001			;"normal" colours
	ld ($ff47),a			; dmg bg palette

	ld a,$1c+4+4-1
	ld ($ff42),a

	ld a,$8b-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;***************************************************************************

	end
