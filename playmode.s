
player_start		equ play_mode_ram+$00	; $01 bytes
player_max		equ play_mode_ram+$01	; $01 bytes

editor_pat_counter	equ play_mode_ram+$02	; $02 bytes
;poopeor			equ play_mode_ram+$04	; $01 bytes

;---------------------------------------------------------------------------
; bugs:
;-note off does not cut channel waveform display properly
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;***************************************************************************
;---------------------------------------------------------------------------
; setup play mode
;---------------------------------------------------------------------------
;***************************************************************************

setup_play_mode_looping:
;	ld a,(mus_song_pat_counter)
;	ld (editor_pat_counter),a
;	ld a,(mus_song_pat_counter+1)
;	ld (editor_pat_counter+1),a
	ld a,1
	ld (editor_looping_mode),a
	call play_select_pattern
	jr setup_play_mode_cont
setup_play_mode_start:
	xor a
	ld (editor_looping_mode),a
	call music_reset
	call editor_init_block_buffer
	jr setup_play_mode_cont
setup_play_mode:
	ld a,1
	ld (editor_looping_mode),a
	call play_select_pattern
	xor a
	ld (editor_looping_mode),a
setup_play_mode_cont:

	call reset_pat_bar
	call clear_cursor
_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,$10
	ld ($fe61),a
	ld ($fe65),a
	ld ($fe69),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

_z_0a:
	ld a,($ff41)
	and 2
	jr nz,_z_0a

	ld a,$87
	ld ($fe63),a
	dec a
	ld ($fe67),a
	inc a
	ld ($fe6b),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	xor a
	ld (editor_line),a

	; clear all macros
;	ld (mus_track_1+o_macro_return),a
;	ld (mus_track_1+o_macro_return+1),a
;	ld (mus_track_2+o_macro_return),a
;	ld (mus_track_2+o_macro_return+1),a
;	ld (mus_track_3+o_macro_return),a
;	ld (mus_track_3+o_macro_return+1),a
;	ld (mus_track_4+o_macro_return),a
;	ld (mus_track_4+o_macro_return+1),a
;
;	; clear all wait_lines
;	ld (mus_track_1+o_wait_lines),a
;	ld (mus_track_2+o_wait_lines),a
;	ld (mus_track_3+o_wait_lines),a
;	ld (mus_track_4+o_wait_lines),a
;
	inc a
	ld (editor_redraw),a
;	ld a,(editor_sequence_1)
;	ld (mus_track_1+o_seq_counter),a
;	ld a,(editor_sequence_1+1)
;	ld (mus_track_1+o_seq_counter+1),a
;	ld a,(editor_sequence_2)
;	ld (mus_track_2+o_seq_counter),a
;	ld a,(editor_sequence_2+1)
;	ld (mus_track_2+o_seq_counter+1),a
;	ld a,(editor_sequence_3)
;	ld (mus_track_3+o_seq_counter),a
;	ld a,(editor_sequence_3+1)
;	ld (mus_track_3+o_seq_counter+1),a
;	ld a,(editor_sequence_4)
;	ld (mus_track_4+o_seq_counter),a
;	ld a,(editor_sequence_4+1)
;	ld (mus_track_4+o_seq_counter+1),a

	ld a,play_mode
	ld (editor_mode),a

	ld a,(gbc)
	and a
	jr z,_dmg_setup

	ld hl,$9cc0
	ld de,pattern_header_gbc_map
	ld bc,$0214
	ld a,mapbank00
	jp scr_copy_to_vram

_dmg_setup:

	ld hl,$9cc0
	ld de,pattern_header_dmg_map
	ld bc,$0214
	ld a,mapbank00
	jp scr_copy_to_vram_dmg

;***************************************************************************
;---------------------------------------------------------------------------
; play mode select pattern
;---------------------------------------------------------------------------
;***************************************************************************

play_select_pattern:

	call get_seq_position

	; now play from here

	ld a,e
	ld (mus_song_pat_counter),a
	ld (editor_pat_counter),a
	ld a,d
	ld (mus_song_pat_counter+1),a
	ld (editor_pat_counter+1),a
	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a
	jp music_advance_sequence

;***************************************************************************
;---------------------------------------------------------------------------
; play mode realtime loop
;---------------------------------------------------------------------------
;***************************************************************************

play_mode_poop_loop:

;	halt
;	nop

;	call editor_draw_block_buffer
	call editor_draw_block_line

;---------------------------------------------------------------------------

	ld a,(file_info_win_flag) ; only do it if we have to
	and a
	call z,update_waveforms

	call joy_update

	ld a,(joy_held)
	bit joy_bit_down,a
	jr z,_not_select

	ld a,1
	ld (fast_forward),a

	ld a,(editor_looping_mode)
	and a
	jr z,_not_looping
	ld a,(editor_pat_counter)
	ld (mus_song_pat_counter),a
	ld a,(editor_pat_counter+1)
	ld (mus_song_pat_counter+1),a
_not_looping:

	call music_player
	call music_player
	call music_player
	jp poop

;---------------------------------------------------------------------------

_not_select:
	xor a
	ld (fast_forward),a

	ld a,(joy_pressed)
	bit joy_bit_start,a
	jr z,_not_start

	ld a,(main_menu_came_from)
	cp edit_sequence_mode
	jr nz,_not_setup_edit_seq

	call setup_edit_seq
	jr _wipe_spr

_not_setup_edit_seq:
	call setup_edit_patt
_wipe_spr:
	ld a,$e0
	ld ($fe60),a
	ld ($fe64),a
	ld ($fe68),a

	jp poop

_not_start:

;---------------------------------------------------------------------------

	ld a,(joy_pressed)
	bit joy_bit_a,a
	jr z,_not_a

_reset_song:
	call setup_play_mode_start
	jp poop

_not_a:

;---------------------------------------------------------------------------

	ld a,(joy_pressed)
	bit joy_bit_b,a
	jr z,_not_b

	call setup_play_mode_looping
_not_b:
	ld a,(joy_pressed)
	bit joy_bit_select,a
	call nz,setup_main_menu

	jp poop

;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; play mode vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

play_mode_vbl_irq:

;	call buffer_cursor_colours

	ei

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
; play music

	ld a,(fast_forward)
	and a
	jr nz,_donot_play

; this makes the song play at half-speed
;
;	ld a,(poopeor)
;	and $01
;	xor $01
;	ld (poopeor),a
;	jr z,_poopeop_poop

	ld a,(editor_looping_mode)
	and a
	jr z,_not_looping
	ld a,(editor_pat_counter)
	ld (mus_song_pat_counter),a
	ld a,(editor_pat_counter+1)
	ld (mus_song_pat_counter+1),a
_not_looping:


	ld a,($ff44)
	cp $90
	jr nc,_no_aug
	add a,$a0
_no_aug:
	ld (player_start),a

	call music_player
	ld a,(player_start)
	ld c,a
	ld a,($ff44)
	cp $90
	jr nc,_no_aug_2
	add a,$a0
_no_aug_2:
	sub c
	ld c,a
	add a,$2b+$18
;	add a,$2b+$1e
	ld b,a
	ld a,(player_max)
	sla c
	dec a
	cp c
	jr nc,_not_new
	ld a,c
_not_new:
	ld (player_max),a
	srl a
	add a,$2b+$18
	ld d,a
_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,$2b+$18
	ld ($fe68),a
	ld a,b
	ld ($fe64),a
	ld a,d
	ld ($fe60),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

;_poopeop_poop:

_donot_play:

	call editor_calc_waves

	jp done_vbl

;***************************************************************************
;---------------------------------------------------------------------------
; play mode lcdc irq
;---------------------------------------------------------------------------
;***************************************************************************

play_mode_lcdc_shit:

;---------------------------------------------------------------------------
; draw bottom of window

_wait_5:		

	ld a,%11011011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

	ld a,$b0
	ld ($ff42),a

	ld a,$90-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; main lcdc irq controller

_play_mode_lcdc_irq:

	ld a,($ff44)
	cp $8d
	jp nc,c_wait_6
	cp $74
	jr nc,_wait_5
;	cp $64-6
;	jr nc,_wait_4
	cp $54-6
	jp nc,_wait_3
	cp $32-1
	jp nc,_wait_2q
	cp $24
	jp nc,_wait_2
	cp $02
	jp c,c_wait_1

;---------------------------------------------------------------------------
; logo raster bars
_logo_raster_bars:

	push bc

	ld hl,raster_bars
	ld bc,$080c
	ld a,%10000010
	ld ($ff68),a
_raster_1:
	ld a,($ff44)
	cp c
	jr c,_raster_1

	ld a,(hli)
	ld ($ff69),a
	ld a,(hli)
	ld ($ff69),a

	ld a,%10000010
	ld ($ff68),a

	inc c
	dec b
	jr nz,_raster_1

	ld a,%10111110
	ld ($ff6a),a

	ld hl,colour_table+$0c
	ld a,(hli)
	ld b,(hl)
	ld c,a

	ld hl,cursor_colour_buffer+2
	ld a,(hli)
	ld h,(hl)
	ld l,a

_raster_2:
	ld a,($ff44)
	cp $0c+$08
	jr c,_raster_2

	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a
	ld a,l
	ld ($ff6b),a
	ld a,h
	ld ($ff6b),a

	ld a,%10000000
	ld ($ff68),a

	ld a,%10111110
	ld ($ff6a),a

	ld hl,colour_table+$08
	ld a,(hli)
	ld b,(hl)
	ld c,a

	ld hl,cursor_colour_buffer+4
	ld a,(hli)
	ld h,(hl)
	ld l,a

_raster_3:
	ld a,($ff44)
	cp $0c+$09
	jr c,_raster_3

	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a
	ld a,l
	ld ($ff6b),a
	ld a,h
	ld ($ff6b),a

	ld c,$0c+$0a
	ld b,5

	ld hl,cursor_colour_buffer+6

_wait_23d:
	ld a,%10111110
	ld ($ff6a),a

_wait_23q:
	ld a,($ff44)
	cp c
	jr c,_wait_23q

	ld a,(hli)
	ld ($ff6b),a
	ld a,(hli)
	ld ($ff6b),a

	inc c
	dec b

	jr nz,_wait_23d






	ld a,$2f-1
	ld ($ff45),a

	pop bc
	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; switch from top window to main edit

_wait_2:
	ld a,($ff44)
	cp $2f
	jr c,_wait_2

	ld a,2
	ld ($ff42),a

	ld a,$32
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_2q:
	ld a,($ff44)
	cp $33
	jr c,_wait_2q

	ld a,%11010011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9800
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

	ld a,(editor_ff42)
	ld ($ff42),a

	ld a,$5b-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; turn on edit bar

_wait_3:
	push bc

	ld a,%10111000
	ld ($ff68),a
	ld a,%10111110
	ld ($ff6a),a

	ld hl,colour_table+$0e
	ld a,(hli)
	ld b,(hl)
	ld c,a

	ld hl,cursor_colour_buffer
	ld a,(hli)
	ld h,(hl)
	ld l,a

_wait_03:
	ld a,($ff44)
	cp $5b
	jr c,_wait_03

	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a
	ld a,l
	ld ($ff6b),a
	ld a,h
	ld ($ff6b),a

	ld a,%10111000
	ld ($ff68),a
	ld a,%10111110
	ld ($ff6a),a

	ld hl,colour_table+$10
	ld a,(hli)
	ld b,(hl)
	ld c,a

	ld hl,cursor_colour_buffer+2
	ld a,(hli)
	ld h,(hl)
	ld l,a

_wait_03a:
	ld a,($ff44)
	cp $5c
	jr c,_wait_03a

	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a
	ld a,l
	ld ($ff6b),a
	ld a,h
	ld ($ff6b),a

	ld c,$5d
	ld b,5

	ld hl,cursor_colour_buffer+4

_wait_3d:
	ld a,%10111110
	ld ($ff6a),a

_wait_3q:
	ld a,($ff44)
	cp c
	jr c,_wait_3q

	ld a,(hli)
	ld ($ff6b),a
	ld a,(hli)
	ld ($ff6b),a

	inc c
	dec b

	jr nz,_wait_3d

	ld a,%10111000
	ld ($ff68),a
	ld a,%10111110
	ld ($ff6a),a

	ld hl,colour_table+$12
	ld a,(hli)
	ld b,(hl)
	ld c,a

	ld hl,cursor_colour_buffer+14
	ld a,(hli)
	ld h,(hl)
	ld l,a

_wait_04b:
	ld a,($ff44)
	cp $62
	jr c,_wait_04b

	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a
	ld a,l
	ld ($ff6b),a
	ld a,h
	ld ($ff6b),a

	ld a,%10111000
	ld ($ff68),a

	ld hl,colour_table+$04
	ld a,(hli)
	ld b,(hl)
	ld c,a

_wait_04a:
	ld a,($ff44)
	cp $63
	jr c,_wait_04a

	ld a,c
	ld ($ff69),a
	ld a,b
	ld ($ff69),a

;	ld a,$83-1
	ld a,$83+8-1
	ld ($ff45),a

	pop bc
	pop hl
	pop af
	reti

;-------------------------------

play_mode_lcdc_irq equ _play_mode_lcdc_irq
logo_lcdc_irq	   equ _logo_raster_bars

;-------------------------------

c_wait_6:		
	ld a,($ff44)
	cp $90
	jr c,c_wait_6

	ld a,$2e
	ld ($ff42),a

;	ld a,$01-1
	xor a
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

c_wait_1:
	push bc
	ld c,$01

_wait_1:
	ld a,($ff44)
	cp c
	jr c,_wait_1

	ld a,$ff
	ld ($ff42),a

	ld a,%10000000
	ld ($ff68),a

	inc c

	ld hl,colour_table+$02
_raster_1:
	ld a,($ff44)
	cp c
	jr c,_raster_1

	ld a,(hli)
	ld ($ff69),a
	ld a,(hl)
	ld ($ff69),a

	ld a,$0c-1
	ld ($ff45),a

	pop bc
	pop hl
	pop af
	reti

;***************************************************************************
;---------------------------------------------------------------------------
; play mode lcdc dmg
;---------------------------------------------------------------------------
;***************************************************************************

play_mode_lcdc_dmg:

	ld a,($ff44)
	cp $8d
	jp nc,d_wait_6
	cp $74
	jp nc,_wait_5
	cp $64-6
	jr nc,_wait_4
	cp $54-6
	jr nc,_wait_3
	cp $32-1
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

	ld a,%11010011
	ld ($ff47),a

	ld a,$32-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_2q:
	ld a,($ff44)
	cp $32
	jr c,_wait_2q

	ld a,%11010011
	ld ($ff47),a

_wait_02r:
	ld a,($ff44)
	cp $34
	jr c,_wait_02r

	ld a,(editor_ff42)
	ld ($ff42),a

	ld a,%11010011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9800
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

	ld a,$5b-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; turn on edit bar

_wait_3:
	ld a,($ff44)
	cp $5b
	jr c,_wait_3

	ld a,%11010000
	ld ($ff47),a
	ld a,%01000010
	ld ($ff48),a

_wait_3a:
	ld a,($ff44)
	cp $5c
	jr c,_wait_3a

	ld a,%11100001
	ld ($ff47),a

	ld a,$62-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; turn off main edit bar

_wait_4:
	ld a,($ff44)
	cp $62
	jr c,_wait_4

	ld a,%11010000
	ld ($ff47),a

_wait_4a:
	ld a,($ff44)
	cp $63
	jr c,_wait_4a

	ld a,%11010011
	ld ($ff47),a

	ld a,%10010011			;"normal" colours
	ld ($ff48),a			; dmg obj 1 palette

	ld a,$8c-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; fade out colours at bottom and also draw bottom of window

_wait_5:		
	ld a,($ff44)
	cp $8c
	jr c,_wait_5

	ld a,%11011011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

	ld a,$b0
	ld ($ff42),a

	ld a,%11010011
	ld ($ff47),a

	ld a,$90-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;-------------------------------
	
d_wait_6:		
	ld a,($ff44)
	cp $90
	jr c,d_wait_6

	ld a,$2e
	ld ($ff42),a

	ld a,%11100100			;"normal" colours
	ld ($ff47),a			; dmg bg palette

;	ld a,$01-1
	xor a
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

d_wait_1:		
	ld a,($ff44)
	cp $01
	jr c,d_wait_1

	ld a,$ff
	ld ($ff42),a

	ld a,$30-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;***************************************************************************

raster_bars:
	;  %0_bbbbb_ggggg_rrrrr ; b,g,r	dw %0
	dw %0_11101_11101_11011
	dw %0_11100_11100_11000
	dw %0_10010_11110_10010
	dw %0_01011_11101_01110
;	dw %0_00100_11100_11011
	dw %0_01101_11001_11101
	dw %0_01101_01111_11110
	dw %0_00111_00101_11111
	dw %0_00001_00001_01111
;	dw %0_10010_10100_00011 ; 18,20,3
;	dw %0_00101_00110_00000

;***************************************************************************

	end
