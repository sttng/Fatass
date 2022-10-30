;---------------------------------------------------------------------------
; editor pattern routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_edit_patt:
; edit_patt_vbl_irq:
; edit_patt_poop_loop:
;---------------------------------------------------------------------------
; test_start_play_modes:
; test_move_around_pattern:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables
;
; track_incr_max:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------


;***************************************************************************
;---------------------------------------------------------------------------
; edit pattern setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_edit_patt:

	xor a
	ld ($ff25),a

	ld a,(current_rom_bank)
	push af

	ld a,edit_pattern_mode
	ld (editor_mode),a
	ld (main_menu_came_from),a

;	ld a,(editor_line)
;	sla a
;	sla a
;	sla a
;	add a,$9d+8
;	ld (editor_ff42),a

	call editor_draw_block_line_up

	call reset_pat_bar

	ld a,(gbc)
	and a
	jr z,_dmg_setup

	ld a,$10
	ld (joy_repeat_time_max),a
	ld a,$04
	ld (joy_repeat_time_min),a

	ld hl,$9cc0
	ld de,pattern_header_gbc_map
	ld bc,$0214
	ld a,mapbank00
	call scr_copy_to_vram

	pop af
	ld (current_rom_bank),a
	ld ($2666),a

	ret

_dmg_setup:

	ld a,$10/2
	ld (joy_repeat_time_max),a
	ld a,$04/2
	ld (joy_repeat_time_min),a

	ld hl,$9cc0
	ld de,pattern_header_dmg_map
	ld bc,$0214
	ld a,mapbank00
	call scr_copy_to_vram_dmg

	pop af
	ld (current_rom_bank),a
	ld ($2666),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; edit pattern vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

edit_patt_vbl_irq:

	call patt_cursor
	call editor_calc_waves

	ei

	jp done_vbl

;***************************************************************************
;---------------------------------------------------------------------------
; edit pattern poop loop
;---------------------------------------------------------------------------
;***************************************************************************

edit_patt_poop_loop:

	call update_waveforms
	call joy_repeat_update
;	call editor_draw_block_line_up ; SIMPLY FOR ERROR PREVENTION
					; THIS SHOULD BE UNECESSARY, BUT IT'S IN HERE
					; TO ENSURE THAT WHATEVER IS FUCKING UP THE
					; LINE YOU ARE EDITING DOES NOT HAPPEN

	ld a,(joy_pressed)

;---------------------------------------------------------------------------
; setect moves us around between different modes
;---------------------------------------------------------------------------

	bit joy_bit_select,a
	jr z,_not_select
	ld a,(joy_held)

	; a + select -> edit sequence mode

	bit joy_bit_a,a
	jr z,_not_sel_a
	call setup_edit_seq
	jp poop

_not_sel_a:

	; b + select -> edit effects mode

	bit joy_bit_b,a
	jr z,_not_sel_b
	call setup_edit_efx
	jp poop

_not_sel_b:

	; start + select -> not used

	; select on it's own -> main menu

	call setup_main_menu
	jp poop

_not_select:

;---------------------------------------------------------------------------
; start initiates different play modes
;---------------------------------------------------------------------------

	call test_start_play_modes

;---------------------------------------------------------------------------
; a+b + up/down lets us scroll through the sequence
; a+b + left/right lets us change patterns!!!!
;---------------------------------------------------------------------------

	ld a,(joy_held)
	and joy_a+joy_b
	cp joy_a+joy_b
	jr nz,_not_ab

	call test_edit_sequence_scroll
	call test_edit_sequence_data
	jp poop

;---------------------------------------------------------------------------

_not_ab:

;---------------------------------------------------------------------------
; a+up/down/left/right : main editor functions are done with a button held
;---------------------------------------------------------------------------

	bit joy_bit_a,a
	jr z,_not_a

	ld a,(joy_held_repeat)
	and joy_up+joy_down+joy_left+joy_right
	jp z,poop

	; no matter what, we need to do this

	call check_last_edit_track ; should retrieve the address to modify
					; retrieves address in bhl
					; retrieves address of effect in cde
					; retrieves value in a
	; returns b=0 if error

	ld a,b
	and a
	jp z,poop

	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(joy_held_repeat)

	; do decrements first:

	bit joy_bit_left,a
	jr z,_not_edit_left

	ld a,(hl)
	cp $80
	jp z,poop
	sub $01
	jr nc,_good_edit
	ld a,$80
	jr _good_edit

_not_edit_left:

	bit joy_bit_down,a
	jr z,_not_edit_down

	ld a,(hl)
	cp $80
	jp z,poop
	sub $0c
	jr nc,_good_edit
	ld a,$80
	jr _good_edit

_not_edit_down:

	; now do increments

	ld a,(hl)
	ld b,a
	cp $80
	jr nz,_not_edit_carry
	xor a
	jr _good_edit

_not_edit_carry:

	ld a,(joy_held_repeat)
	bit joy_bit_right,a
	jr z,_not_edit_right
	inc b
	jr _check_edit_increment

_not_edit_right:
	ld a,b
	add a,$0c
	ld b,a
_check_edit_increment:

	ld a,(editor_track)
	add a,<track_incr_max
	ld e,a
	ld a,>track_incr_max
	adc a,0
	ld d,a
	ld a,(de)
	cp b
	jr c,_good_edit

	ld a,b
_good_edit:
	ld (hl),a

	call redraw_patt_info
	call update_song_info_disp

	jp poop

_not_a:

;---------------------------------------------------------------------------
; b+ up/down instert/delete are done with b 
; b+left/right = ????
;---------------------------------------------------------------------------

	bit joy_bit_b,a
	jp z,_not_b

	ld a,(joy_held_repeat)
	and joy_up+joy_down; +joy_left+joy_right
	jp z,poop

	ld a,(joy_held_repeat)

	; b + up is delete

	bit joy_bit_up,a
	jr z,_no_b_up

	; bad things happen if we have less than 1 event, so prevent it

	ld a,(editor_track)
	add a,<editor_lines_1
	ld l,a
	ld a,>editor_lines_1
	adc a,0
	ld h,a

	ld a,(hl)
	and a
	jp z,poop

	; no matter what, we need to do this

	call check_last_edit_track ; should retrieve the address to modify
					; retrieves address in bhl
					; retrieves address of effect in cde
					; retrieves value in a

	; returns b=0 if error
	
	ld a,b
	and a
	jp z,poop

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call delete_effects_at_cursor
	call check_last_edit_track ; should retrieve the address to modify
	call delete_byte_at_bhl
	call redraw_patt_info
	call update_song_info_disp

	jp poop

_no_b_up:

	; b + down is insert

	bit joy_bit_down,a
	jr z,_no_b_down

	; check max lines

	ld a,(editor_track)
	add a,<editor_lines_1
	ld l,a
	ld a,>editor_lines_1
	adc a,0
	ld h,a

	ld a,(hl)
	cp $ff
	jp nz,_no_poop

	ld a,(editor_line)
	push af

	ld a,($ff)
	ld (editor_line),a

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call delete_effects_at_cursor
	call check_last_edit_track ; should retrieve the address to modify
	call delete_byte_at_bhl

	pop af
	ld (editor_line),a

_no_poop:

	; no matter what, we need to do this

	call check_last_edit_track ; should retrieve the address to modify
					; retrieves address in bhl
					; retrieves address of effect in cde
					; retrieves value in a

	; returns b=0 if error

	ld a,b
	and a
	jr nz,_it_is_ok

	ld a,(editor_line)
	dec a
	ld (editor_line),a

	call check_last_edit_track ; should retrieve the address to modify
					; retrieves address in bhl
					; retrieves address of effect in cde
					; retrieves value in a

	; returns b=0 if error

	ld a,(editor_line)
	inc a
	ld (editor_line),a

	ld a,b
	and a
	jp z,poop

	call check_last_edit_track ; should retrieve the address to modify
					; retrieves address in bhl
					; retrieves address of effect in cde
					; retrieves value in a

_it_is_ok:

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,$80
	ld (hl),a
	call redraw_patt_info
	call update_song_info_disp

	jp poop

_no_b_down:

	jp poop

_not_b:

;---------------------------------------------------------------------------
; just up/down/left/right moves around
;---------------------------------------------------------------------------

	call test_move_around_pattern
	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; test_start_play_modes:
;---------------------------------------------------------------------------
;***************************************************************************

test_start_play_modes:

;---------------------------------------------------------------------------
; start initiates different play modes
;---------------------------------------------------------------------------

	bit joy_bit_start,a
	ret z

	ld a,(joy_held)

	; a + b + start -> reload all effects parameters

	and joy_a+joy_b
	cp joy_a+joy_b
	jr nz,_not_play_ab

	ld a,(editor_seq)
	ld b,a
	ld a,(editor_line)
	ld c,a
	push bc

	xor a
	ld (editor_looping_mode),a

	call music_reset

_check_loop:
	pop bc
	ld a,(editor_seq)
	cp b
	jr z,_gotit
	push bc
	call music_player
	jr _check_loop

_gotit:
	xor a
	ld ($ff25),a

	ld a,c
	ld (editor_line),a
	ret
	
_not_play_ab:

	; a + start -> play from start

	bit joy_bit_a,a
	jr z,_not_start_a
	call setup_play_mode_start
	pop hl
	jp poop

_not_start_a:

	; b + start -> play pattern looping

	bit joy_bit_b,a
	jr z,_not_start_b
	call setup_play_mode_looping
	pop hl
	jp poop

_not_start_b:

	; start + select -> not used

	; start on it's own -> play from here

	call setup_play_mode
	pop hl
	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; test_move_around_pattern:
;---------------------------------------------------------------------------
;***************************************************************************

test_move_around_pattern:

;---------------------------------------------------------------------------
; just up/down/left/right moves around
;---------------------------------------------------------------------------

	ld a,(joy_held_repeat)
	bit joy_bit_up,a
	jr z,_not_up

	pop hl

	ld a,(editor_line)
	and a
	jp z,poop
	dec a
	jr _store_line

;---------------------------------------------------------------------------

_not_up:
	bit joy_bit_down,a
	jr z,_not_down

	pop hl

	ld a,(editor_lines_used)
	ld b,a
	ld a,(editor_line)
	cp b
	jp nc,poop
	inc a
_store_line:
	ld (editor_line),a
	call editor_draw_block_line_up
	jp poop

;---------------------------------------------------------------------------

_not_down:

	bit joy_bit_left,a
	jr z,_not_left

	ld a,(editor_track)
	dec a
	jr _store_track

;---------------------------------------------------------------------------

_not_left:

	bit joy_bit_right,a
	ret z

	ld a,(editor_track)
	inc a
_store_track:
	and $3
	ld (editor_track),a
	pop hl
	jp poop

;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

track_incr_max:

	db ctrlbt_b_8,ctrlbt_b_8,ctrlbt_b_8,$7f
	end
