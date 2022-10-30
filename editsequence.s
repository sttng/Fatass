;***************************************************************************
;---------------------------------------------------------------------------
; setup edit sequence mode
;---------------------------------------------------------------------------
;***************************************************************************

setup_edit_seq:

	xor a
	ld ($ff25),a

	ld a,edit_sequence_mode
	ld (editor_mode),a
	ld (main_menu_came_from),a

	call reset_pat_bar

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; edit sequence vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

edit_seq_vbl_irq:

	ld a,%11100100			;"normal" colours
	ld ($ff47),a			; dmg bg palette

	call seq_cursor

	ei

	jp done_vbl

;***************************************************************************
;---------------------------------------------------------------------------
; edit sequence poop loop
;---------------------------------------------------------------------------
;***************************************************************************

edit_seq_poop_loop:

	call update_waveforms
	call joy_repeat_update

	ld a,(joy_pressed)

;---------------------------------------------------------------------------
; setect moves us around between different modes
;---------------------------------------------------------------------------

	bit joy_bit_select,a
	jr z,_not_select
	ld a,(joy_held)

	; a + select -> edit pattern mode

	bit joy_bit_a,a
	jr z,_not_sel_a
	call setup_edit_patt
	jp poop

_not_sel_a:

	; b + select -> edit effects mode

	bit joy_bit_b,a
	jr z,_not_sel_b
	call setup_edit_seq_efx
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
; a+b + up/down/left/right lets us scroll through the pattern
;---------------------------------------------------------------------------

	ld a,(joy_held)
	and joy_a+joy_b
	cp joy_a+joy_b
	jr nz,_not_ab

	call test_move_around_pattern
	jp poop

_not_ab:

;---------------------------------------------------------------------------
; a+up/down/left/right : main editor functions are done with a button held
;---------------------------------------------------------------------------

	bit joy_bit_a,a
	jr z,_not_a

	call test_edit_sequence_data
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

	; no matter what, we need to do this

	ld a,(joy_held_repeat)

	; b + up is delete

	bit joy_bit_up,a
	jr z,_no_b_up

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call delete_seq_effects_at_cursor

	ld a,(editor_max_seq)
	cp 2
	jr c,_edit_seq_redraw

	call get_seq_position

			; returns address to effects prior to pattern index index in DE
			; returns address to pattern index index in HL
			; returns the pattern index index in A

	ld a,(current_wram_bank)
	ld b,a
	ld c,a

	call delete_byte_at_bhl

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a
	dec hl
	jr _edit_seq_ins_del_done

_no_b_up:

	; b + down is insert

	bit joy_bit_down,a
	jr z,_no_b_down

	ld a,(editor_max_seq)
	cp $ff
	jp z,poop

	call get_seq_position

			; returns address to effects prior to pattern index index in DE
			; returns address to pattern index index in HL
			; returns the pattern index index in A

	push af

	ld a,(current_wram_bank)
	ld b,a
	ld c,a

	call insert_byte_at_cde

	pop af
	ld (de),a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a
	inc hl
_edit_seq_ins_del_done:
	ld a,l
	ld ($d002),a
	ld a,h
	ld ($d003),a

_edit_seq_redraw:
	call redraw_song_info

_no_b_down:

	jp poop

_not_b:

;---------------------------------------------------------------------------
; just up/down/left/right moves around
;---------------------------------------------------------------------------

	call test_edit_sequence_scroll
	jp poop

;---------------------------------------------------------------------------


;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

;***************************************************************************
;***************************************************************************

test_edit_sequence_data:

	ld a,(joy_held_repeat)
	and joy_up+joy_down+joy_left+joy_right
	ret z

	pop hl

	; no matter what, we need to do this

	call get_seq_position

			; returns address to effects prior to pattern index index in DE
			; returns address to pattern index index in HL
			; returns the pattern index index in A

	ld a,(current_wram_bank)
	ld b,a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d006)
	ld e,a

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(joy_held_repeat)

	; do decrements first:

	bit joy_bit_left,a
	jr z,_not_edit_left

	ld a,(hl)
	sub $01
	jr nc,_good_edit
	xor a
	jr _good_edit

_not_edit_left:

	bit joy_bit_down,a
	jr z,_not_edit_down

	ld a,(hl)
	sub $04
	jr nc,_good_edit
	xor a
	jr _good_edit

_not_edit_down:

	; now do increments

	bit joy_bit_right,a
	jr z,_not_edit_right
	ld a,(hl)
	inc a
	jr _check_edit_increment

_not_edit_right:
	ld a,(hl)
	add a,$04
_check_edit_increment:
	
	cp e
	jr c,_good_edit
	ld a,e
	dec a

_good_edit:
	ld (hl),a

	call redraw_song_info
	jp poop

;***************************************************************************
;***************************************************************************

test_edit_sequence_scroll:

	; up -> scroll up in the sequence

	ld a,(joy_held_repeat)
	bit joy_bit_up,a
	jr z,_not_up

	pop hl

	ld a,(editor_seq)
	and a
	jp z,poop
	dec a
	jr _sel_seq

	; down -> scroll down in the sequence

_not_up:
	bit joy_bit_down,a
	ret z

	pop hl

	ld a,(editor_max_seq)
	dec a
	ld c,a
	ld a,(editor_seq)
	cp c
	jp nc,poop
	inc a

_sel_seq:
	ld (editor_seq),a
	call redraw_song_info
	jp poop

;***************************************************************************
;***************************************************************************

	end
