;---------------------------------------------------------------------------
; editor functions and helpers
;---------------------------------------------------------------------------
; contains the following functions:
;
; copy_track_over:
; copy_track_from_cursor_over:
; paste_track_over:
; paste_track_to_cursor_over:
; undo_over:
; redo_over:
;---------------------------------------------------------------------------
; undo_redo:
; copy_patt:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; copy track
;---------------------------------------------------------------------------
;***************************************************************************

copy_track_over:

	call copy_patt

	call get_track_patt_index_addr

	ld a,(hli)
	ld (editor_copy_from),a
	call check_hl_wram_read
	ld a,(hl)
	ld (editor_copy_from+1),a

	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; copy track from cursor
;---------------------------------------------------------------------------
;***************************************************************************

copy_track_from_cursor_over:

	call copy_patt

	call check_last_edit_track

	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error

	ld a,b
	and a
	jr nz,_good_address

	ld de,$0000
	jr _store_address

_good_address:

	ld a,c
	sub music_buffer_start_bank
	and $0f
	swap a
	ld c,a

	ld a,d
	and $0f
	or c
	ld d,a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a

	ld a,e
	sub l
	ld e,a

	ld a,d
	sbc a,h
	ld d,a

_store_address:
	ld a,e
	ld (editor_copy_from),a
	ld a,d
	ld (editor_copy_from+1),a

	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; copy patt
;---------------------------------------------------------------------------
;***************************************************************************

copy_patt:

	call get_patt_index_addr
	call convert_hl_index_to_bhl_address

	ld de,editor_copy_patt

	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a
	inc de
	call check_bhl_wram_read
	ld (de),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; paste track
;---------------------------------------------------------------------------
;***************************************************************************

paste_track_over:

	call get_track_patt_index_addr

	push bc
	push hl	

	call add_bhl_undo

	pop hl
	pop bc

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(editor_copy_from)
	ld c,a
	ld a,(editor_copy_from+1)
	ld b,a

	and a
	ld a,c
	jr nz,_not_null
	and a
	jp z,poop

_not_null:

	ld (hli),a
	call check_hl_wram_read
	ld a,b
	ld (hl),a

	ld a,(editor_track)
	sla a
	sla a
	ld hl,editor_sequence_1
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,c
	ld (hli),a
	ld a,($d003)
	adc a,b
	and $0f
	or $d0
	ld (hl),a

	; clear last edit track

	ld a,$ff
	ld (last_edit_track),a
	ld (last_edit_patt),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; paste track to cursor
;---------------------------------------------------------------------------
;***************************************************************************

paste_track_to_cursor_over:

	; clear last edit track

	ld a,$ff
	ld (last_edit_track),a
	ld (last_edit_patt),a

	call check_last_edit_track

	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error

	ld a,b
	and a
	jp z,poop

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(editor_copy_from)
	ld l,a
	ld a,(editor_copy_from+1)
	ld h,a

	and a
	jr nz,_not_null
	ld a,l
	and a
	jp z,poop

_not_null:

	ld a,($d002)
	add a,l
	ld l,a
	ld a,($d003)
	adc a,h
	ld h,a

	call convert_hl_index_to_bhl_address

	; now we copy to end of pattern

_copy_inner_loop:
	call check_bhl_wram_read
	call check_cde_wram_write
	bit 7,a
	jr z,_copy_inner_loop
	bit 6,a
	jr z,_copy_inner_loop

	cp ctrlbt_track_loop
	jr z,_done_copy

	push de

	ld d,>bytes_per_effect
	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,d
	adc a,$00
	ld d,a
	ld a,(de)
	ld (file_access_temp),a

	pop de

_effect_loop:
	ld a,(file_access_temp)
	and a
	jr z,_copy_inner_loop
	dec a
	ld (file_access_temp),a
	call check_bhl_wram_read
	call check_cde_wram_write
	jr _effect_loop

_done_copy:
	; RESET OUR EOF, IDIOT

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,e
	ld ($d004),a

	ld a,c
	sub music_buffer_start_bank
	and $0f
	swap a
	ld c,a

	ld a,d
	and $0f
	or c
	ld ($d005),a

	; now check to see if it went past line $ff

	ld a,(editor_line)
	push af
	ld a,$ff
	ld (editor_line),a

	call check_last_edit_track

	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error	

	pop af
	ld (editor_line),a
	ld a,b
	and a
	jr z,_not_over_ff

	ld (current_wram_bank),a
	ld ($ff70),a

	inc hl
	call check_hl_wram_read
	ld a,ctrlbt_track_loop
	ld (hli),a
	call check_hl_wram_read

	ld a,(current_wram_bank)
	sub music_buffer_start_bank
	and $0f
	swap a
	ld b,a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,l
	ld ($d004),a

	ld a,h
	and $0f
	or b
	ld ($d005),a

	ld b,b

_not_over_ff:
	; clear last edit track

	ld a,$ff
	ld (last_edit_track),a
	ld (last_edit_patt),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; undo
;---------------------------------------------------------------------------
;***************************************************************************

undo_over:

	; undos are stored as an index word folowed by a word to store there

	ld a,(editor_num_undos)
	and a
	pop hl
	ret z

	push hl

	dec a
	ld (editor_num_undos),a

	call undo_redo

	ld a,(editor_num_redos)
	inc a
	ld (editor_num_redos),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; redo
;---------------------------------------------------------------------------
;***************************************************************************

redo_over:

	; undos are stored as an index word folowed by a word to store there

	ld a,(editor_num_redos)
	and a
	pop hl
	ret z

	push hl

	ld a,(editor_num_undos)

	call undo_redo

	ld a,(editor_num_undos)
	inc a
	ld (editor_num_undos),a

	ld a,(editor_num_redos)
	dec a
	ld (editor_num_redos),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; undo_redo
;---------------------------------------------------------------------------
;***************************************************************************

undo_redo:
	
	ld d,0
	sla a
	rl d
	sla a
	rl d

	add a,<editor_undo_buffer
	ld e,a
	ld a,d
	adc a,>editor_undo_buffer
	ld d,a


	ld a,(de)
	ld l,a
	inc de
	ld a,(de)
	ld h,a
	inc de

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,l
	ld l,a
	ld a,($d003)
	adc a,h
	ld h,a

	call convert_hl_index_to_bhl_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	; swap the bytes, so we can have a redo

	ld a,(hl)
	ld c,a
	ld a,(de)
	ld (hli),a
	ld a,c
	ld (de),a
	inc de
	call check_hl_wram_read
	ld a,(hl)
	ld c,a
	ld a,(de)
	ld (hl),a
	ld a,c
	ld (de),a

	; clear last edit track

	ld a,$ff
	ld (last_edit_track),a
	ld (last_edit_patt),a

	ret

