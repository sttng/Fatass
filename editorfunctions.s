;---------------------------------------------------------------------------
; editor functions and helpers
;---------------------------------------------------------------------------
; contains the following functions:
;
; copy_track: wrapper
; copy_track_from_cursor: wrapper
; paste_track:
; paste_track_to_cursor:
; undo: 
; redo: 
; insert_pattern:
; insert_pattern_copy: wrapper
;---------------------------------------------------------------------------
; redraw_song_info:
; redraw_patt_info:
; get_patt_index_addr:
; get_track_patt_index_addr:
; get_seq_position:
; add_bhl_undo:
; check_last_edit_track:
; insert_pattern_done:
;---------------------------------------------------------------------------
; insert_byte_at_cde:
; delete_byte_at_bhl:
; check_hl_wram_back:
; check_de_wram_back:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; defines
;---------------------------------------------------------------------------

max_undos	equ $ff
default_pattern_length equ $40

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; copy track
;---------------------------------------------------------------------------
;***************************************************************************

copy_track:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp copy_track_over

;***************************************************************************
;---------------------------------------------------------------------------
; copy track from cursor
;---------------------------------------------------------------------------
;***************************************************************************

copy_track_from_cursor:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp copy_track_from_cursor_over

;***************************************************************************
;---------------------------------------------------------------------------
; paste track
;---------------------------------------------------------------------------
;***************************************************************************

paste_track:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call paste_track_over	
	
	call redraw_patt_info
	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; paste track to cursor
;---------------------------------------------------------------------------
;***************************************************************************

paste_track_to_cursor:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call paste_track_to_cursor_over	
	
	call redraw_patt_info
	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; undo
;---------------------------------------------------------------------------
;***************************************************************************

undo:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call undo_over

	call update_song_info_disp
	call redraw_patt_info
	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; redo
;---------------------------------------------------------------------------
;***************************************************************************

redo:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call redo_over

	call update_song_info_disp
	call redraw_patt_info
	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; insert pattern
;---------------------------------------------------------------------------
;***************************************************************************

insert_pattern:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call insert_pattern_over

	call redraw_song_info
	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; insert pattern copy
;---------------------------------------------------------------------------
;***************************************************************************

insert_pattern_copy:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call insert_pattern_copy_over

	call redraw_song_info
	jp setup_edit_patt

;***************************************************************************
;---------------------------------------------------------------------------
; redraw song info:
; forces a redraw of the entire pattern
;---------------------------------------------------------------------------
;***************************************************************************

redraw_song_info:
	ld a,(editor_seq)
	ld l,a
	ld a,(editor_line)
	ld h,a
	push hl

	call music_reset
	xor a
	ld ($ff25),a

	pop hl
	ld a,l
	ld (editor_seq),a
	ld a,h
	ld (editor_line),a

	call draw_song_and_file_info_disp
	call editor_init_block_buffer

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

;***************************************************************************
;---------------------------------------------------------------------------
; redraw patt info:
; forces a redraw of the entire pattern
;---------------------------------------------------------------------------
;***************************************************************************

redraw_patt_info:

	ld a,$ff
	ld (editor_redraw),a

	call get_patt_index_addr
	call convert_hl_index_to_bhl_address

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld e,a
	ld a,($d003)
	ld d,a

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(hli)
	add a,e
	ld (editor_sequence_1),a
	ld a,d
	adc a,0
	ld c,a
	call check_hl_wram_read

	ld a,(hli)
	add a,c
	add a,$d0
	ld (editor_sequence_1+1),a
	call check_hl_wram_read

	ld a,(hli)
	add a,e
	ld (editor_sequence_2),a
	ld a,d
	adc a,0
	ld c,a
	call check_hl_wram_read

	ld a,(hli)
	add a,c
	add a,$d0
	ld (editor_sequence_2+1),a
	call check_hl_wram_read

	ld a,(hli)
	add a,e
	ld (editor_sequence_3),a
	ld a,d
	adc a,0
	ld c,a
	call check_hl_wram_read

	ld a,(hli)
	add a,c
	add a,$d0
	ld (editor_sequence_3+1),a
	call check_hl_wram_read

	ld a,(hli)
	add a,e
	ld (editor_sequence_4),a
	ld a,d
	adc a,0
	ld c,a
	call check_hl_wram_read

	ld a,(hl)
	add a,c
	add a,$d0
	ld (editor_sequence_4+1),a

	; force redraw, that's what we're here for
	xor a
	ld (editor_last_sequence_1+0),a
	ld (editor_last_sequence_1+1),a
	ld (editor_last_sequence_2+0),a
	ld (editor_last_sequence_2+1),a
	ld (editor_last_sequence_3+0),a
	ld (editor_last_sequence_3+1),a
	ld (editor_last_sequence_4+0),a
	ld (editor_last_sequence_4+1),a

	jp editor_draw_block_line

;***************************************************************************
;---------------------------------------------------------------------------
; get_patt_index_addr
;---------------------------------------------------------------------------
;***************************************************************************

get_patt_index_addr:

	; returns the address of the index in the patt header

	call get_seq_position

	ld l,a
	ld h,0
	sla l
	rl h
	sla l
	rl h
	sla l
	rl h

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,l
	ld l,a
	ld a,($d003)
	adc a,h
	ld h,a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; get_track_patt_index_addr
;---------------------------------------------------------------------------
;***************************************************************************

get_track_patt_index_addr:

	call get_patt_index_addr

	ld a,(editor_track)
	sla a
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	call convert_hl_index_to_bhl_address

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; get seq position
;---------------------------------------------------------------------------
;***************************************************************************

get_seq_position:

	; returns address to effects prior to pattern index index in DE
	; returns address to pattern index index in HL
	; returns the pattern index index in A

_try_again_fucker:

	; get header for song

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d000)
	ld l,a
	ld a,($d001)
	add a,$d0
	ld h,a

	ld e,l
	ld d,h

	ld a,(editor_seq)
	ld c,a
	inc c
	ld b,$00
	dec hl
_seq_loop:
	inc hl
	call check_hl_wram_read
	ld a,(hl)
	cp first_seq_effect
	jr c,_not_seq_effect

	cp ctrlbt_track_loop
	jr z,_done_seq_loop

	push de

	ld d,>bytes_per_effect
	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,d
	adc a,$00
	ld d,a
	ld a,(de)
	ld e,a
	ld d,$00
	add hl,de

	pop de

	jr _seq_loop

_not_seq_effect:

	inc b
	dec c
	ret z

	ld e,l
	ld d,h
	inc de

	jr _seq_loop

_done_seq_loop:

	; error! that sequence does not exist!

	; this can happen when scrolling around in the sequence, so-> move
	; it back and try again

	ld a,b
	dec a
	ld (editor_seq),a
	jr _try_again_fucker

;***************************************************************************
;---------------------------------------------------------------------------
; add bhl undo
;---------------------------------------------------------------------------
;***************************************************************************

add_bhl_undo:

	ld a,(editor_num_undos)
	cp max_undos
	jr nz,_not_scroll

	push hl

	ld c,$fc
	ld hl,editor_undo_buffer
	ld de,editor_undo_buffer+4

_scroll_loop:

	ld a,(de)
	inc de
	ld (hli),a
	dec c
	jr nz,_scroll_loop

	ld a,max_undos-1

	pop hl	

_not_scroll:

	ld e,a

	inc a
	ld (editor_num_undos),a

	ld d,0
	sla e
	rl d
	sla e
	rl d

	ld a,<editor_undo_buffer
	add a,e
	ld e,a
	ld a,d
	adc a,>editor_undo_buffer
	ld d,a

	ld a,b
	sub music_buffer_start_bank
	and $0f
	swap a
	ld c,a
	
	push hl

	ld a,h
	and $0f
	or c
	ld h,a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	; store the index

	ld a,($d002)
	ld c,a
	ld a,l
	sub c
	ld (de),a
	inc de

	ld a,($d003)
	ld c,a
	ld a,h
	sbc a,c
	ld (de),a
	inc de

	pop hl

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	; store the value

	ld a,(hli)
	ld (de),a
	inc de
	call check_hl_wram_read
	ld a,(hl)
	ld (de),a

	; clear our redos

	xor a
	ld (editor_num_redos),a

	jp update_song_info_disp


;***************************************************************************
;---------------------------------------------------------------------------
; check_last_edit_track:
;---------------------------------------------------------------------------
;***************************************************************************

check_last_edit_track:

	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error

	call get_seq_position

	ld l,a
	ld a,(last_edit_patt)
	cp l
	jp nz,_not_same

	ld a,(editor_track)
	ld e,a
	ld a,(last_edit_track)
	cp e
	jp nz,_not_same

	; they are the same-> we need to return the address to edit

	ld h,0
	ld d,h
	sla l
	rl h
	sla l
	rl h
	sla l
	rl h

	; -> so hl is the pattern header index

	sla e
	add hl,de

	; -> so hl is the pattern header index

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld c,a
	ld a,($d003)
	ld b,a

	add hl,bc
	push bc

	; -> so hl is the pattern header index proper

	call convert_hl_index_to_bhl_address

	; so now we have to read the pattern until we reach editor_line

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(hli)
	ld b,a
	call check_hl_wram_read
	ld h,(hl)
	ld l,b
	pop bc
	add hl,bc

	call convert_hl_index_to_bhl_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld (file_access_temp),a

	ld e,l
	ld d,h

	ld a,(editor_line)
	ld c,a
	dec hl
_patt_loop:
	inc hl
	call check_hl_wram_read
	ld a,(hl)
	bit 7,a
	jr z,_not_patt_effect
	bit 6,a
	jr nz,_patt_effect

	; uh oh, we have a wait multi

	and $3f ; $00 to $40
	and a			; treat single wait the
	jr z,_not_patt_effect_2 	; same as note
	ld b,a			; total wait length
;	inc a			; actual # of lines to wait
	cp c			; vs actual # remaining
	jr nc,_its_not_ok	; if they are the same or more do the insert nonsense

	ld a,c			; subtract the extra lines
	sub b
	ld c,a
	jr _not_patt_effect_2	; and process as note

_its_not_ok:

	; ok so now we have 3 possible cases:
	; the space we want is at the top,		( 1 insert )
	; the space we want is at the middle,		( 2 inserts)
	; the space we want is at the end of our wait	( 1 insert )

	call check_hl_wram_read

	; ok so first we check for space at the top

	ld a,c
;	dec a
	and a
	jr z,_skip_this_step		; space is at the top
	dec a
	or $80
	ld (hli),a
	call check_hl_wram_read		; space is in the middle or bottom

	ld a,b
	sub c
;	inc a
	ld b,a				; subtract c from b so that b is lines remaining

;	ld c,1				; there is one line left until we reach the one
					; we want, and that is coming up next

	; do our insert

	push bc
	push hl

	ld e,l
	ld d,h
	ld a,(current_wram_bank)
	ld c,a

	call insert_byte_at_cde

	; destroys a,hl

	pop hl
	pop bc

_skip_this_step:

	; ok so now we have to write the middle byte, not so hard
	; as it is just $80

	; also, we should save this address as it is the address we should point to

	ld a,(current_wram_bank)
	ld c,a
	ld e,l
	ld d,h

	ld a,b
	and a
	jr z,_skip_insert

	; do our insert

	push bc
	push hl
	push de

	call insert_byte_at_cde

	; destroys a,hl

	pop de
	pop hl
	pop bc

_skip_insert:

	ld a,$80
;	ld a,$00
	ld (hli),a
	call check_hl_wram_read

	; now we have to test to see how many lines we have left to draw

	ld a,b
	and a
	jr z,_done_this_crap

	ld a,b
	dec a
	or $80
	ld (hl),a

_done_this_crap:

	ld l,e
	ld h,d
	ld b,c

	ld a,$80
	ret	

_patt_effect:

	cp ctrlbt_track_loop
	jr z,_done_patt_loop

	push de

	ld d,>bytes_per_effect
	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,d
	adc a,$00
	ld d,a
	ld a,(de)
	ld e,a
	ld d,$00
	add hl,de

	pop de

	jr _patt_loop

_not_patt_effect_2:

	ld a,$80

_not_patt_effect:

	ld b,a
	dec c
	ld a,c
	cp $ff
	jr nz,_not_done

	push bc

	ld a,(current_wram_bank)
	ld b,a
	ld a,(file_access_temp)
	ld c,a

	pop af

	ret

_not_done:

	ld e,l
	ld d,h
	ld a,(current_wram_bank)
	ld (file_access_temp),a
	inc de

	jp _patt_loop

_done_patt_loop:

	; error, this line is not in the pattern

	; this can happen if you try to edit a line that does not exist,
	; in which case we return b=0 as an error

	ld a,(current_wram_bank)
	ld c,a

	ld b,0

	ret

_not_same:

	ld a,l
	ld (last_edit_patt),a

	ld a,(editor_track)
	ld e,a
	ld (last_edit_track),a

	; they are not the same -> we need to copy the pattern to the end of
	; the file and update the index and the file size and add an undo

	; -> so l is the pattern #

	ld h,0
	ld d,h
	sla l
	rl h
	sla l
	rl h
	sla l
	rl h

	; -> so hl is the pattern header index

	sla e
	add hl,de

	; -> so hl is the pattern header index

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld c,a
	ld a,($d003)
	ld b,a

	add hl,bc

	push bc

	; -> so hl is the pattern header index proper

	ld a,($d004)
	ld e,a
	ld a,($d005)
	ld d,a

	; and de is our eof index
	; what do you know?  we've got the freaking infomation we need to add the undo
	; hey de is that address we are going to need when we replace the header.

	call convert_hl_index_to_bhl_address

	push de
	push hl
	push bc

	call add_bhl_undo
	
	pop bc
	pop hl
	pop de

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	pop bc
	push bc
	push de

	ld a,e
	sub c
	ld e,a
	ld a,d
	sbc a,b
	ld d,a
	
	; read the info and put in our eof value at same time

	ld c,(hl)
	ld a,e
	ld (hli),a

	call check_hl_wram_read
	ld b,(hl)
	ld a,d
	ld (hl),a

	ld l,c
	ld h,b

	pop de
	pop bc
	add hl,bc

	call convert_hl_index_to_bhl_address
	call convert_de_index_to_cde_address

	; -> so bhl is the pattern index header, make that our source
	; cde is our destination

	; now we have to copy the pattern

	xor a
	ld (file_access_temp+2),a

_copy_inner_loop:
	call check_bhl_wram_read
	call check_cde_wram_write

	ld (file_access_temp+1),a

	ld a,(file_access_temp+2)
	and a
	jr z,_copy_not_effect
	dec a
	ld (file_access_temp+2),a
	jr _copy_inner_loop

_copy_not_effect:

	ld a,(file_access_temp+1)

	bit 7,a
	jr z,_copy_inner_loop
	bit 6,a
	jr z,_copy_inner_loop

	cp ctrlbt_track_loop
	jr z,_copy_track_end

	push bc

	and $3f
	add a,<bytes_per_effect
	ld c,a
	ld a,>bytes_per_effect
	adc a,0
	ld b,a

	ld a,(bc)
	ld (file_access_temp+2),a ; number of effects bytes

	pop bc
	jr _copy_inner_loop

_copy_track_end:

	; yay, it is done copying
	; update the file size

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,e
	ld ($d004),a

	ld a,d
	and $0f
	ld d,a

	ld a,c
	sub music_buffer_start_bank
	and $0f
	swap a
	or d
	ld ($d005),a

	jp check_last_edit_track

;***************************************************************************
;---------------------------------------------------------------------------
; insert_byte_at_cde:
;---------------------------------------------------------------------------
;***************************************************************************

insert_byte_at_cde:

	; destroys a,hl

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d004)
	ld l,a
	ld a,($d005)
	ld h,a
	inc hl
	ld a,l
	ld ($d004),a
	ld a,h
	ld ($d005),a

	call convert_hl_index_to_bhl_address
	ld a,b

	ld (current_wram_bank),a
	ld ($ff70),a

_not_done_insert:

	ld a,(hli)
	push af
	call check_hl_wram_read
	pop af
	ld (hld),a
	dec hl
	call check_hl_wram_back

	ld a,l
	cp e
	jr nz,_not_done_insert
	ld a,h
	cp d
	jr nz,_not_done_insert
	ld a,(current_wram_bank)
	cp c
	jr nz,_not_done_insert

	; one more for good measure

	ld a,(hli)
	push af
	call check_hl_wram_read
	pop af
	ld (hld),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; delete_byte_at_bhl:
;---------------------------------------------------------------------------
;***************************************************************************

delete_byte_at_bhl:

	; destroys a,de,hl

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d004)
	ld e,a
	ld a,($d005)
	ld d,a
	dec de
	ld a,e
	ld ($d004),a
	ld a,d
	ld ($d005),a

	inc de

	call convert_de_index_to_cde_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

_not_done_delete:

	inc hl
	call check_hl_wram_read
	ld a,(hld)
	push af
	call check_hl_wram_back
	pop af
	ld (hli),a

	ld a,l
	cp e
	jr nz,_not_done_delete
	ld a,h
	cp d
	jr nz,_not_done_delete
	ld a,(current_wram_bank)
	cp c
	jr nz,_not_done_delete

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; check_hl_wram_back:
;---------------------------------------------------------------------------
;***************************************************************************

check_hl_wram_back:

	ld a,h
	cp $cf
	ret nz

	ld h,$df
	ld a,(current_wram_bank)
	dec a
	ld (current_wram_bank),a
	ld ($ff70),a
	ret

;***************************************************************************
;---------------------------------------------------------------------------
; check_de_wram_back:
;---------------------------------------------------------------------------
;***************************************************************************

check_de_wram_back:

	ld a,d
	cp $cf
	ret nz

	ld d,$df
	ld a,(current_wram_bank)
	dec a
	ld (current_wram_bank),a
	ld ($ff70),a
	ret


