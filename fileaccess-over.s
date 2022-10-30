
;***************************************************************************
; delete_song_from_sram_over
;***************************************************************************

delete_song_from_sram_ov:

	; what this should do is copy all sram from the point past the song
	; backwards and then offset the indexes of all songs that occurred
	; past it, then delete the song info from the header.

	; by doing it this way, there should be no memory fragmentation.

	; store the song number

	ld (file_access_temp),a

	; so, first we need the address of the song

	call get_sram_song_address_hl_bc_e_d

	; get the song number for storage

	ld a,(file_access_temp)
	ld d,a

	push hl ; song addr
	push de ; song bank(e)/number(d)

	ld a,c
	ld (file_access_size_temp),a
	ld a,b
	ld (file_access_size_temp+1),a

	; bc is length
	; hl is dest address
	; e is dest bank

	push hl
	add hl,bc
	pop bc

	ld d,e

	ld a,h

_addr_adj_loop:
	and $e0
	cp $a0
	jr z,_done_addr_adj

	ld a,h
	sub $20
	ld h,a
	inc d
	jr _addr_adj_loop
_done_addr_adj:

	; hl is source address
	; d is source bank
	; bc is dest address
	; e is dest bank

_main_delete_loop:
	ld a,d
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(hli)
	ld (file_access_temp),a

	ld a,e
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(file_access_temp)
	ld (bc),a
	inc bc

	; check for carry

	ld a,h
	cp $c0
	jr nz,_no_source_addr_carry

	; check for done

	xor a
	ld (current_sram_bank),a
	ld ($4666),a
	
	ld a,(max_save_ram_bank_save_ram)
	cp d
	jr z,_done_main_delete_loop

	; do carry

	ld h,$a0
	inc d
	
_no_source_addr_carry:

	ld a,b
	cp $c0
	jr nz,_main_delete_loop

	; do carry

	ld b,$a0
	inc e
	jr _main_delete_loop

_done_main_delete_loop:

	; set info bank

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; we have to update the blank area data - let's do that now as it will be
	; easier than the other stuff

	ld a,(file_access_size_temp)
	ld c,a
	ld a,(file_access_size_temp+1)
	ld b,a

	ld a,(start_blank_addr_save_ram)
	ld l,a

	ld a,(start_blank_addr_save_ram+1)
	ld h,a

	ld a,(start_blank_bank_save_ram)
	ld d,a

	call addr_sub_dhl_bc

	ld a,l
	ld (start_blank_addr_save_ram),a

	ld a,h
	ld (start_blank_addr_save_ram+1),a

	ld a,d
	ld (start_blank_bank_save_ram),a

	; now we have the amazing task of updating all pointers to point to the
	; correct song data - this will be fun.

	; so loop through all pointers, skipping current one

	pop de ; song bank(e)/number(d)
	pop hl ; song addr

	xor a ; counter
	ld (file_access_temp),a

	ld b,>start_blank_bank_save_ram

_header_adjust_loop:

	; check for done

	ld c,a
	ld a,(num_save_ram_songs)
	cp c
	jr z,_done_header_adjust

	; check bank

	ld a,<song_save_bank_table
	add a,c
	ld c,a

	ld a,(bc)
	cp e
	jr c,_skip_this_one
	jr nz,_do_this_one

	; check hibyte

	ld a,(file_access_temp)
	sla a
	add a,<song_save_header_table+1
	ld c,a

	ld a,(bc)
	cp h
	jr c,_skip_this_one
	jr nz,_do_this_one

	; check lowbyte

	dec c
	ld a,(bc)
	cp l
	jr c,_skip_this_one
	jr z,_skip_this_one

_do_this_one:

	ld a,(file_access_temp)
	sla a
	add a,<song_save_header_table
	ld c,a

	; ok, now we know that the one pointed to by c is in need of fixing

	push hl ; song addr
	push de ; song bank(e)/number(d)

	; get addr into hl

	ld a,(bc)
	ld l,a
	inc c
	ld a,(bc)
	ld h,a

	; get bank into d

	ld a,(file_access_temp)
	add a,<song_save_bank_table
	ld c,a

	ld a,(bc)
	ld d,a

	ld a,(file_access_size_temp)
	ld c,a
	ld a,(file_access_size_temp+1)
	ld b,a

	; do math

	call addr_sub_dhl_bc

	; now store it

	ld b,>start_blank_bank_save_ram

	ld a,(file_access_temp)
	add a,<song_save_bank_table
	ld c,a

	ld a,d
	ld (bc),a

	ld a,(file_access_temp)
	sla a
	add a,<song_save_header_table
	ld c,a

	ld a,l
	ld (bc),a
	inc c
	ld a,h
	ld (bc),a

	; now restore all our hellish values

	; right now the stack is :
	; size
	; bank
	; addr

	pop de ; song bank(e)/number(d)
	pop hl ; song addr

_skip_this_one:

	; move on to next one

	ld a,(file_access_temp)
	inc a
	ld (file_access_temp),a
	jr _header_adjust_loop

_done_header_adjust:	

	; now to decrement our numsong counter	

	ld a,(num_save_ram_songs)
	dec a
	ld (num_save_ram_songs),a

	; disable save ram access

	xor a
	ld ($1666),a

	ret

;***************************************************************************
; save_song_into_sram_from_ram_over
;***************************************************************************

save_song_into_sram_from_ram_ov:

	; first thing this should do is delete the song, but retain index
	; then it should save the song and set the index

	; DOES NOT CHECK FOR SONG TOO BIG TO SAVE!!!!

	; for now, just save

	;---------------------------------------------------------------------------
	; get information about the song
	; song number to save is in A
	; store it is an index in BC

	ld c,a
	ld b,0

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	; set our header bank

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; get the song start bank

	ld hl,song_save_bank_table
	add hl,bc

	ld a,(start_blank_bank_save_ram)
	ld (hl),a
	ld e,a

	; convert BC into word index

	sla c

	; get the song start address into HL

	ld hl,song_save_header_table
	add hl,bc

	ld a,(start_blank_addr_save_ram)
	ld (hli),a
	ld a,(start_blank_addr_save_ram+1)
	ld (hl),a
	ld h,a
	ld a,(start_blank_addr_save_ram)
	ld l,a

	; save our time

	push hl

	sla c
	ld hl,song_save_time_table
	add hl,bc

	ld a,(song_time+1)
	ld (hli),a
	ld a,(song_time+2)
	ld (hli),a
	ld a,(song_time+3)
	ld (hli),a
	ld a,(song_time+4)
	ld (hl),a

	pop hl

	; set our destination bank

	ld a,e
	ld (current_sram_bank),a
	ld ($4666),a

	; set our source bank
	
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	;---------------------------------------------------------------------------
	; get our song length into bc

	ld a,($d004)
	ld c,a
	ld a,($d005)
	ld b,a

	;---------------------------------------------------------------------------
	; copy song to sram

	ld de,$d000 ; source addr

_copy_loop:
		
	ld a,(de)
	ld (hli),a
	inc de

	; check for bankover in our dest

	call check_hl_sram_read

	; check for bankover in our source

	call check_de_wram_read

	; count down to finish

	dec bc
	ld a,c
	and a
	jr nz,_copy_loop

	ld a,b	
	and a
	jr nz,_copy_loop

	; save undo information

	ld de,editor_undo_buffer

	ld a,(editor_num_undos)
	ld (hli),a
	ld c,a

	call check_hl_sram_read
	ld a,(editor_num_redos)
	ld (hli),a
	add a,c
	ld c,a

	ld b,0
	sla c
	rl b
	sla c
	rl b

_save_undos_loop:

	ld a,c
	and a
	jr nz,_save_undos_go

	ld a,b	
	and a
	jr z,_save_undos_done

_save_undos_go:

	call check_hl_sram_read

	ld a,(de)
	ld (hli),a
	inc de
	dec bc
	jr _save_undos_loop

_save_undos_done:

	call check_hl_sram_read
		
	ld a,(current_sram_bank)
	ld b,a

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,b	
	ld (start_blank_bank_save_ram),a

	ld a,l
	ld (start_blank_addr_save_ram),a
	ld a,h
	ld (start_blank_addr_save_ram+1),a

	; disable save ram access

	call calculate_save_ram_checksum

	xor a
	ld ($1666),a
	
	ret

;***************************************************************************
; load_song_into_ram_from_sram_over
;***************************************************************************

load_song_into_ram_from_sram_ov:

	; DOES NOT CHECK FOR SONG TOO BIG TO LOAD!!!!

	; load our time

	push af

	ld b,0
	ld c,a

	ld a,$0a
	ld ($1666),a
	xor a
	ld ($4666),a

	sla c
	sla c
	ld hl,song_save_time_table
	add hl,bc

	xor a
	ld (song_time+0),a
	ld a,(hli)
	ld (song_time+1),a
	ld a,(hli)
	ld (song_time+2),a
	ld a,(hli)
	ld (song_time+3),a
	ld a,(hli)
	ld (song_time+4),a

	xor a
	ld ($1666),a

	pop af

	;---------------------------------------------------------------------------
	; get information about the song

	call get_sram_song_address_hl_bc_e

	;---------------------------------------------------------------------------
	; should test to see if song is too big to load here!!!!!!!!!!
	;---------------------------------------------------------------------------

	; set our destination bank
	
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	;---------------------------------------------------------------------------
	; copy song to ram

	ld de,$d000 ; dest addr

_copy_loop:
		
	ld a,(hli)
	ld (de),a
	inc de

	; check for bankover in our source

	call check_hl_sram_read

	; check for bankover in our dest

	call check_de_wram_read

	; count down to finish

	dec bc
	ld a,c
	and a
	jr nz,_copy_loop

	ld a,b	
	and a
	jr nz,_copy_loop
		
	; disable save ram access

	; load undos

	ld de,editor_undo_buffer

	ld a,(hli)
	ld (editor_num_undos),a
	ld c,a
	ld a,(hli)
	ld (editor_num_redos),a
	add a,c
	ld c,a

	ld b,0
	sla c
	rl b
	sla c
	rl b

_load_undos_loop:

	ld a,c
	and a
	jr nz,_load_undos_go

	ld a,b	
	and a
	jr z,_load_undos_done

_load_undos_go:

	call check_hl_sram_read

	ld a,(hli)
	ld (de),a
	inc de
	dec bc
	jr _load_undos_loop

_load_undos_done:

	xor a
	ld ($1666),a

	ret

;***************************************************************************
; optimize_song_in_wram: optimizes the current song in wram
;***************************************************************************

; !!!!! add a remove trailing spaces from filename

optimize_song_in_wram_over:

	; wipe undos

	xor a
	ld (editor_num_undos),a
	ld (editor_num_redos),a

	; TURN OFF TIME DISPLAY

	ld (draw_time_flag),a

	; and do it

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d004)
	ld c,a
	ld a,($d005)
	ld b,a

	inc bc

	ld a,c
	ld (file_access_sram_temp+0),a
	ld a,b
	ld (file_access_sram_temp+1),a

	ld de,$9c4f
	ld h,>numbers

	call write_number_bc_to_vram

	call update_optimize_disp

	; do optimization here

	; what this should do is:

	;---------------------------------------------------------------------------
	; A - remove any junk between filename and sequence
	;	- update size, patterns index, sequence index
	;---------------------------------------------------------------------------
	; B - remove any redundant effects commands from sequence, but only if they
	;     occur in the same section, IE 2 set speeds before a pattern call
	;	- update size, patterns index
	;---------------------------------------------------------------------------
	; C - remove any junk between sequence and pattern indexes
	;	- update size, patterns index
	;---------------------------------------------------------------------------
	; D - remove any unused pattern indexes
	; 	- sequence, patterns indexes, num pattern indexes
	;---------------------------------------------------------------------------
	; E - create a master table of indexes to all patterns
	;---------------------------------------------------------------------------
	; F - sort the table from smallest to largest, deleting identical indexes
	;---------------------------------------------------------------------------
	; G - walk through all patterns and add all macros to that table
	;---------------------------------------------------------------------------
	; H - sort the table from smallest to largest, deleting identical indexes
	;---------------------------------------------------------------------------
	; I - optimize each pattern; remove redundant effects commands, convert waits
	;     to multi waits, etc.
	;---------------------------------------------------------------------------
	; J - search for duplicate patterns and delete redundancies
	;	- pattern indexes, macro indexes in patterns
	;---------------------------------------------------------------------------
	; K - search for duplicate pattern parts and convert to macros
	;	- update size, pattern indexes, macro indexes in patterns
	;---------------------------------------------------------------------------
	; L - defragment song memory
	;	- update size, pattern indexes, macro indexes in patterns
	;---------------------------------------------------------------------------
	; M - search for redundant indexes and delete them
	;---------------------------------------------------------------------------

	; ok, let's do it!

	;---------------------------------------------------------------------------
	; A - remove any junk between filename and sequence
	;	- update size, patterns index, sequence index
	;---------------------------------------------------------------------------

;	ld a,music_buffer_start_bank
;	ld (current_wram_bank),a
;	ld ($ff70),a

	ld hl,$d007
	ld b,0
_find_end_of_filename_loop:
	inc b
	ld a,(hli)
	cp $ff
	jr nz,_find_end_of_filename_loop

	ld a,b
	cp 19
	jr c,_good_filename_size

	ld hl,$d007+18
	ld a,$ff
	ld (hli),a

_good_filename_size:

	ld a,h
	sub $d0
	ld h,a

	; hl is the end of the filename index

	ld a,($d000)
	ld e,a
	sub l
	ld c,a
	ld a,($d001)
	ld d,a
	sbc a,h
	ld b,a

	; bc is the sequence index - the end of the filename index, ie, the difference
	; de is the index of the sequence

	and a
	jr nz,_delete_filename_junk
	ld a,c
	and a
	jr z,_done_delete_filename_junk

_delete_filename_junk:

	; update our indexes

	; sequence

	ld a,($d000)
	sub c
	ld ($d000),a
	ld a,($d001)
	sbc a,b
	ld ($d001),a

	; patterns

	ld a,($d002)
	sub c
	ld ($d002),a
	ld a,($d003)
	sbc a,b
	ld ($d003),a

	; size

	ld a,($d004)
	sub c
	ld ($d004),a
	ld a,($d005)
	sbc a,b
	ld ($d005),a

	ld a,h
	add a,$d0
	ld h,a

	ld a,d
	add a,$d0
	ld d,a

	ld b,music_buffer_start_bank
	ld c,b

	; bhl is the address of the destination
	; cde is the address of the source
	; do our delete

	call move_wram_bhl_cde
	call update_optimize_disp
	
_done_delete_filename_junk:

	;---------------------------------------------------------------------------
	; B - remove any redundant effects commands from sequence, but only if they
	;     occur in the same section, IE 2 set speeds before a pattern call
	;	- update size, patterns index
	;---------------------------------------------------------------------------

	; wow, this will be fun
	; we should also flag used patterns here
	; use the editor_sequence_buffer+$300 for now

	xor a
	ld hl,editor_sequence_buffer+$300
	ld c,$ff
_red_seq_flag_wipe_loop:
	ld (hli),a
	dec c
	jr nz,_red_seq_flag_wipe_loop

_redundant_seq_check:

	ld a,($d000)
	ld l,a
	ld a,($d001)
	add a,$d0
	ld h,a

_redundant_seq_loop:
	ld c,($100-first_seq_effect)*3
	ld de,editor_sequence_buffer
	xor a
_w_r_s_loop:
	ld (de),a
	inc de
	dec c
	jr nz,_w_r_s_loop	

_r_s_e_loop:

	call check_hl_wram_read
	ld a,(hli)
	ld c,a
	sub first_seq_effect
	jr nc,_redundant_seq_effect

	; flag pat header as used

	ld b,>editor_sequence_buffer+$300
	ld a,1
	ld (bc),a

	jr _redundant_seq_loop

_redundant_seq_effect:

	cp ctrlbt_track_loop-first_seq_effect
	jr z,_done_redundant_seq_loop

	ld b,a
	sla a
	add a,b
	add a,<editor_sequence_buffer
	ld e,a
	ld a,>editor_sequence_buffer
	adc a,0
	ld d,a
	ld a,(de)
	and a
	jr nz,_not_a_new_one

	ld a,(current_wram_bank)
	ld (de),a
	inc de
	dec hl
	ld a,l
	ld (de),a
	inc de
	ld a,h
	ld (de),a
	inc hl

	ld a,c
	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,>bytes_per_effect
	adc a,0
	ld d,a

	ld a,(de)
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a
	jr _r_s_e_loop

_not_a_new_one:

	; we should delete the first one

	ld b,a ; our bank
	inc de
	ld a,(de) ; lowbyte
	ld l,a
	inc de
	ld a,(de) ; highbyte
	ld h,a

	; bhl is our destination

	ld a,c
	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,>bytes_per_effect
	adc a,0
	ld d,a

	ld a,(de)
	inc a
	ld c,a

	; c is our difference

	; update our indexes

	; patterns

	ld a,($d002)
	sub c
	ld ($d002),a
	ld a,($d003)
	sbc a,0
	ld ($d003),a

	; size

	ld a,($d004)
	sub c
	ld ($d004),a
	ld a,($d005)
	sbc a,0
	ld ($d005),a

	ld a,l
	add a,c
	ld e,a
	ld a,h
	adc a,0
	ld d,a
	ld c,b
	cp $e0
	jr nz,_no_sqwr_cde_carry

	ld h,$d0
	inc c

_no_sqwr_cde_carry:

	; bhl is the address of the destination
	; cde is the address of the source
	; do our delete

	call move_wram_bhl_cde
	call update_optimize_disp
	jp _redundant_seq_check
	
_done_redundant_seq_loop:	

	;---------------------------------------------------------------------------
	; C - remove any junk between sequence and pattern indexes
	;	- update size, patterns index
	;---------------------------------------------------------------------------

	; convieniently, (current_wram_bank)hl points to our dest already

	call check_hl_wram_read
	ld a,(current_wram_bank)
	ld b,a

	; bhl is our dest

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,h
	sub $d0
	ld d,a

	ld a,b
	sub music_buffer_start_bank
	swap a
	and $f0
	sla a
	or d
	ld d,a

	; dl is our offset for the patterns

	ld a,($d002)
	sub l
	ld e,a
	ld a,($d001)
	sbc a,d
	ld d,a
	jr nz,_delete_sequence_junk

	; de is our difference	

	ld a,l	
	and a
	jr z,_done_delete_sequence_junk

_delete_sequence_junk:

	; update our indexes

	; patterns

	ld a,($d002)
	sub e
	ld ($d002),a
	ld a,($d003)
	sbc a,d
	ld ($d003),a

	; size

	ld a,($d004)
	sub e
	ld ($d004),a
	ld a,($d005)
	sbc a,d
	ld ($d005),a

	ld a,l
	add a,e
	ld e,a
	ld a,h
	add a,d
	ld d,a

	ld c,b

_d_s_carry:
	and $f0
	cp $d0
	jr z,_d_s_no_carry

	ld a,d
	sub $10
	ld d,a

	inc c
	jr _d_s_carry

_d_s_no_carry:

	; bhl is the address of the destination
	; cde is the address of the source
	; do our delete

	call move_wram_bhl_cde
	call update_optimize_disp
	
_done_delete_sequence_junk:

	;---------------------------------------------------------------------------
	; D - remove any unused pattern indexes
	; 	- sequence, patterns indexes, num pattern indexes
	;---------------------------------------------------------------------------

_used_pattern_reset:

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld hl,editor_sequence_buffer+$300
	ld a,($d006)
	ld c,a

_used_pattern_loop:
	ld a,(hli)
	and a
	jp nz,_used_pattern

	; ok, now for fun: search through sequence and decrement all patterns past
	; current by one

	ld a,($d000)
	ld e,a
	ld a,($d001)
	add a,$d0
	ld d,a

	dec de

_used_pattern_decr_loop:
	inc de
	call check_de_wram_read
	ld a,(de)
	cp first_seq_effect
	jr nc,_used_seq_effect

	; flag pat header as used
	
	cp l
	jr c,_used_pattern_decr_loop

	dec a
	ld (de),a
	jr _used_pattern_decr_loop

_used_seq_effect:

	cp ctrlbt_track_loop
	jr z,_done_used_seq_loop

	push hl

	and $3f
	add a,<bytes_per_effect
	ld l,a
	ld a,>bytes_per_effect
	adc a,0
	ld h,a

	ld a,(hl)
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	pop hl
	jr _used_pattern_decr_loop

_done_used_seq_loop:

	; then delete the flag from the sequence buffer

	push hl
	dec hl

	ld e,l
	ld d,h
	inc hl

_delete_used_flag:

	ld a,(hli)
	ld (de),a
	inc de

	ld a,l
	and a
	jr nz,_delete_used_flag

	pop hl

	; then delete the pattern index

	push hl
	push bc

	ld a,($d006)
	sub l
	inc a
	ld c,a
	ld b,0

	sla c
	rl b
	sla c
	rl b
	sla c
	rl b

	ld a,c
	ld (file_access_temp),a
	ld a,b
	ld (file_access_temp+1),a

	dec hl

	ld h,0

	sla l
	rl h
	sla l
	rl h
	sla l
	rl h

	ld a,($d002)
	add a,l
	ld l,a
	ld a,($d003)
	adc a,h
	ld h,a

	call convert_hl_index_to_bhl_address

	ld e,l
	ld d,h
	ld c,b

	ld a,l
	add a,$08
	ld l,a
	ld a,h
	adc a,$00
	cp $e0
	jr nz,_no_used_p_i_carry
	ld a,$d0
	inc b
_no_used_p_i_carry:
	ld h,a

_used_p_i_loop:

	call check_bhl_wram_read
	call check_cde_wram_write

	push bc

	ld a,(file_access_temp)
	ld c,a
	ld a,(file_access_temp+1)
	ld b,a
	dec bc
	ld a,c
	ld (file_access_temp),a
	ld a,b
	ld (file_access_temp+1),a

	and a
	jr nz,_used_not_done_done
	ld a,c
	and a
	jr z,_used_done_done

_used_not_done_done:	

	pop bc
	jr _used_p_i_loop

_used_done_done:

	; then update eof

	ld a,($d006)
	dec a
	ld ($d006),a

	pop bc

	pop bc
	pop hl

	jp _used_pattern_reset

_used_pattern:
	dec c
	jp nz,_used_pattern_loop

	;---------------------------------------------------------------------------
	; E - create a master table of indexes to all patterns
	;---------------------------------------------------------------------------

	; right now we are using $1000 for the index table and up to $408 for patterns 
	; that call macros table.

	; since there is a max of $00fc*4 patterns in the song (if we delete unused
	; patterns-> should do that anyways) there can only be $3f0 patterns that call
	; macros.  If every pattern calls a macro, that is $7e0 for the patterns that
	; call macros table -> already past our max!

	; since the number of macros is unlimited, the $1000 may not be enough for
	; indexes -> currently enough for up to $3ff patterns and macros.

	; anyone who makes a song that big is a freak!

	; ok, we need a large chunk of ram for this, so lets use the editor block buffer
	; also, we can just copy it out of the song header

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a

	call convert_hl_index_to_bhl_address

	ld de,$d000

	ld a,b
	ld (file_access_temp+2),a

	ld a,($d006)
	ld c,a
	xor a
	sla c
	rl a
	sla c
	rl a
	ld (file_access_num_indexes+1),a
	ld b,a
	ld a,c
	ld (file_access_num_indexes+0),a
	
	; (file_access_temp+2)hl is our source address
	; de is our dest address
	; bc is our count

_seq_ind_loop:

	; get word

	ld a,(file_access_temp+2)
	ld (current_wram_bank),a
	ld ($ff70),a

	; add offset

	ld a,($d002)
	add a,(hl)
	ld (file_access_temp+0),a
	ld a,($d003)
	adc a,0
	ld (file_access_temp+1),a
	inc hl
	call check_hl_wram_read
	ld a,(file_access_temp+1)
	add a,(hl)
	ld (file_access_temp+1),a
	inc hl
	call check_hl_wram_read
	
	ld a,(current_wram_bank)
	ld (file_access_temp+2),a

	; store word

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(file_access_temp+0) ; index 
	ld (de),a
	inc de
	ld a,(file_access_temp+1)
	ld (de),a
	inc de

	ld a,(file_access_temp+0) ; index
	ld (de),a
	inc de
	ld a,(file_access_temp+1)
	ld (de),a
	inc de

	dec bc
	ld a,c
	and a
	jr nz,_seq_ind_loop

	ld a,b
	and a
	jr nz,_seq_ind_loop

	;---------------------------------------------------------------------------
	; F - sort the table from smallest to largest, deleting identical indexes
	;---------------------------------------------------------------------------

	call _sort_index_table

	;---------------------------------------------------------------------------
	; G - walk through all patterns and add all macros to that table
	;---------------------------------------------------------------------------

	; shit, this will certainly be fun

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

	; set up our macro pattern counter and table pointer

	ld de,editor_sequence_buffer

	xor a
	ld (file_access_num_macros+0),a
	ld (file_access_num_macros+1),a

_find_macros_loop:

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	dec bc
	push bc

	sla c
	rl b
	sla c
	rl b

	ld a,b
	add a,$d0
	ld b,a

	; bc is the address of the pattern index

	; store pattern index in macro table

	ld a,(bc)
	ld l,a
	ld (de),a
	inc de
	inc bc
	ld a,(bc)
	ld h,a
	ld (de),a
	dec de	

	; hl is our pattern index

	; clear our macro found flag

	xor a
	ld (file_access_temp),a

	call convert_hl_index_to_bhl_address
	
	; now we step through bhl untill we reach end

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

_fm_not_effect:
	call check_hl_wram_read
	ld a,(hli)
	bit 7,a
	jr z,_fm_not_effect
	bit 6,a
	jr z,_fm_not_effect

	cp ctrlbt_track_loop
	jp z,_fm_done_this_one

	cp ctrlbt_macro
	jr nz,_fm_not_macro

	; check to see if we need to add this one

	ld a,(file_access_temp)
	and a
	jr nz,_fm_skip

	; add it

	inc de
	inc de

	ld a,(file_access_num_macros+0)
	ld c,a
	ld a,(file_access_num_macros+1)
	ld b,a
	inc bc
	ld a,c
	ld (file_access_num_macros+0),a
	ld a,b
	ld (file_access_num_macros+1),a

	ld a,1
	ld (file_access_temp),a

_fm_skip:

	; now store the macro address in our address table

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a
	inc bc
	ld a,c
	ld (file_access_num_indexes+0),a
	ld a,b
	ld (file_access_num_indexes+1),a

	dec bc

	sla c
	rl b
	sla c
	rl b

	ld a,b
	add a,$d0
	ld b,a

	push de

	call check_hl_wram_read
	ld a,($d002)
	add a,(hl)
	ld e,a
	ld a,($d003)
	adc a,0
	ld d,a
	inc hl
	call check_hl_wram_read
	ld a,(hli)
	add a,d
	ld d,a
	ld a,(current_wram_bank)
	ld (file_access_temp+2),a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,e
	ld (bc),a
	inc bc
	ld a,d
	ld (bc),a
	inc bc
	ld a,e
	ld (bc),a
	inc bc
	ld a,d
	ld (bc),a

	ld a,(file_access_temp+2)
	ld (current_wram_bank),a
	ld ($ff70),a

	pop de

	jp _fm_not_effect
	
_fm_not_macro:
	and $3f
	add a,<bytes_per_effect
	ld c,a
	ld a,>bytes_per_effect
	adc a,0
	ld b,a

	ld a,(bc)
	add a,l	
	ld l,a
	ld a,h
	adc a,0
	ld h,a
	jp _fm_not_effect

_fm_done_this_one:

	pop bc
	ld a,c
	and a
	jp nz,_find_macros_loop
	ld a,b
	and a
	jp nz,_find_macros_loop
		
	;---------------------------------------------------------------------------
	; H - sort the table from smallest to largest, deleting identical indexes
	;---------------------------------------------------------------------------

	call _sort_index_table

	;---------------------------------------------------------------------------
	; I - optimize each pattern; remove redundant effects commands, convert waits
	;     to multi waits, etc.
	;---------------------------------------------------------------------------

	; well , this will be a pain in the ass, so for now I will just do multi-waits

	; to fix that: since everything is sorted, we should have it check the index of
	; the NEXT item in our table against BHL -> if they are ever the same, then 
	; we need to update that index to CDE (in the table and in all patterns that
	; point to that index, as well as the master table) and get the NEXT item
	; following that for the next check

	; what we can do instead of all that in this loop is create a table of addresses
	; to offset-> that might be an even better way to do it.

	; so what will happen is THIS:

	; 1) we need a subroutine to calc the address to check bhl against
	; 1) we are working backwards, so the first one should be null ($0000)

	; how is that for fun, fun, fun?

	; how about I hold off on that untill I code the defrag?

	; shit, this will certainly be fun

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

_find_patterns_loop:

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,c
	ld (file_access_next_index),a
	ld a,b
	ld (file_access_next_index+1),a

	call _get_file_access_index_data

	dec bc
	push bc

	sla c
	rl b
	sla c
	rl b

	ld a,b
	add a,$d0
	ld b,a

	; bc is the address of the pattern index

	ld a,(bc)
	ld l,a
	inc bc
	ld a,(bc)
	ld h,a

	; hl is our pattern index

	call convert_hl_index_to_bhl_address

	ld e,l
	ld d,h
	ld c,b

	; now we step through bhl untill we reach end or something to be optimized

	xor a
	ld (file_access_temp+0),a
	ld (file_access_temp+1),a

_op_not_effect:
	call _check_file_access_index
	call check_bhl_wram_read
_op_not_effect_2:
	call check_cde_wram_write
	bit 7,a
	jr z,_op_not_effect
	bit 6,a
	jr nz,_op_not_wait

	push af

	dec de
	ld a,d
	cp $cf
	jr nz,_no_op_de_dec_carry
	ld d,$df
	dec c
_no_op_de_dec_carry:

	pop af

_no_op_de_dec_carry_2:

	push hl

	and $3f
	inc a
	ld l,a
	ld a,(file_access_temp)
	add a,l
	ld (file_access_temp),a
	ld a,(file_access_temp+1)
	adc a,0
	ld (file_access_temp+1),a

	pop hl

	push de
	inc de
	call _check_file_access_index
	pop de
	and a
	jr z,_not_force_done

	call check_bhl_wram_read
	jr _op_store_wait_multi

_not_force_done:

	call check_bhl_wram_read
	bit 7,a
	jr z,_op_store_wait_multi
	bit 6,a
	jr z,_no_op_de_dec_carry_2
	
_op_store_wait_multi:
	
	ld (file_access_temp+2),a
	push hl

	ld a,(file_access_temp)
	ld l,a
	ld a,(file_access_temp+1)
	ld h,a

_op_w_loop:

	dec hl

	ld a,h
	and a
	jr nz,_op_hibyte_not_zero

	ld a,l
	cp $3f	
	jr z,_op_hibyte_zero
	jr nc,_op_hibyte_not_zero

_op_hibyte_zero:
	or $80
	call check_cde_wram_write

	pop hl

	xor a
	ld (file_access_temp+0),a
	ld (file_access_temp+1),a

	ld a,(file_access_temp+2)
	jr _op_not_effect_2

_op_hibyte_not_zero:

	ld a,$bf
	call check_cde_wram_write
	ld a,l
	sub $3f
	ld l,a
	ld a,h
	sbc a,0
	ld h,a
	jr _op_w_loop	

_op_not_wait:
	cp ctrlbt_track_loop
	jp z,_op_done_this_one

	and $3f
	add a,<bytes_per_effect

	push bc

	ld c,a
	ld a,>bytes_per_effect
	adc a,0
	ld b,a

	ld a,(bc)
	ld (file_access_temp+0),a

	pop bc

_op_fx_copy_loop:
	and a
	jp z,_op_not_effect

	call _check_file_access_index
	call check_bhl_wram_read
	call check_cde_wram_write

	ld a,(file_access_temp+0)
	dec a
	ld (file_access_temp+0),a
	jr _op_fx_copy_loop

_op_done_this_one:
	
	pop bc

	ld a,c
	and a
	jp nz,_find_patterns_loop
	ld a,b
	and a
	jp nz,_find_patterns_loop

	; now we should do the fun thing of updating all indexes...  YAY!

	call _update_indexes

	;---------------------------------------------------------------------------
	; J - search for duplicate patterns and delete redundancies
	;	- pattern indexes, macro indexes in patterns
	;---------------------------------------------------------------------------

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

	ld hl,$d000-4

_find_duplicates_loop:
	ld a,c
	and a
	jr nz,_fdl_cont
	ld a,b
	and a
	jp z,_fdl_done
_fdl_cont:

	dec bc
	push bc

	inc hl
	inc hl
	inc hl
	inc hl

	push hl

	ld e,l
	ld d,h

_find_duplicates_sub_loop:
	ld a,c
	and a
	jr nz,_fdsl_cont
	ld a,b
	and a
	jp z,_fdsl_done
_fdsl_cont:
	dec bc
	push bc

	inc de
	inc de
	inc de
	inc de

	push de

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(de)
	ld c,a
	inc de
	ld a,(de)
	ld d,a
	ld e,c

	ld a,(hli)
	ld h,(hl)
	ld l,a

	call convert_hl_index_to_bhl_address
	call convert_de_index_to_cde_address

	; cde and bhl are our 2 blocks to compare

	xor a
	ld (file_access_temp+2),a ; number of effects bytes

_fdsl_inner_loop:
	call check_bhl_wram_read
	ld (file_access_temp),a

	ld a,d
	cp $e0
	jr nz,_fdsl_no_carry
	ld d,$d0
	inc c
_fdsl_no_carry:
	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a

	push bc
	ld a,(de)
	inc de
	ld c,a
	ld a,(file_access_temp)
	cp c
	pop bc
	jr nz,_fdsl_not_equal

	ld (file_access_temp+1),a

	ld a,(file_access_temp+2)
	and a
	jr z,_fdsl_not_effect
	dec a
	ld (file_access_temp+2),a
	jr _fdsl_inner_loop

_fdsl_not_effect:

	ld a,(file_access_temp+1)

	bit 7,a
	jr z,_fdsl_inner_loop
	bit 6,a
	jr z,_fdsl_inner_loop

	cp ctrlbt_track_loop
	jr z,_fdsl_track_end

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
	jr _fdsl_inner_loop

	


_fdsl_track_end:

	; they are the same!

	pop de
	pop bc
	pop hl

	push hl
	push bc
	push de

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(hli)
	ld (de),a
	inc de	
	ld a,(hl)
	ld (de),a

_fdsl_not_equal:

	pop de
	pop bc
	pop hl
	push hl
	jp _find_duplicates_sub_loop

_fdsl_done:

	pop hl
	pop bc
	jp _find_duplicates_loop

_fdl_done:

	call _update_indexes
	call _sort_index_table

	;---------------------------------------------------------------------------
	; K - search for duplicate pattern parts and convert to macros
	;	- update size, pattern indexes, macro indexes in patterns
	;---------------------------------------------------------------------------
	; there is no way in hell I am going to get into this right now
	;---------------------------------------------------------------------------

	;---------------------------------------------------------------------------
	; L - defragment song memory
	;	- update size, pattern indexes, macro indexes in patterns
	;---------------------------------------------------------------------------

	; NOW we should calculate the end addresses?

	; now what we have to do is work backwards -> if there is a space between
	; the beginning of the current and the end of the previous, delete it

	; each time we do that we also have to update all our indexes, plus our
	; song size

	; by working backwards, we do not have to keep the ends up to date

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

	and a
	jr nz,_fe_cont_a
	ld a,c
	and a
	jp z,_fe_done

_fe_cont_a:

	; just find the end of the last one

	call _fe_track_end_find

	; de is the end of the previous block

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,e
	ld ($d004),a
	ld a,d
	ld ($d005),a

	push bc

	call update_optimize_disp

	pop bc

_find_ends_loop:
	ld a,c
	and a
	jr nz,_fe_cont
	ld a,b
	and a
	jr z,_fe_done
_fe_cont:

	call _fe_track_end_find

	push bc
	inc bc

	sla c
	rl b
	sla c
	rl b

	ld a,b
	add a,$d0
	ld b,a

	; de is the end of the previous block
	; (bc) is the start of the next block

	call _fee_delete

_fe_done_delete:

	pop bc
	jr _find_ends_loop

_fe_done:

	;---------------------------------------------------------------------------
	; M - search for redundant indexes and delete them
	;---------------------------------------------------------------------------

_red_ind_restart:

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d006)
	ld c,a
	and a
	jp z,_done_red_ind

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a

	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl

	dec c
	ld b,$ff

_red_ind_loop:
	ld a,c
	and a
	jp z,_done_red_ind

	inc b
	dec c
	push bc

	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl

	push hl

	ld e,l
	ld d,h

_red_ind_sub_loop:
	ld a,c
	and a
	jp z,_done_red_ind_sub

	inc b
	dec c
	push bc

	inc de
	inc de
	inc de
	inc de
	inc de
	inc de
	inc de
	inc de

	push de

	ld a,8
	ld (file_access_temp),a

	call convert_hl_index_to_bhl_address
	call convert_de_index_to_cde_address

_red_ind_comp_loop:

	call check_bhl_wram_read
	ld (file_access_temp+1),a

	ld a,d
	cp $e0
	jr nz,_redicl_no_carry
	ld d,$d0
	inc c
_redicl_no_carry:
	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a

	push bc
	ld a,(de)
	inc de
	ld c,a
	ld a,(file_access_temp+1)
	cp c
	pop bc
	jp nz,_redicl_not_equal

	ld a,(file_access_temp)
	dec a
	ld (file_access_temp),a
	jr nz,_red_ind_comp_loop

	; they are equal

	; while we have this set up, we should delete the index

	pop de

	call convert_de_index_to_cde_address

	pop hl
	push hl

	; l is the counter

	ld a,l
	inc a
	ld (file_access_temp),a

	; bhl is our source

	ld a,e
	add a,8
	ld l,a
	ld a,d
	adc a,0
	ld h,a
	ld b,c

_ridcl_del_ind_loop:

	ld a,(file_access_temp)
	and a
	jr z,_ridcl_done_del_ind_loop
	dec a
	ld (file_access_temp),a

	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write
	call check_bhl_wram_read
	call check_cde_wram_write

	jr _ridcl_del_ind_loop

_ridcl_done_del_ind_loop:

	; well, luckily we can pop de to get the address of the one to delete,
	; and we can pop bc, c is the number of remaining ones to move back, and
	; b is the index of this pattern

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	; also decrement our number of indexes counter

	ld a,($d006)
	dec a
	ld ($d006),a
	
	; ok, now for fun: search through sequence and decrement all patterns past
	; current by one

	; however, we have to change the current one to be the one it is the same as

	pop bc
	pop hl
	pop hl
	
	; so h is the old pattern
	; b is the new pattern
	ld c,h
	; c is the old pattern

	ld a,($d000)
	ld e,a
	ld a,($d001)
	add a,$d0
	ld d,a

	dec de

_ricl_pattern_decr_loop:
	inc de
	call check_de_wram_read
	ld a,(de)
	cp first_seq_effect
	jr nc,_ricl_seq_effect

	; flag pat header as used

	cp b
	jr c,_ricl_pattern_decr_loop
	jr nz,_ricl_decr

	ld a,c
	inc a

_ricl_decr:
	dec a
	ld (de),a
	jr _ricl_pattern_decr_loop

_ricl_seq_effect:

	cp ctrlbt_track_loop
	jp z,_red_ind_restart

	and $3f
	add a,<bytes_per_effect
	ld l,a
	ld a,>bytes_per_effect
	adc a,0
	ld h,a

	ld a,(hl)
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	jr _ricl_pattern_decr_loop

_redicl_not_equal:

	; at this point, we need to loop again

	pop de
	pop bc
	pop hl
	push hl
	jp _red_ind_sub_loop
	
_done_red_ind_sub:

	pop hl
	pop bc
	jp _red_ind_loop

_done_red_ind:

	;---------------------------------------------------------------------------
	; I - delete space between last pat index and patterns
	;---------------------------------------------------------------------------

	; number of pattern indexes * 8

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d006)
	ld l,a
	ld h,0
	sla l
	rl h
	sla l
	rl h
	sla l
	rl h

	ld a,($d002) ; + pattern offset
	ld e,a
	ld a,($d003)
	ld d,a
	add hl,de
	ld e,l
	ld d,h

	; de is the end of the previous block
	; (bc) is the start of the next block

	ld bc,$d000

	call _fee_delete	

	;---------------------------------------------------------------------------
	;---------------------------------------------------------------------------
	;---------------------------------------------------------------------------
	;---------------------------------------------------------------------------

	ld de,$9c27
	ld hl,done_optimize_text
	ld b,done_optimize_text_len

_fin_copy_loop:
	call copy_hl_de_vram
	dec b
	jr nz,_fin_copy_loop

	; clear last edit track

	ld a,$ff
	ld (last_edit_track),a
	ld (last_edit_patt),a

_loop:
	call joy_update
	ld a,(joy_pressed)
	and joy_a
	jr z,_loop

	ld a,(file_info_win_flag)
	ld (draw_time_flag),a

	ret

;***************************************************************************

_fe_track_end_find:
	dec bc
	push bc

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	sla c
	rl b
	sla c
	rl b
	ld a,b
	add a,$d0
	ld h,a
	ld l,c

	ld a,(hli)
	ld h,(hl)
	ld l,a

	call convert_hl_index_to_bhl_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

_fe_track_loop:

	call check_hl_wram_read
	ld a,(hli)
	bit 7,a
	jr z,_fe_track_loop
	bit 6,a
	jr z,_fe_track_loop

	cp ctrlbt_track_loop
	jr z,_fe_end_tr	

	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,>bytes_per_effect
	adc a,0
	ld d,a

	ld a,(de)
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a
	jr _fe_track_loop

_fe_end_tr:
	call check_hl_wram_read
	ld e,l

	ld a,h
	and $0f
	ld d,a

	ld a,(current_wram_bank)
	sub music_buffer_start_bank
	and $0f
	swap a
	or d
	ld d,a

	; de is now the end of this item

	pop bc
	ret

;***************************************************************************

_update_indexes:

	; first thing we need to do is get the number of sequence thingies

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d006)
	ld c,a
	ld b,0
	sla c
	rl b
	sla c
	rl b

	; bc is now the number of sequence thingies

	; now for each sequence thingy, we need to test it against all of our indexes

_update_indexes_header_loop:
	ld a,c
	and a
	jr nz,_uihl_cont
	ld a,b
	and a
	jr z,_uihl_done
_uihl_cont:

	dec bc
	push bc
	sla c
	rl b
	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a
	add hl,bc

	call _update_index_against

	pop bc
	jr _update_indexes_header_loop

_uihl_done:

	; now we have to scan all of our macro patterns indexes for poopypants dancers

	ld a,(file_access_num_macros+0)
	ld c,a
	ld a,(file_access_num_macros+1)
	ld b,a

_update_indexes_macro_loop:
	ld a,c
	and a
	jr nz,_uiml_cont
	ld a,b
	and a
	jr z,_uiml_done
_uiml_cont:

	dec bc
	push bc

	sla c
	rl b

	ld hl,editor_sequence_buffer
	add hl,bc

	ld de,_uiml_done_this

	push de
	push hl
	push bc

	ld a,(hli)
	ld e,a
	ld a,(hl)
	ld d,a
	
	jp _update_index_against_ram

_uiml_done_this:

	; ok, so now we have the index in the table corrected

	ld a,(hld)
	ld e,(hl)
	ld d,a

	ld a,($d002)
	add a,e
	ld e,a
	ld a,($d003)
	adc a,d
	ld d,a

	ld a,e
	ld (hli),a
	ld a,d
	ld (hl),a

	; now we have to scan all of our macro patterns for macros regarding poopypants dancers
	; now to parse the pattern and look for macro commands
	
	ld l,e
	ld h,d

	call convert_hl_index_to_bhl_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

_uimlp_loop:
	call check_hl_wram_read
	ld a,(hli)
	bit 7,a
	jr z,_uimlp_loop
	bit 6,a
	jr z,_uimlp_loop
	cp ctrlbt_macro
	jr nz,_uimlp_not_macro

	ld a,(current_wram_bank)
	ld b,a
	push bc
	push hl

	call _update_index_against_2

	pop hl
	pop bc
	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	inc hl
	inc hl

	jr _uimlp_loop

_uimlp_not_macro:

	cp ctrlbt_track_loop
	jr z,_uimlp_track_done
	
	and $3f
	add a,<bytes_per_effect
	ld e,a
	ld a,>bytes_per_effect
	adc a,0
	ld d,a

	ld a,(de)
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a
	jr _uimlp_loop

_uimlp_track_done:

	pop bc
	jr _update_indexes_macro_loop

_uiml_done:

	; now we have to reset all of our index thingies

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

_uiri_loop:
	ld a,c
	and a
	jr nz,_uiri_cont
	ld a,b
	and a
	jr z,_uiri_done
_uiri_cont:
	dec bc
	push bc

	sla c
	rl b
	sla c
	rl b
	ld a,b
	add a,$d0
	ld h,a
	ld l,c

	ld a,(hli)
	ld c,a
	ld a,(hli)
	ld b,a
	ld a,c
	ld (hli),a
	ld a,b
	ld (hl),a

	pop bc
	jr _uiri_loop

_uiri_done:

	ret

;***************************************************************************

_update_index_against:

	; the index of the index to test is in hl

	; we have to test it against everything in our table
	
	call convert_hl_index_to_bhl_address

_update_index_against_2:

	push hl
	push bc

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(hli)
	ld e,a
	call check_hl_wram_read
	ld d,(hl)

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a

	add hl,de

	ld e,l
	ld d,h

_update_index_against_ram:

	; de is now the index to look for

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

_uia_loop:
	ld a,c
	and a
	jr nz,_uia_cont
	ld a,b
	and a
	jr z,_uia_done
_uia_cont:
	dec bc
	push bc

	sla c
	rl b
	sla c
	rl b
	ld a,b
	add a,$d0
	ld h,a
	ld l,c

	inc hl
	inc hl

	; hl is the current number to test

	ld a,(hli)
	cp e
	jr nz,_uia_not_same
	ld a,(hld)
	cp d
	jr nz,_uia_not_same

	ld e,l
	ld d,h

	dec de
	dec de

	; ok, we found it!  now store it!

	pop hl
	pop hl

	ld b,h
	pop hl

	ld a,(de)
	ld c,a
	inc de
	ld a,(de)
	ld d,a
	ld e,c

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld c,a
	ld a,e
	sub c
	ld e,a

	ld a,($d003)
	ld c,a
	ld a,d
	sbc a,c
	ld d,a

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,e
	ld (hli),a
	call check_hl_wram_read

	ld a,d
	ld (hl),a
	ret

_uia_not_same:
	pop bc
	jr _uia_loop

_uia_done:
	; it wasn't in the list- bad times!

	; we should be able to remove this part!!!!!!!!!!!

	ld b,b
	ld b,b
	ld b,b
	ld b,b

	pop hl

	ret

;***************************************************************************

_check_file_access_index:

	; check to see if bhl is equal to our next index

	ld a,(file_access_addr_check+2)
	cp b
	ld a,0
	ret nz

	ld a,(file_access_addr_check+1)
	cp h
	ld a,0
	ret nz

	ld a,(file_access_addr_check+0)
	cp l
	ld a,0
	ret nz

	; it is, so let's update the index relocate with cde

	push bc
	push hl
	push de

	ld a,d
	and $0f
	ld d,a

	ld a,c
	sub music_buffer_start_bank
	and $0f
	swap a
	or d
	ld d,a

	ld a,(file_access_next_index)
	ld c,a
	ld a,(file_access_next_index+1)
	ld b,a

	inc bc

	ld a,c
	ld (file_access_next_index),a
	ld a,b
	ld (file_access_next_index+1),a

	dec bc
	sla c
	rl b
	sla c
	rl b
	ld a,b
	add a,$d0
	ld b,a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,e
	ld (bc),a
	inc bc
	ld a,d
	ld (bc),a
	
	pop de
	jr _get_file_access_index_dat2

_get_file_access_index_data:

	push bc
	push hl

_get_file_access_index_dat2:

	ld a,(file_access_next_index)
	ld c,a
	ld a,(file_access_next_index+1)
	ld b,a

	ld a,(file_access_num_indexes+1)
	cp b
	jr z,_gfaid_same_hi_ind
	jr nc,_gfaid_lower_hi_ind

	jr _gfaid_higher_hi_ind

_gfaid_same_hi_ind:

	ld a,(file_access_num_indexes+0)
	cp c
	jr z,_gfaid_higher_hi_ind
	jr nc,_gfaid_lower_hi_ind

_gfaid_higher_hi_ind:

	xor a
	ld (file_access_addr_check),a
	ld (file_access_addr_check+1),a
	ld (file_access_addr_check+2),a

	inc a ; must have a>0 on exit

	pop hl
	pop bc

	ret

_gfaid_lower_hi_ind:

	sla c
	rl b
	sla c
	rl b
	ld a,b
	add a,$d0
	ld b,a

	; bc is the address of the pattern index

	ld a,(bc)
	ld l,a
	inc bc
	ld a,(bc)
	ld h,a

	; hl is our pattern index

	call convert_hl_index_to_bhl_address

	ld a,l
	ld (file_access_addr_check),a
	ld a,h
	ld (file_access_addr_check+1),a
	ld a,b
	ld (file_access_addr_check+2),a

	; must have a>0 on return, that should do it

	pop hl
	pop bc

	ret

;***************************************************************************

_sort_index_table:

	; E - sort the table from smallest to largest, deleting identical indexes

	; gee this is fun

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a
	
_sort_index_restart:

	ld hl,$d000-4

_sort_index_loop:	

	dec bc

	ld a,c
	and a
	jr nz,_sort_index_loop_c
	ld a,b
	and a
	jr nz,_sort_index_loop_c

	ret z

_sort_index_loop_c:

	push bc

	inc hl
	inc hl
	inc hl
	inc hl

	ld e,l
	ld d,h

_sort_index_sub_loop:

	push bc

	inc de
	inc de
	inc de
	inc de

	ld a,(hli)
	ld c,a
	ld a,(hld)
	ld b,a	

	inc de
	ld a,(de)
	dec de
	cp b
	jr z,_may_be_equal
	jr nc,_is_less_than
	jr _is_not_equal

_may_be_equal:

	ld a,(de)
	cp c
	jr z,_be_equal
	jr nc,_is_less_than
	jr _is_not_equal
	
_be_equal:

	; they are equal, delete one in DE and start over

	ld l,e
	ld h,d

	inc hl
	inc hl
	inc hl
	inc hl

	pop bc

	push hl

	sla c
	rl b

	ld l,c
	ld h,b

	sla c
	rl b

	add hl,bc
	ld c,l
	ld b,h

	pop hl

_sort_index_del_loop:

	ld a,(hli)
	ld (de),a
	inc de

	dec bc

	ld a,c
	and a
	jr nz,_sort_index_del_loop
	ld a,b
	and a
	jr nz,_sort_index_del_loop

	pop bc

	ld a,(file_access_num_indexes+0)
	ld c,a
	ld a,(file_access_num_indexes+1)
	ld b,a

	dec bc

	ld a,c
	ld (file_access_num_indexes+0),a
	ld a,b
	ld (file_access_num_indexes+1),a

	jr _sort_index_restart

_is_not_equal:

	; swap them

	ld a,(de)
	ld (hli),a
	ld a,c
	ld (de),a
	inc de
	ld a,(de)
	ld (hli),a
	ld a,b
	ld (de),a
	inc de

	ld a,(hli)
	ld c,a
	ld a,(hld)
	ld b,a	

	ld a,(de)
	ld (hli),a
	ld a,c
	ld (de),a
	inc de
	ld a,(de)
	ld (hld),a
	ld a,b
	ld (de),a
	dec de

	dec hl
	dec hl
	dec de
	dec de

	; and keep looking

_is_less_than:

	pop bc
	dec bc
	ld a,c
	and a
	jp nz,_sort_index_sub_loop
	ld a,b
	and a
	jp nz,_sort_index_sub_loop

	pop bc
	jp _sort_index_loop

;***************************************************************************

_fee_delete:

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	; de is the end of the previous block
	; (bc) is the start of the next block

	; what we should do is sub (bc)-de
	; if (bc)-de>0 then we delete the space

	; save pointer to start of next index

	ld a,c
	ld (file_access_temp_addr),a
	ld a,b
	ld (file_access_temp_addr+1),a

	ld a,(bc)
	ld l,a
	inc bc
	ld a,(bc)
	ld h,a

	; now hl=(bc)
	; so, hl-de

	ld a,l
	sub e
	ld c,a
	ld a,h
	sbc a,d
	ld b,a

	ret c

	; bc is the difference

	and a
	jr nz,_fe_delete
	ld a,c
	and a
	ret z

_fe_delete:

	; very simply, we can now sub bc from the song size

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d004)
	sub c
	ld ($d004),a

	ld a,($d005)
	sbc a,b
	ld ($d005),a

	; so, to recap:
	; de = index of end of previous
	; hl = index of start of next
	; bc = difference

	; we now need to do the indexes

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	push de
	push hl

	ld a,(file_access_temp_addr)
	ld l,a
	ld a,(file_access_temp_addr+1)
	ld h,a

	ld a,e
	ld (hli),a
	ld a,d
	ld (hli),a
	inc hl
	inc hl

	; ! wee have to subtract bc from all indexes PAST this one!

_fe_subtr_loop:
	ld a,(hl)
	sub c
	ld (hli),a
	ld a,(hl)
	sbc a,b
	ld (hli),a

	inc hl
	inc hl
	ld a,h
	cp $e0
	jr nz,_fe_subtr_loop

	call _update_indexes

	pop de
	pop hl	; note that we swap which is which

	call convert_hl_index_to_bhl_address
	call convert_de_index_to_cde_address

	; do the delete

	call move_wram_bhl_cde
	jp update_optimize_disp

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************


;***************************************************************************
; insert pattern copy
;***************************************************************************

insert_pattern_copy_over:

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a
	ld a,($d006)
	cp first_seq_effect
	jr nz,_ok_seq_num
	pop hl
	ret

_ok_seq_num:

	ld a,(editor_copy_patt)
	cp $ff
	jr nz,_good_copy_buffer
	ld a,(editor_copy_patt+1)
	cp $ff
	jr nz,_good_copy_buffer

	pop hl
	ret

_good_copy_buffer:

	; first, let the other routine do all the work

	call insert_pattern_over

	; then we should decrement the song size by 2 as the new pattern
	; that was added will be unused
	
	ld a,($d004)
	sub $02
	ld ($d004),a
	ld a,($d005)
	sbc a,0
	ld ($d005),a	

	; then we have to update the pointers in our pattern

	call get_patt_index_addr ; -> this should point us to the new pattern

	ld e,l
	ld d,h

	ld hl,editor_copy_patt

	call convert_de_index_to_cde_address

	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	call check_cde_wram_write
	ld a,(hli)
	jp check_cde_wram_write

;***************************************************************************
; insert pattern
;***************************************************************************

insert_pattern_over:

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a
	ld a,($d006)
	cp first_seq_effect
	jr nz,_ok_seq_num
	pop hl
	ret

_ok_seq_num:

	; what this will do is insert a new blank pattern at the end of the song
	; it also has to do some other funky shit like insert the pattern 
	; header, and add 8 to all pattern headers, macro jumps, and 
	; undo addresses.  Yeah, fun.

	; ok, now for blank pattern

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	; first increment song size by 10

	ld a,($d004)
	ld l,a
	add a,$0a
	ld ($d004),a
	ld a,($d005)
	ld h,a
	adc a,0
	ld ($d005),a	

	; first, insert blank pattern at end

	; hl is the index of where to add blank pattern

	push hl ; save for later -> it's the number we put in the header as well

	call convert_hl_index_to_bhl_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,$bf
	ld (hli),a
	call check_hl_wram_read
	ld a,ctrlbt_track_loop
	ld (hli),a
	call check_hl_wram_read

	push hl
	
	ld a,(current_wram_bank)
	ld b,a
	
	; ok, our blank pattern is in there.

	; now, we have to insert 8 bytes at the end of our pattern table

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a

	ld a,($d006)	
	ld e,a
	inc a		; increment our pattern counter while we're at it
	ld ($d006),a

	ld d,0
	sla e
	rl d
	sla e
	rl d
	sla e
	rl d

	add hl,de
	ld e,l
	ld d,h

	dec de

	; hl is the index of where to insert our new information

	call convert_de_index_to_cde_address ; cde is our stop area

	pop hl
	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

_insert_header:
	ld a,(hli)
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	push af
	call check_hl_wram_read
	pop af
	ld (hld),a
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	call check_hl_wram_back

	ld a,l
	cp e
	jr nz,_insert_header
	ld a,h
	cp d
	jr nz,_insert_header
	ld a,(current_wram_bank)
	cp c
	jr nz,_insert_header

	; now store our header

	pop de
	ld a,(current_wram_bank)
	push af
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld c,a
	ld a,($d003)
	ld b,a

	ld a,e
	sub c
	ld e,a
	ld a,d
	sbc a,b
	ld d,a

	pop af
	ld (current_wram_bank),a
	ld ($ff70),a

	inc hl
	call check_hl_wram_read

	ld a,e
	ld (hli),a
	call check_hl_wram_read
	ld a,d
	ld (hli),a
	call check_hl_wram_read
	
	ld a,e
	ld (hli),a
	call check_hl_wram_read
	ld a,d
	ld (hli),a
	call check_hl_wram_read

	ld a,e
	ld (hli),a
	call check_hl_wram_read
	ld a,d
	ld (hli),a
	call check_hl_wram_read

	ld a,e
	ld (hli),a
	call check_hl_wram_read
	ld a,d
	ld (hl),a

	; now we have the awesome task of adding 8 to all of our headers, macros and undos

	; bc is our number of thingies to do

	xor a
	ld (file_access_temp),a
	ld a,$d0
	ld (file_access_temp+1),a ; address of first patterns already tested

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d006)
	ld c,a
	ld b,0
	sla c
	rl b
	sla c
	rl b

_update_headers_loop:
	ld a,c
	and a
	jr nz,_not_done_headers
	ld a,b
	and b
	jr z,_done_headers
	
_not_done_headers:

	dec bc
	push bc

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a

	sla c
	rl b
	add hl,bc

	; so hl is the index of the pattern to check

	call convert_hl_index_to_bhl_address

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	call _check_the_pattern

	pop bc
	jr _update_headers_loop
	
_done_headers:

	; now we have to update all the undos

	ld a,(editor_num_undos)
	ld c,a
	ld a,(editor_num_redos)
	add a,c
	ld c,a

	ld b,0

_update_undos_loop:
	ld a,c
	and a
	jr nz,_not_done_undos
	ld a,b
	and b
	jr z,_done_undos
	
_not_done_undos:

	dec bc
	push bc

	sla c
	rl b
	sla c
	rl b
	
	ld hl,editor_undo_buffer+2
	add hl,bc

	; damnit we also have to test each undo as weell, damn that sucks

	call _check_the_pattern

	pop bc

	jr _update_undos_loop

_done_undos:

	; shit, what about our copy buffer?  how about we FIX THAT NOW?!

	ld a,(editor_copy_from)
	add a,8
	ld (editor_copy_from),a
	ld a,(editor_copy_from+1)
	adc a,0
	ld (editor_copy_from+1),a

	ld a,(editor_copy_patt+0)
	add a,8
	ld (editor_copy_patt+0),a
	ld a,(editor_copy_patt+1)
	adc a,0
	ld (editor_copy_patt+1),a

	ld a,(editor_copy_patt+2)
	add a,8
	ld (editor_copy_patt+2),a
	ld a,(editor_copy_patt+3)
	adc a,0
	ld (editor_copy_patt+3),a

	ld a,(editor_copy_patt+4)
	add a,8
	ld (editor_copy_patt+4),a
	ld a,(editor_copy_patt+5)
	adc a,0
	ld (editor_copy_patt+5),a

	ld a,(editor_copy_patt+6)
	add a,8
	ld (editor_copy_patt+6),a
	ld a,(editor_copy_patt+7)
	adc a,0
	ld (editor_copy_patt+7),a

	; AND we still have to do the annoying shit of inserting it into the sequence

	call get_seq_position

	ld a,(current_wram_bank)
	ld c,a

	call insert_byte_at_cde

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	ld l,a
	ld a,($d003)
	ld h,a
	inc hl
	ld a,l
	ld ($d002),a
	ld a,h
	ld ($d003),a

	ld a,($d006)	
	dec a
	ld b,a

	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,b
	ld (de),a

	; clear last edit track

	ld a,$ff
	ld (last_edit_track),a
	ld (last_edit_patt),a

	ret

;***************************************************************************

_check_the_pattern:

	; this does a whole bunch of stuff, first it updates the value,
	; then it reads through the pattern for macros,
	; then if it finds a macro, it tests against a list of previously changed
	; macros, and if it is unique, it updates it.

	; GUESS WHAT?  IT FUCKS UP ON MACROS

	; GUESS WHAT? IT FUCKS UP ON LOTS OF THINGS

	ld a,(hli)
	ld e,a
	call check_hl_wram_read
	ld a,(hld)
	ld d,a
	call check_hl_wram_back

	ld a,e
	add a,8
	ld (hli),a
	ld a,d
	adc a,0
	ld d,a
	call check_hl_wram_read
	ld a,d
	ld (hl),a

	ret


; here's what I had before I decided to rip out all the macro bullshit
;
;	ld a,(hl)
;	add a,$08
;	ld (hli),a
;	ld e,a
;	push af
;	call check_hl_wram_read
;	pop af
;	ld a,(hl)
;	adc a,0
;	ld (hl),a
;	ld d,a
;
;	ld l,e
;	ld h,d
;
;	call convert_hl_index_to_bhl_address
;
;	ld a,b
;	ld (current_wram_bank),a
;	ld ($ff70),a
;
;_patt_fix_loop:
;
;	call check_hl_wram_read
;	ld a,(hli)
;	bit 7,a
;	jr z,_patt_fix_loop
;	bit 6,a
;	jr z,_patt_fix_loop
;
;	cp ctrlbt_macro
;	jr nz,_not_macro
;
;	LD B,B
;
;	; ok, we had better test this address against our table of macros
;
;	ld a,(current_wram_bank)
;	push af
;
;	sub music_buffer_start_bank
;	and $0f
;	swap a
;	ld d,a
;	ld a,h
;	and $0f
;	or d
;	ld d,a
;	ld e,l
;
;	ld a,editor_block_buffer_bank
;	ld (current_rom_bank),a
;	ld ($ff70),a
;
;	ld a,(file_access_temp)
;	ld c,a
;	ld a,(file_access_temp+1)
;	ld b,a
;
;	push bc
;
;_prev_tested_check_loop:
;	pop bc
;
;	ld a,c
;	and a
;	jr nz,_prev_tested_check
;	ld a,b
;	cp $d0
;	jr nz,_prev_tested_check
;
;	; it's unique!
;
;	ld a,(file_access_temp)
;	ld c,a
;	ld a,(file_access_temp+1)
;	ld b,a
;
;	ld a,e
;	ld (bc),a
;	inc bc
;	ld a,d
;	ld (bc),a
;	inc bc
;
;	ld a,c
;	ld (file_access_temp),a
;	ld a,b
;	ld (file_access_temp+1),a
;
;	pop af
;	ld (current_wram_bank),a
;	ld ($ff70),a
;
;	jr _done_prev_tested_check
;
;_prev_tested_check:
;
;	dec bc
;	dec bc
;
;	push bc
;
;	ld a,(bc)
;	cp e
;	jr nz,_prev_tested_check_loop
;	inc bc
;	ld a,(bc)
;	cp d
;	jr nz,_prev_tested_check_loop
;
;	; ok it's the same as something, exit the loop
;
;	pop bc
;	pop af
;	ld (current_wram_bank),a
;	ld ($ff70),a
;
;	ld a,2
;	jr _ptc_exit_same
;
;_done_prev_tested_check:	
;	
;	call check_hl_wram_read
;	ld a,(hl)
;	add a,$08
;	ld (hli),a
;	push af
;	call check_hl_wram_read
;	pop af
;	ld a,(hl)
;	adc a,0
;	ld (hli),a
;	jr _patt_fix_loop
;
;_not_macro:
;	cp ctrlbt_track_loop
;	ret z
;
;	and $3f
;	add a,<bytes_per_effect
;	ld e,a
;	ld a,>bytes_per_effect
;	adc a,0
;	ld d,a
;
;	ld a,(de)
;_ptc_exit_same:
;	add a,l
;	ld l,a
;	ld a,h
;	adc a,0
;	ld h,a
;	jp _patt_fix_loop	


;***************************************************************************
; get_sram_song_address_hl_bc_e: gets addr in hl, len in bc )with undoes(, bank in a,
; and number of undoes in d for good measure
; call with index in a
;***************************************************************************

get_sram_song_address_hl_bc_ed_o:

	call get_sram_song_address_hl_bc_e_ov

	; gee, we should add in the undo info, eh?

	push hl
	add hl,bc

	ld d,e
	ld a,h

_addr_adj_loop:
	and $e0
	cp $a0
	jr z,_done_addr_adj

	ld a,h
	sub $20
	ld h,a
	inc d
	jr _addr_adj_loop
_done_addr_adj:

	; hl is source address
	; d is source bank

	ld a,d
	ld (current_sram_bank),a
	ld ($4666),a

	; and why not return the number of undos in d?

	ld a,(hli)
	ld d,a
	call check_hl_sram_read
	ld a,(hl)
	add a,d
	ld d,a

	inc bc
	inc bc

	push de

	ld d,0
	sla a
	rl d
	sla a
	rl d
	
	add a,c
	ld c,a
	ld a,d
	adc a,b
	ld b,a

	pop de
	
	pop hl

	ret

;***************************************************************************
; get_sram_song_address_hl_bc_e: gets addr in hl, len in bc, bank in a,
; call with index in a
;***************************************************************************

get_sram_song_address_hl_bc_e_ov:

	; song number to load is in A
	; store it is an index in BC

	ld c,a
	ld b,0

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	; set our header bank

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; get the song start bank into E

	ld hl,song_save_bank_table
	add hl,bc

	ld e,(hl)

	; convert BC into word index

	sla c
;	rl b

	; get the song start address into HL

	ld hl,song_save_header_table
	add hl,bc
	ld a,(hli)
	ld h,(hl)
	ld l,a

	; set our source bank

	ld a,e
	ld (current_sram_bank),a
	ld ($4666),a

	;---------------------------------------------------------------------------
	; get our song length into bc

	push hl

	ld bc,$0004
	add hl,bc
	call check_hl_sram_read
	ld a,(hli)
	ld c,a
	call check_hl_sram_read
	ld b,(hl)

	pop hl

	ret



	end

