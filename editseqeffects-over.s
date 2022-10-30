;---------------------------------------------------------------------------
; seq effects editor menu
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_edit_seq_efx_over:
; edit_seq_efx_poop_loop_over:
; edit_seq_effects_reset_data_ov:
; edit_seq_effects_save_data_ov:
; delete_seq_effects_at_cursor:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_edit_seq_efx_over:

	call get_seq_position

	; returns address to effects prior to pattern index index in DE
	; returns address to pattern index index in HL
	; returns the pattern index index in A
	; alters seq position if bad

	ld a,edit_seq_effects_mode
	ld (editor_mode),a

	ld a,1
	ld (data_entry_data_mode),a

	ld a,<efx_seq_sel_info_table
	ld (data_entry_sel_info_table),a
	ld a,>efx_seq_sel_info_table
	ld (data_entry_sel_info_table+1),a

	ld a,<efx_seq_x_sel_num_table
	ld (data_entry_x_sel_num_table),a
	ld a,>efx_seq_x_sel_num_table
	ld (data_entry_x_sel_num_table+1),a

	ld a,<efx_seq_sel_idx_table
	ld (data_entry_sel_idx_table),a
	ld a,>efx_seq_sel_idx_table
	ld (data_entry_sel_idx_table+1),a

	ld a,<efx_seq_data_type_table
	ld (data_entry_data_type_table),a
	ld a,>efx_seq_data_type_table
	ld (data_entry_data_type_table+1),a

	ld a,<efx_seq_group_table
	ld (data_entry_groups_table),a
	ld a,>efx_seq_group_table
	ld (data_entry_groups_table+1),a

	call edit_seq_effects_reset_data_ov

	ld a,(seq_efx_x_menu_sel)
	ld (data_entry_x_menu_sel),a

	ld a,(seq_efx_menu_sel)
	ld b,a

	ld a,5
	ld de,seq_effects_menu_gbc_map
	jp data_entry_menu_setup

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects poop loop
;---------------------------------------------------------------------------
;***************************************************************************

edit_seq_efx_poop_loop_over:

	call data_entry_menu_cursor

	ld a,(joy_pressed)
	bit joy_bit_select,a
	jr z,_not_select

	ld a,(joy_held)
	and joy_a+joy_b
	cp joy_a+joy_b
	jr z,_not_select

	bit joy_bit_a,a
	jr nz,_save

	bit joy_bit_b,a
	jr nz,_exit

_not_select:

	ld a,(data_entry_menu_mode)
	cp data_entry_type_first_button
	jp nc,_buttons

	and a
	jp z,poop

	ld a,(data_entry_x_menu_sel)
	ld (seq_efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (seq_efx_menu_sel),a

	jp poop

_buttons:

	cp data_entry_type_save_button
	jr nz,_not_save
_save:
	call edit_seq_effects_save_data_ov
_exit:
	call setup_edit_seq
	jr _done_button_process

_not_save:
	cp data_entry_type_reset_button
	jr nz,_not_reset

	call edit_seq_effects_reset_data_ov
	call data_entry_redraw_data

_done_button_process:
	xor a
	ld (data_entry_menu_mode),a
	jp poop

_not_reset:

	cp data_entry_type_user_button_0
	jr nz,_not_copy

	ld a,(data_entry_x_menu_sel)
	ld (seq_efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (seq_efx_menu_sel),a

	ld c,$d0
	ld hl,effects_copy_buffer
	xor a
_wipe_loop:
	ld (hli),a
	dec c
	jr nz,_wipe_loop

	; copy panning

	ld hl,data_entry_work_ram
	ld c,$10
	ld de,effects_copy_buffer+$40
_copy_pan_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_copy_pan_loop

	; copy speed/shuffle/transpose

	ld c,$20
	ld de,effects_copy_buffer+$a0
_copy_speed_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_copy_speed_loop

	jr _done_button_process

_not_copy:

	cp data_entry_type_user_button_1
	jr nz,_not_paste

	ld a,(data_entry_x_menu_sel)
	ld (seq_efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (seq_efx_menu_sel),a

	; paste panning

	ld a,(effects_copy_buffer+$040)
	ld (data_entry_work_ram+$000),a
	ld a,(effects_copy_buffer+$041)
	ld (data_entry_work_ram+$001),a
	ld a,(effects_copy_buffer+$042)
	ld (data_entry_work_ram+$002),a
	ld a,(effects_copy_buffer+$043)
	ld (data_entry_work_ram+$003),a
	ld a,(effects_copy_buffer+$04e)
	and %00001111
	ld (data_entry_work_ram+$00e),a

	; paste speed

	ld hl,effects_copy_buffer+$a0
	ld de,data_entry_work_ram+$10
	ld c,$10
_paste_speed_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_paste_speed_loop

	; paste transpose

;	ld a,(effects_copy_buffer+$0b0)
	ld a,(hl)
	ld (data_entry_work_ram+$020),a
	ld a,(effects_copy_buffer+$0be)
	and %00000001
	ld (data_entry_work_ram+$02e),a

	call data_entry_redraw_data

	jp _done_button_process

_not_paste:

	cp data_entry_type_user_button_2
	jr nz,_not_mutate

	ld a,(data_entry_x_menu_sel)
	ld (efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (efx_menu_sel),a

	call mutate_effects
	call data_entry_redraw_data

	jp _done_button_process

_not_mutate:

	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; edit seq effects reset data
;---------------------------------------------------------------------------
;***************************************************************************

edit_seq_effects_reset_data_ov:

	; wipe ram table

	ld hl,data_entry_work_ram
	ld bc,$0260
	xor a

_wipe:
	ld (hli),a
	dec c
	jr nz,_wipe
	dec b
	jr nz,_wipe

	; now read effects from song

	call get_seq_position

	; returns address to effects prior to pattern index index in DE
	; returns address to pattern index index in HL
	; returns the pattern index index in A
	; alters seq position if bad

	ld l,e
	ld d,h

_read_effects_loop:
;	call check_hl_wram_read
	ld a,(hli)
	cp first_seq_effect
	ret c
	
	; line one
	; panning

	cp ctrlbt_panning
	jr nz,_not_ctrlbt_panning

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$000),a

	sra e
	ld a,e
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$001),a

	sra e
	ld a,e
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$002),a

	sra e
	ld a,e
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$003),a

	ld a,(data_entry_work_ram+$00e)
	or %00001111
	ld (data_entry_work_ram+$00e),a

	jp _read_effects_loop

_not_ctrlbt_panning:

	; line two
	; song speed/shuffle

	cp ctrlbt_song_speed
	jr nz,_not_ctrlbt_song_speed

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$010),a

	ld a,(data_entry_work_ram+$01e)
	or %00000001
	ld (data_entry_work_ram+$01e),a

	jp _read_effects_loop

_not_ctrlbt_song_speed:
	
	cp ctrlbt_shuffle
	jr nz,_not_ctrlbt_shuffle

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$011),a

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	and $0f
	ld (data_entry_work_ram+$012),a

	ld a,e
	swap a
	and $0f
	ld (data_entry_work_ram+$013),a

	ld a,(data_entry_work_ram+$01e)
	or %00001110
	ld (data_entry_work_ram+$01e),a

	jp _read_effects_loop

_not_ctrlbt_shuffle:

	; line three
	; transpose

	cp ctrlbt_transpose
	jr nz,_not_ctrlbt_transpose

	call check_hl_wram_read
	ld a,(hli)	
	add a,$80
	ld (data_entry_work_ram+$020),a

	ld a,(data_entry_work_ram+$02e)
	or %00000001
	ld (data_entry_work_ram+$02e),a

	jp _read_effects_loop

_not_ctrlbt_transpose:

	; other unsupported effect

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
	jp _read_effects_loop

;***************************************************************************
;---------------------------------------------------------------------------
; edit seq effects save data
;---------------------------------------------------------------------------
;***************************************************************************

edit_seq_effects_save_data_ov:

	call delete_seq_effects_at_cursor

	; ok, now we need to scan through all our effects and save them if necessary... fun.

	call get_seq_position

	; returns address to effects prior to pattern index index in DE
	; returns address to pattern index index in HL
	; returns the pattern index index in A
	; alters seq position if bad

	ld a,(current_wram_bank)
	ld b,a
	ld c,a

	; line one
	; panning

	ld a,(data_entry_work_ram+$00e)
	and %00001111
	jr z,_not_ctrlbt_panning

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_panning
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back

	ld a,(data_entry_work_ram+$000)
	and %0_0_0_0_0_0_0_1
	ld b,a
	ld a,(data_entry_work_ram+$000)
	and %0_0_0_0_0_0_1_0
	sra a
	swap a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$001)
	and %0_0_0_0_0_0_0_1
	sla a
	or b
	ld b,a
	ld a,(data_entry_work_ram+$001)
	and %0_0_0_0_0_0_1_0
	swap a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$002)
	and %0_0_0_0_0_0_0_1
	sla a
	sla a
	or b
	ld b,a
	ld a,(data_entry_work_ram+$002)
	and %0_0_0_0_0_0_1_0
	swap a
	sla a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$003)
	and %0_0_0_0_0_0_0_1
	sla a
	sla a
	sla a
	or b
	ld b,a
	ld a,(data_entry_work_ram+$003)
	and %0_0_0_0_0_0_1_0
	swap a
	sla a
	sla a
	or b

	ld (hl),a
	call check_cde_wram_incr

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,2
	ld ($d002),a
	ld a,($d003)
	adc a,0
	ld ($d003),a

_not_ctrlbt_panning:

	; line two
	; song speed/shuffle

	ld a,(data_entry_work_ram+$01e)
	and %00000001
	jr z,_not_ctrlbt_song_speed

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_song_speed
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$010)
	ld (hl),a
	call check_cde_wram_incr

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,2
	ld ($d002),a
	ld a,($d003)
	adc a,0
	ld ($d003),a

_not_ctrlbt_song_speed:
	
	ld a,(data_entry_work_ram+$01e)
	and %00001110
	jr z,_not_ctrlbt_shuffle

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_shuffle
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$011)
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$012)
	ld b,a
	ld a,(data_entry_work_ram+$013)
	swap a
	or b
	ld (hl),a
	call check_cde_wram_incr

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,3
	ld ($d002),a
	ld a,($d003)
	adc a,0
	ld ($d003),a

_not_ctrlbt_shuffle:

	; line three
	; transpose

	ld a,(data_entry_work_ram+$02e)
	and %00000001
	jr z,_not_ctrlbt_transpose

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_transpose
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$020)
	sub $80
	ld (hl),a
	call check_cde_wram_incr

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	add a,2
	ld ($d002),a
	ld a,($d003)
	adc a,0
	ld ($d003),a

_not_ctrlbt_transpose:

	call update_song_info_disp
	jp redraw_song_info

;***************************************************************************
;---------------------------------------------------------------------------
; delete_seq_effects_at_cursor:
;---------------------------------------------------------------------------
;***************************************************************************

delete_seq_effects_at_cursor:

	; well, first we should delete all the effects that are in there,
	; then we should insert all our new effects.  Yes, that makes the
	; most sense.  Awesome, it will destroy any macros.  FUN FUN!

	call get_seq_position

	; returns address to effects prior to pattern index index in DE
	; returns address to pattern index index in HL
	; returns the pattern index index in A
	; alters seq position if bad

	ld a,(current_wram_bank)
	ld b,a
	ld c,a

	; ok hang on one sec, we need a danged length

	push hl
	push de

	ld a,h
	and $0f
	ld h,a
	ld a,b
	sub music_buffer_start_bank
	and $0f
	swap a
	or h
	ld h,a

	ld a,d
	and $0f
	ld d,a
	ld a,c
	sub music_buffer_start_bank
	and $0f
	swap a
	or d
	ld d,a

	ld a,l
	sub e
	ld e,a
	ld a,h
	sbc a,d
	ld d,a

;	and a
;	jr nz,_not_skip_delete
;	ld a,e
;	and a
;	jr z,_skip_delete
;
;_not_skip_delete:

	; de is our length
	; use it to update song length

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d002)
	sub e
	ld ($d002),a
	ld a,($d003)
	sbc a,d
	ld ($d003),a
		
	ld a,($d004)
	sub e
	ld ($d004),a
	ld (file_access_temp),a
	ld a,($d005)
	sbc a,d
	ld ($d005),a
	ld d,a
	and $0f
	or $d0
	ld (file_access_temp+1),a

	ld a,d
	and $f0
	swap a
	add a,music_buffer_start_bank
	ld (file_access_temp+2),a

	pop de
	pop hl

_delete_loop:
	; so here we copy from bhl to cde
	call check_bhl_wram_read
	call check_cde_wram_write

	ld a,(file_access_temp)
	cp e
	jr nz,_delete_loop
	ld a,(file_access_temp+1)
	cp d
	jr nz,_delete_loop
	ld a,(file_access_temp+2)
	cp c
	jr nz,_delete_loop

	; one more for good measure

	call check_bhl_wram_read
	jp check_cde_wram_write
