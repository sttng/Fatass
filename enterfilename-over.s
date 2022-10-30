;***************************************************************************
;---------------------------------------------------------------------------
; enter filename setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_enter_filename_over:

	ld a,enter_filename_mode
	ld (editor_mode),a

	xor a
	ld (data_entry_data_mode),a

	call update_song_and_file_info_disp
	ld a,1
	ld (draw_time_flag),a

	ld a,<en_fn_sel_info_table
	ld (data_entry_sel_info_table),a
	ld a,>en_fn_sel_info_table
	ld (data_entry_sel_info_table+1),a

	ld a,<en_fn_x_sel_num_table
	ld (data_entry_x_sel_num_table),a
	ld a,>en_fn_x_sel_num_table
	ld (data_entry_x_sel_num_table+1),a

	ld a,<en_fn_sel_idx_table
	ld (data_entry_sel_idx_table),a
	ld a,>en_fn_sel_idx_table
	ld (data_entry_sel_idx_table+1),a

	ld a,<en_fn_data_type_table
	ld (data_entry_data_type_table),a
	ld a,>en_fn_data_type_table
	ld (data_entry_data_type_table+1),a

	ld a,<efx_de_group_table
	ld (data_entry_groups_table),a
	ld a,>efx_de_group_table
	ld (data_entry_groups_table+1),a

	call enter_filename_reset_data

	xor a
	ld (data_entry_x_menu_sel),a

	ld b,a

	ld a,1
	ld de,filename_menu_gbc_map
	jp data_entry_menu_setup

;***************************************************************************
;---------------------------------------------------------------------------
; enter filename poop loop
;---------------------------------------------------------------------------
;***************************************************************************

enter_filename_poop_loop_over:

	call data_entry_menu_cursor

	ld a,(joy_pressed)
	and joy_b+joy_select
	jr z,_no_exit
	
	call setup_save_to_sram_menu_over
	jp poop

_no_exit:

	ld a,(data_entry_menu_mode)
	cp data_entry_type_first_button
	jp c,poop

	cp data_entry_type_save_button
	jr nz,_not_save
	call enter_filename_save_data

	xor a
	ld (data_entry_menu_mode),a
	ld hl,poop
	push hl
	call draw_song_and_file_info_disp
	jp setup_edit_patt

	; save song damnit

	; set up main menu mode

_not_save:
	cp data_entry_type_reset_button
	jr nz,_not_reset

	call enter_filename_reset_data
	call data_entry_redraw_data

	xor a
	ld (data_entry_menu_mode),a

_not_reset:

	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; enter_filename effects reset data
;---------------------------------------------------------------------------
;***************************************************************************

enter_filename_reset_data_over:

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

	; copy filename to table

	ld hl,data_entry_work_ram
	ld de,$d007
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a
	ld c,18

_read_loop:
	ld a,(de)
	inc de
	cp $ff
	jr z,_done_read

	ld (hli),a
	dec c
	jr nz,_read_loop

_done_read:
_pad_loop:
	ld a,c
	and a
	jr z,_done_pad

	ld a,$20
	ld (hli),a
	dec c
	jr _pad_loop

_done_pad:
	ret

;***************************************************************************
;---------------------------------------------------------------------------
; enter filename effects save data
;---------------------------------------------------------------------------
;***************************************************************************

enter_filename_save_data_over:

	; ok, calculate our new filename length

	ld c,19
	ld hl,data_entry_work_ram+17

_fn_len_loop:
	ld a,(hld)
	dec c
	jr z,_fn_null_length
	cp $20
	jr z,_fn_len_loop
	cp $60
	jr z,_fn_len_loop

_fn_null_length:

	inc hl
	inc hl
	ld a,$ff
	ld (hl),a
	; now calculate our old filename length:

	ld hl,$d007
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld b,$ff

_fn_old_len_loop:
	inc b
	ld a,(hli)
	cp $ff
	jr nz,_fn_old_len_loop

	; ok, so we have old length in b, new length in c

	; what do we do with them?

	; IF THE NEW ONE is longer, we need to insert some space
	; if the new one is shorter, we need to delete some space
	; if they are the same, we do nothing

	ld a,c
	cp b
	jp z,_no_ins_or_del
	jr nc,_insert

	; ok, let's delete some space

	ld a,b
	sub c
	ld c,a

	ld a,($d000)
	sub c
	ld ($d000),a
	ld a,($d001)
	sbc a,0
	ld ($d001),a

	ld a,($d002)
	sub c
	ld ($d002),a
	ld a,($d003)
	sbc a,0
	ld ($d003),a

	ld a,($d004)
	sub c
	ld ($d004),a
	ld (file_access_temp),a
	ld a,($d005)
	sbc a,0
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

	ld de,$d007
	ld a,e
	add a,c
	ld l,a
	ld a,d
	adc a,0
	ld h,a
	
	ld a,music_buffer_start_bank
	ld b,a
	ld c,a

_delete_fn_loop:
	; so here we copy from bhl to cde
	call check_bhl_wram_read
	call check_cde_wram_write

	ld a,(file_access_temp)
	cp e
	jr nz,_delete_fn_loop
	ld a,(file_access_temp+1)
	cp d
	jr nz,_delete_fn_loop
	ld a,(file_access_temp+2)
	cp c
	jr nz,_delete_fn_loop

	; one more for good measure

	call check_bhl_wram_read
	call check_cde_wram_write

	jr _no_ins_or_del

_insert:

	; ok, let's insert some space
	sub b
	ld c,a

	ld a,($d000)
	add a,c
	ld ($d000),a
	ld a,($d001)
	adc a,0
	ld ($d001),a

	ld a,($d002)
	add a,c
	ld ($d002),a
	ld a,($d003)
	adc a,0
	ld ($d003),a

	ld a,($d004)
	ld l,a
	add a,c
	ld ($d004),a
	ld e,a
	ld a,($d005)
	ld h,a
	adc a,0
	ld ($d005),a
	ld d,a

	call convert_hl_index_to_bhl_address
	call convert_de_index_to_cde_address

_insert_fn_loop:
	; so here we copy from bhl to cde
	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a
	ld a,(hld)
	push af
	call check_hl_wram_back
	ld a,(current_wram_bank)
	ld b,a

	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a
	pop af
	ld (de),a
	dec de
	call check_de_wram_back
	ld a,(current_wram_bank)
	ld c,a

	ld a,e
	cp $06
	jr nz,_insert_fn_loop
	ld a,d
	cp $d0
	jr nz,_insert_fn_loop
	ld a,c
	cp music_buffer_start_bank
	jr nz,_insert_fn_loop

_no_ins_or_del:

	; ok, let's copy the filename over

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a
	ld hl,data_entry_work_ram
	ld de,$d007

_copy_fn_loop:
	ld a,(hli)
	ld (de),a
	inc de
	cp $ff
	jr nz,_copy_fn_loop

	; ok, overwrite the old filename untill we encounter an $ff



	; then save

	call get_num_save_ram_songs_into_e
	ld a,(file_access_save)	; process the selected item
	cp e
	jr z,_dont_delete_song

	call delete_song_from_sram

_dont_delete_song:

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	; get number of songs

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(num_save_ram_songs)
	inc a
	ld (num_save_ram_songs),a

	; disable save ram access

	xor a
	ld ($1666),a

	ld a,(file_access_save)	; process the selected item
	call save_song_into_sram_from_ram

	ld a,(file_info_win_flag)
	ld (draw_time_flag),a

	ret

;	jp draw_song_and_file_info_disp


