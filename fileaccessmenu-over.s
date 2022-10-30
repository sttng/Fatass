;***************************************************************************
;---------------------------------------------------------------------------
; setup_load_from_sram_menu:
;---------------------------------------------------------------------------
;***************************************************************************

setup_load_from_sram_menu_over:

	xor a
	ld (draw_time_flag),a

	call get_num_save_ram_songs_into_e

	; return if no songs

	ld a,e
	and a
	jp z,poop
	
	; setup menu

	call file_access_sram_rem_calc
	call draw_file_info_inset

	ld a,load_from_sram_menu_mode
	ld (editor_mode),a

	call get_num_save_ram_songs_into_e

	ld a,(load_from_sram_menu_sel)
	cp e
	jr c,_ok_index
	ld a,e
	dec a
_ok_index:
	ld b,a
	ld c,$93

	ld a,e
	dec a
	ld de,blank_menu_gbc_map
	call setup_menu
	ld a,$ff
	ld (load_from_sram_menu_sel),a
	
	jp draw_sram_file_list

;***************************************************************************
;---------------------------------------------------------------------------
; load_from_sram_poop_loop:
;---------------------------------------------------------------------------
;***************************************************************************

load_from_sram_menu_poop_over:

	call menu_poop_loop

	ld a,(load_from_sram_menu_sel)
	ld b,a
	ld a,(menu_cur_sel)
	cp b
	jr z,_no_draw
	ld (load_from_sram_menu_sel),a

	call draw_sram_file_info

_no_draw:

	ld a,(joy_pressed)
	bit joy_bit_a,a
	jr z,_not_a_pressed

	ld a,(menu_cur_sel)	; process the selected item
	call load_song_into_ram_from_sram
	ld a,(file_info_win_flag)
	ld (draw_time_flag),a
	call redraw_song_info
	call setup_edit_patt

	jp poop

_not_a_pressed:

	and joy_b+joy_select
	jp z,poop
	ld a,(file_info_win_flag)
	ld (draw_time_flag),a
	call draw_song_and_file_info_disp
	call setup_main_menu
	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; setup_delete_from_sram_menu:
;---------------------------------------------------------------------------
;***************************************************************************

setup_delete_from_sram_menu_over:

	xor a
	ld (draw_time_flag),a

	call get_num_save_ram_songs_into_e

	; return if no songs

	ld a,e
	and a
	jp z,poop
	
	; setup menu

	call draw_file_info_inset

	ld a,delete_from_sram_menu_mode
	ld (editor_mode),a

	call get_num_save_ram_songs_into_e

	ld a,(delete_from_sram_menu_sel)
	cp e
	jr c,_ok_index
	ld a,e
	dec a
_ok_index:
	ld b,a
	ld c,$93

	ld a,e
	dec a
	ld de,blank_menu_gbc_map
	call setup_menu
	ld a,$ff
	ld (delete_from_sram_menu_sel),a

	jp draw_sram_file_list

;***************************************************************************
;---------------------------------------------------------------------------
; delete_from_sram_menu_poop_loop:
;---------------------------------------------------------------------------
;***************************************************************************

delete_from_sram_menu_poop_over:

	call menu_poop_loop
	ld a,(delete_from_sram_menu_sel)
	ld b,a
	ld a,(menu_cur_sel)
	cp b
	jr z,_no_draw
	ld (delete_from_sram_menu_sel),a

	call file_access_sram_rem_calc

	ld a,(menu_cur_sel)
	call get_sram_song_address_hl_bc_e_d
	ld a,(file_access_sram_temp+0)
	add a,c
	ld (file_access_sram_temp+0),a
	ld a,(file_access_sram_temp+1)
	adc a,b
	ld (file_access_sram_temp+1),a
	ld a,(file_access_sram_temp+2)
	adc a,0
	ld (file_access_sram_temp+2),a

	ld a,(menu_cur_sel)
	call draw_sram_file_info

_no_draw:

	ld a,(joy_pressed)
	bit joy_bit_a,a
	jr z,_not_a_pressed

	ld a,(menu_cur_sel)	; process the selected item
	call delete_song_from_sram

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; finally, after all that monkeying around with the stack,
	; we can delete the pointer from the header

	ld a,(menu_cur_sel)	; process the selected item
	ld d,a

	; our song number is in D

	ld a,(num_save_ram_songs)
	inc a
	sub d
	jr z,_dont_move_any
	ld e,a

	; number of songs to move is in e

	ld hl,song_save_bank_table
	ld c,d
	ld b,0
	add hl,bc
	ld c,l
	ld b,h
	inc bc

	; bc is our source, hl is our dest

_bank_header_delete_loop:

	ld a,(bc)
	inc c
	ld (hli),a
	dec e
	jr nz,_bank_header_delete_loop

	; now for addrs

	ld a,(num_save_ram_songs)
	inc a
	sub d
	ld e,a

	sla e
	sla d
	push de

	; number of bytes to move is in e

	ld hl,song_save_header_table
	ld c,d
	ld b,0
	add hl,bc
	ld c,l
	ld b,h
	inc bc
	inc bc

	; bc is our source, hl is our dest

_bank_addr_delete_loop:

	ld a,(bc)
	inc c
	ld (hli),a
	dec e
	jr nz,_bank_addr_delete_loop

	; now for TIMES

	pop de
	sla e
	sla d

	; number of bytes to move is in e

	ld hl,song_save_time_table
	ld c,d
	ld b,0
	add hl,bc
	ld c,l
	ld b,h
	inc bc
	inc bc
	inc bc
	inc bc

	; bc is our source, hl is our dest

_time_delete_loop:

	ld a,(bc)
	inc c
	ld (hli),a
	dec e
	jr nz,_time_delete_loop

_dont_move_any:

	call calculate_save_ram_checksum
	ld a,(file_info_win_flag)
	ld (draw_time_flag),a
	call redraw_song_info
	call setup_edit_patt
	jp poop

_not_a_pressed:

	and joy_b+joy_select
	jp z,poop
	ld a,(file_info_win_flag)
	ld (draw_time_flag),a
	call draw_song_and_file_info_disp
	call setup_main_menu
	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; setup_save_to_sram_menu:
;---------------------------------------------------------------------------
;***************************************************************************

setup_save_to_sram_menu_over:

	xor a
	ld (draw_time_flag),a

	; setup menu

	call draw_file_info_inset

	ld a,save_to_sram_menu_mode
	ld (editor_mode),a

	call get_num_save_ram_songs_into_e

	ld a,e
	ld b,a
	ld c,$93

	ld a,e
	ld de,blank_menu_gbc_map
	call setup_menu

	ld a,$ff
	ld (save_from_sram_menu_sel),a

	call draw_sram_file_list

	ld a,c
	cp 24
	ret z

	ld hl,new_song_text
	ld b,new_song_text_len

_copy_loop:
	call copy_hl_de_vram
	dec b
	jr nz,_copy_loop

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; save_to_sram_poop_loop:
;---------------------------------------------------------------------------
;***************************************************************************

save_to_sram_menu_poop_over:

	call menu_poop_loop

	ld a,(save_from_sram_menu_sel)
	ld b,a
	ld a,(menu_cur_sel)
	cp b
	jp z,_no_draw
	ld (save_from_sram_menu_sel),a

	call file_access_sram_rem_calc
	call get_num_save_ram_songs_into_e
	ld a,(menu_cur_sel)	; process the selected item
	cp e
	jr z,_dont_calc_sub

	call get_sram_song_address_hl_bc_e_d
	ld a,(file_access_sram_temp+0)
	add a,c
	ld (file_access_sram_temp+0),a
	ld a,(file_access_sram_temp+1)
	adc a,b
	ld (file_access_sram_temp+1),a
	ld a,(file_access_sram_temp+2)
	adc a,0
	ld (file_access_sram_temp+2),a

_dont_calc_sub:
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(editor_num_undos)
	ld c,a
	ld a,(editor_num_redos)
	add a,c
	ld c,a
	ld b,0
	sla c
	rl b
	sla c
	rl b

	ld a,($d004)
	add a,c
	ld c,a
	ld a,($d005)
	adc a,b
	ld b,a

	inc bc
	inc bc

	ld a,(file_access_sram_temp+0)
	sub c
	ld (file_access_sram_temp+0),a
	ld a,(file_access_sram_temp+1)
	sbc a,b
	ld (file_access_sram_temp+1),a
	ld a,(file_access_sram_temp+2)
	sbc a,0
	ld (file_access_sram_temp+2),a
	jr nc,_not_over

	xor a
	ld (file_access_sram_temp+0),a
	ld (file_access_sram_temp+1),a
	ld (file_access_sram_temp+2),a

_not_over:

	call get_num_save_ram_songs_into_e

	ld a,(menu_cur_sel)
	cp e
	jr nz,_not_e

	push af
	ld a,1
	ld (draw_time_flag),a

	pop af
	call update_song_and_file_info_disp
	jr _no_draw

_not_e:

	xor a
	ld (draw_time_flag),a
	call draw_file_info_inset_only
	ld a,(menu_cur_sel)
	call draw_sram_file_info

_no_draw:

	ld a,(joy_pressed)
	bit joy_bit_a,a
	jr z,_not_a_pressed

	ld a,(file_access_sram_temp+2)
	and a
	jr nz,_ok_to_save

	ld a,(file_access_sram_temp+1)
	and a
	jr nz,_ok_to_save
	
	ld a,(file_access_sram_temp+0)
	and a
	jp z,poop

_ok_to_save:

	ld a,(menu_cur_sel)
	ld (file_access_save),a

	call setup_enter_filename
	jp poop

_not_a_pressed:

	and joy_b+joy_select
	jp z,poop
	ld a,(file_info_win_flag)
	ld (draw_time_flag),a
	call draw_song_and_file_info_disp
	call setup_main_menu
	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; setup_load_from_rom_menu:
;---------------------------------------------------------------------------
;***************************************************************************

setup_load_from_rom_menu_over:

	; setup menu

	call file_access_sram_rem_calc
	call draw_file_info_inset

	ld a,load_from_rom_menu_mode
	ld (editor_mode),a

	xor a
	ld (draw_time_flag),a

	ld a,(load_from_rom_menu_sel)
	cp mus_num_songs-1
	jr c,_ok_index
	ld a,mus_num_songs-1
_ok_index:
	ld b,a
	ld c,$93

	ld a,mus_num_songs-1
	ld de,blank_menu_gbc_map
	call setup_menu

	ld a,$ff
	ld (load_from_rom_menu_sel),a

	; load filenames to screen

	ld bc,$0000
	ld de,$9d01

	jp copy_name_from_rom
	
;***************************************************************************
;---------------------------------------------------------------------------
; load_from_rom_poop_loop:
;---------------------------------------------------------------------------
;***************************************************************************

load_from_rom_menu_poop_over:

	call menu_poop_loop

	ld a,(load_from_rom_menu_sel)
	ld b,a
	ld a,(menu_cur_sel)
	cp b
	jr z,_no_draw
	ld (load_from_rom_menu_sel),a

	call draw_rom_file_info

_no_draw:

	ld a,(joy_pressed)
	bit joy_bit_a,a
	jr z,_not_a_pressed

	ld a,(file_info_win_flag)
	ld (draw_time_flag),a

	ld a,(menu_cur_sel)	; process the selected item

	call load_song_into_ram_from_rom
	call redraw_song_info
	call setup_edit_patt
	jp poop

_not_a_pressed:

	and joy_b+joy_select
	jp z,poop

	ld a,(file_info_win_flag)
	ld (draw_time_flag),a
	call draw_song_and_file_info_disp
	call setup_main_menu
	jp poop

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************

draw_sram_file_info:

	push af

	sla a
	sla a
	add a,<song_save_time_table
	ld l,a

	ld a,>song_save_time_table
	adc a,0
	ld h,a

	ld a,$0a
	ld ($1666),a
	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	inc hl
	inc hl

	ld a,(hld)
	and $f0
	jr nz,_draw_minutes

	dec hl

_draw_minutes:

	ld a,(hli)
	ld c,a
	push hl

	ld h,>numbers
	ld de,$9c71
	call write_number_c_to_vram

	pop hl
	ld a,(hli)
	ld c,a
	push hl

	ld h,>numbers
	ld e,$6e
	call write_number_c_to_vram

	pop hl
	ld a,(hli)
	ld c,a

	ld h,>numbers
	ld e,$6c
	call write_digit_c_to_vram

	xor a
	ld ($1666),a

	ld b,58
	ld hl,$9c6d
	call copy_b_to_hl_vram
	ld l,$70
	call copy_b_to_hl_vram

	pop af

	call get_sram_song_address_hl_bc_e_d

	ld l,d
	ld h,0

	sla l
	rl h
	sla l
	rl h

	ld a,c
	sub l
	ld c,a
	ld a,b
	sbc a,h
	ld b,a
	
	jp update_file_info_disp

;***************************************************************************
