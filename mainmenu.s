;***************************************************************************
;---------------------------------------------------------------------------
; main menu setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_main_menu:

	ld a,main_menu_mode
	ld (editor_mode),a

	xor a
	ld ($ff25),a

	ld a,(main_menu_sel)
	ld b,a
	ld c,$93

	; TEMPORARY SAMPLE TEST
	LD A,$FF
	LD (file_access_temp),A

	ld a,22
	ld de,main_menu_gbc_map
	jp setup_menu

;***************************************************************************
;---------------------------------------------------------------------------
; main menu vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

;***************************************************************************
;---------------------------------------------------------------------------
; main menu poop loop
;---------------------------------------------------------------------------
;***************************************************************************

main_menu_poop_loop:

	call menu_poop_loop
	ld a,(menu_cur_sel)
	ld (main_menu_sel),a

;---------------------------------------------------------------------------

	ld hl,poop
	push hl

	ld a,(joy_pressed)
	bit joy_bit_a,a
	jr z,_not_a_pressed

	ld a,(menu_cur_sel)	; process the selected item
	and a
	jp z,setup_play_mode_start
	cp $01
	jp z,copy_track
	cp $02
	jp z,paste_track
	cp $03
	jp z,copy_track_from_cursor
	cp $04
	jp z,paste_track_to_cursor
	cp $05
	jp z,insert_pattern_copy
	cp $06
	jp z,insert_pattern
	cp $07
	jp z,undo
	cp $08
	jp z,redo
	CP $09
	JR NZ,_not_sample
	; TEMPORARY SAMPLE TEST

	LD A,(file_access_temp)
	INC A
	CP $70
	JR NZ,_no_carry
	XOR A
_no_carry:
	LD (file_access_temp),A
	LD E,A
	LD D,0
	LD L,10
	jp play_sample
	
_not_sample:
	cp $0b
	jp z,setup_load_from_sram_menu
	cp $0c
	jp z,setup_load_from_rom_menu
	cp $0d
	jp z,setup_save_to_sram_menu
	cp $0e
	jp z,setup_delete_from_sram_menu
	cp $0f
	jp z,optimize_song_in_wram
	cp $10
	jr z,toggle_song_and_file_info_disp
	cp $13
	jp z,setup_config_menu
	cp $14
	jp z,setup_help_menu
	cp $15
	jp z,setup_plasma
	cp $16
	jp z,setup_logo

	ret

_not_a_pressed:

	and joy_b+joy_select
	ret z

	ld a,(main_menu_came_from)
	cp edit_sequence_mode
	jp z,setup_edit_seq

	jp setup_edit_patt

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************

toggle_song_and_file_info_disp:

	ld a,(file_info_win_flag)
	xor 1
	ld (file_info_win_flag),a
	ld (draw_time_flag),a

;***************************************************************************
;***************************************************************************
;***************************************************************************

draw_song_and_file_info_disp:

	ld a,(file_info_win_flag)
	and a
	jr z,_do_logo

	call file_access_sram_rem_calc

	call draw_file_info_inset

	jr update_song_and_file_info_disp

_do_logo:

	ld hl,$9c00
	ld bc,$0614
	ld de,main_window_gbc_map
	ld a,mapbank00
	call scr_copy_to_vram

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	ret

;***************************************************************************
;***************************************************************************

draw_file_info_inset:

	ld hl,$9c00
	ld bc,$0614
	ld de,main_info_window_gbc_map
	ld a,mapbank00
	call scr_copy_to_vram

;***************************************************************************

draw_file_info_inset_only:

	ld hl,$9c27
	ld bc,$040c
	ld de,file_info_inset
	ld a,insetbank00
	call scr_copy_to_vram_dmg

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	ret

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************

update_song_info_disp:

	ld a,(file_info_win_flag)
	and a
	ret z

update_song_and_file_info_disp:

	xor a
	ld ($ff4f),a

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,mapbank00
	ld (current_rom_bank),a
	ld ($2666),a

	; draw song name

	ld hl,$d007
	ld de,$9c27
	ld c,$0c

_name_loop
	ld a,(hl)
	cp $ff
	jr nz,_not_done_name

	ld hl,blank_menu_dmg_map

_not_done_name:
	call copy_hl_de_vram
	dec c
	jr nz,_name_loop

_done_name:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call draw_time

	ld a,($d005)
	ld b,a
	ld a,($d004)
	ld c,a

	inc bc
	inc bc

	ld a,(editor_num_undos)
	ld d,a
	ld a,(editor_num_redos)
	add a,d
	ld d,a

;***************************************************************************

update_file_info_disp:
	
	; add undo size to file size

	ld a,d
	ld d,0
	sla a
	rl d
	sla a
	rl d

	add a,c
	ld c,a
	ld a,b
	adc a,d
	ld b,a
	
;	push de

	; draw file size

	ld de,$9c4f
	ld h,>numbers

	call write_number_bc_to_vram

	; draw wram remaining

;	_max_wram = (music_buffer_end_bank+1-music_buffer_start_bank)*$10
;
;	xor a
;	sub c
;	ld c,a
;	ld a,_max_wram
;	sbc a,b
;	ld b,a
;
;	dec bc

;	; screw that, draw undo ram
;
;	pop bc
;
;	ld c,b
;	ld b,0
;	sla c
;	rl b
;	sla c
;	rl b
;
;	ld e,$6f
;
;	call write_number_bc_to_vram

	; draw the sram value

	ld h,>numbers
	ld de,$9c8d

	ld a,(file_access_sram_temp+2)
	ld b,a
	swap a
	and $0f
	add a,<numbers
	ld l,a
	call copy_hl_de_vram

	ld a,b
	and $0f
	add a,<numbers
	ld l,a
	call copy_hl_de_vram

	ld a,(file_access_sram_temp+1)
	ld b,a
	ld a,(file_access_sram_temp+0)
	ld c,a

	jp write_number_bc_to_vram

;***************************************************************************

	; calculate remaining sram

file_access_sram_rem_calc:

	ld a,$0a
	ld ($1666),a
	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(start_blank_bank_save_ram)
	ld h,a
	ld l,0

	sra h
	rr l
	sra h
	rr l
	sra h
	rr l

	ld a,(start_blank_addr_save_ram+1)
	and $1f
	or l
	ld b,a
	ld a,(start_blank_addr_save_ram)
	ld c,a

	; so now the addr is stored in hbc
	
	ld a,(max_save_ram_bank_save_ram)
	ld d,a
	ld e,0

	xor a
	ld ($1666),a

	sra d
	rr e
	sra d
	rr e
	sra d
	rr e

	; add $2000

	ld a,$20
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	; now the max is stored as de0

	xor a
	sub c
	ld c,a
	ld a,e
	sbc a,b
	ld b,a
	ld a,d
	sbc a,h
	ld h,a

	; now we got the size+1 in hbc

	ld a,c
	sub 1
	ld c,a
	ld a,b
	sbc a,0
	ld b,a
	ld a,h
	sbc a,0

	; now we have the proper size in abc

	ld (file_access_sram_temp+2),a
	ld a,b
	ld (file_access_sram_temp+1),a
	ld a,c
	ld (file_access_sram_temp+0),a

	ret

;***************************************************************************

write_number_bc_to_vram:

	ld a,b
	swap a
	and $0f
	add a,<numbers
	ld l,a
	call copy_hl_de_vram

	ld a,b
	and $0f
	add a,<numbers
	ld l,a
	call copy_hl_de_vram

write_number_c_to_vram:

	ld a,c
	swap a
	and $0f
	add a,<numbers
	ld l,a
	call copy_hl_de_vram

write_digit_c_to_vram:

	ld a,c
	and $0f
	add a,<numbers
	ld l,a
	call copy_hl_de_vram
	
	ret

;***************************************************************************

draw_time:

	ld a,(draw_time_flag)
	and a
	ret z

	xor a
	ld ($ff4f),a

	; DRAW TIME

	; toggle the colons

	ld b,$20
	ld a,(song_time+1)
	and 1
	jr nz,_not_off
	ld b,58
_not_off:

	ld hl,$9c6d
	call copy_b_to_hl_vram
	ld l,$70
	call copy_b_to_hl_vram

	ld h,>numbers

	ld a,(song_time+3)
	and $f0
	jr z,_draw_seconds
	
	ld de,$9c71
	ld a,(song_time+2)
	ld c,a
	call write_number_c_to_vram

	ld e,$6e
	ld a,(song_time+3)
	ld c,a
	call write_number_c_to_vram

	ld e,$6c
	ld a,(song_time+4)
	ld c,a
	jp write_digit_c_to_vram

_draw_seconds:

	ld de,$9c71
	ld a,(song_time+1)
	ld c,a
	call write_number_c_to_vram

	ld e,$6e
	ld a,(song_time+2)
	ld c,a
	call write_number_c_to_vram

	ld e,$6c
	ld a,(song_time+3)
	ld c,a
	jp write_digit_c_to_vram

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************

	end
