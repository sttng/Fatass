;---------------------------------------------------------------------------
; main config menu routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_config_menu_over:
; config_poop_loop_over:
; config_reset_data_over:
; config_update_data_over:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; configuration menu setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_config_menu_over:

	ld a,configuration_mode
	ld (editor_mode),a

	xor a
	ld (data_entry_data_mode),a

	ld a,<conf_de_sel_info_table
	ld (data_entry_sel_info_table),a
	ld a,>conf_de_sel_info_table
	ld (data_entry_sel_info_table+1),a

	ld a,<conf_de_x_sel_num_table
	ld (data_entry_x_sel_num_table),a
	ld a,>conf_de_x_sel_num_table
	ld (data_entry_x_sel_num_table+1),a

	ld a,<conf_de_sel_idx_table
	ld (data_entry_sel_idx_table),a
	ld a,>conf_de_sel_idx_table
	ld (data_entry_sel_idx_table+1),a

	ld a,<conf_de_data_type_table
	ld (data_entry_data_type_table),a
	ld a,>conf_de_data_type_table
	ld (data_entry_data_type_table+1),a

	ld a,<efx_de_group_table
	ld (data_entry_groups_table),a
	ld a,>efx_de_group_table
	ld (data_entry_groups_table+1),a

	call config_reset_data_over

	ld a,(conf_x_menu_sel)
	ld (data_entry_x_menu_sel),a

	ld a,(conf_menu_sel)
	ld b,a
	ld a,17
	ld de,configuration_menu_gbc_map
	jp data_entry_menu_setup

;***************************************************************************
;---------------------------------------------------------------------------
; configuration poop loop
;---------------------------------------------------------------------------
;***************************************************************************

config_poop_loop_over:

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

	ld a,(data_entry_changed)
	and a
	call nz,config_update_data_over
	xor a
	ld (data_entry_changed),a

	ld a,(data_entry_menu_mode)
	and a
	jp z,poop

	ld a,(data_entry_x_menu_sel)
	ld (conf_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (conf_menu_sel),a

	jp poop

_buttons:

	cp data_entry_type_save_button
	jr nz,_not_save
_save:
	call palette_ram_save
	call cset_save
	call plasma_save
	call calculate_save_ram_checksum
_exit:
	call setup_main_menu
	jr _done_button_process

_not_save:
	cp data_entry_type_reset_button
	jr nz,_not_reset

	call cset_load
	call palette_ram_load
	call config_reset_data_over
	call data_entry_redraw_data
	call config_update_data_over
	jr _done_button_process

_not_reset:

	; must be a preset

	sub data_entry_type_user_button_0
	cp 6
	jr nc,_not_preset
	ld c,a
	swap c
	sla a
	sla a
	ld b,a
	sla a
	add a,c
	add a,b

	ld c,a
	ld b,0

	ld hl,colour_table_presets
	add hl,bc

	ld bc,colour_table
	ld e,$1c
_pal_preset_copy_loop:
	ld a,(hli)
	ld (bc),a
	inc bc
	dec e
	jr nz,_pal_preset_copy_loop

	call palette_setup_main 
	call config_reset_data_over
	call data_entry_redraw_data

_done_button_process:
	xor a
	ld (data_entry_menu_mode),a

	jp poop

_not_preset:
	sub 6
	sla a	; * 2
	sla a
	add a,>normal_cset_tileset
	ld (font_index),a
	
	call cset_draw

	jr _done_button_process


;***************************************************************************
;---------------------------------------------------------------------------
; configuration reset data
;---------------------------------------------------------------------------
;***************************************************************************

config_reset_data_over:

	ld hl,data_entry_work_ram
	ld de,colour_table
	ld c,$1c/2

_reset_data_loop:

	ld a,(de)
	ld b,a
	and %000_11111
	ld (hli),a
		
	srl b
	srl b
	srl b
	srl b
	srl b

	inc de
	ld a,(de)
	and %000000_11
	sla a
	sla a
	sla a
	or b
	ld (hli),a

	ld a,(de)
	and %0_11111_00
	srl a
	srl a
	ld (hl),a

	inc de

	ld a,$10-2
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	dec c
	jr nz,_reset_data_loop

	ld a,(plasma_cset)
	ld hl,data_entry_work_ram+$100
	ld (hli),a
	ld a,(plasma_col_rat)
	ld (hli),a
	ld a,(plasma_play_mus)
	ld (hl),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; configuration update data
;---------------------------------------------------------------------------
;***************************************************************************

config_update_data_over:

	ld hl,data_entry_work_ram
	ld de,colour_table
	ld c,$1c/2

_update_data_loop:

	ld a,(hli)
	ld b,a
	ld a,(hl)
	sla a
	sla a
	sla a
	sla a
	sla a
	or b
	ld (de),a
	inc de

	ld a,(hli)
	srl a
	srl a
	srl a
	ld b,a
	ld a,(hl)
	sla a
	sla a
	or b
	ld (de),a	

	inc de

	ld a,$10-2
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	dec c
	jr nz,_update_data_loop

	call palette_setup_main 

	ld hl,data_entry_work_ram+$100
	ld a,(hli)
	ld (plasma_cset),a
	ld a,(hli)
	ld (plasma_col_rat),a
	ld a,(hl)
	ld (plasma_play_mus),a

	ret

;***************************************************************************
