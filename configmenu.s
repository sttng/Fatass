;---------------------------------------------------------------------------
; main config menu routine wrappers
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_config_menu: wrapper
; config_poop_loop: wrapper
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables
;
; conf_de_x_sel_num_table:
; conf_de_sel_info_table:
; conf_de_sel_idx_table:
; _colour_x_sel_table
; _preset_x_sel_table:
; _plasma_x_sel_table
; conf_de_data_type_table:
; _colour_rgb_data_table:
; _preset_data_table:
; _charset_data_table:
; _plasma_data_table:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; configuration menu setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_config_menu:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp setup_config_menu_over

;***************************************************************************
;---------------------------------------------------------------------------
; configuration poop loop
;---------------------------------------------------------------------------
;***************************************************************************

config_poop_loop:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp config_poop_loop_over
	
;***************************************************************************

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

conf_de_x_sel_num_table:
	db 3 ; top background 	- red/green/blue
	db 3 ; top foreground	- red/green/blue
	db 3 ; bottom background- red/green/blue
	db 3 ; bottom foreground- red/green/blue
	db 3 ; top highlight	- red/green/blue
	db 3 ; top main		- red/green/blue
	db 3 ; top shadow	- red/green/blue
	db 3 ; bottom highlight	- red/green/blue
	db 3 ; bottom main	- red/green/blue
	db 3 ; bottom shadow	- red/green/blue
	db 3 ; scope/top text 1	- red/green/blue
	db 3 ; top text 2	- red/green/blue
	db 3 ; bottom text 1	- red/green/blue
	db 3 ; bottom text 2	- red/green/blue
	db 6 ; presets
	db 6 ; charsets
	db 3 ; plasma
	db 2 ; save/reset

conf_de_sel_info_table:
	dw conf_colour_info ; top background 	- red/green/blue
	dw conf_colour_info ; top foreground	- red/green/blue
	dw conf_colour_info ; bottom background - red/green/blue
	dw conf_colour_info ; bottom foreground	- red/green/blue
	dw conf_colour_info ; top highlight	- red/green/blue
	dw conf_colour_info ; top main		- red/green/blue
	dw conf_colour_info ; top shadow	- red/green/blue
	dw conf_colour_info ; bottom highlight	- red/green/blue
	dw conf_colour_info ; bottom main	- red/green/blue
	dw conf_colour_info ; bottom shadow	- red/green/blue
	dw conf_colour_info ; scope/top text 1	- red/green/blue
	dw conf_colour_info ; top text 2	- red/green/blue
	dw conf_colour_info ; bottom text 1	- red/green/blue
	dw conf_colour_info ; bottom text 2	- red/green/blue
	dw conf_preset_info ; presets
	dw conf_charset_info ; charsets
	dw conf_plasma_info ; plasma
	dw data_entry_save_reset_info  ; save/reset

conf_de_sel_idx_table:
	dw _colour_x_sel_table ; top background - red/green/blue
	dw _colour_x_sel_table ; top foreground	- red/green/blue
	dw _colour_x_sel_table ; bot background - red/green/blue
	dw _colour_x_sel_table ; bot foreground	- red/green/blue
	dw _colour_x_sel_table ; top highlight	- red/green/blue
	dw _colour_x_sel_table ; top main	- red/green/blue
	dw _colour_x_sel_table ; top shadow	- red/green/blue
	dw _colour_x_sel_table ; bottom highlight-red/green/blue
	dw _colour_x_sel_table ; bottom main	- red/green/blue
	dw _colour_x_sel_table ; bottom shadow	- red/green/blue
	dw _colour_x_sel_table ; scope/top text 1-red/green/blue
	dw _colour_x_sel_table ; top text 2	- red/green/blue
	dw _colour_x_sel_table ; bottom text 1	- red/green/blue
	dw _colour_x_sel_table ; bottom text 2	- red/green/blue
	dw _preset_x_sel_table ; presets
	dw _preset_x_sel_table ; charsets
	dw _plasma_x_sel_table ; plasma
	dw data_entry_save_reset_sel_table	; save/reset

_colour_x_sel_table
	db (11*8)+8,2 ; red
	db (14*8)+8,2 ; green
	db (17*8)+8,2 ; blue

_preset_x_sel_table:
	db (8*8)+8,1	; preset
	db (10*8)+8,1	; preset
	db (12*8)+8,1	; preset
	db (14*8)+8,1	; preset
	db (16*8)+8,1	; preset
	db (18*8)+8,1	; preset

_plasma_x_sel_table
	db (8*8)+8,1 ; graphics type
	db (10*8)+8,2 ; colour morph rate
	db (13*8)+8,3 ; play music

conf_de_data_type_table:
	dw _colour_rgb_data_table ; top background 	- red/green/blue
	dw _colour_rgb_data_table ; top foreground- red/green/blue
	dw _colour_rgb_data_table ; bottom background 	- red/green/blue
	dw _colour_rgb_data_table ; bottom foreground- red/green/blue
	dw _colour_rgb_data_table ; top highlight	- red/green/blue
	dw _colour_rgb_data_table ; top main	- red/green/blue
	dw _colour_rgb_data_table ; top shadow	- red/green/blue
	dw _colour_rgb_data_table ; bottom highlight-red/green/blue
	dw _colour_rgb_data_table ; bottom main	- red/green/blue
	dw _colour_rgb_data_table ; bottom shadow	- red/green/blue
	dw _colour_rgb_data_table ; scope/top text 1-red/green/blue
	dw _colour_rgb_data_table ; top text 2	- red/green/blue
	dw _colour_rgb_data_table ; bottom text 1	- red/green/blue
	dw _colour_rgb_data_table ; bottom text 2	- red/green/blue
	dw _preset_data_table	; preset data table
	dw _charset_data_table	; charset data table
	dw _plasma_data_table	; charset data table
	dw data_entry_save_reset_data_table	; save/reset

_colour_rgb_data_table:
	db data_entry_type_byte_00_1f
	db data_entry_type_byte_00_1f
	db data_entry_type_byte_00_1f

_preset_data_table:
	db data_entry_type_user_button_0
	db data_entry_type_user_button_1
	db data_entry_type_user_button_2
	db data_entry_type_user_button_3
	db data_entry_type_user_button_4
	db data_entry_type_user_button_5

_charset_data_table:
	db data_entry_type_user_button_6
	db data_entry_type_user_button_7
	db data_entry_type_user_button_8
	db data_entry_type_user_button_9
	db data_entry_type_user_button_a
	db data_entry_type_user_button_b

_plasma_data_table:
	db data_entry_type_nybble_0_1
	db data_entry_type_byte_00_ff
	db data_entry_type_off_on

	end

;***************************************************************************
;---------------------------------------------------------------------------
; configuration reset data
;---------------------------------------------------------------------------
;***************************************************************************
;
;config_reset_data:
;
;	ld a,codeoverbank
;	ld (current_rom_bank),a
;	ld ($2666),a
;
;	jp config_reset_data_over
;
;***************************************************************************
;---------------------------------------------------------------------------
; configuration update data
;---------------------------------------------------------------------------
;***************************************************************************
;
;config_update_data:
;
;	ld a,codeoverbank
;	ld (current_rom_bank),a
;	ld ($2666),a
;
;	jp config_update_data_over
;
