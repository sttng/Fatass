;***************************************************************************
;---------------------------------------------------------------------------
; enter filename setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_enter_filename:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp setup_enter_filename_over

;***************************************************************************
;---------------------------------------------------------------------------
; enter filename reset data
;---------------------------------------------------------------------------
;***************************************************************************

enter_filename_reset_data:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp enter_filename_reset_data_over

;***************************************************************************
;---------------------------------------------------------------------------
; enter filename save data
;---------------------------------------------------------------------------
;***************************************************************************

enter_filename_save_data:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp enter_filename_save_data_over

;***************************************************************************
;---------------------------------------------------------------------------
; enter filename poop loop
;---------------------------------------------------------------------------
;***************************************************************************

enter_filename_poop_loop:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a
	
	jp enter_filename_poop_loop_over

;***************************************************************************

en_fn_x_sel_num_table:
	db 18	; filename
	db 2 	; save/reset

en_fn_sel_info_table:
	dw filename_entry_info
	dw data_entry_save_reset_info  ; save/reset

en_fn_sel_idx_table:
	dw _filename_entry_x_sel_table
	dw data_entry_save_reset_sel_table	; save/reset

_filename_entry_x_sel_table:
	db (01*8)+8,1 ; entry
	db (02*8)+8,1 ; entry
	db (03*8)+8,1 ; entry
	db (04*8)+8,1 ; entry
	db (05*8)+8,1 ; entry
	db (06*8)+8,1 ; entry
	db (07*8)+8,1 ; entry
	db (08*8)+8,1 ; entry
	db (09*8)+8,1 ; entry
	db (10*8)+8,1 ; entry
	db (11*8)+8,1 ; entry
	db (12*8)+8,1 ; entry
	db (13*8)+8,1 ; entry
	db (14*8)+8,1 ; entry
	db (15*8)+8,1 ; entry
	db (16*8)+8,1 ; entry
	db (17*8)+8,1 ; entry
	db (18*8)+8,1 ; entry

en_fn_data_type_table:
	dw _filename_data_type_table
	dw data_entry_save_reset_data_table	; save/reset

_filename_data_type_table:
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text
	db data_entry_type_text

	end
