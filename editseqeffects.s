;---------------------------------------------------------------------------
; seq effects editor menu
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_edit_seq_efx: wrapper
; edit_seq_efx_poop_loop: wrapper
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables:
;
; efx_seq_group_table:
; efx_seq_x_sel_num_table:
; efx_seq_sel_info_table:
; efx_seq_sel_idx_table:
; efx_seq_data_type_table:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_edit_seq_efx:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp setup_edit_seq_efx_over

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects poop loop
;---------------------------------------------------------------------------
;***************************************************************************

edit_seq_efx_poop_loop:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp edit_seq_efx_poop_loop_over

;***************************************************************************

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

efx_seq_group_table:
	dw panning_group_table
	dw speed_group_table
	dw transpose_group_table
	dw song_loop_group_table

efx_seq_x_sel_num_table:
	db 4	; panning
	db 4	; speed/shuffle
	db 1	; transpose/fine tune
	db 1	; song loop
	db 3	; copy/paste/MUTATE
	db 2 	; save/reset

efx_seq_sel_info_table:
	dw efx_sel_panning_info
	dw efx_sel_speed_info
	dw efx_sel_transpose_info
	dw efx_sel_song_loop_info
	dw efx_sel_copy_paste_info
	dw data_entry_save_reset_info  ; save/reset

efx_seq_sel_idx_table:
	dw panning_x_sel_table
	dw speed_x_sel_table
	dw transpose_x_sel_table
	dw song_loop_x_sel_table
	dw copy_paste_x_sel_table	; save/reset
	dw data_entry_save_reset_sel_table	; save/reset

efx_seq_data_type_table:
	dw panning_data_type_table
	dw speed_data_type_table
	dw transpose_data_type_table
	dw song_loop_data_type_table
	dw copy_paste_data_type_table
	dw data_entry_save_reset_data_table	; save/reset

	end


;***************************************************************************
;---------------------------------------------------------------------------
; edit effects save data
;---------------------------------------------------------------------------
;***************************************************************************
;
;edit_seq_effects_save_data:
;
;	ld a,codeoverbank
;	ld (current_rom_bank),a
;	ld ($2666),a
;
;	jp edit_seq_effects_save_data_ov
;
;***************************************************************************
;---------------------------------------------------------------------------
; edit effects poop loop
;---------------------------------------------------------------------------
;***************************************************************************
;
;edit_seq_effects_reset_data:
;
;	ld a,codeoverbank
;	ld (current_rom_bank),a
;	ld ($2666),a
;
;	jp edit_seq_effects_reset_data_ov
;
