;---------------------------------------------------------------------------
; pattern effects edit menu routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_edit_efx:
; edit_efx_poop_loop: wrapper
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables
;
; efx_de_attributes_table:
; efx_de_group_table:
; pulsewidth_group_table:
; waveform_group_table:
; volume_group_table:
; duration_group_table:
; panning_group_table:
; vibrato_group_table:
; portamento_group_table:
; arpeggio_group_table:
; sweep_group_table:
; trigger_group_table:
; speed_group_table:
; transpose_group_table:
; sample_group_table:
; efx_de_x_sel_num_table:
; efx_de_sel_info_table:
; efx_de_sel_idx_table:
; pulsewidth_x_sel_table:
; waveform_x_sel_table:
; volume_x_sel_table:
; duration_x_sel_table:
; panning_x_sel_table:
; vibrato_x_sel_table:
; portamento_x_sel_table:
; arpeggio_x_sel_table:
; sweep_x_sel_table:
; trigger_x_sel_table:
; speed_x_sel_table:
; transpose_x_sel_table:
; sample_x_sel_table:
; copy_paste_x_sel_table:
; efx_de_data_type_table:
; pulsewidth_data_type_table:
; waveform_data_type_table:
; volume_data_type_table:
; duration_data_type_table:
; panning_data_type_table:
; vibrato_data_type_table:
; portamento_data_type_table:
; arpeggio_data_type_table:
; sweep_data_type_table:
; trigger_data_type_table:
; speed_data_type_table:
; transpose_data_type_table:
; sample_data_type_table:
; copy_paste_data_type_table:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_edit_efx:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call setup_edit_efx_over

	ld a,(gbc)
	and a
	ret z

	ld a,(editor_track)
	sla a
	ld c,a
	ld b,0
	ld hl,efx_de_attributes_table
	add hl,bc
	ld a,(hli)
	ld d,(hl)
	ld e,a
	ld hl,$9d01
	ld bc,$1812
	ld a,efxselbank00
	ld (current_rom_bank),a
	ld ($2666),a			; ROMB0 Set lowbyte for $4000-$7fff
	ld a,1
	ld ($ff4f),a
	jp dmg_scr_copy_loop

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects poop loop
;---------------------------------------------------------------------------
;***************************************************************************

edit_efx_poop_loop:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp edit_efx_poop_loop_over

;***************************************************************************

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

efx_de_attributes_table:
	dw effects_menu_1_gbc_attr
	dw effects_menu_2_gbc_attr
	dw effects_menu_3_gbc_attr
	dw effects_menu_4_gbc_attr

efx_de_group_table:
	dw pulsewidth_group_table
	dw waveform_group_table
	dw volume_group_table
	dw duration_group_table
	dw panning_group_table
	dw vibrato_group_table
	dw portamento_group_table
	dw arpeggio_group_table
	dw sweep_group_table
	dw trigger_group_table
	dw speed_group_table
	dw transpose_group_table
	dw sample_group_table

pulsewidth_group_table:
	db %00000001,%00000000 ; pulse width
	db %00111110,%00000000 ; auto pwm
	db %00111110,%00000000 ; auto pwm
	db %00111110,%00000000 ; auto pwm
	db %00111110,%00000000 ; auto pwm
	db %00111110,%00000000 ; auto pwm

waveform_group_table:
	db %00000011,%00000000 ; set waveform
	db %00000011,%00000000 ; set waveform
	db %00001100,%00000000 ; waveform gain
	db %00001100,%00000000 ; waveform gain
	db %01110000,%00000000 ; waveform mix
	db %01110000,%00000000 ; waveform mix
	db %01110000,%00000000 ; waveform mix

volume_group_table:
	db %00000001,%00000000 ; volume
	db %00000010,%00000000 ; envelope restart
	db %00001100,%00000000 ; envelope dir
	db %00001100,%00000000 ; envelope rate

panning_group_table:
	db %00001111,%00000000 ; panning
	db %00001111,%00000000 ; panning
	db %00001111,%00000000 ; panning
	db %00001111,%00000000 ; panning
	db %11110000,%00000001 ; auto panner
	db %11110000,%00000001 ; auto panner
	db %11110000,%00000001 ; auto panner
	db %11110000,%00000001 ; auto panner
	db %11110000,%00000001 ; auto panner

vibrato_group_table:
	db %00000011,%00000000 ; vibrato depth
	db %00000011,%00000000 ; vibrato rate
	db %00000100,%00000000 ; vibrato reset

song_loop_group_table:
;	db %00000001,%00000000 ; song_loop
trigger_group_table:
;	db %00000001,%00000000 ; trigger
;	db %00000010,%00000000 ; retrigger
transpose_group_table:
;	db %00000001,%00000000 ; transpose
;	db %00000010,%00000000 ; fine tune
duration_group_table:
;	db %00000001,%00000000 ; duration
;	db %00000010,%00000000 ; hold mode
portamento_group_table:
	db %00000001,%00000000 ; porta from
	db %00000010,%00000000 ; portamento rate
	db %00000100,%00000000 ; glissando mode

arpeggio_group_table:
	db %00011111,%00000000 ; arpeggio
	db %00011111,%00000000 ; arpeggio
	db %00011111,%00000000 ; arpeggio
	db %00011111,%00000000 ; arpeggio
	db %00011111,%00000000 ; arpeggio

sweep_group_table:
	db %00000111,%00000000 ; sweep
	db %00000111,%00000000 ; sweep
	db %00000111,%00000000 ; sweep

speed_group_table:
	db %00000001,%00000000 ; speed
	db %00001110,%00000000 ; shuffle
	db %00001110,%00000000 ; shuffle
	db %00001110,%00000000 ; shuffle


sample_group_table:
	db %00000011,%00000000 ; sample
	db %00000011,%00000000 ; sample
	db %00000100,%00000000 ; stop sample

efx_de_x_sel_num_table:
	db 6	; pulsewidth/auto-pwm
	db 7	; waveform/gain/mix
	db 4	; volume/envelope
	db 2	; duration/hold mode
	db 9	; panning/auto-pan
	db 3	; vibrato
	db 3	; portamento
	db 5	; arpeggio
	db 3	; sweep
	db 2	; trigger delay/retrigger
	db 4	; speed/shuffle
	db 2	; transpose/fine tune
	db 3	; sample/stop sample
	db 3 	; copy/paste
	db 2 	; save/reset

efx_de_sel_info_table:
	dw efx_sel_pulse_info
	dw efx_sel_wave_info
	dw efx_sel_volume_info
	dw efx_sel_duration_info
	dw efx_sel_panning_info
	dw efx_sel_vibrato_info
	dw efx_sel_portamento_info
	dw efx_sel_arpeggio_info
	dw efx_sel_sweep
	dw efx_sel_trigger_info
	dw efx_sel_speed_info
	dw efx_sel_transpose_info
	dw efx_sel_sample_info
	dw efx_sel_copy_paste_info
	dw data_entry_save_reset_info  ; save/reset

efx_de_sel_idx_table:
	dw pulsewidth_x_sel_table
	dw waveform_x_sel_table
	dw volume_x_sel_table
	dw duration_x_sel_table
	dw panning_x_sel_table
	dw vibrato_x_sel_table
	dw portamento_x_sel_table
	dw arpeggio_x_sel_table
	dw sweep_x_sel_table
	dw trigger_x_sel_table
	dw speed_x_sel_table
	dw transpose_x_sel_table
	dw sample_x_sel_table
	dw copy_paste_x_sel_table
	dw data_entry_save_reset_sel_table	; save/reset

pulsewidth_x_sel_table:
	db (06*8)+8,1 ; pulse width
	db (08*8)+8,1 ; autopwm 1
	db (09*8)+8,1 ; autopwm 2
	db (10*8)+8,1 ; autopwm 3
	db (11*8)+8,1 ; autopwm 4
	db (13*8)+8,3 ; autopwm rate

waveform_x_sel_table:
	db (06*8)+8,1 ; waveform pre/usr
	db (07*8)+8,2 ; waveform
	db (10*8)+8,1 ; distortion
	db (11*8)+8,1 ; gain
	db (13*8)+8,1 ; mix waveform pre/usr
	db (14*8)+8,2 ; mix waveform
	db (17*8)+8,2 ; mix tonefactor

volume_x_sel_table:
	db (06*8)+8,1 ; volume
	db (08*8)+8,3 ; envelope restart
	db (12*8)+8,2 ; envelope direction
	db (15*8)+8,3 ; envelope rate

duration_x_sel_table:
	db (06*8)+8,2 ; duration
	db (09*8)+8,3 ; hold mode

panning_x_sel_table:
	db (06*8)+8,1 ; panning track 1
	db (07*8)+8,1 ; panning track 2
	db (08*8)+8,1 ; panning track 3
	db (09*8)+8,1 ; panning track 4
	db (11*8)+8,1 ; autopan part 1
	db (12*8)+8,1 ; autopan part 2
	db (13*8)+8,1 ; autopan part 3
	db (14*8)+8,1 ; autopan part 4
	db (16*8)+8,3 ; autopan rate

vibrato_x_sel_table:
	db (06*8)+8,2 ; vibrato depth
	db (09*8)+8,3 ; vibrato rate
	db (13*8)+8,3 ; vibrato reset
	
portamento_x_sel_table:
	db (06*8)+8,3 ; portamento from
	db (10*8)+8,3 ; portamento rate
	db (14*8)+8,3 ; glissando mode

arpeggio_x_sel_table:
	db (06*8)+8,1 ; arpeggio part 1
	db (07*8)+8,1 ; arpeggio part 2
	db (08*8)+8,1 ; arpeggio part 3
	db (09*8)+8,1 ; arpeggio part 4
	db (11*8)+8,3 ; arpeggio rate

sweep_x_sel_table:
	db (06*8)+8,2 ; sweep direction
	db (09*8)+8,1 ; sweep depth
	db (11*8)+8,3 ; sweep rate

trigger_x_sel_table:
	db (06*8)+8,2 ; trigger delay
	db (09*8)+8,3 ; retrigger delay

speed_x_sel_table:
	db (06*8)+8,2 ; song speed
	db (09*8)+8,2 ; shuffle phase
	db (12*8)+8,1 ; shuffle rate
	db (14*8)+8,3 ; shuffle depth

transpose_x_sel_table:
	db (06*8)+8,3 ; transpose
	db (10*8)+8,3 ; fine-tune

sample_x_sel_table:
	db (06*8)+8,1 ; sample pre/usr
	db (07*8)+8,2 ; sample
	db (10*8)+8,4 ; stop sample

copy_paste_x_sel_table:
	db (01*8)+8,4 ; copy
	db (06*8)+8,5 ; paste
	db (12*8)+8,6 ; mutate

song_loop_x_sel_table:
	db (15*8)+8,2 ; song loop
		
efx_de_data_type_table:
	dw pulsewidth_data_type_table
	dw waveform_data_type_table
	dw volume_data_type_table
	dw duration_data_type_table
	dw panning_data_type_table
	dw vibrato_data_type_table
	dw portamento_data_type_table
	dw arpeggio_data_type_table
	dw sweep_data_type_table
	dw trigger_data_type_table
	dw speed_data_type_table
	dw transpose_data_type_table
	dw sample_data_type_table
	dw copy_paste_data_type_table
	dw data_entry_save_reset_data_table	; save/reset

pulsewidth_data_type_table:
	db data_entry_type_nybble_0_3 	; pulse width
	db data_entry_type_nybble_0_3 	; autopwm 1
	db data_entry_type_nybble_0_3 	; autopwm 2
	db data_entry_type_nybble_0_3 	; autopwm 3
	db data_entry_type_nybble_0_3 	; autopwm 4
	db data_entry_type_byte_off_ff 	; autopwm rate

waveform_data_type_table:
	db data_entry_type_p_u 		; waveform pre/usr
	db data_entry_type_byte_00_ff 	; waveform
	db data_entry_type_nybble_0_7 	; distortion type
	db data_entry_type_nybble_0_f 	; gain
	db data_entry_type_p_u 		; mix waveform pre/usr
	db data_entry_type_byte_00_ff 	; mix waveform
	db data_entry_type_byte_00_ff	; mix tonefactor

volume_data_type_table:
	db data_entry_type_nybble_0_f 	; volume
	db data_entry_type_res		; envelope restart
	db data_entry_type_up_dn	; envelope direction
	db data_entry_type_nybble_off_7	; envelope rate

duration_data_type_table:
	db data_entry_type_byte_00_ff 	; duration
	db data_entry_type_off_on	; hold mode

panning_data_type_table:
	db data_entry_type_o_l_r_c 	; panning track 1
	db data_entry_type_o_l_r_c 	; panning track 2
	db data_entry_type_o_l_r_c 	; panning track 3
	db data_entry_type_o_l_r_c 	; panning track 4
	db data_entry_type_o_l_r_c 	; autopan part 1
	db data_entry_type_o_l_r_c 	; autopan part 2
	db data_entry_type_o_l_r_c 	; autopan part 3
	db data_entry_type_o_l_r_c 	; autopan part 4
	db data_entry_type_byte_off_ff	; autopan rate

vibrato_data_type_table:
	db data_entry_type_byte_00_1f 	; vibrato depth
	db data_entry_type_byte_off_7f 	; vibrato rate
	db data_entry_type_res	 	; vibrato reset
	
portamento_data_type_table:
	db data_entry_type_note 	; portamento from
	db data_entry_type_byte_off_ff 	; portamento rate
	db data_entry_type_off_on 	; glissando mode

arpeggio_data_type_table:
	db data_entry_type_nybble_0_f 	; arpeggio part 1
	db data_entry_type_nybble_0_f	; arpeggio part 2
	db data_entry_type_nybble_0_f 	; arpeggio part 3
	db data_entry_type_nybble_0_f 	; arpeggio part 4
	db data_entry_type_byte_off_ff 	; arpeggio rate

sweep_data_type_table:
	db data_entry_type_up_dn 	; sweep direction
	db data_entry_type_nybble_0_7 	; sweep depth
	db data_entry_type_nybble_off_7	; sweep rate

song_loop_data_type_table:
;	db data_entry_type_byte_00_xx 	; song loop

trigger_data_type_table:
	db data_entry_type_byte_00_ff 	; trigger delay
	db data_entry_type_byte_off_ff 	; retrigger delay

speed_data_type_table:
	db data_entry_type_byte_00_ff 	; song speed
	db data_entry_type_byte_00_ff 	; shuffle phase
	db data_entry_type_nybble_0_f 	; shuffle rate
	db data_entry_type_nybble_off_f	; shuffle depth

transpose_data_type_table:
	db data_entry_type_signed_7f_80 ; transpose
	db data_entry_type_signed_7f_80 ; fine-tune

sample_data_type_table:
	db data_entry_type_p_u 		; sample pre/usr
	db data_entry_type_byte_00_ff 	; sample
	db data_entry_type_stop 	; stop sample

copy_paste_data_type_table:
	db data_entry_type_user_button_0 ; copy
	db data_entry_type_user_button_1 ; paste
	db data_entry_type_user_button_2 ; mutate

	end

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects poop loop
;---------------------------------------------------------------------------
;***************************************************************************
;
;edit_effects_reset_data:
;
;	ld a,codeoverbank
;	ld (current_rom_bank),a
;	ld ($2666),a
;
;	jp edit_effects_reset_data_ov
;
;***************************************************************************
;---------------------------------------------------------------------------
; edit effects save data
;---------------------------------------------------------------------------
;***************************************************************************
;
;edit_effects_save_data:
;
;	ld a,codeoverbank
;	ld (current_rom_bank),a
;	ld ($2666),a
;
;	jp edit_effects_save_data_ov
;
