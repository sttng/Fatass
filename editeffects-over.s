;---------------------------------------------------------------------------
; pattern effects edit menu routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; setup_edit_efx_over:
; edit_efx_poop_loop_over:
; mutate_effects:
; edit_effects_reset_data_ov:
; edit_effects_save_data_ov:
;---------------------------------------------------------------------------
; delete_effects_at_cursor:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables
;
; de_r_x_bits:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_edit_efx_over:

	call check_last_edit_track
	; returns b=0 if error
	ld a,b
	and a
	ret z

	ld a,edit_effects_mode
	ld (editor_mode),a

	ld a,1
	ld (data_entry_data_mode),a

	ld a,<efx_de_sel_info_table
	ld (data_entry_sel_info_table),a
	ld a,>efx_de_sel_info_table
	ld (data_entry_sel_info_table+1),a

	ld a,<efx_de_x_sel_num_table
	ld (data_entry_x_sel_num_table),a
	ld a,>efx_de_x_sel_num_table
	ld (data_entry_x_sel_num_table+1),a

	ld a,<efx_de_sel_idx_table
	ld (data_entry_sel_idx_table),a
	ld a,>efx_de_sel_idx_table
	ld (data_entry_sel_idx_table+1),a

	ld a,<efx_de_data_type_table
	ld (data_entry_data_type_table),a
	ld a,>efx_de_data_type_table
	ld (data_entry_data_type_table+1),a

	ld a,<efx_de_group_table
	ld (data_entry_groups_table),a
	ld a,>efx_de_group_table
	ld (data_entry_groups_table+1),a

	call edit_effects_reset_data_ov

	ld a,(efx_x_menu_sel)
	ld (data_entry_x_menu_sel),a

	ld a,(efx_menu_sel)
	ld b,a

	ld a,14
	ld de,effects_menu_gbc_map
	jp data_entry_menu_setup

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects poop loop
;---------------------------------------------------------------------------
;***************************************************************************

edit_efx_poop_loop_over:

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
	ld (efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (efx_menu_sel),a

	jp poop

_buttons:
	cp data_entry_type_save_button
	jr nz,_not_save
_save:
	call edit_effects_save_data_ov
_exit:
	call setup_edit_patt

	jr _done_button_process

_not_save:
	cp data_entry_type_reset_button
	jr nz,_not_reset

	call edit_effects_reset_data_ov
	call data_entry_redraw_data

_done_button_process:
	xor a
	ld (data_entry_menu_mode),a
	jp poop

_not_reset:

	cp data_entry_type_user_button_0
	jr nz,_not_copy

	ld a,(data_entry_x_menu_sel)
	ld (efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (efx_menu_sel),a

	ld c,$d0
	ld hl,data_entry_work_ram
	ld de,effects_copy_buffer
_copy_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_copy_loop

	jr _done_button_process

_not_copy:

	cp data_entry_type_user_button_1
	jr nz,_not_paste

	ld a,(data_entry_x_menu_sel)
	ld (efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (efx_menu_sel),a

	ld c,$d0
	ld hl,effects_copy_buffer
	ld de,data_entry_work_ram
_paste_loop:
	ld a,(hli)
	ld (de),a
	inc de
	dec c
	jr nz,_paste_loop

	call data_entry_redraw_data

	jr _done_button_process

_not_paste:

	cp data_entry_type_user_button_2
	jr nz,_not_mutate

	ld a,(data_entry_x_menu_sel)
	ld (efx_x_menu_sel),a

	ld a,(data_entry_menu_cur_sel)
	ld (efx_menu_sel),a

	call mutate_effects
	call data_entry_redraw_data

	jr _done_button_process

_not_mutate:

	jp poop

;***************************************************************************
;---------------------------------------------------------------------------
; mutate_effects:
;---------------------------------------------------------------------------
;***************************************************************************

mutate_effects:

	; check all on/off bits
	; if it is on, then figure out what the max is for it
	; use that to determine how much to mutate by
	; then mutate it

	ld bc,$0000

_check_bits_loop:
	push bc

	ld a,b
	swap a
	and $0f
	ld h,a

	ld a,b
	swap a
	and $f0
	add a,<data_entry_disp_bits
	ld l,a

	ld a,h
	adc a,>data_entry_disp_bits
	ld h,a

	ld a,c
	sla a
	add a,<de_r_x_bits
	ld e,a

	ld a,>de_r_x_bits
	adc a,0
	ld d,a

	ld a,(hli)	
	ld c,a
	ld a,(de)
	and c
	ld c,a
	inc de
	ld a,(hl)
	ld b,a
	ld a,(de)
	and b
	or c

	jp z,_bit_not_set		

	; ok, we need to mutate this value at y=b, x=c in our table
	; we need a random value

	; and then we need to constrain it within +8--8, and then make sure
	; that it does not go over the min/max

	pop bc
	push bc

	ld a,(data_entry_data_type_table)
	ld l,a

	ld a,b
	sla a
	add a,l
	ld l,a
	ld a,(data_entry_data_type_table+1)
	adc a,0
	ld h,a

	ld a,(hli)
	ld h,(hl)
	add a,c
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	add a,<data_entry_max_limit_table
	ld l,a
	ld a,>data_entry_max_limit_table
	adc a,0
	ld h,a

	ld a,(hl) ; this is the max value
	ld (file_access_temp),a ; we will need this shortly
	ld d,0
	and a
	jr z,_got_size

	cp 8
	jr nc,_not_real_8

	ld d,$01
	jr _got_size

_not_real_8:

	cp 32
	jr nc,_not_real_32

	ld d,$03
	jr _got_size

_not_real_32:

	cp 128
	jr nc,_not_real_128

	ld d,$07
	jr _got_size

_not_real_128:

	ld d,$0f

_got_size:
	
	; for now, here's an easy way:

	ld a,c
	add a,b
	and $0e
	add a,<cursor_colour_buffer
	ld l,a
	ld a,>cursor_colour_buffer
	adc a,0
	ld h,a

	ld a,(hli)
	ld e,a
	ld a,(hl)
	add a,e

	sra a
	ld e,a

	ld a,($ff44)
	add a,e
	ld e,a
	
	and d
	ld d,a

	; so d is our random number
	; and we can use a bit from e to toggle +/-, perhaps bit 6

	pop bc
	push bc

	ld a,b
	swap a
	and $0f
	ld h,a

	ld a,b
	swap a
	and $f0
	add a,c
	add a,<data_entry_work_ram
	ld l,a

	ld a,h
	adc a,>data_entry_work_ram
	ld h,a

	; so hl is the address to mutate

	ld a,e
	bit 6,a
	ld a,(hl)
	jr z,_sub

	add a,d
	jr nc,_no_add_over
	ld a,$ff
_no_add_over:
	ld e,a
	ld a,(file_access_temp)
	cp e
	jr nc,_no_add_over_max
	ld e,a
_no_add_over_max:
	ld a,e
	ld (hl),a
	jr _bit_not_set

_sub:
	sub d
	jr nc,_no_sub_over
	xor a
_no_sub_over:
	ld (hl),a

_bit_not_set:
	pop bc
	inc c
	ld a,c
	cp $0e
	jp nz,_check_bits_loop

	ld c,0
	inc b
	ld a,b
	cp 22
	jp nz,_check_bits_loop

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; edit effects reset data
;---------------------------------------------------------------------------
;***************************************************************************

edit_effects_reset_data_ov:

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

	call check_last_edit_track
	
	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error

	ld a,b
	and a
	jr nz,_good

	; in theory this should never happen

	ld b,b
	ld b,b
	ld b,b

_good:
	ld l,e
	ld h,d
	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a

_read_effects_loop:
	call check_hl_wram_read
	ld a,(hli)
	bit 7,a
	ret z
	bit 6,a
	ret z
	
	; line one
	; pulse width/auto pwm

	cp ctrlbt_pulse_width
	jr nz,_not_ctrlbt_pulse_width

	call check_hl_wram_read
	ld a,(hli)
	swap a
	sra a
	sra a
	and %00000011
	ld (data_entry_work_ram+$000),a

	ld a,(data_entry_work_ram+$00e)
	or %00000001
	ld (data_entry_work_ram+$00e),a

	jr _read_effects_loop

_not_ctrlbt_pulse_width:

	cp ctrlbt_auto_pwm
	jr nz,_not_ctrlbt_auto_pwm

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	swap a
	sra a
	sra a
	and %00000011
	ld (data_entry_work_ram+$001),a
	ld a,e
	swap a
	and %00000011
	ld (data_entry_work_ram+$002),a
	ld a,e
	sra a
	sra a
	and %00000011
	ld (data_entry_work_ram+$003),a
	ld a,e
	and %00000011
	ld (data_entry_work_ram+$004),a
	
	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$005),a
	
	ld a,(data_entry_work_ram+$00e)
	or %00111110
	ld (data_entry_work_ram+$00e),a

	jr _read_effects_loop

_not_ctrlbt_auto_pwm:

	; line two
	; waveform/gain/mix

	cp ctrlbt_waveform
	jr nz,_not_ctrlbt_waveform

	; no bank selection in this effect

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$011),a
	
	ld a,(data_entry_work_ram+$01e)
	or %00000011
	ld (data_entry_work_ram+$01e),a

	jr _read_effects_loop
	
_not_ctrlbt_waveform:

	cp ctrlbt_gain_waveform
	jr nz,_not_ctrlbt_gain_waveform

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	swap a
	and $0f
	ld (data_entry_work_ram+$012),a

	ld a,e
	and $0f
	ld (data_entry_work_ram+$013),a
	
	ld a,(data_entry_work_ram+$01e)
	or %00001100
	ld (data_entry_work_ram+$01e),a

	jp _read_effects_loop

_not_ctrlbt_gain_waveform:

	cp ctrlbt_mix_waveform
	jr nz,_not_ctrlbt_mix_waveform

	; no bank selection in this effect

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$015),a

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$016),a
	
	ld a,(data_entry_work_ram+$01e)
	or %01110000
	ld (data_entry_work_ram+$01e),a

	jp _read_effects_loop

_not_ctrlbt_mix_waveform:

	; line three
	; volume/envelope

	cp ctrlbt_volume
	jr nz,_not_ctrlbt_volume

	call check_hl_wram_read
	ld a,(hli)
	swap a
	and $0f
	ld (data_entry_work_ram+$020),a
	
	ld a,(data_entry_work_ram+$02e)
	or %00000001
	ld (data_entry_work_ram+$02e),a

	jp _read_effects_loop

_not_ctrlbt_volume:

	cp ctrlbt_restart_envelope
	jr nz,_not_ctrlbt_restart_env

	ld a,(data_entry_work_ram+$02e)
	or %00000010
	ld (data_entry_work_ram+$02e),a

	jp _read_effects_loop

_not_ctrlbt_restart_env:

	cp ctrlbt_envelope
	jr nz,_not_ctrlbt_envelope

	call check_hl_wram_read
	ld a,(hli)
	and $0f
	ld e,a
	sra a
	sra a
	sra a
	ld (data_entry_work_ram+$022),a

	ld a,e
	and $07
	ld (data_entry_work_ram+$023),a
	
	ld a,(data_entry_work_ram+$02e)
	or %00001100
	ld (data_entry_work_ram+$02e),a

	jp _read_effects_loop

_not_ctrlbt_envelope:
		
	; line four
	; duration/hold mode

	cp ctrlbt_duration
	jr nz,_not_ctrlbt_duration

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$030),a

	ld a,(data_entry_work_ram+$03e)
	or %00000001
	ld (data_entry_work_ram+$03e),a

	jp _read_effects_loop

_not_ctrlbt_duration:

	cp ctrlbt_hold_mode
	jr nz,_not_ctrlbt_hold_mode

	call check_hl_wram_read
	ld a,(hli)
	cpl
	and $01
	ld (data_entry_work_ram+$031),a

	ld a,(data_entry_work_ram+$03e)
	or %00000010
	ld (data_entry_work_ram+$03e),a

	jp _read_effects_loop

_not_ctrlbt_hold_mode:

	; line five
	; panning/auto pwm

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
	ld (data_entry_work_ram+$040),a

	sra e
	ld a,e
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$041),a

	sra e
	ld a,e
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$042),a

	sra e
	ld a,e
	and %0_0_0_0_0_0_0_1
	ld d,a
	ld a,e
	swap a
	and %0_0_0_0_0_0_0_1
	sla a
	or d
	ld (data_entry_work_ram+$043),a

	ld a,(data_entry_work_ram+$04e)
	or %00001111
	ld (data_entry_work_ram+$04e),a

	jp _read_effects_loop

_not_ctrlbt_panning:

	cp ctrlbt_auto_panner
	jr nz,_not_ctrlbt_auto_panner

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	swap a
	sra a
	sra a
	and %00000011
	ld (data_entry_work_ram+$044),a
	ld a,e
	swap a
	and %00000011
	ld (data_entry_work_ram+$045),a
	ld a,e
	sra a
	sra a
	and %00000011
	ld (data_entry_work_ram+$046),a
	ld a,e
	and %00000011
	ld (data_entry_work_ram+$047),a
	
	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$048),a
	
	ld a,(data_entry_work_ram+$04e)
	or %11110000
	ld (data_entry_work_ram+$04e),a

	ld a,(data_entry_work_ram+$04f)
	or %00000001
	ld (data_entry_work_ram+$04f),a

	jp _read_effects_loop

_not_ctrlbt_auto_panner:

	; line six
	; vibrato/vibrato reset

	cp ctrlbt_vibrato
	jr nz,_not_ctrlbt_vibrato

	call check_hl_wram_read
	ld a,(hli)
	and $1f
	ld (data_entry_work_ram+$050),a

	call check_hl_wram_read
	ld a,(hli)
	and $7f
	ld (data_entry_work_ram+$051),a

	ld a,(data_entry_work_ram+$05e)
	or %00000011
	ld (data_entry_work_ram+$05e),a

	jp _read_effects_loop

_not_ctrlbt_vibrato:

	cp ctrlbt_vibrato_reset
	jr nz,_not_ctrlbt_vibrato_reset

	ld a,(data_entry_work_ram+$05e)
	or %00000100
	ld (data_entry_work_ram+$05e),a

	jp _read_effects_loop

_not_ctrlbt_vibrato_reset:

	; line 7
	; porta from/portamento/glissando

	cp ctrlbt_porta_from
	jr nz,_not_ctrlbt_porta_from

	call check_hl_wram_read
	ld a,(hli)
	dec a
	and $7f
	ld (data_entry_work_ram+$060),a

	ld a,(data_entry_work_ram+$06e)
	or %00000001
	ld (data_entry_work_ram+$06e),a

	jp _read_effects_loop

_not_ctrlbt_porta_from:

	cp ctrlbt_portamento
	jr nz,_not_ctrlbt_portamento

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$061),a

	ld a,(data_entry_work_ram+$06e)
	or %00000010
	ld (data_entry_work_ram+$06e),a

	jp _read_effects_loop

_not_ctrlbt_portamento:
	
	cp ctrlbt_glissando
	jr nz,_not_ctrlbt_glissando

	call check_hl_wram_read
	ld a,(hli)
	and $01
	ld (data_entry_work_ram+$062),a

	ld a,(data_entry_work_ram+$06e)
	or %00000100
	ld (data_entry_work_ram+$06e),a

	jp _read_effects_loop

_not_ctrlbt_glissando:

	; line eight
	; arpeggio

	cp ctrlbt_arpeggio
	jr nz,_not_ctrlbt_arpeggio

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	swap a
	and $0f
	ld (data_entry_work_ram+$070),a

	ld a,e
	and $0f
	ld (data_entry_work_ram+$071),a

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	swap a
	and $0f
	ld (data_entry_work_ram+$072),a

	ld a,e
	and $0f
	ld (data_entry_work_ram+$073),a

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$074),a

	ld a,(data_entry_work_ram+$07e)
	or %00011111
	ld (data_entry_work_ram+$07e),a

	jp _read_effects_loop

_not_ctrlbt_arpeggio:

	; line nine
	; sweep

	cp ctrlbt_sweep
	jr nz,_not_ctrlbt_sweep

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	cpl
	and %0_000_1_000
	sla a
	swap a
	ld (data_entry_work_ram+$080),a

	ld a,e
	and $07
	ld (data_entry_work_ram+$081),a

	ld a,e
	and %0_111_0_000
	swap a
	ld (data_entry_work_ram+$082),a

	ld a,(data_entry_work_ram+$08e)
	or %00000111
	ld (data_entry_work_ram+$08e),a

	jp _read_effects_loop

_not_ctrlbt_sweep:

	; line ten
	; trigger delay/retrigger delay

	cp ctrlbt_trigger_delay
	jr nz,_not_ctrlbt_trigger_delay

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$090),a

	ld a,(data_entry_work_ram+$09e)
	or %00000001
	ld (data_entry_work_ram+$09e),a

	jp _read_effects_loop

_not_ctrlbt_trigger_delay:

	cp ctrlbt_retrigger
	jr nz,_not_ctrlbt_retrigger

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$091),a

	ld a,(data_entry_work_ram+$09e)
	or %00000010
	ld (data_entry_work_ram+$09e),a

	jp _read_effects_loop

_not_ctrlbt_retrigger:

	; line eleven
	; song speed/shuffle

	cp ctrlbt_song_speed
	jr nz,_not_ctrlbt_song_speed

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$0a0),a

	ld a,(data_entry_work_ram+$0ae)
	or %00000001
	ld (data_entry_work_ram+$0ae),a

	jp _read_effects_loop

_not_ctrlbt_song_speed:
	
	cp ctrlbt_shuffle
	jr nz,_not_ctrlbt_shuffle

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$0a1),a

	call check_hl_wram_read
	ld a,(hli)
	ld e,a
	and $0f
	ld (data_entry_work_ram+$0a2),a

	ld a,e
	swap a
	and $0f
	ld (data_entry_work_ram+$0a3),a

	ld a,(data_entry_work_ram+$0ae)
	or %00001110
	ld (data_entry_work_ram+$0ae),a

	jp _read_effects_loop

_not_ctrlbt_shuffle:

	; line twelve
	; transpose/fine tune

	cp ctrlbt_transpose
	jr nz,_not_ctrlbt_transpose

	call check_hl_wram_read
	ld a,(hli)	
	add a,$80
	ld (data_entry_work_ram+$0b0),a

	ld a,(data_entry_work_ram+$0be)
	or %00000001
	ld (data_entry_work_ram+$0be),a

	jp _read_effects_loop

_not_ctrlbt_transpose:

	cp ctrlbt_fine_tune
	jr nz,_not_ctrlbt_fine_tune

	call check_hl_wram_read
	ld a,(hli)
	add a,$80
	ld (data_entry_work_ram+$0b1),a

	ld a,(data_entry_work_ram+$0be)
	or %00000010
	ld (data_entry_work_ram+$0be),a

	jp _read_effects_loop

_not_ctrlbt_fine_tune:

	; line thirteen
	; sample/stop sample

	cp ctrlbt_sample
	jr nz,_not_ctrlbt_sample

	call check_hl_wram_read
	ld a,(hli)
	ld (data_entry_work_ram+$0c1),a

	ld a,(data_entry_work_ram+$0ce)
	or %00000011
	ld (data_entry_work_ram+$0ce),a

	jp _read_effects_loop

_not_ctrlbt_sample:

	cp ctrlbt_stop_sample
	jr nz,_not_ctrlbt_stop_sample

	ld a,(data_entry_work_ram+$0ce)
	or %00000100
	ld (data_entry_work_ram+$0ce),a

	jp _read_effects_loop

_not_ctrlbt_stop_sample:

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
; edit effects save data
;---------------------------------------------------------------------------
;***************************************************************************

edit_effects_save_data_ov:

	call delete_effects_at_cursor

	; ok, now we need to scan through all our effects and save them if necessary... fun.

	call check_last_edit_track
	
	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error

	; line one
	; pulse width/auto pwm

	ld a,(data_entry_work_ram+$00e)
	and %00000001
	jr z,_not_ctrlbt_pulse_width

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_pulse_width
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$000)
	sla a
	sla a
	swap a
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_pulse_width:

	ld a,(data_entry_work_ram+$00e)
	and %00111110
	jr z,_not_ctrlbt_auto_pwm

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_auto_pwm
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back

	ld a,(data_entry_work_ram+$001)
	sla a
	sla a
	swap a
	ld b,a

	ld a,(data_entry_work_ram+$002)
	swap a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$003)
	sla a
	sla a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$004)
	or b
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$005)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_auto_pwm:

	; line two
	; waveform/gain/mix

	ld a,(data_entry_work_ram+$01e)
	and %00000011
	jr z,_not_ctrlbt_waveform

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_waveform
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$011)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_waveform:

	ld a,(data_entry_work_ram+$01e)
	and %00001100
	jr z,_not_ctrlbt_gain_waveform

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_gain_waveform
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$012)
	swap a
	ld b,a
	ld a,(data_entry_work_ram+$013)
	or b
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_gain_waveform:

	ld a,(data_entry_work_ram+$01e)
	and %01110000
	jr z,_not_ctrlbt_mix_waveform

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_mix_waveform
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$015)
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$016)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_mix_waveform:

	; line three
	; volume/envelope

	ld a,(data_entry_work_ram+$02e)
	and %00000001
	jr z,_not_ctrlbt_volume

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_volume
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$020)
	swap a
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_volume:

	ld a,(data_entry_work_ram+$02e)
	and %00000010
	jr z,_not_ctrlbt_restart_env

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_restart_envelope
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_restart_env:

	ld a,(data_entry_work_ram+$02e)
	and %00001100
	jr z,_not_ctrlbt_envelope

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_envelope
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$022)
	sla a
	sla a
	sla a
	ld b,a
	ld a,(data_entry_work_ram+$023)
	or b
	ld (hl),a
	call check_cde_wram_incr
	
_not_ctrlbt_envelope:

	; line four
	; duration/hold mode

	ld a,(data_entry_work_ram+$03e)
	and %00000001
	jr z,_not_ctrlbt_duration

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_duration
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$030)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_duration:

	ld a,(data_entry_work_ram+$03e)
	and %00000010
	jr z,_not_ctrlbt_hold_mode

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_hold_mode
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$031)
	xor 1
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_hold_mode:

	; line five
	; panning/auto pwm

	ld a,(data_entry_work_ram+$04e)
	and %00001111
	jr z,_not_ctrlbt_panning

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_panning
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back

	ld a,(data_entry_work_ram+$040)
	and %0_0_0_0_0_0_0_1
	ld b,a
	ld a,(data_entry_work_ram+$040)
	and %0_0_0_0_0_0_1_0
	sra a
	swap a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$041)
	and %0_0_0_0_0_0_0_1
	sla a
	or b
	ld b,a
	ld a,(data_entry_work_ram+$041)
	and %0_0_0_0_0_0_1_0
	swap a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$042)
	and %0_0_0_0_0_0_0_1
	sla a
	sla a
	or b
	ld b,a
	ld a,(data_entry_work_ram+$042)
	and %0_0_0_0_0_0_1_0
	swap a
	sla a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$043)
	and %0_0_0_0_0_0_0_1
	sla a
	sla a
	sla a
	or b
	ld b,a
	ld a,(data_entry_work_ram+$043)
	and %0_0_0_0_0_0_1_0
	swap a
	sla a
	sla a
	or b

	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_panning:

	ld a,(data_entry_work_ram+$04e)
	and %11110000
	ld b,a
	ld a,(data_entry_work_ram+$04f)
	and %00000001
	or b
	jr z,_not_ctrlbt_auto_panner

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_auto_panner
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back

	ld a,(data_entry_work_ram+$044)
	sla a
	sla a
	swap a
	ld b,a

	ld a,(data_entry_work_ram+$045)
	swap a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$046)
	sla a
	sla a
	or b
	ld b,a

	ld a,(data_entry_work_ram+$047)
	or b

	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$048)
	ld (hl),a
	call check_cde_wram_incr
	
_not_ctrlbt_auto_panner:

	; line six
	; vibrato/vibrato reset

	ld a,(data_entry_work_ram+$05e)
	and %00000011
	jr z,_not_ctrlbt_vibrato

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_vibrato
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$050)
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$051)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_vibrato:

	ld a,(data_entry_work_ram+$05e)
	and %00000100
	jr z,_not_ctrlbt_vibrato_reset

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_vibrato_reset
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_vibrato_reset:

	; line 7
	; porta from/portamento/glissando

	ld a,(data_entry_work_ram+$06e)
	and %00000001
	jr z,_not_ctrlbt_porta_from

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_porta_from
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$060)
	inc a
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_porta_from:

	ld a,(data_entry_work_ram+$06e)
	and %00000010
	jr z,_not_ctrlbt_portamento

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_portamento
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$061)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_portamento:
	
	ld a,(data_entry_work_ram+$06e)
	and %00000100
	jr z,_not_ctrlbt_glissando

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_glissando
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$062)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_glissando:

	; line eight
	; arpeggio

	ld a,(data_entry_work_ram+$07e)
	and %00011111
	jr z,_not_ctrlbt_arpeggio

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_arpeggio
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$070)
	swap a
	ld b,a
	ld a,(data_entry_work_ram+$071)
	or b
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$072)
	swap a
	ld b,a
	ld a,(data_entry_work_ram+$073)
	or b
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$074)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_arpeggio:

	; line nine
	; sweep

	ld a,(data_entry_work_ram+$08e)
	and %00000111
	jr z,_not_ctrlbt_sweep

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_sweep
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$080)
	swap a
	sra a
	ld b,a
	ld a,(data_entry_work_ram+$081)
	or b
	ld b,a
	ld a,(data_entry_work_ram+$082)
	swap a
	or b
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_sweep:

	; line ten
	; trigger delay/retrigger delay

	ld a,(data_entry_work_ram+$09e)
	and %00000001
	jr z,_not_ctrlbt_trigger_delay

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_trigger_delay
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$090)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_trigger_delay:

	ld a,(data_entry_work_ram+$09e)
	and %00000010
	jr z,_not_ctrlbt_retrigger

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_retrigger
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$091)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_retrigger:

	; line eleven
	; song speed/shuffle

	ld a,(data_entry_work_ram+$0ae)
	and %00000001
	jr z,_not_ctrlbt_song_speed

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_song_speed
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$0a0)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_song_speed:
	
	ld a,(data_entry_work_ram+$0ae)
	and %00001110
	jr z,_not_ctrlbt_shuffle

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_shuffle
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$0a1)
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$0a2)
	ld b,a
	ld a,(data_entry_work_ram+$0a3)
	swap a
	or b
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_shuffle:

	; line twelve
	; transpose/fine tune

	ld a,(data_entry_work_ram+$0be)
	and %00000001
	jr z,_not_ctrlbt_transpose

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_transpose
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$0b0)
	sub $80
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_transpose:

	ld a,(data_entry_work_ram+$0be)
	and %00000010
	jr z,_not_ctrlbt_fine_tune

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_fine_tune
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$0b1)
	sub $80
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_fine_tune:

	; line thirteen
	; sample/stop sample

	ld a,(data_entry_work_ram+$0ce)
	and %00000011
	jr z,_not_ctrlbt_sample

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_sample
	ld (hl),a
	call check_cde_wram_incr

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,(data_entry_work_ram+$0c1)
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_sample:

	ld a,(data_entry_work_ram+$0ce)
	and %00000100
	jr z,_not_ctrlbt_stop_sample

	call insert_byte_at_cde
	call check_hl_wram_back
	ld a,ctrlbt_stop_sample
	ld (hl),a
	call check_cde_wram_incr

_not_ctrlbt_stop_sample:

	call update_song_info_disp
	jp redraw_patt_info

;***************************************************************************
;---------------------------------------------------------------------------
; delete effects at cursor
;---------------------------------------------------------------------------
;***************************************************************************

delete_effects_at_cursor:

	; well, first we should delete all the effects that are in there,
	; then we should insert all our new effects.  Yes, that makes the
	; most sense.  Awesome, it will destroy any macros.  FUN FUN!

	call check_last_edit_track
	
	; retrieves address in bhl
	; retrieves address of effect in cde
	; retrieves value in a
	; returns b=0 if error

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

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

de_r_x_bits:
	db %00000001,%00000000
	db %00000010,%00000000
	db %00000100,%00000000
	db %00001000,%00000000
	db %00010000,%00000000
	db %00100000,%00000000
	db %01000000,%00000000
	db %10000000,%00000000
	db %00000000,%00000001
	db %00000000,%00000010
	db %00000000,%00000100
	db %00000000,%00001000
	db %00000000,%00010000
	db %00000000,%00100000

	end
