;***************************************************************************
; core music routines v2.12a
; 11:07 PM 3/10/00
;***************************************************************************
;
;---------------------------------------------------------------------------
; bugs:
; -speed fluctuations in some of randy's songs
; -changing waveforms cuts sound output, you need to put restart after
; -mixing waveforms uses the waveforms PAST the dest waveform if you pick a
;  high note.
; -restart envelope seems to be putting a weird tone in portamentos in one
;  of randy's songs
; -big performance hit when it reaches the end of a pattern and preparing the
;  next one-> should optimize that code
;---------------------------------------------------------------------------
; make options for quick/accurate player.  On doublespeeds we can probably
; use quick player which updates the patterns accross 2 calls (3&4, then 1&2)?
;---------------------------------------------------------------------------
; **** Need to test the following more thoroughly:
; -everything
;***************************************************************************

	include music_player_2_h.inc

;---------------------------------------------------------------------------
; setup/reset the music player & sound hardware
;---------------------------------------------------------------------------

;***************************************************************************
;***************************************************************************
;***************************************************************************
;---------------------------------------------------------------------------
; mus_hard_reset
; turns sound off and then back on to reset sound hardware, then sets up song
;-call with interrupts disabled for best results
;---------------------------------------------------------------------------

mus_hard_reset:

	xor a			; turn all sound off - do this first
	ld ($ff26),a

	ld a,%1_000_0_0_0_0	; turn all sound on - do this first
	ld ($ff26),a

;***************************************************************************
;---------------------------------------------------------------------------
;***************************************************************************
;---------------------------------------------------------------------------
; mus_reset
; sets up song and sound hardware
;-call with interrupts disabled for best results
;-call using the mainbank wrapper
;---------------------------------------------------------------------------

mus_reset:

	ld a,(fast_forward)		; @@@@ editor required
	ld (editor_ff_temp),a		; @@@@ editor required
	ld a,1				; @@@@ editor required
	ld (fast_forward),a		; @@@@ editor required

	ld a,%0_111_0_111 	; set max volume, and turn both sides on
	ld ($ff24),a

	ld a,%1_1_0_1_1_1_1_0	; turn output for all sounds on both sides on
	ld ($ff25),a

	xor a			; clear track parameters
	ld (mus_song_transpose),a

	ld (mus_song_speed_counter),a	; set the song timer to 0

	ld hl,mus_track_1		; wipe track ram
	ld b,$34*4

_mrs_wipe_track_ram:
	ld (hli),a
	dec b
	jr nz,_mrs_wipe_track_ram

	ld ($ff13),a	; set the initial, counter, pitch and reset to 0
	ld ($ff14),a
	ld ($ff18),a
	ld ($ff19),a
	ld ($ff1d),a
	ld ($ff1e),a
	ld ($ff22),a
	ld ($ff23),a

	ld a,$ff
	ld (mus_track_1+o_trigger_cnt),a
	ld (mus_track_2+o_trigger_cnt),a
	ld (mus_track_3+o_trigger_cnt),a
	ld (mus_track_4+o_trigger_cnt),a
	ld a,>mus_note_table
	ld (mus_track_1+o_play_note+1),a
	ld (mus_track_1+o_source_note+1),a
	ld (mus_track_1+o_dest_note+1),a
	ld (mus_track_2+o_play_note+1),a
	ld (mus_track_2+o_source_note+1),a
	ld (mus_track_2+o_dest_note+1),a
	ld (mus_track_3+o_play_note+1),a
	ld (mus_track_3+o_source_note+1),a
	ld (mus_track_3+o_dest_note+1),a
	ld a,>mus_noisefreq_table
	ld (mus_track_4+o_play_note+1),a
	ld (mus_track_4+o_source_note+1),a
	ld (mus_track_4+o_dest_note+1),a
	ld a,<mus_vibrato_sinus_0
	ld (mus_track_1+o_vibrato_table),a
	ld (mus_track_2+o_vibrato_table),a
	ld (mus_track_3+o_vibrato_table),a
	ld (mus_track_4+o_vibrato_table),a
	ld a,>mus_vibrato_sinus_0
	ld (mus_track_1+o_vibrato_table+1),a
	ld (mus_track_2+o_vibrato_table+1),a
	ld (mus_track_3+o_vibrato_table+1),a
	ld (mus_track_4+o_vibrato_table+1),a

;	ld a,mus_flag_continuous
;	ld (mus_track_1+o_hold_mode),a
;	ld (mus_track_2+o_hold_mode),a
;	ld (mus_track_3+o_hold_mode),a
;	ld (mus_track_4+o_hold_mode),a
	ld a,%0_000_0_000	; set sound 1 sweep
	ld ($ff10),a	
	ld (mus_track_1+o_sweep),a
	ld a,%0_0_0_1_0_0_0_0	; set panning
	ld (mus_track_1+o_panning),a
	ld a,%0_0_0_0_0_0_1_0
	ld (mus_track_2+o_panning),a
	ld a,%0_1_0_0_0_1_0_0
	ld (mus_track_3+o_panning),a
	sla a
	ld (mus_track_4+o_panning),a

	ld a,%00_000001		; set sound 1 pulse width and duration
	ld ($ff11),a
	ld (mus_track_1+o_pw_dur),a

	ld a,%10_000000		; set sound 2 pulse width and duration
	ld ($ff16),a
	ld (mus_track_2+o_pw_dur),a

	ld a,%00000011		; set sound 3 duration !!!! 8 bit for sound 3
	ld ($ff1b),a
	ld (mus_track_3+o_pw_dur),a

	ld a,%00_000111		; set sound 4 duration
	ld ($ff20),a
	ld (mus_track_4+o_pw_dur),a

	ld a,%1111_0_011	; set sound 1 initial volume and envelope
	ld ($ff12),a
	ld (mus_track_1+o_envelope),a

	ld a,%0111_0_011	; set sound 2 initial volume and envelope
	ld ($ff17),a
	ld (mus_track_2+o_envelope),a

	ld a,%0_01_00000	; set sound 3 initial volume !!!! 2 bit for sound 3
	ld ($ff1c),a
	ld (mus_track_3+o_envelope),a

	ld a,%1000_0_011	; set sound 4 initial volume and envelope
	ld ($ff21),a
	ld (mus_track_4+o_envelope),a

	ld hl,mus_default_waveform	; set the default waveform
	ld de,mus_waveform
	ld bc,$1030

	xor a		; shut off sound 3
	ld ($ff1a),a
_wave_setup_loop:
	ld a,(hli)
	ld (de),a
	ld (c),a
	inc e
	inc c
	dec b
	jr nz,_wave_setup_loop

	ld a,%10000000	; turn on sound 3
	ld ($ff1a),a

	; set the default song speed

	ld a,<mus_shuffle_sinus_0
	ld (mus_song_shuffle_table),a	; shuffle counter
	ld a,>mus_shuffle_sinus_0
	ld (mus_song_shuffle_table+1),a	; shuffle counter

	ld a,mus_def_speed
;	sla a				; ^^^^ poopeop
	ld (mus_song_speed),a

	;---------------------------------------------------------------------------
	; pattern position setup
	;---------------------------------------------------------------------------

	; get the song bank

;	ld hl,mus_song_bank_table 	;##### required for standalone
;	ld a,(mus_song_number) 		;##### required for standalone
;	ld c,a			 	;##### required for standalone
;	ld b,0			 	;##### required for standalone
;	add hl,bc		 	;##### required for standalone
;	ld a,(hl)		 	;##### required for standalone
;	ld (mus_song_bank),a	 	;##### required for standalone

	; reset pattern sequence to first part

	call mus_reset_sequence

	; initialize shuffle correctly

	ld a,(mus_song_shuffle)		; get shuffle rate
	ld hl,mus_shuffle_rates
	or l
	ld l,a
	ld a,(hli)
	ld d,(hl)
	ld e,a

	ld hl,mus_song_shuffle_count
	ld a,(hli)			; get counter
	sub e
	ld e,a
	ld a,(hl)
	sbc a,d

	and $0f

	ld d,a				; store it backwards
	ld (hld),a	
	ld a,e
	ld (hld),a

	dec l

	ld a,(hl)			; update position
	and $f0
	or d
	ld (hl),a

	ld a,(editor_ff_temp)		; @@@@ editor required
	ld (fast_forward),a		; @@@@ editor required

	ret

;***************************************************************************
;---------------------------------------------------------------------------
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;---------------------------------------------------------------------------
;-main music player routine
; call one (or more) times per vbl using the mainbank wrapper
;---------------------------------------------------------------------------

mus_player:

	; update and check song timer

	ld a,(mus_song_shuffle_table)
	ld l,a
	ld a,(mus_song_shuffle_table+1)
	ld h,a

	ld c,(hl)
	
	ld a,(mus_song_speed)
	ld b,a

	ld hl,$0000

	ld a,c
	cp $20
	jr c,_less_than_20

	sub $20
	ld c,a

	ld a,b
	swap a
	ld e,a
	
	and $0f
	ld d,a

	ld a,e
	and $f0
	ld e,a

	add hl,de
	add hl,de

	ld a,c

_less_than_20:

	cp $10
	jr c,_less_than_10

	sub $10
	ld c,a

	ld a,b
	swap a
	ld e,a
	
	and $0f
	ld d,a

	ld a,e
	and $f0
	ld e,a

	add hl,de

	ld a,c

_less_than_10:

	cp $08
	jr c,_less_than_8

	sub $08
	ld c,a

	ld e,b
	xor a

	sla e
	rla
	sla e
	rla
	sla e
	rla
	ld d,a

	add hl,de

	ld a,c

_less_than_8:

	cp $04
	jr c,_less_than_4

	sub $04
	ld c,a

	ld e,b
	xor a

	sla e
	rla
	sla e
	rla
	ld d,a

	add hl,de

	ld a,c

_less_than_4:

	cp $02
	jr c,_less_than_2

	sub $02
	ld c,a

	ld e,b
	ld d,0

	add hl,de
	add hl,de

	ld a,c

_less_than_2:

	cp $01
	jr c,_less_than_1

	ld e,b
	ld d,0

	add hl,de

_less_than_1:

	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l

	xor a
	cp l
	jr nz,_not_zero_speed
	inc l
_not_zero_speed:
	ld a,(mus_song_speed_counter)
	add a,l
	ld (mus_song_speed_counter),a
	jr nc,mus_done_pattern_update

_mus_pattern_update:
	ld a,(mus_song_shuffle)		; get shuffle rate
	ld hl,mus_shuffle_rates
	or l
	ld l,a
	ld a,(hli)
	ld d,(hl)
	ld e,a

	ld hl,mus_song_shuffle_count
	ld a,(hli)			; get counter
	add a,e
	ld e,a
	ld a,(hl)
	adc a,d

	and $0f

	ld d,a				; store it backwards
	ld (hld),a	
	ld a,e
	ld (hld),a

	dec l

	ld a,(hl)			; update position
	and $f0
	or d
	ld (hl),a

	; clear song flags

mus_player_loop:

	xor a
	ld (mus_song_flags),a

	ld a,(editor_line)	; @@@@ editor required
	inc a			; @@@@ editor required
	ld (editor_line),a	; @@@@ editor required

	ld hl,mus_track_1+o_trigger_flag
	inc (hl)	; increment trigger flag
	inc l
	ld a,(hli)	; get trigger delay
	ld (hl),a	; store it in counter

	ld l,<(mus_track_2+o_trigger_flag)
	inc (hl)	; increment trigger flag
	inc l
	ld a,(hli)	; get trigger delay
	ld (hl),a	; store it in counter

	ld l,<(mus_track_3+o_trigger_flag)
	inc (hl)	; increment trigger flag
	inc l
	ld a,(hli)	; get trigger delay
	ld (hl),a	; store it in counter

	ld l,<(mus_track_4+o_trigger_flag)
	inc (hl)	; increment trigger flag
	inc l
	ld a,(hli)	; get trigger delay
	ld (hl),a	; store it in counter

mus_done_pattern_update:

	; update sound registers and effects here

	;---------------------------------------------------------------------------
	; track 1
	;---------------------------------------------------------------------------

	ld a,<mus_track_1
	call mus_upate_track_effects

	ld a,<mus_track_2
	call mus_upate_track_effects

	ld a,(mus_sample_finished)
	and a
	jr z,_no_sample_finished
	xor a
	ld (mus_sample_finished),a
	
	ld a,(mus_track_3+o_reg_flags)	; tell the music player to reload the waveform
	or mus_flag_set_waveform+mus_flag_set_panning+mus_flag_set_envelope
	ld (mus_track_3+o_reg_flags),a

	ld a,(mus_track_3+o_hold_mode)	; tell the music player to reload the panning & volume
	or mus_flag_set_initial
	ld (mus_track_3+o_hold_mode),a

_no_sample_finished:

	ld a,<mus_track_3
	call mus_upate_track_effects

	ld a,<mus_track_4
	call mus_upate_track_effects

	; check for pattern end

	ld a,(mus_song_flags)
	bit mus_bit_pattern_end,a
	ret z
	; advance pattern
	call music_advance_sequence
	jr mus_player_loop

	ret

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************

;***************************************************************************
;***************************************************************************
;***************************************************************************
;---------------------------------------------------------------------------
; music_advance_pattern:
; call here with track number to work on in a
;---------------------------------------------------------------------------
;***************************************************************************
;***************************************************************************
;***************************************************************************

music_advance_pattern:

	; store track offset

	ld a,c
	ld (mus_song_track),a

	; check wait_lines

	add a,o_wait_lines
	ld l,a
	ld a,(hl)
	and a
	jr z,_no_wait
	dec (hl)
	ret

_no_wait:
	; get track pattern index

	ld l,c
	ld a,(hli)
	ld d,(hl)
	ld e,a

;***************************************************************************
; music_advance_pattern_loop: jump here to continue checking track for parameters
;***************************************************************************
mus_nop:
music_advance_pattern_loop:

	; read parameter

	call music_read_parameter_de

	; check for note

	and a			; $00 is now the note off
	jp z,mus_set_note_off
	
	bit 7,a			; $01-$7f are reserved for notes/noise freqs
	jr z,mus_set_note

	bit 6,a			; $80-$bf are reserved for wait_lines
	jr nz,_do_effect

	; do wait_lines

	and $3f
	ld b,a

	ld a,(mus_song_track)
	add a,o_wait_lines
	ld l,a
	ld h,>mus_ram
	ld (hl),b
	jr mus_wait

_do_effect:
	; convert to word offset

	and $3f
	ld c,a
	sla c			; *2 for words
	ld b,0			; so no carry, thank you

	; convert to index

	ld hl,music_routines_table
	add hl,bc

	; get address of routine

	ld a,(hli)
	ld h,(hl)
	ld l,a

	; call routine

	jp (hl)

;***************************************************************************
;-music_advance_pattern_done: jump here after done checking parameters for this track
;***************************************************************************
mus_wait:
music_advance_pattern_done:

	; store track pattern index

	ld a,(mus_song_track)
	ld l,a
	ld h,>mus_ram
	ld a,e
	ld (hli),a
	ld (hl),d

	ret
;---------------------------------------------------------------------------
;***************************************************************************
;***************************************************************************
;***************************************************************************
; mus_set_note:
; this sets the note to be played for the tracks
;---------------------------------------------------------------------------
mus_set_note:

	; set up variables

;	ld b,a			; !!!! read paramater puts it in b anyway
	ld h,>mus_ram

	ld a,(mus_song_track)
	ld c,a

	; check for track 4 which uses a different note-table

	cp <mus_track_4
	jr nz,_not_mus_msn_track_4

	ld l,<mus_track_4+o_hold_mode	; tone mode flag is in hold mode

	ld a,b
	bit 6,a
	jr nz,_tone_mode

	; noise mode

	ld a,(hl)
	and $ff-mus_flag_tone_mode		; clear tone flag
	ld (hl),a

	jr _mus_msn_track_4

_tone_mode:

	; tone mode

	and $3f					; restrain to $00-$3f
	ld b,a

	ld a,(hl)
	or mus_flag_tone_mode			; set tone flag
	ld (hl),a

	jr _mus_msn_track_4

;---------------------------------------------------------------------------
;-$01-$48 ctrlbt_c_3 - ctrlbt_b_8 play note---------------------------------

_not_mus_msn_track_4:

	; ensure it's valid

	ld a,b
	cp ctrlbt_b_8+1
	jr nc,music_advance_pattern_done

_mus_msn_track_4:

	; check portamento

	push de

	ld a,o_porta_speed
	add a,c
	ld l,a
	ld a,(hl)
	and a
	ld a,o_dest_note
	jr nz,_porta_on

;;;

	ld a,o_hold_mode			; set the initial flag
	add a,c
	ld l,a
	ld a,(hl)
	and $ff-mus_flag_set_sample
	or mus_flag_set_initial
	ld (hl),a

;;; moved from ;;; to make portamento destinations not affect this flag

	ld a,o_play_note
_porta_on:
	add a,c
	ld e,a
	ld d,h

	; set up note storage

	ld a,o_source_note
	add a,c
	ld l,a

	; multiply it by 128 because the notetable is stored in 64s as words
	; quick way, *256/2

	xor a

	srl b
	rr a

	; set the note table index

	ld (hli),a
	ld (de),a
	inc e
	ld a,b
	add a,>mus_note_table
	ld (hl),a
	ld (de),a

	pop de

	; set note flag

;;;	ld a,o_hold_mode			; set the initial flag
;;;	add a,c
;;;	ld l,a
;;;	ld a,(hl)
;;;	and $ff-mus_flag_set_sample
;;;	or mus_flag_set_initial
;;;	ld (hl),a
;;; moved to ;;; just up there to make portas not set initial on new dest note

	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	and $ff-mus_flag_set_note_off
	or mus_flag_set_note+mus_flag_set_panning+mus_flag_set_hold_mode+mus_flag_set_pw_dur	; must set pw_dur after note to avoid bad bug
	ld (hl),a

	jr music_advance_pattern_done

;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
; effects are here
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************
;***************************************************************************

;---------------------------------------------------------------------------
; mus_macro: sets up a macro
;---------------------------------------------------------------------------
mus_macro:

	; set up destination
	ld a,(mus_song_track)
	add a,o_macro_return
	ld l,a
	ld h,>mus_ram

	; get value of macro into bc
	call music_read_parameter_de
	ld c,a
	call music_read_parameter_de
;	ld b,a ; music read parameter does that

	; test for nested macros, that's a nono

	ld a,(hli)
	and a
	jp nz,music_advance_pattern_loop
	ld a,(hld)
	and a
	jp nz,music_advance_pattern_loop

	; store current de for macro_return

	ld a,e
	ld (hli),a
	ld (hl),d

	; set de to our macro address

	ld a,(mus_song_pat_address)
	ld l,a
	ld a,(mus_song_pat_address+1)
	ld h,a

	add hl,bc
	ld e,l
	ld d,h

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_auto_pwm: sets up an auto-pulse-width-modulator for the current track
;---------------------------------------------------------------------------
mus_auto_pwm:
	; set up destination
	ld a,(mus_song_track)
	; restrict to tracks 1&2
	cp <mus_track_3
	jr c,_restricted
	; abort
	inc de
	jp music_advance_pattern_loop
_restricted:
	ld c,a
	ld h,>mus_ram
	add a,o_auto_pwm
	ld l,a
	; get value
	call music_read_parameter_de
	ld (hli),a
	call music_read_parameter_de
	ld (hli),a
	ld (hl),a
	and a				; check for off
	jp nz,music_advance_pattern_loop

	; reset pulse_width

	ld a,o_reg_flags	; set register update flag
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_pw_dur
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_auto_panner: sets up an auto-panner for the current track
;---------------------------------------------------------------------------
mus_auto_panner:
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_auto_panner
	ld l,a
	; get value
	call music_read_parameter_de
	ld (hli),a
	call music_read_parameter_de
	ld (hli),a
	ld (hl),a
	and a				; check for off
	jp nz,music_advance_pattern_loop

	; reset panning

	ld a,o_reg_flags	; set register update flag
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_panning
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_gain_waveform: applies the specified gain matrix to the waveform
;---------------------------------------------------------------------------
mus_gain_waveform:
	; set up destination
	ld a,(mus_song_track)
	; restrict to track 3
	cp <mus_track_3
	jr z,_restricted
	; abort
	inc de
	inc de
	jp music_advance_pattern_loop
_restricted:
	
;	ld c,a
	ld h,>mus_ram
	add a,o_reg_flags	; set register update flag
	ld l,a
	ld a,(hl)
	or mus_flag_set_waveform
	ld (hl),a
	; get gain matrix index
	call music_read_parameter_de

	ld c,a
	ld b,0
	sla c
	rl b
	sla c
	rl b
	sla c
	rl b
	
	ld hl,mus_gain_matrix
	add hl,bc

	ld bc,mus_waveform
	push de
	push hl
_gain_loop:
	ld a,(bc)
	ld e,a
	and $f0
	swap a
	srl a
	pop hl
	push hl
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	bit 0,e
	jr z,_no_hi_swap	; if low bit is SET we use the LOW NYBBLE
	swap a
_no_hi_swap:
	and $f0
	ld d,a
	
	ld a,e
	and $0f
	srl a
	pop hl
	push hl
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	bit 4,e
	jr nz,_no_lo_swap	; if low bit is NOT SET we use the LOW NYBBLE
	swap a
_no_lo_swap:
	and $0f
	or d
	ld (bc),a

	inc c			; increment our storage/source counter
	ld a,c			; test for done
	cp <mus_waveform+$10
	jr nz,_gain_loop

	pop hl
	pop de

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_mix_waveform: mixes the current waveform with a new one!
;---------------------------------------------------------------------------
mus_mix_waveform:
	; set up destination
	ld a,(mus_song_track)
	; restrict to track 3
	cp <mus_track_3
	jr z,_restricted
	; abort
	inc de
	inc de
	jp music_advance_pattern_loop
_restricted:
	
;	ld c,a
	ld h,>mus_ram
	add a,o_reg_flags	; set register update flag
	ld l,a
	ld a,(hl)
	or mus_flag_set_waveform
	ld (hl),a
	; get wf
	call music_read_parameter_de
	
	swap a
	and $f0
	ld c,a
	ld a,b
	swap a
	and $0f
	ld b,a
	
	ld hl,mus_default_waveform
	add hl,bc

	; get the rate
	call music_read_parameter_de
	ld bc,mus_waveform
	push de
	ld (mus_temp),a
	ld e,0

_buffer_waveform_loop:
	ld a,(hl)		; get our new waveform byte
	bit 4,e			; check our nybble switch bit
	jr z,_other_nybble
	swap a			; switch nybbles

_other_nybble:
	and $f0			; process new waveform nybble
	srl a
	ld d,a

	ld a,(bc)		; get our old waveform byte
	ld b,a			; store it for future use
	and $f0			; process old waveform nybble
	srl a
	add a,d			; average with new waveform nybble
	and $f0
	ld d,a

	ld a,(mus_temp)		; do the timing thing
	add a,e			; add it to our counter
	ld e,a

	and $e0			; check the byte counter
	swap a			; process byte counter
	srl a
	
	add a,l			; add to new waveform pointer
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,e			; strip it to just be nybble pointer
	and $1f
	ld e,a

	ld a,b			; get the old byte
	and $0f			; strip low nybble
	ld b,a			; store it for safe keeping

	ld a,(hl)		; get our new waveform byte
	bit 4,e			; check our nybble switch bit
	jr nz,_other_nybble2
	swap a			; switch nybbles

_other_nybble2:
	and $0f			; process new waveform nybble
	add a,b
	srl a
	or d			; add it to other nybble
	ld b,>mus_waveform
	ld (bc),a		; store it

	ld a,(mus_temp)		; do the timing thing
	add a,e			; add it to our counter
	ld e,a

	and $e0			; check the byte counter
	swap a			; process byte counter
	srl a
	
	add a,l			; add to new waveform pointer
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,e			; strip it to just be nybble pointer
	and $1f
	ld e,a

	inc c			; increment our storage/source counter
	ld a,c			; test for done
	cp <mus_waveform+$10
	jr nz,_buffer_waveform_loop

	pop de

	; done
	jp music_advance_pattern_loop
	

;---------------------------------------------------------------------------
; mus_stop_sample: stops a sample that is playing
;---------------------------------------------------------------------------
mus_stop_sample:
	ld a,(mus_song_track)
	; restrict to track 3
	cp <mus_track_3
	jp nz,music_advance_pattern_loop

	ld a,(sample_priority)
	cp $01			; if it is not a music sample, then skip
	jp nc,music_advance_pattern_loop

	call stop_sample
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_vibrato_reset: resets the vibrato table index to put things back in tune
;---------------------------------------------------------------------------
mus_vibrato_reset:
	; set up destination
	ld a,(mus_song_track)
	add a,o_vibrato_table
	ld l,a
	ld h,>mus_ram
	ld a,(hl)
	and $80
	ld (hl),a

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_waveform: sets the waveform for track 3
;---------------------------------------------------------------------------
mus_set_waveform:
	; set up destination
	ld a,(mus_song_track)
	; restrict to track 3
	cp <mus_track_3
	jr z,_restricted
	; abort
	inc de
	jp music_advance_pattern_loop
_restricted:
	
;	ld c,a
	ld h,>mus_ram
	add a,o_reg_flags	; set register update flag
	ld l,a
	ld a,(hl)
	or mus_flag_set_waveform
	ld (hl),a
	; get flag
	call music_read_parameter_de
	
	swap a
	and $f0
	ld c,a
	ld a,b
	swap a
	and $0f
	ld b,a
	
	ld hl,mus_default_waveform
	add hl,bc
	ld bc,mus_waveform

_buffer_waveform_loop:
	ld a,(hli)
	ld (bc),a
	inc c
	ld a,c
	cp <mus_waveform+$10
	jr nz,_buffer_waveform_loop

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_fine_tune: mus_set_fine_tune sets the fine tune for tracks 1,2 and 3
;---------------------------------------------------------------------------
mus_set_fine_tune:
	; set up destination
	ld a,(mus_song_track)
	; restrict to tracks 1,2&3
	cp <mus_track_4
	jr c,_restricted
	; abort
	inc de
	jp music_advance_pattern_loop
_restricted:
	ld c,a
	ld h,>mus_ram
	add a,o_fine_tune
	ld l,a
	; get value
	call music_read_parameter_de
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_trigger_sample: triggers a sample to be played
;---------------------------------------------------------------------------
mus_trigger_sample:
	ld a,(mus_song_track)
	; restrict to track 3
	cp <mus_track_3
	jr z,_restricted
	; abort
	inc de
	jp music_advance_pattern_loop
_restricted:
	ld h,>mus_ram
	add a,o_hold_mode			; set the initial flag
	ld l,a
	ld a,(hl)
	or mus_flag_set_sample
	ld (hl),a

	call music_read_parameter_de
	push de
	ld e,a
	ld d,0
	ld l,1
	ld (mus_sample),a

	call music_play_sample
	pop de
	jp music_advance_pattern_loop
	
;---------------------------------------------------------------------------
; mus_set_arpeggio: ctrlbt_arpeggio sets the c64 style arpeggio pattern
;---------------------------------------------------------------------------
mus_set_arpeggio:
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_arpeggio
	ld l,a
	; get flag
	call music_read_parameter_de
	ld (hli),a
	call music_read_parameter_de
	ld (hli),a
	call music_read_parameter_de
	ld (hli),a
	ld (hl),a
	and a		; check for off, in which case we
			; should reset transpose
	jp nz,music_advance_pattern_loop
	ld a,o_source_transpose	; get source transpose
	add a,c
	ld l,a
	ld a,(hld)
	ld (hl),a		; store it in destination
	
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_glissando: ctrlbt_glissando sets the glissando flag for portamentos
;---------------------------------------------------------------------------
mus_set_glissando:
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_hold_mode
	ld l,a
	; get flag
	call music_read_parameter_de
	and a
	ld a,(hl)
	jr z,_clear_flag
	or mus_flag_glissando
	jr _set_flag
_clear_flag:
	and $ff-mus_flag_glissando
_set_flag:
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_porta_from: ctrlbt_porta_from sets the source note for portamentos
;---------------------------------------------------------------------------
mus_set_porta_from:
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	add a,o_play_note
	ld l,a
	ld h,>mus_ram
	; get depth
	call music_read_parameter_de
;	ld b,a			; !!!! read paramater puts it in b anyway
	; multiply it by 128 because the notetable is stored in 64s as words
	; quick way, *256/2
	xor a
	srl b
	rr a
	; set the note table index
	ld (hli),a
	ld c,a
	ld a,b
	add a,>mus_note_table
	ld (hli),a
	inc l			; set source note for retrigger
	inc l
	inc l
	ld (hld),a
	ld (hl),c

	; done
	jp music_advance_pattern_loop
	
;---------------------------------------------------------------------------
; mus_set_portamento: ctrlbt_portamento sets the portamento rate
;---------------------------------------------------------------------------
mus_set_portamento:
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_porta_speed
	ld l,a
	; get speed
	call music_read_parameter_de
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_vibrato: ctrlbt_vibrato sets the vibrato table and rate
;---------------------------------------------------------------------------
mus_set_vibrato: 
	; set up destination
	ld a,(mus_song_track)
	add a,o_vibrato_table
	ld l,a
	ld h,>mus_ram
	; get depth
	call music_read_parameter_de
;	ld b,a			; !!!! read paramater puts it in b anyway
	; multiply it by 128 because the notetable is stored in 64s as words
	; quick way, *256/2

	ld c,0
	srl b
	rr c

	; store it
	
	ld a,(hl)
	and $7f
	or c
	ld (hli),a
	ld a,b
	add a,>mus_vibrato_sinus_0
	ld (hl),a

	; now do rate

	ld a,(mus_song_track)
	ld c,a
	add a,o_vibrato_rate
	ld l,a

	call music_read_parameter_de
	ld (hl),a

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_restart_envelope: ctrlbt_restart_envelope restarts the envelope
; 				and turns the track back on
;---------------------------------------------------------------------------
mus_set_restart_envelope:
	; set up destination
	ld h,>mus_ram
	ld a,(mus_song_track)

	ld c,a
	add a,o_reg_flags	; set register update flag
	ld l,a
	ld a,(hl)
	and $ff-mus_flag_set_note_off
	or mus_flag_set_note+mus_flag_set_panning+mus_flag_set_hold_mode+mus_flag_set_pw_dur	; must set pw_dur after note to avoid bad bug
;	or mus_flag_set_panning+mus_flag_set_hold_mode+mus_flag_set_pw_dur	; must set pw_dur after note to avoid bad bug
	ld (hl),a

	ld a,o_hold_mode			; set the initial flag
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_initial
	ld (hl),a

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_note_off: ctrlbt_note_off turns the track off and advances
;---------------------------------------------------------------------------
mus_set_note_off: 
	ld h,>mus_ram
	ld a,(mus_song_track)
	; set up destination
	add a,o_reg_flags	; set register update flag
	ld l,a
	ld a,(hl)
	or mus_flag_set_note_off
	ld (hl),a
	; done & done track
	jp music_advance_pattern_done

;---------------------------------------------------------------------------
; mus_set_retrigger: ctrlbt_retrigger sets the track retrigger delay
;---------------------------------------------------------------------------
mus_set_retrigger:
	; set up destination
	ld a,(mus_song_track)
	add a,o_retrig_dly
	ld l,a
	ld h,>mus_ram
	; read parameter
	call music_read_parameter_de
	; store it
	ld (hl),a

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_trigger_delay: ctrlbt_trigger_delay sets the track trigger delay
;---------------------------------------------------------------------------
mus_set_trigger_delay:
	; set up destination
	ld a,(mus_song_track)
	add a,o_trigger_dly
	ld l,a
	ld h,>mus_ram
	; read parameter
	call music_read_parameter_de
	; store it
	ld (hl),a	; trigger delay
;	ld (hli),a	; trigger delay
;	ld (hl),a	; trigger cnt
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_hold_mode: ctrlbt_hold_mode sets the track hold mode
;---------------------------------------------------------------------------
mus_set_hold_mode: 

	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_hold_mode
	ld l,a
	; read parameter
	call music_read_parameter_de
	; store it
	and a
	ld a,(hl)
	jr z,_clear_flag
	or mus_flag_continuous
	jr _set_flag
_clear_flag:
	and $ff-mus_flag_continuous
_set_flag:
	ld (hl),a
	; set flag
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_hold_mode+mus_flag_set_envelope
	ld (hl),a
	; done
	jp music_advance_pattern_loop


;---------------------------------------------------------------------------
; mus_set_panning: ctrlbt_panning sets the track panning
;---------------------------------------------------------------------------
mus_set_panning:
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram

	; set flag
	add a,o_reg_flags
	ld l,a
	ld a,(hl)
	or mus_flag_set_panning
	ld (hl),a

	ld a,o_panning
	add a,c
	ld l,a

	ld a,c
	ld c,%0_0_0_1_0_0_0_1
	cp <mus_track_1
	jr z,_gotit
	sla c
	cp <mus_track_2
	jr z,_gotit
	sla c
	cp <mus_track_3
	jr z,_gotit
	sla c
_gotit:
	; read parameter
	call music_read_parameter_de
	and c
	; store it
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_sweep: ctrlbt_sweep sets the hardware sweep for track 1
;---------------------------------------------------------------------------
mus_set_sweep:
	; set up destination
	ld a,(mus_song_track)
	; restrict to tracks 1
	cp <mus_track_1
	jr z,_restricted
	; abort
	inc de
	jp music_advance_pattern_loop
_restricted:
	ld c,a
	ld h,>mus_ram

	add a,o_sweep
	ld l,a
	; read parameter
	call music_read_parameter_de
	; store it
	ld (hl),a
	; set flag
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_sweep
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_volume: ctrlbt_volume sets the track volume
;---------------------------------------------------------------------------
mus_set_volume: 
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_envelope
	ld l,a
	; read parameter
	call music_read_parameter_de
;	ld b,a			; !!!! read paramater puts it in b anyway
	; restrict to tracks 1,2&4
	ld a,c
	cp <mus_track_3
	jr nz,_restricted
	; track 3 volume works differently
	ld a,b
	jr _store_volume
_restricted:
	; store it
	ld a,(hl)
	and %0000_1_111
	or b
_store_volume:
	ld (hl),a
	; set flag
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_envelope
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_envelope: ctrlbt_envelope sets the track envelope
;---------------------------------------------------------------------------
mus_set_envelope:
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_envelope
	ld l,a
	; read parameter
	call music_read_parameter_de
;	ld b,a			; !!!! read paramater puts it in b anyway
	; restrict to tracks 1,2&4
	ld a,c
	cp <mus_track_3
	jr nz,_restricted
	; track 3 volume works differently
	ld a,b
	jr _store_volume
_restricted:
	; store it
	ld a,(hl)
	and %1111_0_000
	or b
_store_volume:
	ld (hl),a
	; set flag
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_envelope
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_duration: ctrlbt_duration sets the track duration
;---------------------------------------------------------------------------
mus_set_duration: 
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	add a,o_pw_dur
	ld l,a
	; read parameter
	call music_read_parameter_de
;	ld b,a			; !!!! read paramater puts it in b anyway
	; restrict to tracks 1,2&4
	ld a,c
	cp <mus_track_3
	jr nz,_restricted
	; track 3 duration works differently
	ld a,b
	jr _store_duration
_restricted:
	; store it
	ld a,(hl)
	and %11_000000
	or b
_store_duration:
	ld (hl),a
	; set flag
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_pw_dur
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_pulse_width: ctrlbt_pulse_width sets the pulse width for tracks 1 and 2
;---------------------------------------------------------------------------
mus_set_pulse_width:
	; set up destination
	ld a,(mus_song_track)
	; restrict to tracks 1&2
	cp <mus_track_3
	jr c,_restricted
	; abort
	inc de
	jp music_advance_pattern_loop
_restricted:
	ld c,a
	ld h,>mus_ram
	add a,o_pw_dur
	ld l,a
	; read parameter
	call music_read_parameter_de
;	ld b,a			; !!!! read paramater puts it in b anyway
	; store it
	ld a,(hl)
	and %00_111111
	or b
	ld (hl),a
	; set flag
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_pw_dur
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_shuffle: ctrlbt_shuffle sets the song shuffle
;---------------------------------------------------------------------------
mus_set_shuffle:
	; get the shuffle value
	call music_read_parameter_de
	swap a
	and $0f
	ld h,a

	ld a,b
	swap a
	and $f0

	ld (mus_song_shuffle_count),a	; shuffle counter
	ld a,h
	ld (mus_song_shuffle_count+1),a	; shuffle counter

	call music_read_parameter_de
	; store it
;	ld b,a			; !!!! read paramater puts it in b anyway
	and $0f
	sla a
	ld (mus_song_shuffle),a		; shuffle rate index
	ld a,b
	and $f0
	or h
	add a,<mus_shuffle_sinus_0
	ld (mus_song_shuffle_table),a	; shuffle table index
	ld a,>mus_shuffle_sinus_0
	adc a,0
	ld (mus_song_shuffle_table+1),a	; shuffle table index

	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_track_transpose: ctrlbt_transpose sets the track transpose
;---------------------------------------------------------------------------
mus_set_track_transpose:
	; set up destination
	ld a,(mus_song_track)
	ld c,a
	ld h,>mus_ram
	ld a,o_source_transpose
	add a,c
	ld l,a
	; read parameter
	call music_read_parameter_de
	; store it
	ld (hld),a
	ld (hl),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_set_song_speed: ctrlbt_song_speed sets the song speed
;---------------------------------------------------------------------------
mus_set_song_speed:
	; read parameter
	call music_read_parameter_de
;	sla a				; ^^^^ poopeop
	ld (mus_song_speed),a
	; done
	jp music_advance_pattern_loop

;---------------------------------------------------------------------------
; mus_track_loop: ctrlbt_track_loop loops the pattern
;---------------------------------------------------------------------------
mus_track_loop:

	; check for macro exit
	ld a,(mus_song_track)
	add a,o_macro_return
	ld l,a
	ld h,>mus_ram

	; test for macro exit

	ld a,(hli)
	and a
	jr nz,_macro_exit_l
	ld a,(hld)
	and a
	jr nz,_macro_exit_h

	; set loop flag
	ld a,(mus_song_flags)
	or mus_flag_pattern_end
	ld (mus_song_flags),a
	; done with this track
	jp music_advance_pattern_done

_macro_exit_l:
	dec l
_macro_exit_h:

	; reload pointer

	ld a,(hli)
	ld e,a
	ld d,(hl)

	; clear exit vector

	xor a
	ld (hld),a
	ld (hl),a

	; done
	jp music_advance_pattern_loop

;***************************************************************************
;***************************************************************************
;***************************************************************************
;---------------------------------------------------------------------------
; music_advance_sequence - this advances the pattern sequence pointer, deals
; 			with all pattern based effects.
;---------------------------------------------------------------------------
;***************************************************************************
;***************************************************************************
;***************************************************************************
music_advance_sequence:

	; clear all macros

	xor a
	ld (mus_track_1+o_macro_return),a
	ld (mus_track_1+o_macro_return+1),a
	ld (mus_track_2+o_macro_return),a
	ld (mus_track_2+o_macro_return+1),a
	ld (mus_track_3+o_macro_return),a
	ld (mus_track_3+o_macro_return+1),a
	ld (mus_track_4+o_macro_return),a
	ld (mus_track_4+o_macro_return+1),a

	; clear all wait_lines
	ld (mus_track_1+o_wait_lines),a
	ld (mus_track_2+o_wait_lines),a
	ld (mus_track_3+o_wait_lines),a
	ld (mus_track_4+o_wait_lines),a

	; get current pattern index

	ld a,(mus_song_pat_counter)
	ld l,a
	ld a,(mus_song_pat_counter+1)
	ld h,a

mus_map_main_loop:

	call music_read_parameter_hl

	;-------------------------------------------
	; check for song panning
	;-------------------------------------------

	cp ctrlbt_panning
	jr nz,mus_map_no_panning

	; get the panning value

	call music_read_parameter_hl
;	ld b,a 			; read parameter hl bufferes to b
	and %00010001
	ld (mus_track_1+o_panning),a
	ld a,b
	and %00100010
	ld (mus_track_2+o_panning),a
	ld a,b
	and %01000100
	ld (mus_track_3+o_panning),a
	ld a,b
	and %10001000
	ld (mus_track_4+o_panning),a

	ld a,(mus_track_1+o_reg_flags)
	or mus_flag_set_panning
	ld (mus_track_1+o_reg_flags),a
	ld a,(mus_track_2+o_reg_flags)
	or mus_flag_set_panning
	ld (mus_track_2+o_reg_flags),a
	ld a,(mus_track_3+o_reg_flags)
	or mus_flag_set_panning
	ld (mus_track_3+o_reg_flags),a
	ld a,(mus_track_4+o_reg_flags)
	or mus_flag_set_panning
	ld (mus_track_4+o_reg_flags),a

	; loop

	jr mus_map_main_loop

mus_map_no_panning:

	;-------------------------------------------
	; check for song shuffle
	;-------------------------------------------

	cp ctrlbt_shuffle
	jr nz,mus_map_no_shuffle

	; get the shuffle value

	call music_read_parameter_hl
	swap a
	and $0f
	ld d,a

	ld a,b
	swap a
	and $f0

	ld (mus_song_shuffle_count),a	; shuffle counter
	ld a,d
	ld (mus_song_shuffle_count+1),a	; shuffle counter

	call music_read_parameter_hl
	; store it
;	ld b,a			; !!!! read paramater puts it in b anyway
	and $0f
	sla a
	ld (mus_song_shuffle),a		; shuffle rate index
	ld a,b
	and $f0
	or d
	add a,<mus_shuffle_sinus_0
	ld (mus_song_shuffle_table),a	; shuffle table index
	ld a,>mus_shuffle_sinus_0
	adc a,0
	ld (mus_song_shuffle_table+1),a	; shuffle table index

	; loop

	jp mus_map_main_loop

mus_map_no_shuffle:

	;-------------------------------------------
	; check for song transpose
	;-------------------------------------------

	cp ctrlbt_transpose
	jr nz,mus_map_no_transpose

	; get the transpose value

	call music_read_parameter_hl
	ld (mus_song_transpose),a

	; loop

	jp mus_map_main_loop

mus_map_no_transpose:

	;-------------------------------------------
	; check for song speed
	;-------------------------------------------

	cp ctrlbt_song_speed
	jr nz,mus_map_no_song_speed

	; get the song speed

	call music_read_parameter_hl
;	sla a				; ^^^^ poopeop
	ld (mus_song_speed),a

	; loop

	jp mus_map_main_loop

mus_map_no_song_speed:

	;-------------------------------------------
	; check for track loop
	;-------------------------------------------

	cp ctrlbt_track_loop
	jr nz,mus_map_no_track_loop

mus_reset_sequence:

	; get the song address, the header is always at the top

;	ld hl,mus_song_address_table 	;##### required for standalone
;	ld a,(mus_song_number)		;##### required for standalone
;	sla a				;##### required for standalone
;	add a,l				;##### required for standalone
;	ld l,a				;##### required for standalone
;	ld a,h				;##### required for standalone
;	adc a,0				;##### required for standalone
;	ld h,a				;##### required for standalone
;
;	ld a,(hli)			;##### required for standalone
;	ld e,a				;##### required for standalone
;	ld a,(hl)			;##### required for standalone
;	ld d,a				;##### required for standalone
;
;	ld l,e				;##### required for standalone
;	ld h,d				;##### required for standalone

	ld hl,$d000			;@@@@@ required for editor mode

	; get address of sequence

	call music_read_parameter_hl
;	add a,e					;##### required for standalone
	ld (mus_song_pat_counter),a
	call music_read_parameter_hl
;	adc a,d					;##### required for standalone
	add a,h					;@@@@@ required for editor mode
	ld (mus_song_pat_counter+1),a

	; get address of pattern indexes

	call music_read_parameter_hl
;	add a,e					;##### required for standalone
	ld (mus_song_pat_address),a
	call music_read_parameter_hl
;	adc a,d					;##### required for standalone
	add a,h					;@@@@@ required for editor mode
	ld (mus_song_pat_address+1),a

	ld a,(editor_looping_mode); @@@@ editor required
	and a			; @@@@ editor required
	jr nz,_looping		; @@@@ editor required
	ld a,$ff		; @@@@ editor required
	ld (editor_seq),a	; @@@@ editor required
_looping:			; @@@@ editor required

	; loop, but we need to reload hl so do the whole thing again

	jp music_advance_sequence

mus_map_no_track_loop:

	;-------------------------------------------
	; set the value
	;-------------------------------------------

	; store a for safe keeping

	ld e,a

	; set current pattern index

	ld a,l
	ld (mus_song_pat_counter),a
	ld a,h
	ld (mus_song_pat_counter+1),a

	; calculate offset

	ld l,e
	ld h,0

	sla l	; *2
	rl h
	sla l	; *4
	rl h
	sla l	; *8
	rl h

	; get the address of the patterns

	ld a,(mus_song_pat_address)
	ld e,a
	ld a,(mus_song_pat_address+1)
	ld d,a

	; calculate the address of the patterns

	add hl,de

	; get track 1

	call music_read_parameter_hl
	add a,e
	ld (mus_track_1+o_seq_counter),a
	ld (editor_sequence_1),a	; @@@@ editor required
	ld a,d				; @@@@ editor required
	adc a,0				; @@@@ editor required
	ld d,a				; @@@@ editor required
	call music_read_parameter_hl
;	adc a,d				; #### required for standalone
	add a,d				; @@@@ editor required
	ld (mus_track_1+o_seq_counter+1),a
	ld (editor_sequence_1+1),a	; @@@@ editor required

	ld a,(mus_song_pat_address+1)	; @@@@ editor required
	ld d,a				; @@@@ editor required
		
	; get track 2

	call music_read_parameter_hl
	add a,e
	ld (mus_track_2+o_seq_counter),a
	ld (editor_sequence_2),a	; @@@@ editor required
	ld a,d				; @@@@ editor required
	adc a,0				; @@@@ editor required
	ld d,a				; @@@@ editor required
	call music_read_parameter_hl
;	adc a,d				; #### required for standalone
	add a,d				; @@@@ editor required
	ld (mus_track_2+o_seq_counter+1),a
	ld (editor_sequence_2+1),a	; @@@@ editor required

	ld a,(mus_song_pat_address+1)	; @@@@ editor required
	ld d,a				; @@@@ editor required
		
	; get track 3

	call music_read_parameter_hl
	add a,e
	ld (mus_track_3+o_seq_counter),a
	ld (editor_sequence_3),a	; @@@@ editor required
	ld a,d				; @@@@ editor required
	adc a,0				; @@@@ editor required
	ld d,a				; @@@@ editor required
	call music_read_parameter_hl
;	adc a,d				; #### required for standalone
	add a,d				; @@@@ editor required
	ld (mus_track_3+o_seq_counter+1),a
	ld (editor_sequence_3+1),a	; @@@@ editor required

	ld a,(mus_song_pat_address+1)	; @@@@ editor required
	ld d,a				; @@@@ editor required
		
	; get track 4

	call music_read_parameter_hl
	add a,e
	ld (mus_track_4+o_seq_counter),a
	ld (editor_sequence_4),a	; @@@@ editor required
	ld a,d				; @@@@ editor required
	adc a,0				; @@@@ editor required
	ld d,a				; @@@@ editor required
	call music_read_parameter_hl
;	adc a,d				; #### required for standalone
	add a,d				; @@@@ editor required
	ld (mus_track_4+o_seq_counter+1),a
	ld (editor_sequence_4+1),a	; @@@@ editor required

	xor a
	ld (mus_track_1+o_trigger_flag),a
	ld (mus_track_2+o_trigger_flag),a
	ld (mus_track_3+o_trigger_flag),a
	ld (mus_track_4+o_trigger_flag),a
	
	; we are done

	ld (editor_line),a	; @@@@ editor required
	inc a			; @@@@ editor required	
	ld (editor_redraw),a	; @@@@ editor required
	ld a,(editor_looping_mode); @@@@ editor required
	and a			; @@@@ editor required
	jr nz,_looping_2	; @@@@ editor required
	ld a,(editor_seq)	; @@@@ editor required
	inc a			; @@@@ editor required
	ld (editor_seq),a	; @@@@ editor required
_looping_2:			; @@@@ editor required

	ret

;***************************************************************************
;***************************************************************************
;***************************************************************************

mus_upate_track_effects:

	; store track offset
	ld (mus_song_track),a

	; set up for reading track data
	ld c,a
	ld h,>mus_ram

	; get trigger_flag

	ld a,o_trigger_flag
	add a,c
	ld l,a
	ld a,(hli)			; get trigger flag
	and a
	jr z,_move_on_to_retrigger ; if it is not set, move on to retrigger

	; check the trigger delay
	inc l
	ld a,(hl)		; get trigger counter
	sub 1			; use sub instead of dec because we care about carry
	ld (hli),a
	jr nc,_done_trigger_code

	ld a,(hld)		; get retrigger delay
	ld (hld),a		; store it in counter
	dec l
	dec (hl)		; decrement trigger flag

	call music_advance_pattern	; advance this channel
	ld a,(mus_song_flags)		; return if pattern done
	bit mus_bit_pattern_end,a
	ret nz

	ld a,(mus_song_track)

	; set up for reading track data
	ld c,a
	ld h,>mus_ram

	jr _done_trigger_code		; update effects
	
; retrigger nonsense

_move_on_to_retrigger:

	inc l
	inc l
	ld a,(hld)		; check for retrigger
	and a
	jr z,_done_trigger_code

	dec (hl)		; decrement retrigger counter
	jr nz,_done_trigger_code
	ld (hl),a		; reset retrigger counter

;	restart_envelope_here
;	restart_sample_here
;	reset_source_note_here

	ld a,o_hold_mode			; set the initial flag
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_set_sample,a
	jr z,_ret_not_sample
	push de
	ld a,(mus_sample)
	ld e,a
	ld d,0
	ld l,1
	call music_play_sample
	pop de
	; set up for reading track data
	ld h,>mus_ram
	ld a,(mus_song_track)
	ld c,a
	jr _ret_do_note

_ret_not_sample:
	or mus_flag_set_initial
	ld (hl),a

	ld a,o_reg_flags	; set register update flag
	add a,c
	ld l,a
	ld a,(hl)
	and $ff-mus_flag_set_note_off
	or mus_flag_set_note+mus_flag_set_panning+mus_flag_set_hold_mode+mus_flag_set_pw_dur	; must set pw_dur after note to avoid bad bug
	ld (hl),a

_ret_do_note:

	; copy note for portamentos

	ld a,o_source_note
	add a,c
	ld l,a
	ld a,(hld)
	dec l
	dec l
	dec l
	ld (hli),a
	inc l
	inc l
	inc l
	inc l
	ld a,(hld)
	dec l
	dec l
	dec l
	ld (hl),a
	
_done_trigger_code:

	;-------------------------------------------
	;-------------------------------------------
	;-------------------------------------------
	;-------------------------------------------
	; do effect updates here
	;-------------------------------------------

	; **** envelope modulation
	; **** instruments/macrosounds
	; **** master volume?!
	; **** waveform related
	; **** sample related

	;---------------------------------------------------------------------------
	; update track arpeggio
	;---------------------------------------------------------------------------

;	ld h,>mus_ram

	ld a,o_arpeggio_dly ; get register update flag
	add a,c
	ld l,a
	ld a,(hld)
	and a
	jr z,_no_arpeggio

	dec (hl)
	jr nz,_no_arp_update
	ld (hld),a
;	push de

	ld a,(hld)
	ld e,a
	ld d,(hl)
	sla d
	rla
	sla d
	rla
	sla d
	rla
	sla d
	rla
	ld b,a
	ld a,e
	swap a
	and $0f
	or d
	ld (hli),a
	ld (hl),b
	inc l
;	pop de

_no_arp_update:
	dec l
	dec l
	ld a,(hl)
	swap a
	and $0f
	ld b,a

	ld a,o_source_transpose	; get register update flag
	add a,c
	ld l,a
	ld a,(hld)
	add a,b
	ld (hl),a
_no_arpeggio:

	;---------------------------------------------------------------------------
	; update track portamento
	;---------------------------------------------------------------------------

;;;	ld h,>mus_ram

	ld a,o_porta_speed	; get register update flag
	add a,c
	ld l,a
	ld a,(hl)

	and a
	jr z,_done_portamento	; if it is 0, then no portamento
	ld e,a
	ld d,0

	sla e				; *2 as table is words
	rl d

	push de			; de is the speed

	ld a,o_play_note	; get register update flag
	add a,c
	ld l,a
	ld a,(hli)
	ld e,a
	ld a,(hli)
	ld d,a

	push de			; de is current note

	ld a,(hli)
	ld e,a
	ld d,(hl)		; de is dest note

	pop hl			; hl is current note
	pop bc			; bc is the speed

	; check for highbyte direction

	ld a,h
	cp d
	jr c,_mus_portamento_lower  	; high byte is lower
	jr nc,_mus_portamento_greater	; high byte is higher
					; high byte is equal

	; check for lowbyte direction

	ld a,l
	cp e
	jr c,_mus_portamento_lower	; low byte is lower
	jr nc,_mus_portamento_greater	; low byte is higher
					; low byte is equal
	; if it's equal, we are done
	
	jr _mus_clear_portamento

_mus_portamento_greater:

	; subtract the speed

	ld a,l
	sub c
	ld l,a
	ld a,h
	sbc a,b
	ld h,a

	; check for overflow

	cp d				; h-d
	jr c,_mus_clear_portamento	; if hl < de then we have an overflow
	jr nz,_mus_store_portamento 	; if hl <> de then we don't have an overflow

	ld a,l
	cp e				; l-e
	jr c,_mus_clear_portamento	; if hl < de then we have an overflow
	jr nz,_mus_store_portamento 	; if hl <> de then we don't have an overflow

	jr _mus_clear_portamento	; if hl == de then we are done, same as overflow

_mus_portamento_lower:

	; add the speed

	ld a,l
	add a,c
	ld l,a
	ld a,h
	adc a,b
	ld h,a

	; check for overflow

	ld a,d
	cp h				; d-h
	jr c,_mus_clear_portamento	; if de < hl then we have an overflow
	jr nz,_mus_store_portamento 	; if de <> hl then we don't have an overflow

	ld a,e
	cp l				; e-l
	jr c,_mus_clear_portamento	; if de < hl then we have an overflow
	jr nz,_mus_store_portamento 	; if de <> hl then we don't have an overflow

;	jr _mus_clear_portamento	; if de == hl then we are done, same as overflow

_mus_clear_portamento:

	; handle overflow

	ld h,>mus_ram
	jr _mus_store_portamento_2

_mus_store_portamento:

	ld e,l
	ld d,h

_mus_store_portamento_2:

	; get track offset
	ld a,(mus_song_track)
	; set up for reading track data
	ld c,a

	ld h,>mus_ram

	ld a,o_play_note	; get register update flag
	add a,c
	ld l,a

	ld a,e
	ld (hli),a
	ld (hl),d

	; set note set flag

	ld a,o_reg_flags	; set register update flag
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_note+mus_flag_set_pw_dur	; must set pw_dur after note to avoid bad bug
	ld (hl),a

_done_portamento:

	;-------------------------------------------
	;-------------------------------------------
	;-------------------------------------------
	;-------------------------------------------
	; send values to sound chip
	;-------------------------------------------

	;-------------------------------------------
	; do waveform
	;-------------------------------------------

	; **** check for track 3 and EXTERNAL sample playing, in which case we are done

	ld a,c
	cp <mus_track_3
	jr nz,_done_waveform

	add a,o_reg_flags	; get register update flag
	ld l,a
	ld a,(hl)
	bit mus_bit_set_waveform,a
	jr z,_done_waveform

	ld a,(sample_priority)
	and a
	jr nz,_done_waveform

	ld hl,mus_waveform	; set the default waveform

;	ld a,($ff1a)	; get sound 3 on value
;	and %10000000
;	ld b,a

	ld b,%10000000

	xor a		; shut off sound 3
	ld ($ff1a),a

	ld a,(hli)		; 2 cycles
	ld ($ff30),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff31),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff32),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff33),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff34),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff35),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff36),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff37),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff38),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff39),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3a),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3b),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3c),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3d),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3e),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3f),a		; 3 cycles

	ld a,b			; turn on sound 3
	ld ($ff1a),a

	ld h,>mus_ram

_done_waveform:

	;-------------------------------------------
	; do panning before note
	;-------------------------------------------

;;;	ld h,>mus_ram

	; check for track on
	
	ld a,o_reg_flags
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_set_note_off,a
	jr z,_not_note_off
	xor a
	jp _do_panning

_not_note_off:

	ld a,o_auto_panner_dly
	add a,c
	ld l,a
	ld a,(hld)
	and a
	jr z,_not_auto_panner
	ld b,a			; store delay

	ld a,(hl)		; get value
	inc l
	inc l
	dec (hl)		; decrement counter
	jr nz,_not_ap_next
	ld (hl),b
	rlca			; rotate value
	rlca
	dec l
	dec l
	ld (hl),a		; store it
_not_ap_next:
	and %11000000
	ld b,a
	and %10000000
	swap a
	srl a
	or b
	and %01000100
	rlca
	rlca
	ld b,a
	ld a,c
	cp <mus_track_1
	jr nz,_autopanner_not_track_1

	ld a,($ff25)
	and %11101110
	or b
	ld ($ff25),a

	jr _done_panning

_autopanner_not_track_1:

	sla b
	cp <mus_track_2
	jr nz,_autopanner_not_track_2

	ld a,($ff25)
	and %11011101
	or b
	ld ($ff25),a

	jr _done_panning

_autopanner_not_track_2:

	sla b
	cp <mus_track_3
	jr nz,_autopanner_not_track_3

	ld a,(sample_priority)
	cp $01			; if it is not a music sample, then skip
	jr nc,_done_panning

	ld a,($ff25)
	and %10111011
	or b
	ld ($ff25),a

	jr _done_panning

_autopanner_not_track_3:

	sla b
	ld a,($ff25)
	and %01110111
	or b
	ld ($ff25),a

	jr _done_panning

_not_auto_panner:

	ld a,o_reg_flags	; get register update flag
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_set_panning,a
	jr z,_done_panning

	ld a,o_panning			; panning
	add a,c
	ld l,a
	ld a,(hl)
_do_panning:
	ld b,a

	ld a,c
	cp <mus_track_1
	jr nz,_panning_not_track_1

	ld a,($ff25)
	and %11101110
	or b
	ld ($ff25),a

	jr _done_panning

_panning_not_track_1:

	cp <mus_track_2
	jr nz,_panning_not_track_2

	ld a,($ff25)
	and %11011101
	or b
	ld ($ff25),a

	jr _done_panning

_panning_not_track_2:

	cp <mus_track_3
	jr nz,_panning_not_track_3

	ld a,(sample_priority)
	cp $01			; if it is not a music sample, then skip
	jr nc,_done_panning

	ld a,($ff25)
	and %10111011
	or b
	ld ($ff25),a

	jr _done_panning

_panning_not_track_3:

	ld a,($ff25)
	and %01110111
	or b
	ld ($ff25),a

_done_panning:

	;-------------------------------------------
	; do envelope before note
	;-------------------------------------------

;;;	ld h,>mus_ram

	ld a,o_reg_flags	; get register update flag
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_set_envelope,a
	jr z,_done_envelope

	ld a,o_envelope			; envelope
	add a,c
	ld l,a
	ld b,(hl)

; **** !!!! &&&& remove this neext bit if you want normal hold mode, ie. just no duration

;	ld a,o_hold_mode			; glissando
;	add a,c
;	ld l,a
;	ld a,(hl)
;	bit mus_bit_continuous,a
;
;	jr nz,_env_hold_mode_off
;
;	ld a,b
;	and %1111_0_000
;	or %0000_1_000
;	ld b,a
;
;_env_hold_mode_off:

	ld a,c
	cp <mus_track_1
	jr nz,_envelope_not_track_1

	ld a,b
	ld ($ff12),a

	jr _done_envelope

_envelope_not_track_1:

	cp <mus_track_2
	jr nz,_envelope_not_track_2

	ld a,b
	ld ($ff17),a

	jr _done_envelope

_envelope_not_track_2:

	cp <mus_track_3
	jr nz,_envelope_not_track_3

	; track 3

	ld a,(sample_priority)
	cp $01			; if it is not a music sample, then skip
	jr nc,_done_envelope

	ld a,b
	ld ($ff1c),a

	jr _done_envelope

_envelope_not_track_3:

	ld a,b
	ld ($ff21),a

_done_envelope:

	;-------------------------------------------
	; do sweep
	;-------------------------------------------

	ld a,c
	cp <mus_track_1
	jr nz,_done_sweep

	add a,o_reg_flags	; get register update flag
	ld l,a
	ld a,(hl)
	bit mus_bit_set_sweep,a
	jr z,_done_sweep

	ld a,o_sweep		; get sweep
	add a,c
	ld l,a
	ld a,(hl)

	ld ($ff10),a	

_done_sweep:

	;-------------------------------------------
	; calculate note offset
	;-------------------------------------------

	; check set_note bit & vibrato bit & arpeggio bit

	ld a,o_reg_flags	; get register update flag
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_set_note,a
	jr nz,_update_note

	ld a,o_arpeggio	; get register update flag
	add a,c
	ld l,a
	ld a,(hl)
	and a
	jr nz,_update_note

	ld a,o_vibrato_rate			; vibrato
	add a,c
	ld l,a
	ld a,(hl)
	and a					; check the speed
	jp z,_no_note_update			; if 0 no vibrato

	ld a,o_reg_flags	; set register update flag
	add a,c
	ld l,a
	ld a,(hl)
	or mus_flag_set_note+mus_flag_set_pw_dur	; must set pw_dur after note to avoid bad bug
	ld (hl),a

	;-------------------------------------------
	; song transpose
	;-------------------------------------------

_update_note:

	ld d,$00				; song transpose
	ld a,(mus_song_transpose)
	ld e,a
	bit 7,a
	jr z,_not_negative_song_trans
	ld d,$ff

_not_negative_song_trans:

	;-------------------------------------------
	; track transpose
	;-------------------------------------------

	ld a,o_track_transpose			; track transpose
	add a,c
	ld l,a
	ld a,(hl)
	bit 7,a
	jr z,_not_negative_track_trans

;	and $7f					; do negative
	cpl
	inc a
	ld b,a
	ld a,e
	sub b
	ld e,a
	ld a,d
	sbc a,$00
	ld d,a

	jr _done_track_trans

_not_negative_track_trans:

	add a,e					; do positive
	ld e,a
	ld a,d
	adc a,0
	ld d,a

_done_track_trans:

	;-------------------------------------------
	; adjust to be a proper offset
	;-------------------------------------------

	ld a,d
	ld d,e
	ld e,0	; *$100
	srl a
	rr d
	rr e	; /2 = *$80

	;-------------------------------------------
	; get note
	;-------------------------------------------

	ld a,o_play_note			; get note
	add a,c
	ld l,a
	ld a,(hli)
	add a,e
	ld e,a
	ld a,(hl)
	adc a,d
	ld d,a

	;-------------------------------------------
	; glissando
	;-------------------------------------------

	ld a,o_hold_mode			; glissando
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_glissando,a
	jr z,_no_glissando

	ld a,e
	add a,$40	; round instead of truncate
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	ld a,e
	and $80
	ld e,a
_no_glissando:

	;-------------------------------------------
	; vibrato
	;-------------------------------------------

	ld a,o_vibrato_rate			; vibrato
	add a,c
	ld l,a
;	ld a,(hl)
;	and a					; check the speed
;	jr z,_done_vibrato			; if 0 no vibrato
;
;	ld b,a

	ld b,(hl)

	push de					; store current offset

	ld a,o_vibrato_table			; update the position
	add a,c
	ld l,a
	ld a,(hl)				; get old low position
	ld e,a
	add a,b					; add in the offset
	and $7f					; restrain it to one vibrato table
	ld b,a
	ld a,e					; add in highbit
	and $80
	or b					; add in lowbits
	ld (hli),a				; store it

	and $fe					; chop low bit as table is stored in words

	ld h,(hl)				; get high byte
	ld l,a					; hl is now address of vibrato value

	pop de					; get current offset

	ld a,(hli)
	add a,e
	ld e,a
	ld a,(hl)
	adc a,d
	ld d,a

	ld h,>mus_ram

_done_vibrato:

	;-------------------------------------------
	; check for track 4
	;-------------------------------------------
	; glissando is forced for tone mode 
	; fine tune has no function on track 4
	; as it has 1/$80 the resolution of other tracks

	ld a,c
	cp <mus_track_4
	jr nz,_not_track_4

	ld a,d
	sub >mus_note_table		; get rid of pointer to note table

	; /$80 quick way -> *$2/$100

	sla e	; *$2
	rl a

	; check for overflow

	bit 7,a
	jr z,_noise_not_negative

	xor a
	jr _done_noise_overflow

_noise_not_negative:

	bit 6,a
	jr z,_done_noise_overflow

	ld a,$3f

_done_noise_overflow:

	; get address to table

	ld de,mus_noisefreq_table
	or e
	ld e,a
	
	ld l,<mus_track_4+o_hold_mode		; tone mode
	ld b,(hl)

	ld a,(de)				; get frequency

	bit mus_bit_tone_mode,b			; check for tone mode
	jr z,_not_noise_tone_mode	

	or %0000_1_000				; or tone mode flag

_not_noise_tone_mode:

	; now we can store it!
	ld ($ff22),a

	ld a,b					; get hold mode flag
	and mus_flag_continuous+mus_flag_set_initial
	ld ($ff23),a

	and mus_flag_set_initial		; @@@@ editor_required
	jr z,_ed_dont_set_init_4		; @@@@ editor_required
	ld (editor_initial_4),a			; @@@@ editor_required
_ed_dont_set_init_4:				; @@@@ editor_required

	jp _done_note_update_2			; @@@@ editor_required
;	jr _done_note_update_2

_not_track_4:

	;-------------------------------------------
	; tracks 1,2 and 3
	;-------------------------------------------
	; fine tune
	;-------------------------------------------

	ld a,o_fine_tune			; fine tune
	add a,c
	ld l,a
	ld a,(hl)
	bit 7,a
	jr z,_not_negative_fine_tune

	sla a					; do negative
	ld b,a
	ld a,e
	sub b
	ld e,a
	ld a,d
	sbc a,$00
	ld d,a

	jr _done_fine_tune

_not_negative_fine_tune:

	sla a
	add a,e					; do positive
	ld e,a
	ld a,d
	adc a,0
	ld d,a

_done_fine_tune:

	;-------------------------------------------
	; check for overflow
	;-------------------------------------------

	ld a,d
	cp >mus_note_table_end-2
	jr c,_track_note_not_over

	ld a,e
	cp <mus_note_table_end-2
	jr c,_track_note_not_over

	; handle overflow

	ld a,<mus_note_table_end-2
	ld e,a
	ld a,>mus_note_table_end-2
	ld d,a
	jr _track_note_not_under

_track_note_not_over:

	; check for underflow

	ld a,d
	cp >mus_note_table
	jr nc,_track_note_not_under

	; handle underflow

	ld a,<mus_note_table
	ld e,a
	ld a,>mus_note_table
	ld d,a

_track_note_not_under:

	; de is now the offset of the note to play in the notetable!!!

	ld a,o_hold_mode			; hold mode
	add a,c
	ld l,a
	ld a,(hl)
	and mus_flag_continuous+mus_flag_set_initial
	ld h,a

	call music_read_notetable_de
	ld l,a					; l is the freq low byte
	call music_read_notetable_de
	or h					; set hold mode + inital
	ld h,a					; d is the freq high byte

	ld a,c
	cp <mus_track_1
	jr nz,_note_not_track_1

	ld a,l
	ld ($ff13),a
	ld a,h
	ld ($ff14),a

	and mus_flag_set_initial		; @@@@ editor_required
	jr z,_ed_dont_set_init_1		; @@@@ editor_required
	ld (editor_initial_1),a			; @@@@ editor_required
_ed_dont_set_init_1:				; @@@@ editor_required

	jr _done_note_update

_note_not_track_1:

	cp <mus_track_2
	jr nz,_note_not_track_2

	ld a,l
	ld ($ff18),a
	ld a,h
	ld ($ff19),a

	and mus_flag_set_initial		; @@@@ editor_required
	jr z,_ed_dont_set_init_2		; @@@@ editor_required
	ld (editor_initial_2),a			; @@@@ editor_required
_ed_dont_set_init_2:				; @@@@ editor_required

	jr _done_note_update

_note_not_track_2:

	; track 3

	ld a,(sample_priority)
	and a
	jr nz,_done_note_update

;	ld a,$80
;	ld ($ff1a),a
	ld a,l
	ld ($ff1d),a
	ld a,h
	ld ($ff1e),a

	and mus_flag_set_initial		; @@@@ editor_required
;	ld h,a					; @@@@ editor_required - unused
;	ld a,(editor_initial_3)			; @@@@ editor_required - unused
;	or h					; @@@@ editor_required - unused
;	ld (editor_initial_3),a			; @@@@ editor_required - unused

_done_note_update:

	ld h,>mus_ram

_done_note_update_2:

	; check for retrigger
	; if we are in retrigger mode, then don't clear flags

	ld a,o_retrig_dly
	add a,c
	ld l,a
	ld a,(hl)
	and a
	jr nz,_retrigger_is_on

	ld a,o_hold_mode			; clear the initial flag
	add a,c
	ld l,a
	ld a,(hl)
	and $ff-mus_flag_set_initial
	ld (hl),a

_retrigger_is_on:
_no_note_update:

	;-------------------------------------------
	; do pulse width/dur after note
	;-------------------------------------------

	; do auto pwm 

	ld a,c
	cp <mus_track_3
	jr nc,_not_auto_pwm

	add a,o_auto_pwm_dly
	ld l,a
	ld a,(hld)
	and a
	jr z,_not_auto_pwm
	ld b,a			; store delay

	ld a,(hl)		; get value
	inc l
	inc l
	dec (hl)		; decrement counter
	jr nz,_not_apwm_next
	ld (hl),b
	rlca			; rotate value
	rlca
	dec l
	dec l
	ld (hl),a		; store it
_not_apwm_next:
	and %11000000
	ld b,a
	ld a,o_pw_dur
	add a,c
	ld l,a
	ld a,(hl)
	and %00111111
	or b
	jr _store_pw_dir

_not_auto_pwm:

	ld a,o_reg_flags	; get register update flag
	add a,c
	ld l,a
	ld a,(hl)
	bit mus_bit_set_pw_dur,a
	jr z,_done_pw_dur

	ld a,o_pw_dur			; pulse width/duration
	add a,c
	ld l,a
	ld a,(hl)
_store_pw_dir:
	ld b,a

	ld a,c
	cp <mus_track_1
	jr nz,_pw_dur_not_track_1

	ld a,b
	ld ($ff11),a

	jr _done_pw_dur

_pw_dur_not_track_1:

	cp <mus_track_2
	jr nz,_pw_dur_not_track_2

	ld a,b
	ld ($ff16),a

	jr _done_pw_dur

_pw_dur_not_track_2:

	cp <mus_track_3
	jr nz,_pw_dur_not_track_3

	; track 3

	ld a,(sample_priority)
	and a
	jr nz,_pw_dur_not_track_3

	ld a,b
	ld ($ff1b),a

	jr _done_pw_dur

_pw_dur_not_track_3:

	ld a,b
	ld ($ff20),a

_done_pw_dur:

	; clear register update flags

	ld a,o_reg_flags	; get register update flag
	add a,c
	ld l,a
	xor a
	ld (hl),a

	ret

;***************************************************************************
;***************************************************************************
;***************************************************************************

	end



