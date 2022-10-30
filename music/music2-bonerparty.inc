;***************************************************************************
;---------------------------------------------------------------------------

bonerparty_header:
	dw _seq-bonerparty_header	; sequence offset
	dw _pat-bonerparty_header	; patterns offset
	dw _end-bonerparty_header	; end offset
	db 19 ; # of patterns

;---------------------------------------------------------------------------

	db 'BONERPARTY HOLIDAY!'			; song name
	db $ff

;---------------------------------------------------------------------------

_seq:

	db ctrlbt_song_speed,$40
;	db ctrlbt_shuffle,$03,$33

	db 0	; verse intro
	db 1	; verse 1
	db 2	; verse 2
	db 3	; verse 3
	db 4	; verse 4
	db 5	; verse end

;	db ctrlbt_shuffle,$33,$53

	db 6	; chorus 1
	db 7	; chorus 2
	db 8	; chorus 3
	db 9	; chorus 4

;	db ctrlbt_shuffle,$03,$33

	db 0	; verse intro
	db 5	; verse end
	db 1	; verse 1
	db 4	; verse 4

;	db ctrlbt_shuffle,$33,$53

	db 6	; chorus 1
	db 9	; chorus 4
	db 10	; chorus end

;	db ctrlbt_shuffle,$01,$21

	db 11	; bridge 1
	db 12	; bridge 2
	db 13	; bridge 3
	db 14	; bridge end

;	db ctrlbt_shuffle,$0b,$2b

	db 15	; 34 part 1
	db 16	; 34 part 2
	db 17	; 34 part 3
	db 18	; 34 part end

	db ctrlbt_track_loop

;---------------------------------------------------------------------------

_pat:
	dw _guitar_verse_intro-_pat,_silent_16		   -_pat,_silent_16	  -_pat,_silent_16	  -_pat
	dw _guitar_verse      -_pat,_keyboard_verse_1	   -_pat,_bass_verse	  -_pat,_drums_verse	  -_pat
	dw _guitar_verse      -_pat,_keyboard_verse_2	   -_pat,_bass_verse	  -_pat,_drums_verse	  -_pat
	dw _guitar_verse      -_pat,_keyboard_verse_3	   -_pat,_bass_verse	  -_pat,_drums_verse	  -_pat
	dw _guitar_verse      -_pat,_keyboard_verse_4	   -_pat,_bass_verse	  -_pat,_drums_verse	  -_pat
	dw _guitar_verse_end  -_pat,_keyboard_verse_end	   -_pat,_bass_verse_end  -_pat,_drums_verse_end  -_pat
	dw _guitar_chorus     -_pat,_keyboard_chorus	   -_pat,_bass_chorus	  -_pat,_drums_chorus	  -_pat
	dw _guitar_chorus     -_pat,_keyboard_chorus	   -_pat,_bass_chorus	  -_pat,_drums_chorus	  -_pat
	dw _guitar_chorus     -_pat,_keyboard_chorus	   -_pat,_bass_chorus	  -_pat,_drums_chorus	  -_pat
	dw _guitar_chorus     -_pat,_keyboard_chorus	   -_pat,_bass_chorus	  -_pat,_drums_chorus	  -_pat
	dw _guitar_chorus_end -_pat,_silent_16		   -_pat,_silent_16	  -_pat,_drums_chorus_end -_pat
	dw _guitar_bridge_1   -_pat,_keyboard_bridge_1	   -_pat,_bass_bridge_1	  -_pat,_drums_bridge_1	  -_pat
	dw _guitar_bridge_2   -_pat,_keyboard_bridge_2	   -_pat,_bass_bridge_2	  -_pat,_drums_bridge_2	  -_pat
	dw _guitar_bridge_3   -_pat,_keyboard_bridge_3	   -_pat,_bass_bridge_3	  -_pat,_drums_bridge_3	  -_pat
	dw _guitar_bridge_end -_pat,_keyboard_bridge_end   -_pat,_bass_bridge_end -_pat,_drums_bridge_end -_pat
	dw _guitar_34_part    -_pat,_keyboard_34_part_1	   -_pat,_bass_34_part	  -_pat,_drums_34_part	  -_pat
	dw _guitar_34_part    -_pat,_keyboard_34_part_2	   -_pat,_bass_34_part	  -_pat,_drums_34_part	  -_pat
	dw _guitar_34_part    -_pat,_keyboard_34_part_2	   -_pat,_bass_34_part	  -_pat,_drums_34_part	  -_pat
	dw _guitar_34_part_end-_pat,_keyboard_34_part_end  -_pat,_bass_34_part_end-_pat,_drums_34_part_end-_pat

	db "test crap"

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;-guitar--------------------------------------------------------------------

_guitar_verse_intro:

;Verse
;E    5 - - -   3 - - -	a-4, a-5, g#5, e-5
;A    - - - 7   - - - -	g-4, d-6, f#6, g-6, f#6
;D    - - 76-   - - - -
;G    - - - -   - - - -
;B    - - - -   - 3 - -
;E    - - - -   - - 232

;	db ctrlbt_hold_mode,0

;	db ctrlbt_panning,$ff
;	db ctrlbt_panning,$00
	db ctrlbt_auto_panner,%10_10_01_10,$01

	db ctrlbt_auto_pwm,0,0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,%1011_0_000
	db ctrlbt_arpeggio,$00,$00,$00

	db ctrlbt_a_4
	db ctrlbt_wait_3

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_verse:

;Verse
;E    5 - - -   3 - - -	a-4, a-5, g#5, e-5
;A    - - - 7   - - - -	g-4, d-6, f#6, g-6, f#6
;D    - - 76-   - - - -
;G    - - - -   - - - -
;B    - - - -   - 3 - -
;E    - - - -   - - 232

	db ctrlbt_arpeggio,$00,$00,$00

	db ctrlbt_a_4
	db ctrlbt_wait_3

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_g_4
	db ctrlbt_wait_3

	db ctrlbt_d_6
	db ctrlbt_wait_3

	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_g_6
	db ctrlbt_wait

	db ctrlbt_f#6
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_verse_end:

;Verse
;E    5 - - -   3 - - -	a-4, a-5, g#5, e-5
;A    - - - 7   - - - -	g-4, d-6, f#6, g-6, f#6
;D    - - 76-   - - - -
;G    - - - -   - - - -
;B    - - - -   - 3 - -
;E    - - - -   - - 232

	db ctrlbt_arpeggio,$00,$00,$00

	db ctrlbt_g_4
	db ctrlbt_wait_3

	db ctrlbt_d_6
	db ctrlbt_wait_3

	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_g_6
	db ctrlbt_wait

	db ctrlbt_f#6
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_chorus:

;Chorus (reggae)
;A (A + C# + E + A)
;F# + A  (doo doo)
;E + A ...E + B (doo doo doo)

	db ctrlbt_arpeggio,$04,$7c,$01

	db ctrlbt_a_5
	db ctrlbt_wait_3

	db ctrlbt_a_5
	db ctrlbt_wait_3

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_arpeggio,$05,$05,$01

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_arpeggio,$03,$03,$01

	db ctrlbt_wait_4

	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_arpeggio,$03,$03,$01

	db ctrlbt_f#4
	db ctrlbt_wait_3

	db ctrlbt_f#4
	db ctrlbt_wait_3

	db ctrlbt_arpeggio,$05,$05,$01

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait

	db ctrlbt_arpeggio,$07,$07,$01

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_chorus_end:

;Chorus (reggae)
;A (A + C# + E + A)
;F# + A  (doo doo)
;E + A ...E + B (doo doo doo)

	db ctrlbt_arpeggio,$04,$7c,$01

	db ctrlbt_a_5
	db ctrlbt_wait_3

	db ctrlbt_a_5
	db ctrlbt_wait_3

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_arpeggio,$05,$05,$01

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_arpeggio,$03,$03,$01

	db ctrlbt_wait_4

	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_bridge_1:

;Bridge
;Evil D chord to Evil A
;.... to evil B#
;.... to evil B
;.....to evil C or D??

	db ctrlbt_arpeggio,$06,$9c,$01
	db ctrlbt_transpose,12

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_c_5

	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_bridge_2:

;Bridge
;Evil D chord to Evil A
;.... to evil B#
;.... to evil B
;.....to evil C or D??

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_b_4

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_bridge_3:

;Bridge
;Evil D chord to Evil A
;.... to evil B#
;.... to evil B
;.....to evil C or D??

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_d_5

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_bridge_end:

;Bridge
;Evil D chord to Evil A
;.... to evil B#
;.... to evil B
;.....to evil C or D??

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_31

	db ctrlbt_transpose,0

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_34_part:

;3/4 Part
;A (A + C# + E + A)
;D# ( D# + e + a# + D#)
;C (c +e + g + c)
;B (b + f# + a + c#)

	db ctrlbt_arpeggio,$04,$7c,$01

	db ctrlbt_a_6
	db ctrlbt_wait_3

	db ctrlbt_a_6
	db ctrlbt_wait_3

	db ctrlbt_a_6
	db ctrlbt_wait_7

	db ctrlbt_arpeggio,$01,$8c,$01

	db ctrlbt_d#6
	db ctrlbt_wait_3

	db ctrlbt_d#6
	db ctrlbt_wait_7

	db ctrlbt_arpeggio,$04,$7c,$01

	db ctrlbt_c_6
	db ctrlbt_wait_3

	db ctrlbt_c_6
	db ctrlbt_wait_7

;B (b + f# + a + c#)

	db ctrlbt_arpeggio,$07,$ae,$01

	db ctrlbt_b_5
	db ctrlbt_wait_3

	db ctrlbt_b_5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_guitar_34_part_end:

	db ctrlbt_arpeggio,$04,$7c,$01

	db ctrlbt_a_6
	db ctrlbt_wait_3

	db ctrlbt_a_6
	db ctrlbt_wait_3

	db ctrlbt_a_6
	db ctrlbt_wait_39

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;-keys/reggae muted guitar--------------------------------------------------------------------------

_keyboard_verse_1:

	; keyboard melody

;	db ctrlbt_panning,$ff
	db ctrlbt_auto_panner,%01_01_10_01,$01

;	db ctrlbt_hold_mode,0

	db ctrlbt_duration,1
	db ctrlbt_envelope,%0000_1_010
	db ctrlbt_volume,%0001_0_000
	db ctrlbt_arpeggio,$00,$00,$00
	db ctrlbt_auto_pwm,%00_10_11_10,15
	db ctrlbt_portamento,$60
	db ctrlbt_porta_from,ctrlbt_a_7
	db ctrlbt_restart_envelope

	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_vibrato_reset

	db ctrlbt_a_7
	db ctrlbt_wait_7

	db ctrlbt_restart_envelope
	db ctrlbt_e_6
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_g#6
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_d_6
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_f#6
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_a_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_f#6
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_verse_2:

	; keyboard melody

	db ctrlbt_restart_envelope
	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_b_4
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_b_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_d_6
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_g_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_f#5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_verse_3:

	; keyboard melody

	db ctrlbt_restart_envelope
	db ctrlbt_a_6
	db ctrlbt_wait_7

	db ctrlbt_restart_envelope
	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_g_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_b_5
	db ctrlbt_wait_3

	db ctrlbt_f#5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_g_5
	db ctrlbt_wait_3

	db ctrlbt_f#5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_verse_4:

	; keyboard melody

	db ctrlbt_restart_envelope
	db ctrlbt_a_6
	db ctrlbt_wait_7

	db ctrlbt_restart_envelope
	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_g_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_g_5
	db ctrlbt_wait_3

	db ctrlbt_restart_envelope
	db ctrlbt_a_5
	db ctrlbt_wait_7

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_verse_end:

	db ctrlbt_arpeggio,$00,$00,$00

	db ctrlbt_wait_8

	db ctrlbt_volume,%1111_0_000
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_restart_envelope

	db ctrlbt_wait_8

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_chorus:

	db ctrlbt_wait_4

	db ctrlbt_duration,1
	db ctrlbt_volume,%1001_0_000
	db ctrlbt_arpeggio,$04,$7c,$02
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_auto_pwm,%00_01_10_11,6
	db ctrlbt_portamento,0

	db ctrlbt_vibrato,$00,$00

	db ctrlbt_a_6
	db ctrlbt_wait
	db ctrlbt_a_6
	db ctrlbt_wait_5

	db ctrlbt_a_6
	db ctrlbt_wait_7

	db ctrlbt_a_6
	db ctrlbt_wait
	db ctrlbt_a_6
	db ctrlbt_wait_5

	db ctrlbt_a_6
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_bridge_1:

;	db ctrlbt_envelope,%0000_1_111
;	db ctrlbt_volume,%0000_0_000
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_volume,%0111_0_000
	db ctrlbt_arpeggio,$00,$00,$00
	db ctrlbt_auto_pwm,%00_11_01_10,66

	db ctrlbt_portamento,$13

	db ctrlbt_restart_envelope

	db ctrlbt_vibrato,$1f,$5e
	db ctrlbt_vibrato_reset

	db ctrlbt_a_3
	db ctrlbt_wait_15

	db ctrlbt_d_8
	db ctrlbt_wait_15

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_bridge_2:

	db ctrlbt_portamento,$10

	db ctrlbt_wait_32

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_bridge_3:

	db ctrlbt_a_4
	db ctrlbt_wait_31

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_bridge_end:

	db ctrlbt_wait_16

	db ctrlbt_envelope,%0000_0_111

	db ctrlbt_wait_32

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_34_part_1:

;	db ctrlbt_volume,%0111_0_000
	db ctrlbt_volume,%1101_0_000
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_restart_envelope
	db ctrlbt_auto_pwm,%00_11_01_10,15

	db ctrlbt_portamento,$01
	db ctrlbt_c_3

	db ctrlbt_vibrato,$12,$1e
	db ctrlbt_vibrato_reset

	db ctrlbt_wait_47

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_34_part_2:

	db ctrlbt_wait_48

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_keyboard_34_part_end:

	db ctrlbt_portamento,$01
	db ctrlbt_wait_4

	db ctrlbt_portamento,$02
	db ctrlbt_wait_4

	db ctrlbt_portamento,$03
	db ctrlbt_wait_4

	db ctrlbt_portamento,$04
	db ctrlbt_wait_2

	db ctrlbt_envelope,%0000_0_111
;	db ctrlbt_restart_envelope
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_wait_2

	db ctrlbt_portamento,$05
	db ctrlbt_wait_4

	db ctrlbt_portamento,$06
	db ctrlbt_wait_4

	db ctrlbt_portamento,$07
	db ctrlbt_wait_4

	db ctrlbt_portamento,$08
	db ctrlbt_wait_19

	db ctrlbt_portamento,0
	db ctrlbt_note_off

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;-bass guitar--------------------------------------------------------------------------

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_verse:

;	db ctrlbt_hold_mode,0

	db ctrlbt_waveform,$00
;	db ctrlbt_gain_waveform,$59
	db ctrlbt_gain_waveform,$0c

	db ctrlbt_a_3
	db ctrlbt_wait_3

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait

	db ctrlbt_e_4
	db ctrlbt_wait_7

	db ctrlbt_g_3
	db ctrlbt_wait_3

	db ctrlbt_b_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_7

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_verse_end:

	db ctrlbt_g_3
	db ctrlbt_wait_3

	db ctrlbt_b_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_7

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_chorus:

	db ctrlbt_a_3
	db ctrlbt_wait_5

	db ctrlbt_a_4
	db ctrlbt_wait

	db ctrlbt_e_4
	db ctrlbt_wait_3

	db ctrlbt_g_4
	db ctrlbt_wait_3

	db ctrlbt_f#4
	db ctrlbt_wait_3

	db ctrlbt_f#4
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_7

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_bridge_1:

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_bridge_2:

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_bridge_3:

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_bridge_end:

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_d_5
	db ctrlbt_wait_3

	db ctrlbt_d_3
	db ctrlbt_wait_3

	db ctrlbt_d_4
	db ctrlbt_wait_3

	db ctrlbt_note_off
	db ctrlbt_wait_27

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_34_part:

	db ctrlbt_a_3
	db ctrlbt_wait_3

	db ctrlbt_c#5
	db ctrlbt_wait_3

	db ctrlbt_g_5
	db ctrlbt_wait_3

	db ctrlbt_c#4
	db ctrlbt_wait_3

	db ctrlbt_c_5
	db ctrlbt_wait_3

	db ctrlbt_f#5
	db ctrlbt_wait_3

	db ctrlbt_c_4
	db ctrlbt_wait_3

	db ctrlbt_b_4
	db ctrlbt_wait_3

	db ctrlbt_f_5
	db ctrlbt_wait_3

	db ctrlbt_b_3
	db ctrlbt_wait_3

	db ctrlbt_a#4
	db ctrlbt_wait_3

	db ctrlbt_e_5
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_bass_34_part_end:

	db ctrlbt_a_3
	db ctrlbt_wait_3

	db ctrlbt_a_3
	db ctrlbt_wait_3

	db ctrlbt_a_3
	db ctrlbt_wait_5

	db ctrlbt_note_off
	db ctrlbt_wait_33

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;-drums--------------------------------------------------------------------------

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_verse:

	db ctrlbt_volume,%0111_0_000
	db ctrlbt_hold_mode,1
	db ctrlbt_duration,50

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait
	db $2f
	db ctrlbt_wait

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait
	db $2f
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_verse_end:

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $2f
	db $2f
	db $2f
	db $2f

	db $2f
	db $2f
	db $2f
	db $2f

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_chorus:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait
	db $2f
	db ctrlbt_wait

	db $2f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_chorus_end:

	db $3f
	db ctrlbt_wait_15

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_bridge_1:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_bridge_2:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait
	db $2f
	db $2f

	db $2f
	db ctrlbt_wait
	db $2f
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_bridge_3:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait
	db $2f
	db $2f

	db $2f
	db ctrlbt_wait
	db $2f
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_bridge_end:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_25

	db $3f
	db ctrlbt_wait

	db $2f
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_34_part:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $2f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_drums_34_part_end:

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_3

	db $3f
	db ctrlbt_wait_39

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_silent_16:

	db ctrlbt_note_off
	db ctrlbt_wait_15

	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_end:

	end
