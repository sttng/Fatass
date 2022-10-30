;***************************************************************************

missionc_header:
	dw missionc_sequence-missionc_header	; sequence index
	dw missionc_patterns-missionc_header	; patterns index
	dw missionc_end-missionc_header	; patterns index
	db 29
	db 'MISSIONC'					; song name
	db $ff

missionc_sequence:

	db ctrlbt_shuffle,$00,$00
	db ctrlbt_panning,%11111111
	db ctrlbt_song_speed,$20

	db $00
	db ctrlbt_shuffle,$40,$a3
	db $01
	db ctrlbt_shuffle,$40,$c3
	db $02
	db $03
	db ctrlbt_shuffle,$00,$11
	db $04
	db $05
	db $06
	db $07
	db $08
	db $09
	db $0a
	db $0b
	db $0c
	db $0d
	db ctrlbt_shuffle,$40,$c3
	db $0e
	db $0f
	db ctrlbt_shuffle,$00,$11
	db $10
	db $11
	db $12
	db $13
	db $14
	db $15
	db $16
	db $17
	db $18
	db $19
	db $1a
	db $1b
	db $1c

	db ctrlbt_track_loop


missionc_patterns:
	dw missionc_tr_1_pt_00-missionc_patterns,missionc_tr_2_pt_00-missionc_patterns,missionc_tr_3_pt_00-missionc_patterns,missionc_tr_4_pt_00-missionc_patterns
	dw missionc_tr_1_pt_01-missionc_patterns,missionc_tr_2_pt_01-missionc_patterns,missionc_tr_3_pt_01-missionc_patterns,missionc_tr_4_pt_01-missionc_patterns
	dw missionc_tr_1_pt_02-missionc_patterns,missionc_tr_2_pt_02-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_02-missionc_patterns
	dw missionc_tr_1_pt_02-missionc_patterns,missionc_tr_2_pt_02-missionc_patterns,missionc_tr_3_pt_02-missionc_patterns,missionc_tr_4_pt_02-missionc_patterns
	dw missionc_tr_1_pt_03-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_07-missionc_patterns
	dw missionc_tr_1_pt_04-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_04-missionc_patterns
	dw missionc_tr_1_pt_03-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_03-missionc_patterns,missionc_tr_4_pt_05-missionc_patterns
	dw missionc_tr_1_pt_04-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_04-missionc_patterns,missionc_tr_4_pt_04-missionc_patterns
	dw missionc_tr_1_pt_03-missionc_patterns,missionc_tr_2_pt_03-missionc_patterns,missionc_tr_3_pt_03-missionc_patterns,missionc_tr_4_pt_03-missionc_patterns
	dw missionc_tr_1_pt_04-missionc_patterns,missionc_tr_2_pt_04-missionc_patterns,missionc_tr_3_pt_04-missionc_patterns,missionc_tr_4_pt_05-missionc_patterns
	dw missionc_tr_1_pt_05-missionc_patterns,missionc_tr_2_pt_05-missionc_patterns,missionc_tr_3_pt_05-missionc_patterns,missionc_tr_4_pt_03-missionc_patterns
	dw missionc_tr_1_pt_06-missionc_patterns,missionc_tr_2_pt_06-missionc_patterns,missionc_tr_3_pt_06-missionc_patterns,missionc_tr_4_pt_04-missionc_patterns
	dw missionc_tr_1_pt_01-missionc_patterns,missionc_tr_2_pt_01-missionc_patterns,missionc_tr_3_pt_01-missionc_patterns,missionc_tr_4_pt_06-missionc_patterns
	dw missionc_tr_1_pt_01-missionc_patterns,missionc_tr_2_pt_01-missionc_patterns,missionc_tr_3_pt_01-missionc_patterns,missionc_tr_4_pt_05-missionc_patterns
	dw missionc_tr_1_pt_02-missionc_patterns,missionc_tr_2_pt_02-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_02-missionc_patterns
	dw missionc_tr_1_pt_02-missionc_patterns,missionc_tr_2_pt_02-missionc_patterns,missionc_tr_3_pt_02-missionc_patterns,missionc_tr_4_pt_03-missionc_patterns
	dw missionc_tr_1_pt_03-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_05-missionc_patterns
	dw missionc_tr_1_pt_04-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_04-missionc_patterns
	dw missionc_tr_1_pt_05-missionc_patterns,missionc_tr_2_pt_05-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_03-missionc_patterns
	dw missionc_tr_1_pt_06-missionc_patterns,missionc_tr_2_pt_06-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_05-missionc_patterns
	dw missionc_tr_1_pt_08-missionc_patterns,missionc_tr_2_pt_08-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_07-missionc_patterns
	dw missionc_tr_1_pt_09-missionc_patterns,missionc_tr_2_pt_09-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_09-missionc_patterns
	dw missionc_tr_1_pt_08-missionc_patterns,missionc_tr_2_pt_08-missionc_patterns,missionc_tr_3_pt_08-missionc_patterns,missionc_tr_4_pt_08-missionc_patterns
	dw missionc_tr_1_pt_09-missionc_patterns,missionc_tr_2_pt_09-missionc_patterns,missionc_tr_3_pt_09-missionc_patterns,missionc_tr_4_pt_09-missionc_patterns
	dw missionc_tr_1_pt_08-missionc_patterns,missionc_tr_2_pt_08-missionc_patterns,missionc_tr_3_pt_08-missionc_patterns,missionc_tr_4_pt_08-missionc_patterns
	dw missionc_tr_1_pt_09-missionc_patterns,missionc_tr_2_pt_09-missionc_patterns,missionc_tr_3_pt_09-missionc_patterns,missionc_tr_4_pt_09-missionc_patterns
	dw missionc_tr_1_pt_10-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_10-missionc_patterns,missionc_tr_4_pt_10-missionc_patterns
	dw missionc_tr_1_pt_11-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_11-missionc_patterns,missionc_tr_4_pt_11-missionc_patterns
	dw missionc_tr_1_pt_12-missionc_patterns,missionc_tr_2_pt_07-missionc_patterns,missionc_tr_3_pt_07-missionc_patterns,missionc_tr_4_pt_07-missionc_patterns

;--------------------------------------------------

missionc_tr_1_pt_00:

                ; M 1

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_song_speed,$20
	db ctrlbt_auto_pwm,%10_01_11_01,1
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_song_speed,$40
	db ctrlbt_g_3      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_auto_pwm,%10_01_11_01,2
	db ctrlbt_g#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_song_speed,$20
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_pwm,%10_01_11_01,3
	db ctrlbt_g_3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_11_01,4
	db ctrlbt_f_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_11_01,5
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_auto_panner,%11_10_11_01,3 ; autopan off
	db ctrlbt_auto_pwm,%10_01_11_01,0
	db ctrlbt_song_speed,$30
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0      
	db ctrlbt_wait
	db ctrlbt_volume,$e0 
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db ctrlbt_wait
	db ctrlbt_volume,$b0 
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_3
	db ctrlbt_portamento,$08
	db ctrlbt_f_6
	db ctrlbt_volume,$a0  
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_auto_pwm,%10_01_11_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_wait
	db ctrlbt_volume,$60
	db ctrlbt_wait
	db ctrlbt_volume,$50 
	db ctrlbt_wait
	db ctrlbt_volume,$60
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_auto_panner,%00_01_00_01,0 ; autopan off
	db ctrlbt_auto_pwm,%10_01_11_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

missionc_tr_1_pt_01:

                ; M 5

	db ctrlbt_song_speed,$40
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 7

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_a_4        
	db ctrlbt_wait       
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 9

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_g#3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 10

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

missionc_tr_1_pt_02:

                ; M 11

	db ctrlbt_song_speed,$30

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_song_speed,$33
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 13

	db ctrlbt_song_speed,$37
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14

	db ctrlbt_song_speed,$40
;	db ctrlbt_song_speed,$32
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait
;	db ctrlbt_song_speed,$34      
	db ctrlbt_f_4       
	db ctrlbt_wait      
	db ctrlbt_f_4      
	db ctrlbt_wait
;	db ctrlbt_song_speed,$36  
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait
;	db ctrlbt_song_speed,$38
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_1_pt_03:

                ; M 15

	db ctrlbt_song_speed,$40

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
        db ctrlbt_duration,40

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_c#5       
	db ctrlbt_volume,$90
	db ctrlbt_a#3      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3       
	db ctrlbt_volume,$90
	db ctrlbt_c#5      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3      
	db ctrlbt_volume,$90
	db ctrlbt_a#3       
	db ctrlbt_volume,$f0
	db ctrlbt_a#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3       
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$90
	db ctrlbt_a#4
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_a#3

                ; M 16

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_c#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#5       
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#4      
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4     
	db ctrlbt_volume,$90
	db ctrlbt_c#4       
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_c#4

                ; M 17

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3      
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_g#3      
	db ctrlbt_volume,$90
	db ctrlbt_g#4       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3       
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_g#3

                ; M 18

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4      
	db ctrlbt_volume,$90
	db ctrlbt_g#3      
	db ctrlbt_volume,$f0
	db ctrlbt_g#4      
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_g#3     
	db ctrlbt_volume,$90
	db ctrlbt_g#4       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3       
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_g#3

        db ctrlbt_duration,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_1_pt_04:

                ; M 19

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3      
	db ctrlbt_volume,$f0
	db ctrlbt_a#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3      
	db ctrlbt_volume,$90
	db ctrlbt_a#4       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3       
	db ctrlbt_volume,$f0
	db ctrlbt_a#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_a#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_a#3

                ; M 20

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_c#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#5       
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#4      
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4     
	db ctrlbt_volume,$90
	db ctrlbt_c#4       
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_c#4

                ; M 21

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#4       
	db ctrlbt_volume,$90
	db ctrlbt_d_4      
	db ctrlbt_volume,$f0
	db ctrlbt_a#4
	db ctrlbt_volume,$90
	db ctrlbt_g#4      
	db ctrlbt_volume,$f0
	db ctrlbt_d_4      
	db ctrlbt_volume,$90
	db ctrlbt_a#4       
	db ctrlbt_volume,$f0
	db ctrlbt_g#4       
	db ctrlbt_volume,$90
	db ctrlbt_d_4       
	db ctrlbt_volume,$f0
	db ctrlbt_f#4
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_f#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d_4

                ; M 22

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4      
	db ctrlbt_volume,$90
	db ctrlbt_d_4      
	db ctrlbt_volume,$f0
	db ctrlbt_g#4      
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_d_4     
	db ctrlbt_volume,$90
	db ctrlbt_g#4       
	db ctrlbt_volume,$f0
	db ctrlbt_a#4       
	db ctrlbt_volume,$90
	db ctrlbt_d_4       
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_a#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d_4
	db ctrlbt_volume,$f0
	db ctrlbt_f#4
	db ctrlbt_volume,$90
	db ctrlbt_g#4

        db ctrlbt_duration,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_1_pt_05:

                ; M 23

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_f#4
	db ctrlbt_volume,$f0
	db ctrlbt_c#5       
	db ctrlbt_volume,$90
	db ctrlbt_a#3      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3       
	db ctrlbt_volume,$90
	db ctrlbt_c#5      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3      
	db ctrlbt_volume,$90
	db ctrlbt_a#3       
	db ctrlbt_volume,$f0
	db ctrlbt_a#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3       
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$90
	db ctrlbt_a#4
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_a#3

                ; M 24

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_c#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#5       
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#4      
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4     
	db ctrlbt_volume,$90
	db ctrlbt_c#4       
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_c#4

                ; M 25

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3      
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_g#3      
	db ctrlbt_volume,$90
	db ctrlbt_g#4       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3       
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_g#3

                ; M 26

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4      
	db ctrlbt_volume,$90
	db ctrlbt_g#3      
	db ctrlbt_volume,$f0
	db ctrlbt_g#4      
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_g#3     
	db ctrlbt_volume,$90
	db ctrlbt_g#4       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3       
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_g#3

        db ctrlbt_duration,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

missionc_tr_1_pt_06:

                ; M 27

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3      
	db ctrlbt_volume,$f0
	db ctrlbt_a#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3      
	db ctrlbt_volume,$90
	db ctrlbt_a#4       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_a#3       
	db ctrlbt_volume,$f0
	db ctrlbt_a#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_volume,$90
	db ctrlbt_a#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_a#3

                ; M 28

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_c#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#5       
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_c#4      
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4     
	db ctrlbt_volume,$90
	db ctrlbt_c#4       
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_c#4
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_c#4

                ; M 29

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$f0
	db ctrlbt_e_3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4       
	db ctrlbt_volume,$90
	db ctrlbt_e_3      
	db ctrlbt_volume,$f0
	db ctrlbt_e_4
	db ctrlbt_volume,$90
	db ctrlbt_e_3      
	db ctrlbt_volume,$f0
	db ctrlbt_e_3      
	db ctrlbt_volume,$90
	db ctrlbt_e_4       
	db ctrlbt_volume,$f0
	db ctrlbt_b_3       
	db ctrlbt_volume,$90
	db ctrlbt_e_3       
	db ctrlbt_volume,$f0
	db ctrlbt_e_4
	db ctrlbt_volume,$90
	db ctrlbt_b_3
	db ctrlbt_volume,$f0
	db ctrlbt_e_3
	db ctrlbt_volume,$90
	db ctrlbt_e_4
	db ctrlbt_volume,$f0
	db ctrlbt_b_3
	db ctrlbt_volume,$90
	db ctrlbt_e_3

                ; M 30

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_b_3
	db ctrlbt_volume,$f0
	db ctrlbt_b_3      
	db ctrlbt_volume,$90
	db ctrlbt_d#4      
	db ctrlbt_volume,$f0
	db ctrlbt_d#4      
	db ctrlbt_volume,$90
	db ctrlbt_g#3      
	db ctrlbt_volume,$f0
	db ctrlbt_g#3     
	db ctrlbt_volume,$90
	db ctrlbt_d#4       
	db ctrlbt_volume,$f0
	db ctrlbt_g#4       
	db ctrlbt_volume,$90
	db ctrlbt_g#3       
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_volume,$90
	db ctrlbt_d#4
	db ctrlbt_volume,$f0
	db ctrlbt_d#4
	db ctrlbt_volume,$90
	db ctrlbt_g#3

        db ctrlbt_duration,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

missionc_tr_1_pt_07:

                ; M 27 Blank

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_transpose,00

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_1_pt_08:

                ; M 31 Blank

	db ctrlbt_song_speed,$40
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,00

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_b_5      
	db ctrlbt_wait       
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 32

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_a#5       
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_f#5      
	db ctrlbt_wait       
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait

                ; M 33

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_f#5
	db ctrlbt_wait       
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 34

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_1_pt_09:

                ; M 35

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,00

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 36

	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_b_5      
	db ctrlbt_wait       
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 37

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait       
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_c#5      
	db ctrlbt_wait       
	db ctrlbt_f#4
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

missionc_tr_1_pt_10:

                ; M 39

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,00

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_c#3      
	db ctrlbt_wait       
	db ctrlbt_c#5      
	db ctrlbt_wait       
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 40

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_c_3      
	db ctrlbt_wait       
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_c_5
	db ctrlbt_wait

                ; M 41

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_e_5       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait       
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 42

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_c#3      
	db ctrlbt_wait       
	db ctrlbt_c#5      
	db ctrlbt_wait       
	db ctrlbt_c#4
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_1_pt_11:

                ; M 43

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,00

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_c_3      
	db ctrlbt_wait       
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 44

	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_5       
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_a_4      
	db ctrlbt_wait       
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 45

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait       
	db ctrlbt_a_4      
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait

                ; M 46

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_e_5      
	db ctrlbt_wait       
	db ctrlbt_e_3
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

missionc_tr_1_pt_12:

                ; M 47

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,00

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_a_4      
	db ctrlbt_wait       
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_b_4
	db ctrlbt_wait

                ; M 48

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_e_5       
	db ctrlbt_wait      
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait     
;	db ctrlbt_wait       
;	db ctrlbt_wait     
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait

                ; M 49

;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait
;	db ctrlbt_wait       
;	db ctrlbt_wait     
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait

                ; M 50

;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait     
;	db ctrlbt_wait       
;	db ctrlbt_wait     
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

missionc_tr_2_pt_00:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_auto_pwm,%10_01_11_01,1
	db ctrlbt_arpeggio,$c0,$c0,3
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$3f
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_f_6
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$05,$3f
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$06,$3f 
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_00_11_00,0
	db ctrlbt_arpeggio,$7c,$ca,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_d#6
	db ctrlbt_d_6
	db ctrlbt_c_6
	db ctrlbt_a_5
	db ctrlbt_a#5
	db ctrlbt_c_6

	db ctrlbt_arpeggio,$7c,$ca,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------------

missionc_tr_2_pt_01:

                ; M 5

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_transpose,00

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait

                ; M 6

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait

                ; M 7

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait

                ; M 8

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3       
	db ctrlbt_wait    
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3       
	db ctrlbt_wait

                ; M 9

	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3     	     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3               
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 10

	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3     	     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3               
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_2_pt_02:

                ; M 11

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_transpose,00

	db ctrlbt_arpeggio,$c0,$c0,5
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,0
	db ctrlbt_f_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g_4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g#4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,0
	db ctrlbt_g#3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 13

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,47
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,47
	db ctrlbt_f_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g_4
	db ctrlbt_portamento,$2f
	db ctrlbt_duration,47
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,0   
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_d_4
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,47
	db ctrlbt_d_3
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_d_4
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,47
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g#4
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,47
	db ctrlbt_g#3
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------------

missionc_tr_2_pt_03:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,0

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_auto_panner,%11_10_11_01,3 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3



	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_pulse_width,%01_000000
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f 
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0 ; autopan off
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_2_pt_04:

                ; M 19

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    

	db ctrlbt_auto_panner,%11_10_11_01,3 ; autopan off
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 21


	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_pulse_width,%01_000000
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_d_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_b_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_c#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f       
	db ctrlbt_b_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%11_10_11_01,0 ; autopan off
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_2_pt_05:

                ; M 23

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,0

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_auto_panner,%11_10_11_01,3 ; autopan off
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 25


	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f     
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f           
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00    
	db ctrlbt_auto_panner,%11_10_11_01,0 ; autopan off
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;---------------------------------------------

missionc_tr_2_pt_06:

                ; M 27

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,0

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_auto_panner,%11_10_11_01,3 ; autopan off
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29


	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f           
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f     
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00   
	db ctrlbt_auto_panner,%11_10_11_01,0 ; autopan off
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_2_pt_07:

                ; M 27 Blank

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$9f
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_transpose,12

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;----------------------------------------------

missionc_tr_2_pt_08:

                ; M 31

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_transpose,0

	db ctrlbt_volume,$f0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_volume,$b0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_volume,$f0
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_f#5      
	db ctrlbt_volume,$f0
	db ctrlbt_c#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$b0 
	db ctrlbt_c#5       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

	db ctrlbt_volume,$f0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_volume,$b0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_a#5       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_a#5      
	db ctrlbt_volume,$f0
	db ctrlbt_d#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$b0 
	db ctrlbt_d#5       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 33

	db ctrlbt_volume,$f0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_volume,$b0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_f#5      
	db ctrlbt_volume,$f0
	db ctrlbt_d#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$b0 
	db ctrlbt_d#5       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 34

	db ctrlbt_volume,$f0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_g#5
	db ctrlbt_volume,$f0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0       
	db ctrlbt_f#5      
	db ctrlbt_volume,$f0
	db ctrlbt_a#5
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$b0      
	db ctrlbt_a#5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;----------------------------------------------

missionc_tr_2_pt_09:

                ; M 35

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_transpose,0

	db ctrlbt_volume,$f0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_volume,$b0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_volume,$f0
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_f#5      
	db ctrlbt_volume,$f0
	db ctrlbt_d#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$b0 
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

	db ctrlbt_volume,$f0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_volume,$b0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_volume,$f0
	db ctrlbt_a#5       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_a#5      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$f0  
	db ctrlbt_f#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_f#5

                ; M 37

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_volume,$b0      
	db ctrlbt_g#5      
	db ctrlbt_volume,$f0
	db ctrlbt_g#5
	db ctrlbt_volume,$b0
	db ctrlbt_a#5      
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_volume,$f0 
	db ctrlbt_f#5      
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_volume,$b0
	db ctrlbt_f#5

                ; M 38

	db ctrlbt_volume,$f0
	db ctrlbt_d#5
	db ctrlbt_volume,$b0
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_volume,$f0
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_volume,$b0       
	db ctrlbt_f#5
	db ctrlbt_volume,$f0
	db ctrlbt_c#5
	db ctrlbt_volume,$b0
	db ctrlbt_g#5      
	db ctrlbt_wait      
	db ctrlbt_c#5
	db ctrlbt_volume,$f0 
	db ctrlbt_d#5      
	db ctrlbt_wait       
	db ctrlbt_a#4
	db ctrlbt_volume,$b0
	db ctrlbt_d#5

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

missionc_tr_3_pt_00:

                ; M 1

        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_gain_waveform,$08
	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_f_6       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_b_5
	db ctrlbt_portamento,$96
	db ctrlbt_e_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00   
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#5
	db ctrlbt_portamento,$96
	db ctrlbt_d#6
	db ctrlbt_wait

                ; M 2

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c_6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f     
	db ctrlbt_g_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$96
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07 
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$2f       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$06,$2f
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_arpeggio,$c0,$c0,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;------------------------------------------------

missionc_tr_3_pt_01:

                ; M 5

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_gain_waveform,$08
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f      
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00  
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f 
	db ctrlbt_d_6
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_d#6
	db ctrlbt_wait

                ; M 7

	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_c_6       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00  
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait

                ; M 9

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f 
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00  
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 10

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f 
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_d#6
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------------

missionc_tr_3_pt_02:

                ; M 11

        db ctrlbt_transpose,24
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$20
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$16
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$20
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$24
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 13

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$20
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$16   
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$22
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$06,$28
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_3_pt_03:

                ; M 15

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 17

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f   
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------

missionc_tr_3_pt_04:

                ; M 19

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$88
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 21

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_d_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_c#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_3_pt_05:

                ; M 23

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 25

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;---------------------------------------------

missionc_tr_3_pt_06:

                ; M 27

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$88
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-----------------------------------------

missionc_tr_3_pt_07:

                ; M 27 Blank

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop


;---------------------------------------------

missionc_tr_3_pt_08:

                ; M 31

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$16
        db ctrlbt_transpose,0
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$40

	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

	db ctrlbt_a#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 33

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#6
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#7
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 34

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00   
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

missionc_tr_3_pt_09:

                ; M 35

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$16
        db ctrlbt_transpose,0
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$40

	db ctrlbt_g#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_g#6
	db ctrlbt_wait

                ; M 36

	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 37

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#7
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_c#7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00   
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

missionc_tr_3_pt_10:

                ; M 39

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30

	db ctrlbt_b_6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$30      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_a#6     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_c#7
	db ctrlbt_wait

                ; M 40

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_a_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$30  
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_g#6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#7
	db ctrlbt_wait

                ; M 41

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_g_6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$30      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 42

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$30       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_f#6
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00   
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;---------------------------------------

missionc_tr_3_pt_11:

                ; M 43

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30

	db ctrlbt_g#6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_a_6       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_b_6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$30    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_c#7
	db ctrlbt_wait

                ; M 44

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_d#7
	db ctrlbt_wait
	db ctrlbt_e_7       
	db ctrlbt_wait      
	db ctrlbt_f#7
	db ctrlbt_wait      
	db ctrlbt_e_7      
	db ctrlbt_wait       
	db ctrlbt_f#7
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_e_7
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$39
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$37
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$35
	db ctrlbt_wait

                ; M 45

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$33
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$30
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$31
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$33
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$35
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$37
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$30      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$39
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$30       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait

                ; M 46

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00   
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------

missionc_tr_4_pt_00:

                ; M 1

        db ctrlbt_duration,30
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,55
	db ctrlbt_volume,$d0
	db $3a       
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3a
	db ctrlbt_wait
        db ctrlbt_duration,0 
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,55
	db $3a
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $38
	db ctrlbt_volume,$90
	db $36
	db ctrlbt_volume,$d0
	db $34
	db ctrlbt_volume,$a0
	db $32

	db ctrlbt_track_loop

;------------------------------------------

missionc_tr_4_pt_01:

                ; M 5

	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$f0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $35
	db ctrlbt_wait

                ; M 6

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_volume,$e0
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait

                ; M 7

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$e0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait

                ; M 8

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db $35       
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,45
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 9

	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 10

	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $35       
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

missionc_tr_4_pt_02:

                ; M 11

	db ctrlbt_auto_panner,%11_10_11_01,3 ; autopan off
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_duration,0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_111
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 13

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 14

        db ctrlbt_duration,0
	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_101
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,39
	db ctrlbt_volume,$40
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$60
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0 ; autopan off
	db ctrlbt_track_loop

;----------------------------------------

missionc_tr_4_pt_03:

                ; M 11

	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$2f
	db ctrlbt_wait

                ; M 12

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 13

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait

                ; M 14

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$25       
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------


missionc_tr_4_pt_04:

                ; M 11

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$2f
	db ctrlbt_wait

                ; M 12

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 13

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait

                ; M 14

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,56
	db $40+$2f
	db ctrlbt_volume,$a0
	db $40+$2e
	db ctrlbt_volume,$d0
	db $40+$2d
	db ctrlbt_volume,$a0
	db $40+$2c

	db ctrlbt_track_loop

;------------------------------------------

missionc_tr_4_pt_05:

                ; M 11

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$2f
	db ctrlbt_wait

                ; M 12

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 13

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait

                ; M 14

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_4_pt_06:

                ; M 11

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$2f
	db ctrlbt_wait

                ; M 12

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 13

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_wait

                ; M 14

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait

                ; M 15

	db ctrlbt_volume,$f0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$2f
	db ctrlbt_wait

                ; M 16

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,43
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$2f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$2b
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

missionc_tr_4_pt_07:

                ; M 11

	db ctrlbt_auto_panner,%11_10_11_01,0 ; autopan off
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_duration,0
	db $40+$21
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 13

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 14

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$70
	db $35
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_volume,$80
	db $35
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_duration,10
	db $3f
	db ctrlbt_wait       

	db ctrlbt_track_loop

;---------------------------------------

missionc_tr_4_pt_08:

                ; M 31

	db ctrlbt_auto_panner,%11_10_11_01,0 
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,40

	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $35
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $35
	db ctrlbt_volume,$80
	db $3d

                ; M 32

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $35
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_duration,40
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d

                ; M 33

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$b0
	db $3f

                ; M 34

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $35
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------

missionc_tr_4_pt_09:

                ; M 35

	db ctrlbt_auto_panner,%11_10_11_01,0 
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,40

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$80
	db $3d

                ; M 36

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d

                ; M 37

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait      


                ; M 38

	db ctrlbt_volume,$f0
	db $40+$25       
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
        db ctrlbt_duration,20
	db $40+$25       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$25       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------

missionc_tr_4_pt_10:

                ; M 39

	db ctrlbt_auto_panner,%11_10_11_01,0 
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,40

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait       

                ; M 40

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait       

                ; M 41

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       

                ; M 42


	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$80
	db $35
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait       

	db ctrlbt_track_loop

;-----------------------------------------

missionc_tr_4_pt_11:

                ; M 43

	db ctrlbt_auto_panner,%11_10_11_01,0 
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,40

	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       

                ; M 44

	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       

                ; M 45


	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       

                ; M 46


	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       
	db $35
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait       

	db ctrlbt_track_loop

missionc_end:

        end

;-----------------------------------------------
