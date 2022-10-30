;***************************************************************************

missiona_header:
	dw missiona_sequence-missiona_header	; sequence index
	dw missiona_patterns-missiona_header	; patterns index
	dw missiona_end-missiona_header	; patterns index
	db 15
	db 'MISSIONA'					; song name
	db $ff

missiona_sequence:


	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$20
	db ctrlbt_panning,%11111111
	db $00
	db $01
	db $02
	db $03
	db $04
	db ctrlbt_song_speed,$27
	db ctrlbt_panning,%11101101
	db ctrlbt_shuffle,$40,$73
	db $05
	db $06
	db ctrlbt_song_speed,$20
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_panning,%11111111
	db $07
	db $08
	db $09
	db $0a
	db $0b
	db $0c
	db ctrlbt_panning,%11101101
	db $0d
	db $0e

	db ctrlbt_track_loop


missiona_patterns:
	dw missiona_tr_1_pt_09-missiona_patterns,missiona_tr_2_pt_09-missiona_patterns,missiona_tr_3_pt_00-missiona_patterns,missiona_tr_4_pt_04-missiona_patterns
	dw missiona_tr_1_pt_01-missiona_patterns,missiona_tr_2_pt_01-missiona_patterns,missiona_tr_3_pt_01-missiona_patterns,missiona_tr_4_pt_00-missiona_patterns
	dw missiona_tr_1_pt_02-missiona_patterns,missiona_tr_2_pt_02-missiona_patterns,missiona_tr_3_pt_02-missiona_patterns,missiona_tr_4_pt_00-missiona_patterns
	dw missiona_tr_1_pt_03-missiona_patterns,missiona_tr_2_pt_03-missiona_patterns,missiona_tr_3_pt_03-missiona_patterns,missiona_tr_4_pt_01-missiona_patterns
	dw missiona_tr_1_pt_04-missiona_patterns,missiona_tr_2_pt_04-missiona_patterns,missiona_tr_3_pt_04-missiona_patterns,missiona_tr_4_pt_02-missiona_patterns
	dw missiona_tr_1_pt_05-missiona_patterns,missiona_tr_2_pt_05-missiona_patterns,missiona_tr_3_pt_05-missiona_patterns,missiona_tr_4_pt_03-missiona_patterns
	dw missiona_tr_1_pt_06-missiona_patterns,missiona_tr_2_pt_06-missiona_patterns,missiona_tr_3_pt_06-missiona_patterns,missiona_tr_4_pt_05-missiona_patterns
	dw missiona_tr_1_pt_07-missiona_patterns,missiona_tr_2_pt_07-missiona_patterns,missiona_tr_3_pt_07-missiona_patterns,missiona_tr_4_pt_06-missiona_patterns
	dw missiona_tr_1_pt_08-missiona_patterns,missiona_tr_2_pt_08-missiona_patterns,missiona_tr_3_pt_08-missiona_patterns,missiona_tr_4_pt_06-missiona_patterns
	dw missiona_tr_1_pt_00-missiona_patterns,missiona_tr_2_pt_00-missiona_patterns,missiona_tr_3_pt_00-missiona_patterns,missiona_tr_4_pt_01-missiona_patterns
	dw missiona_tr_1_pt_00-missiona_patterns,missiona_tr_2_pt_00-missiona_patterns,missiona_tr_3_pt_00-missiona_patterns,missiona_tr_4_pt_02-missiona_patterns
	dw missiona_tr_1_pt_03-missiona_patterns,missiona_tr_2_pt_03-missiona_patterns,missiona_tr_3_pt_03-missiona_patterns,missiona_tr_4_pt_00-missiona_patterns
	dw missiona_tr_1_pt_04-missiona_patterns,missiona_tr_2_pt_04-missiona_patterns,missiona_tr_3_pt_04-missiona_patterns,missiona_tr_4_pt_00-missiona_patterns
	dw missiona_tr_1_pt_07-missiona_patterns,missiona_tr_2_pt_07-missiona_patterns,missiona_tr_3_pt_07-missiona_patterns,missiona_tr_4_pt_01-missiona_patterns
	dw missiona_tr_1_pt_08-missiona_patterns,missiona_tr_2_pt_08-missiona_patterns,missiona_tr_3_pt_08-missiona_patterns,missiona_tr_4_pt_02-missiona_patterns



;--------------------------------------------------


missiona_tr_1_pt_00:


                ; intro
                ; M 1


	db ctrlbt_pulse_width,%10_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_auto_pwm,%11_01_00_11,1
        db ctrlbt_transpose,-24
	db ctrlbt_g_5     
	db ctrlbt_wait
	db ctrlbt_g_5       
	db ctrlbt_wait      
        db ctrlbt_transpose,-12
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
        db ctrlbt_transpose,-24
	db ctrlbt_g#5       
	db ctrlbt_wait       
	db ctrlbt_g#5
	db ctrlbt_wait
        db ctrlbt_transpose,0
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%11_00_00_11,0

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------
	
missiona_tr_1_pt_01:

                ; M 2

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_duration,20

	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 3


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_f_6       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_a#5
	db ctrlbt_wait


                ; M 4


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_c_6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_g#5
	db ctrlbt_wait


                ; M 5

	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_duration,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$35
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_duration,5
	db ctrlbt_restart_envelope
	db ctrlbt_glissando,0
	db ctrlbt_porta_from,ctrlbt_d_5
	db ctrlbt_portamento,$3f
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_glissando,0
	db ctrlbt_porta_from,ctrlbt_a#5
	db ctrlbt_portamento,$3f
	db ctrlbt_f_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

missiona_tr_1_pt_02:


                ; M 6

	db ctrlbt_pulse_width,%10_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_duration,20
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_auto_panner,%11_10_11_01,1

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 7


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_f_6       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_g#6
	db ctrlbt_wait


                ; M 8


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_d#6
	db ctrlbt_wait


                ; M 9


	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_duration,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$35
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_duration,5
	db ctrlbt_restart_envelope
	db ctrlbt_glissando,0
	db ctrlbt_porta_from,ctrlbt_d_6
	db ctrlbt_portamento,$3f
	db ctrlbt_g_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00      
	db ctrlbt_d#6      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_glissando,0
	db ctrlbt_porta_from,ctrlbt_c_7
	db ctrlbt_portamento,$3f   
	db ctrlbt_g#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

missiona_tr_1_pt_03:

                ; M 10

	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%11_10_01_00,0
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_f_5      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0 
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 11


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_d#6      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0 
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_c#6       
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_c#6


                ; M 12


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait


                ; M 13


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0   
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,1
	db ctrlbt_c#6
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_auto_pwm,%11_11_00_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------


missiona_tr_1_pt_04:


                ; M 14


	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%11_10_01_00,0
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_f_5      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c4,$7e,0 
	db ctrlbt_g_5       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011 
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010  
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 15


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_arpeggio,$ca,$cf,0
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_arpeggio,$c4,$7e,0
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%11_10_01_00,0
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_e_6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 16


	db ctrlbt_d#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 17


	db ctrlbt_song_speed,$19
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_song_speed,$18
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_song_speed,$20
	db ctrlbt_auto_pwm,%11_11_00_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_1_pt_05:

                ; M 18

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_g_5     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_6      
	db ctrlbt_wait      
	db ctrlbt_d_6      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010   
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 19

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_c_6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c_6     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110 
	db ctrlbt_d_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#6
	db ctrlbt_wait

                ; M 21

	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

missiona_tr_1_pt_06:

                ; M 22

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f


	db ctrlbt_b_5    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_d_6     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110  
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g#6    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_g_6      
	db ctrlbt_wait      
	db ctrlbt_g#6     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 24


	db ctrlbt_d#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c_6     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100   
	db ctrlbt_d_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#6
	db ctrlbt_wait


                ; M 25


	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_1_pt_07:

                ; M 22

	db ctrlbt_pulse_width,%00_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010  
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 23

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_f_5      
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


                ; M 24

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_f_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010       
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait


                ; M 25

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d_5
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
	db ctrlbt_d_5
	db ctrlbt_d#5
	db ctrlbt_f_5
	db ctrlbt_g_5
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g#5
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

missiona_tr_1_pt_08:

                ; M 22

	db ctrlbt_pulse_width,%00_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0

        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d#5
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
	db ctrlbt_d#5
	db ctrlbt_g_5
	db ctrlbt_g#5
	db ctrlbt_a#5
	db ctrlbt_g#5
	db ctrlbt_wait

                ; M 23

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g_5    
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
	db ctrlbt_d#5
	db ctrlbt_g_5
	db ctrlbt_g#5
	db ctrlbt_a#5
	db ctrlbt_g#5
	db ctrlbt_wait


                ; M 24


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g_5   
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 25


	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

missiona_tr_1_pt_09:

                ; intro Blank

                ; M 1

	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f

	db ctrlbt_auto_panner,%01_00_01_00,1
	db ctrlbt_auto_pwm,%11_01_00_11,0
	db ctrlbt_fine_tune,28
        db ctrlbt_transpose,0
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_auto_panner,%00_11_11_00,0
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%00_11_11_00,0
	db ctrlbt_fine_tune,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-------------------------------------------

missiona_tr_2_pt_00:


                ; intro
                ; M 1

	db ctrlbt_auto_pwm,%11_01_00_11,1
	db ctrlbt_auto_panner,%00_11_11_00,0
	db ctrlbt_pulse_width,%10_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

        db ctrlbt_transpose,-24
	db ctrlbt_wait
	db ctrlbt_g_5     
	db ctrlbt_wait
	db ctrlbt_g_5       
	db ctrlbt_wait    
        db ctrlbt_transpose,-12
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
        db ctrlbt_transpose,-24  
	db ctrlbt_g#5       
	db ctrlbt_wait       
	db ctrlbt_g#5
	db ctrlbt_wait
        db ctrlbt_transpose,0
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_d#5

	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%11_00_00_11,0

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_2_pt_01:



                ; M 2

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00

	db ctrlbt_duration,40
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_6       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_c_6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 5

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_duration,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$35
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;--------------------------------------------------

missiona_tr_2_pt_02:

                ; M 6

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_pulse_width,%01_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_duration,40
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 7

	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_6       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 8

	db ctrlbt_g#6
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait



                ; M 9


	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_duration,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$35
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6      
	db ctrlbt_wait       
	db ctrlbt_g#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_2_pt_03:


                ; M 10


	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_auto_pwm,%11_00_00_11,0
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5

                ; M 11

	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d#6      
	db ctrlbt_wait      
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_c#6       
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_c#6
	db ctrlbt_wait



                ; M 12

	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5



                ; M 13

	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait       
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0
	db ctrlbt_auto_pwm,%11_11_00_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------
missiona_tr_2_pt_04:


                ; M 14


	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_auto_pwm,%11_00_00_11,0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5


                ; M 15

	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_auto_panner,%11_01_11_10,0
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_e_6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 16

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 17

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_5
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

	db ctrlbt_auto_panner,%11_01_11_10,0
	db ctrlbt_auto_pwm,%11_11_00_01,0

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

missiona_tr_2_pt_05:

                ; M 18

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_01_11_10,0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_5     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_6      
	db ctrlbt_wait      
	db ctrlbt_d_6      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011  
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_c_6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 20

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c_6     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110 
	db ctrlbt_d_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 21


	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_portamento,$00

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

missiona_tr_2_pt_06:

                ; M 22

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_01_11_10,0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_5    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_d_6     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110 
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 23

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#6    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g_6      
	db ctrlbt_wait      
	db ctrlbt_g#6     
	db ctrlbt_wait
	db ctrlbt_g_6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_6
	db ctrlbt_wait



                ; M 24

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c_6     
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_d_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 25

	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_portamento,$00
	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

missiona_tr_2_pt_07:

                ; M 22

	db ctrlbt_pulse_width,%01_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0

        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait


                ; M 23

	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 24

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_f_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait



                ; M 25

	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d_5
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
	db ctrlbt_d_5
	db ctrlbt_d#5
	db ctrlbt_f_5
	db ctrlbt_g_5

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

missiona_tr_2_pt_08:

                ; M 22

	db ctrlbt_pulse_width,%01_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,00
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$04,$2f

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_d#5
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
	db ctrlbt_d#5
	db ctrlbt_g_5
	db ctrlbt_g#5
	db ctrlbt_a#5


                ; M 23

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g_5    
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
	db ctrlbt_d#5
	db ctrlbt_g_5
	db ctrlbt_g#5
	db ctrlbt_a#5



                ; M 24

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_g_5   
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait



                ; M 25

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------

missiona_tr_2_pt_09:


                ; intro
                ; M 1

	db ctrlbt_auto_pwm,%11_01_00_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f

	db ctrlbt_auto_panner,%10_00_10_00,1
        db ctrlbt_transpose,0
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_auto_panner,%00_11_11_00,0
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%00_11_11_00,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_3_pt_00:

                ; Intro

                ; M 1

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80

	db ctrlbt_c_3      
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_c_3      
	db ctrlbt_note_off
	db ctrlbt_c_3      
	db ctrlbt_c_4      
	db ctrlbt_d#4       
	db ctrlbt_g#3        
	db ctrlbt_wait       
	db ctrlbt_g#4
	db ctrlbt_d#3
	db ctrlbt_note_off
	db ctrlbt_d#3
	db ctrlbt_d#4
	db ctrlbt_d#4


        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-----------------------------------------------

missiona_tr_3_pt_01:


                ; M 2

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3     
	db ctrlbt_waveform,$5a
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_waveform,$7c       
	db ctrlbt_f_3     
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4       
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_g#4     
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$5a   
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c#4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c_4
	db ctrlbt_waveform,$7c
	db ctrlbt_f_4


                ; M 3

	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_waveform,$7c       
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4       
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_g#4      
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3       
	db ctrlbt_waveform,$5a
	db ctrlbt_wait        
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_waveform,$5a
	db ctrlbt_c#3
	db ctrlbt_waveform,$7c
	db ctrlbt_c#4
	db ctrlbt_waveform,$5a
	db ctrlbt_c#3
	db ctrlbt_waveform,$7c
	db ctrlbt_c_4
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4


                ; M 4


	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_waveform,$7c       
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4       
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_g#4      
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_waveform,$5a  
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c#4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c_4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#4


                ; M 5


	db ctrlbt_waveform,$7c
	db ctrlbt_d#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_waveform,$7c       
	db ctrlbt_d#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_d#4       
	db ctrlbt_waveform,$7c
	db ctrlbt_d#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_d#4
	db ctrlbt_waveform,$7c      
	db ctrlbt_d_3       
	db ctrlbt_wait
	db ctrlbt_waveform,$5a  
	db ctrlbt_d_3       
	db ctrlbt_wait
	db ctrlbt_waveform,$7c
	db ctrlbt_d_3
	db ctrlbt_waveform,$5a
	db ctrlbt_d_4
	db ctrlbt_waveform,$7c
	db ctrlbt_d_3
	db ctrlbt_waveform,$5a
	db ctrlbt_d_4
	db ctrlbt_waveform,$7c
	db ctrlbt_d#4

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_3_pt_02:

                ; M 6

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3     
	db ctrlbt_waveform,$5a
	db ctrlbt_f_3
	db ctrlbt_waveform,$7c
	db ctrlbt_wait       
	db ctrlbt_f_3     
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4       
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      	
	db ctrlbt_waveform,$5a
	db ctrlbt_g#4     
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_wait        
	db ctrlbt_c#3
	db ctrlbt_waveform,$7c    
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c#4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c_4
	db ctrlbt_waveform,$7c
	db ctrlbt_f_4


                ; M 7


	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_3
	db ctrlbt_waveform,$7c
	db ctrlbt_wait       
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4       
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_g#4      
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_waveform,$5a
	db ctrlbt_c#3       
	db ctrlbt_waveform,$7c
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c#4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c_4
	db ctrlbt_waveform,$7c
	db ctrlbt_f_4


                ; M 8


	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_3
	db ctrlbt_waveform,$7c
	db ctrlbt_wait       
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_f_4       
	db ctrlbt_waveform,$7c
	db ctrlbt_f_3      
	db ctrlbt_waveform,$5a
	db ctrlbt_g#4      
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_waveform,$5a
	db ctrlbt_c#3       
	db ctrlbt_waveform,$7c
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c#4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#3
	db ctrlbt_waveform,$5a
	db ctrlbt_c_4
	db ctrlbt_waveform,$7c
	db ctrlbt_c#4


                ; M 9

	db ctrlbt_waveform,$7c
	db ctrlbt_d#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_d#3
	db ctrlbt_waveform,$7c
	db ctrlbt_wait       
	db ctrlbt_d#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_d#4
	db ctrlbt_waveform,$7c 
	db ctrlbt_d#3      
	db ctrlbt_waveform,$5a
	db ctrlbt_d#4      
	db ctrlbt_waveform,$7c
	db ctrlbt_f#3       
	db ctrlbt_waveform,$5a
	db ctrlbt_wait        
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_waveform,$7c
	db ctrlbt_f#3
	db ctrlbt_waveform,$5a
	db ctrlbt_f#4
	db ctrlbt_waveform,$7c
	db ctrlbt_f#3
	db ctrlbt_waveform,$5a
	db ctrlbt_f#4
	db ctrlbt_waveform,$7c
	db ctrlbt_f#4

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

missiona_tr_3_pt_03:

                ; M 10

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#3     
	db ctrlbt_c#3
	db ctrlbt_c#4      
	db ctrlbt_c#4     
	db ctrlbt_c#3       
	db ctrlbt_c#3      
	db ctrlbt_c#4     
	db ctrlbt_c#4      
	db ctrlbt_d#3        
	db ctrlbt_d#3      
	db ctrlbt_d#4
	db ctrlbt_d#4
	db ctrlbt_d#3
	db ctrlbt_d#3
	db ctrlbt_d#4
	db ctrlbt_d#4

                ; M 11

	db ctrlbt_g#3      
	db ctrlbt_g#3
	db ctrlbt_g#4       
	db ctrlbt_g#4      
	db ctrlbt_g#3       
	db ctrlbt_g#3      
	db ctrlbt_g#4      
	db ctrlbt_g#4       
	db ctrlbt_d#3        
	db ctrlbt_d#3      
	db ctrlbt_d#4
	db ctrlbt_d#4
	db ctrlbt_d#3
	db ctrlbt_d#3
	db ctrlbt_d#4
	db ctrlbt_d#4

                ; M 12

	db ctrlbt_c#3      
	db ctrlbt_c#3
	db ctrlbt_c#4       
	db ctrlbt_c#4     
	db ctrlbt_c#3       
	db ctrlbt_c#3      
	db ctrlbt_c#4      
	db ctrlbt_c#4       
	db ctrlbt_d#3        
	db ctrlbt_d#3       
	db ctrlbt_d#4
	db ctrlbt_d#4
	db ctrlbt_d#3
	db ctrlbt_d#3
	db ctrlbt_d#4
	db ctrlbt_d#4

                ; M 13

	db ctrlbt_g#3      
	db ctrlbt_g#3
	db ctrlbt_g#4       
	db ctrlbt_g#4      
	db ctrlbt_g#3       
	db ctrlbt_g#3      
	db ctrlbt_g#4      
	db ctrlbt_g#4       
	db ctrlbt_a_4        
	db ctrlbt_a_4       
	db ctrlbt_a_4
	db ctrlbt_a_4
	db ctrlbt_a_3
	db ctrlbt_a_3
	db ctrlbt_a_4
	db ctrlbt_a_4

	db ctrlbt_track_loop

;--------------------------------------------------

missiona_tr_3_pt_04:


                ; M 14

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30

	db ctrlbt_c#3     
	db ctrlbt_c#3
	db ctrlbt_c#4      
	db ctrlbt_c#4     
	db ctrlbt_c#3       
	db ctrlbt_c#3      
	db ctrlbt_c#4     
	db ctrlbt_c#4      
	db ctrlbt_d#3        
	db ctrlbt_d#3      
	db ctrlbt_d#4
	db ctrlbt_d#4
	db ctrlbt_d#3
	db ctrlbt_d#3
	db ctrlbt_d#4
	db ctrlbt_d#4

                ; M 15

	db ctrlbt_g#3      
	db ctrlbt_g#3
	db ctrlbt_g#4       
	db ctrlbt_g#4      
	db ctrlbt_g#3       
	db ctrlbt_g#3      
	db ctrlbt_g#4      
	db ctrlbt_g#4       
	db ctrlbt_a_3        
	db ctrlbt_a_3      
	db ctrlbt_a_4
	db ctrlbt_a_4
	db ctrlbt_a_3
	db ctrlbt_a_3
	db ctrlbt_a_4
	db ctrlbt_a_4

                ; M 16

	db ctrlbt_a_3        
	db ctrlbt_a_3      
	db ctrlbt_a_4
	db ctrlbt_a_4
	db ctrlbt_a_3
	db ctrlbt_a_3
	db ctrlbt_a_4
	db ctrlbt_a_4
	db ctrlbt_a_3        
	db ctrlbt_a_3      
	db ctrlbt_a_4
	db ctrlbt_a_4
	db ctrlbt_a_3
	db ctrlbt_a_3
	db ctrlbt_a_4
	db ctrlbt_a_4

                ; M 17

	db ctrlbt_b_3      
	db ctrlbt_b_3
	db ctrlbt_b_4       
	db ctrlbt_b_4      
	db ctrlbt_b_3       
	db ctrlbt_b_3      
	db ctrlbt_b_4    
	db ctrlbt_b_4       
	db ctrlbt_b_3        
	db ctrlbt_b_3       
	db ctrlbt_b_4
	db ctrlbt_b_4
	db ctrlbt_b_3
	db ctrlbt_b_3
	db ctrlbt_b_4
	db ctrlbt_b_4

	db ctrlbt_track_loop

;----------------------------------------------

missiona_tr_3_pt_05:

                ; M 18

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30

	db ctrlbt_c_4
;	db ctrlbt_waveform,$38    
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$30    
	db ctrlbt_wait     
	db ctrlbt_c_4
	db ctrlbt_waveform,$38   
	db ctrlbt_wait      
	db ctrlbt_c_4    
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g#3
;	db ctrlbt_waveform,$38      
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$30
	db ctrlbt_wait

                ; M 19

	db ctrlbt_g_3
;	db ctrlbt_waveform,$38   
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_waveform,$30    
	db ctrlbt_wait     
	db ctrlbt_g_3
	db ctrlbt_waveform,$38 
	db ctrlbt_wait      
	db ctrlbt_g_3    
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_c_4       
;	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$30
	db ctrlbt_wait

                ; M 20

	db ctrlbt_g#3     
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait     
	db ctrlbt_g#3      
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g#3    
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_waveform,$30       
	db ctrlbt_wait      
	db ctrlbt_a#3
;	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait

                ; M 21


	db ctrlbt_c_4
	db ctrlbt_waveform,$30    
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$38    
	db ctrlbt_wait     
	db ctrlbt_c_4      
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_c_4    
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------

missiona_tr_3_pt_06:

                ; M 22

	db ctrlbt_g_3
	db ctrlbt_waveform,$30    
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_waveform,$38
	db ctrlbt_wait     
	db ctrlbt_g_3      
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g_3    
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_c_4       
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$38
	db ctrlbt_wait

                ; M 23

	db ctrlbt_g#3   
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait     
	db ctrlbt_g#3      
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g#3    
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait

                ; M 24

	db ctrlbt_g#3     
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait     
	db ctrlbt_g#3      
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g#3    
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_waveform,$38
	db ctrlbt_wait

                ; M 25

	db ctrlbt_c_4     
	db ctrlbt_waveform,$30
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_waveform,$38    
	db ctrlbt_wait     
	db ctrlbt_c_4      
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_c_4    
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_waveform,$30
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_waveform,$38
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_waveform,$35
	db ctrlbt_g#3
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

missiona_tr_3_pt_07:


                ; M 26

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30

	db ctrlbt_c#3     
	db ctrlbt_wait
	db ctrlbt_c#4     
	db ctrlbt_wait     
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a    
	db ctrlbt_c#4    
	db ctrlbt_wait
	db ctrlbt_waveform,$38   
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_waveform,$3f
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 27

	db ctrlbt_waveform,$30
	db ctrlbt_g#3   
	db ctrlbt_wait
	db ctrlbt_a#3     
	db ctrlbt_wait     
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a
	db ctrlbt_a#3    
	db ctrlbt_waveform,$38
	db ctrlbt_a#3      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_a#3
	db ctrlbt_waveform,$3f
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_waveform,$38
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 28

	db ctrlbt_waveform,$30
	db ctrlbt_c#3    
	db ctrlbt_wait
	db ctrlbt_c#4     
	db ctrlbt_wait     
	db ctrlbt_c#3      
	db ctrlbt_wait      
	db ctrlbt_waveform,$3a
	db ctrlbt_c#4    
	db ctrlbt_wait
	db ctrlbt_waveform,$38
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_waveform,$3f
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 29

	db ctrlbt_waveform,$30
	db ctrlbt_g#3    
	db ctrlbt_wait
	db ctrlbt_a#3     
	db ctrlbt_wait     
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a
	db ctrlbt_a#3    
	db ctrlbt_waveform,$38
	db ctrlbt_a#3      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_waveform,$3d
	db ctrlbt_c_4
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------------

missiona_tr_3_pt_08:

                ; M 30

	db ctrlbt_waveform,$30
	db ctrlbt_c#3     
	db ctrlbt_wait
	db ctrlbt_c#4     
	db ctrlbt_wait     
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a
	db ctrlbt_c#4    
	db ctrlbt_wait
	db ctrlbt_waveform,$38
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_waveform,$3f
	db ctrlbt_d_4
	db ctrlbt_wait


                ; M 2

	db ctrlbt_waveform,$30
	db ctrlbt_d#3  
	db ctrlbt_wait
	db ctrlbt_d#4     
	db ctrlbt_wait     
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a
	db ctrlbt_d#4    
	db ctrlbt_wait
	db ctrlbt_waveform,$38
	db ctrlbt_d#3      
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_waveform,$3f
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 3

	db ctrlbt_waveform,$30
	db ctrlbt_f_3    
	db ctrlbt_wait
	db ctrlbt_f_4     
	db ctrlbt_wait     
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a      
	db ctrlbt_d#4    
	db ctrlbt_wait
	db ctrlbt_waveform,$38
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_waveform,$3f
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 4

	db ctrlbt_waveform,$30
	db ctrlbt_d#3   
	db ctrlbt_wait
	db ctrlbt_f_3     
	db ctrlbt_wait     
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_waveform,$3a
	db ctrlbt_f_3    
	db ctrlbt_waveform,$38
	db ctrlbt_f_3      
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_waveform,$3f
	db ctrlbt_f_3
	db ctrlbt_wait

	db ctrlbt_waveform,$30
	db ctrlbt_track_loop

;-------------------------------------------------

missiona_tr_4_pt_00:

                ; M 1

	db ctrlbt_duration,50
	db ctrlbt_envelope,%0000_0_001

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $2e
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0   
	db $40+$24     
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$f0
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,40
	db $2e
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $3d
	db ctrlbt_volume,$90
	db $3d


                ; M 2


	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $2e
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$e0
	db ctrlbt_duration,50
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $2e
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d

                ; M 3

	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $2e
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$c0   
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0      
	db ctrlbt_duration,50
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $2e
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$90
	db $3d


                ; M 4

	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $2e
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$e0
	db ctrlbt_duration,50
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,40
	db $2e
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,50
	db $3d
	db ctrlbt_volume,$90
	db $3d

	db ctrlbt_track_loop

;-----------------------------------------

missiona_tr_4_pt_01:

                ; M 1

	db ctrlbt_duration,50
	db ctrlbt_envelope,%0000_0_001

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_duration,0
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$f0
	db ctrlbt_duration,50
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d


                ; M 2

	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$c0
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$f0
	db ctrlbt_duration,50
	db $40+$24
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,0
	db $3d


                ; M 3


	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30     
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$f0
	db ctrlbt_duration,50
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$c0
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d

                ; M 4

	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$f0
	db $40+$24            
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$c0
	db ctrlbt_duration,40
	db $40+$24
	db ctrlbt_volume,$90
	db $30       
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d


	db ctrlbt_track_loop

;-----------------------------------------------

missiona_tr_4_pt_02:


                ; M 1

	db ctrlbt_envelope,%0000_0_001

	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$f0
	db $40+$24            
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$c0
	db $40+$24
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d

                ; M 2

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30     
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$f0
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$c0
	db ctrlbt_duration,50
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db $3d

                ; M 3

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$f0
	db $40+$24            
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$c0
	db ctrlbt_duration,50
	db $40+$24
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,0
	db $3d


                ; M 4

	db ctrlbt_duration,50
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$80
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$b0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db ctrlbt_duration,56
	db $3d
	db ctrlbt_volume,$a0
	db ctrlbt_duration,50
	db $40+$24     
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30     
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$f0
	db $40+$24            
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$80
	db ctrlbt_duration,0
	db $3d
	db ctrlbt_volume,$c0
	db ctrlbt_duration,50
	db $30       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$b0
	db $30       
	db ctrlbt_volume,$80
	db $3d


	db ctrlbt_track_loop


;------------------------------------------------

missiona_tr_4_pt_03:


                ; M 1

	db ctrlbt_duration,12
	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$32       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32       
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_duration,12
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24            
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db ctrlbt_duration,12
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33       
	db ctrlbt_wait


                ; M 4

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_duration,12
	db ctrlbt_volume,$b0
	db $40+$32       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24            
	db ctrlbt_wait


	db ctrlbt_track_loop

;---------------------------------------

missiona_tr_4_pt_04:

                ; M 1

	db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_portamento,$00

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
	db ctrlbt_volume,$c0
	db $30
	db ctrlbt_wait
	db ctrlbt_duration,57
	db ctrlbt_volume,$90
	db $30
	db ctrlbt_volume,$c0
	db $30
	db ctrlbt_volume,$d0
	db $30
	db ctrlbt_volume,$a0
	db $30

	db ctrlbt_portamento,$00

	db ctrlbt_track_loop

;----------------------------------------

missiona_tr_4_pt_05:

                ; M 1

	db ctrlbt_duration,12

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_duration,12
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33       
	db ctrlbt_wait


                ; M 2


	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$32       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24            
	db ctrlbt_wait



                ; M 3

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_duration,12
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33       
	db ctrlbt_wait


                ; M 4


	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$33       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$33     
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_duration,12
	db $40+$32       
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$24            
	db ctrlbt_wait


	db ctrlbt_track_loop


;---------------------------------------------


missiona_tr_4_pt_06:

                ; M 1

	db ctrlbt_duration,25
	db ctrlbt_envelope,%0000_0_001

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$b0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$c0
	db $40+$24
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f

                ; M 2

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$a0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$c0
	db $40+$24
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f

                ; M 3

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$24     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$c0
	db $40+$24
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f

                ; M 4

	db ctrlbt_volume,$f0
	db $40+$24     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$a0
	db $36     
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$24            
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$d0
	db $36       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$c0
	db $40+$24
	db ctrlbt_volume,$a0
	db $36       
	db ctrlbt_volume,$b0
	db $36       
	db ctrlbt_volume,$b0
	db $3f

	db ctrlbt_track_loop

missiona_end:

        end

;-----------------------------------------------
