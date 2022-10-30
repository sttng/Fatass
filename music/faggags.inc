;***************************************************************************

faggags_header:
	dw faggags_sequence-faggags_header	; sequence index
	dw faggags_patterns-faggags_header	; patterns index
	dw faggags_end-faggags_header	; patterns index
	db 26

	db 'FAGGAGS'					; song name
	db $ff

faggags_sequence:

	db ctrlbt_panning,%11111111
	db ctrlbt_song_speed,$34
	db ctrlbt_shuffle,$00,$00
	db $00
	db ctrlbt_song_speed,$35
	db ctrlbt_shuffle,$40,$33
	db $01
	db ctrlbt_song_speed,$36
	db ctrlbt_shuffle,$40,$53
	db $02
	db $03
	db $04
	db ctrlbt_shuffle,$40,$53
	db ctrlbt_song_speed,$37
	db $05
	db $06
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$40
	db $07
	db $08
	db $09
	db $0a
	db $0b
	db $0c
	db ctrlbt_song_speed,$38
	db ctrlbt_shuffle,$40,$73
	db $0d
	db ctrlbt_song_speed,$34
	db ctrlbt_shuffle,$00,$00
	db $0e
	db $0f
	db $10
	db ctrlbt_song_speed,$36
	db ctrlbt_shuffle,$40,$53
	db $11
	db $12
	db $13
	db ctrlbt_shuffle,$40,$53
	db ctrlbt_song_speed,$37
	db $14
	db $15
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$40
	db $16
	db $17
	db $18
	db $19


	db ctrlbt_track_loop


faggags_patterns:
	dw faggags_tr_1_pt_07-faggags_patterns,faggags_tr_2_pt_11-faggags_patterns,faggags_tr_3_pt_06-faggags_patterns,faggags_tr_4_pt_10-faggags_patterns
	dw faggags_tr_1_pt_00-faggags_patterns,faggags_tr_2_pt_00-faggags_patterns,faggags_tr_3_pt_00-faggags_patterns,faggags_tr_4_pt_00-faggags_patterns
	dw faggags_tr_1_pt_01-faggags_patterns,faggags_tr_2_pt_01-faggags_patterns,faggags_tr_3_pt_00-faggags_patterns,faggags_tr_4_pt_07-faggags_patterns
	dw faggags_tr_1_pt_00-faggags_patterns,faggags_tr_2_pt_00-faggags_patterns,faggags_tr_3_pt_07-faggags_patterns,faggags_tr_4_pt_00-faggags_patterns
	dw faggags_tr_1_pt_01-faggags_patterns,faggags_tr_2_pt_01-faggags_patterns,faggags_tr_3_pt_08-faggags_patterns,faggags_tr_4_pt_07-faggags_patterns
	dw faggags_tr_1_pt_02-faggags_patterns,faggags_tr_2_pt_02-faggags_patterns,faggags_tr_3_pt_05-faggags_patterns,faggags_tr_4_pt_01-faggags_patterns
	dw faggags_tr_1_pt_02-faggags_patterns,faggags_tr_2_pt_09-faggags_patterns,faggags_tr_3_pt_10-faggags_patterns,faggags_tr_4_pt_08-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_03-faggags_patterns,faggags_tr_3_pt_13-faggags_patterns,faggags_tr_4_pt_03-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_04-faggags_patterns,faggags_tr_3_pt_14-faggags_patterns,faggags_tr_4_pt_11-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_03-faggags_patterns,faggags_tr_3_pt_11-faggags_patterns,faggags_tr_4_pt_03-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_04-faggags_patterns,faggags_tr_3_pt_12-faggags_patterns,faggags_tr_4_pt_11-faggags_patterns
	dw faggags_tr_1_pt_04-faggags_patterns,faggags_tr_2_pt_06-faggags_patterns,faggags_tr_3_pt_04-faggags_patterns,faggags_tr_4_pt_04-faggags_patterns
	dw faggags_tr_1_pt_05-faggags_patterns,faggags_tr_2_pt_05-faggags_patterns,faggags_tr_3_pt_05-faggags_patterns,faggags_tr_4_pt_06-faggags_patterns
	dw faggags_tr_1_pt_06-faggags_patterns,faggags_tr_2_pt_10-faggags_patterns,faggags_tr_3_pt_05-faggags_patterns,faggags_tr_4_pt_06-faggags_patterns
	dw faggags_tr_1_pt_00-faggags_patterns,faggags_tr_2_pt_07-faggags_patterns,faggags_tr_3_pt_00-faggags_patterns,faggags_tr_4_pt_00-faggags_patterns
	dw faggags_tr_1_pt_07-faggags_patterns,faggags_tr_2_pt_08-faggags_patterns,faggags_tr_3_pt_00-faggags_patterns,faggags_tr_4_pt_07-faggags_patterns
	dw faggags_tr_1_pt_07-faggags_patterns,faggags_tr_2_pt_11-faggags_patterns,faggags_tr_3_pt_00-faggags_patterns,faggags_tr_4_pt_10-faggags_patterns
	dw faggags_tr_1_pt_00-faggags_patterns,faggags_tr_2_pt_00-faggags_patterns,faggags_tr_3_pt_05-faggags_patterns,faggags_tr_4_pt_10-faggags_patterns
	dw faggags_tr_1_pt_00-faggags_patterns,faggags_tr_2_pt_00-faggags_patterns,faggags_tr_3_pt_07-faggags_patterns,faggags_tr_4_pt_00-faggags_patterns
	dw faggags_tr_1_pt_01-faggags_patterns,faggags_tr_2_pt_01-faggags_patterns,faggags_tr_3_pt_08-faggags_patterns,faggags_tr_4_pt_07-faggags_patterns
	dw faggags_tr_1_pt_02-faggags_patterns,faggags_tr_2_pt_02-faggags_patterns,faggags_tr_3_pt_09-faggags_patterns,faggags_tr_4_pt_01-faggags_patterns
	dw faggags_tr_1_pt_02-faggags_patterns,faggags_tr_2_pt_09-faggags_patterns,faggags_tr_3_pt_10-faggags_patterns,faggags_tr_4_pt_08-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_03-faggags_patterns,faggags_tr_3_pt_11-faggags_patterns,faggags_tr_4_pt_03-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_04-faggags_patterns,faggags_tr_3_pt_12-faggags_patterns,faggags_tr_4_pt_11-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_03-faggags_patterns,faggags_tr_3_pt_11-faggags_patterns,faggags_tr_4_pt_03-faggags_patterns
	dw faggags_tr_1_pt_03-faggags_patterns,faggags_tr_2_pt_04-faggags_patterns,faggags_tr_3_pt_12-faggags_patterns,faggags_tr_4_pt_11-faggags_patterns



;--------------------------------------------------

faggags_tr_1_pt_00:

                ; M 1

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001 
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_a_3
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_g_3
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_f_3
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_e_3
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

faggags_tr_1_pt_01:

                ; M 1

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

faggags_tr_1_pt_02:

                ; M 1

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope

	db ctrlbt_volume,$d0

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_f_3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_001      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_f_3      
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111      
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 3

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_001      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_f_3      
	db ctrlbt_wait       
	db ctrlbt_g#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111     
	db ctrlbt_f_3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

faggags_tr_1_pt_03:

                ; M 1

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
        db ctrlbt_hold_mode,01

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_d#4    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_d#4       
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#3     
	db ctrlbt_wait
	db ctrlbt_c#4      
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
	db ctrlbt_c_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait


                ; M 3


	db ctrlbt_c_4     
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 4

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_d#3      
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_d#4        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

faggags_tr_1_pt_04:

                ; M 1

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
        db ctrlbt_hold_mode,00

	db ctrlbt_arpeggio,$0c,$7c,3
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_d#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,00      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
        db ctrlbt_hold_mode,01
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$7c,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

faggags_tr_1_pt_05:

                ; M 1
                ; 5 meas's long

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
;        db ctrlbt_hold_mode,00

	db ctrlbt_arpeggio,$0c,$7c,3
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011    
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110  
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,01       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_d#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 5

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110

	db ctrlbt_arpeggio,$00,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_1_pt_06:

                ; M 3  (3 meas's long)

	db ctrlbt_song_speed,$37
	db ctrlbt_arpeggio,$0c,$7c,3
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110

	db ctrlbt_d#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 5

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110

	db ctrlbt_arpeggio,$00,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------

faggags_tr_1_pt_07:

                ; M 1

	db ctrlbt_pulse_width,%10_000000
;	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
;        db ctrlbt_hold_mode,00


	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait


                ; M 2


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait


                ; M 3


;	db ctrlbt_song_speed,$35
	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_2_pt_00:

                ; M 1

        db ctrlbt_hold_mode,01

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_001

;        db ctrlbt_vibrato_reset

	db ctrlbt_f#4     
	db ctrlbt_wait
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#5      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01       
	db ctrlbt_f#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01
	db ctrlbt_a_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 3

	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_f#5      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01     
	db ctrlbt_f#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_5      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01       
	db ctrlbt_a_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_c_5
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_2_pt_01:

                ; M 1

        db ctrlbt_hold_mode,01

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_001

;        db ctrlbt_vibrato_reset

	db ctrlbt_f_4     
	db ctrlbt_wait
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f_5     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01              
	db ctrlbt_f_6        
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_f_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_f_5      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01  
	db ctrlbt_f_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 3

	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#4        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01       
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,01
	db ctrlbt_a_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_c_5
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_2_pt_02:

                ; M 1

        db ctrlbt_hold_mode,00

	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$40
	db ctrlbt_a_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_c_5      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$90
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#4
	db ctrlbt_portamento,$40
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_volume,$90
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_4
	db ctrlbt_portamento,$3c
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

            ; M 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$a0
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$40
	db ctrlbt_a_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000      
	db ctrlbt_c_5      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_volume,$90
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_volume,$a0
	db ctrlbt_pulse_width,%11_000000
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#4
	db ctrlbt_portamento,$40
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_volume,$90
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_5
	db ctrlbt_portamento,$3c      
	db ctrlbt_d#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_portamento,$00
	db ctrlbt_auto_pwm,%10_00_11_01,00

	db ctrlbt_track_loop

;--------------------------------------------------

faggags_tr_2_pt_03:

                ; M 1

        db ctrlbt_hold_mode,00

;	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a#6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2

	db ctrlbt_auto_pwm,%00_01_10_11,01
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
	db ctrlbt_auto_pwm,%00_01_10_11,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3


	db ctrlbt_g#6      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


	db ctrlbt_auto_pwm,%00_01_10_11,01
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
	db ctrlbt_auto_pwm,%00_01_10_11,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------------

faggags_tr_2_pt_04:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111

	db ctrlbt_c#7     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_7       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_auto_pwm,%00_01_10_11,01
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

	db ctrlbt_auto_pwm,%00_01_10_11,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g#6      
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
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_d#7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c#7      
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
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;-------------------------------------------------

faggags_tr_2_pt_05:

                ; M 1
                ; 5 meas's long

	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope

	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
;        db ctrlbt_hold_mode,00


	db ctrlbt_arpeggio,$0c,$7c,3
	db ctrlbt_fine_tune,3
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011    
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110  
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2


        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3


	db ctrlbt_fine_tune,20
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_d#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 5


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_fine_tune,00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-------------------------------------------------

faggags_tr_2_pt_06:


                ; M 1

	db ctrlbt_arpeggio,$0c,$7c,3
	db ctrlbt_pulse_width,%11_000000
;	db ctrlbt_restart_envelope

	db ctrlbt_volume,$d0

        db ctrlbt_hold_mode,00


	db ctrlbt_fine_tune,3
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_d#3
	db ctrlbt_fine_tune,4
	db ctrlbt_wait
	db ctrlbt_fine_tune,5
	db ctrlbt_wait       
	db ctrlbt_fine_tune,6
	db ctrlbt_wait      
	db ctrlbt_fine_tune,7
	db ctrlbt_wait       
	db ctrlbt_fine_tune,8
	db ctrlbt_wait      
	db ctrlbt_fine_tune,9
	db ctrlbt_wait     
	db ctrlbt_fine_tune,10
	db ctrlbt_wait
	db ctrlbt_fine_tune,3
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3
	db ctrlbt_fine_tune,4    
	db ctrlbt_wait       
	db ctrlbt_fine_tune,5
	db ctrlbt_wait
	db ctrlbt_fine_tune,6
	db ctrlbt_wait
	db ctrlbt_fine_tune,7
	db ctrlbt_wait
	db ctrlbt_fine_tune,8
	db ctrlbt_wait
	db ctrlbt_fine_tune,9
	db ctrlbt_wait
	db ctrlbt_fine_tune,10
	db ctrlbt_wait


                ; M 3


	db ctrlbt_fine_tune,3
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
        db ctrlbt_hold_mode,00      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a#3
	db ctrlbt_fine_tune,4
	db ctrlbt_wait
	db ctrlbt_fine_tune,5
	db ctrlbt_wait      
	db ctrlbt_fine_tune,6
	db ctrlbt_wait      
	db ctrlbt_fine_tune,7
	db ctrlbt_wait      
	db ctrlbt_fine_tune,8
	db ctrlbt_wait      
	db ctrlbt_fine_tune,9
	db ctrlbt_wait     
	db ctrlbt_fine_tune,10
	db ctrlbt_wait
	db ctrlbt_fine_tune,3
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$7c,0
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-------------------------------------------------
;--------------------------------------------------

faggags_tr_2_pt_07:


                ; M 1


        db ctrlbt_hold_mode,00

	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_000
	
	db ctrlbt_c_6    
	db ctrlbt_wait
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
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_pwm,%00_01_10_11,01     
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_b_5
	db ctrlbt_wait

                ; M 2


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
	db ctrlbt_auto_pwm,%00_01_10_11,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 3

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
	db ctrlbt_auto_pwm,%00_01_10_11,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_b_5
	db ctrlbt_wait


                ; M 4


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
	db ctrlbt_auto_pwm,%00_01_10_11,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_e_6
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;------------------------------------------------

faggags_tr_2_pt_08:


                ; M 1


        db ctrlbt_hold_mode,00

	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_000

	db ctrlbt_c_6    
	db ctrlbt_wait
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
	db ctrlbt_auto_pwm,%00_01_10_11,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 2


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
	db ctrlbt_auto_pwm,%00_01_10_11,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_10_11,00
	db ctrlbt_g#5
	db ctrlbt_wait

                ; M 3

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
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a_5
	db ctrlbt_wait


                ; M 4


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
	db ctrlbt_vibrato,$06,$3f        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_2_pt_09:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_111

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$40
	db ctrlbt_a_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_c_5      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_volume,$90
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$a0
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#4
	db ctrlbt_portamento,$40
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 2


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_volume,$90
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_4
	db ctrlbt_portamento,$3c
	db ctrlbt_d#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

            ; M 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$a0
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$40
	db ctrlbt_a_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%10_000000      
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_a#5      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_volume,$90
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$a0
	db ctrlbt_auto_pwm,%00_01_10_11,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_5
	db ctrlbt_portamento,$40
	db ctrlbt_a#5
	db ctrlbt_wait


                ; M 4


	db ctrlbt_d#6      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_volume,$90
	db ctrlbt_auto_pwm,%00_01_10_11,1
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_6
	db ctrlbt_portamento,$3c      
	db ctrlbt_g#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_portamento,$00
	db ctrlbt_auto_pwm,%10_00_11_01,0
	db ctrlbt_track_loop

;--------------------------------------------------

faggags_tr_2_pt_10:

                ; M 3

	db ctrlbt_fine_tune,20
	db ctrlbt_arpeggio,$0c,$7c,3
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_d#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_hold_mode,01       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_a#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 5


	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_fine_tune,00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-------------------------------------------------

faggags_tr_2_pt_11:

                ; M 1 Blank

        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_000

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
	db ctrlbt_track_loop

;------------------------------------------------

faggags_tr_3_pt_00:


                ; M 1


;	db ctrlbt_sample,$01
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
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomb
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2



;	db ctrlbt_sample,$01
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
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomb
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3


;	db ctrlbt_sample,$01
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
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,toma
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


;	db ctrlbt_sample,$01
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
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,toma
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop


;-----------------------------------------------

;--------------------------------------------------


faggags_tr_3_pt_01:


                ; M 1


;	db ctrlbt_sample,$01
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$0d             
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomb
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2



;	db ctrlbt_sample,$01
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$0d             
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomb
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3


;	db ctrlbt_sample,$01
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$0d             
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,toma
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


;	db ctrlbt_sample,$01
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
;	db ctrlbt_sample,$0d     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$0d       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,tomc   
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,toma
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop


;-----------------------------------------------
;-----------------------------------------------

faggags_tr_3_pt_04:



                ; M 1

;	db ctrlbt_restart_envelope
;	db ctrlbt_gain_waveform,$00

;	db ctrlbt_sample,$01
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait           
	db ctrlbt_wait  
	db ctrlbt_wait       
	db ctrlbt_wait
;	db ctrlbt_sample,$0d
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,$01
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2



	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$0d
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$01      
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,$0d
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3



	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
;	db ctrlbt_sample,$01      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
;	db ctrlbt_sample,$0d            
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


;	db ctrlbt_sample,$01
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
;	db ctrlbt_sample,$0d           
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,$0d      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_gain_waveform,$00

	db ctrlbt_track_loop



;-----------------------------------------------

faggags_tr_3_pt_05:

                ; M 1

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

                ; M 5

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

	db ctrlbt_track_loop

;-----------------------------------------------

faggags_tr_3_pt_06:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$05
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3a

	db ctrlbt_d_4
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait

                ; M 2


	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$05
	db ctrlbt_d_4
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$05
	db ctrlbt_a_7
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$06
	db ctrlbt_a_6
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$06
	db ctrlbt_d_6
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$06
	db ctrlbt_a_5
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_d_5
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;---------------------------------------

faggags_tr_3_pt_07:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$0d
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_a_6      
	db ctrlbt_wait
	db ctrlbt_c#7       
	db ctrlbt_wait
	db ctrlbt_d_7       
	db ctrlbt_e_7      
	db ctrlbt_d_7      
	db ctrlbt_wait       
	db ctrlbt_e_7        
	db ctrlbt_wait       
	db ctrlbt_f#7
	db ctrlbt_wait
	db ctrlbt_g_7
	db ctrlbt_wait
	db ctrlbt_a_7
	db ctrlbt_g_7

                ; M 2


	db ctrlbt_f#7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$0d
	db ctrlbt_a_7        
	db ctrlbt_wait       
	db ctrlbt_g_7
	db ctrlbt_wait
	db ctrlbt_e_7
	db ctrlbt_wait
	db ctrlbt_d_7
	db ctrlbt_e_7

                ; M 3

	db ctrlbt_a_7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$0d
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#7       
	db ctrlbt_wait      
	db ctrlbt_e_7      
	db ctrlbt_wait       
	db ctrlbt_a_7        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;---------------------------------------------

faggags_tr_3_pt_08:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$0d

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_f_7
	db ctrlbt_wait
	db ctrlbt_e_7       
	db ctrlbt_wait
	db ctrlbt_d_7      
	db ctrlbt_wait      
	db ctrlbt_c_7     
	db ctrlbt_wait       
	db ctrlbt_d_7        
	db ctrlbt_wait       
	db ctrlbt_e_7
	db ctrlbt_wait
	db ctrlbt_f_7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2


	db ctrlbt_a#7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$0d
	db ctrlbt_f_7
	db ctrlbt_wait       
	db ctrlbt_e_7
	db ctrlbt_wait
	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_e_7
	db ctrlbt_f_7

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0d
	db ctrlbt_a_7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$0d
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#7       
	db ctrlbt_wait      
	db ctrlbt_e_7      
	db ctrlbt_wait       
	db ctrlbt_a_7        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------------

faggags_tr_3_pt_09:

                ; M 1

        db ctrlbt_transpose,-12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$f0
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_a_7
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_g#7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_7
	db ctrlbt_wait

                ; M 2


	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_c_7
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_a_7
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_c_7        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_7
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$01
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_d#7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_3_pt_10:

                ; M 1
       
        db ctrlbt_transpose,-12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$f0
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$20

	db ctrlbt_c_7
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_g#7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_7
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_c_7
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_a_7
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_c_7        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_7
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_d#7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;---------------------------------------------

faggags_tr_3_pt_11:

                ; M 1

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_f_5    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_f_5
	db ctrlbt_wait

                ; M 2


	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0      
	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0      
	db ctrlbt_f_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_f_5
	db ctrlbt_wait

                ; M 4

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_a#5      
	db ctrlbt_wait      
	db ctrlbt_g_5     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0       
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;-----------------------------------------------

faggags_tr_3_pt_12:

                ; M 1

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_gain_waveform,$0b
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0 
	db ctrlbt_f_5    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_f_5
	db ctrlbt_wait

                ; M 2


	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0      
	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_g#5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e1
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e2
	db ctrlbt_a#5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e7
	db ctrlbt_c_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e9
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ea
	db ctrlbt_a#5      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$eb
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ec
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ed       
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ec
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$eb
	db ctrlbt_c_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ea
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e9
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_a#5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e7
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e6
	db ctrlbt_g#5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e3
	db ctrlbt_a#5      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e1
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e0
	db ctrlbt_g#5    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e1
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e2
	db ctrlbt_g_5       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e3
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e4
	db ctrlbt_f_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e6
	db ctrlbt_g_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e7
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e8
	db ctrlbt_f_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$e9
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;------------------------------------------------

faggags_tr_3_pt_13:

                ; M 1

        db ctrlbt_hold_mode,01
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$05
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#6     
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_note_off  
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_c#6
	db ctrlbt_wait

                ; M 2

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_c#6
	db ctrlbt_wait      
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_c#6
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_d#6
	db ctrlbt_wait

                ; M 3

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_g#6      
	db ctrlbt_wait
	db ctrlbt_note_off       
	db ctrlbt_wait      
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_note_off  
	db ctrlbt_wait       
	db ctrlbt_a#6        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_d#6
	db ctrlbt_wait

                ; M 4

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_d#6
	db ctrlbt_wait      
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_d#6
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$7c,0
	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------

faggags_tr_3_pt_14:

                ; M 1

	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_hold_mode,01
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$05

	db ctrlbt_arpeggio,$0c,$0c,1
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#7     
	db ctrlbt_wait
	db ctrlbt_note_off       
	db ctrlbt_wait      
	db ctrlbt_c_7       
	db ctrlbt_wait      
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_c#6
	db ctrlbt_wait


                ; M 2

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_c#6
	db ctrlbt_wait      
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_c#6
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait

                ; M 3


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_g#6      
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_a#6       
	db ctrlbt_wait      
	db ctrlbt_note_off  
	db ctrlbt_wait       
	db ctrlbt_c#7        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_d#7
	db ctrlbt_wait


                ; M 4

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_c#7      
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_a#6       
	db ctrlbt_wait      
	db ctrlbt_note_off  
	db ctrlbt_wait       
	db ctrlbt_g#6        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_f#6
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$0c,0
	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;-----------------------------------------

faggags_tr_4_pt_00:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_duration,55
	db ctrlbt_volume,$f0     
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f       
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2


	db ctrlbt_volume,$f0
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_011
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,55
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,40       
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_duration,30
	db ctrlbt_volume,$90
	db $3d       
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $39       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,55
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f       
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_011
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0     
        db ctrlbt_duration,55
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,40       
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_track_loop

;-------------------------------------------------

faggags_tr_4_pt_01:

                ; M 1

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,30
	db ctrlbt_volume,$c0   
	db ctrlbt_envelope,%0000_0_010
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_010
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_010
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_4_pt_02:

                ; M 1

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2c        
	db ctrlbt_wait
	db ctrlbt_volume,$c0   
	db ctrlbt_envelope,%0000_0_010
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001   
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_011
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_001
	db $40+$28
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$f0
	db $40+$24        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_011
	db $3f
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$24       
	db ctrlbt_wait
	db ctrlbt_volume,$c0   
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$a0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_001   
	db $40+$28
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait


                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

faggags_tr_4_pt_03:

                ; M 1
                ; 5 meas's

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$a0      
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$a0      
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 5

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$a0      
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   	
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait


	db ctrlbt_track_loop

;------------------------------------------------

faggags_tr_4_pt_04:

                ; M 1

	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$b0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f               
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$b0
	db $40+$24        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$28
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
	db $3f
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$24       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$c0
	db $40+$28
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait


                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$d0      
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$a0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait


	db ctrlbt_track_loop

;------------------------------------------------

faggags_tr_4_pt_05:


                ; M 1
                ; 5 meas's

	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0      
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$f0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,40       
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 5

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

faggags_tr_4_pt_06:

                ; M 1

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$b0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f               
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$b0
	db $40+$24        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$28
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
	db $3f
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$24       
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db $3f        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$c0
	db $40+$28
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_volume,$d0      
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$a0
	db $3f        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait

                ; M 5

	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$d0
	db $3f        
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f               
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$b0
	db $40+$24        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$28
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

faggags_tr_4_pt_07:

                ; M 1

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_duration,55
	db ctrlbt_volume,$f0     
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f       
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2


	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_010
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,55
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,40       
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
	db $3f
	db ctrlbt_wait              
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_010
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,55
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f       
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$24        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_010
	db $40+$20        
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0     
        db ctrlbt_duration,45
	db ctrlbt_envelope,%0000_0_010
	db $40+$20   
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$24   
	db ctrlbt_wait
        db ctrlbt_duration,40       
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_011
	db $40+$2c
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------

faggags_tr_4_pt_08:

                ; M 1

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
        db ctrlbt_duration,30
	db ctrlbt_volume,$c0   
	db ctrlbt_envelope,%0000_0_010
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$e0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_011
	db $40+$28
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_010
	db $40+$28
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,30
	db ctrlbt_envelope,%0000_0_010
	db $40+$28
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $40+$2c
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db $40+$2c
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_011
	db $40+$30
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

faggags_tr_4_pt_09:

                ; M 1 Blank

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

	db ctrlbt_track_loop

;----------------------------------------------

faggags_tr_4_pt_10:

                ; M 1 

	db ctrlbt_auto_panner,%11_10_11_01,0

	db ctrlbt_volume,$a0
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_011
	db $40+$3b
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$30
	db ctrlbt_volume,$a0
	db $40+$3b
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$3b
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$a0
	db $40+$3b
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$30
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $40+$3b
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_auto_panner,%11_00_11_00,1
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_000
	db $2c
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111       
	db $2c
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
	db $40+$38
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,40
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$90
	db $40+$38
	db ctrlbt_wait       
	db ctrlbt_volume,$70
	db $40+$38
	db ctrlbt_wait
	db ctrlbt_volume,$50
	db $40+$38
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db $40+$38
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_track_loop

;--------------------------------------------------

faggags_tr_4_pt_11:

                ; M 1
                ; 5 meas's

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait
	db ctrlbt_volume,$a0      
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f       
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db $40+$28        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
	db $40+$24
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_duration,56
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_duration,50
	db ctrlbt_volume,$d0
	db $30        
	db ctrlbt_volume,$b0
	db $30        
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_volume,$c0
	db $30        
	db ctrlbt_volume,$b0
	db $30        
	db ctrlbt_volume,$d0
	db $30        
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $30        
	db ctrlbt_wait


	db ctrlbt_track_loop

faggags_end:

        end

;-----------------------------------------------
