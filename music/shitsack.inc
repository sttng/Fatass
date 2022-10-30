;***************************************************************************

shitsack_header:
	dw shitsack_sequence-shitsack_header	; sequence index
	dw shitsack_patterns-shitsack_header	; patterns index
	dw shitsack_end-shitsack_header
	db 20
	db 'SHITSACK'					; song name
	db $ff

shitsack_sequence:

	db ctrlbt_panning,%11111111
	db ctrlbt_song_speed,$20

	db $00
	db $01
	db $02
	db $03
	db $04
	db $05
	db ctrlbt_panning,%11011110
	db $06
	db $07
	db $08
	db $09
	db ctrlbt_panning,%11111111
	db $0a
	db $0b
	db $0c
	db $0d
	db $0e
	db $0f
	db ctrlbt_panning,%11011110
	db $10
	db $11
	db $12
	db $13





	db ctrlbt_track_loop


shitsack_patterns:
	dw shitsack_tr_1_pt_01-shitsack_patterns,shitsack_tr_2_pt_02-shitsack_patterns,shitsack_tr_3_pt_05-shitsack_patterns,shitsack_tr_4_pt_05-shitsack_patterns
	dw shitsack_tr_1_pt_04-shitsack_patterns,shitsack_tr_2_pt_05-shitsack_patterns,shitsack_tr_3_pt_04-shitsack_patterns,shitsack_tr_4_pt_04-shitsack_patterns
	dw shitsack_tr_1_pt_00-shitsack_patterns,shitsack_tr_2_pt_02-shitsack_patterns,shitsack_tr_3_pt_00-shitsack_patterns,shitsack_tr_4_pt_00-shitsack_patterns
	dw shitsack_tr_1_pt_00-shitsack_patterns,shitsack_tr_2_pt_00-shitsack_patterns,shitsack_tr_3_pt_00-shitsack_patterns,shitsack_tr_4_pt_00-shitsack_patterns
	dw shitsack_tr_1_pt_00-shitsack_patterns,shitsack_tr_2_pt_00-shitsack_patterns,shitsack_tr_3_pt_00-shitsack_patterns,shitsack_tr_4_pt_00-shitsack_patterns
	dw shitsack_tr_1_pt_01-shitsack_patterns,shitsack_tr_2_pt_01-shitsack_patterns,shitsack_tr_3_pt_01-shitsack_patterns,shitsack_tr_4_pt_01-shitsack_patterns
	dw shitsack_tr_1_pt_02-shitsack_patterns,shitsack_tr_2_pt_03-shitsack_patterns,shitsack_tr_3_pt_02-shitsack_patterns,shitsack_tr_4_pt_02-shitsack_patterns
	dw shitsack_tr_1_pt_03-shitsack_patterns,shitsack_tr_2_pt_04-shitsack_patterns,shitsack_tr_3_pt_03-shitsack_patterns,shitsack_tr_4_pt_03-shitsack_patterns
	dw shitsack_tr_1_pt_02-shitsack_patterns,shitsack_tr_2_pt_03-shitsack_patterns,shitsack_tr_3_pt_02-shitsack_patterns,shitsack_tr_4_pt_02-shitsack_patterns
	dw shitsack_tr_1_pt_03-shitsack_patterns,shitsack_tr_2_pt_04-shitsack_patterns,shitsack_tr_3_pt_03-shitsack_patterns,shitsack_tr_4_pt_03-shitsack_patterns
	dw shitsack_tr_1_pt_00-shitsack_patterns,shitsack_tr_2_pt_00-shitsack_patterns,shitsack_tr_3_pt_00-shitsack_patterns,shitsack_tr_4_pt_00-shitsack_patterns
	dw shitsack_tr_1_pt_00-shitsack_patterns,shitsack_tr_2_pt_00-shitsack_patterns,shitsack_tr_3_pt_00-shitsack_patterns,shitsack_tr_4_pt_00-shitsack_patterns
	dw shitsack_tr_1_pt_01-shitsack_patterns,shitsack_tr_2_pt_01-shitsack_patterns,shitsack_tr_3_pt_01-shitsack_patterns,shitsack_tr_4_pt_01-shitsack_patterns
	dw shitsack_tr_1_pt_01-shitsack_patterns,shitsack_tr_2_pt_02-shitsack_patterns,shitsack_tr_3_pt_05-shitsack_patterns,shitsack_tr_4_pt_05-shitsack_patterns
	dw shitsack_tr_1_pt_01-shitsack_patterns,shitsack_tr_2_pt_02-shitsack_patterns,shitsack_tr_3_pt_05-shitsack_patterns,shitsack_tr_4_pt_05-shitsack_patterns
	dw shitsack_tr_1_pt_01-shitsack_patterns,shitsack_tr_2_pt_01-shitsack_patterns,shitsack_tr_3_pt_01-shitsack_patterns,shitsack_tr_4_pt_01-shitsack_patterns
	dw shitsack_tr_1_pt_02-shitsack_patterns,shitsack_tr_2_pt_03-shitsack_patterns,shitsack_tr_3_pt_02-shitsack_patterns,shitsack_tr_4_pt_02-shitsack_patterns
	dw shitsack_tr_1_pt_03-shitsack_patterns,shitsack_tr_2_pt_04-shitsack_patterns,shitsack_tr_3_pt_03-shitsack_patterns,shitsack_tr_4_pt_03-shitsack_patterns
	dw shitsack_tr_1_pt_02-shitsack_patterns,shitsack_tr_2_pt_03-shitsack_patterns,shitsack_tr_3_pt_02-shitsack_patterns,shitsack_tr_4_pt_02-shitsack_patterns
	dw shitsack_tr_1_pt_03-shitsack_patterns,shitsack_tr_2_pt_04-shitsack_patterns,shitsack_tr_3_pt_03-shitsack_patterns,shitsack_tr_4_pt_03-shitsack_patterns

;--------------------------------------------------

shitsack_tr_1_pt_00:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_transpose,00

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
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3           
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3         
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

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
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3                 
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af
	db ctrlbt_duration,47
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3         
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$af   
	db ctrlbt_c_3       
	db ctrlbt_wait    
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

shitsack_tr_1_pt_01:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_transpose,3

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_6
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait     	     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_6
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3     
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3           
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_6
	db ctrlbt_portamento,$3f
	db ctrlbt_duration,5
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_duration,40
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_6
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3     
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_6
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_5
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c_4
	db ctrlbt_portamento,$3f   
	db ctrlbt_c_3

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

shitsack_tr_1_pt_02:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,10
        db ctrlbt_transpose,0
	db ctrlbt_duration,0
 
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 2

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

shitsack_tr_1_pt_03:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,10
        db ctrlbt_transpose,3


	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 2

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

shitsack_tr_1_pt_04:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$3f
        db ctrlbt_transpose,12

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_b_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 5

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c_5      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_c_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 9

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_d_5      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_d_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

shitsack_tr_2_pt_00:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$3f
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,24

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_auto_pwm,%00_11_11_01,0       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait


                ; M 2

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait
        db ctrlbt_transpose,36
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_auto_pwm,%00_11_11_01,1
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$3f
	db ctrlbt_auto_pwm,%00_11_11_01,0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_transpose,24
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_auto_pwm,%00_11_11_01,1
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_c_3       
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
	db ctrlbt_auto_pwm,%00_11_11_01,0      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

shitsack_tr_2_pt_01:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$3f
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,27

	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_auto_pwm,%00_11_11_01,0       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait


                ; M 2

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait
        db ctrlbt_transpose,39
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_auto_pwm,%00_11_11_01,1
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_transpose,27
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$3f
	db ctrlbt_auto_pwm,%00_11_11_01,0
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_auto_pwm,%00_11_11_01,1
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_c_3       
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
	db ctrlbt_auto_pwm,%00_11_11_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

shitsack_tr_2_pt_02:

                ; M 1  Blank

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,15

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

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

shitsack_tr_2_pt_03:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 2

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------------

shitsack_tr_2_pt_04:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,3

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 2

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d#3      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait      
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

shitsack_tr_2_pt_05:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12


	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 2

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_d#4     
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait

                ; M 4

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_c_4      
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 5

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait      
	db ctrlbt_a#3      
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait       
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 6

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait      
	db ctrlbt_g#3      
	db ctrlbt_wait       
	db ctrlbt_c_4       
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait

                ; M 7

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_d#4       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 8

	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_d#4      
	db ctrlbt_wait      
	db ctrlbt_g#4      
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_g#4       
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 9

	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_f_4      
	db ctrlbt_wait      
	db ctrlbt_a#4      
	db ctrlbt_wait      
	db ctrlbt_f_4
	db ctrlbt_wait       
	db ctrlbt_a#4       
	db ctrlbt_wait       
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait


	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

shitsack_tr_3_pt_00:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_gain_waveform,$28
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12

	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_c_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_a#3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_d#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_d#3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_a#3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_c_3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_d#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_c_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_a#3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_d#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_a#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_c_3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_g_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_a#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_g_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

shitsack_tr_3_pt_01:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,15

	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_c_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_a#3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_d#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_d#3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_a#3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_c_3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_d#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_c_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_a#3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_d#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_c_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_a#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a0
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a1
	db ctrlbt_d#3      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a3
	db ctrlbt_c_3       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a5
	db ctrlbt_c_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a6
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a7
	db ctrlbt_c_3        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$a9
	db ctrlbt_g_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ab
	db ctrlbt_a#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ac
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ad
	db ctrlbt_g_3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ae
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------------

shitsack_tr_3_pt_02:

                ; M 1  

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$29
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$16
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_f_6     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait

                ; M 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$18
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a#5
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$19
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

shitsack_tr_3_pt_03:

                ; M 1  

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$28
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,3

	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$16
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 2

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_f_6     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait

                ; M 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$18
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a#5
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
	db ctrlbt_vibrato,$01,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-----------------------------------------------

shitsack_tr_3_pt_04:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
        db ctrlbt_transpose,12

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_5      
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_d#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_a#4       
	db ctrlbt_wait      
	db ctrlbt_a_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g#4       
	db ctrlbt_g_4       
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_f_4
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_f_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_d#4       
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 5

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3      
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 6

	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_a#4       
	db ctrlbt_wait      
	db ctrlbt_g#4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_g#4       
	db ctrlbt_wait
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3      
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 8

	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_g#4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_g#4       
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 9

	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_f_4      
	db ctrlbt_wait      
	db ctrlbt_a#4
	db ctrlbt_wait       
	db ctrlbt_a#4       
	db ctrlbt_wait       
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

shitsack_tr_3_pt_05:

                ; M 1 Intro

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8f
	db ctrlbt_gain_waveform,$29
        db ctrlbt_transpose,0

	db ctrlbt_d#4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$aa
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$15
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$40
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$6f
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$35
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$58
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$c8
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$7a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8f
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$45
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------

shitsack_tr_4_pt_00:

                ; M 1

        db ctrlbt_duration,37

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,00
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f

                ; M 2

        db ctrlbt_duration,37       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f

                ; M 3

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f

                ; M 4

        db ctrlbt_duration,37       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,37       
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f

	db ctrlbt_track_loop

;----------------------------------------------

shitsack_tr_4_pt_01:

                ; M 1

        db ctrlbt_duration,37       

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$b0
	db $40+$33
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,00
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f

                ; M 2

        db ctrlbt_duration,37       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$b0
	db $40+$33
	db ctrlbt_volume,$a0
	db $3f

                ; M 3

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,37       
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$b0
	db $40+$33
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f

                ; M 4

        db ctrlbt_duration,37       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
        db ctrlbt_duration,37       
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$2f
	db ctrlbt_volume,$a0
	db $3f

	db ctrlbt_track_loop

;---------------------------------------------

shitsack_tr_4_pt_02:

                ; M 1

        db ctrlbt_duration,58

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$c0
        db ctrlbt_duration,58
	db $40+$37
	db ctrlbt_volume,$b0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

                ; M 2

        db ctrlbt_duration,58
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

                ; M 3

        db ctrlbt_duration,58
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$c0
        db ctrlbt_duration,58 
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

                ; M 4

        db ctrlbt_duration,58
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

	db ctrlbt_track_loop

;------------------------------------------------

shitsack_tr_4_pt_03:

                ; M 1

        db ctrlbt_duration,58

	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$80
	db $40+$37
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$c0
        db ctrlbt_duration,58
	db $40+$37
	db ctrlbt_volume,$b0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

                ; M 2

        db ctrlbt_duration,58
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$d0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,58
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

                ; M 3

        db ctrlbt_duration,58
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_volume,$c0
        db ctrlbt_duration,58 
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f

                ; M 4

        db ctrlbt_duration,58
	db ctrlbt_volume,$f0
	db $40+$25
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$25
	db ctrlbt_volume,$a0
	db $40+$25
	db ctrlbt_volume,$c0
	db $40+$37
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $40+$37
	db ctrlbt_volume,$a0
	db $40+$37

	db ctrlbt_track_loop

;------------------------------------------------

shitsack_tr_4_pt_04:

                ; M 1

        db ctrlbt_duration,62

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d

                ; M 4

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d      
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db $3d

                ; M 5

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d

                ; M 6

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d

                ; M 7

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d

                ; M 8

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$80
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$a0
	db $3d

                ; M 9

	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$f0
	db $3d
	db ctrlbt_volume,$90
	db $3d
	db ctrlbt_volume,$d0
	db $3d
	db ctrlbt_volume,$90
	db $3d


        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;----------------------------------------

shitsack_tr_4_pt_05:

                ; M 1

        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00      
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_volume,$d0
	db $40+$21
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$24
	db ctrlbt_volume,$c0
	db $3d
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,01
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

shitsack_end:

        end

