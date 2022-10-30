	;***************************************************************************

orlee_header:
	dw orlee_sequence-orlee_header	; sequence index
	dw orlee_patterns-orlee_header	; patterns index
	dw orlee_end-orlee_header
	db 6

	db 'ORLEE'					; song name
	db $ff

orlee_sequence:

	db ctrlbt_song_speed,$40
	db ctrlbt_panning,%11011011
	db ctrlbt_shuffle,$40,$b3

	db $00
	db $01
	db $02
	db ctrlbt_song_speed,$28
	db ctrlbt_shuffle,$00,$11
	db $03
	db ctrlbt_song_speed,$30
	db ctrlbt_panning,%11111111
	db $04
	db $05

	db ctrlbt_track_loop


orlee_patterns:
	dw orlee_tr_1_pt_04-orlee_patterns,orlee_tr_2_pt_04-orlee_patterns,orlee_tr_3_pt_04-orlee_patterns,orlee_tr_4_pt_04-orlee_patterns
	dw orlee_tr_1_pt_00-orlee_patterns,orlee_tr_2_pt_00-orlee_patterns,orlee_tr_3_pt_00-orlee_patterns,orlee_tr_4_pt_00-orlee_patterns
	dw orlee_tr_1_pt_01-orlee_patterns,orlee_tr_2_pt_01-orlee_patterns,orlee_tr_3_pt_01-orlee_patterns,orlee_tr_4_pt_01-orlee_patterns
	dw orlee_tr_1_pt_02-orlee_patterns,orlee_tr_2_pt_02-orlee_patterns,orlee_tr_3_pt_02-orlee_patterns,orlee_tr_4_pt_02-orlee_patterns
	dw orlee_tr_1_pt_03-orlee_patterns,orlee_tr_2_pt_03-orlee_patterns,orlee_tr_3_pt_03-orlee_patterns,orlee_tr_4_pt_03-orlee_patterns
	dw orlee_tr_1_pt_03-orlee_patterns,orlee_tr_2_pt_03-orlee_patterns,orlee_tr_3_pt_03-orlee_patterns,orlee_tr_4_pt_03-orlee_patterns

;--------------------------------------------------

orlee_tr_1_pt_00:

                ; M 1

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
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
	db ctrlbt_g_3      
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
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20      
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
	db ctrlbt_a#3
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
	db ctrlbt_g_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait  
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 9

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_song_speed,$35
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 10

	db ctrlbt_song_speed,$30
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20      
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
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

orlee_tr_1_pt_01:

                ; M 10.4

	db ctrlbt_song_speed,$40
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_g#3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20      
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
	db ctrlbt_g#3
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
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20 
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
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 17

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111       
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
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

orlee_tr_1_pt_02:

                ; M 20.4

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$20
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_c_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 21

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_c_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 25

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_3      
	db ctrlbt_wait  
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_3
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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------

orlee_tr_1_pt_03:

                ; M 29.4

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

;	db ctrlbt_auto_pwm,%10_10_01_01,1
	db ctrlbt_e_3
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 30

	db ctrlbt_volume,$f0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_volume,$c0   
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 31

	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db ctrlbt_c_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_c_3     
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 32

	db ctrlbt_volume,$f0
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_a_3       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_g_3
	db ctrlbt_wait

                ; M 33

	db ctrlbt_volume,$f0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_g_3
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db ctrlbt_c_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0  
	db ctrlbt_c_3
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 34

	db ctrlbt_volume,$f0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0      
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 35

	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3      
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_c_3   
	db ctrlbt_volume,$c0
	db ctrlbt_c_3    
	db ctrlbt_wait
	db ctrlbt_volume,$f0   
	db ctrlbt_c_3
	db ctrlbt_volume,$c0
	db ctrlbt_c_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0  
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 36

	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0       
	db ctrlbt_c_3        
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait

                ; M 37

	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3       
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0  
	db ctrlbt_c_3        
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 38

	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0  
	db ctrlbt_a#3        
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 39

	db ctrlbt_volume,$f0
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_f_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db ctrlbt_f_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db ctrlbt_f_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0  
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_f_3
	db ctrlbt_wait

;	db ctrlbt_auto_pwm,%10_01_10_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

orlee_tr_1_pt_04:

                ; M 1 new intro

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
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
	db ctrlbt_g_3      
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
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20      
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
	db ctrlbt_a#3
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
	db ctrlbt_g_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait  
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
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
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

orlee_tr_2_pt_00:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_100

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_c_5
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
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_f#4
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
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait  
	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g#4      
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_g#4      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100  
	db ctrlbt_g_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g#4
	db ctrlbt_wait

                ; M 9

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_g#4       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010       
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c_5
	db ctrlbt_wait

                ; M 10

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100       
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

orlee_tr_2_pt_01:

                ; M 10.4

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100

	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_e_5      
	db ctrlbt_wait
	db ctrlbt_d#5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 13

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 14

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_f#5      
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_d#5       
	db ctrlbt_wait  
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_e_5     
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 17

	db ctrlbt_wait
	db ctrlbt_f#6
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_wait
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_wait
	db ctrlbt_d#6       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_6      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

orlee_tr_2_pt_02:

                ; M 20.4

        db ctrlbt_hold_mode,01
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001

	db ctrlbt_auto_panner,%10_11_01_11,1
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 21

	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$f0     
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$b0 
	db ctrlbt_d_4
	db ctrlbt_volume,$f0      
	db ctrlbt_e_4       
	db ctrlbt_volume,$b0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 22

	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_volume,$f0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$b0 
	db ctrlbt_d_4      
	db ctrlbt_volume,$f0
	db ctrlbt_e_4       
	db ctrlbt_volume,$b0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 23

	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_volume,$f0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$b0 
	db ctrlbt_d_4      
	db ctrlbt_volume,$f0
	db ctrlbt_e_4       
	db ctrlbt_volume,$b0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 24

	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_volume,$f0
	db ctrlbt_a_4      
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$b0 
	db ctrlbt_a_4
	db ctrlbt_volume,$f0 
	db ctrlbt_e_4       
	db ctrlbt_volume,$b0
	db ctrlbt_g_4
	db ctrlbt_volume,$f0
	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 25

	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_volume,$f0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$b0 
	db ctrlbt_d_4
	db ctrlbt_volume,$f0      
	db ctrlbt_e_4       
	db ctrlbt_volume,$b0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 26

	db ctrlbt_g_4
	db ctrlbt_volume,$b0
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_volume,$f0    
	db ctrlbt_g_4      
	db ctrlbt_wait
	db ctrlbt_volume,$d0            
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_auto_panner,%10_11_01_11,3
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$d0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f#4       
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
	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_e_4     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_g_4
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
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%10_11_01_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

orlee_tr_2_pt_03:

                ; M 29.4

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$20
        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_011

	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_auto_panner,%10_11_01_11,1
	db ctrlbt_e_5
	db ctrlbt_wait       
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5

                ; M 30

	db ctrlbt_volume,$c0
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5       
	db ctrlbt_volume,$70
	db ctrlbt_g#5      
	db ctrlbt_volume,$c0
	db ctrlbt_g#5       
	db ctrlbt_volume,$70
	db ctrlbt_e_5      
	db ctrlbt_volume,$90
	db ctrlbt_f#5      
	db ctrlbt_volume,$70
	db ctrlbt_g#5
	db ctrlbt_volume,$c0       
	db ctrlbt_e_5
	db ctrlbt_volume,$70      
	db ctrlbt_f#5       
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5

                ; M 31

	db ctrlbt_volume,$c0
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5       
	db ctrlbt_volume,$70
	db ctrlbt_g#5      
	db ctrlbt_volume,$c0
	db ctrlbt_g#5       
	db ctrlbt_volume,$70
	db ctrlbt_e_5      
	db ctrlbt_volume,$90
	db ctrlbt_f#5      
	db ctrlbt_volume,$70
	db ctrlbt_g#5       
	db ctrlbt_volume,$c0
	db ctrlbt_e_5      
	db ctrlbt_volume,$70
	db ctrlbt_f#5       
	db ctrlbt_volume,$90
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5
	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_g#5
	db ctrlbt_volume,$90
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5

                ; M 32

	db ctrlbt_volume,$c0
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5       
	db ctrlbt_volume,$70
	db ctrlbt_g#5      
	db ctrlbt_volume,$c0
	db ctrlbt_g#5       
	db ctrlbt_volume,$70
	db ctrlbt_e_5      
	db ctrlbt_volume,$90
	db ctrlbt_f#5      
	db ctrlbt_volume,$70
	db ctrlbt_g#5       
	db ctrlbt_volume,$c0
	db ctrlbt_e_5      
	db ctrlbt_volume,$70
	db ctrlbt_f#5       
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_volume,$90
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5

                ; M 33

	db ctrlbt_volume,$c0
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5       
	db ctrlbt_volume,$70
	db ctrlbt_g#5      
	db ctrlbt_volume,$c0
	db ctrlbt_g#5       
	db ctrlbt_volume,$70
	db ctrlbt_e_5      
	db ctrlbt_volume,$90
	db ctrlbt_a_5      
	db ctrlbt_volume,$70
	db ctrlbt_g#5       
	db ctrlbt_volume,$c0
	db ctrlbt_g#5      
	db ctrlbt_volume,$70
	db ctrlbt_a_5       
	db ctrlbt_volume,$90
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_g#5
	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5

                ; M 34

	db ctrlbt_volume,$c0
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5       
	db ctrlbt_volume,$70
	db ctrlbt_g#5      
	db ctrlbt_volume,$c0
	db ctrlbt_a_5       
	db ctrlbt_volume,$70
	db ctrlbt_e_5      
	db ctrlbt_volume,$90
	db ctrlbt_g#5      
	db ctrlbt_volume,$70
	db ctrlbt_a_5       
	db ctrlbt_volume,$c0
	db ctrlbt_f#5      
	db ctrlbt_volume,$70
	db ctrlbt_g#5       
	db ctrlbt_volume,$90
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_e_5
	db ctrlbt_volume,$90
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5

                ; M 35

	db ctrlbt_volume,$c0
	db ctrlbt_g_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5       
	db ctrlbt_volume,$70
	db ctrlbt_g_5      
	db ctrlbt_volume,$c0
	db ctrlbt_f#5       
	db ctrlbt_volume,$70
	db ctrlbt_e_5      
	db ctrlbt_volume,$90
	db ctrlbt_g_5      
	db ctrlbt_volume,$70
	db ctrlbt_f#5       
	db ctrlbt_volume,$c0
	db ctrlbt_e_5      
	db ctrlbt_volume,$70
	db ctrlbt_g_5       
	db ctrlbt_volume,$90
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5
	db ctrlbt_volume,$c0
	db ctrlbt_a_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$90
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_a_5

                ; M 36

	db ctrlbt_volume,$c0
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5
	db ctrlbt_volume,$90
	db ctrlbt_a_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5      
	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_a_5      
	db ctrlbt_volume,$90
	db ctrlbt_f#5      
	db ctrlbt_volume,$70
	db ctrlbt_e_5       
	db ctrlbt_volume,$c0
	db ctrlbt_b_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5       
	db ctrlbt_volume,$90
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_b_5
	db ctrlbt_volume,$c0
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5
	db ctrlbt_volume,$90
	db ctrlbt_b_5
	db ctrlbt_volume,$70
	db ctrlbt_f#5

                ; M 37

	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_volume,$70
	db ctrlbt_b_5
	db ctrlbt_volume,$90
	db ctrlbt_f#5
	db ctrlbt_volume,$70
	db ctrlbt_e_5
	db ctrlbt_volume,$c0
	db ctrlbt_d_6       
	db ctrlbt_volume,$70
	db ctrlbt_f#5      
	db ctrlbt_volume,$90
	db ctrlbt_e_5     
	db ctrlbt_volume,$70
	db ctrlbt_d_6       
	db ctrlbt_volume,$c0
	db ctrlbt_f#5
	db ctrlbt_volume,$70 
	db ctrlbt_e_5       
	db ctrlbt_volume,$90
	db ctrlbt_d_6
	db ctrlbt_volume,$70
	db ctrlbt_f#5
	db ctrlbt_volume,$c0
	db ctrlbt_f_5
	db ctrlbt_volume,$70
	db ctrlbt_d_6
	db ctrlbt_volume,$90
	db ctrlbt_a#5
	db ctrlbt_volume,$70
	db ctrlbt_f_5

                ; M 38

	db ctrlbt_volume,$c0
	db ctrlbt_c_6
	db ctrlbt_volume,$70
	db ctrlbt_a#5
	db ctrlbt_volume,$90
	db ctrlbt_a#5       
	db ctrlbt_volume,$70
	db ctrlbt_c_6      
	db ctrlbt_volume,$c0
	db ctrlbt_d_6
	db ctrlbt_volume,$70
	db ctrlbt_a#5      
	db ctrlbt_volume,$90
	db ctrlbt_c_6      
	db ctrlbt_volume,$70
	db ctrlbt_d_6       
	db ctrlbt_volume,$c0
	db ctrlbt_a#5
	db ctrlbt_volume,$70
	db ctrlbt_c_6       
	db ctrlbt_volume,$90
	db ctrlbt_c_6
	db ctrlbt_volume,$70
	db ctrlbt_a#5
	db ctrlbt_volume,$c0
	db ctrlbt_a#5
	db ctrlbt_volume,$70
	db ctrlbt_c_6
	db ctrlbt_volume,$90
	db ctrlbt_a_5
	db ctrlbt_volume,$70
	db ctrlbt_a#5

                ; M 39

	db ctrlbt_volume,$c0
	db ctrlbt_c_6
	db ctrlbt_volume,$70
	db ctrlbt_a_5
	db ctrlbt_volume,$90
	db ctrlbt_d_6       
	db ctrlbt_volume,$70
	db ctrlbt_c_6      
	db ctrlbt_volume,$c0
	db ctrlbt_a_5       
	db ctrlbt_volume,$70
	db ctrlbt_d_6      
	db ctrlbt_volume,$90
	db ctrlbt_g_5      
	db ctrlbt_volume,$70
	db ctrlbt_c_6       
	db ctrlbt_volume,$c0
	db ctrlbt_a_5
	db ctrlbt_volume,$70
	db ctrlbt_g_5
	db ctrlbt_volume,$90
	db ctrlbt_g_5
	db ctrlbt_volume,$70
	db ctrlbt_a_5
	db ctrlbt_volume,$c0
	db ctrlbt_c_6
	db ctrlbt_volume,$70
	db ctrlbt_g_5       
	db ctrlbt_volume,$90
	db ctrlbt_d_6
	db ctrlbt_volume,$70
	db ctrlbt_g_5

	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------

orlee_tr_2_pt_04:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_100

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_c_5
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
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_f#4
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
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait  
	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

orlee_tr_3_pt_00:

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
	db ctrlbt_gain_waveform,$08
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

                ; M 1

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_note_off       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$10
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
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
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_a#5
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_gain_waveform,$08
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00     
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 5

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_gain_waveform,$08
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a#5
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
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

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
	db ctrlbt_c_6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_g#5       
	db ctrlbt_wait      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_g_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait

                ; M 9

	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_g#5       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 10

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
	db ctrlbt_g_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

orlee_tr_3_pt_01:

                ; M 10.4

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00



	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait

                ; M 11

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
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_d#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5      
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait

                ; M 12

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_e_5       
	db ctrlbt_wait      
	db ctrlbt_d#5
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 13

	db ctrlbt_b_5
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait

                ; M 14

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
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_5        
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait

                ; M 15

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_c#5
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait

                ; M 16

	db ctrlbt_e_5
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 17

	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
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
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

orlee_tr_3_pt_02:

                ; M 20.4

        db ctrlbt_transpose,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00


	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 21

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
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
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 25

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
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

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------------

orlee_tr_3_pt_03:

                ; M 29.4

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
	db ctrlbt_gain_waveform,$0a
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00


	db ctrlbt_g#5
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 30

	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6
	db ctrlbt_wait      
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_a_5     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 31

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_c_6      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_d_6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_5     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 33

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 34

	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c#6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_5     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

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
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_e_6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
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
	db ctrlbt_g_6
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

                ; M 38

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_a#6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_c_6     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 39

	db ctrlbt_a_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a_6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

orlee_tr_3_pt_04:

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82
	db ctrlbt_gain_waveform,$08
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

                ; M 1

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_note_off       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_gain_waveform,$08
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00     
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 5

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
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
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a#5
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
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

orlee_tr_4_pt_00:

                ; M 1

	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 2

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,0
	db $3c
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 3

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 4

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 5

        db ctrlbt_duration,0
	db $3c
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,59
	db $3c
	db $3c

                ; M 6

        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 7

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,59
	db $3c
	db $3c

                ; M 8

        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 9

	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       

                ; M 10

        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $33
	db ctrlbt_wait       
	db $33
	db ctrlbt_wait       

	db ctrlbt_track_loop

;----------------------------------------------

orlee_tr_4_pt_01:

                ; M 10.4


	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_duration,40

	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait       
        db ctrlbt_duration,50
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_volume,$80
	db $3c

                ; M 11

	db ctrlbt_volume,$a0
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3c
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_volume,$c0
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,0
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_wait

                ; M 12

	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait      
        db ctrlbt_duration,50
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_volume,$80
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait

                ; M 13

	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_wait
	db ctrlbt_volume,$80
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$23
	db ctrlbt_wait
        db ctrlbt_duration,50
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait

                ; M 14

	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait      
        db ctrlbt_duration,50
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_wait       
	db ctrlbt_volume,$90
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3c
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_volume,$90
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_volume,$90
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait

                ; M 16

	db ctrlbt_volume,$c0
        db ctrlbt_duration,40
	db $40+$2c
	db ctrlbt_wait
	db ctrlbt_volume,$80
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,40
	db $40+$2c
        db ctrlbt_duration,50
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
        db ctrlbt_duration,40
	db $40+$2c
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait

                ; M 17

	db ctrlbt_volume,$90
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
        db ctrlbt_duration,50
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait

                ; M 18

	db ctrlbt_volume,$b0
        db ctrlbt_duration,50
	db $3c
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,40
	db $40+$23
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $40+$23
	db ctrlbt_wait      
        db ctrlbt_duration,50
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_volume,$90
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait
        db ctrlbt_duration,50
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_volume,$80
	db $3c

                ; M 19

	db ctrlbt_volume,$c0
        db ctrlbt_duration,40
	db $40+$2c
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2c
        db ctrlbt_duration,50
	db ctrlbt_volume,$80
	db $3c
	db ctrlbt_volume,$c0
	db $3c
	db ctrlbt_volume,$80
	db $3c
        db ctrlbt_duration,40
	db ctrlbt_volume,$c0
	db $40+$2c
	db ctrlbt_volume,$80
	db $3c
        db ctrlbt_duration,0
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

;--------------------------------------------

orlee_tr_4_pt_02:

                ; M 20.4


        db ctrlbt_hold_mode,01
        db ctrlbt_duration,55
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait

                ; M 21

	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_wait

                ; M 22

	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait

                ; M 23

	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait     
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_wait

                ; M 24

	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait

                ; M 25

	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_wait

                ; M 26

	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$2a
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $40+$35
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $40+$35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 27

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait

                ; M 28

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait

                ; M 29

	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $35 
	db ctrlbt_volume,$70
	db $35 
	db ctrlbt_volume,$c0
	db $35 
	db ctrlbt_volume,$80
	db $35 
	db ctrlbt_volume,$c0
	db $35 
	db ctrlbt_volume,$70
	db $35 
	db ctrlbt_volume,$c0
	db $35 
	db ctrlbt_volume,$80
	db $35 

	db ctrlbt_track_loop

;-------------------------------------------

orlee_tr_4_pt_03:

                ; M 29.4

        db ctrlbt_duration,48

	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f 

                ; M 30

	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$80
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$80
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 31

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 32

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$90 
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35 
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 33

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$d0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f 

                ; M 34

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $35
	db ctrlbt_wait

                ; M 35

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 36

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 37

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 38

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait

                ; M 39

	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $35
	db ctrlbt_volume,$b0
	db $35
	db ctrlbt_volume,$f0
	db $3f 
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $35
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------

orlee_tr_4_pt_04:

                ; M 1

	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 2

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,0
	db $3c
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 3

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 4

	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

                ; M 5

        db ctrlbt_duration,0
	db $3c
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait
        db ctrlbt_duration,59
	db $3c
	db $3c

                ; M 6

        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait      
	db $3c
	db ctrlbt_wait       
	db $3c
	db ctrlbt_wait       
        db ctrlbt_duration,59
	db $3c
	db $3c
	db $3c
        db ctrlbt_duration,55
	db ctrlbt_wait
	db $3c
	db ctrlbt_wait

	db ctrlbt_track_loop

orlee_end:

        end


;-----------------------------------------------
