;***************************************************************************

journey_header:
	dw journey_sequence-journey_header	; sequence index
	dw journey_patterns-journey_header	; patterns index
	dw journey_end-journey_header
	db 35

	db 'JOURNEY'					; song name
	db $ff

journey_sequence:


	db ctrlbt_shuffle,$00,$00
	db ctrlbt_panning,%11011110
	db ctrlbt_song_speed,$40

	db $00
	db $01
	db $02
	db $03
	db $04
	db $05
	db $06
	db ctrlbt_song_speed,$40
	db ctrlbt_shuffle,$60,$c3
	db $07
	db $08
	db $09
	db $0a
	db $0b
	db $0c
	db $0d
	db $0e
	db $0f
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
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$40
	db $1d
	db $1e
	db $1f
	db $20
	db $21
	db $22

	db ctrlbt_track_loop


journey_patterns:
	dw journey_tr_1_pt_10-journey_patterns,journey_tr_2_pt_10-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_11-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_00-journey_patterns,journey_tr_4_pt_00-journey_patterns
	dw journey_tr_1_pt_01-journey_patterns,journey_tr_2_pt_01-journey_patterns,journey_tr_3_pt_00-journey_patterns,journey_tr_4_pt_00-journey_patterns
	dw journey_tr_1_pt_02-journey_patterns,journey_tr_2_pt_02-journey_patterns,journey_tr_3_pt_01-journey_patterns,journey_tr_4_pt_01-journey_patterns
	dw journey_tr_1_pt_03-journey_patterns,journey_tr_2_pt_03-journey_patterns,journey_tr_3_pt_02-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_03-journey_patterns,journey_tr_2_pt_03-journey_patterns,journey_tr_3_pt_03-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_04-journey_patterns,journey_tr_2_pt_04-journey_patterns,journey_tr_3_pt_04-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_05-journey_patterns,journey_tr_2_pt_05-journey_patterns,journey_tr_3_pt_05-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_06-journey_patterns,journey_tr_2_pt_06-journey_patterns,journey_tr_3_pt_06-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_05-journey_patterns,journey_tr_2_pt_05-journey_patterns,journey_tr_3_pt_07-journey_patterns,journey_tr_4_pt_04-journey_patterns
	dw journey_tr_1_pt_07-journey_patterns,journey_tr_2_pt_07-journey_patterns,journey_tr_3_pt_08-journey_patterns,journey_tr_4_pt_05-journey_patterns
	dw journey_tr_1_pt_08-journey_patterns,journey_tr_2_pt_08-journey_patterns,journey_tr_3_pt_09-journey_patterns,journey_tr_4_pt_06-journey_patterns
	dw journey_tr_1_pt_09-journey_patterns,journey_tr_2_pt_09-journey_patterns,journey_tr_3_pt_10-journey_patterns,journey_tr_4_pt_07-journey_patterns
	dw journey_tr_1_pt_03-journey_patterns,journey_tr_2_pt_03-journey_patterns,journey_tr_3_pt_14-journey_patterns,journey_tr_4_pt_09-journey_patterns
	dw journey_tr_1_pt_04-journey_patterns,journey_tr_2_pt_04-journey_patterns,journey_tr_3_pt_14-journey_patterns,journey_tr_4_pt_09-journey_patterns
	dw journey_tr_1_pt_03-journey_patterns,journey_tr_2_pt_03-journey_patterns,journey_tr_3_pt_02-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_04-journey_patterns,journey_tr_2_pt_04-journey_patterns,journey_tr_3_pt_04-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_04-journey_patterns,journey_tr_2_pt_04-journey_patterns,journey_tr_3_pt_04-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_05-journey_patterns,journey_tr_2_pt_05-journey_patterns,journey_tr_3_pt_05-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_06-journey_patterns,journey_tr_2_pt_06-journey_patterns,journey_tr_3_pt_06-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_05-journey_patterns,journey_tr_2_pt_05-journey_patterns,journey_tr_3_pt_07-journey_patterns,journey_tr_4_pt_04-journey_patterns
	dw journey_tr_1_pt_07-journey_patterns,journey_tr_2_pt_07-journey_patterns,journey_tr_3_pt_08-journey_patterns,journey_tr_4_pt_05-journey_patterns
	dw journey_tr_1_pt_08-journey_patterns,journey_tr_2_pt_08-journey_patterns,journey_tr_3_pt_09-journey_patterns,journey_tr_4_pt_06-journey_patterns
	dw journey_tr_1_pt_09-journey_patterns,journey_tr_2_pt_09-journey_patterns,journey_tr_3_pt_10-journey_patterns,journey_tr_4_pt_07-journey_patterns
	dw journey_tr_1_pt_03-journey_patterns,journey_tr_2_pt_03-journey_patterns,journey_tr_3_pt_14-journey_patterns,journey_tr_4_pt_09-journey_patterns
	dw journey_tr_1_pt_12-journey_patterns,journey_tr_2_pt_04-journey_patterns,journey_tr_3_pt_14-journey_patterns,journey_tr_4_pt_09-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_00-journey_patterns,journey_tr_2_pt_00-journey_patterns,journey_tr_3_pt_11-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_11-journey_patterns,journey_tr_2_pt_11-journey_patterns,journey_tr_3_pt_11-journey_patterns,journey_tr_4_pt_03-journey_patterns
	dw journey_tr_1_pt_13-journey_patterns,journey_tr_2_pt_13-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_02-journey_patterns
	dw journey_tr_1_pt_13-journey_patterns,journey_tr_2_pt_13-journey_patterns,journey_tr_3_pt_12-journey_patterns,journey_tr_4_pt_03-journey_patterns

;--------------------------------------------------


journey_tr_1_pt_00:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00

                ; M 1

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_1_pt_01:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 5

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 6

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 7

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

journey_tr_1_pt_02:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 9

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 10

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 11

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 12

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_song_speed,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait
;	db ctrlbt_song_speed,$38      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait
;	db ctrlbt_song_speed,$37  
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

journey_tr_1_pt_03:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                  
                ; M 13/17

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14/18

	db ctrlbt_arpeggio,$c0,$c0,0                  
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15/19

	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16/20

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
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

;-------------------------------------------------

journey_tr_1_pt_04:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 21

	db ctrlbt_arpeggio,$00,$c0,0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_arpeggio,$00,$c0,0
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5             
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------

journey_tr_1_pt_05:

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 25/33

;	db ctrlbt_arpeggio,$c0,$c0,3             
;	db ctrlbt_auto_pwm,%00_00_11_00,7
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011     
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26/34

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27/35

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011 
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28/36

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_auto_pwm,%11_00_11_00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------


journey_tr_1_pt_06:

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 29

;	db ctrlbt_arpeggio,$c0,$c0,3             
;	db ctrlbt_auto_pwm,%00_00_11_00,7
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 31

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0  
	db ctrlbt_auto_pwm,%11_00_11_00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_1_pt_07:

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 37

;	db ctrlbt_arpeggio,$c0,$c0,3
;	db ctrlbt_auto_pwm,%00_00_11_00,7
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011   
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 39

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 40

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_1_pt_08:

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 41

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_e_4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 42

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 43

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 44

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 45

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_f#4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 46

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 47

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 48

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

journey_tr_1_pt_09:

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 49

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_e_4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 50

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 51

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 52

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 53

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011    
	db ctrlbt_f#4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 54

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 55

;	db ctrlbt_envelope,%0000_0_100
;	db ctrlbt_e_4
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_envelope,%0000_0_011
;	db ctrlbt_f#4
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_envelope,%0000_0_001       
;	db ctrlbt_c#4
;	db ctrlbt_wait

                ; M 56

;	db ctrlbt_envelope,%0000_0_100
;	db ctrlbt_e_4
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_b_3    
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

journey_tr_1_pt_10:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00

                ; M 1

	db ctrlbt_volume,$10
	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_volume,$10      
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_volume,$20
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_volume,$30
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$40
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_volume,$40
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_volume,$50
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_volume,$60
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$60
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_volume,$70
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$90
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_volume,$a0
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0   
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_1_pt_11:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00

                ; M 1

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_volume,$d0
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$b0
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_volume,$a0
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_volume,$90
  	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_volume,$80
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait
	db ctrlbt_volume,$50
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

journey_tr_1_pt_12:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 21

	db ctrlbt_arpeggio,$00,$c0,0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_arpeggio,$00,$c0,0
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_song_speed,$30
	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5             
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_1_pt_13:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                  
                ; M 13/17

	db ctrlbt_arpeggio,$c0,$c0,0
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

                ; M 14/18

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15/19

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16/20

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
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


;--------------------------------------------------

journey_tr_2_pt_00:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,6
        db ctrlbt_transpose,00

                ; M 1

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_2_pt_01:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,6
        db ctrlbt_transpose,0

                ; M 5

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 6

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 7

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

journey_tr_2_pt_02:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,6
        db ctrlbt_transpose,0

                ; M 9

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 10

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 11

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 12

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

journey_tr_2_pt_03:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,16
        db ctrlbt_transpose,0


                ; M 13/17

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14/18

	db ctrlbt_arpeggio,$c0,$c0,0                  
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15/19

	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16/20

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
	db ctrlbt_auto_pwm,%00_01_10_11,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-------------------------------------------------

journey_tr_2_pt_04:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,16
        db ctrlbt_transpose,0

                ; M 21

	db ctrlbt_arpeggio,$00,$c0,0
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_arpeggio,$00,$c0,0
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5                   
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_arpeggio,$c0,$c0,5             
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
	db ctrlbt_arpeggio,$c0,$c0,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop



;-------------------------------------------------

journey_tr_2_pt_05:

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,2
        db ctrlbt_transpose,0

                ; M 25/33

;	db ctrlbt_arpeggio,$c0,$c0,3
;	db ctrlbt_auto_pwm,%00_00_11_00,7
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26/34

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27/35

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28/36

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_d_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------


journey_tr_2_pt_06:

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,2
        db ctrlbt_transpose,0

                ; M 29

;	db ctrlbt_arpeggio,$c0,$c0,3
;	db ctrlbt_auto_pwm,%00_00_11_00,7
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 31

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------


journey_tr_2_pt_07:

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,2
        db ctrlbt_transpose,0

                ; M 37

;	db ctrlbt_arpeggio,$c0,$c0,3
;	db ctrlbt_auto_pwm,%00_00_11_00,7
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011  
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 39

	db ctrlbt_arpeggio,$c0,$c0,0
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_fine_tune,8
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 40

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

journey_tr_2_pt_08:

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,8
        db ctrlbt_transpose,0

                ; M 41

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_e_4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 42

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 43

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 44

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 45

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_f#4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 46

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 47

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 48

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

journey_tr_2_pt_09:

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,8
        db ctrlbt_transpose,0

                ; M 49

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_e_4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 50

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 51

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 52

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 53

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_f#4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001   
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 54

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 55

;	db ctrlbt_envelope,%0000_0_100
;	db ctrlbt_e_4
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_wait
;	db ctrlbt_envelope,%0000_0_011      
;	db ctrlbt_f#4
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_envelope,%0000_0_001       
;	db ctrlbt_c#4
;	db ctrlbt_wait

                ; M 56

;	db ctrlbt_envelope,%0000_0_100
;	db ctrlbt_e_4
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_wait      
;	db ctrlbt_b_3    
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_2_pt_10:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$e0
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,6
        db ctrlbt_transpose,00

                ; M 1

	db ctrlbt_volume,$10
	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_volume,$10      
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_volume,$20
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_volume,$30
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$40
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_volume,$40
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_volume,$50
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_volume,$60
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_volume,$60
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_volume,$70
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$90
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_volume,$a0
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0   
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait


        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_2_pt_11:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,6
        db ctrlbt_transpose,00


                ; M 1

	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_d_3       
	db ctrlbt_volume,$d0
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 2

	db ctrlbt_volume,$b0
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d_3
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_volume,$a0
	db ctrlbt_wait      
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_volume,$90
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 3

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_volume,$90
  	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_f#3      
	db ctrlbt_volume,$80
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait
	db ctrlbt_volume,$50
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_d_3
	db ctrlbt_wait

	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

journey_tr_2_pt_13:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                  
                ; M 13/17

	db ctrlbt_arpeggio,$c0,$c0,0
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

                ; M 14/18

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15/19

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16/20

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
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



;--------------------------------------------------

journey_tr_3_pt_00:

                ; M 1-4 & 5-8

        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,01
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c

	db ctrlbt_a#4     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2/6

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3/7

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4/8

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_f#4        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

journey_tr_3_pt_01:

                ; M 9

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
        db ctrlbt_transpose,12
	db ctrlbt_a#4     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 10

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$09
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_gain_waveform,$0c
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$25      
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

journey_tr_3_pt_02:

                ; M 13


	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05
        db ctrlbt_transpose,0
	db ctrlbt_b_5
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
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$04             
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 14

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05
	db ctrlbt_e_5
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04              
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03              
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02                     
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$05             
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_a_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
	db ctrlbt_b_5
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
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$04             
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 16

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80       
	db ctrlbt_a_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

journey_tr_3_pt_03:

                ; M 17

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05
        db ctrlbt_transpose,0
	db ctrlbt_b_5    
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
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$04             
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80      
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 18

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_a_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
	db ctrlbt_b_5
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
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$04             
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 20

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80       
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

journey_tr_3_pt_04:

                ; M 21

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
        db ctrlbt_transpose,0
	db ctrlbt_b_5    
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
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$04             
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 22

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85      
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80       
	db ctrlbt_a_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
	db ctrlbt_b_5
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
	db ctrlbt_waveform,$85      
	db ctrlbt_gain_waveform,$04             
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 24

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_gain_waveform,$05              
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85      
	db ctrlbt_g_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
	db ctrlbt_a_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------


journey_tr_3_pt_05:


                ; M 25


        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$07
        db ctrlbt_transpose,12
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00  
	db ctrlbt_b_5     
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 26


	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a_5      
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 27


	db ctrlbt_a_5
	db ctrlbt_wait
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_3_pt_06:

                ; M 29

        db ctrlbt_transpose,12
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a_5     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 31

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_b_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_c#6        
	db ctrlbt_wait       
	db ctrlbt_d_6
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

journey_tr_3_pt_07:

                ; M 33

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_5     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 34

	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_b_5
	db ctrlbt_wait
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a_5
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
	db ctrlbt_f#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

journey_tr_3_pt_08:

                ; M 37

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20      
	db ctrlbt_wait     
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a_5       
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait

                ; M 38

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 39

        db ctrlbt_transpose,24
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 40

	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_c#6      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_3_pt_09:

                ; M 41

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
        db ctrlbt_transpose,24

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00            
	db ctrlbt_b_5    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_b_4
	db ctrlbt_wait

                ; M 42

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_g#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 43

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_a_4
	db ctrlbt_wait

                ; M 44

	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00            
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#4
	db ctrlbt_wait

                ; M 45

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00            
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_f#4
	db ctrlbt_wait

                ; M 46

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_g#4
	db ctrlbt_wait      
	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_b_4        
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait

                ; M 47

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00            
	db ctrlbt_d_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 48

	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00            
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

journey_tr_3_pt_10:

                ; M 49

        db ctrlbt_transpose,24
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00      

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_5     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_b_4
	db ctrlbt_wait

                ; M 50

	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00            
	db ctrlbt_c#6      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait

                ; M 51

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d_6     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 52

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_e_6      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait

                ; M 53

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#6    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 54

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_g#5       
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_e_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00

                ; M 55

;	db ctrlbt_e_5
;	db ctrlbt_wait
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;       db ctrlbt_vibrato_reset
;	db ctrlbt_vibrato,$02,$20      
;	db ctrlbt_wait     
;	db ctrlbt_wait
;	db ctrlbt_wait       
;	db ctrlbt_wait       
;	db ctrlbt_wait
;	db ctrlbt_wait

                ; M 56

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


	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_3_pt_11:

                ; M 1

        db ctrlbt_transpose,0

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_gain_waveform,$0a
	db ctrlbt_arpeggio,$00,$0c,3
        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait       
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

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$55
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
        db ctrlbt_transpose,12
	db ctrlbt_arpeggio,$07,$0c,1
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$0c,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

journey_tr_3_pt_12:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_gain_waveform,$08
	db ctrlbt_arpeggio,$00,$0c,3
        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20

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

                ; M 2

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20
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

                ; M 3

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$24
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

                ; M 4

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$0c,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

journey_tr_3_pt_13:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_gain_waveform,$05
	db ctrlbt_arpeggio,$00,$c0,5
        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20

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

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$0c,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------


journey_tr_3_pt_14:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_gain_waveform,$09
	db ctrlbt_portamento,$3f
	db ctrlbt_arpeggio,$00,$c0,5
        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$02,$20

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$08
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$03 
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$01
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$00       
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

        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$0c,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop



;--------------------------------------------------

journey_tr_4_pt_00:

                ; M 1-4 & 5-8

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3a
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait

                ; M 2/6

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$c0
	db $3a
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait

                ; M 3/7

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3a
	db ctrlbt_wait       
	db ctrlbt_volume,$90
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait

                ; M 4/8

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_volume,$a0
	db $3a
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

journey_tr_4_pt_01:

                ; M 9

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3a
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait

                ; M 10

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3a
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait

                ; M 11

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3a
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait

                ; M 12

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$30
        db ctrlbt_duration,20
	db $3a
	db ctrlbt_volume,$40
	db $3a
	db ctrlbt_volume,$50
	db $3a
	db ctrlbt_volume,$60
	db $3a
	db ctrlbt_volume,$70
	db $3a
	db ctrlbt_volume,$80
	db $3a
	db ctrlbt_volume,$90
	db $3a
	db ctrlbt_volume,$80
	db $3a

	db ctrlbt_track_loop

;--------------------------------------------------


journey_tr_4_pt_02:

                ; M 13-16 & 17-20

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $29
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3b
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 14

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,56
	db $29
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3b
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 15

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $29
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 16

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3c
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      

	db ctrlbt_track_loop

;------------------------------------------

journey_tr_4_pt_03:

                ; M 25/29

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 26/30

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 27/31

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 28/32

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      

        db ctrlbt_duration,56
	db ctrlbt_track_loop

;------------------------------------------

journey_tr_4_pt_04:

                ; M 33

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 34

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 35

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 36

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3d
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      

	db ctrlbt_track_loop

;-------------------------------------------

journey_tr_4_pt_05:

                ; M 37

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 38

        db ctrlbt_duration,56
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,56
	db $3d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 39

        db ctrlbt_duration,55
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait

                ; M 40


	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait      

	db ctrlbt_track_loop

;----------------------------------------

journey_tr_4_pt_06:

                ; M 41

        db ctrlbt_duration,55
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait

                ; M 42

	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait

                ; M 43

	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,55
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait

                ; M 44

	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait      

                ; M 45

	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait

                ; M 46

	db ctrlbt_volume,$d0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $31
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $35
	db ctrlbt_wait

                ; M 47

	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait

                ; M 48

	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $3c
	db ctrlbt_wait      

	db ctrlbt_track_loop

;-------------------------------------------

journey_tr_4_pt_07:

                ; M 49

        db ctrlbt_duration,55
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $2d
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_wait

                ; M 50

	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait

                ; M 51

	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait

                ; M 52

	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_wait      

                ; M 53

	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $29
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $31
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
	db $2d
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3c
	db ctrlbt_wait

                ; M 54

	db ctrlbt_volume,$e0
	db $35
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $2d
	db ctrlbt_wait      
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $31
	db ctrlbt_wait       
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $2d
	db ctrlbt_wait

                ; M 55

;	db ctrlbt_volume,$e0
;	db $29
;	db ctrlbt_wait      
;	db ctrlbt_volume,$b0
;	db $29
;	db ctrlbt_wait      
;	db ctrlbt_volume,$e0
;	db $31
;	db ctrlbt_wait      
;	db ctrlbt_volume,$b0
;	db $2d
;	db ctrlbt_wait       
;	db ctrlbt_volume,$e0
;	db $3f
;	db ctrlbt_wait       
;	db ctrlbt_volume,$b0
;	db $2d
;	db ctrlbt_wait

                ; M 56

;	db ctrlbt_volume,$e0
;	db $29
;	db ctrlbt_wait
;	db ctrlbt_volume,$b0
;	db $29
;	db ctrlbt_wait      
;	db ctrlbt_volume,$e0
;	db $29
;	db ctrlbt_wait      
;	db ctrlbt_volume,$b0
;	db $31
;	db ctrlbt_wait      
;	db ctrlbt_volume,$e0
;	db $31
;	db ctrlbt_wait      
;	db ctrlbt_volume,$b0
;	db $35
;	db ctrlbt_wait      

	db ctrlbt_track_loop

;---------------------------------------------

journey_tr_4_pt_08:

                ; M 1


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
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

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------

journey_tr_4_pt_09:

                ; M 25/29

        db ctrlbt_duration,0
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_volume,$f0
	db $40+$21
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db ctrlbt_envelope,%0000_1_100
	db $40+$2a
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26/30

	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_110
	db $40+$2e
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db ctrlbt_envelope,%0000_1_100
	db $40+$2a
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27/31

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_110
	db $40+$21
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_envelope,%0000_1_100
	db $40+$31
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28/32

	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_110
	db $40+$2d
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$b0
        db ctrlbt_duration,36
	db $38
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $38
	db ctrlbt_wait      

        db ctrlbt_duration,56
	db ctrlbt_track_loop


journey_end:

        end

;-----------------------------------------------
