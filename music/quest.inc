;***************************************************************************

quest_header:
	dw quest_sequence-quest_header	; sequence index
	dw quest_patterns-quest_header	; patterns index
	dw quest_end-quest_header
	db 16

	db 'QUEST'					; song name
	db $ff

quest_sequence:

	db ctrlbt_song_speed,$40
	db ctrlbt_panning,%11111111
;	db ctrlbt_shuffle,$40,$c3

	db $00
	db $01
	db $02
	db $03
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
	db $0e
	db $0f

	db ctrlbt_track_loop


quest_patterns:
	dw quest_tr_1_pt_00-quest_patterns,quest_tr_2_pt_00-quest_patterns,quest_tr_3_pt_00-quest_patterns,quest_tr_4_pt_02-quest_patterns
	dw quest_tr_1_pt_00-quest_patterns,quest_tr_2_pt_01-quest_patterns,quest_tr_3_pt_00-quest_patterns,quest_tr_4_pt_02-quest_patterns
	dw quest_tr_1_pt_01-quest_patterns,quest_tr_2_pt_05-quest_patterns,quest_tr_3_pt_01-quest_patterns,quest_tr_4_pt_00-quest_patterns
	dw quest_tr_1_pt_01-quest_patterns,quest_tr_2_pt_05-quest_patterns,quest_tr_3_pt_01-quest_patterns,quest_tr_4_pt_01-quest_patterns
	dw quest_tr_1_pt_02-quest_patterns,quest_tr_2_pt_00-quest_patterns,quest_tr_3_pt_02-quest_patterns,quest_tr_4_pt_08-quest_patterns
	dw quest_tr_1_pt_03-quest_patterns,quest_tr_2_pt_00-quest_patterns,quest_tr_3_pt_03-quest_patterns,quest_tr_4_pt_02-quest_patterns
	dw quest_tr_1_pt_04-quest_patterns,quest_tr_2_pt_00-quest_patterns,quest_tr_3_pt_04-quest_patterns,quest_tr_4_pt_03-quest_patterns
	dw quest_tr_1_pt_05-quest_patterns,quest_tr_2_pt_04-quest_patterns,quest_tr_3_pt_05-quest_patterns,quest_tr_4_pt_05-quest_patterns
	dw quest_tr_1_pt_06-quest_patterns,quest_tr_2_pt_02-quest_patterns,quest_tr_3_pt_06-quest_patterns,quest_tr_4_pt_04-quest_patterns
	dw quest_tr_1_pt_07-quest_patterns,quest_tr_2_pt_03-quest_patterns,quest_tr_3_pt_07-quest_patterns,quest_tr_4_pt_04-quest_patterns
	dw quest_tr_1_pt_08-quest_patterns,quest_tr_2_pt_02-quest_patterns,quest_tr_3_pt_08-quest_patterns,quest_tr_4_pt_04-quest_patterns
	dw quest_tr_1_pt_09-quest_patterns,quest_tr_2_pt_03-quest_patterns,quest_tr_3_pt_09-quest_patterns,quest_tr_4_pt_04-quest_patterns
	dw quest_tr_1_pt_01-quest_patterns,quest_tr_2_pt_01-quest_patterns,quest_tr_3_pt_01-quest_patterns,quest_tr_4_pt_06-quest_patterns
	dw quest_tr_1_pt_01-quest_patterns,quest_tr_2_pt_01-quest_patterns,quest_tr_3_pt_01-quest_patterns,quest_tr_4_pt_07-quest_patterns
	dw quest_tr_1_pt_01-quest_patterns,quest_tr_2_pt_01-quest_patterns,quest_tr_3_pt_00-quest_patterns,quest_tr_4_pt_06-quest_patterns
	dw quest_tr_1_pt_01-quest_patterns,quest_tr_2_pt_01-quest_patterns,quest_tr_3_pt_00-quest_patterns,quest_tr_4_pt_07-quest_patterns

;--------------------------------------------------

quest_tr_1_pt_00:

                ; M 1


	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
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

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

quest_tr_1_pt_01:

                ; M 9

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait       
	db ctrlbt_d#4        
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 10

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_e_4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 11

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait       
	db ctrlbt_d#4        
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 12

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait       
	db ctrlbt_e_4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_d#4  
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------------

quest_tr_1_pt_02:

                ; M 17

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_auto_pwm,%11_01_00_01,5
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_c#5     
	db ctrlbt_wait       
	db ctrlbt_d_5        
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
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

                ; M 19

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
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a#5      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

quest_tr_1_pt_03:

                ; M 21

	db ctrlbt_auto_pwm,%11_01_00_01,5
	db ctrlbt_volume,$c0


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_e_5      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_d_5       
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_b_5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;----------------------------------------------

quest_tr_1_pt_04:

                ; M 25

	db ctrlbt_auto_pwm,%11_01_00_01,5
	db ctrlbt_volume,$c0

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_d_6     
	db ctrlbt_wait       
	db ctrlbt_e_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g_6
	db ctrlbt_wait      
	db ctrlbt_a_6
	db ctrlbt_wait       
	db ctrlbt_g_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_6      
	db ctrlbt_wait      
	db ctrlbt_a_6     
	db ctrlbt_wait       
	db ctrlbt_g_6      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_f#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a#6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_b_6      
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_c#7      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_7 
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%11_01_00_01,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_1_pt_05:

                ; M 29

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_auto_pwm,%11_01_00_01,5
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#7    
	db ctrlbt_wait       
	db ctrlbt_d_7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#7
	db ctrlbt_wait

                ; M 30

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_b_6
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_d_7
	db ctrlbt_wait

                ; M 31

	db ctrlbt_c#7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_7      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_7
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_auto_pwm,%11_01_00_01,0
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_b_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------------

quest_tr_1_pt_06:

                ; M 33

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_e_5
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

                ; M 34

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c#5     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

quest_tr_1_pt_07:

                ; M 37

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
        db ctrlbt_transpose,12

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
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 39

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#5     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 40

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------------

quest_tr_1_pt_08:

                ; M 41

	db ctrlbt_volume,$c0
        db ctrlbt_transpose,12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c#5
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 42

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_4
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 43

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_d_5
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 44

	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_1_pt_09:

                ; M 45

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait       
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
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 46

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 47

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
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
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

quest_tr_2_pt_00:

                ; M 1 Blank

        db ctrlbt_hold_mode,01
	db ctrlbt_pulse_width,%10_000000
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

;-----------------------------------------------

quest_tr_2_pt_01:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$2f
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------------

quest_tr_2_pt_02:

        db ctrlbt_hold_mode,01
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,24

                ; M 33/41

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_b_3        
	db ctrlbt_wait       
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 34/42

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_a_4       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait       
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_a_4        
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 35/43

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 36/43

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000       
	db ctrlbt_f#4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;------------------------------------------

quest_tr_2_pt_03:

        db ctrlbt_hold_mode,01
	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,24

                ; M 37/45

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_b_3        
	db ctrlbt_wait       
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 38/46

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_b_3       
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait       
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_c#4        
	db ctrlbt_wait       
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 39/47

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait      
	db ctrlbt_b_3       
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait       
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_e_4        
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 40/47

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_f#4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-------------------------------------------

quest_tr_2_pt_04:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
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

                ; M 4

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_panner,%11_01_11_10,1
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_f#4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_01_11_10,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_2_pt_05:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$2f
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop


;--------------------------------------------

quest_tr_3_pt_00:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$50
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$c0,$00,0
        db ctrlbt_transpose,12

	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$51
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$52
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$53
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$54
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$55
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$56
	db ctrlbt_g_4      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$57
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$58
	db ctrlbt_d#4        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$59
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5a
	db ctrlbt_d#4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5c
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5e
	db ctrlbt_d#4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5f
	db ctrlbt_wait

                ; M 2

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5f
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5e
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5d
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5b
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$59
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$58
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$57
	db ctrlbt_e_4        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$56
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$55
	db ctrlbt_e_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$54
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$53
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$52
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$51
	db ctrlbt_d#4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$50
	db ctrlbt_wait

                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$50
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$51
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$52
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$53
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$54
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$55
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$56
	db ctrlbt_g_4      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$57
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$58
	db ctrlbt_d#4        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$59
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5a
	db ctrlbt_d#4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5c
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5e
	db ctrlbt_d#4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5f
	db ctrlbt_wait

                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5f
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5e
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5d
	db ctrlbt_e_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5b
	db ctrlbt_e_4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$5a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$59
	db ctrlbt_g_4      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$58
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$57
	db ctrlbt_e_4        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$56
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$55
	db ctrlbt_e_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$54
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$53
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$52
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$51
	db ctrlbt_d#4  
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$50
	db ctrlbt_wait

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------------

quest_tr_3_pt_01:

                ; M 9/13

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,12

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30 
	db ctrlbt_c_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 10

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30 
	db ctrlbt_c_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30 
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$c0,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

quest_tr_3_pt_02:

                ; M 17

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$2f
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

                ; M 20

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------

quest_tr_3_pt_03:

                ; M 21

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

                ; M 24

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

quest_tr_3_pt_04:

                ; M 25

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

quest_tr_3_pt_05:

                ; M 29

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

                ; M 31

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

quest_tr_3_pt_06:

                ; M 33

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 34

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

                ; M 35

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_3_pt_07:

                ; M 37

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

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

                ; M 39

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 40

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

quest_tr_3_pt_08:

                ; M 41

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 42

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

                ; M 43

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 44

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_3_pt_09:

                ; M 45

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 46

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 47

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

quest_tr_3_pt_10:

                ; M 48

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 49

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait       
	db ctrlbt_wait
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
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 51

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------

quest_tr_3_pt_11:

                ; M 52

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 53

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 54

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 55

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------

quest_tr_3_pt_12:

                ; M 56

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 57

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 58

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 59

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_wait       
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
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

quest_tr_3_pt_13:

                ; M 60

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$28
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_arpeggio,$0c,$00,1
        db ctrlbt_transpose,0

	db ctrlbt_g_4
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

                ; M 61

	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 62

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 63

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_arpeggio,$0c,$00,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

quest_tr_4_pt_00:

                ; M 1

	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01

	db ctrlbt_volume,$f0
        db ctrlbt_duration,50
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,50
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

                ; M 4

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,50
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------

quest_tr_4_pt_01:

                ; M 5

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

                ; M 6

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,50
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

quest_tr_4_pt_02:

                ; M 5

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

                ; M 6

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------

quest_tr_4_pt_03:

                ; M 5

	db ctrlbt_volume,$f0
        db ctrlbt_duration,20
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 6

	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 7

	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 8

	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------

quest_tr_4_pt_04:

                ; M 5

	db ctrlbt_volume,$b0
        db ctrlbt_duration,40
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait       
	db $3e
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 6

        db ctrlbt_duration,40
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait       
	db $3e
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

                ; M 7

	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait       
	db $3e
	db ctrlbt_wait
	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 8

        db ctrlbt_duration,40
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait       
	db $3e
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------

quest_tr_4_pt_05:

                ; M 5

	db ctrlbt_volume,$f0
        db ctrlbt_duration,20
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 6

	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 7

	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 8

        db ctrlbt_duration,40
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait      
	db $3e
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3e
	db ctrlbt_wait       
	db $3e
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3e
	db ctrlbt_wait
	db $3e
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

quest_tr_4_pt_06:

                ; M 5


	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_100
        db ctrlbt_hold_mode,00

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

                ; M 6

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_100
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

                ; M 8

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

        db ctrlbt_hold_mode,01
	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_4_pt_07:

                ; M 5

	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_100
        db ctrlbt_hold_mode,00

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

                ; M 6

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_100
	db $40+$29
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 8

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
	db $3e
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db $40+$2d        
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

        db ctrlbt_hold_mode,01
	db ctrlbt_track_loop

;--------------------------------------------

quest_tr_4_pt_08:

                ; M 5

	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00

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

                ; M 6

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
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
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait

        db ctrlbt_hold_mode,01
	db ctrlbt_track_loop

quest_end:

        end



;-----------------------------------------------
