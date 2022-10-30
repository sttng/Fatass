;***************************************************************************

wanderin_header:
	dw wanderin_sequence-wanderin_header	; sequence index
	dw wanderin_patterns-wanderin_header	; patterns index
	dw wanderin_end-wanderin_header		; end index
	db 31 ; # of patterns

	db 'WANDERIN'					; song name
	db $ff

wanderin_sequence:

	db ctrlbt_song_speed,$40
	db ctrlbt_panning,%11111111

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
	db $1d
	db $1e


	db ctrlbt_track_loop


wanderin_patterns:
	dw wanderin_tr_1_pt_00-wanderin_patterns,wanderin_tr_2_pt_00-wanderin_patterns,wanderin_tr_3_pt_10-wanderin_patterns,wanderin_tr_4_pt_14-wanderin_patterns
	dw wanderin_tr_1_pt_01-wanderin_patterns,wanderin_tr_2_pt_01-wanderin_patterns,wanderin_tr_3_pt_10-wanderin_patterns,wanderin_tr_4_pt_14-wanderin_patterns
	dw wanderin_tr_1_pt_02-wanderin_patterns,wanderin_tr_2_pt_02-wanderin_patterns,wanderin_tr_3_pt_10-wanderin_patterns,wanderin_tr_4_pt_14-wanderin_patterns
	dw wanderin_tr_1_pt_03-wanderin_patterns,wanderin_tr_2_pt_03-wanderin_patterns,wanderin_tr_3_pt_10-wanderin_patterns,wanderin_tr_4_pt_15-wanderin_patterns
	dw wanderin_tr_1_pt_00-wanderin_patterns,wanderin_tr_2_pt_00-wanderin_patterns,wanderin_tr_3_pt_00-wanderin_patterns,wanderin_tr_4_pt_00-wanderin_patterns
	dw wanderin_tr_1_pt_01-wanderin_patterns,wanderin_tr_2_pt_01-wanderin_patterns,wanderin_tr_3_pt_01-wanderin_patterns,wanderin_tr_4_pt_00-wanderin_patterns
	dw wanderin_tr_1_pt_02-wanderin_patterns,wanderin_tr_2_pt_02-wanderin_patterns,wanderin_tr_3_pt_02-wanderin_patterns,wanderin_tr_4_pt_13-wanderin_patterns
	dw wanderin_tr_1_pt_03-wanderin_patterns,wanderin_tr_2_pt_03-wanderin_patterns,wanderin_tr_3_pt_03-wanderin_patterns,wanderin_tr_4_pt_01-wanderin_patterns
	dw wanderin_tr_1_pt_04-wanderin_patterns,wanderin_tr_2_pt_04-wanderin_patterns,wanderin_tr_3_pt_04-wanderin_patterns,wanderin_tr_4_pt_02-wanderin_patterns
	dw wanderin_tr_1_pt_05-wanderin_patterns,wanderin_tr_2_pt_05-wanderin_patterns,wanderin_tr_3_pt_05-wanderin_patterns,wanderin_tr_4_pt_12-wanderin_patterns
	dw wanderin_tr_1_pt_06-wanderin_patterns,wanderin_tr_2_pt_06-wanderin_patterns,wanderin_tr_3_pt_06-wanderin_patterns,wanderin_tr_4_pt_03-wanderin_patterns
	dw wanderin_tr_1_pt_04-wanderin_patterns,wanderin_tr_2_pt_04-wanderin_patterns,wanderin_tr_3_pt_04-wanderin_patterns,wanderin_tr_4_pt_02-wanderin_patterns
	dw wanderin_tr_1_pt_05-wanderin_patterns,wanderin_tr_2_pt_05-wanderin_patterns,wanderin_tr_3_pt_05-wanderin_patterns,wanderin_tr_4_pt_12-wanderin_patterns
	dw wanderin_tr_1_pt_06-wanderin_patterns,wanderin_tr_2_pt_06-wanderin_patterns,wanderin_tr_3_pt_06-wanderin_patterns,wanderin_tr_4_pt_03-wanderin_patterns
	dw wanderin_tr_1_pt_07-wanderin_patterns,wanderin_tr_2_pt_07-wanderin_patterns,wanderin_tr_3_pt_07-wanderin_patterns,wanderin_tr_4_pt_04-wanderin_patterns
	dw wanderin_tr_1_pt_08-wanderin_patterns,wanderin_tr_2_pt_08-wanderin_patterns,wanderin_tr_3_pt_08-wanderin_patterns,wanderin_tr_4_pt_05-wanderin_patterns
	dw wanderin_tr_1_pt_09-wanderin_patterns,wanderin_tr_2_pt_09-wanderin_patterns,wanderin_tr_3_pt_09-wanderin_patterns,wanderin_tr_4_pt_06-wanderin_patterns
	dw wanderin_tr_1_pt_10-wanderin_patterns,wanderin_tr_2_pt_10-wanderin_patterns,wanderin_tr_3_pt_10-wanderin_patterns,wanderin_tr_4_pt_07-wanderin_patterns
	dw wanderin_tr_1_pt_11-wanderin_patterns,wanderin_tr_2_pt_11-wanderin_patterns,wanderin_tr_3_pt_11-wanderin_patterns,wanderin_tr_4_pt_08-wanderin_patterns
	dw wanderin_tr_1_pt_12-wanderin_patterns,wanderin_tr_2_pt_12-wanderin_patterns,wanderin_tr_3_pt_12-wanderin_patterns,wanderin_tr_4_pt_09-wanderin_patterns
	dw wanderin_tr_1_pt_13-wanderin_patterns,wanderin_tr_2_pt_13-wanderin_patterns,wanderin_tr_3_pt_13-wanderin_patterns,wanderin_tr_4_pt_10-wanderin_patterns
	dw wanderin_tr_1_pt_14-wanderin_patterns,wanderin_tr_2_pt_14-wanderin_patterns,wanderin_tr_3_pt_14-wanderin_patterns,wanderin_tr_4_pt_11-wanderin_patterns
	dw wanderin_tr_1_pt_14-wanderin_patterns,wanderin_tr_2_pt_14-wanderin_patterns,wanderin_tr_3_pt_14-wanderin_patterns,wanderin_tr_4_pt_11-wanderin_patterns
	dw wanderin_tr_1_pt_10-wanderin_patterns,wanderin_tr_2_pt_10-wanderin_patterns,wanderin_tr_3_pt_10-wanderin_patterns,wanderin_tr_4_pt_07-wanderin_patterns
	dw wanderin_tr_1_pt_11-wanderin_patterns,wanderin_tr_2_pt_11-wanderin_patterns,wanderin_tr_3_pt_11-wanderin_patterns,wanderin_tr_4_pt_08-wanderin_patterns
	dw wanderin_tr_1_pt_04-wanderin_patterns,wanderin_tr_2_pt_04-wanderin_patterns,wanderin_tr_3_pt_04-wanderin_patterns,wanderin_tr_4_pt_02-wanderin_patterns
	dw wanderin_tr_1_pt_05-wanderin_patterns,wanderin_tr_2_pt_05-wanderin_patterns,wanderin_tr_3_pt_05-wanderin_patterns,wanderin_tr_4_pt_12-wanderin_patterns
	dw wanderin_tr_1_pt_06-wanderin_patterns,wanderin_tr_2_pt_06-wanderin_patterns,wanderin_tr_3_pt_06-wanderin_patterns,wanderin_tr_4_pt_03-wanderin_patterns
	dw wanderin_tr_1_pt_07-wanderin_patterns,wanderin_tr_2_pt_07-wanderin_patterns,wanderin_tr_3_pt_07-wanderin_patterns,wanderin_tr_4_pt_04-wanderin_patterns
	dw wanderin_tr_1_pt_08-wanderin_patterns,wanderin_tr_2_pt_08-wanderin_patterns,wanderin_tr_3_pt_08-wanderin_patterns,wanderin_tr_4_pt_05-wanderin_patterns
	dw wanderin_tr_1_pt_09-wanderin_patterns,wanderin_tr_2_pt_09-wanderin_patterns,wanderin_tr_3_pt_09-wanderin_patterns,wanderin_tr_4_pt_06-wanderin_patterns


;--------------------------------------------------


wanderin_tr_1_pt_00:

                ; M 1

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
;	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g_5       
	db ctrlbt_a#5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_g_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010 
	db ctrlbt_f#5       
	db ctrlbt_a_5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_c_6     
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-----------------------------------------------

wanderin_tr_1_pt_01:

                ; M 5

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_g_5       
	db ctrlbt_a#5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6     
	db ctrlbt_wait       
	db ctrlbt_g_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#5       
	db ctrlbt_a_5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_6       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_1_pt_02:

                ; M 9

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
;	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_g_5       
	db ctrlbt_a#5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6     
	db ctrlbt_wait       
	db ctrlbt_a#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 10

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010 
	db ctrlbt_f#5       
	db ctrlbt_a_5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c_6    
	db ctrlbt_wait       
	db ctrlbt_c_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_1_pt_03:

                ; M 13

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
;	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_g_5       
	db ctrlbt_a#5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6     
	db ctrlbt_wait       
	db ctrlbt_g_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14

	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010 
	db ctrlbt_f#5       
	db ctrlbt_a_5      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d_6      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_a_5    
	db ctrlbt_wait       
	db ctrlbt_d_6        
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_1_pt_04:

                ; M 17

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_3       
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_c#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_1_pt_05:

                ; M 21

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_f#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_a_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_1_pt_06:

                ; M 25

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_3       
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_g#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_g#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       


        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

wanderin_tr_1_pt_07:

                ; M 27

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 28

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 31

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

wanderin_tr_1_pt_08:

                ; M 32

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00


	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 33

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 34

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_e_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;-----------------------------------------------

wanderin_tr_1_pt_09:

                ; M 36

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,00
        db ctrlbt_transpose,00


	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 37

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      


        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_1_pt_10:

                ; M 38

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00


	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 39

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011 
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 40

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 41

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100       
	db ctrlbt_c#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 42

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_c#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 43

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 44

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 45

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_c#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 46

	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100 
	db ctrlbt_c#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 47

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 48

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 49

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 50

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3       
	db ctrlbt_wait      


                ; M 51

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_3       
	db ctrlbt_wait      


                ; M 52

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#3       
	db ctrlbt_wait      

                ; M 53


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 54

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

wanderin_tr_1_pt_11:

                ; M 54

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00


	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 55

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011 
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 56

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 57

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 58

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_c#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 59

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 60

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 61

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011       
	db ctrlbt_c#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 62

	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100 
	db ctrlbt_c#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 63

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 64

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 65

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_a_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_f#3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 66

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3       
	db ctrlbt_wait      

                ; M 67

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a_3       
	db ctrlbt_wait      

                ; M 68

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3       
	db ctrlbt_wait      

                ; M 69

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_b_3       
	db ctrlbt_wait      

                ; M 70

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 71

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_1_pt_12:

                ; M 71

	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00


	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 72

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 73

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 74

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 75

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_g#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 76

	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_g#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 77

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011      
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 78

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010       
	db ctrlbt_c#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 79

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011 
	db ctrlbt_c#3      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 80

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 81

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g#3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_d#3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 82

	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_1_pt_13:

                ; M 82

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
        db ctrlbt_duration,30

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 83

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_e_3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 84

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_e_3       
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_e_3    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 85

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_e_3       
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_e_3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 86

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_f#3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 87

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#3    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 88

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_f#3    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 89

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_f#3
	db ctrlbt_wait

                ; M 90

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_d#3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 91

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#3       
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_c#3    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 92

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_f#3    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001    
	db ctrlbt_c#3
	db ctrlbt_wait

        db ctrlbt_duration,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_1_pt_14:

                ; M 93

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00


	db ctrlbt_c#3
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c#3     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 94

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait
	db ctrlbt_d#3     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 95

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c#3    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 96

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait      
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_f#3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_c#3
	db ctrlbt_wait

                ; M 97

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_c#4      
	db ctrlbt_wait
	db ctrlbt_c#3    
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait


                ; M 98

	db ctrlbt_c#3
	db ctrlbt_wait
	db ctrlbt_c#3
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

wanderin_tr_2_pt_00:

                ; M 1

        db ctrlbt_hold_mode,00
	db ctrlbt_auto_panner,%10_10_01_01,4
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00 
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00 
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f       
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
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;--------------------------------------------


wanderin_tr_2_pt_01:

                ; M 5

        db ctrlbt_hold_mode,00

	db ctrlbt_auto_panner,%10_10_01_01,4
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f      
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f     
	db ctrlbt_wait        
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
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f      
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;--------------------------------------------------

wanderin_tr_2_pt_02:

                ; M 9

        db ctrlbt_hold_mode,00

	db ctrlbt_auto_panner,%10_10_01_01,4
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f     
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 10

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f     
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f      
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
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

wanderin_tr_2_pt_03:

                ; M 13

	db ctrlbt_auto_panner,%10_10_01_01,4
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f     
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f      
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_111     
	db ctrlbt_f#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$1f       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_2_pt_04:

                ; M 17

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_e_5     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db ctrlbt_d#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0 
	db ctrlbt_c#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_volume,$90
	db ctrlbt_b_4      
	db ctrlbt_wait
		db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_g#4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_volume,$c0
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_d#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_volume,$90
	db ctrlbt_c#4     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0      
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_2_pt_05:

                ; M 21

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_g#3     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db ctrlbt_f#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0 
	db ctrlbt_e_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_volume,$90
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db ctrlbt_d#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_volume,$c0
	db ctrlbt_c#5     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_b_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0 
	db ctrlbt_a_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_volume,$90
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0      
	db ctrlbt_f#4      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_e_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

wanderin_tr_2_pt_06:

                ; M 25

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0 
	db ctrlbt_b_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_volume,$90
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_g#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_f#3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_volume,$c0
	db ctrlbt_e_3    
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_d#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_2_pt_07:

                ; M 27

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 28

	db ctrlbt_volume,$90
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_c#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_b_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_volume,$c0
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_f#4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 30

	db ctrlbt_volume,$90
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_e_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 31

	db ctrlbt_volume,$c0
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_g#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_a_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_2_pt_08:

                ; M 32

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00


	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_c#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_d#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 33

	db ctrlbt_volume,$90
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_c#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 34

	db ctrlbt_volume,$c0
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_a_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_g#4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 35

	db ctrlbt_volume,$90
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_d#4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_2_pt_09:

                ; M 36

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 37

	db ctrlbt_volume,$90
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_f#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_e_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 38

	db ctrlbt_volume,$c0
	db ctrlbt_d#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

wanderin_tr_2_pt_10:

                ; M 38

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00


	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 39

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100   
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 40

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 41

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_d#4
	db ctrlbt_wait


                ; M 42

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait


                ; M 43

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 44

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100  
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 45

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_f#4     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 46

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait


                ; M 47

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100       
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 48

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100       
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 49

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 50

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4       
	db ctrlbt_wait

                ; M 51

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4       
	db ctrlbt_wait

                ; M 52

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4       
	db ctrlbt_wait

                ; M 53

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4       
	db ctrlbt_wait

                ; M 54

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_2_pt_11:

                ; M 54

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00


	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 55

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 56

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 57

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100       
	db ctrlbt_d#4
	db ctrlbt_wait


                ; M 58

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100       
	db ctrlbt_e_4
	db ctrlbt_wait


                ; M 59

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100  
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 60

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100  
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 61

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
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 62

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait


                ; M 63

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 64

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_f#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 65

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_d#4
	db ctrlbt_wait

                ; M 66

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4       
	db ctrlbt_wait

                ; M 67

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4       
	db ctrlbt_wait

                ; M 68

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f#4       
	db ctrlbt_wait

                ; M 69

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#4       
	db ctrlbt_wait

                ; M 70

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 71

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_2_pt_12:

                ; M 71

	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,12
;	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_pwm,%11_01_01_11,3

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_f_4
	db ctrlbt_wait

                ; M 72

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_portamento,$00
	db ctrlbt_a#3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 73

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 74

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_g#4
	db ctrlbt_wait


                ; M 75

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


                ; M 76

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_d#4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 77

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_f_4
	db ctrlbt_wait


                ; M 78

	db ctrlbt_wait
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


                ; M 79

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 80

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_f_4
	db ctrlbt_wait

                ; M 81

	db ctrlbt_wait
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


                ; M 82

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_auto_pwm,%00_01_10_11,00      
	db ctrlbt_auto_panner,%10_10_01_01,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------------

wanderin_tr_2_pt_13:

                ; M 82


	db ctrlbt_auto_pwm,%11_01_01_11,1      
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 83


	db ctrlbt_e_4
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

                ; M 84

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
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 85

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_d#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 86

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 87

	db ctrlbt_e_4
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

                ; M 88

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_f#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 89

	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 90

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_d#4     
	db ctrlbt_wait       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 91

	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#4
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 92

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait


	db ctrlbt_auto_pwm,%00_01_10_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;----------------------------------------

wanderin_tr_2_pt_14:

                ; M 93

	db ctrlbt_auto_pwm,%11_01_01_11,1
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96

	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 94

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_b_3     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 95

	db ctrlbt_d#4
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

                ; M 96

	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_d#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 97

	db ctrlbt_e_4
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

                ; M 98

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%00_01_10_11,0      
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;----------------------------------------------

wanderin_tr_3_pt_00:

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$40
	db ctrlbt_gain_waveform,$26

                ; M 1

	db ctrlbt_g_6
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00 
	db ctrlbt_f#6      
	db ctrlbt_wait       
	db ctrlbt_g_6        
	db ctrlbt_wait       
	db ctrlbt_a_6
	db ctrlbt_wait

                ; M 2

	db ctrlbt_a#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_6       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

	db ctrlbt_c_7
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

	db ctrlbt_a_6      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
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

wanderin_tr_3_pt_01:

                ; M 5

	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00 
	db ctrlbt_c_7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_a#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_7       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 7

	db ctrlbt_a_6
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_a#6        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_c_7
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18  
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00     
	db ctrlbt_a#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop


;--------------------------------------------------

wanderin_tr_3_pt_02:

                ; M 9

	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_f_7
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 11

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_d_7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c_7        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 12

	db ctrlbt_g_7
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00  
	db ctrlbt_a_7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop


;---------------------------------------------------

wanderin_tr_3_pt_03:

                ; M 13

	db ctrlbt_a#7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_7       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_7
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 14

	db ctrlbt_a#7
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c_8       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a#7       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$20
	db ctrlbt_d_8
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$24
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------------

wanderin_tr_3_pt_04:

                ; M 17

	db ctrlbt_note_off
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

                ; M 18

	db ctrlbt_wait
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

                ; M 19

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
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_note_off      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_3_pt_05:

                ; M 21

	db ctrlbt_b_5
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

                ; M 22

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

                ; M 24

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

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------


wanderin_tr_3_pt_06:

                ; M 25

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00  
	db ctrlbt_note_off      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_e_6
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

                ; M 27

	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_note_off       
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       


	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_3_pt_07:

                ; M 27

	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 28

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

                ; M 29

	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 31

	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#6       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------------

wanderin_tr_3_pt_08:

                ; M 32

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_b_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_note_off       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 33

	db ctrlbt_wait
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

                ; M 34

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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_e_6
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


	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_3_pt_09:

                ; M 36

	db ctrlbt_wait
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

                ; M 37

	db ctrlbt_d#6
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

                ; M 38

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      


	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------------

wanderin_tr_3_pt_10:

                ; M 38

	db ctrlbt_note_off
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

                ; M 40

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 41

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
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

                ; M 44

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 45

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
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

                ; M 48

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 49

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait            
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 50

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 51

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait            
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 52

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 53

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 54

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_3_pt_11:

                ; M 54

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g#4
	db ctrlbt_portamento,$2f
	db ctrlbt_waveform,$00
        db ctrlbt_transpose,24
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       

                ; M 55

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 56

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f#4
	db ctrlbt_portamento,$2f
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 57

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#5          
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 58

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 59

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 60

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f#4
	db ctrlbt_portamento,$2f
	db ctrlbt_f#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 61

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_5            
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 62

	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_c#6
	db ctrlbt_portamento,$20
	db ctrlbt_c#5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 63

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 64

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f#4
	db ctrlbt_portamento,$2f
	db ctrlbt_f#5      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 65

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#5            
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 66

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#5       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 67

	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_e_6
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g#5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 68

	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#5       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 69

	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#6       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_f#5       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      


                ; M 70

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$2f
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$2f
	db ctrlbt_c#6       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 71

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      


        db ctrlbt_transpose,0
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_3_pt_12:

                ; M 71

        db ctrlbt_hold_mode,00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,12
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait

                ; M 72

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_5
	db ctrlbt_portamento,$3f
	db ctrlbt_a#5
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 73

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 74

	db ctrlbt_wait
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
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_f_6
	db ctrlbt_wait

                ; M 75

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_5
	db ctrlbt_portamento,$3f
	db ctrlbt_a#5
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 76

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 77

	db ctrlbt_wait
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
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_d#6
	db ctrlbt_wait

                ; M 78

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g#4
	db ctrlbt_portamento,$2f
	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 79

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 80

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_d#6
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 81

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 82

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_3_pt_13:

                ; M 82

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_gain_waveform,$23
	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12

	db ctrlbt_e_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait

                ; M 83

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_d#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait

                ; M 84

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait

                ; M 85

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_e_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_d#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait

                ; M 86

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_e_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait

                ; M 87

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_d#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait

                ; M 88

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait


                ; M 89

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_e_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_d#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait

                ; M 90

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait

                ; M 91

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18       
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait

                ; M 92

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_3_pt_14:

                ; M 93

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$23
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
        db ctrlbt_transpose,12

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
	db ctrlbt_wait       
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait

                ; M 94

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18     
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait

                ; M 95

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait

                ; M 96

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait

                ; M 97

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3e
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_b_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3d
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$38
	db ctrlbt_wait

                ; M 98

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_c#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$32
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$34
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$36
	db ctrlbt_wait


        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_4_pt_00:

                ; M 1 (1-4) & (5-8) & (9-12)

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 2

	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 3

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 4

	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,59
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_4_pt_01:

                ; M 13

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 14

	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 15

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 16

        db ctrlbt_duration,59
	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------

wanderin_tr_4_pt_02:

                ; M 17

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait

                ; M 18

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait


                ; M 19

	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 20

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
        db ctrlbt_duration,59
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

wanderin_tr_4_pt_03:

                ; M 25

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait

                ; M 26

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 27

	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------

wanderin_tr_4_pt_04:

                ; M 27

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 28

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait


                ; M 29

	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait

                ; M 30

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 31

	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait


	db ctrlbt_track_loop

;---------------------------------------------

wanderin_tr_4_pt_05:

                ; M 32

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

                ; M 33

	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 34

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 35

	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;------------------------------------------

wanderin_tr_4_pt_06:


                ; M 36

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait


                ; M 37

	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 38

	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $40+$32
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_4_pt_07:

                ; M 38

	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 39

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 40

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 41

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 42

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 43

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 44

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 45

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 46

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 47

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 48

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 49

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 50

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 51

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 52

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait


                ; M 53

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 54

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------

wanderin_tr_4_pt_08:

                ; M 54

	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 55

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$27
	db ctrlbt_wait

                ; M 56

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 57

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 58

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 59

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 60

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 61

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 62

	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 63

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 64

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 65

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 66

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 67

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait

                ; M 68

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait


                ; M 69

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait

                ; M 70

	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 71


	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_track_loop

;-------------------------------------------

wanderin_tr_4_pt_09:

                ; M 71

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait

                ; M 72

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 73

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 74

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait

                ; M 75

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 76

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 77

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait

                ; M 78

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 79

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $29
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 80

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $29
	db ctrlbt_wait

                ; M 81

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait

                ; M 82

	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------

wanderin_tr_4_pt_10:

                ; M 82

        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_000        
        db ctrlbt_duration,0
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 83

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 84

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 85

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 86

	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_duration,5
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 87

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 88

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 89

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 90

        db ctrlbt_duration,59
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 91

	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 92

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

        db ctrlbt_hold_mode,01

	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_4_pt_11:

                ; M 93

        db ctrlbt_duration,59
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,40
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 94

        db ctrlbt_duration,59
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 95

        db ctrlbt_duration,40
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,59
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 96

        db ctrlbt_duration,59
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,40
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,59
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 97

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
        db ctrlbt_duration,40
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 98

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------

wanderin_tr_4_pt_12:

                ; M 21

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait

                ; M 22

	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait


                ; M 23

	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 24

	db ctrlbt_volume,$e0
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------

wanderin_tr_4_pt_13:

                ; M 1 (1-4) & (5-8) & (9-12)

        db ctrlbt_duration,59
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 2

	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 3

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 4

	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
        db ctrlbt_duration,0
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,59
	db $40+$32
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_volume,$80
	db $3f

	db ctrlbt_track_loop

;-----------------------------------------

wanderin_tr_4_pt_14:

                ; blank
                ; M 1

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,5
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,5 
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,5
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 4

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,5
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_track_loop

;----------------------------------------

wanderin_tr_4_pt_15:

                ; blank
                ; M 1

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
        db ctrlbt_duration,5 
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3f 
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,5
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 3

        db ctrlbt_duration,15
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
        db ctrlbt_duration,5
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

        db ctrlbt_duration,59
	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$32
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $40+$32
	db ctrlbt_wait

	db ctrlbt_track_loop

wanderin_end:

        end

	
