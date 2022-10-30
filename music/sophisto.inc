;***************************************************************************

sophisto_header:
	dw sophisto_sequence-sophisto_header	; sequence index
	dw sophisto_patterns-sophisto_header	; patterns index
	dw sophisto_end-sophisto_header
	db 30
	db 'SOPHISTO'					; song name
	db $ff

sophisto_sequence:

	db ctrlbt_panning,%11011110
	db ctrlbt_song_speed,$40

	db $00
	db $01
	db $02
	db $03
	db $04
	db $05
	db ctrlbt_panning,%11111111
	db $06
	db $07
	db $08
	db $09
	db $0a
	db $0b
	db $0c
	db $0d
	db $0e
	db ctrlbt_panning,%11011110
	db $0f
	db $10
	db $11
	db $12
	db $13
	db $14
	db $15
	db $16
	db ctrlbt_panning,%11111111
	db $17
	db $18
	db $19
	db $1a
	db $1b
	db ctrlbt_panning,%11011110
	db $1c
	db $1d

	db ctrlbt_track_loop


sophisto_patterns:
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_00-sophisto_patterns,sophisto_tr_4_pt_00-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_01-sophisto_patterns,sophisto_tr_4_pt_00-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_01-sophisto_patterns,sophisto_tr_4_pt_00-sophisto_patterns
	dw sophisto_tr_1_pt_01-sophisto_patterns,sophisto_tr_2_pt_01-sophisto_patterns,sophisto_tr_3_pt_02-sophisto_patterns,sophisto_tr_4_pt_12-sophisto_patterns
	dw sophisto_tr_1_pt_02-sophisto_patterns,sophisto_tr_2_pt_02-sophisto_patterns,sophisto_tr_3_pt_03-sophisto_patterns,sophisto_tr_4_pt_01-sophisto_patterns
	dw sophisto_tr_1_pt_03-sophisto_patterns,sophisto_tr_2_pt_03-sophisto_patterns,sophisto_tr_3_pt_04-sophisto_patterns,sophisto_tr_4_pt_02-sophisto_patterns
	dw sophisto_tr_1_pt_04-sophisto_patterns,sophisto_tr_2_pt_04-sophisto_patterns,sophisto_tr_3_pt_05-sophisto_patterns,sophisto_tr_4_pt_03-sophisto_patterns
	dw sophisto_tr_1_pt_05-sophisto_patterns,sophisto_tr_2_pt_05-sophisto_patterns,sophisto_tr_3_pt_06-sophisto_patterns,sophisto_tr_4_pt_04-sophisto_patterns
	dw sophisto_tr_1_pt_06-sophisto_patterns,sophisto_tr_2_pt_06-sophisto_patterns,sophisto_tr_3_pt_07-sophisto_patterns,sophisto_tr_4_pt_05-sophisto_patterns
	dw sophisto_tr_1_pt_07-sophisto_patterns,sophisto_tr_2_pt_07-sophisto_patterns,sophisto_tr_3_pt_08-sophisto_patterns,sophisto_tr_4_pt_06-sophisto_patterns
	dw sophisto_tr_1_pt_08-sophisto_patterns,sophisto_tr_2_pt_08-sophisto_patterns,sophisto_tr_3_pt_09-sophisto_patterns,sophisto_tr_4_pt_07-sophisto_patterns
	dw sophisto_tr_1_pt_12-sophisto_patterns,sophisto_tr_2_pt_12-sophisto_patterns,sophisto_tr_3_pt_15-sophisto_patterns,sophisto_tr_4_pt_03-sophisto_patterns
	dw sophisto_tr_1_pt_11-sophisto_patterns,sophisto_tr_2_pt_11-sophisto_patterns,sophisto_tr_3_pt_14-sophisto_patterns,sophisto_tr_4_pt_11-sophisto_patterns
	dw sophisto_tr_1_pt_09-sophisto_patterns,sophisto_tr_2_pt_09-sophisto_patterns,sophisto_tr_3_pt_10-sophisto_patterns,sophisto_tr_4_pt_10-sophisto_patterns
	dw sophisto_tr_1_pt_10-sophisto_patterns,sophisto_tr_2_pt_10-sophisto_patterns,sophisto_tr_3_pt_11-sophisto_patterns,sophisto_tr_4_pt_04-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_00-sophisto_patterns,sophisto_tr_4_pt_08-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_00-sophisto_patterns,sophisto_tr_4_pt_09-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_12-sophisto_patterns,sophisto_tr_4_pt_08-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_13-sophisto_patterns,sophisto_tr_4_pt_09-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_12-sophisto_patterns,sophisto_tr_4_pt_08-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_13-sophisto_patterns,sophisto_tr_4_pt_09-sophisto_patterns
	dw sophisto_tr_1_pt_02-sophisto_patterns,sophisto_tr_2_pt_02-sophisto_patterns,sophisto_tr_3_pt_03-sophisto_patterns,sophisto_tr_4_pt_01-sophisto_patterns
	dw sophisto_tr_1_pt_03-sophisto_patterns,sophisto_tr_2_pt_03-sophisto_patterns,sophisto_tr_3_pt_04-sophisto_patterns,sophisto_tr_4_pt_02-sophisto_patterns
	dw sophisto_tr_1_pt_04-sophisto_patterns,sophisto_tr_2_pt_04-sophisto_patterns,sophisto_tr_3_pt_05-sophisto_patterns,sophisto_tr_4_pt_03-sophisto_patterns
	dw sophisto_tr_1_pt_05-sophisto_patterns,sophisto_tr_2_pt_05-sophisto_patterns,sophisto_tr_3_pt_06-sophisto_patterns,sophisto_tr_4_pt_04-sophisto_patterns
	dw sophisto_tr_1_pt_06-sophisto_patterns,sophisto_tr_2_pt_06-sophisto_patterns,sophisto_tr_3_pt_07-sophisto_patterns,sophisto_tr_4_pt_05-sophisto_patterns
	dw sophisto_tr_1_pt_07-sophisto_patterns,sophisto_tr_2_pt_07-sophisto_patterns,sophisto_tr_3_pt_08-sophisto_patterns,sophisto_tr_4_pt_06-sophisto_patterns
	dw sophisto_tr_1_pt_08-sophisto_patterns,sophisto_tr_2_pt_08-sophisto_patterns,sophisto_tr_3_pt_09-sophisto_patterns,sophisto_tr_4_pt_07-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_00-sophisto_patterns,sophisto_tr_4_pt_08-sophisto_patterns
	dw sophisto_tr_1_pt_00-sophisto_patterns,sophisto_tr_2_pt_00-sophisto_patterns,sophisto_tr_3_pt_00-sophisto_patterns,sophisto_tr_4_pt_09-sophisto_patterns

;--------------------------------------------------

sophisto_tr_1_pt_00:

	db ctrlbt_song_speed,$40

                ; M 1/5/9

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$40
	db ctrlbt_arpeggio,$00,$c0,1
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_g#4      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 2/6/10

        db ctrlbt_transpose,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_b_4      
	db ctrlbt_wait
        db ctrlbt_transpose,24
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 3/7/11

        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000      
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait

                ; M 4/8/12

        db ctrlbt_transpose,0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4       
	db ctrlbt_wait      
        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_e_4
	db ctrlbt_wait

	db ctrlbt_arpeggio,$00,$c0,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

sophisto_tr_1_pt_01:

                ; M 13

;	db ctrlbt_song_speed,$38
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_arpeggio,$00,$c0,1
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_g#4       
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 14

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_g#4       
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 15

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_g#4        
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 16

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_a_4       
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait       
	db ctrlbt_e_4        
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait

	db ctrlbt_arpeggio,$00,$c0,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_1_pt_02:

                ; M 17

;	db ctrlbt_song_speed,$36
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 18

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 19

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 20

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 21

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 22

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 23

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 24

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100      
	db ctrlbt_a_3     
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

sophisto_tr_1_pt_03:

                ; M 25

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 26

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 27

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 28

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 29

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 30

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 31

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 32

;	db ctrlbt_song_speed,$38
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_b_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;--------------------------------------------------

sophisto_tr_1_pt_04:

;	db ctrlbt_song_speed,$40
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 33

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 34

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 35

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 36

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_g_3
	db ctrlbt_wait

                ; M 37

	db ctrlbt_envelope,%0000_0_110
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

                ; M 38

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

                ; M 39

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

                ; M 40

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

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_1_pt_05:

                ; M 41


	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 42

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 43

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 44

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 45

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_song_speed,$20      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 46

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$03,$3f
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

;----------------------------------------------

sophisto_tr_1_pt_06:

                ; M 46

	db ctrlbt_song_speed,$40

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 47

	db ctrlbt_envelope,%0000_0_011
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

                ; M 48

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 49

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_a_3       
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

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 52

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_4
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c_4        
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
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 54

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010  
	db ctrlbt_g_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g#3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 55

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 56

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 57

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#4
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_1_pt_07:

                ; M 57

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 58

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 59

	db ctrlbt_envelope,%0000_0_100
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

                ; M 60

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

                ; M 61

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 62

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 63

	db ctrlbt_envelope,%0000_0_011
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

                ; M 64

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f_3
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 65

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       

        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_1_pt_08:

                ; M 65

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 66

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 67

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_4        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 68

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 69

	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 70

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_f_3
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 71

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_g#3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 72

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 73

	db ctrlbt_volume,$00
	db ctrlbt_envelope,%0000_1_111
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
	db ctrlbt_wait     
	db ctrlbt_wait       

                ; M 75

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_111      
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
	db ctrlbt_wait     
	db ctrlbt_wait       

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_1_pt_09:

           ; Copy transpose up 1 step

;	db ctrlbt_song_speed,$40
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,1

                ; M 33

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 34

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 35

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 36

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_g_3
	db ctrlbt_wait

                ; M 37

	db ctrlbt_envelope,%0000_0_100
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

                ; M 38

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

                ; M 39

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

                ; M 40

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

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_1_pt_10:

                ; M 41


	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,1

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 42

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 43

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a#3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 44

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 45

	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_song_speed,$20       
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

        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_1_pt_11:

                ; M 98

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_g_3       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010     
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_g_3
	db ctrlbt_wait

                ; M 99

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_1_pt_12:


	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

                ; M 33

	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_3       
	db ctrlbt_wait      
	db ctrlbt_d_3      
	db ctrlbt_wait       
	db ctrlbt_g_3        
	db ctrlbt_wait       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 34

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a#3
	db ctrlbt_wait

                ; M 35

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a#3      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_a_3
	db ctrlbt_wait

                ; M 36

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010      
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001       
	db ctrlbt_g_3
	db ctrlbt_wait

                ; M 37

	db ctrlbt_envelope,%0000_0_110
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

                ; M 38

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

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_2_pt_00:

                ; M 1/5/9

        db ctrlbt_hold_mode,00
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$01,$40
	db ctrlbt_arpeggio,$00,$c0,1
        db ctrlbt_transpose,12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_g#4      
	db ctrlbt_wait       
	db ctrlbt_d_4       
	db ctrlbt_wait       

                ; M 2/6/10

        db ctrlbt_transpose,0
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
        db ctrlbt_transpose,24
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_b_4      
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       

                ; M 3/7/11

        db ctrlbt_transpose,12
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       

                ; M 4/8/12

        db ctrlbt_transpose,00
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_e_4       
	db ctrlbt_wait      
        db ctrlbt_transpose,24
	db ctrlbt_b_4      
	db ctrlbt_wait       
	db ctrlbt_d_4        
	db ctrlbt_wait       

	db ctrlbt_arpeggio,$00,$c0,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;---------------------------------------------------

sophisto_tr_2_pt_01:

                ; M 13

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_arpeggio,$00,$c0,1

	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_g#4       
	db ctrlbt_wait       


                ; M 14

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_g#4       
	db ctrlbt_wait       


                ; M 15

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_g#4        
	db ctrlbt_wait       


                ; M 16

	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait      
	db ctrlbt_a_4       
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait       
	db ctrlbt_e_4        
	db ctrlbt_wait       


	db ctrlbt_arpeggio,$00,$c0,0
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-------------------------------------------------

sophisto_tr_2_pt_02:

                ; M 17

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12
        db ctrlbt_duration,30


	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       

                ; M 18

	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait

                ; M 19

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
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

                ; M 20

	db ctrlbt_f#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       

                ; M 21

	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
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

                ; M 22

	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait

                ; M 23

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
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

                ; M 24

	db ctrlbt_f#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_2_pt_03:

                ; M 25

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_wait
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

                ; M 26

	db ctrlbt_e_4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c_4       
	db ctrlbt_wait      
	db ctrlbt_c#4       
	db ctrlbt_wait      
	db ctrlbt_c_4      
	db ctrlbt_wait

                ; M 27

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       

                ; M 28

	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c#4      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       

                ; M 29

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       

                ; M 30

	db ctrlbt_c#4      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait

                ; M 31

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_b_3
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       

                ; M 32

	db ctrlbt_b_3       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010     
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       

        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_2_pt_04:

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12
	db ctrlbt_auto_pwm,%11_00_11_00,0

                ; M 33

	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0

	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 35

	db ctrlbt_volume,$20

	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0

	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 37

	db ctrlbt_volume,$20

	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 39

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

        db ctrlbt_transpose,0
	db ctrlbt_auto_pwm,%11_10_01_00,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_2_pt_05:

                ; M 41


	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12

	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 43

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
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

                ; M 44

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 45

	db ctrlbt_volume,$10
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_e_4
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
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_volume,$b0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%11_10_01_00,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_2_pt_06:

                ; M 46

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$80
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_arpeggio,$0c,$c0,1
	db ctrlbt_auto_panner,%11_10_11_01,1

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c#6
	db ctrlbt_wait
	db ctrlbt_c_6       
	db ctrlbt_wait      

                ; M 47

	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_a#5       
	db ctrlbt_wait
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_g#5     
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 48

	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_d#5       
	db ctrlbt_wait      
	db ctrlbt_d_5      
	db ctrlbt_wait       
	db ctrlbt_c#5        
	db ctrlbt_wait       
	db ctrlbt_c_5
	db ctrlbt_wait

                ; M 49

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_a#4       
	db ctrlbt_wait      
	db ctrlbt_a_4       
	db ctrlbt_wait     
	db ctrlbt_a#4       
	db ctrlbt_wait
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait

                ; M 50

	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait      
	db ctrlbt_d#5
	db ctrlbt_wait      
	db ctrlbt_e_5     
	db ctrlbt_wait       
	db ctrlbt_f_5        
	db ctrlbt_wait       
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 51

	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_g#5     
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_b_5        
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 52

	db ctrlbt_d_6
	db ctrlbt_wait      
	db ctrlbt_c#6     
	db ctrlbt_wait       
	db ctrlbt_c_6        
	db ctrlbt_wait       
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 53

	db ctrlbt_g#5
	db ctrlbt_wait      
	db ctrlbt_g_5     
	db ctrlbt_wait       
	db ctrlbt_f#5
	db ctrlbt_wait       
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait

                ; M 54

	db ctrlbt_d_5
	db ctrlbt_wait      
	db ctrlbt_c#5     
	db ctrlbt_wait       
	db ctrlbt_c_5
	db ctrlbt_wait       
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_a#4        
	db ctrlbt_wait       
	db ctrlbt_b_4
	db ctrlbt_wait

                ; M 55

	db ctrlbt_c_5
	db ctrlbt_wait      
	db ctrlbt_c#5     
	db ctrlbt_wait       
	db ctrlbt_d_5        
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait       
	db ctrlbt_f_5
	db ctrlbt_wait

                ; M 56

	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_g_5     
	db ctrlbt_wait       
	db ctrlbt_g#5
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_b_5
	db ctrlbt_wait

                ; M 57

	db ctrlbt_c_6
	db ctrlbt_wait      
	db ctrlbt_d_6     
	db ctrlbt_wait       

	db ctrlbt_auto_panner,%11_11_00_00,0
	db ctrlbt_arpeggio,$0c,$0c,0
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_2_pt_07:

                ; M 57

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$80
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_auto_panner,%11_10_11_01,1

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_d_6       
	db ctrlbt_wait      
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_b_5       
	db ctrlbt_wait      

                ; M 58

	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_g#5       
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_f#5     
	db ctrlbt_wait       
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait

                ; M 59

	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait      
	db ctrlbt_c#5       
	db ctrlbt_wait      
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_b_4        
	db ctrlbt_wait       
	db ctrlbt_a#4
	db ctrlbt_wait

                ; M 60


	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_a#4       
	db ctrlbt_wait      
	db ctrlbt_b_4       
	db ctrlbt_wait     
	db ctrlbt_c_5       
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_d_5
	db ctrlbt_wait

                ; M 61

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_e_5       
	db ctrlbt_wait      
	db ctrlbt_f_5
	db ctrlbt_wait      
	db ctrlbt_f#5     
	db ctrlbt_wait       
	db ctrlbt_g_5        
	db ctrlbt_wait       
	db ctrlbt_g#5
	db ctrlbt_wait

                ; M 62

	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_a#5     
	db ctrlbt_wait       
	db ctrlbt_b_5
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait
	db ctrlbt_d_6        
	db ctrlbt_wait       
	db ctrlbt_c#6
	db ctrlbt_wait

                ; M 63

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_c_6
	db ctrlbt_wait      
	db ctrlbt_b_5     
	db ctrlbt_wait       
	db ctrlbt_a#5        
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_g#5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 64

	db ctrlbt_f#5
	db ctrlbt_wait      
	db ctrlbt_f_5     
	db ctrlbt_wait       
	db ctrlbt_e_5
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_d_5        
	db ctrlbt_wait       
	db ctrlbt_c#5
	db ctrlbt_wait

                ; M 65

	db ctrlbt_c_5
	db ctrlbt_wait      
	db ctrlbt_b_4     
	db ctrlbt_wait       

	db ctrlbt_auto_panner,%11_11_00_00,0
	db ctrlbt_arpeggio,$0c,$0c,0
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_2_pt_08:

                ; M 65

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$d0
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 66

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 67

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 68

	db ctrlbt_envelope,%0000_0_111
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

                ; M 69

	db ctrlbt_envelope,%0000_0_000
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 70

	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 71

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_g#5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 72

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 73

	db ctrlbt_volume,$10
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_f_6
	db ctrlbt_wait      
	db ctrlbt_wait     
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
	db ctrlbt_wait     
	db ctrlbt_wait       

                ; M 75

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_111      
	db ctrlbt_f_6
	db ctrlbt_wait      
	db ctrlbt_wait     
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

;-------------------------------------------------

sophisto_tr_2_pt_09:

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,13

                ; M 33

	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 35

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 37

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 39

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

	db ctrlbt_auto_pwm,%11_00_11_00,0
        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;------------------------------------------------

sophisto_tr_2_pt_10:

                ; M 41

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,13
	db ctrlbt_auto_pwm,%11_00_11_00,0

	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 43

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
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

                ; M 44

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 45

	db ctrlbt_volume,$10
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
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

                ; M 46

	db ctrlbt_volume,$b0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_e_4
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%11_00_11_00,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;--------------------------------------------------

sophisto_tr_2_pt_11:

                ; M 98

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
	db ctrlbt_volume,$f0
	db ctrlbt_envelope,%0000_0_001
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,0

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_a_4
	db ctrlbt_wait       
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_a_4
	db ctrlbt_wait

                ; M 99

	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_a_3       
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_a_5      
	db ctrlbt_wait       
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_a_5
	db ctrlbt_wait

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;-------------------------------------------------

sophisto_tr_2_pt_12:

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_fine_tune,0
        db ctrlbt_transpose,12
	db ctrlbt_auto_pwm,%11_00_11_00,0

                ; M 33

	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 35

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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

                ; M 37

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%11_00_11_00,0
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_wait       
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

	db ctrlbt_volume,$c0
	db ctrlbt_auto_pwm,%11_00_11_00,1
	db ctrlbt_envelope,%0000_0_111
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


	db ctrlbt_auto_pwm,%11_00_11_00,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;--------------------------------------------------

sophisto_tr_3_pt_00:

                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
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

	db ctrlbt_track_loop

;--------------------------------------------

sophisto_tr_3_pt_01:

                ; M 5/9

        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$29
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_3       
	db ctrlbt_wait      
	db ctrlbt_e_3      
	db ctrlbt_wait       
	db ctrlbt_d_3        
	db ctrlbt_wait       
	db ctrlbt_e_3
	db ctrlbt_wait

                ; M 6/10

	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_3       
	db ctrlbt_wait      
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_a_3	        
	db ctrlbt_wait       
	db ctrlbt_b_3
	db ctrlbt_wait

                ; M 7/11

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4      
	db ctrlbt_wait       
	db ctrlbt_c_4        
	db ctrlbt_wait       
	db ctrlbt_d_4
	db ctrlbt_wait

                ; M 4/12

	db ctrlbt_c_4
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

        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_3_pt_02:

                ; M 13

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$18
	db ctrlbt_gain_waveform,$29
	db ctrlbt_portamento,$44
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00        

	db ctrlbt_a_3      
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
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 15

	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_portamento,$00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00 
	db ctrlbt_a_3        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 16

	db ctrlbt_b_3
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

        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_3_pt_03:

                ; M 17

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_gain_waveform,$29
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00        

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

                ; M 18

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

                ; M 19

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
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

                ; M 20

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

                ; M 21

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
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

                ; M 22

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

                ; M 23

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
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

                ; M 24

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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_3_pt_04:

                ; M 25

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$30
	db ctrlbt_gain_waveform,$29
        db ctrlbt_transpose,12
        db ctrlbt_hold_mode,00        

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

                ; M 26

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

                ; M 27

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

                ; M 28

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

                ; M 29

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
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

                ; M 30

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

                ; M 31

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
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

                ; M 32

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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_3_pt_05:

                ; M 33

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_transpose,24
        db ctrlbt_hold_mode,00        

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait       
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 34

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_g_5
	db ctrlbt_wait
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
	db ctrlbt_g_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

	db ctrlbt_f_5
	db ctrlbt_wait
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
	db ctrlbt_d_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 37

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_a_5      
	db ctrlbt_wait       
	db ctrlbt_d_6        
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 38

	db ctrlbt_d_6
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

                ; M 39

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
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 40

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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_3_pt_06:

                ; M 41

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_transpose,24
        db ctrlbt_hold_mode,00        

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait       
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 42

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 43

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a_5
	db ctrlbt_wait
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
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 44

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 45

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$02
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$04
	db ctrlbt_gain_waveform,$07
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$20
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$06
	db ctrlbt_gain_waveform,$07
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$08
	db ctrlbt_gain_waveform,$06
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$24
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 46

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$06
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$26
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0c
	db ctrlbt_gain_waveform,$05
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$28
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0e
	db ctrlbt_gain_waveform,$04
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2a
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0f
	db ctrlbt_gain_waveform,$03
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2c
	db ctrlbt_wait      
	db ctrlbt_wait       

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_3_pt_07:

                ; M 46

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,12

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 47

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_arpeggio,$0c,$0c,3
	db ctrlbt_e_6
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 48

	db ctrlbt_note_off
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

                ; M 49

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_c#5      
	db ctrlbt_wait     
	db ctrlbt_note_off       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 50

	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_note_off       
	db ctrlbt_wait      
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_note_off        
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
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 52

	db ctrlbt_note_off
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

                ; M 53

	db ctrlbt_note_off
	db ctrlbt_g_4      
	db ctrlbt_wait
	db ctrlbt_note_off       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 54

	db ctrlbt_d_5
	db ctrlbt_wait      
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_d#5        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait

                ; M 55

	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_g#4        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_a#4
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait

                ; M 56

	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d#5        
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait

                ; M 57

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_note_off

	db ctrlbt_arpeggio,$0c,$0c,0
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_3_pt_08:

                ; M 57

	db ctrlbt_restart_envelope

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,24
	db ctrlbt_arpeggio,$0c,$0c,3

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      

                ; M 58

	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4     
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 59

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d_4
	db ctrlbt_wait      
	db ctrlbt_note_off        
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 60

	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_note_off             
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_wait


                ; M 61

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_note_off     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 62

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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 63

	db ctrlbt_note_off
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_c_4       
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_g_4
	db ctrlbt_wait

                ; M 64

	db ctrlbt_note_off
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait       
	db ctrlbt_note_off
	db ctrlbt_wait

                ; M 65

	db ctrlbt_g_4
	db ctrlbt_wait      
	db ctrlbt_note_off
	db ctrlbt_wait       

	db ctrlbt_arpeggio,$0c,$0c,0
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_3_pt_09:

                ; M 65

	db ctrlbt_restart_envelope
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
        db ctrlbt_transpose,0

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$08
	db ctrlbt_g#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2e
	db ctrlbt_wait      

                ; M 66

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_a#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 67

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 68

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_g#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2e
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait

                ; M 69

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_d#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 70

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_f_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait

                ; M 71

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2e
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_d#6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 72

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 73

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20
	db ctrlbt_gain_waveform,$00
	db ctrlbt_d_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_gain_waveform,$01
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_gain_waveform,$02
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_gain_waveform,$03
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2e
	db ctrlbt_gain_waveform,$04
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$2a
	db ctrlbt_gain_waveform,$05
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$28
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_wait       

                ; M 74

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$26
	db ctrlbt_gain_waveform,$06
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$24
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$22
	db ctrlbt_gain_waveform,$07
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$20     
	db ctrlbt_wait       
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

                ; M 75

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
	db ctrlbt_gain_waveform,$02
	db ctrlbt_wait       

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_track_loop


;------------------------------------------------

sophisto_tr_3_pt_10:

                ; M 33

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_transpose,25
        db ctrlbt_hold_mode,00        

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait       
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 34

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_g_5
	db ctrlbt_wait
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
	db ctrlbt_g_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

	db ctrlbt_f_5
	db ctrlbt_wait
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
	db ctrlbt_d_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 37

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_a_5      
	db ctrlbt_wait       
	db ctrlbt_d_6        
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 38

	db ctrlbt_d_6
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

                ; M 39

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
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 40

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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_3_pt_11:

                ; M 41

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_transpose,25
        db ctrlbt_hold_mode,00        

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait       
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 42

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_c_5      
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 43

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a_5
	db ctrlbt_wait
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
	db ctrlbt_a_5
	db ctrlbt_wait

                ; M 44

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 45

	db ctrlbt_a_5
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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_3_pt_12:

                ; M 112

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$16
        db ctrlbt_transpose,24
        db ctrlbt_hold_mode,00        

	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$17
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$19
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1a
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1b
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 113

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1c
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1e
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1e
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1f
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 114

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1e
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1c
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1b
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1a
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$19
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 115

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$18
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$18
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$17
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$17
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$16
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$16
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_3_pt_13:

                ; M 116

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$16
        db ctrlbt_transpose,24
        db ctrlbt_hold_mode,00        

	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$17
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$18
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$19
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1a
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1b
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 117

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1c
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1e
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1e
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1f
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 118

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1e
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1c
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1b
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1a
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$19
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 119

	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$18
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$22
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$19
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1a
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1b
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1c
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$1d
	db ctrlbt_wait
	db ctrlbt_wait

        db ctrlbt_transpose,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_3_pt_14:

                ; M 98

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,24

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_g_5
	db ctrlbt_wait       
	db ctrlbt_e_5        
	db ctrlbt_wait       
	db ctrlbt_d#5
	db ctrlbt_wait

                ; M 99

	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_d#5       
	db ctrlbt_wait
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_g_5      
	db ctrlbt_wait       
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_b_5
	db ctrlbt_wait

        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_3_pt_15:

                ; M 33

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$00
	db ctrlbt_gain_waveform,$08
        db ctrlbt_transpose,24
        db ctrlbt_hold_mode,00        

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait       
	db ctrlbt_f#5        
	db ctrlbt_wait       
	db ctrlbt_g_5
	db ctrlbt_wait

                ; M 34

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_a#5
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
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 35

	db ctrlbt_g_5
	db ctrlbt_wait
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
	db ctrlbt_g_5        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 36

	db ctrlbt_f_5
	db ctrlbt_wait
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
	db ctrlbt_d_5
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 37

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$18
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_g_5
	db ctrlbt_wait      
	db ctrlbt_a_5      
	db ctrlbt_wait       
	db ctrlbt_d_6        
	db ctrlbt_wait       
	db ctrlbt_c_6
	db ctrlbt_wait

                ; M 38

	db ctrlbt_d_6
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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,0

	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_4_pt_00:

                ; M 1/5/9

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 2/6/10

	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 3/7/11

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 4/8/12

	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_4_pt_01:

                ; M 17

        db ctrlbt_hold_mode,01
        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,00
	db $3f
	db ctrlbt_wait

                ; M 18

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 19

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 20

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 21

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 22

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 23

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 24

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------

sophisto_tr_4_pt_02:

                ; M 25

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,00
	db $3f
	db ctrlbt_wait

                ; M 26

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 27

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 28

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 29

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 30

        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait

                ; M 31

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
        db ctrlbt_duration,55
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait

                ; M 32

        db ctrlbt_duration,55
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $37
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$c0
	db $37
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_volume,$b0
	db $37
	db ctrlbt_volume,$80
	db $37

	db ctrlbt_track_loop

;--------------------------------------------------

sophisto_tr_4_pt_03:

                ; M 33

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 34

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 35

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 36

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait

                ; M 37

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 38

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$c0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$90
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$90
	db $37
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
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait

                ; M 40

	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_volume,$b0
	db $37
	db ctrlbt_volume,$80
	db $37

	db ctrlbt_track_loop

;-------------------------------------------

sophisto_tr_4_pt_04:

                ; M 41

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 42

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
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
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 44

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait

                ; M 45

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,10
	db $3f
	db ctrlbt_wait       
	db $3f
	db ctrlbt_wait

                ; M 46

        db ctrlbt_duration,55
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_4_pt_05:

                ; M 46

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $37      
	db ctrlbt_wait      

                ; M 47

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,20
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 48

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 49

        db ctrlbt_duration,40
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
        db ctrlbt_duration,20
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 50

        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,10
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$70
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait

                ; M 51

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$70
	db $37
	db ctrlbt_wait

                ; M 52

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 53

        db ctrlbt_duration,20
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$70
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,10
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 54

        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,20
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 55

        db ctrlbt_duration,20
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,10
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 56

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 57

	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $37      
	db ctrlbt_wait      


	db ctrlbt_track_loop

;-------------------------------------------------

sophisto_tr_4_pt_06:

                ; M 57

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $37      
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      

                ; M 58

        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,20
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 59

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 60

        db ctrlbt_duration,45
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_duration,10
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 61

        db ctrlbt_duration,45
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,10
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$70
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 62

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
        db ctrlbt_duration,45
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$70
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 63

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 64

        db ctrlbt_duration,20
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$70
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,20
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
        db ctrlbt_duration,45
	db $37
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 65

        db ctrlbt_duration,45
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db $37
	db ctrlbt_wait      

	db ctrlbt_track_loop

;-----------------------------------------------

sophisto_tr_4_pt_07:

                ; M 65

        db ctrlbt_duration,45

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      

                ; M 66

	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 67

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait

                ; M 68

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 69

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 70

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 71

        db ctrlbt_duration,45
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 72

        db ctrlbt_duration,0
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
        db ctrlbt_duration,45
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

                ; M 73

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


                ; M 74

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


                ; M 75

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


	db ctrlbt_track_loop

;----------------------------------------------

sophisto_tr_4_pt_08:

                ; M 1/5/9

	db ctrlbt_auto_panner,%11_11_00_00,0
        db ctrlbt_duration,36

	db ctrlbt_volume,$f0
	db $40+$27      
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$27      
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$27      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$27      
	db ctrlbt_wait       
	db ctrlbt_volume,$70
	db $40+$27      
	db ctrlbt_wait       
	db ctrlbt_volume,$50
	db $40+$27      
	db ctrlbt_wait

                ; M 2/6/10

	db ctrlbt_volume,$d0
	db $40+$2b      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$2b      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$2b      
	db ctrlbt_wait      
	db ctrlbt_volume,$70
	db $40+$2b      
	db ctrlbt_wait       
	db ctrlbt_volume,$50
	db $40+$2b      
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db $40+$2b      
	db ctrlbt_wait

                ; M 3/7/11

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
	db ctrlbt_volume,$50
	db $40+$2f      
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db $40+$2f      
	db ctrlbt_wait


                ; M 4/8/12

	db ctrlbt_volume,$d0
	db $40+$2b      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$2b      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$2b      
	db ctrlbt_wait      
	db ctrlbt_volume,$70
	db $40+$2b      
	db ctrlbt_wait       
	db ctrlbt_volume,$50
	db $40+$2b      
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db $40+$2b      
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_11_00_00,0
	db ctrlbt_track_loop

;-------------------------------------------

sophisto_tr_4_pt_09:

                ; M 1/5/9

	db ctrlbt_auto_panner,%11_11_00_00,0
        db ctrlbt_duration,36

	db ctrlbt_volume,$f0
	db $40+$25      
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$25      
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $40+$25      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$25      
	db ctrlbt_wait       
	db ctrlbt_volume,$70
	db $40+$25      
	db ctrlbt_wait       
	db ctrlbt_volume,$50
	db $40+$25      
	db ctrlbt_wait

                ; M 2/6/10

	db ctrlbt_volume,$d0
	db $40+$29      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$29      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$29      
	db ctrlbt_wait      
	db ctrlbt_volume,$70
	db $40+$29      
	db ctrlbt_wait       
	db ctrlbt_volume,$50
	db $40+$29      
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db $40+$29      
	db ctrlbt_wait
	
                ; M 3/7/11

	db ctrlbt_volume,$d0
	db $40+$2d      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$2d      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$2d      
	db ctrlbt_wait      
	db ctrlbt_volume,$70
	db $40+$2d      
	db ctrlbt_wait       
	db ctrlbt_volume,$50
	db $40+$2d      
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db $40+$2d      
	db ctrlbt_wait


                ; M 4/8/12

	db ctrlbt_volume,$d0
	db $40+$29      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$29      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$29      
	db ctrlbt_wait      
        db ctrlbt_duration,30
	db ctrlbt_volume,$b0
	db $40+$29      
	db ctrlbt_wait       
	db ctrlbt_volume,$d0
	db $40+$29      
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $40+$29      
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_11_00_00,0
	db ctrlbt_track_loop

;-------------------------------------------

sophisto_tr_4_pt_10:

                ; M 41

        db ctrlbt_duration,55

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 42

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$c0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
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
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 44

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait

                ; M 45

        db ctrlbt_duration,10
	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
        db ctrlbt_duration,10
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 46

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
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
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_wait

                ; M 48

	db ctrlbt_volume,$f0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$b0
        db ctrlbt_duration,55
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait

	db ctrlbt_track_loop

;--------------------------------------------

sophisto_tr_4_pt_11:

                ; M 98

        db ctrlbt_duration,50

	db ctrlbt_volume,$f0
	db $25
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f      
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$b0
	db $3f
	db ctrlbt_wait

                ; M 99

	db ctrlbt_volume,$f0
	db $25
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$b0
	db $37
	db ctrlbt_wait      
	db ctrlbt_volume,$d0
        db ctrlbt_duration,45
	db $3f
	db ctrlbt_wait       
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_wait       
	db ctrlbt_volume,$80
	db $37
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------

sophisto_tr_4_pt_12:

                ; M 1/5/9

        db ctrlbt_duration,0

        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$f0
	db $40+$23
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 2/6/10

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 3/7/11

	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait

                ; M 4/8/12

        db ctrlbt_hold_mode,01
        db ctrlbt_duration,50
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$c0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait      
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_wait       
        db ctrlbt_duration,0
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_track_loop


sophisto_end:

        end

;-----------------------------------------------
