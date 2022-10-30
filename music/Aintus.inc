;***************************************************************************

aintus_header:
	dw aintus_sequence-aintus_header	; sequence index
	dw aintus_patterns-aintus_header	; patterns index
	dw aintus_end-aintus_header	; patterns index
	db 14
	db 'AINTUS'					; song name
	db $ff

aintus_sequence:

	db ctrlbt_song_speed,$20
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_panning,%11111111
	db $00
	db ctrlbt_song_speed,$40
	db ctrlbt_shuffle,$40,$a3
	db $01
	db $02
	db $03
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$26
	db $04
	db ctrlbt_song_speed,$30
	db ctrlbt_shuffle,$40,$33
	db $05
	db ctrlbt_song_speed,$36
	db ctrlbt_shuffle,$40,$53
	db $06
	db ctrlbt_song_speed,$38
	db ctrlbt_shuffle,$40,$53
	db $07
	db ctrlbt_shuffle,$40,$a3
	db ctrlbt_song_speed,$40
	db $08
	db $09
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$26
	db $0a
	db ctrlbt_song_speed,$30
	db ctrlbt_shuffle,$40,$33
	db $0b	
	db ctrlbt_song_speed,$36
	db ctrlbt_shuffle,$40,$53
	db $0c	
	db ctrlbt_song_speed,$38
	db ctrlbt_shuffle,$40,$93
	db $0d	


	db ctrlbt_track_loop

aintus_patterns:
	dw aintus_tr_1_pt_03-aintus_patterns,aintus_tr_2_pt_05-aintus_patterns,aintus_tr_3_pt_04-aintus_patterns,aintus_tr_4_pt_02-aintus_patterns
	dw aintus_tr_1_pt_00-aintus_patterns,aintus_tr_2_pt_00-aintus_patterns,aintus_tr_3_pt_07-aintus_patterns,aintus_tr_4_pt_03-aintus_patterns
	dw aintus_tr_1_pt_00-aintus_patterns,aintus_tr_2_pt_00-aintus_patterns,aintus_tr_3_pt_00-aintus_patterns,aintus_tr_4_pt_05-aintus_patterns
	dw aintus_tr_1_pt_00-aintus_patterns,aintus_tr_2_pt_00-aintus_patterns,aintus_tr_3_pt_06-aintus_patterns,aintus_tr_4_pt_00-aintus_patterns
	dw aintus_tr_1_pt_01-aintus_patterns,aintus_tr_2_pt_01-aintus_patterns,aintus_tr_3_pt_01-aintus_patterns,aintus_tr_4_pt_04-aintus_patterns
	dw aintus_tr_1_pt_01-aintus_patterns,aintus_tr_2_pt_02-aintus_patterns,aintus_tr_3_pt_01-aintus_patterns,aintus_tr_4_pt_04-aintus_patterns
	dw aintus_tr_1_pt_02-aintus_patterns,aintus_tr_2_pt_03-aintus_patterns,aintus_tr_3_pt_02-aintus_patterns,aintus_tr_4_pt_01-aintus_patterns
	dw aintus_tr_1_pt_01-aintus_patterns,aintus_tr_2_pt_04-aintus_patterns,aintus_tr_3_pt_03-aintus_patterns,aintus_tr_4_pt_01-aintus_patterns
	dw aintus_tr_1_pt_00-aintus_patterns,aintus_tr_2_pt_00-aintus_patterns,aintus_tr_3_pt_05-aintus_patterns,aintus_tr_4_pt_03-aintus_patterns
	dw aintus_tr_1_pt_00-aintus_patterns,aintus_tr_2_pt_00-aintus_patterns,aintus_tr_3_pt_00-aintus_patterns,aintus_tr_4_pt_00-aintus_patterns
	dw aintus_tr_1_pt_02-aintus_patterns,aintus_tr_2_pt_03-aintus_patterns,aintus_tr_3_pt_01-aintus_patterns,aintus_tr_4_pt_05-aintus_patterns
	dw aintus_tr_1_pt_01-aintus_patterns,aintus_tr_2_pt_04-aintus_patterns,aintus_tr_3_pt_01-aintus_patterns,aintus_tr_4_pt_05-aintus_patterns
	dw aintus_tr_1_pt_02-aintus_patterns,aintus_tr_2_pt_03-aintus_patterns,aintus_tr_3_pt_02-aintus_patterns,aintus_tr_4_pt_04-aintus_patterns
	dw aintus_tr_1_pt_01-aintus_patterns,aintus_tr_2_pt_01-aintus_patterns,aintus_tr_3_pt_03-aintus_patterns,aintus_tr_4_pt_04-aintus_patterns

;-------------------------------------------------------------------------------------------------

aintus_tr_1_pt_00:

                  ; M 1-4 
                  ; intro

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$f0


     db ctrlbt_g_3           
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_a#4
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_auto_pwm,%11_00_11_01,1
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%10_01_11_01,0
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_f_3
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait

           ; Meas 2

     db ctrlbt_c_4           
     db ctrlbt_wait
     db ctrlbt_a#5
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_auto_pwm,%11_00_11_01,1
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%11_00_11_01,0
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_e_3
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait


           ; Meas 3

     db ctrlbt_d_4           
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_e_3
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait


           ; Meas 4

     db ctrlbt_g_3          
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#4
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_auto_pwm,%11_00_11_01,1
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%11_00_11_01,0
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait


           ; Meas 5

     db ctrlbt_c_4           
     db ctrlbt_wait
     db ctrlbt_a#5
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_auto_pwm,%11_00_11_01,1
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_auto_pwm,%11_00_11_01,0
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_e_3
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait


           ; Meas 6

     db ctrlbt_d_4           
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait 
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait
     db ctrlbt_e_3
     db ctrlbt_wait
     db ctrlbt_a#3
     db ctrlbt_wait


     db ctrlbt_track_loop

;-------------------------------------------------------------------------------------------------

aintus_tr_1_pt_01:


	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$f0

           ; Meas 13


     db ctrlbt_d_3           
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
     db ctrlbt_f_4
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_f_4
     db ctrlbt_wait

           ; Meas 14

     db ctrlbt_c_4           
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_b_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait


           ; Meas 15

     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
     db ctrlbt_f_4
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_f_4
     db ctrlbt_wait


           ; Meas 16

     db ctrlbt_c_4          
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_b_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_d_4
     db ctrlbt_wait


     db ctrlbt_track_loop

;-------------------------------------------------------------------------------------------------

aintus_tr_1_pt_02:


	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$f0

           ; Meas 17


     db ctrlbt_d_3           
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
     db ctrlbt_f_4
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_f_4
     db ctrlbt_wait

           ; Meas 18

     db ctrlbt_c_4           
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_b_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait


           ; Meas 19

     db ctrlbt_c_3
     db ctrlbt_wait
     db ctrlbt_c_3
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
     db ctrlbt_f_4
     db ctrlbt_wait
     db ctrlbt_c_3
     db ctrlbt_wait
     db ctrlbt_d_4
     db ctrlbt_wait
     db ctrlbt_f_4
     db ctrlbt_wait


           ; Meas 20

     db ctrlbt_c_4          
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_b_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_d_3
     db ctrlbt_wait
     db ctrlbt_c_4
     db ctrlbt_wait
     db ctrlbt_d_4
     db ctrlbt_wait

 db ctrlbt_track_loop

;---------------------------------------------

aintus_tr_1_pt_03:


	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_volume,$c0
        db ctrlbt_hold_mode,00

           ; Meas 17


     db ctrlbt_note_off
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
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

        db ctrlbt_hold_mode,01
     db ctrlbt_track_loop

;----------------------------------------------

aintus_tr_2_pt_00:

           ; Meas 1

        db ctrlbt_hold_mode,00
        db ctrlbt_volume,$80
	db ctrlbt_pulse_width,%10_000000
        db ctrlbt_transpose,12    

	db ctrlbt_envelope,%0000_0_110
     db ctrlbt_d_5           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_f_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_a#5
     db ctrlbt_wait
     db ctrlbt_a_5
     db ctrlbt_wait

           ; Meas 2

     db ctrlbt_g_5           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_5
	db ctrlbt_portamento,$3f
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_f_5           
     db ctrlbt_wait
     db ctrlbt_g_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_f_5
	db ctrlbt_portamento,$3f
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
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_f_5
     db ctrlbt_wait
     db ctrlbt_g_5
     db ctrlbt_wait


           ; Meas 4

     db ctrlbt_a#5         
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_a#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 5

	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_c_6          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#5
	db ctrlbt_portamento,$3f
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 6

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_a#5           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait 
	db ctrlbt_envelope,%0000_0_110
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#5
	db ctrlbt_portamento,$3f
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_010
     db ctrlbt_a#5
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
        db ctrlbt_transpose,00

     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_2_pt_01:


	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$c0
        db ctrlbt_hold_mode,00
	db ctrlbt_arpeggio,$0c,$0c,5
	db ctrlbt_auto_panner,%11_10_11_01,3 

           ; Meas 13

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 14

     db ctrlbt_wait          
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_a_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait


           ; Meas 15

     db ctrlbt_wait           
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
     db ctrlbt_a_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 16

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_arpeggio,$00,$00,0

     db ctrlbt_track_loop

;-------------------------------------------------------------------------------------------------

aintus_tr_2_pt_02:


	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_envelope,%0000_0_110
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$c0
	db ctrlbt_auto_panner,%11_10_11_01,3 
	db ctrlbt_arpeggio,$0c,$0c,3

           ; Meas 17


     db ctrlbt_e_5          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 18

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 19

     db ctrlbt_e_5           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 20

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_arpeggio,$00,$00,0

     db ctrlbt_track_loop

;-------------------------------------------------------------------------------------------------

aintus_tr_2_pt_03:

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_envelope,%0000_0_011
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$c0
	db ctrlbt_auto_panner,%11_10_11_01,3 
	db ctrlbt_arpeggio,$0c,$0c,1

           ; Meas 21

     db ctrlbt_wait         
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_110
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 22

     db ctrlbt_wait          
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_a_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait

           ; Meas 23

     db ctrlbt_wait           
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_a_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 24

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
     db ctrlbt_auto_panner,%11_10_11_01,0
     db ctrlbt_arpeggio,$00,$00,0
     db ctrlbt_track_loop

;-------------------------------------------------------------------------------------------------

aintus_tr_2_pt_04:

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$c0
	db ctrlbt_auto_panner,%11_10_11_01,3 
	db ctrlbt_arpeggio,$0c,$0c,1

           ; Meas 25

     db ctrlbt_e_5          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 26

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 27

     db ctrlbt_e_5           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_c_5
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_100
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 28

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
     db ctrlbt_c_5
     db ctrlbt_wait
     db ctrlbt_d_5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait
     db ctrlbt_e_5
     db ctrlbt_wait
     db ctrlbt_f#5
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
    db ctrlbt_auto_panner,%11_10_11_01,0
    db ctrlbt_arpeggio,$00,$00,0
    db ctrlbt_track_loop

;---------------------------------------------

aintus_tr_2_pt_05:

	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_volume,$c0
        db ctrlbt_hold_mode,00

           ; Meas 25

     db ctrlbt_note_off
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
     db ctrlbt_e_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_3_pt_00:
               
           ; Meas 1

        db ctrlbt_hold_mode,0
        db ctrlbt_transpose,12
	db ctrlbt_portamento,$74
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 2

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_d_6          
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
	db ctrlbt_restart_envelope
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 5

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_g_6
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 6

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait	
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

     db ctrlbt_transpose,0
     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_3_pt_01:

           ; Meas 13  Blank

	db ctrlbt_waveform,$0d
        db ctrlbt_hold_mode,0

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 14

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 15

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 16

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
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

aintus_tr_3_pt_02:

           ; Meas 21  

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
        db ctrlbt_hold_mode,0

     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$74
     db ctrlbt_d_7
     db ctrlbt_wait
;	db ctrlbt_restart_envelope
;	db ctrlbt_waveform,$03
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
;	db ctrlbt_restart_envelope
;	db ctrlbt_waveform,$01
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,0
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_d_6
     db ctrlbt_wait

           ; Meas 22

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$74
     db ctrlbt_b_6
     db ctrlbt_wait
;	db ctrlbt_restart_envelope
;	db ctrlbt_waveform,$03
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait

           ; Meas 23

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
        db ctrlbt_hold_mode,0
     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$74
     db ctrlbt_d_7
     db ctrlbt_wait
;	db ctrlbt_restart_envelope
;	db ctrlbt_waveform,$03
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_hold_mode,0
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_d_6
     db ctrlbt_wait

           ; Meas 24

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_b_6
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_b_6
     db ctrlbt_wait

     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_3_pt_03:

           ; Meas 25  

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0d
	db ctrlbt_gain_waveform,$0d
        db ctrlbt_hold_mode,0

     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$74
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
     db ctrlbt_d_5
     db ctrlbt_wait

           ; Meas 26

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$74
     db ctrlbt_b_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 27

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$74
     db ctrlbt_d_7
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
     db ctrlbt_d_5
     db ctrlbt_wait

           ; Meas 28

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_d_6
     db ctrlbt_wait
     db ctrlbt_f_6
     db ctrlbt_wait
     db ctrlbt_f_5
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_b_5
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
     db ctrlbt_b_5
     db ctrlbt_wait

     db ctrlbt_track_loop

;------------------------------------------------

aintus_tr_3_pt_04:

               
           ; Meas 1

        db ctrlbt_hold_mode,0

	db ctrlbt_portamento,$50
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0d
     db ctrlbt_wait
     db ctrlbt_wait

     db ctrlbt_portamento,$00
     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_3_pt_05:

               
           ; Meas 1 Blank

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


           ; Meas 2

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 3

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 4

     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 5

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 6

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
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

aintus_tr_3_pt_06:

               
           ; Meas 1

        db ctrlbt_hold_mode,0
        db ctrlbt_transpose,12
	db ctrlbt_portamento,$74
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_d_6           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_c_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 2

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_g_5
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_d_6          
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_a#6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 5

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_a_6
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_g_6
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

           ; Meas 6

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait	
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$01
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$0a
	db ctrlbt_gain_waveform,$0a
     db ctrlbt_wait
     db ctrlbt_wait

     db ctrlbt_transpose,0
     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_3_pt_07:

           ; Meas 1 Blank

        db ctrlbt_hold_mode,0
        db ctrlbt_transpose,12
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a               
	db ctrlbt_portamento,$74

     db ctrlbt_b_6
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_g_6
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a               
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a               
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
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

           ; Meas 2

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
	db ctrlbt_gain_waveform,$02
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$01
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$01
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


           ; Meas 3

     db ctrlbt_wait          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


           ; Meas 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$03
	db ctrlbt_gain_waveform,$0a               
     db ctrlbt_d_7
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_a#6
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a               
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$0a               
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$09
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

           ; Meas 5

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
	db ctrlbt_gain_waveform,$02
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$02
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$01
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_gain_waveform,$01
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

           ; Meas 6

     db ctrlbt_wait           
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait	
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait


     db ctrlbt_portamento,$00
     db ctrlbt_transpose,0
     db ctrlbt_track_loop

;-------------------------------------------------

; noise perc in "d"


;       db $40+$33       perc in "a#"

;       db $40+$37       perc in "a#" octave higher

;	db ctrlbt_volume,$90 

;       db $40+$2c          ;  kik  "D" Low

;	db ctrlbt_volume,$80
;       db $40+$30          ;  Perc middle

;       db $40+$34          ;  snare High

;	db ctrlbt_envelope,%0000_0_001
;        db $3f              ; hat

;       db $37               Snare mode "_0_"

;--------------------------------------------------------

aintus_tr_4_pt_00:


           ; Meas 1

        db ctrlbt_duration,50
        db ctrlbt_volume,$f0
     db $40+$24
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $3d   
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$33
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $3d 
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d
     db ctrlbt_wait

           ; Meas 2

        db ctrlbt_volume,$a0
     db $40+$33               
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $3d   
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$f0 
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$24
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d 
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $3d
     db ctrlbt_wait
			
           ; Meas 3

        db ctrlbt_volume,$90
     db $37               
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d   
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$33 
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d 
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d
        db ctrlbt_volume,$90
     db $37
			
           ; Meas 4

        db ctrlbt_volume,$f0
     db $40+$24
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $3d   
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$33
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d 
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0
     db $3d
     db ctrlbt_wait

           ; Meas 5

        db ctrlbt_volume,$a0
     db $40+$33               
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d   
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$24
     db ctrlbt_wait
        db ctrlbt_volume,$f0
	db ctrlbt_song_speed,$38
     db $3d 
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d
     db ctrlbt_wait
			
           ; Meas 6

	db ctrlbt_song_speed,$36
        db ctrlbt_volume,$90
     db $37               
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $3d   
     db ctrlbt_wait
        db ctrlbt_volume,$90
	db ctrlbt_song_speed,$34
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0
     db $3d       
     db ctrlbt_wait
	db ctrlbt_song_speed,$32
        db ctrlbt_volume,$f0
     db $40+$24
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $37
     db ctrlbt_wait
	db ctrlbt_song_speed,$30
        db ctrlbt_volume,$b0
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d
        db ctrlbt_volume,$90
     db $37

	db ctrlbt_song_speed,$40		
        db ctrlbt_track_loop

;-------------------------------------------------

;       db $40+$2c          ;  kik  "D" Low

;	db ctrlbt_volume,$80
;       db $40+$30          ;  Perc middle

;       db $40+$34          ;  snare High

;------------------------------------------------

aintus_tr_4_pt_01:


           ; Meas 13

        db ctrlbt_duration,40
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$34         
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$30         
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$34          
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
        db ctrlbt_duration,0
     db $3d       
     db ctrlbt_wait


           ; Meas 14

        db ctrlbt_duration,40
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
        db ctrlbt_duration,0
     db $3d       
     db ctrlbt_wait
        db ctrlbt_duration,40
        db ctrlbt_volume,$90
     db $40+$34          
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$30          
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34          
        db ctrlbt_volume,$c0
     db $3d 
        db ctrlbt_volume,$f0
     db $3d 
        db ctrlbt_volume,$d0 
     db $3d 


           ; Meas 15

        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$34          
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$30          
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25         
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34         
     db ctrlbt_wait
        db ctrlbt_volume,$d0
        db ctrlbt_duration,0
     db $3d 
     db ctrlbt_wait

           ; Meas 16

        db ctrlbt_duration,40
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34          
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$30          
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34         
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$30          
        db ctrlbt_volume,$c0
     db $40+$34          

     db ctrlbt_track_loop

;---------------------------------------------

aintus_tr_4_pt_02:

           ; Meas Intro

        db ctrlbt_hold_mode,00
        db ctrlbt_duration,0
        db ctrlbt_volume,$c0
     db $40+$30          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait      
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$34          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$70
     db $40+$38
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait

        db ctrlbt_hold_mode,01
     db ctrlbt_track_loop

;---------------------------------------------

aintus_tr_4_pt_03:

           ; Meas 1

        db ctrlbt_duration,0
        db ctrlbt_volume,$f0
     db $3d            
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_duration,20
        db ctrlbt_volume,$a0
     db $40+$33
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d
     db ctrlbt_wait

           ; Meas 2

        db ctrlbt_volume,$a0
     db $40+$33               
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$f0
        db ctrlbt_duration,0
     db $3d
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
        db ctrlbt_duration,20
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
			
           ; Meas 3

        db ctrlbt_volume,$90
     db $37               
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$33 
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
			
           ; Meas 4

        db ctrlbt_duration,0
        db ctrlbt_volume,$f0
     db $3d            
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_duration,20
        db ctrlbt_volume,$a0
     db $40+$33
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d
     db ctrlbt_wait

           ; Meas 5


        db ctrlbt_volume,$90
     db $37               
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$80
     db $40+$34
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
        db ctrlbt_hold_mode,00
        db ctrlbt_duration,0
        db ctrlbt_volume,$f0
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$96
     db $40+$25
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$40
	db ctrlbt_envelope,%0000_1_100
     db $34
     db ctrlbt_wait
			

           ; Meas 6

     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,30
        db ctrlbt_volume,$80
	db ctrlbt_auto_panner,%11_10_11_01,0
	db ctrlbt_restart_envelope
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_portamento,$00
     db $40+$37
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
        db ctrlbt_volume,$a0
        db ctrlbt_duration,30
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $37
     db ctrlbt_wait
        db ctrlbt_volume,$b0
        db ctrlbt_duration,40
     db $37
        db ctrlbt_volume,$90
     db $37
        db ctrlbt_volume,$b0
     db $3d
        db ctrlbt_volume,$90
     db $37
			
     db ctrlbt_track_loop

;-----------------------------------------------

aintus_tr_4_pt_04:


           ; Meas 13

        db ctrlbt_duration,20
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$34         
     db ctrlbt_wait
     db ctrlbt_wait     
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
     db ctrlbt_wait       
     db ctrlbt_wait
        db ctrlbt_volume,$b0
     db $40+$34          
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
        db ctrlbt_duration,0
     db $3d       
     db ctrlbt_wait


           ; Meas 14

        db ctrlbt_duration,20
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_duration,20
        db ctrlbt_volume,$90
     db $40+$34          
     db ctrlbt_wait
     db ctrlbt_wait      
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34          
        db ctrlbt_volume,$c0
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d 
        db ctrlbt_volume,$d0 
     db ctrlbt_wait


           ; Meas 15

        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d       
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $40+$34          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25         
     db ctrlbt_wait
     db ctrlbt_wait      
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34         
     db ctrlbt_wait
        db ctrlbt_volume,$d0
        db ctrlbt_duration,0
     db $3d 
     db ctrlbt_wait

           ; Meas 16

        db ctrlbt_duration,20
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34          
     db ctrlbt_wait
     db ctrlbt_wait      
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $40+$25          
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$34         
     db ctrlbt_wait
     db ctrlbt_wait      
     db ctrlbt_wait      

     db ctrlbt_track_loop

;-------------------------------------------------------

aintus_tr_4_pt_05:


           ; Meas 1 Copy of 3

        db ctrlbt_hold_mode,00
        db ctrlbt_duration,0
        db ctrlbt_volume,$f0
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_restart_envelope
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_portamento,$7f
     db $40+$25
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$30
	db ctrlbt_envelope,%0000_1_100
     db $40+$34
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_portamento,$00
	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_hold_mode,01
        db ctrlbt_volume,$80
        db ctrlbt_duration,30
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0 
     db $3d
     db ctrlbt_wait

           ; Meas 2

        db ctrlbt_volume,$a0
     db $40+$33               
     db ctrlbt_wait
     db $3d
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$f0
        db ctrlbt_duration,0
     db $3d
     db ctrlbt_wait
     db $3d
     db ctrlbt_wait
        db ctrlbt_duration,30
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
			
           ; Meas 3

        db ctrlbt_volume,$90
     db $37               
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
        db ctrlbt_volume,$a0
     db $40+$33 
     db ctrlbt_wait
     db $3d
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
			
           ; Meas 4

        db ctrlbt_duration,0
        db ctrlbt_volume,$f0
     db $3d            
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
        db ctrlbt_duration,30
        db ctrlbt_volume,$a0
     db $40+$33
     db ctrlbt_wait
     db $3d
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
        db ctrlbt_volume,$80
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$f0
     db $3d
     db ctrlbt_wait

           ; Meas 5

        db ctrlbt_volume,$a0
     db $40+$33               
     db ctrlbt_wait
     db $3d
     db ctrlbt_wait
        db ctrlbt_volume,$90
     db $37
     db ctrlbt_wait
     db $3d
     db ctrlbt_wait
        db ctrlbt_volume,$f0
        db ctrlbt_duration,0
     db $3d
     db ctrlbt_wait
     db ctrlbt_wait 
     db ctrlbt_wait
        db ctrlbt_duration,30
        db ctrlbt_volume,$90
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$d0
     db $3d
     db ctrlbt_wait
			

           ; Meas 6


        db ctrlbt_volume,$a0
     db $40+$37
     db ctrlbt_wait
        db ctrlbt_volume,$c0
     db $3d
     db ctrlbt_wait
        db ctrlbt_hold_mode,00
        db ctrlbt_duration,0
        db ctrlbt_volume,$f0
	db ctrlbt_auto_panner,%11_10_11_01,1
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$7f
     db $40+$25
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait
     db ctrlbt_wait   
     db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
        db ctrlbt_duration,55
	db ctrlbt_auto_panner,%11_10_11_01,0
        db ctrlbt_hold_mode,01
        db ctrlbt_volume,$f0
     db $40+$25
        db ctrlbt_volume,$b0
     db $40+$25
        db ctrlbt_volume,$f0
     db $40+$25
        db ctrlbt_volume,$d0
     db $40+$25
			
     db ctrlbt_track_loop

aintus_end:

     end

;--------------------------------------------------
