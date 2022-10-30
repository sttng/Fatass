;***************************************************************************

world1_header:
	dw world1_sequence-world1_header	; sequence index
	dw world1_patterns-world1_header	; patterns index
	dw world1_end-world1_header	; patterns index
	db 1
	db 'WORLD1'					; song name
	db $ff

world1_sequence:

	db ctrlbt_song_speed,$36

	db $00

	db ctrlbt_track_loop


world1_patterns:
	dw world1_tr_1_pt_00-world1_patterns,world1_tr_2_pt_00-world1_patterns,world1_tr_3_pt_00-world1_patterns,world1_tr_4_pt_00-world1_patterns




;--------------------------------------------------

world1_tr_1_pt_00:

                ; M 1

	db ctrlbt_auto_pwm,%00_01_00_11,0
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,0

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_4
	db ctrlbt_portamento,$3f
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,3       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
	db ctrlbt_song_speed,$40
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_00_11,3
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_song_speed,$40
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f      
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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_auto_pwm,%00_01_00_11,3
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_00_11,0      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,3
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$3f
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,3      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111     
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$4f
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

                ; M 6

	db ctrlbt_song_speed,$10
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
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

	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop


;--------------------------------------------------


world1_tr_2_pt_00:

                ; M 1

	db ctrlbt_auto_pwm,%00_01_00_11,0
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_000
        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_fine_tune,12

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_4
	db ctrlbt_portamento,$3f
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,3       
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_00_11,3
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 2


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$2f      
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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_auto_pwm,%00_01_00_11,3
	db ctrlbt_b_5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_auto_pwm,%00_01_00_11,0      
	db ctrlbt_a_5       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,3
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4

	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$3f
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%00_01_00_11,3      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait        
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$02,$4f
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


	db ctrlbt_fine_tune,0
	db ctrlbt_auto_pwm,%00_01_00_11,0
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_portamento,$00
        db ctrlbt_transpose,00
	db ctrlbt_track_loop

;-----------------------------------------------

world1_tr_3_pt_00:

                ; M 1

        db ctrlbt_transpose,12
        db ctrlbt_vibrato_reset
 	db ctrlbt_vibrato,$00,$00
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b0
	db ctrlbt_gain_waveform,$26
	db ctrlbt_porta_from,ctrlbt_e_3
	db ctrlbt_portamento,$76
	db ctrlbt_e_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b1      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b2
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b4
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b5
	db ctrlbt_wait       
	db ctrlbt_portamento,$00
	db ctrlbt_d_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b6
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b8
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_3
	db ctrlbt_portamento,$76
	db ctrlbt_e_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b9
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ba
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bb
	db ctrlbt_wait


                ; M 2


        db ctrlbt_vibrato_reset
        db ctrlbt_vibrato,$02,$3f
	db ctrlbt_waveform,$bc
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bd
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$be
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bf
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b0
	db ctrlbt_portamento,$00      
	db ctrlbt_d_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b2
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b3
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b5
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_g_3
	db ctrlbt_portamento,$96
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b8
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b9
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ba
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bb
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bc
	db ctrlbt_portamento,$00       
	db ctrlbt_a_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bd
	db ctrlbt_wait


                ; M 3

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$be
	db ctrlbt_g_4
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bc
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bf
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$be
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bd
	db ctrlbt_f#4       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bc
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bb
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ba
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b9
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b6
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b5
	db ctrlbt_d_5
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b3
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b2
	db ctrlbt_wait


                ; M 4

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b1
	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_e_5
	db ctrlbt_portamento,$46
	db ctrlbt_e_6
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b2
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b3
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b5
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b6
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b8
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b9
	db ctrlbt_wait        
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ba
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bb
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bc
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bd
	db ctrlbt_gain_waveform,$26
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$be
	db ctrlbt_gain_waveform,$25
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bf
	db ctrlbt_gain_waveform,$24
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$be
	db ctrlbt_gain_waveform,$23
	db ctrlbt_wait

                ; M 5

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bd
	db ctrlbt_gain_waveform,$22
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bc
	db ctrlbt_gain_waveform,$21
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bb
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ba
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$bb
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$ba
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b9
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b8
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b7
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b6
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b5
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b4
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b3
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b2
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b1
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$b0
	db ctrlbt_gain_waveform,$20
	db ctrlbt_wait

                ; M 6

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


        db ctrlbt_transpose,00
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

	db ctrlbt_track_loop


;--------------------------------------------------

world1_tr_4_pt_00:

                ; M 1

	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
        db ctrlbt_duration,45

	db ctrlbt_volume,$f0
	db $3f      
	db ctrlbt_wait
	db ctrlbt_volume,$a0
        db ctrlbt_duration,58
	db $3f      
	db ctrlbt_volume,$e0
	db $3f      
	db ctrlbt_volume,$90
	db $40+$35            
	db ctrlbt_wait
	db ctrlbt_volume,$70 
        db ctrlbt_duration,45
	db $40+$35
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait         
	db ctrlbt_volume,$90
	db $40+$35
	db ctrlbt_volume,$e0
        db ctrlbt_duration,58
	db $3f    
	db ctrlbt_volume,$90
	db $3f          
	db ctrlbt_volume,$c0
	db $3f          
	db ctrlbt_volume,$90
        db ctrlbt_duration,45
	db $3f          
	db ctrlbt_volume,$e0
	db $3f          

                ; M 2
	

	db ctrlbt_volume,$90
	db $40+$31
	db ctrlbt_volume,$e0
	db $3f          
	db ctrlbt_volume,$80
	db $40+$31
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_volume,$90
	db $40+$31
	db ctrlbt_wait
	db ctrlbt_volume,$e0
        db ctrlbt_duration,58
	db $3f    
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$d0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$90
        db ctrlbt_duration,45
	db $40+$32
	db ctrlbt_volume,$d0
	db $3f    
	db ctrlbt_volume,$80
	db $40+$32
	db ctrlbt_wait
  

                ; M 3


	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_volume,$90
	db $40+$32
	db ctrlbt_volume,$e0
        db ctrlbt_duration,58
	db $3f    
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$d0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
        db ctrlbt_duration,45
	db $3f    
	db ctrlbt_volume,$90
	db $40+$34
	db ctrlbt_volume,$d0
	db $40+$34
	db ctrlbt_volume,$a0
	db $40+$34
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db $40+$34
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$34
	db ctrlbt_wait


                ; M 4

        db ctrlbt_duration,0
	db ctrlbt_envelope,%0000_0_111
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$f0
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


        db ctrlbt_hold_mode,01
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_track_loop

world1_end:

        end

;-----------------------------------------------
