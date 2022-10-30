;***************************************************************************


seven8_header:
	dw seven8_sequence-seven8_header	; sequence index
	dw seven8_patterns-seven8_header	; patterns index
	dw seven8_end-seven8_header		; song length
	db 23 ; # of patterns

	db 'SEVEN8'				; song name
	db $ff

seven8_sequence:

	db ctrlbt_panning,%11111111
        db ctrlbt_transpose,0
	db ctrlbt_shuffle,$05,$30
	db ctrlbt_song_speed,$22
	db $00	
	db $01
	db ctrlbt_shuffle,$00,$00
	db ctrlbt_song_speed,$28
	db $02		
	db ctrlbt_song_speed,$22
	db ctrlbt_shuffle,$05,$30
	db $03	
	db ctrlbt_shuffle,$00,$00
        db ctrlbt_transpose,0
	db ctrlbt_song_speed,$28
	db $04	 
	db $05
	db $06
	db $07
	db $08
        db ctrlbt_transpose,0
	db $09
        db ctrlbt_transpose,-1
	db ctrlbt_song_speed,$28
	db $0a
	db $0b
        db ctrlbt_transpose,0
	db ctrlbt_song_speed,$28
	db $0c
	db $0d
        db ctrlbt_transpose,3
	db $0e
	db $0f
        db ctrlbt_transpose,-1
	db $10
	db $11
        db ctrlbt_transpose,2
	db $12
	db $13
        db ctrlbt_transpose,0
	db $14
	db $15
	db $16

	db ctrlbt_track_loop

seven8_patterns:
	dw seven8_tr_1_pt_06-seven8_patterns,seven8_tr_2_pt_06-seven8_patterns,seven8_tr_3_pt_09-seven8_patterns,seven8_tr_4_pt_06-seven8_patterns
	dw seven8_tr_1_pt_00-seven8_patterns,seven8_tr_2_pt_00-seven8_patterns,seven8_tr_3_pt_00-seven8_patterns,seven8_tr_4_pt_07-seven8_patterns
	dw seven8_tr_1_pt_05-seven8_patterns,seven8_tr_2_pt_01-seven8_patterns,seven8_tr_3_pt_01-seven8_patterns,seven8_tr_4_pt_04-seven8_patterns
	dw seven8_tr_1_pt_00-seven8_patterns,seven8_tr_2_pt_00-seven8_patterns,seven8_tr_3_pt_00-seven8_patterns,seven8_tr_4_pt_00-seven8_patterns
	dw seven8_tr_1_pt_05-seven8_patterns,seven8_tr_2_pt_01-seven8_patterns,seven8_tr_3_pt_01-seven8_patterns,seven8_tr_4_pt_04-seven8_patterns
	dw seven8_tr_1_pt_01-seven8_patterns,seven8_tr_2_pt_01-seven8_patterns,seven8_tr_3_pt_04-seven8_patterns,seven8_tr_4_pt_01-seven8_patterns
	dw seven8_tr_1_pt_02-seven8_patterns,seven8_tr_2_pt_02-seven8_patterns,seven8_tr_3_pt_02-seven8_patterns,seven8_tr_4_pt_02-seven8_patterns
	dw seven8_tr_1_pt_03-seven8_patterns,seven8_tr_2_pt_03-seven8_patterns,seven8_tr_3_pt_03-seven8_patterns,seven8_tr_4_pt_03-seven8_patterns
	dw seven8_tr_1_pt_07-seven8_patterns,seven8_tr_2_pt_08-seven8_patterns,seven8_tr_3_pt_08-seven8_patterns,seven8_tr_4_pt_02-seven8_patterns
	dw seven8_tr_1_pt_02-seven8_patterns,seven8_tr_2_pt_02-seven8_patterns,seven8_tr_3_pt_02-seven8_patterns,seven8_tr_4_pt_02-seven8_patterns
	dw seven8_tr_1_pt_02-seven8_patterns,seven8_tr_2_pt_02-seven8_patterns,seven8_tr_3_pt_08-seven8_patterns,seven8_tr_4_pt_02-seven8_patterns
	dw seven8_tr_1_pt_03-seven8_patterns,seven8_tr_2_pt_07-seven8_patterns,seven8_tr_3_pt_06-seven8_patterns,seven8_tr_4_pt_03-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_05-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_07-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_05-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_07-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_05-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_07-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_05-seven8_patterns,seven8_tr_4_pt_05-seven8_patterns
	dw seven8_tr_1_pt_04-seven8_patterns,seven8_tr_2_pt_05-seven8_patterns,seven8_tr_3_pt_07-seven8_patterns,seven8_tr_4_pt_08-seven8_patterns
	dw seven8_tr_1_pt_06-seven8_patterns,seven8_tr_2_pt_06-seven8_patterns,seven8_tr_3_pt_00-seven8_patterns,seven8_tr_4_pt_07-seven8_patterns
	dw seven8_tr_1_pt_00-seven8_patterns,seven8_tr_2_pt_00-seven8_patterns,seven8_tr_3_pt_09-seven8_patterns,seven8_tr_4_pt_06-seven8_patterns
	dw seven8_tr_1_pt_00-seven8_patterns,seven8_tr_2_pt_00-seven8_patterns,seven8_tr_3_pt_00-seven8_patterns,seven8_tr_4_pt_00-seven8_patterns



seven8_tr_1_pt_00:

   
                ; 14 events/measure
                ; M 1


	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0
	db ctrlbt_f_3      
	db ctrlbt_wait  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_f_3
	db ctrlbt_volume,$f0
	db ctrlbt_wait    
	db ctrlbt_f_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0        
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_a#3
	db ctrlbt_volume,$f0     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_1_100
	db ctrlbt_volume,$30
	db ctrlbt_d#3 
	db ctrlbt_wait   


                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_f_3      
	db ctrlbt_wait        
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3      
	db ctrlbt_wait
	db ctrlbt_d#4 
	db ctrlbt_wait


                ; M 3

	db ctrlbt_volume,$f0
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0 
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_g#3
	db ctrlbt_wait    
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_d#3
	db ctrlbt_wait 
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_g#3      
	db ctrlbt_wait        
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_a#3     
	db ctrlbt_g#3
	db ctrlbt_wait 
	db ctrlbt_wait

                ; M 5

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0        
	db ctrlbt_f_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_g_3     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_1_100
	db ctrlbt_volume,$30
	db ctrlbt_c_3
	db ctrlbt_wait


                ; M 6

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0        
	db ctrlbt_f_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_volume,$c0
	db ctrlbt_c_4


                ; M 7


	db ctrlbt_volume,$f0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0  
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_volume,$f0   
	db ctrlbt_d_3      
	db ctrlbt_wait        
	db ctrlbt_f_3      
	db ctrlbt_wait
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_volume,$c0
	db ctrlbt_c_3


                ; M 8

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0    
	db ctrlbt_f_3      
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_f_3 
	db ctrlbt_wait


	db ctrlbt_track_loop


;---------------------------------------------------



seven8_tr_1_pt_01:

                ; 10 events/measure                
                ; M 9

	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$d0
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_c_3      
	db ctrlbt_wait      
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait


                ; M 10

	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait

                ; M 11

	db ctrlbt_c_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait


                ; M 12

	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait


                ; M 13

	db ctrlbt_c_3      
	db ctrlbt_wait      
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_d#3
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait


                ; M 14

	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait


                ; M 15

	db ctrlbt_c_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait


                ; M 16

	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_g_3      
	db ctrlbt_wait        
	db ctrlbt_d#3     
	db ctrlbt_wait



	db ctrlbt_track_loop


;---------------------------------------------------


seven8_tr_1_pt_02:

                ; 16 events/measure
                ; M 17

	db ctrlbt_auto_pwm,%10_01_10_01,1
;	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_song_speed,$32
	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    
   

                ; M 18

	db ctrlbt_song_speed,$33
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait    
   



                ; M 19

	db ctrlbt_song_speed,$34
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    
   

                ; M 20

	db ctrlbt_song_speed,$35
	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    


                ; M 21

	db ctrlbt_song_speed,$36
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait    
      

                ; M 22

	db ctrlbt_song_speed,$37
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    
      

                ; M 23

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    

	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_track_loop


;-------------------------------------------

seven8_tr_1_pt_03:

                ; 16 events/measure

                ; M 24

	db ctrlbt_auto_pwm,%10_01_10_01,1
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait    


                ; M 25

	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    


                ; M 26

	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_a_3     
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    


                ; M 27

	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_f_3      
	db ctrlbt_wait       
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_f_3     
	db ctrlbt_wait      
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    


                ; M 28

	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_f_3     
	db ctrlbt_wait      
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    


                ; M 29

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_g_3
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


                ; M 30

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_note_off
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_note_off      
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    


                ; M 31

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_note_off      
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_note_off    


                ; M 32

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait    
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    


	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_track_loop


;---------------------------------------------------

;---------------------------------------------------



seven8_tr_1_pt_04:

                ; 10 events/measure                
                ; M 9


	db ctrlbt_wait      
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

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait        
	db ctrlbt_wait    
	db ctrlbt_wait

                ; M 11

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
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
	db ctrlbt_wait     
	db ctrlbt_wait        
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 13

	db ctrlbt_wait      
	db ctrlbt_wait      
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


                ; M 15

	db ctrlbt_wait      
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

	db ctrlbt_wait      
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

;-------------------------------------------------
;---------------------------------------------------


seven8_tr_1_pt_05:

                ; 10 events/measure                
                ; M 9

	db ctrlbt_auto_panner,%11_01_11_10,5
        db ctrlbt_transpose,36
	db ctrlbt_auto_pwm,%10_01_10_01,1
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_volume,$40
;	db ctrlbt_pulse_width,%11_000000
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


                ; M 10

	db ctrlbt_volume,$40
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

                ; M 11

	db ctrlbt_volume,$40
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


                ; M 12

	db ctrlbt_volume,$40
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


                ; M 13

	db ctrlbt_volume,$40
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


                ; M 14

	db ctrlbt_volume,$40
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


                ; M 15

	db ctrlbt_volume,$40
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


                ; M 16

	db ctrlbt_volume,$40
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_volume,$70        
	db ctrlbt_d#3     
	db ctrlbt_wait

	db ctrlbt_auto_panner,%11_11_11_11,0
        db ctrlbt_transpose,0
	db ctrlbt_auto_pwm,%10_01_10_01,0


	db ctrlbt_track_loop


;---------------------------------------------------

seven8_tr_1_pt_06:

   
                ; 14 events/measure
                ; M 1


	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0
	db ctrlbt_f_3      
	db ctrlbt_wait  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_volume,$c0        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0     
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_1_100
	db ctrlbt_volume,$30
	db ctrlbt_d#3 
	db ctrlbt_wait   


                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_f_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_wait      
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 3

	db ctrlbt_volume,$f0
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_g#4
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_volume,$c0       
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_wait

                ; M 4

	db ctrlbt_volume,$f0
	db ctrlbt_f_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_wait      
	db ctrlbt_wait        
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_wait

                ; M 5

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0
	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_volume,$c0        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_wait    
	db ctrlbt_wait
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_1_100
	db ctrlbt_volume,$30
	db ctrlbt_c_3
	db ctrlbt_wait


                ; M 6

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_volume,$f0    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$c0        
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_volume,$f0
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_volume,$c0
	db ctrlbt_wait


                ; M 7


	db ctrlbt_volume,$f0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_volume,$c0  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_volume,$f0   
	db ctrlbt_wait      
	db ctrlbt_wait        
	db ctrlbt_f_3      
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait 
	db ctrlbt_volume,$c0
	db ctrlbt_wait


                ; M 8

	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_volume,$c0
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$f0    
	db ctrlbt_f_3      
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_volume,$c0
	db ctrlbt_f_3 
	db ctrlbt_wait


	db ctrlbt_track_loop


;---------------------------------------------------
seven8_tr_1_pt_07:

                ; 16 events/measure
                ; M 17

	db ctrlbt_auto_pwm,%10_01_10_01,1
;	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_song_speed,$32
	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    
   

                ; M 18

	db ctrlbt_song_speed,$33
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait    
   



                ; M 19

	db ctrlbt_song_speed,$34
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    
   

                ; M 20

	db ctrlbt_song_speed,$35
	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    


                ; M 21

	db ctrlbt_song_speed,$36
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_4     
	db ctrlbt_wait      
	db ctrlbt_c_3
	db ctrlbt_wait
	db ctrlbt_c_4
	db ctrlbt_wait    
      

                ; M 22

	db ctrlbt_song_speed,$37
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_d_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait      
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait    
      

                ; M 23

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_3
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait    
	db ctrlbt_b_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait    

	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_track_loop



;----------------------------------------------------

seven8_tr_2_pt_00:

   
                ; 14 events/measure
                ; M 1

	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_transpose,24
	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$80

	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_c_4      
	db ctrlbt_wait      
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_porta_from,ctrlbt_a#3
	db ctrlbt_portamento,$1a
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_f_3
	db ctrlbt_wait   
	db ctrlbt_wait     
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_auto_pwm,%10_01_10_01,1
	db ctrlbt_g#3 
	db ctrlbt_wait  


                ; M 2

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_c_4
	db ctrlbt_wait    
	db ctrlbt_g#3     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111    
	db ctrlbt_f_3     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_f_3 
	db ctrlbt_wait


                ; M 3

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_envelope,%0000_0_010
	db ctrlbt_g#3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_c_4     
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,1
;	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_wait 
	db ctrlbt_wait

                ; M 4

	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111     
	db ctrlbt_g#3      
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_g#3    
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait

                ; M 5

	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_porta_from,ctrlbt_d_4
	db ctrlbt_portamento,$1f
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_portamento,$00
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_auto_pwm,%10_01_10_01,1
;	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_a_3
	db ctrlbt_wait


                ; M 6

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111    
	db ctrlbt_a_3     
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_a_3     
	db ctrlbt_wait
	db ctrlbt_f_3 
	db ctrlbt_wait


                ; M 7


	db ctrlbt_d_3      
	db ctrlbt_wait      
	db ctrlbt_a_3
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_f_3    
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,1
	db ctrlbt_envelope,%0000_0_111 
;	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_c_4 
	db ctrlbt_wait


                ; M 8

	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_vibrato_reset
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_111     
	db ctrlbt_a_3     
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_c_4     
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_transpose,00

	db ctrlbt_track_loop


;---------------------------------------------------


seven8_tr_2_pt_01:

                ; 10 events/measure                
                ; M 9

	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_transpose,24

	db ctrlbt_envelope,%0000_0_011  
	db ctrlbt_volume,$b0
	db ctrlbt_g_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$a0    
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$90
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_pulse_width,%11_000000       
	db ctrlbt_g_4     
	db ctrlbt_wait


                ; M 10

	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_volume,$a0
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_111  
	db ctrlbt_porta_from,ctrlbt_e_4
	db ctrlbt_portamento,$3f
	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_portamento,$00
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_b_4     
	db ctrlbt_d_5
	db ctrlbt_pulse_width,%11_000000       
	db ctrlbt_b_4     
	db ctrlbt_a_4

                ; M 11

	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_c_5      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$a0  
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_e_4      
	db ctrlbt_f_4
	db ctrlbt_pulse_width,%11_000000        
	db ctrlbt_g_4     
	db ctrlbt_wait


                ; M 12

	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_volume,$a0
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_111  
	db ctrlbt_porta_from,ctrlbt_e_4
	db ctrlbt_portamento,$3f
	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_volume,$b0
	db ctrlbt_portamento,$00
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_d_5
	db ctrlbt_wait
	db ctrlbt_volume,$90    
	db ctrlbt_c_5     
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_pulse_width,%11_000000        
	db ctrlbt_g_4    
	db ctrlbt_wait


                ; M 13

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_g_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000     
	db ctrlbt_volume,$90
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$90    
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_pulse_width,%11_000000        
	db ctrlbt_g_4    
	db ctrlbt_wait


                ; M 14

	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_volume,$a0
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_porta_from,ctrlbt_e_4
	db ctrlbt_portamento,$3f
	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_001     
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_portamento,$00
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_volume,$90
	db ctrlbt_b_4     
	db ctrlbt_d_5
	db ctrlbt_pulse_width,%11_000000       
	db ctrlbt_b_4    
	db ctrlbt_a_4


                ; M 15

	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_c_5      
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_pulse_width,%11_000000   
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0    
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_e_4     
	db ctrlbt_f_4
	db ctrlbt_pulse_width,%11_000000        
	db ctrlbt_g_4     
	db ctrlbt_wait


                ; M 16

	db ctrlbt_volume,$a0
	db ctrlbt_vibrato,$06,$3f
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_porta_from,ctrlbt_e_4
	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_portamento,$3f
	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_auto_pwm,%10_01_10_01,0
	db ctrlbt_portamento,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db ctrlbt_pulse_width,%11_000000        
	db ctrlbt_c_5     
	db ctrlbt_wait

	db ctrlbt_transpose,00


	db ctrlbt_track_loop


;---------------------------------------------------



seven8_tr_2_pt_02:

                ; 16 events/measure
                ; M 17

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_pulse_width,%10_000000

        db ctrlbt_transpose,24


	db ctrlbt_volume,$80
	db ctrlbt_arpeggio,$0c,$0c,3
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_porta_from,ctrlbt_b_3
	db ctrlbt_portamento,$3f
	db ctrlbt_d#4      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_portamento,$00
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
   

                ; M 18

	db ctrlbt_pulse_width,%01_000000 
	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_vibrato,$07,$2f  
	db ctrlbt_c_4	     
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
        db ctrlbt_hold_mode,00
	db ctrlbt_d_4
	db ctrlbt_wait    
   

                ; M 19

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
	db ctrlbt_vibrato,$04,$3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
   

                ; M 20

        db ctrlbt_hold_mode,01
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_arpeggio,$0c,$0c,1
;	db ctrlbt_arpeggio,$c0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f 
	db ctrlbt_porta_from,ctrlbt_b_3
	db ctrlbt_portamento,$3f
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_arpeggio,$0c,$0c,1
;	db ctrlbt_arpeggio,$c0
	db ctrlbt_portamento,$00
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 21

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f  
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
        db ctrlbt_hold_mode,00
	db ctrlbt_d_4
	db ctrlbt_wait    
      

                ; M 22

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
	db ctrlbt_wait
	db ctrlbt_wait    
      

                ; M 23

        db ctrlbt_hold_mode,01
	db ctrlbt_pulse_width,%10_000000 
	db ctrlbt_arpeggio,$0c,$0c,1
;	db ctrlbt_arpeggio,$c0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f
	db ctrlbt_porta_from,ctrlbt_b_3
	db ctrlbt_portamento,$3f
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_portamento,$00
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    

        db ctrlbt_transpose,00
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00

	db ctrlbt_track_loop

;------------------------------------------------

seven8_tr_2_pt_03:

                ; M 24

	db ctrlbt_envelope,%0000_0_000

        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,24
	db ctrlbt_volume,$80
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$07,$2f
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


                ; M 25

	db ctrlbt_pulse_width,%10_000000 
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_d_4      
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
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 26

	db ctrlbt_arpeggio,$0c,$dc,1
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait    


                ; M 27

	db ctrlbt_f_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_f_5      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_4    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    


                ; M 28

	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f_4     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 29

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_g_4
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


                ; M 30

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    


                ; M 31

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 32

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait    
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    

        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$90
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00
        db ctrlbt_transpose,00


	db ctrlbt_track_loop


;---------------------------------------------------

seven8_tr_2_pt_04:

          ; blank

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
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
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


;--------------------------------------------------

seven8_tr_2_pt_05:

                ; 10 events/measure                
                ; M 9

	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_transpose,12

	db ctrlbt_envelope,%0000_0_011  
	db ctrlbt_volume,$b0
	db ctrlbt_g_4      
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$a0    
	db ctrlbt_a_4
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$90
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0   
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_pulse_width,%11_000000       
	db ctrlbt_b_4     
	db ctrlbt_wait


	db ctrlbt_track_loop

;--------------------------------------------------
;---------------------------------------------------

seven8_tr_2_pt_06:

	db ctrlbt_auto_pwm,%10_01_10_01,4
;        db ctrlbt_hold_mode,00  

            ; M1

	db ctrlbt_volume,$20
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_transpose,00
	db ctrlbt_arpeggio,$0c,$0c,12
	db ctrlbt_a_5
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,3
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,2
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,1
	db ctrlbt_volume,$90
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_a_5
	db ctrlbt_wait

            ; M2


	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_volume,$90
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

            ; M3

	db ctrlbt_volume,$30
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_g#5     
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

            ; M4

	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_a_5    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
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

            ; M5

	db ctrlbt_volume,$20
	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_d_6      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,3
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,2        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,1
	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_volume,$90
	db ctrlbt_d_6
	db ctrlbt_wait

            ; M6


	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_auto_pwm,%10_01_10_01,4
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_c#6
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


            ; M7

	db ctrlbt_volume,$30
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
	db ctrlbt_wait
	db ctrlbt_wait


            ; M8

	db ctrlbt_restart_envelope
	db ctrlbt_porta_from,ctrlbt_a#4
	db ctrlbt_portamento,$10
	db ctrlbt_volume,$90
	db ctrlbt_envelope,%0000_0_000
	db ctrlbt_c#6      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait        
	db ctrlbt_wait     
	db ctrlbt_wait
;	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_portamento,$00
	db ctrlbt_arpeggio,$0c,$0c,00
        db ctrlbt_hold_mode,01

	db ctrlbt_track_loop



;------------------------------------------------


;------------------------------------------------

seven8_tr_2_pt_07:

                ; M 24

	db ctrlbt_envelope,%0000_0_000

        db ctrlbt_hold_mode,00
        db ctrlbt_transpose,24
	db ctrlbt_volume,$80
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$07,$2f
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


                ; M 25

	db ctrlbt_pulse_width,%10_000000 
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_d_4      
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
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 26

	db ctrlbt_arpeggio,$0c,$dc,1
	db ctrlbt_a_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait    
	db ctrlbt_a_3      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_a_5
	db ctrlbt_wait
	db ctrlbt_a_4
	db ctrlbt_wait    


                ; M 27

	db ctrlbt_f_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_f_5      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_f_4    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    


                ; M 28

	db ctrlbt_f_5      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_f_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait       
	db ctrlbt_f_4     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 29

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_g_4
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


                ; M 30

	db ctrlbt_g_3      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_g_4
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait      
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait    


                ; M 31

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait      
        db ctrlbt_transpose,36
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_e_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 32

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_e_3
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait    
	db ctrlbt_e_3      
	db ctrlbt_wait      
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait      
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait    

        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$90
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00
        db ctrlbt_transpose,00


	db ctrlbt_track_loop


;---------------------------------------------------
seven8_tr_2_pt_08:

                ; 16 events/measure
                ; M 17

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_pulse_width,%10_000000

        db ctrlbt_transpose,24


	db ctrlbt_volume,$80
	db ctrlbt_arpeggio,$0c,$0c,3
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_porta_from,ctrlbt_b_3
	db ctrlbt_portamento,$3f
	db ctrlbt_d#4      
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_portamento,$00
	db ctrlbt_b_3      
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
   

                ; M 18

	db ctrlbt_pulse_width,%01_000000 
	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_vibrato,$07,$2f  
	db ctrlbt_c_4	     
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
        db ctrlbt_hold_mode,00
	db ctrlbt_d_5
	db ctrlbt_wait    
   

                ; M 19


	db ctrlbt_d_4     
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_c#5      
	db ctrlbt_wait       
	db ctrlbt_c#4     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$04,$3f      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
   

                ; M 20

        db ctrlbt_hold_mode,01
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_arpeggio,$0c,$0c,1
;	db ctrlbt_arpeggio,$c0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f 
	db ctrlbt_porta_from,ctrlbt_b_3
	db ctrlbt_portamento,$3f
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_arpeggio,$0c,$0c,1
;	db ctrlbt_arpeggio,$c0
	db ctrlbt_portamento,$00
	db ctrlbt_b_4     
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    


                ; M 21

	db ctrlbt_e_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f  
	db ctrlbt_c_4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
        db ctrlbt_hold_mode,00
	db ctrlbt_d_5
	db ctrlbt_wait    
      

                ; M 22


	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_c#5    
	db ctrlbt_wait       
	db ctrlbt_c#4     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    
      

                ; M 23

        db ctrlbt_hold_mode,01
	db ctrlbt_pulse_width,%10_000000 
	db ctrlbt_arpeggio,$0c,$0c,1
;	db ctrlbt_arpeggio,$c0
	db ctrlbt_d_4      
	db ctrlbt_wait      
	db ctrlbt_b_3
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_pulse_width,%01_000000  
	db ctrlbt_vibrato,$07,$2f
	db ctrlbt_porta_from,ctrlbt_b_3
	db ctrlbt_portamento,$3f
	db ctrlbt_d#4      
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_vibrato_reset
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_portamento,$00
	db ctrlbt_b_4     
	db ctrlbt_wait      
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait    

        db ctrlbt_transpose,00
	db ctrlbt_arpeggio,$00,$00,0
;	db ctrlbt_arpeggio,$00

	db ctrlbt_track_loop


;------------------------------------------------

seven8_tr_3_pt_00:

   
                ; 14 events/measure
                ; M 1

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$13
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 2

;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14



                ; M 3

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04
	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_wait      ;11
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;12
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 4

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;11
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 5

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$15
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14




                ; M 6

;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14




                ; M 7


	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 8


	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
	db ctrlbt_wait      ;14

	db ctrlbt_stop_sample


	db ctrlbt_track_loop


;---------------------------------------------------

seven8_tr_3_pt_01:

                ; 10 events/measure                
                ; M 9


	db ctrlbt_portamento,$00
	db ctrlbt_gain_waveform,$27
	db ctrlbt_waveform,$60

        db ctrlbt_transpose,-12
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_e_5      
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$61
	db ctrlbt_c_5
	db ctrlbt_e_5      
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00
	db ctrlbt_g_5


                ; M 10


	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,0
	db ctrlbt_waveform,$62
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff
	db ctrlbt_f_5
	db ctrlbt_a_5
	db ctrlbt_f_5
	db ctrlbt_a_5
	db ctrlbt_waveform,$63
	db ctrlbt_c_5    
	db ctrlbt_f_5      
	db ctrlbt_a_5
	db ctrlbt_f_5
	db ctrlbt_portamento,$00     
	db ctrlbt_a_5


                ; M 11

	db ctrlbt_gain_waveform,$27
        db ctrlbt_transpose,-12
	db ctrlbt_waveform,$64
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$65
	db ctrlbt_c_5    
	db ctrlbt_e_5
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00     
	db ctrlbt_g_5


                ; M 12

	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,0
	db ctrlbt_waveform,$66
	db ctrlbt_d_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_f#5      
	db ctrlbt_a_5
	db ctrlbt_d_5
	db ctrlbt_a_5
	db ctrlbt_waveform,$67
	db ctrlbt_d_5
	db ctrlbt_f#5      
	db ctrlbt_a_5
	db ctrlbt_d_5
	db ctrlbt_portamento,$00     
	db ctrlbt_a_5


                ; M 13

	db ctrlbt_gain_waveform,$27
        db ctrlbt_transpose,-12
	db ctrlbt_waveform,$68
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_e_5      
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$69
	db ctrlbt_c_5
	db ctrlbt_e_5      
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00     
	db ctrlbt_g_5



                ; M 14

	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,0
	db ctrlbt_waveform,$6a
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff     
	db ctrlbt_f_5      
	db ctrlbt_a_5
	db ctrlbt_f_5
	db ctrlbt_a_5
	db ctrlbt_waveform,$6b
	db ctrlbt_c_5
	db ctrlbt_f_5      
	db ctrlbt_a_5        
	db ctrlbt_f_5
	db ctrlbt_portamento,$00     
	db ctrlbt_a_5




                ; M 15


	db ctrlbt_gain_waveform,$27
        db ctrlbt_transpose,-12
	db ctrlbt_waveform,$6c
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_e_5      
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$6d
	db ctrlbt_c_5
	db ctrlbt_e_5      
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00     
	db ctrlbt_g_5


                ; M 16


	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,12
	db ctrlbt_waveform,$6e
	db ctrlbt_d_5
	db ctrlbt_portamento,$ff
	db ctrlbt_waveform,$6d   
	db ctrlbt_f#5      
	db ctrlbt_waveform,$6c
	db ctrlbt_a_5
	db ctrlbt_waveform,$6b
	db ctrlbt_d_5
	db ctrlbt_waveform,$6a
	db ctrlbt_a_5
	db ctrlbt_waveform,$69
	db ctrlbt_d_5
	db ctrlbt_waveform,$68
	db ctrlbt_f#5
	db ctrlbt_waveform,$66     
	db ctrlbt_a_5
	db ctrlbt_waveform,$64       
	db ctrlbt_d_5
	db ctrlbt_portamento,$00   
	db ctrlbt_waveform,$62
	db ctrlbt_a_5


        db ctrlbt_transpose,0
	db ctrlbt_track_loop


;---------------------------------------------------

seven8_tr_3_pt_02:

          ; M 17

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


          ; M 18

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
;	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 19

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 20

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait    


             ; M 21

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 22

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait    

             ; M 23

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    

	db ctrlbt_track_loop

;-----------------------------------------

seven8_tr_3_pt_03:


             ; M 24

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    

             ; M 25

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 26

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    


             ; M 27

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 28

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    


             ; M 29

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    



             ; M 30

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    



             ; M 31

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 32

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    


	db ctrlbt_track_loop


;---------------------------------------------------

seven8_tr_3_pt_04:

                ; 10 events/measure                
                ; M 9


	db ctrlbt_portamento,$00
	db ctrlbt_gain_waveform,$27
	db ctrlbt_waveform,$60

        db ctrlbt_transpose,0
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_e_5      
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$61
	db ctrlbt_c_5
	db ctrlbt_e_5      
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00
	db ctrlbt_g_5


                ; M 10


	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,12
	db ctrlbt_waveform,$62
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff
	db ctrlbt_f_5
	db ctrlbt_a_5
	db ctrlbt_f_5
	db ctrlbt_a_5
	db ctrlbt_waveform,$63
	db ctrlbt_c_5    
	db ctrlbt_f_5      
	db ctrlbt_a_5
	db ctrlbt_f_5
	db ctrlbt_portamento,$00     
	db ctrlbt_a_5


                ; M 11

	db ctrlbt_gain_waveform,$27
        db ctrlbt_transpose,0
	db ctrlbt_waveform,$64
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$65
	db ctrlbt_c_5    
	db ctrlbt_e_5
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00     
	db ctrlbt_g_5


                ; M 12

	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,12
	db ctrlbt_waveform,$66
	db ctrlbt_d_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_f#5      
	db ctrlbt_a_5
	db ctrlbt_d_5
	db ctrlbt_a_5
	db ctrlbt_waveform,$67
	db ctrlbt_d_5
	db ctrlbt_f#5      
	db ctrlbt_a_5
	db ctrlbt_d_5
	db ctrlbt_portamento,$00     
	db ctrlbt_a_5


                ; M 13

	db ctrlbt_gain_waveform,$27
        db ctrlbt_transpose,0
	db ctrlbt_waveform,$68
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_e_5      
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$69
	db ctrlbt_c_5
	db ctrlbt_e_5      
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00     
	db ctrlbt_g_5



                ; M 14

	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,12
	db ctrlbt_waveform,$6a
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff     
	db ctrlbt_f_5      
	db ctrlbt_a_5
	db ctrlbt_f_5
	db ctrlbt_a_5
	db ctrlbt_waveform,$6b
	db ctrlbt_c_5
	db ctrlbt_f_5      
	db ctrlbt_a_5        
	db ctrlbt_f_5
	db ctrlbt_portamento,$00     
	db ctrlbt_a_5




                ; M 15


	db ctrlbt_gain_waveform,$27
        db ctrlbt_transpose,0
	db ctrlbt_waveform,$6c
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_e_5      
	db ctrlbt_g_5
	db ctrlbt_e_5
	db ctrlbt_g_5
	db ctrlbt_waveform,$6d
	db ctrlbt_c_5
	db ctrlbt_e_5      
	db ctrlbt_g_5        
	db ctrlbt_e_5
	db ctrlbt_portamento,$00     
	db ctrlbt_g_5


                ; M 16


	db ctrlbt_gain_waveform,$2a
        db ctrlbt_transpose,12
	db ctrlbt_waveform,$6e
	db ctrlbt_d_5
	db ctrlbt_portamento,$ff
	db ctrlbt_waveform,$6d   
	db ctrlbt_f#5      
	db ctrlbt_waveform,$6c
	db ctrlbt_a_5
	db ctrlbt_waveform,$6b
	db ctrlbt_d_5
	db ctrlbt_waveform,$6a
	db ctrlbt_a_5
	db ctrlbt_waveform,$69
	db ctrlbt_d_5
	db ctrlbt_waveform,$68
	db ctrlbt_f#5
	db ctrlbt_waveform,$66     
	db ctrlbt_a_5
	db ctrlbt_waveform,$64       
	db ctrlbt_d_5
	db ctrlbt_portamento,$00   
	db ctrlbt_waveform,$62
	db ctrlbt_a_5


        db ctrlbt_transpose,0
	db ctrlbt_track_loop


;--------------------------------------------------


seven8_tr_3_pt_05:


                ; 10 events/measure                
                ; M 9


	db ctrlbt_portamento,$00
	db ctrlbt_gain_waveform,$25
	db ctrlbt_waveform,$60


	db ctrlbt_sample,$04
        db ctrlbt_transpose,-12
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_gain_waveform,$26
	db ctrlbt_e_5      
	db ctrlbt_gain_waveform,$27
	db ctrlbt_g_5
	db ctrlbt_gain_waveform,$28
	db ctrlbt_e_5
	db ctrlbt_gain_waveform,$29
	db ctrlbt_g_5
	db ctrlbt_gain_waveform,$2a
	db ctrlbt_c_5
	db ctrlbt_gain_waveform,$2b
	db ctrlbt_e_5      
	db ctrlbt_gain_waveform,$2c
	db ctrlbt_g_5        
	db ctrlbt_gain_waveform,$2d
	db ctrlbt_e_5
	db ctrlbt_gain_waveform,$2e
	db ctrlbt_portamento,$00
	db ctrlbt_g_5


	db ctrlbt_track_loop


;------------------------------------------------

;-----------------------------------------

seven8_tr_3_pt_06:


             ; M 24

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_sample,$0a      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    

             ; M 25

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$04     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 26

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a    
	db ctrlbt_wait     
	db ctrlbt_wait
;	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait    


             ; M 27

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$0a       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 28

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_sample,$0a      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 29

	db ctrlbt_sample,$0a
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_sample,$04      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    



             ; M 30

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait
	db ctrlbt_wait    



             ; M 31

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_sample,$0a      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 32

	db ctrlbt_sample,$0a
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a 
	db ctrlbt_wait
	db ctrlbt_wait    


	db ctrlbt_track_loop


;---------------------------------------------------

;--------------------------------------------------


seven8_tr_3_pt_07:


                ; 10 events/measure                
                ; M 9


	db ctrlbt_portamento,$00
	db ctrlbt_gain_waveform,$25
	db ctrlbt_waveform,$60


	db ctrlbt_sample,$04
        db ctrlbt_transpose,7
	db ctrlbt_c_5
	db ctrlbt_portamento,$ff      
	db ctrlbt_gain_waveform,$26
	db ctrlbt_e_5      
	db ctrlbt_gain_waveform,$27
	db ctrlbt_g_5
	db ctrlbt_gain_waveform,$28
	db ctrlbt_e_5
	db ctrlbt_gain_waveform,$29
	db ctrlbt_g_5
	db ctrlbt_gain_waveform,$2a
	db ctrlbt_c_5
	db ctrlbt_gain_waveform,$2b
	db ctrlbt_e_5      
	db ctrlbt_gain_waveform,$2c
	db ctrlbt_g_5        
	db ctrlbt_gain_waveform,$2d
	db ctrlbt_e_5
	db ctrlbt_gain_waveform,$2e
	db ctrlbt_portamento,$00
	db ctrlbt_g_5

	db ctrlbt_gain_waveform,$00

	db ctrlbt_track_loop


;------------------------------------------------
;---------------------------------------------------

seven8_tr_3_pt_08:

          ; M 17

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


          ; M 18

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
;	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 19

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 20

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait    


             ; M 21

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait    


             ; M 22

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$04       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait    

             ; M 23

	db ctrlbt_sample,$04
	db ctrlbt_wait      
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_sample,$0a       
	db ctrlbt_wait    
	db ctrlbt_wait
;	db ctrlbt_sample,$04
	db ctrlbt_wait
	db ctrlbt_sample,$0a    
	db ctrlbt_wait
	db ctrlbt_sample,$0a  
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait
	db ctrlbt_sample,$0a
	db ctrlbt_wait    

	db ctrlbt_track_loop

;-----------------------------------------
;------------------------------------------------

seven8_tr_3_pt_09:

   
                ; 14 events/measure
                ; M 1

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 2

;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14



                ; M 3

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_wait      ;11
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;12
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 4

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;11
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 5

	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14




                ; M 6

;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14




                ; M 7


	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
;	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
;	db ctrlbt_sample,$04
	db ctrlbt_wait      ;14


                ; M 8


	db ctrlbt_sample,$04
	db ctrlbt_wait      ;1
	db ctrlbt_wait      ;2
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;3
	db ctrlbt_wait      ;4
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;5
	db ctrlbt_wait      ;6
	db ctrlbt_sample,$04
	db ctrlbt_wait      ;7
	db ctrlbt_wait      ;8
;	db ctrlbt_sample,$04  
	db ctrlbt_wait      ;9
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;10
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;11
	db ctrlbt_wait      ;12
	db ctrlbt_sample,$0b
	db ctrlbt_wait      ;13
	db ctrlbt_wait      ;14

	db ctrlbt_stop_sample


	db ctrlbt_track_loop


;---------------------------------------------------


;----------------------------------------

seven8_tr_4_pt_00:

        
          ; Meas 1


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait


          ; Meas 2


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$60
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f

          ; Meas 3

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f

          ; Meas 4

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f

          ; Meas 5

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 6

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$60
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f

          ; Meas 7

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f

          ; Meas 8

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db $3f    
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $3f 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db $3f

	db ctrlbt_track_loop

;---------------------------------------------------



seven8_tr_4_pt_01:

        
          ; Meas 9

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f 
	db ctrlbt_volume,%0101_0_000
	db $3f    
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 10

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f 
	db ctrlbt_volume,%0101_0_000
	db $3f    
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 11


	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%1101_0_000
	db $3f 
	db ctrlbt_volume,%0101_0_000
	db $3f    
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 12

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%1101_0_000
	db $3f 
	db ctrlbt_volume,%0101_0_000
	db $3f    
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 13

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%1101_0_000
	db $3f 
	db ctrlbt_volume,%0101_0_000
	db $3f    
	db ctrlbt_volume,%0111_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 14

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000  
	db $37
	db ctrlbt_volume,%0101_0_000     
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%1101_0_000
	db $3f   
	db ctrlbt_volume,%0101_0_000
	db $3f      
	db ctrlbt_volume,%0111_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 15

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000  
	db $37
	db ctrlbt_volume,%0101_0_000     
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f   
	db ctrlbt_volume,%0101_0_000
	db $3f      
	db ctrlbt_volume,%0111_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f

          ; Meas 16

	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%0101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000  
	db $37
	db ctrlbt_volume,%0101_0_000     
	db $3f
	db ctrlbt_volume,%1101_0_000
	db $3f
	db ctrlbt_volume,%1000_0_000
	db $37
	db ctrlbt_volume,%1101_0_000
	db $3f   
	db ctrlbt_volume,%0101_0_000
	db $3f      
	db ctrlbt_volume,%0111_0_000
	db $37
	db ctrlbt_volume,%0101_0_000
	db $3f

	db ctrlbt_track_loop

;---------------------------------------------------


seven8_tr_4_pt_02:

        
          ; Meas 17

	db ctrlbt_volume,$e0
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

	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
   

          ; Meas 18

	db ctrlbt_volume,$e0
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
	db ctrlbt_wait
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


          ; Meas 19


	db ctrlbt_volume,$e0
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

	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
   

          ; Meas 20

	db ctrlbt_volume,$e0
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
	db ctrlbt_wait
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

          ; Meas 21

	db ctrlbt_volume,$e0
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

	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
   

          ; Meas 22

	db ctrlbt_volume,$e0
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
	db ctrlbt_wait
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

          ; Meas 23


	db ctrlbt_volume,$e0
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

	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
   

	db ctrlbt_track_loop

;--------------------------------------------

seven8_tr_4_pt_03:

          ; Meas 24


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

          ; Meas 25


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
   

          ; Meas 26

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

          ; Meas 27


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
   

          ; Meas 28

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

          ; Meas 29


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
   

          ; Meas 30

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

          ; Meas 31


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait
   

          ; Meas 32

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0  
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$90     
	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
  
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_wait


	db ctrlbt_track_loop


;---------------------------------------------------



seven8_tr_4_pt_04:

        
          ; Meas 9

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait


          ; Meas 10

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait

          ; Meas 11

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait


          ; Meas 12

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait


          ; Meas 13

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait


          ; Meas 14

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait


          ; Meas 15

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0 
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait


          ; Meas 16

	db ctrlbt_envelope,%0000_0_111 
	db ctrlbt_volume,$d0

	db $3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$60
	db ctrlbt_envelope,%0000_0_001
	db $37
	db ctrlbt_volume,$70
	db $37
	db ctrlbt_volume,$80 
	db $37
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$a0    
	db $37
	db ctrlbt_wait

	db ctrlbt_track_loop

;-------------------------------------------------------

seven8_tr_4_pt_05:


          ; Meas 15

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_envelope,%0000_0_011   
	db $3f
	db ctrlbt_wait

	db ctrlbt_track_loop

;----------------------------------------------------


seven8_tr_4_pt_06:

        
          ; Meas 1


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$36
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db $40+$3a
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait


          ; Meas 2


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$60
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$3a
	db ctrlbt_volume,$a0
	db ctrlbt_wait 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 3

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$36
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$3a
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 4

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$3a
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $40+$36
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80	
	db $40+$36
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 5


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$36
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$3a
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$3a
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 6

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$60
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$36
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$3a
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 7

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$3a
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db $40+$36
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$36
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 8

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$36
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$3a
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db $40+$36
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------



seven8_tr_4_pt_07:

        
          ; Meas 1


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait


          ; Meas 2


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$60
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 3

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 4

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80	
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 5


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 6

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$60
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 7

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

          ; Meas 8

	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f 
	db ctrlbt_volume,$a0
	db ctrlbt_wait   
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_wait 
	db ctrlbt_volume,$e0
	db $3f    
	db ctrlbt_volume,$a0
	db ctrlbt_wait

	db ctrlbt_track_loop

;---------------------------------------------------

seven8_tr_4_pt_08:


          ; Meas 15

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$a0
	db ctrlbt_envelope,%0000_0_001
	db $37
	db ctrlbt_volume,$d0
	db $37
	db ctrlbt_volume,$a0
	db $37
	db ctrlbt_envelope,%0000_0_011   
	db ctrlbt_volume,$d0
	db $37
	db ctrlbt_wait

	db ctrlbt_track_loop


seven8_end:

        end

;---------------------------------------------------
