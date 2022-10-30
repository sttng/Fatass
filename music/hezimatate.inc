;***************************************************************************

hezimatate_header:
	dw hezimatate_sequence-hezimatate_header	; sequence index
	dw hezimatate_patterns-hezimatate_header	; patterns index
	dw hezimatate_end-hezimatate_header		; song length
	db 10	; # of patterns

	db 'HEZIMATATE'					; song name
	db $ff
	db "test junk"

hezimatate_sequence:

	db ctrlbt_song_speed,$16
	db ctrlbt_song_speed,$16

	db $00
	db $01
	db $02
	db $03
	db $04
	db $05
	db $06
	db $07
	db $08

	db ctrlbt_track_loop

	db "test junk"

hezimatate_patterns:
	dw hezimatate_tr_1_pt_00-hezimatate_patterns,hezimatate_tr_2_pt_00-hezimatate_patterns,hezimatate_tr_3_pt_00-hezimatate_patterns,hezimatate_tr_4_pt_00-hezimatate_patterns
	dw hezimatate_tr_1_pt_01-hezimatate_patterns,hezimatate_tr_2_pt_01-hezimatate_patterns,hezimatate_tr_3_pt_01-hezimatate_patterns,hezimatate_tr_4_pt_01-hezimatate_patterns
	dw hezimatate_tr_1_pt_02-hezimatate_patterns,hezimatate_tr_2_pt_02-hezimatate_patterns,hezimatate_tr_3_pt_02-hezimatate_patterns,hezimatate_tr_4_pt_02-hezimatate_patterns
	dw hezimatate_tr_1_pt_03-hezimatate_patterns,hezimatate_tr_2_pt_03-hezimatate_patterns,hezimatate_tr_3_pt_03-hezimatate_patterns,hezimatate_tr_4_pt_03-hezimatate_patterns
	dw hezimatate_tr_1_pt_04-hezimatate_patterns,hezimatate_tr_2_pt_04-hezimatate_patterns,hezimatate_tr_3_pt_04-hezimatate_patterns,hezimatate_tr_4_pt_04-hezimatate_patterns
	dw hezimatate_tr_1_pt_05-hezimatate_patterns,hezimatate_tr_2_pt_05-hezimatate_patterns,hezimatate_tr_3_pt_05-hezimatate_patterns,hezimatate_tr_4_pt_05-hezimatate_patterns
	dw hezimatate_tr_1_pt_06-hezimatate_patterns,hezimatate_tr_2_pt_06-hezimatate_patterns,hezimatate_tr_3_pt_06-hezimatate_patterns,hezimatate_tr_4_pt_06-hezimatate_patterns
	dw hezimatate_tr_1_pt_07-hezimatate_patterns,hezimatate_tr_2_pt_07-hezimatate_patterns,hezimatate_tr_3_pt_07-hezimatate_patterns,hezimatate_tr_4_pt_07-hezimatate_patterns
	dw hezimatate_tr_1_pt_08-hezimatate_patterns,hezimatate_tr_2_pt_08-hezimatate_patterns,hezimatate_tr_3_pt_08-hezimatate_patterns,hezimatate_tr_4_pt_08-hezimatate_patterns
	dw hezimatate_tr_1_pt_08-hezimatate_patterns,hezimatate_tr_2_pt_08-hezimatate_patterns,hezimatate_tr_3_pt_08-hezimatate_patterns,hezimatate_tr_4_pt_08-hezimatate_patterns

	db "test junk"

;--------------------------------------------------


hezimatate_tr_1_pt_00:



                ; M 1

                ; Lead

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_envelope,%0000_0_111
;	db ctrlbt_volume,$60

        db ctrlbt_transpose,12

        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset

	db ctrlbt_volume,$80
	db ctrlbt_a#4

                ; M 2

	db ctrlbt_volume,$70
	db ctrlbt_a#5      
	db ctrlbt_g_5
	db ctrlbt_volume,$60
	db ctrlbt_c_5       
	db ctrlbt_e_5      
	db ctrlbt_volume,$50
	db ctrlbt_g_5       
	db ctrlbt_c_6      
	db ctrlbt_volume,$60
	db ctrlbt_c#6      
	db ctrlbt_c_6
	db ctrlbt_volume,$70
	db ctrlbt_a#5        
	db ctrlbt_c_5       
	db ctrlbt_volume,$80
	db ctrlbt_g_5

               ; M 3

	db ctrlbt_volume,$a0
	db ctrlbt_c_6      
	db ctrlbt_f#4
	db ctrlbt_volume,$80
	db ctrlbt_c#6       
	db ctrlbt_a#5      
	db ctrlbt_volume,$70
	db ctrlbt_c_5       
	db ctrlbt_e_5      
	db ctrlbt_volume,$60
	db ctrlbt_g_5      
	db ctrlbt_c_6
	db ctrlbt_volume,$50
	db ctrlbt_c#6        
	db ctrlbt_c_6       
	db ctrlbt_volume,$60
	db ctrlbt_a#5


                ; M 4

	db ctrlbt_volume,$70
	db ctrlbt_c_5
	db ctrlbt_g_5
	db ctrlbt_volume,$80
	db ctrlbt_c_6       
	db ctrlbt_a#4      
	db ctrlbt_volume,$a0
	db ctrlbt_a#5       
	db ctrlbt_g_5      
	db ctrlbt_volume,$80
	db ctrlbt_c_5      
	db ctrlbt_e_5
	db ctrlbt_volume,$70         
	db ctrlbt_g_5
	db ctrlbt_c_6
	db ctrlbt_volume,$60         
	db ctrlbt_c#6

                ; M 5

	db ctrlbt_volume,$50  
	db ctrlbt_c_6
	db ctrlbt_a#5       
	db ctrlbt_volume,$60  
	db ctrlbt_c#5      
	db ctrlbt_f_5
	db ctrlbt_volume,$70
	db ctrlbt_g#5      
	db ctrlbt_a#5      
	db ctrlbt_volume,$80  
	db ctrlbt_g_5       
	db ctrlbt_g#5
	db ctrlbt_volume,$a0
	db ctrlbt_g_5      
	db ctrlbt_g#5
	db ctrlbt_volume,$80
	db ctrlbt_g_5        

                ; M 6

	db ctrlbt_volume,$70
	db ctrlbt_g#5
	db ctrlbt_g_5
	db ctrlbt_volume,$60
	db ctrlbt_g#5      
	db ctrlbt_g_5
	db ctrlbt_volume,$50
	db ctrlbt_g#5      
	db ctrlbt_g_5      
	db ctrlbt_volume,$60  
	db ctrlbt_g#5       
	db ctrlbt_c#6
	db ctrlbt_volume,$70
	db ctrlbt_c_6     
	db ctrlbt_a#5
	db ctrlbt_volume,$80   
	db ctrlbt_g#5       

                ; M 7

	db ctrlbt_volume,$a0
	db ctrlbt_g_5
	db ctrlbt_g#5
	db ctrlbt_volume,$80         
	db ctrlbt_g_5      
	db ctrlbt_g#5
	db ctrlbt_volume,$70
	db ctrlbt_g_5      
	db ctrlbt_g#5      
	db ctrlbt_volume,$60  
	db ctrlbt_g_5       
	db ctrlbt_g#5
	db ctrlbt_volume,$50
	db ctrlbt_g_5     
	db ctrlbt_g#5
	db ctrlbt_volume,$60
	db ctrlbt_g_5       

                ; M 8

	db ctrlbt_volume,$70
	db ctrlbt_g#5
	db ctrlbt_c#6
	db ctrlbt_volume,$80         
	db ctrlbt_c_6      
	db ctrlbt_a#5
	db ctrlbt_volume,$a0
	db ctrlbt_g#5      
	db ctrlbt_d#6      
	db ctrlbt_volume,$80  
	db ctrlbt_a#5       
	db ctrlbt_d#6
	db ctrlbt_volume,$70
	db ctrlbt_a#5     
	db ctrlbt_d#6
	db ctrlbt_volume,$60
	db ctrlbt_a#5       


                ; M 9



	db ctrlbt_volume,$50
	db ctrlbt_d#6
	db ctrlbt_a#5
	db ctrlbt_volume,$80
	db ctrlbt_d#6      
        db ctrlbt_hold_mode,00
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_song_speed,$20
	db ctrlbt_wait       
	db ctrlbt_wait     
	db ctrlbt_wait     
	db ctrlbt_wait       

	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
;	db ctrlbt_wait

	db ctrlbt_arpeggio,$00,$00,0
        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
      
        db ctrlbt_transpose,00

	db ctrlbt_track_loop

	db "test junk"


;--------------------------------------------------

hezimatate_tr_1_pt_01:

        db ctrlbt_hold_mode,01
	db ctrlbt_envelope,%0000_0_100

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_vibrato,$01,$3f

	db ctrlbt_volume,$b0
	db ctrlbt_song_speed,$22

                ; M 10

	db ctrlbt_c_5
	db ctrlbt_d#5       
	db ctrlbt_g#5      
	db ctrlbt_wait       
	db ctrlbt_a#5     
	db ctrlbt_wait     
	db ctrlbt_g#5       
	db ctrlbt_wait       
	db ctrlbt_f#5    
	db ctrlbt_wait     
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait

                ; M 11

	db ctrlbt_f_5
	db ctrlbt_wait       
	db ctrlbt_c_5     
	db ctrlbt_wait       
	db ctrlbt_f_5     
	db ctrlbt_wait     
	db ctrlbt_c_5       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_c_5
	db ctrlbt_d#5
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait


                ; M 12

	db ctrlbt_c#5
	db ctrlbt_wait       
	db ctrlbt_a#5     
	db ctrlbt_wait       
	db ctrlbt_g#5     
	db ctrlbt_wait     
	db ctrlbt_g_5       
	db ctrlbt_wait       
	db ctrlbt_d#5    
	db ctrlbt_wait     
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_c_5
	db ctrlbt_d#5

                ; M 13

	db ctrlbt_g#5
	db ctrlbt_wait       
	db ctrlbt_a#5     
	db ctrlbt_wait       
	db ctrlbt_g#5     
	db ctrlbt_wait     
	db ctrlbt_f#5       
	db ctrlbt_wait       
	db ctrlbt_f_5    
	db ctrlbt_wait     
	db ctrlbt_c#5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_g_5
	db ctrlbt_wait
	db ctrlbt_d#5
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait

                ; M 14

	db ctrlbt_song_speed,$21
	db ctrlbt_c_5
	db ctrlbt_wait       
	db ctrlbt_f_5     
	db ctrlbt_wait       
	db ctrlbt_c_5     
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_song_speed,$20       
	db ctrlbt_c_5   
	db ctrlbt_d#5     
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_a#5
	db ctrlbt_wait
	db ctrlbt_g#5
	db ctrlbt_wait
	db ctrlbt_song_speed,$19
	db ctrlbt_f#5
	db ctrlbt_wait
	db ctrlbt_f_5
	db ctrlbt_wait
	db ctrlbt_c#5
	db ctrlbt_wait



	db ctrlbt_track_loop


;--------------------------------------------------

hezimatate_tr_1_pt_02:



        db ctrlbt_transpose,12

	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_song_speed,$18

                ; M 15

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_a#5
	db ctrlbt_g#5     
	db ctrlbt_g_5     
	db ctrlbt_d#5
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_c_5   
	db ctrlbt_d#5
	db ctrlbt_f#5
	db ctrlbt_d#5
	db ctrlbt_f#5
	db ctrlbt_d#5
	db ctrlbt_c_5


                ; M 16

	db ctrlbt_a#5
	db ctrlbt_g#5     
	db ctrlbt_g_5     
	db ctrlbt_d#5       
	db ctrlbt_c_5   
	db ctrlbt_d#5
	db ctrlbt_f#5
	db ctrlbt_d#5
	db ctrlbt_f#5
	db ctrlbt_d#5
	db ctrlbt_c_5

                ; M 17

	db ctrlbt_f#5
	db ctrlbt_d#5     
	db ctrlbt_f#5     
	db ctrlbt_d#5       
	db ctrlbt_c_5   
	db ctrlbt_d#5
	db ctrlbt_f_5
	db ctrlbt_d_5
	db ctrlbt_a#4
	db ctrlbt_f_5
	db ctrlbt_d_5

                ; M 18

	db ctrlbt_f_5
	db ctrlbt_a#4
        db ctrlbt_hold_mode,00    
	db ctrlbt_wait
        db ctrlbt_hold_mode,01    
	db ctrlbt_c_5      
	db ctrlbt_d#5   
	db ctrlbt_f#5
	db ctrlbt_d#5
	db ctrlbt_f#5
	db ctrlbt_d#5
	db ctrlbt_c_5
	db ctrlbt_d#5

                ; M 19

	db ctrlbt_f_5
	db ctrlbt_d_5     
	db ctrlbt_a#4     
	db ctrlbt_f_5      
	db ctrlbt_d_5   
	db ctrlbt_f_5
	db ctrlbt_d_5
	db ctrlbt_f_5
	db ctrlbt_c_5
	db ctrlbt_d#5
	db ctrlbt_f#5

                ; M 20

	db ctrlbt_d#5
	db ctrlbt_f#5     
	db ctrlbt_d#5     
	db ctrlbt_c_5      
	db ctrlbt_d#5
	db ctrlbt_volume,$30
        db ctrlbt_hold_mode,00
	db ctrlbt_vibrato,$01,$3f
	db ctrlbt_envelope,%0000_1_111   
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 21

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


                ; M 22

	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait  
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0


        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,24

	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_volume,$40
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_g_4
	db ctrlbt_wait


                ; M 23

	db ctrlbt_volume,$a0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0    
	db ctrlbt_e_4    
	db ctrlbt_wait
	db ctrlbt_volume,$70  
	db ctrlbt_e_4  
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_g_4


                ; M 24


	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_g_4    
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_d#4     
	db ctrlbt_wait  
	db ctrlbt_volume,$70
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_g_4
	db ctrlbt_wait

;        db ctrlbt_transpose,00


        db ctrlbt_hold_mode,01

	db ctrlbt_track_loop


;--------------------------------------------------

hezimatate_tr_1_pt_03:





                ; M 25

        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_g_4    
	db ctrlbt_wait
	db ctrlbt_volume,$a0  
	db ctrlbt_c_4 
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_c_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_d_4

                ; M 26

	db ctrlbt_d#4
	db ctrlbt_d_4    
	db ctrlbt_wait    
	db ctrlbt_d#4     
	db ctrlbt_d_4  
	db ctrlbt_d#4
	db ctrlbt_d_4
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait


                ; M 27

	db ctrlbt_volume,$40
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_g_4    
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_g_4  
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_b_4
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_b_4


                ; M 28

	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_g_4    
	db ctrlbt_wait
	db ctrlbt_volume,$70 
	db ctrlbt_g_4     
	db ctrlbt_wait  
	db ctrlbt_volume,$a0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_a#4
	db ctrlbt_wait


                ; M 29

	db ctrlbt_volume,$40
	db ctrlbt_a#4
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_g_4    
	db ctrlbt_wait
	db ctrlbt_volume,$a0  
	db ctrlbt_g_4  
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_a_4
	db ctrlbt_a#4
	db ctrlbt_a_4


                ; M 30

	db ctrlbt_wait
	db ctrlbt_g_4    
	db ctrlbt_a_4
	db ctrlbt_volume,$40    
	db ctrlbt_g_4     
	db ctrlbt_wait  
	db ctrlbt_volume,$70
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_d_4
	db ctrlbt_wait


                ; M 31

	db ctrlbt_volume,$70
	db ctrlbt_d_4
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_d_4    
	db ctrlbt_wait
	db ctrlbt_volume,$40  
	db ctrlbt_f#4  
	db ctrlbt_wait
	db ctrlbt_volume,$70
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_volume,$a0
	db ctrlbt_c_4

                ; M 32

	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_c#4   
	db ctrlbt_c_4     
	db ctrlbt_wait  
	db ctrlbt_f#4
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait

                ; M 33

	db ctrlbt_c_4
	db ctrlbt_c#4    
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_a#3    
	db ctrlbt_wait     
	db ctrlbt_wait 
	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait
;	db ctrlbt_wait

        db ctrlbt_hold_mode,01
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_transpose,00

	db ctrlbt_track_loop



;-----------------------------------------------

hezimatate_tr_1_pt_04:


                ; M 34


	db ctrlbt_song_speed,$24
	db ctrlbt_pulse_width,%10_000000

	db ctrlbt_envelope,%0000_0_100

        db ctrlbt_hold_mode,00
;        db ctrlbt_vibrato_reset

	db ctrlbt_volume,$f0
	db ctrlbt_g_3      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_d_5      
	db ctrlbt_wait      
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0     
	db ctrlbt_g_4     
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_d_5       
	db ctrlbt_wait     
	db ctrlbt_a#5
	db ctrlbt_wait

                ; M 35

	db ctrlbt_volume,$f0
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g_3      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_g_5       
	db ctrlbt_wait
	db ctrlbt_volume,$f0     
	db ctrlbt_g_4
	db ctrlbt_wait


                ; M 36

	db ctrlbt_volume,$b0
	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_a_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_b_4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#5
	db ctrlbt_wait

                ; M 37

	db ctrlbt_volume,$b0
	db ctrlbt_b_5      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 38

	db ctrlbt_volume,$b0
	db ctrlbt_c#5      
	db ctrlbt_wait
	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_d#5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g#5
	db ctrlbt_wait


                ; M 39

	db ctrlbt_volume,$b0
	db ctrlbt_g_3      
	db ctrlbt_wait
	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g_5      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g_4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_d_5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_a#5
	db ctrlbt_wait


                ; M 40

	db ctrlbt_volume,$b0
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g_3      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g_5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g_4
	db ctrlbt_wait


                ; M 41

	db ctrlbt_volume,$b0
	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_a_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_b_3       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#5
	db ctrlbt_wait


        db ctrlbt_hold_mode,01
	db ctrlbt_track_loop



;------------------------------------------------

hezimatate_tr_1_pt_05:

                ; M 42

	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$b0
	db ctrlbt_b_5      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 43

	db ctrlbt_volume,$b0
	db ctrlbt_c#5      
	db ctrlbt_wait
	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g#3      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_d#5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g#5
	db ctrlbt_wait


        db ctrlbt_hold_mode,01

	db ctrlbt_track_loop


;--------------------------------------------------------

hezimatate_tr_1_pt_06:



                ; M 44


	db ctrlbt_envelope,%0000_0_100
        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$b0
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d#4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait



                ; M 45


	db ctrlbt_volume,$b0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_a_4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a_4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait



                ; M 46


	db ctrlbt_volume,$b0
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_c#4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 47


	db ctrlbt_volume,$b0
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#3     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 48


	db ctrlbt_volume,$b0
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_b_4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_b_3     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_b_4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 49


	db ctrlbt_volume,$b0
	db ctrlbt_d#3      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d#3     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f#4
	db ctrlbt_wait



                ; M 50


	db ctrlbt_volume,$b0
	db ctrlbt_a#3      
	db ctrlbt_wait
	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_a#3     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_a#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f_4
	db ctrlbt_wait


                ; M 51


	db ctrlbt_volume,$b0
	db ctrlbt_c#3      
	db ctrlbt_wait
	db ctrlbt_g#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_c#3     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f_4
	db ctrlbt_wait


        db ctrlbt_hold_mode,01

	db ctrlbt_track_loop


;--------------------------------------------------------

hezimatate_tr_1_pt_07:


                ; M 52


        db ctrlbt_hold_mode,00
	db ctrlbt_volume,$b0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_g#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f_4
	db ctrlbt_wait


                ; M 53


	db ctrlbt_volume,$b0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_g#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_g#4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_f_4
	db ctrlbt_wait


                ; M 54


	db ctrlbt_volume,$b0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_g#4      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_b_4       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_g#4
	db ctrlbt_wait


                ; M 55


	db ctrlbt_volume,$b0
	db ctrlbt_d_3      
	db ctrlbt_wait
	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_b_4      
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_4     
	db ctrlbt_wait
	db ctrlbt_volume,$f0 
	db ctrlbt_f_5       
	db ctrlbt_wait
	db ctrlbt_volume,$b0     
	db ctrlbt_d_5
	db ctrlbt_wait


        db ctrlbt_hold_mode,01
	db ctrlbt_track_loop


;--------------------------------------------------------

hezimatate_tr_1_pt_08:


                ; M 56

	db ctrlbt_song_speed,$28

	db ctrlbt_envelope,%0000_0_010
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$d0
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#4      
	db ctrlbt_wait
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_f#3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait


                ; M 57


	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_c#4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#3     
	db ctrlbt_wait
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait


                ; M 58


	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait


                ; M 59


	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 60


	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 61


	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_4     
	db ctrlbt_wait
	db ctrlbt_d_4       
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait


                ; M 62


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_g_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait



                ; M 63


	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_d_3     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_d_4
	db ctrlbt_wait


                ; M 64


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait


                ; M 65


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait
	db ctrlbt_b_3       
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait


                ; M 66


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_f#4
	db ctrlbt_wait



                ; M 67


	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_a_3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_c#4
	db ctrlbt_wait


                ; M 68


	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_c#4      
	db ctrlbt_wait
	db ctrlbt_f#3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait



                ; M 69


	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#4     
	db ctrlbt_wait
	db ctrlbt_e_4       
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait


                ; M 70


	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_a_3      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_c#4     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_f#3
	db ctrlbt_wait



                ; M 71


	db ctrlbt_c#4      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_f#3      
	db ctrlbt_wait
	db ctrlbt_b_3     
	db ctrlbt_wait
	db ctrlbt_f#3       
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait


                ; M 72


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_4      
	db ctrlbt_wait
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_g_3
	db ctrlbt_wait



                ; M 73


	db ctrlbt_e_3     
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait
	db ctrlbt_b_3       
	db ctrlbt_wait
	db ctrlbt_e_3
	db ctrlbt_wait


                ; M 74


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_d_4     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_e_4
	db ctrlbt_wait



                ; M 75


	db ctrlbt_d_4      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_g_3     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_b_3
	db ctrlbt_wait


                ; M 76


	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_e_3      
	db ctrlbt_wait
	db ctrlbt_b_3      
	db ctrlbt_wait
	db ctrlbt_e_3     
	db ctrlbt_wait
	db ctrlbt_e_3       
	db ctrlbt_wait
	db ctrlbt_a_3
	db ctrlbt_wait



                ; M 77


	db ctrlbt_e_3      
	db ctrlbt_wait
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



	db ctrlbt_track_loop



;--------------------------------------------------

hezimatate_tr_2_pt_00:


                ; counter melody


	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_volume,$30
	db ctrlbt_envelope,%0000_1_010


;        db ctrlbt_hold_mode,00
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00

        db ctrlbt_transpose,12

                ; M 1


      
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait   
	db ctrlbt_wait     
	db ctrlbt_volume,$30
	db ctrlbt_f_4 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_vibrato,$02,$3f
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$d0
	db ctrlbt_f_4


                ; M 2


	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
        db ctrlbt_hold_mode,01
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_1_010
	db ctrlbt_volume,$30  
	db ctrlbt_e_4   
	db ctrlbt_wait     
	db ctrlbt_wait 
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_g_4
	db ctrlbt_wait


                ; M 3

	db ctrlbt_wait
	db ctrlbt_vibrato,$02,$3f
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$d0
	db ctrlbt_d#4    
	db ctrlbt_wait   
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_1_010
	db ctrlbt_pulse_width,%10_000000
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$30  
	db ctrlbt_e_4 
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 4

	db ctrlbt_volume,$30
	db ctrlbt_g_4
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_vibrato,$02,$3f
        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_volume,$d0
	db ctrlbt_c#5     
	db ctrlbt_wait 
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_1_010        
        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$30  
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_e_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_f_4


                ; M 5

	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_volume,$30
	db ctrlbt_f_4   
	db ctrlbt_wait     
	db ctrlbt_wait 
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_volume,$10
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_g_4
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 6

	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait   
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_1_011
	db ctrlbt_volume,$30
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_a#4


                ; M 7

	db ctrlbt_wait
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000 
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 8

	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_envelope,%0000_1_011
	db ctrlbt_volume,$30
	db ctrlbt_c#5    
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_volume,$30  
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_d#5 
	db ctrlbt_wait
	db ctrlbt_pulse_width,%00_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

  
                ; M 9

	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%00_000000    
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_pulse_width,%10_000000  
	db ctrlbt_wait
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_wait
	db ctrlbt_pulse_width,%01_000000
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_wait
;	db ctrlbt_wait
	db ctrlbt_volume,$30
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_g#4

        db ctrlbt_transpose,00

        db ctrlbt_hold_mode,01

	db ctrlbt_track_loop


;------------------------------------------------------

hezimatate_tr_2_pt_01:


                ; M 10

        db ctrlbt_transpose,12

	db ctrlbt_vibrato,$02,$3f

	db ctrlbt_pulse_width,%10_000000
	db ctrlbt_envelope,%0000_1_011

;        db ctrlbt_hold_mode,00

	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_volume,$30    
	db ctrlbt_c#4       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 11

	db ctrlbt_volume,$30
	db ctrlbt_f_4
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$30       
	db ctrlbt_g#4    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30    
	db ctrlbt_d#4    
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_volume,$30  
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

                ; M 13

	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_volume,$30
	db ctrlbt_c#4    
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_f_4
	db ctrlbt_wait


                ; M 14

	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_volume,$30 
	db ctrlbt_g#4       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_volume,$30
	db ctrlbt_c#4
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


;------------------------------------------------------

hezimatate_tr_2_pt_02:


	db ctrlbt_envelope,%0000_0_110
	db ctrlbt_volume,$c0

                ; M 15

        db ctrlbt_hold_mode,00    
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
        db ctrlbt_hold_mode,01       
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_arpeggio,$0c,$0c,1
	db ctrlbt_c_4    
	db ctrlbt_d#4     
	db ctrlbt_f#4       
	db ctrlbt_d#4       
	db ctrlbt_f#4    
	db ctrlbt_d#4     
	db ctrlbt_c_4


                ; M 16

	db ctrlbt_d#4
	db ctrlbt_a#3       
	db ctrlbt_d_4    
	db ctrlbt_f_4       
	db ctrlbt_a#3    
	db ctrlbt_f_4     
	db ctrlbt_d_4       
	db ctrlbt_f_4       
	db ctrlbt_d_4    
	db ctrlbt_c_4     
	db ctrlbt_d#4


                ; M 17

	db ctrlbt_f#4
	db ctrlbt_d#4       
	db ctrlbt_f#4    
	db ctrlbt_d#4       
	db ctrlbt_c_4    
	db ctrlbt_d#4     
	db ctrlbt_f_4       
	db ctrlbt_d_4       
	db ctrlbt_a#3    
	db ctrlbt_f_4     
	db ctrlbt_d_4


                ; M 18

	db ctrlbt_f_4
        db ctrlbt_hold_mode,00    
	db ctrlbt_a#3       
	db ctrlbt_wait    
        db ctrlbt_hold_mode,01
	db ctrlbt_c_4       
	db ctrlbt_d#4    
	db ctrlbt_f#4     
	db ctrlbt_d#4       
	db ctrlbt_f#4       
	db ctrlbt_d#4    
	db ctrlbt_c_4     
	db ctrlbt_d#4


                ; M 19

	db ctrlbt_f_4
	db ctrlbt_d_4       
	db ctrlbt_a#3    
	db ctrlbt_f_4       
	db ctrlbt_d_4    
	db ctrlbt_f_4     
	db ctrlbt_d_4       
	db ctrlbt_f_4       
	db ctrlbt_c_4    
	db ctrlbt_d#4     
	db ctrlbt_f#4


                ; M 20

	db ctrlbt_d#4
	db ctrlbt_f#4       
	db ctrlbt_d#4    
	db ctrlbt_c_4       
	db ctrlbt_d#4
	db ctrlbt_arpeggio,$00,$00,0 

        db ctrlbt_hold_mode,00    
	db ctrlbt_envelope,%0000_0_111
	db ctrlbt_vibrato,$02,$3f
	db ctrlbt_a#4     
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 21

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


                ; M 22

	db ctrlbt_envelope,%0000_1_111
	db ctrlbt_volume,$30  

        db ctrlbt_hold_mode,01
	db ctrlbt_vibrato,$03,$3f
	db ctrlbt_a#4
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_arpeggio,$00,$00,0

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111  
	db ctrlbt_vibrato,$02,$3f 
	db ctrlbt_b_4      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 23

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30   
	db ctrlbt_b_4      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111     
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_d#5   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 24

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30  
	db ctrlbt_d#5     
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111       
	db ctrlbt_vibrato,$02,$3f 
	db ctrlbt_g_5    
	db ctrlbt_wait

	db ctrlbt_arpeggio,$00,$00,0


        db ctrlbt_transpose,00


	db ctrlbt_track_loop


;------------------------------------------------------

hezimatate_tr_2_pt_03:


	db ctrlbt_envelope,%0000_0_111
;	db ctrlbt_volume,$80



                ; M 25

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30  
	db ctrlbt_g_5   
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111       
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_d_5    
	db ctrlbt_wait


                ; M 26

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30         
	db ctrlbt_d_5      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 27

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111         
	db ctrlbt_vibrato,$02,$3f 
	db ctrlbt_e_5
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_vibrato,$04,$3f 
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30              
	db ctrlbt_e_5   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 28

	db ctrlbt_wait
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111         
	db ctrlbt_vibrato,$02,$3f   
	db ctrlbt_d#5      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_vibrato,$04,$3f 
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30              
	db ctrlbt_d#5
	db ctrlbt_wait


                ; M 29

	db ctrlbt_wait
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111         
	db ctrlbt_vibrato,$02,$3f   
	db ctrlbt_c_5   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30               
	db ctrlbt_c_5    
	db ctrlbt_wait


                ; M 30

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111         
	db ctrlbt_vibrato,$04,$3f 
	db ctrlbt_d_5      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 31

	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$30               
	db ctrlbt_d_5
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00
	db ctrlbt_volume,$b0
	db ctrlbt_envelope,%0000_0_111         
	db ctrlbt_vibrato,$02,$3f  
	db ctrlbt_a#5   
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 32

	db ctrlbt_vibrato,$02,$3f 
	db ctrlbt_wait
	db ctrlbt_wait      
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


                ; M 33

	db ctrlbt_vibrato,$06,$3f 
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_vibrato,$08,$3f 
	db ctrlbt_wait    
;	db ctrlbt_wait       
;	db ctrlbt_wait      
;	db ctrlbt_wait   
;	db ctrlbt_wait    
;	db ctrlbt_wait


        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00  


	db ctrlbt_track_loop


;-----------------------------------------------

hezimatate_tr_2_pt_04:


                ; M 34

                ; Lead

        db ctrlbt_hold_mode,00
;	db ctrlbt_pulse_width,%11_000000
	db ctrlbt_envelope,%0000_1_110
	db ctrlbt_volume,$40

        db ctrlbt_transpose,12

;        db ctrlbt_hold_mode,00
;        db ctrlbt_vibrato_reset

	db ctrlbt_auto_pwm,%01_00_01_00,1
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



                ; M 36

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_volume,$40
	db ctrlbt_f#4      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 37


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_volume,$40
	db ctrlbt_a#4     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 38


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_volume,$40
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait




                ; M 39


	db ctrlbt_volume,$40
	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_wait     
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
	db ctrlbt_volume,$40
	db ctrlbt_b_4      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



	db ctrlbt_auto_pwm,%10_00_11_01,00

	db ctrlbt_track_loop


;-----------------------------------------------

hezimatate_tr_2_pt_05:


                ; M 42

	db ctrlbt_auto_pwm,%11_00_11_00,1

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_volume,$40   
	db ctrlbt_a#4     
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
	db ctrlbt_volume,$40
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_auto_pwm,%10_00_11_01,00

	db ctrlbt_track_loop


;--------------------------------------------------------

hezimatate_tr_2_pt_06:


                ; M 44

	db ctrlbt_auto_pwm,%01_00_01_00,1
	db ctrlbt_volume,$40

	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_wait     
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

	db ctrlbt_volume,$40

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




                ; M 46


	db ctrlbt_volume,$40

	db ctrlbt_a#4      
	db ctrlbt_wait
	db ctrlbt_wait     
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

	db ctrlbt_volume,$40

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


                ; M 49

	db ctrlbt_volume,$40
	db ctrlbt_a#4     
	db ctrlbt_wait
	db ctrlbt_wait     
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


	db ctrlbt_volume,$40
	db ctrlbt_g#4     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait

	db ctrlbt_auto_pwm,%10_00_11_01,00

	db ctrlbt_track_loop


;-----------------------------------------------

hezimatate_tr_2_pt_07:


                ; M 52

	db ctrlbt_auto_pwm,%01_00_01_00,1
	db ctrlbt_volume,$40

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

	db ctrlbt_volume,$40

	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 55

	db ctrlbt_volume,$40

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

	db ctrlbt_auto_pwm,%10_00_11_01,00

        db ctrlbt_transpose,00

	db ctrlbt_track_loop



;----------------------------------------------------
;--------------------------------------------------------

hezimatate_tr_2_pt_08:


                ; M 56


        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$b0
	db ctrlbt_f#4      
	db ctrlbt_g#4
	db ctrlbt_a#4      
	db ctrlbt_g#4
	db ctrlbt_a#4      
	db ctrlbt_c#5
	db ctrlbt_a#4     
	db ctrlbt_c#5
	db ctrlbt_d#5       
	db ctrlbt_c#5
	db ctrlbt_d#5
	db ctrlbt_f#5


                ; M 57


	db ctrlbt_d#5      
	db ctrlbt_f#5
	db ctrlbt_g#5     
	db ctrlbt_f#5
	db ctrlbt_g#5      
	db ctrlbt_a#5
	db ctrlbt_g#5     
	db ctrlbt_a#5
	db ctrlbt_c#6       
	db ctrlbt_a#5
	db ctrlbt_c#6
	db ctrlbt_d#6


                ; M 58


	db ctrlbt_d#6      
	db ctrlbt_c#6
	db ctrlbt_a#5      
	db ctrlbt_c#6
	db ctrlbt_a#5      
	db ctrlbt_g#5
	db ctrlbt_a#5     
	db ctrlbt_g#5
	db ctrlbt_f#5       
	db ctrlbt_g#5
	db ctrlbt_f#5
	db ctrlbt_d#5


                ; M 59


	db ctrlbt_f#5      
	db ctrlbt_d#5
	db ctrlbt_c#5      
	db ctrlbt_d#5
	db ctrlbt_c#5      
	db ctrlbt_a#4
	db ctrlbt_c#5    
	db ctrlbt_a#4
	db ctrlbt_g#4       
	db ctrlbt_a#4
	db ctrlbt_g#4
	db ctrlbt_f#4


                ; M 60

	db ctrlbt_f#4      
	db ctrlbt_g#4
	db ctrlbt_a#4      
	db ctrlbt_g#4
	db ctrlbt_a#4      
	db ctrlbt_c#5
	db ctrlbt_a#4     
	db ctrlbt_c#5
	db ctrlbt_d#5       
	db ctrlbt_c#5
	db ctrlbt_d#5
	db ctrlbt_f#5



                ; M 61

	db ctrlbt_d#5      
	db ctrlbt_f#5
	db ctrlbt_g#5     
	db ctrlbt_f#5
	db ctrlbt_e_4      
	db ctrlbt_f#4
	db ctrlbt_g#4     
	db ctrlbt_f#4
	db ctrlbt_g#4       
	db ctrlbt_b_4
	db ctrlbt_g#4
	db ctrlbt_b_4



                ; M 62


	db ctrlbt_c#5      
	db ctrlbt_b_4
	db ctrlbt_c#5      
	db ctrlbt_e_5
	db ctrlbt_c#5      
	db ctrlbt_e_5
	db ctrlbt_f#5     
	db ctrlbt_e_5
	db ctrlbt_f#5       
	db ctrlbt_g#5
	db ctrlbt_f#5
	db ctrlbt_g#5



                ; M 63


	db ctrlbt_b_5      
	db ctrlbt_g#5
	db ctrlbt_b_5      
	db ctrlbt_c#6
	db ctrlbt_c#6      
	db ctrlbt_b_5
	db ctrlbt_g#5     
	db ctrlbt_b_5
	db ctrlbt_g#5       
	db ctrlbt_f#5
	db ctrlbt_g#5
	db ctrlbt_f#5


                ; M 64


	db ctrlbt_e_5      
	db ctrlbt_f#5
	db ctrlbt_e_5      
	db ctrlbt_c#5
	db ctrlbt_e_5      
	db ctrlbt_c#5
	db ctrlbt_b_4     
	db ctrlbt_c#5
	db ctrlbt_b_4       
	db ctrlbt_g#4
	db ctrlbt_b_4
	db ctrlbt_g#4


                ; M 65


	db ctrlbt_f#4      
	db ctrlbt_g#4
	db ctrlbt_f#4      
	db ctrlbt_e_4
	db ctrlbt_e_4     
	db ctrlbt_f#4
	db ctrlbt_g#4    
	db ctrlbt_f#4
	db ctrlbt_g#4      
	db ctrlbt_b_4
	db ctrlbt_g#4
	db ctrlbt_b_4


                ; M 66


	db ctrlbt_c#5      
	db ctrlbt_b_4
	db ctrlbt_c#5      
	db ctrlbt_e_5
	db ctrlbt_c#5      
	db ctrlbt_e_5
	db ctrlbt_f#5     
	db ctrlbt_e_5
	db ctrlbt_f#4       
	db ctrlbt_g#4
	db ctrlbt_a#4
	db ctrlbt_g#4



                ; M 67


	db ctrlbt_a#4      
	db ctrlbt_c#5
	db ctrlbt_a#4      
	db ctrlbt_c#5
	db ctrlbt_d#5      
	db ctrlbt_c#5
	db ctrlbt_d#5     
	db ctrlbt_f#5
	db ctrlbt_d#5       
	db ctrlbt_f#5
	db ctrlbt_g#5
	db ctrlbt_f#5


                ; M 68


	db ctrlbt_g#5      
	db ctrlbt_a#5
	db ctrlbt_g#5      
	db ctrlbt_a#5
	db ctrlbt_c#6      
	db ctrlbt_a#5
	db ctrlbt_c#6     
	db ctrlbt_d#6
	db ctrlbt_d#6       
	db ctrlbt_c#6
	db ctrlbt_a#5
	db ctrlbt_c#6



                ; M 69


	db ctrlbt_a#5     
	db ctrlbt_g#5
	db ctrlbt_a#5      
	db ctrlbt_g#5
	db ctrlbt_f#5      
	db ctrlbt_g#5
	db ctrlbt_f#5     
	db ctrlbt_d#5
	db ctrlbt_f#5      
	db ctrlbt_d#5
	db ctrlbt_c#5
	db ctrlbt_d#5


                ; M 70


	db ctrlbt_c#5      
	db ctrlbt_a#4
	db ctrlbt_c#5     
	db ctrlbt_a#4
	db ctrlbt_g#4     
	db ctrlbt_a#4
	db ctrlbt_g#4    
	db ctrlbt_f#4
	db ctrlbt_f#4       
	db ctrlbt_g#4
	db ctrlbt_a#4
	db ctrlbt_g#4



                ; M 71


	db ctrlbt_a#4     
	db ctrlbt_c#5
	db ctrlbt_a#4      
	db ctrlbt_c#5
	db ctrlbt_d#5     
	db ctrlbt_c#5
	db ctrlbt_d#5     
	db ctrlbt_f#5
	db ctrlbt_d#5       
	db ctrlbt_f#5
	db ctrlbt_g#5
	db ctrlbt_f#5


                ; M 72


	db ctrlbt_e_4      
	db ctrlbt_f#4
	db ctrlbt_g#4      
	db ctrlbt_f#4
	db ctrlbt_g#4      
	db ctrlbt_b_4
	db ctrlbt_g#4     
	db ctrlbt_b_4
	db ctrlbt_c#5       
	db ctrlbt_b_4
	db ctrlbt_c#5
	db ctrlbt_e_5



                ; M 73


	db ctrlbt_c#5     
	db ctrlbt_e_5
	db ctrlbt_f#5      
	db ctrlbt_e_5
	db ctrlbt_f#5      
	db ctrlbt_g#5
	db ctrlbt_f#5     
	db ctrlbt_g#5
	db ctrlbt_b_5      
	db ctrlbt_g#5
	db ctrlbt_b_5
	db ctrlbt_c#6


                ; M 74


	db ctrlbt_c#6      
	db ctrlbt_b_5
	db ctrlbt_g#5     
	db ctrlbt_b_5
	db ctrlbt_g#5      
	db ctrlbt_f#5
	db ctrlbt_g#5     
	db ctrlbt_f#5
	db ctrlbt_e_5       
	db ctrlbt_f#5
	db ctrlbt_e_5
	db ctrlbt_c#5



                ; M 75


	db ctrlbt_e_5      
	db ctrlbt_c#5
	db ctrlbt_b_4      
	db ctrlbt_c#5
	db ctrlbt_b_4      
	db ctrlbt_g#4
	db ctrlbt_b_4    
	db ctrlbt_g#4
	db ctrlbt_f#4       
	db ctrlbt_g#4
	db ctrlbt_f#4
	db ctrlbt_e_4


                ; M 76


	db ctrlbt_e_4      
	db ctrlbt_f#4
	db ctrlbt_g#4      
	db ctrlbt_f#4
	db ctrlbt_g#4      
	db ctrlbt_b_4
	db ctrlbt_g#4    
	db ctrlbt_b_4
	db ctrlbt_c#5       
	db ctrlbt_b_4
	db ctrlbt_c#5
	db ctrlbt_e_5



                ; M 77


	db ctrlbt_c#5     
	db ctrlbt_e_5
	db ctrlbt_f#5      
	db ctrlbt_e_5
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



;---------------------------------------------------

hezimatate_tr_3_pt_00:


;        db ctrlbt_transpose,12

        db ctrlbt_hold_mode,0

                ; Bass melody
                ; M 1

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$60
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$65    
	db ctrlbt_a#3       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70     
	db ctrlbt_c_4       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89
	db ctrlbt_a#3


                ; M 2


	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$60
	db ctrlbt_c_4       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$65      
	db ctrlbt_a#3      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86    
	db ctrlbt_c_4       
	db ctrlbt_wait



                ; M 3


	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$75      
	db ctrlbt_f#3
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$60      
	db ctrlbt_c_4       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$65       
	db ctrlbt_a#3        
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 4


	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89
	db ctrlbt_a#3
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$75
	db ctrlbt_c_4      
	db ctrlbt_wait
	db ctrlbt_wait        
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$65       
	db ctrlbt_a#3


                ; M 5


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$7f       
	db ctrlbt_d#4      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$60
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
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70    
	db ctrlbt_d#4       


                ; M 7

	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c      
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_wait       
	db ctrlbt_wait      


                ; M 8

	db ctrlbt_wait      
	db ctrlbt_c#4
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c      
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85      
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84       
	db ctrlbt_wait      


                ; M 9

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83      
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$82       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$81       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$79       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$78       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$76             
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$74       
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$72       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
;	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_g#3      


	db ctrlbt_track_loop


;-----------------------------------------------

hezimatate_tr_3_pt_01:


        db ctrlbt_hold_mode,00

                ; M 10

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$79      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$78       
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$77       
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$76       
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$75       
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$74       
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$73       
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$3f
	db ctrlbt_c#4       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f              
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 11

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f             
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b        
	db ctrlbt_wait     
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70              
	db ctrlbt_g#4    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f            
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait


                ; M 12

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70              
	db ctrlbt_d#4    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f              
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b              
	db ctrlbt_wait  
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70            
	db ctrlbt_f_4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_g#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait

                ; M 13

	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70          
	db ctrlbt_c#4    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f         
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_d#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_f_4
	db ctrlbt_wait


                ; M 14

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70        
	db ctrlbt_g#4       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f              
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_c#4
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3f      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$3b       
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_portamento,$00


	db ctrlbt_track_loop


;-----------------------------------------------

hezimatate_tr_3_pt_02:


        db ctrlbt_hold_mode,00


                ; M 15

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$9f
	db ctrlbt_vibrato,$04,$3f
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_d#4
	db ctrlbt_wait       
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_waveform,$57
        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$00      
	db ctrlbt_c_4    
	db ctrlbt_d#4     
	db ctrlbt_f#4       
	db ctrlbt_d#4       
	db ctrlbt_f#4    
	db ctrlbt_d#4     
	db ctrlbt_c_4


                ; M 16

	db ctrlbt_d#4
	db ctrlbt_a#3       
	db ctrlbt_d_4    
	db ctrlbt_f_4       
	db ctrlbt_a#3    
	db ctrlbt_f_4     
	db ctrlbt_d_4       
	db ctrlbt_f_4       
	db ctrlbt_d_4    
	db ctrlbt_c_4     
	db ctrlbt_d#4


                ; M 17

	db ctrlbt_f#4
	db ctrlbt_d#4       
	db ctrlbt_f#4    
	db ctrlbt_d#4       
	db ctrlbt_c_4    
	db ctrlbt_d#4     
	db ctrlbt_f_4       
	db ctrlbt_d_4       
	db ctrlbt_a#3    
	db ctrlbt_f_4     
	db ctrlbt_d_4


                ; M 18

	db ctrlbt_f_4
	db ctrlbt_a#3       
	db ctrlbt_wait    
	db ctrlbt_c_4       
	db ctrlbt_d#4    
	db ctrlbt_f#4     
	db ctrlbt_d#4       
	db ctrlbt_f#4       
	db ctrlbt_d#4    
	db ctrlbt_c_4     
	db ctrlbt_d#4


                ; M 19

	db ctrlbt_f_4
	db ctrlbt_d_4       
	db ctrlbt_a#3    
	db ctrlbt_f_4       
	db ctrlbt_d_4    
	db ctrlbt_f_4     
	db ctrlbt_d_4       
	db ctrlbt_f_4       
	db ctrlbt_c_4    
	db ctrlbt_d#4     
	db ctrlbt_f#4


                ; M 20

	db ctrlbt_d#4
	db ctrlbt_f#4       
	db ctrlbt_d#4    
	db ctrlbt_c_4       
	db ctrlbt_d#4    
	db ctrlbt_a#4     
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c     
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89 
	db ctrlbt_wait


                ; M 21

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
;	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83 
;	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70      
;	db ctrlbt_wait   
;	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c   
;	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
;	db ctrlbt_wait    
;	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89      
;	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86    
;	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83
;	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70  
;	db ctrlbt_wait


                ; M 22

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$3f      
	db ctrlbt_e_4      
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait


                ; M 23

	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86     
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70   
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_d#3   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c    
	db ctrlbt_wait


                ; M 24

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70   
	db ctrlbt_wait    
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_c_3    
	db ctrlbt_wait


	db ctrlbt_track_loop


;-----------------------------------------------

hezimatate_tr_3_pt_03:



                ; M 25

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86     
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70 
	db ctrlbt_wait       
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_g_3


                ; M 26

	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70    
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89    
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86 
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70    
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 27

	db ctrlbt_e_3
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89 
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86 
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70  
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 28

	db ctrlbt_wait
	db ctrlbt_d#3      
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70   
	db ctrlbt_wait   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86       
	db ctrlbt_wait     
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70   
	db ctrlbt_wait    
	db ctrlbt_wait


                ; M 29

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_c_3   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c   
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89    
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86 
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70    
	db ctrlbt_wait


                ; M 30

	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_g_3      
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait       
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89  
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86 
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83 
	db ctrlbt_wait


                ; M 31

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_f#3   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8c  
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70       
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89   
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86     
	db ctrlbt_wait


                ; M 32

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83 
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$70
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$8a
	db ctrlbt_wait      
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$89
	db ctrlbt_wait       
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$88
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$87
	db ctrlbt_wait   
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$86
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85
	db ctrlbt_wait


                ; M 33

	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$84
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$83      
	db ctrlbt_wait   
	db ctrlbt_wait      
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$81
	db ctrlbt_wait   
	db ctrlbt_wait    
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$80
;	db ctrlbt_wait       
;	db ctrlbt_wait      
;	db ctrlbt_wait   
;	db ctrlbt_wait    
;	db ctrlbt_wait

	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00

	db ctrlbt_track_loop

;------------------------------------------------


;------------------------------------------------


hezimatate_tr_3_pt_04:




                ; M 34

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$1f
        db ctrlbt_hold_mode,0
	db ctrlbt_restart_envelope
	db ctrlbt_waveform,$85

	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_c#5      
	db ctrlbt_wait     
	db ctrlbt_d_5
	db ctrlbt_wait


                ; M 35


	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_c#5      
	db ctrlbt_wait     
	db ctrlbt_d_5
	db ctrlbt_wait


                ; M 36

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



                ; M 37


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_f#5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_g#5
	db ctrlbt_wait


                ; M 38


	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_f#5    
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_d#5     
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 39


	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_c#5      
	db ctrlbt_wait     
	db ctrlbt_d_5
	db ctrlbt_wait



                ; M 40


	db ctrlbt_d_5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_d_5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_c#5      
	db ctrlbt_wait     
	db ctrlbt_d_5
	db ctrlbt_wait


                ; M 41


	db ctrlbt_d_5      
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



	db ctrlbt_track_loop

;------------------------------------------------


hezimatate_tr_3_pt_05:


                ; M 42


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_f#5      
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_g#5
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
	db ctrlbt_a#5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait




	db ctrlbt_track_loop


;--------------------------------------------------------

hezimatate_tr_3_pt_06:


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
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 46



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


                ; M 47


	db ctrlbt_vibrato,$03,$1f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
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

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$1f
	db ctrlbt_d#5     
	db ctrlbt_wait
	db ctrlbt_wait     
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


	db ctrlbt_f#5    
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_vibrato,$03,$1f
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 50

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$1f
	db ctrlbt_g#5     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_vibrato,$03,$1f
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 51


	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$2f
	db ctrlbt_g#6     
	db ctrlbt_wait
	db ctrlbt_wait     
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
	db ctrlbt_vibrato,$00,$1f
	db ctrlbt_restart_envelope
	db ctrlbt_portamento,$00
	db ctrlbt_track_loop

;------------------------------------------------


hezimatate_tr_3_pt_07:



                ; M 52


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


                ; M 53


	db ctrlbt_vibrato,$02,$1f
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
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


	db ctrlbt_vibrato,$03,$1f
	db ctrlbt_wait      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 55

	db ctrlbt_vibrato,$04,$1f

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_note_off

        db ctrlbt_vibrato_reset
	db ctrlbt_vibrato,$00,$1f

	db ctrlbt_track_loop



;--------------------------------------------------
;------------------------------------------------


hezimatate_tr_3_pt_08:



                ; M 56

	db ctrlbt_portamento,$96

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
	db ctrlbt_b_5
	db ctrlbt_wait


                ; M 57


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
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 58


	db ctrlbt_a#5      
	db ctrlbt_wait
	db ctrlbt_c#6     
	db ctrlbt_wait      
	db ctrlbt_b_5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 59


	db ctrlbt_e_6     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_b_5    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_c#6
	db ctrlbt_wait


                ; M 60


	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_b_5      
	db ctrlbt_wait     
	db ctrlbt_c#6
	db ctrlbt_wait


                ; M 61


	db ctrlbt_b_5     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_a#5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 62


	db ctrlbt_f#5      
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 63


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_a#5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 64


	db ctrlbt_b_5    
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_a#5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_g#5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 65


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
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
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_e_6      
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
	db ctrlbt_b_5    
	db ctrlbt_wait
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_c#6
	db ctrlbt_wait



                ; M 68


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_a#5      
	db ctrlbt_wait     
	db ctrlbt_c#6
	db ctrlbt_wait


                ; M 69


	db ctrlbt_b_5     
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



                ; M 70


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
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 71


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_b_5     
	db ctrlbt_wait
	db ctrlbt_c#6    
	db ctrlbt_wait
	db ctrlbt_b_5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait


                ; M 72


	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_a#5     
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_f#5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 73

	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait     
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
	db ctrlbt_a#5    
	db ctrlbt_wait
	db ctrlbt_wait    
	db ctrlbt_wait
	db ctrlbt_b_5      
	db ctrlbt_wait     
	db ctrlbt_wait
	db ctrlbt_wait



                ; M 75


	db ctrlbt_a#5     
	db ctrlbt_wait
	db ctrlbt_wait     
	db ctrlbt_wait      
	db ctrlbt_g#5     
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
	db ctrlbt_wait
	db ctrlbt_wait


	db ctrlbt_portamento,$00

	db ctrlbt_track_loop





;--------------------------------------------------


hezimatate_tr_4_pt_00:



                ; M 1


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$2b
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $40+$37
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f

                ; M 2


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_volume,$e0			
	db $3f
	db ctrlbt_volume,$b0
;        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $3f
;	db ctrlbt_volume,$80
;        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 3


	db ctrlbt_volume,$80
	db $40+$2b
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f



                ; M 4

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$c0
	db $40+$27
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$2b
	db ctrlbt_volume,$e0
	db ctrlbt_wait
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $40+$37
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f



                ; M 5


	db ctrlbt_volume,$80
	db $40+$27
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f


                ; M 6


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_volume,$e0			
	db $3f
	db ctrlbt_volume,$b0
;        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $3f
;	db ctrlbt_volume,$80
;        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 7


	db ctrlbt_volume,$80
	db $40+$2b
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f



                ; M 8


	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_volume,$e0			
	db $3f
	db ctrlbt_volume,$b0
;        db ctrlbt_hold_mode,00
	db ctrlbt_envelope,%0000_0_111
	db $40+$27
	db ctrlbt_wait
	db ctrlbt_envelope,%0000_0_011
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_wait
	db ctrlbt_volume,$90
	db $3f
	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $3f
;	db ctrlbt_volume,$80
;        db ctrlbt_hold_mode,01
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 9


	db ctrlbt_volume,$80
	db $40+$2b
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$60
	db $37
	db ctrlbt_volume,$70
	db $37
	db ctrlbt_volume,$90
	db $37
	db ctrlbt_volume,$b0
;	db $37


	db ctrlbt_track_loop

;--------------------------------------------------

hezimatate_tr_4_pt_01:


                ; M 10


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f


                ; M 11


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f


                ; M 12


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f


                ; M 13


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f


                ; M 14


	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f



	db ctrlbt_track_loop

;--------------------------------------------------

hezimatate_tr_4_pt_02:



                ; M 15


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 16


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 17


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 18


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 19


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 20


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 21


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
;	db $3f
	db ctrlbt_volume,$70			
;	db $3f
	db ctrlbt_volume,$80
;	db $3f
	db ctrlbt_volume,$e0
;	db $3f
	db ctrlbt_volume,$70
;	db $3f
	db ctrlbt_volume,$80
;	db $3f
	db ctrlbt_volume,$e0
;	db $3f
	db ctrlbt_volume,$80
;	db $3f
	db ctrlbt_volume,$e0
;	db $3f
	db ctrlbt_volume,$80
;	db $3f
	db ctrlbt_volume,$d0
;	db $3f


                ; M 22


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 23


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 24


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f




	db ctrlbt_track_loop

;--------------------------------------------------

hezimatate_tr_4_pt_03:


                ; M 25

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 26


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 27


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 28


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 29


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 30


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 31


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 32


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f


                ; M 33


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
;	db $3f
	db ctrlbt_volume,$80
;	db $3f
	db ctrlbt_volume,$e0
;	db $3f
	db ctrlbt_volume,$80
;	db $3f
	db ctrlbt_volume,$d0
;	db $3f

	db ctrlbt_track_loop


;------------------------------------------------------------


;------------------------------------------------------------


hezimatate_tr_4_pt_04:


                ; M 34


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 35


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 36


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 37


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 38


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 39

	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 40


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 41


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


	db ctrlbt_track_loop


;------------------------------------------------------------


hezimatate_tr_4_pt_05:



                ; M 42


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 43


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 44


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


	db ctrlbt_track_loop

;--------------------------------------------------


hezimatate_tr_4_pt_06:


                ; M 45


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f

                ; M 46


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 47


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 48


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 49


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 50


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 51


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



	db ctrlbt_track_loop


;------------------------------------------------------------


hezimatate_tr_4_pt_07:



                ; M 52


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 53


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 54


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 55


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


	db ctrlbt_track_loop

;-----------------------------------------------

hezimatate_tr_4_pt_08:



                ; M 56


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 57


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


               ; M 58


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 59


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 60


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 61


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 62


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_100
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 63


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 64


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 65


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 66


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 67


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 68


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0	
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 69


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 70


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 71


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 72


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 73


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$e0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 74


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 75


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f


                ; M 76


	db ctrlbt_envelope,%0000_0_100
	db ctrlbt_volume,$e0
	db $40+$24
	db ctrlbt_volume,$70			
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$90
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$80
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$d0
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$80
	db $3f



                ; M 77


	db ctrlbt_envelope,%0000_0_001
	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$70			
	db $3f
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$70
	db ctrlbt_envelope,%0000_0_100
	db $40+$24
	db ctrlbt_volume,$80
	db ctrlbt_envelope,%0000_0_001
	db $3f
	db ctrlbt_volume,$e0
	db $3f
	db ctrlbt_volume,$80
	db $3f

	db ctrlbt_volume,$e0
	db $40+$30
	db ctrlbt_volume,$80
	db $3f
	db ctrlbt_volume,$d0
	db $3f
	db ctrlbt_volume,$80
	db $3f



	db ctrlbt_track_loop

	db ctrlbt_arpeggio,$00,$00,0
	db ctrlbt_volume,$50
	db ctrlbt_c_5      
	db ctrlbt_e_5
	db ctrlbt_volume,$60
	db ctrlbt_g_5       
	db ctrlbt_c_6      
	db ctrlbt_volume,$70
	db ctrlbt_c#6       
	db ctrlbt_c_6      
	db ctrlbt_volume,$80
	db ctrlbt_a#5      
	db ctrlbt_c_5
	db ctrlbt_volume,$a0
	db ctrlbt_g_5        
	db ctrlbt_c_6       

	db ctrlbt_track_loop

hezimatate_end:

        end           
    
;-----------------------------------------------
