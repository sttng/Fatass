;---------------------------------------------------------------------------
; core keypad control routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; joy_update:	the main keypad control routine
; joy_repeat_update:  same as above, but with repeat function
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; variables
;---------------------------------------------------------------------------

joy_pressed		equ joy_ram+$00	; $01 bytes - which buttons have been PRESSED
joy_held		equ joy_ram+$01	; $01 bytes - old button status - which buttons are held
joy_held_repeat		equ joy_ram+$02	; $01 bytes - which buttons are triggered on the repeat delay

joy_repeat_time_max	equ joy_ram+$03	; $01 bytes - initial value for the joy repeat
joy_repeat_time_min	equ joy_ram+$04	; $01 bytes - initial value for the joy repeat
joy_repeat_time		equ joy_ram+$05	; $01 bytes - initial value for the joy repeat

joy_repeat_count	equ joy_ram+$06	; $01 bytes - counter for the joy repeat flag

;---------------------------------------------------------------------------
; defines
;---------------------------------------------------------------------------

joy_down	equ %10000000
joy_up		equ %01000000
joy_left	equ %00100000
joy_right	equ %00010000
joy_start	equ %00001000
joy_select	equ %00000100
joy_b		equ %00000010
joy_a		equ %00000001

joy_bit_down	equ 7
joy_bit_up	equ 6
joy_bit_left	equ 5
joy_bit_right	equ 4
joy_bit_start	equ 3
joy_bit_select	equ 2
joy_bit_b	equ 1
joy_bit_a	equ 0

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; joy_update
;
; gets the status of the keypad and updates it
;---------------------------------------------------------------------------
;***************************************************************************

joy_update:

        ld a,$20 		; Set 0 at the output line P14
        ld ($ff00),a
        ld a,($ff00)	 	; Read JOYPAD several times to accomodate the noise
        ld a,($ff00)
        cpl			; Bits 0-3 are now 1s if corresponding buttons pressed
        and $0f			; Extract lower 4 bits carrying button status...
        swap a  		; ...and move them into upper for bits
        ld b,a	       		; At this point: B = START.SELECT.B.A.x.x.x.x
        ld a,$10		; Set 0 at the output line P15
        ld ($ff00),a 
        ld a,($ff00)		; Read JOYPAD several times to accomodate the noise
        ld a,($ff00) 
        ld a,($ff00) 
        ld a,($ff00) 
        ld a,($ff00) 
        ld a,($ff00) 
        cpl         	 	; Bits 0-3 are now 1s if corresponding buttons pressed
        and $0f      		; Extract lower 4 bits carrying buttons' status...
        or b         		; ...and combine them with 4 other button status bits
        ld d,a	       		; At this point: D = START.SELECT.B.A.DOWN.UP.LEFT.RIGHT
        ld a,(joy_held)		; Read old button status from RAM
        xor d        		; Set 1s for buttons whose status has changed
        and d        		; Extract buttons which were *pressed* since held check
        ld (joy_pressed),a 	; Save information of those buttons
        ld a,$30     		; Set 1s at both P14 and P15 lines
        ld ($ff00),a 		; [probably to reset the circuitry]
        ld a,d       		; Update button status in RAM
        ld (joy_held),a 
	ret 

;	cp joy_start+joy_select+joy_b+joy_a
;	ret nz
;	xor a
;	ld (joy_pressed),a
;	ld a,(gbc)
;	cp $00
;	jp z,reset
;	ld a,$11
;	jp reset
;	jp setup_main_menu

;***************************************************************************
;---------------------------------------------------------------------------
; joy_repeat_update
;
; gets the status of the keypad and updates it
;---------------------------------------------------------------------------

joy_repeat_update:

	call joy_update
	ld a,(joy_pressed)
	and $f0
	jr z,_no_new_pressed
	ld (joy_held_repeat),a

	ld a,(joy_repeat_time_max)
	ld (joy_repeat_time),a
	ld (joy_repeat_count),a
	ret

_no_new_pressed:
	xor a
	ld (joy_held_repeat),a
	ld a,(joy_held)
	and $f0
	ret z
	
	ld a,(joy_repeat_count)
	dec a
	ld (joy_repeat_count),a
	ret nz

	ld a,(joy_repeat_time_min)
	ld b,a
	ld a,(joy_repeat_time)
	cp b
	jr z,_no_dec
	dec a
	cp b
	jr z,_no_dec
	dec a
_no_dec:
	ld (joy_repeat_time),a
	ld (joy_repeat_count),a
	
	ld a,(joy_held)
	and $f0
	ld (joy_held_repeat),a
	ret
	
;***************************************************************************

	end
