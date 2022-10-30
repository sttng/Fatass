;---------------------------------------------------------------------------
; gb/gbc sample player core routines
; by Jeremy Evers
; last updated 12:54 AM 9/28/99
;
; sam_setup: 	setup or reset the sample player
;		this sets up the timer irq
; play_sample:	call this with the index of the sample to play in A
;		this turns on the timer irq
; stop_sample:	call this to stop a sample that is playing back
;		this turns off the timer irq
; tim_irq_fast: play the sample routines with fast method
; tim_irq_hq:	play the sample routines with high quality method
;		org $000:$0050	; Timer Overflow
;		jp tim_irq	; do our timer routine
;---------------------------------------------------------------------------
; these routines have been optimized quite a few times now
;---------------------------------------------------------------------------
; samplebank00 contains 3 tables:
;
; sample_bank_table: 	simply a table of bytes that point to the bank
;			where the sample resides
; sample_start_table:	simply a table of words that point to the start
;			address of the sample
; sample_length_table:	simply a table of words that point to the end of the sample
;---------------------------------------------------------------------------
; notes:-the sample player turns the timer irq off when not playing a sample
;	 and turns the timer irq on when it is playing a sample
;	-samples do not span banks, this will not happen
;	-all sample index data must be in samplebank00
;	-only $ccd (3277) different samples may be used
;	-assumes 32megabit cart or less
;	-up to 1/256th second delay+length of sample_play routine delay on
;	 trigger - definitely up to 1/256th second variance on trigger
;	 could use $ff05 (TIMA) to force sync, but it is likely unecessary,
; 	 as the variance is pretty much unnoticeable
; 	-a higher priority sample will override a sample of the same or less
; 	 priority that is already playing
;	-if you always use a priority of 1 or more, you can check
;	 sample_priority instead of $ffff to see if a sample is playing
;	-should add up the cycle counts again to verify
;---------------------------------------------------------------------------

;***************************************************************************
; variables

sam_bank		equ sam_ram+$00	; $01 byte	; bank of the sample
sam_position		equ sam_ram+$01	; $02 bytes	; address of the next sample pack
sam_end			equ sam_ram+$03	; $02 bytes	; address of the end
timer_rom_bank		equ sam_ram+$05 ; $01 bytes	; temporary holder for the rom bank
current_rom_bank	equ sam_ram+$06	; $01 bytes	; marker for the rom bank, used by everything
stack_temp		equ sam_ram+$07	; $02 bytes	; temporary holder for the stack pointer
sample_priority		equ sam_ram+$09 ; $01 bytes	; sample priority - doesn't really need to be in high ram
current_sram_bank	equ sam_ram+$0a ; $01 bytes	; marker for the sram bank, used by everything
current_wram_bank	equ sam_ram+$0b ; $01 bytes	; marker for the wram bank, used by everything

;***************************************************************************
; routines

;***************************************************************************
;---------------------------------------------------------------------------
; sam_setup
; setup/reset the sample player
; call this routine in your initial setup, AFTER the music player setup
; ensure that sound is turned on, and global volume and panning are set
; Destroys A
;---------------------------------------------------------------------------
;***************************************************************************

sam_setup:

	ld a,($ffff)		; turn off timer interrupt
	and %11111011
	ld ($ffff),a

	; audio globals already set in music_hard_reset
	; which is sound on and then global volume and panning

	; set 256hz timer for sample player
	; timer is 4096hz/16 = 256hz for singlespeed
	; timer is 2*4096hz/32 = 256hz for doublespeed
	; f = 4194304hz in singlespeed mode
	; f = 8388608hz in doublespeed mode

	ld a,(gbc)
	and a
	jr z,_dmg

	ld a,256-32		; modulo is so that timer is 1/32 for double speed
	jr _gbc

_dmg:
	ld a,256-16		; modulo is so that timer is 1/16 for single speed
_gbc:

	ld ($ff06),a		; TMA
	ld a,%00000100		; freq is (f/2^10)*modulo and Timer mode = run
	ld ($ff07),a		; TAC

	xor a
	ld (sample_priority),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; play_sample
; Call this routine with:
; DE - the index of the sample
; L - the priority of the sample
; a higher priority sample will override a sample of the same or less
; priority that is already playing.
; Destroys A,DE,HL
;---------------------------------------------------------------------------
;***************************************************************************

play_sample:

	ld a,(sample_priority)	; check to see if a high priority sample is
	ld h,a			; already playing
	ld a,l			
	cp h			; if the old sample is less, 
	ret c			; abort

	ld (sample_priority),a	; store new sample priority
	cp 2
	jr c,_music_sample_pan

	ld a,%0_01_00000
	ld ($ff1c),a		; set sound 3 volume to full

	ld a,($ff25)
	or %0_1_0_0_0_1_0_0
	ld ($ff25),a

	jr _normal_sample_pan

_music_sample_pan:

	ld a,(mus_track_3+o_panning)
	ld l,a
	ld a,($ff25)
	or l
	ld ($ff25),a

_normal_sample_pan:

	ld hl,sample_bank_table
	add hl,de		; hl now points to the sample bank

	ld a,samplebank00	; our sample tables are stored in samplebank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,($ffff)		; turn off timer interrupt to prevent weirdness
	and %11111011
	ld ($ffff),a

	ld a,(hl)		; get the sample bank
	ld (sam_bank),a

	sla e			; multiply our index by 2 for dealing with words
	rl d
	ld hl,sample_start_table
	add hl,de		; hl now points to the entry in the start address table

	ld a,(hli)		; store the start address
	ld (sam_position),a
	ld a,(hl)
	ld (sam_position+1),a

	ld hl,sample_length_table
	add hl,de		; hl now points to the entry in the sample length table

	ld a,(hli)		; store the sample length
	ld (sam_end),a
	ld a,(hl)
	ld (sam_end+1),a

;	ld a,$ff
;	ld ($ff05),a		; TIMA - this parameter is basically optional, you can
				; use it to force the sync on played samples if you
				; want to get into it

	xor a
	ld ($ff1a),a		; turn sound 3 off
	ld ($ff1d),a		; set sound 3 low freq to 0
;	ld ($ff05),a		; TIMA - this parameter is basically optional, you can
				; use it to force the sync on played samples if you
				; want to get into it
;	dec a			; sound 3 length of 1/256 sec if a is $ff
	ld ($ff1b),a		; set sound 3 length to 256/256 sec if a is $00
				; (although only 1/256 should be all that is required)
				; by using a value > 1/256th, it carries past minor
				; delays in the timer irq triggering, however, make
				; sure you stop the sample by setting $ff1a to 0
				; since we are using continuous mode, this should not be
				; necessary anyways, but better safe than sorry

	ld a,($ffff)		; turn on timer interrupt
	or %00000100
	ld ($ffff),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; stop_sample
; call this from anywhere to stop a sample
; Destroys A
;---------------------------------------------------------------------------
;***************************************************************************

stop_sample:

	ld a,($ffff)		; turn off timer interrupt
	and %11111011
	ld ($ffff),a

	xor a
	ld (sample_priority),a	; reset priority
	ld ($ff1a),a		; turn off track 3
	inc a
	ld (mus_sample_finished),a
	
	ret

;***************************************************************************
;---------------------------------------------------------------------------
; tim_irq_fast
; this is the sample player, takes slightly less time to update (about 29-
; 31 cycles), but more time (12 cycles) when it counts for sample quality
;---------------------------------------------------------------------------
;***************************************************************************

tim_irq:

	push af			; 4 cycles	; save everything that will be modified
	push hl			; 4 cycles
	ld a,(current_rom_bank)	; 3 cycles
	ld (timer_rom_bank),a	; 3 cycles

	ld hl,sam_bank		; 3 cycles
	ld a,(hli)		; 2 cycles	; set the sample data bank
	ld (current_rom_bank),a	; 3 cycles
	ld ($2666),a		; 3 cycles	; ROMB0 Set lowbyte for $4000-$7fff
	
	ld a,(hli)		; 2 cycles	; get the current position
	ld h,(hl)		; 2 cycles
	ld l,a			; 1 cycle

	xor a			; 1 cycle
	ld ($ff1a),a		; 3 cycles	; shut off sound 3

				; 34 cycles for setup

				; load the next 16 bytes of sample data

	ld a,(hli)		; 2 cycles
	ld ($ff30),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff31),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff32),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff33),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff34),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff35),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff36),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff37),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff38),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff39),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3a),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3b),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3c),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3d),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3e),a		; 3 cycles
	ld a,(hli)		; 2 cycles
	ld ($ff3f),a		; 3 cycles

				; 80 cycles for copy

	ld a,%10000000		; 2 cycles
	ld ($ff1a),a		; 3 cycles	; turn on sound 3
	ld a,%10000111		; 2 cycles
	ld ($ff1e),a		; 3 cycles	; set initial and 256 hz

				; 10 cycles for restart

	; update the sample position
	
	ld a,l			; 1 cycle
	ld (sam_position),a	; 3 cycles
	ld a,h			; 1 cycle	; **** this is where we can check for $80 and increment
	ld (sam_position+1),a	; 3 cycles	;      the bank if necessary, right now we can fit 4
						;      seconds of samples per bank, which is plenty for
						;      our purposes
				; 11 cycles

				; **** if samples are allowed to span banks, max length
				;      will be $fdff when there are sprites on the screen
				;      or $ffff when there are not.  That works out to 
				;      roughly 253/255 seconds, and would fill $40 banks.

	ld a,(sam_end)		; 3 cycles	; check the sample length for end (lowbyte)
	cp l			; 1 cycle		
	jr nz,_done_sample	; 3/2 cycles	; if it is not zero, we do not stop sample
	ld a,(sam_end+1)	; 3 cycles	; check the sample length for end (hibyte)
	cp h			; 1 cycle
	jr nz,_done_sample	; 3/2 cycles	; if it is not zero, we do not stop sample

	; stop playback

	ld a,($ffff)		; 3 cycles	; turn off timer interrupt
	and %11111011		; 2 cycles
	ld ($ffff),a		; 3 cycles	

	xor a			; 2 cycles
	ld (sample_priority),a	; 3 cycles 	; reset priority
	ld ($ff1a),a		; 3 cycles	; turn off track 3 * we can remove these 2 lines
						; if we set the duration to 1/256seconds, but then
						; the 256hz pulse is more pronounced

	inc a
	ld (mus_sample_finished),a

				; 5,12 or 29 cycles

_done_sample:

	ld a,(timer_rom_bank)	; 3 cycles	; restore everything that we saved
	ld (current_rom_bank),a	; 3 cycles
	ld ($2666),a		; 4 cycles	; ROMB0 Set lowbyte for $4000-$7fff
	pop hl			; 3 cycles
	pop af			; 3 cycles

	reti			; 4 cycles

				; 20 cycles
				; ==========
				; 162,169,186 cycles

;***************************************************************************

	end

; if you want to use the other tim_irq, just swap them, nothing after the
; end command is compiled

;***************************************************************************
;---------------------------------------------------------------------------
; tim_irq_hq
; this is the high quality sample player
; uses slightly more time than the fast method (about 29-31 cycles)
; but less time (12 cycles) where it counts for sample quality
;---------------------------------------------------------------------------
;***************************************************************************

tim_irq:

	push af			; 4 cycles	; save everything that will be modified
	push hl			; 4 cycles	
	push bc			; 4 cycles	
	push de			; 4 cycles	
	ld a,(current_rom_bank)	; 3 cycles
	ld (timer_rom_bank),a	; 3 cycles

	ldhl sp,$00		; 3 cycles	; even the stack pointer
	ld a,l			; 1 cycle
	ld (stack_temp),a	; 3 cycles
	ld a,h			; 1 cycle
	ld (stack_temp+1),a	; 3 cycles

	ld hl,sam_bank		; 3 cycles	; set the sample data bank

	ld a,(hli)		; 2 cycles
	ld (current_rom_bank),a	; 3 cycles
	ld ($2666),a		; 4 cycles	; ROMB0 Set lowbyte for $4000-$7fff
	
	ld a,(hli)		; 2 cycles	; get the current position
	ld h,(hl)		; 2 cycles
	ld l,a			; 1 cycle

	ld sp,hl		; 2 cycles
	ld hl,$ff30		; 3 cycles
	ld bc,$871a		; 3 cycles

	xor a			; 1 cycle
	ld (c),a		; 2 cycles	; shut off sound 3

				; 61 cycles for setup

				; load the next 16 bytes of sample data

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld a,d			; 1 cycle
	ld (hli),a		; 2 cycles

	pop de			; 3 cycles
	ld a,e			; 1 cycle
	ld (hli),a		; 2 cycles
	ld (hl),d		; 2 cycles

				; 71 cycles for copy

	ld a,h			; 1 cycle 	; h is $ff
	ld (c),a		; 2 cycles	; turn on sound 3
	ld a,b			; 1 cycle
	ld ($ff1e),a		; 3 cycles	; set initial and 256 hz

				; 7 cycles for sample restart

	; update the sample position

	ldhl sp,$00		; 3 cycles
	
	ld a,l			; 1 cycle
	ld (sam_position),a	; 3 cycles
	ld a,h			; 1 cycle	; **** this is where we can check for $80 and increment
	ld (sam_position+1),a	; 3 cycles	;      the bank if necessary, right now we can fit 4
						;      seconds of samples per bank, which is plenty for
						;      our purposes
				; 11 cycles

				; **** if samples are allowed to span banks, max length
				;      will be $fdff when there are sprites on the screen
				;      or $ffff when there are not.  That works out to 
				;      roughly 253/255 seconds, and would fill $40 banks.

	ld a,(sam_end)		; 3 cycles	; check the sample length for end (lowbyte)
	cp l			; 1 cycle		
	jr nz,_done_sample	; 3/2 cycles	; if it is not zero, we do not stop sample
	ld a,(sam_end+1)	; 3 cycles	; check the sample length for end (hibyte)
	cp h			; 1 cycle
	jr nz,_done_sample	; 3/2 cycles	; if it is not zero, we do not stop sample

	; stop playback

	xor a			; 1 cycle
	ld (c),a		; 2 cycles	; turn off track 3 * we can remove these 2 lines
						; if we set the duration to 1/256seconds, but then
						; the 256hz pulse is more pronounced

	ld (sample_priority),a	; 3 cycles	; reset priority


	ld a,($ffff)		; 3 cycles	; turn off timer interrupt
	and %11111011		; 2 cycles
	ld ($ffff),a		; 3 cycles	

	ld a,1
	ld (mus_sample_finished),a

				; 5,12 or 27 cycles

_done_sample:

	ld a,(stack_temp)	; 3 cycles	; restore stack pointer
	ld l,a			; 1 cycle
	ld a,(stack_temp+1)	; 3 cycles
	ld h,a			; 1 cycle
	ld sp,hl		; 2 cycles

	ld a,(timer_rom_bank)	; 3 cycles	; restore everything that we saved
	ld (current_rom_bank),a	; 3 cycles
	ld ($2666),a		; 4 cycles	; ROMB0 Set lowbyte for $4000-$7fff
	pop de			; 3 cycles
	pop bc			; 3 cycles
	pop hl			; 3 cycles
	pop af			; 3 cycles

	reti			; 4 cycles

				; 36 cycles
				; ==========
				; 193,200 or 215 cycles

;***************************************************************************

	end

