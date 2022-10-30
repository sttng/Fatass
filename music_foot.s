;---------------------------------------------------------------------------
; music player bank 0 routines and defines!
;
; music_hard_reset: initial setup - will trash sound 3
; music_reset: setup or reset the player & sound hardware
;	- call resets with song number to play in A
; music_player: call once (or more for doublespeed+) per vbl to play song
;---------------------------------------------------------------------------

;***************************************************************************
; routines

;---------------------------------------------------------------------------
; setup/reset the music player & sound hardware
;---------------------------------------------------------------------------


;***************************************************************************

music_hard_reset:

;	ld (mus_song_number),a 	;##### required for standalone

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

;	jp mus_hard_reset	;##### required for standalone
	call mus_hard_reset	;@@@@@ required for editor mode

	ld a,(current_wram_bank)	;@@@@@ required for editor mode
	ld ($ff70),a		;@@@@@ required for editor mode

	ret			;@@@@@ required for editor mode

;***************************************************************************

music_reset:

;	ld (mus_song_number),a	;##### required for standalone

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

;	jp mus_reset		;##### required for standalone
	call mus_reset		;@@@@@ required for editor mode

	ld a,(current_wram_bank)	;@@@@@ required for editor mode
	ld ($ff70),a		;@@@@@ required for editor mode

	ret			;@@@@@ required for editor mode

;***************************************************************************
;***************************************************************************

music_player:

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

;	jp mus_player		;##### required for standalone
	call mus_player		;@@@@@ required for editor mode

	ld a,(current_wram_bank)	;@@@@@ required for editor mode
	ld ($ff70),a		;@@@@@ required for editor mode

	ret			;@@@@@ required for editor mode

;***************************************************************************

music_read_parameter_hl:

;	ld a,(mus_song_bank)	;##### required for standalone
;	ld (current_rom_bank),a	;##### required for standalone
;	ld ($2666),a		;##### required for standalone ; ROMB0 Set lowbyte for $4000-$7fff
;
;	ld a,(hli)		;##### required for standalone
;	ld b,a			;##### required for standalone
;
;	ld a,musicbank00	;##### required for standalone
;	ld (current_rom_bank),a	;##### required for standalone
;	ld ($2666),a		;##### required for standalone ; ROMB0 Set lowbyte for $4000-$7fff
;
;	ld a,b			;##### required for standalone
;
;	ret			;##### required for standalone

	;;; does not have to be in bank 0 for editor mode

	push hl			;@@@@@ required for editor mode
	ld b,music_buffer_start_bank			;@@@@@ required for editor mode
	ld a,h			;@@@@@ required for editor mode
_bank_loop:			;@@@@@ required for editor mode
	and $f0			;@@@@@ required for editor mode
	cp $d0			;@@@@@ required for editor mode
	jr z,_got_bank		;@@@@@ required for editor mode

	inc b			;@@@@@ required for editor mode
	ld a,h			;@@@@@ required for editor mode
	sub $10			;@@@@@ required for editor mode
	ld h,a			;@@@@@ required for editor mode
	jr _bank_loop		;@@@@@ required for editor mode
	
_got_bank:			;@@@@@ required for editor mode

	ld a,b			;@@@@@ required for editor mode
	ld ($ff70),a		;@@@@@ required for editor mode

	ld a,(hl)		;@@@@@ required for editor mode
	ld b,a			;@@@@@ required for editor mode

	pop hl			;@@@@@ required for editor mode
	inc hl			;@@@@@ required for editor mode

	ret			;@@@@@ required for editor mode

;***************************************************************************

music_read_parameter_de:

;	ld a,(mus_song_bank)	;##### required for standalone
;music_read_paramater_de_bank:	;##### required for standalone
;	ld (current_rom_bank),a	;##### required for standalone
;	ld ($2666),a		;##### required for standalone	; ROMB0 Set lowbyte for $4000-$7fff
;
;	ld a,(de)		;##### required for standalone
;	ld b,a			;##### required for standalone
;
;	ld a,musicbank00	;##### required for standalone
;	ld (current_rom_bank),a	;##### required for standalone
;	ld ($2666),a		;##### required for standalone	; ROMB0 Set lowbyte for $4000-$7fff
;
;	ld a,b			;##### required for standalone
;
;	inc de			;##### required for standalone
;
;	ret			;##### required for standalone

	;;; does not have to be in bank 0 for editor mode

	push de			;@@@@@ required for editor mode
	ld b,music_buffer_start_bank		;@@@@@ required for editor mode
	ld a,d			;@@@@@ required for editor mode
_bank_loop:			;@@@@@ required for editor mode
	and $f0			;@@@@@ required for editor mode
	cp $d0			;@@@@@ required for editor mode
	jr z,_got_bank		;@@@@@ required for editor mode

	inc b			;@@@@@ required for editor mode
	ld a,d			;@@@@@ required for editor mode
	sub $10			;@@@@@ required for editor mode
	ld d,a			;@@@@@ required for editor mode
	jr _bank_loop		;@@@@@ required for editor mode
	
_got_bank:			;@@@@@ required for editor mode

	ld a,b			;@@@@@ required for editor mode
	ld ($ff70),a		;@@@@@ required for editor mode

	ld a,(de)		;@@@@@ required for editor mode
	ld b,a			;@@@@@ required for editor mode

	pop de			;@@@@@ required for editor mode
	inc de			;@@@@@ required for editor mode

	ret			;@@@@@ required for editor mode

;***************************************************************************

music_read_notetable_de:

	ld a,musicbank01
;	jr music_read_paramater_de_bank	;##### required for standalone
	ld (current_rom_bank),a	;@@@@@ required for editor mode
	ld ($2666),a		;@@@@@ required for editor mode; ROMB0 Set lowbyte for $4000-$7fff

	ld a,(de)		;@@@@@ required for editor mode
	ld b,a			;@@@@@ required for editor mode

	ld a,musicbank00	;@@@@@ required for editor mode
	ld (current_rom_bank),a	;@@@@@ required for editor mode
	ld ($2666),a		;@@@@@ required for editor mode	; ROMB0 Set lowbyte for $4000-$7fff

	ld a,b			;@@@@@ required for editor mode

	inc de			;@@@@@ required for editor mode

	ret			;@@@@@ required for editor mode

;***************************************************************************

music_play_sample:
	call play_sample

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff
	ret	

;***************************************************************************
	
	end

music_player_fast:

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	call mus_player_fast

	ret

;***************************************************************************

