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

	ld (mus_song_number),a

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	jp mus_hard_reset

;***************************************************************************

music_reset:

	ld (mus_song_number),a

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	jp mus_reset

;***************************************************************************
;***************************************************************************

music_player:

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	jp mus_player

;***************************************************************************

music_read_parameter_hl:

	ld a,(mus_song_bank)
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,(hli)	
	ld b,a
;	push af

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,b
;	pop af

	ret

;***************************************************************************

music_read_parameter_de:

	ld a,(mus_song_bank)
music_read_paramater_de_bank:
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,(de)
	ld b,a
;	push af

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a		; ROMB0 Set lowbyte for $4000-$7fff

	ld a,b
;	pop af

	inc de

	ret

;***************************************************************************

music_read_notetable_de:

	ld a,musicbank01
	jr music_read_paramater_de_bank

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

	jp mus_player_fast

;***************************************************************************

