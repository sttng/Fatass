file_access_temp	equ file_access_ram+$00	; $03 bytes
file_access_size_temp	equ file_access_ram+$03	; $02 bytes
file_access_sram_temp	equ file_access_ram+$05	; $03 bytes
file_access_num_indexes	equ file_access_ram+$08	; $02 bytes
file_access_num_macros	equ file_access_ram+$0a	; $02 bytes
file_access_next_index  equ file_access_ram+$0c	; $02 bytes
file_access_addr_check	equ file_access_ram+$0e	; $03 bytes
file_access_temp_addr	equ file_access_ram+$11	; $02 bytes
file_access_save	equ file_access_ram+$13	; $01 bytes

;;; change the jr dones to ret once I know no more code is necessary

; DOES NOT CHECK FOR SONG TOO BIG TO LOAD!!!! -> should not be a problem

;;; clean up this mess a little

;***************************************************************************
;---------------------------------------------------------------------------
; file access routines
;---------------------------------------------------------------------------
;***************************************************************************

;***************************************************************************
; load_song_into_ram_from_rom: loads a song from rom, call with index in a
; DOES NOT CHECK FOR SONG TOO BIG TO LOAD!!!!
; DOES NOT LOAD UNDOS AS ROM SONGS SHOULDN'T HAVE ANY!
;***************************************************************************

load_song_into_ram_from_rom:

	; DOES NOT CHECK FOR SONG TOO BIG TO LOAD!!!!

	;---------------------------------------------------------------------------
	; get information about the song

	call get_rom_song_address_hl_bc_e

	;---------------------------------------------------------------------------
	; should test to see if song is too big to load here!!!!!!!!!!
	;---------------------------------------------------------------------------

	; set our destination bank
	
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	;---------------------------------------------------------------------------
	; copy song to ram

	ld de,$d000 ; dest addr

_copy_loop:
		
	ld a,(hli)
	ld (de),a
	inc de

	; check for bankover in our source

	call check_hl_rom_read

	; check for bankover in our dest

	call check_de_wram_read

	; count down to finish

	dec bc
	ld a,c
	and a
	jr nz,_copy_loop

	ld a,b	
	and a
	jr nz,_copy_loop

	xor a
	ld (editor_num_undos),a
	ld (editor_num_redos),a

	ld (song_time+0),a
	ld (song_time+1),a
	ld (song_time+2),a
	ld (song_time+3),a
	ld (song_time+4),a

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

init_copy_buffer:

	; clear last edit track

	xor a
	ld (editor_seq),a
	ld (editor_line),a
	ld (editor_track),a
	ld (editor_copy_from),a
	ld (editor_copy_from+1),a

	dec a
	ld (editor_copy_patt),a
	ld (editor_copy_patt+1),a

	ld (last_edit_track),a
	ld (last_edit_patt),a

	ret

;***************************************************************************
; load_song_into_ram_from_sram: loads a song from sram, call with index in a
; DOES NOT CHECK FOR SONG TOO BIG TO LOAD!!!!
;***************************************************************************

load_song_into_ram_from_sram:

	push af

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	pop af

	call load_song_into_ram_from_sram_ov

	jr init_copy_buffer

;***************************************************************************
; save_song_into_sram_from_ram: saves a song to sram, call with index in a
; DOES NOT CHECK FOR SONG TOO BIG TO SAVE!!!!
;***************************************************************************

save_song_into_sram_from_ram:

	push af

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	pop af

	call save_song_into_sram_from_ram_ov

	ret

;***************************************************************************
; delete_song_from_sram: deletes a song from sram, call with index in a
;***************************************************************************

delete_song_from_sram:

	push af

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	pop af

	call delete_song_from_sram_ov

	ret

;***************************************************************************
; get_rom_song_address_hl_bc_e: gets addr in hl, len in bc, bank in a,
; call with index in a
;***************************************************************************

get_rom_song_address_hl_bc_e:

	; song number to load is in A
	; store it is an index in BC

	ld c,a
	ld b,0

	; set our header bank

	ld a,musicbank00
	ld (current_rom_bank),a
	ld ($2666),a

	; get the song start bank into E

	ld hl,mus_song_bank_table
	add hl,bc

	ld e,(hl)

	; convert BC into word index

	sla c
;	rl b

	; get the song start address into HL

	ld hl,mus_song_address_table
	add hl,bc
	ld a,(hli)
	ld h,(hl)
	ld l,a

	; set our source bank

	ld a,e
	ld (current_rom_bank),a
	ld ($2666),a
	
	; set our destination bank
	
	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	;---------------------------------------------------------------------------
	; get our song length into bc

	push hl

	ld bc,$0004
	add hl,bc

	call check_hl_rom_read
	ld a,(hli)
	ld c,a
	call check_hl_rom_read
	ld b,(hl)

	pop hl

	; since we have no undos enabled for rom, that's it	

	;---------------------------------------------------------------------------

	ret

;***************************************************************************
; get_sram_song_address_hl_bc_e: gets addr in hl, len in bc )with undoes(, bank in a,
; and number of undoes in d for good measure
; call with index in a
;***************************************************************************

get_sram_song_address_hl_bc_e_d:

	push af

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	pop af

	jp get_sram_song_address_hl_bc_ed_o

;***************************************************************************
; get_sram_song_address_hl_bc_e: gets addr in hl, len in bc, bank in a,
; call with index in a
;***************************************************************************

get_sram_song_address_hl_bc_e:

	push af

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	pop af

	jp get_sram_song_address_hl_bc_e_ov

;***************************************************************************
; check_hl_rom_read: checks to make sure hl is a valid rom read address
;***************************************************************************

check_hl_rom_read:

	ld a,h
	cp $80
	ret nz
	
	ld a,(current_rom_bank)
	inc a
	ld (current_rom_bank),a
	ld ($2666),a

	ret

;***************************************************************************
; check_hl_sram_read:
;***************************************************************************

check_hl_sram_read:

	ld a,h
	cp $c0
	ret nz

	ld h,$a0
	ld a,(current_sram_bank)
	inc a
	ld (current_sram_bank),a
	ld ($4666),a

	ret
	
;***************************************************************************
; check_de_wram_read:
;***************************************************************************

check_de_wram_read:

	ld a,d
	cp $e0
	ret nz

	ld d,$d0
	ld a,(current_wram_bank)
	inc a
	ld (current_wram_bank),a
	ld ($ff70),a

	ret

;***************************************************************************
; check_hl_wram_read:
;***************************************************************************

check_hl_wram_read:

	ld a,h
	cp $e0
	ret nz

	ld h,$d0
	ld a,(current_wram_bank)
	inc a
	ld (current_wram_bank),a
	ld ($ff70),a

	ret

;***************************************************************************
; addr_sub_dhl_bc: subtracts bc from address dhl and corrects it to be proper
;		saveram addressing
;***************************************************************************

addr_sub_dhl_bc:

	ld a,l
	sub c
	ld l,a
	ld a,h
	sbc a,b
	ld h,a

_addr_adj_loop:
	and $e0
	cp $a0
	ret z

	ld a,h
	add a,$20
	ld h,a
	dec d
	jr _addr_adj_loop







;***************************************************************************
;---------------------------------------------------------------------------
; draw_sram_file_list:
;---------------------------------------------------------------------------
;***************************************************************************

draw_sram_file_list:

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	; load filenames to screen

	ld bc,$0000
	ld de,$9d01

	; get number of songs

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; check for 0

	ld a,(num_save_ram_songs)
	and a
	jr z,_done

	ld (file_access_temp),a

_get_name:

	; store important stuff for later

	push bc
	push de

	; set our header bank

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; get the song start bank into E

	ld hl,song_save_bank_table
	add hl,bc

	ld e,(hl)

	; convert BC into word index

	sla c
;	rl b

	; get the song start address into HL

	ld hl,song_save_header_table
	add hl,bc
	ld a,(hli)
	ld h,(hl)
	ld l,a

	; add the song name offset

	ld bc,$0007
	add hl,bc

	; set our source bank

	ld a,e
	ld (current_sram_bank),a
	ld ($4666),a

	; now copy

	pop de

_copy_name:

	call check_hl_sram_read
	ld a,(hl)
	cp $ff
	jr z,_done_this_one

	call copy_hl_de_vram
	inc b
	ld a,b
	cp 18
	jr nz,_copy_name
	
_done_this_one:

	ld a,$20
	sub b
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	pop bc
	inc c
	ld a,c
	cp 24
	jr z,_done

	; get number of songs

	ld a,(file_access_temp)
	cp c
	jr nz,_get_name

_done:

	; disable save ram access

	xor a
	ld ($1666),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; get_num_save_ram_songs_into_e:
;---------------------------------------------------------------------------
;***************************************************************************

get_num_save_ram_songs_into_e:

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	; get number of songs

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(num_save_ram_songs)
	ld e,a

	; disable save ram access

	xor a
	ld ($1666),a

	ret


;***************************************************************************

;***************************************************************************
; optimize_song_in_wram: optimizes the current song in wram
;***************************************************************************

optimize_song_in_wram:

	; set up display

	ld hl,$9c00
	ld bc,$0614
	ld de,main_info_window_gbc_map
	ld a,mapbank00
	call scr_copy_to_vram

	ld hl,$9c27
	ld bc,$040c
	ld de,optimize_info_inset
	ld a,insetbank00
	call scr_copy_to_vram_dmg

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	call optimize_song_in_wram_over
	call init_copy_buffer
	jp redraw_song_info

;***************************************************************************

update_optimize_disp:

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d004)
	ld c,a
	ld a,($d005)
	ld b,a

	inc bc

	ld de,$9c6f
	ld h,>numbers

	call write_number_bc_to_vram

	ld a,(file_access_sram_temp+0)
	sub c
	ld c,a
	ld a,(file_access_sram_temp+1)
	sbc a,b
	ld b,a

	ld e,$8f
	ld h,>numbers

	jp write_number_bc_to_vram
	
;***************************************************************************
	
move_wram_bhl_cde:

	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(de)
	inc de
	ld (file_access_temp),a

	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(file_access_temp)
	ld (hli),a

	; check source carry

	ld a,d
	cp $e0
	jr nz,_no_source_carry

	ld d,$d0
	inc c

_no_source_carry:

	; check dest carry

	ld a,h
	cp $e0
	jr nz,move_wram_bhl_cde

	ld h,$d0
	inc b

	ld a,b
	cp music_buffer_end_bank+1
	jr nz,move_wram_bhl_cde

	ret

;***************************************************************************

convert_hl_index_to_bhl_address:

	ld a,h
	and $f0
	swap a
	add a,music_buffer_start_bank
	ld b,a

	ld a,h
	and $0f
	or $d0
	ld h,a

	ret

convert_de_index_to_cde_address:

	ld a,d
	and $f0
	swap a
	add a,music_buffer_start_bank
	ld c,a

	ld a,d
	and $0f
	or $d0
	ld d,a

	ret

;***************************************************************************

check_bhl_wram_read:
	ld a,h
	cp $e0
	jr nz,_no_carry
	ld h,$d0
	inc b
_no_carry:
	ld a,b
	ld (current_wram_bank),a
	ld ($ff70),a
	ld a,(hli)
	ret
	
check_cde_wram_write:
	push af
	ld a,d
	cp $e0
	jr nz,_no_carry
	ld d,$d0
	inc c
_no_carry:
	ld a,c
	ld (current_wram_bank),a
	ld ($ff70),a
	pop af
	ld (de),a
	inc de
	ret


check_cde_wram_incr:
	inc de
	ld a,d
	cp $e0
	ret nz
	ld d,$d0
	inc c
	ret

new_song_text:
	db "-NEW-"
new_song_text_len equ 5

done_optimize_text:
	db "DONE-PRESS A"
done_optimize_text_len equ 12
