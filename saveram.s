;***************************************************************************
;---------------------------------------------------------------------------
; save ram init routines
;---------------------------------------------------------------------------
;***************************************************************************

saveram_format_word_d		equ $6606	; what to use as our saveram
						; format word

max_save_ram_bank_save_ram	equ $a000	; $01 byte
saveram_format_word		equ $a001	; $02 bytes
saveram_checksum		equ $a003	; $02 bytes
colour_table_save_ram		equ $a005	; $1c bytes
font_index_save_ram		equ $a021	; $01 byte
plasma_save_ram			equ $a022 	; $03 bytes
num_save_ram_songs		equ $a025	; $01 byte
start_blank_addr_save_ram	equ $a026	; $02 bytes
start_blank_bank_save_ram	equ $a028	; $01 byte
song_save_header_table		equ $a040	; $30 bytes -> 2 bytes per entry currently 24 max songs DONUT CROSS $100!
song_save_bank_table		equ $a080	; $18 bytes -> 1 bytes per entry currently space for 32 DONUT CROSS $100!
song_save_time_table		equ $a088	; $60 bytes -> 4 bytes per entry
save_ram_first_addr		equ $a100	; area for songs (and samples?!)

init_save_ram:

	; enable save ram access

	ld a,$0a
	ld ($1666),a

	; set bank 0 - all header info is here

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; check for formatted ram
	; first check our test word

	ld a,(saveram_format_word)
	cp <saveram_format_word_d
	jr nz,_format
	ld a,(saveram_format_word+1)
	cp >saveram_format_word_d
	jr nz,_format

	; now check the checksum

	call saveram_checksum_calc

	ld a,e
	cp l
	jr nz,_format

	ld a,d
	cp h
	jr nz,_format

	; disable saveram access

	xor a
	ld ($1666),a

	ret

;***************************************************************************

_format:
	; disable saveram access

	xor a
	ld ($1666),a

	; do saveram screen

	ld a,%11100100			;"normal" colours
	ld ($ff47),a			; dmg bg palette

	xor a
	ld ($ff4f),a	

	ld a,tilebank00
	ld ($2666),a

	ld hl,format_warning_tileset
	ld de,$8000
_tileloop:
	ld a,(hli)
	ld (de),a
	inc de
	ld a,d
	cp $98
	jr nz,_tileloop

	ld a,palettebank00
	ld ($2666),a

	ld a,$80
	ld ($ff68),a
	
	ld hl,format_warning_palette
	ld c,$40
_palloop:
	ld a,(hli)
	ld ($ff69),a
	dec c
	jr nz,_palloop

	ld de,format_warning_gbc_map
	ld hl,$9c00
	ld bc,$1214
	ld a,mapbank00
	call scr_copy_to_vram

	xor a
	ld ($ff42),a
	ld ($ff43),a

	call wait_vbl
	ld a,%11011011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

_joy_loop:
	call joy_update
	ld a,(joy_pressed)
	and joy_start
	jr z,_joy_loop

	ld a,$0a
	ld ($1666),a

	; here is where we format the blank saveram

	; first find out what the max save ram bank availiable is to provide
	; support for all types of carts and emulators

	; do this by writing a non-zero byte to each page of saveram untill it is
	; mirrored in page 0

	xor a

	ld (current_sram_bank),a
	ld ($4666),a
	ld ($a000),a

	ld d,1

_write_loop:

	ld a,d
	
	ld (current_sram_bank),a
	ld ($4666),a
	ld ($a000),a

	xor a
	ld (current_sram_bank),a
	ld ($4666),a
	ld a,($a000)
	and a
	jr nz,_done_write_loop

	inc d
;	LD A,D	; max for dumbass no$ windows?
;	CP $10
	jr nz,_write_loop

_done_write_loop:

	; d is now max_save_ram_bank+1

	; now do the format, write 0 to everything

	xor a
	ld b,a

_format_loop2:

	; set our bank and address

	ld (current_sram_bank),a
	ld ($4666),a
	ld hl,$a000
	
_format_loop:

	; fill this bank with 0

	xor a
	ld (hli),a
	ld a,h
	cp $c0
	jr nz,_format_loop

	; check for next bank

	inc b
	ld a,b

	; loop until done

	cp d ; max_save_ram_bank+1
	jr c,_format_loop2

	;----------------------------------------------------------
	; now write our header

	; reset to bank 0

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	; save our max bank value

	dec d
	ld a,d
	ld (max_save_ram_bank_save_ram),a		; save max save ram bank

	; save our default palette

	ld hl,colour_table_save_ram
	ld bc,colour_table_presets
	ld e,28

_default_loop:

	ld a,(bc)
	ld (hli),a
	inc bc
	dec e
	jr nz,_default_loop

	; now the font

	ld a,>normal_cset_tileset
	ld (font_index_save_ram),a

	; now the plasma

;	xor a 	 ; graphics type
;	ld (plasma_save_ram),a
	ld a,$04 ; colour anim speed
	ld (plasma_save_ram+1),a
	ld a,$01 ; music on
	ld (plasma_save_ram+2),a

	; now the format word

	ld a,<saveram_format_word_d
	ld (saveram_format_word),a
	ld a,>saveram_format_word_d
	ld (saveram_format_word+1),a

	; now the start of blank ram

	ld a,<save_ram_first_addr
	ld (start_blank_addr_save_ram),a
	ld a,>save_ram_first_addr
	ld (start_blank_addr_save_ram+1),a

;	xor a
;	ld (start_blank_bank_save_ram),a

;***************************************************************************

calculate_save_ram_checksum:

	; enable saveram access

	ld a,$0a
	ld ($1666),a

	; calculate our checksum

	call saveram_checksum_calc

	; save our checksum	

	ld a,l
	ld (saveram_checksum),a
	ld a,h
	ld (saveram_checksum+1),a

	; disable saveram access

	xor a
	ld ($1666),a

	ret

;***************************************************************************

saveram_checksum_calc:	

	; clear variables

	xor a	; bank
	ld d,a	; for addition
	ld h,a	; total
	ld l,a

_loop2:
	; set bank

	ld (current_sram_bank),a
	ld ($4666),a

	; reset address

	ld bc,$a000
_loop:
	; add byte to total

	ld a,(bc)
	ld e,a
	add hl,de

	; loop until done this bank

	inc c
	jr nz,_loop
	inc b
	ld a,b
	cp $c0
	jr nz,_loop

	; chek to see if done last bank

	ld a,(current_sram_bank)
	ld b,a

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(max_save_ram_bank_save_ram)
	ld c,a
	inc c

	ld a,b
	inc a
	cp c ; max_save_ram_bank+1
	jr c,_loop2

	; now subtract current checksum

	; set bank

	xor a
	ld (current_sram_bank),a
	ld ($4666),a

	ld a,(saveram_checksum)
	ld e,a
	ld a,l
	sub e
	ld l,a
	ld a,h
	sbc a,0
	ld h,a

	ld a,(saveram_checksum+1)
	ld d,a
	ld a,l
	sub d
	ld l,a
	ld a,h
	sbc a,0
	ld h,a

	ret

;***************************************************************************

	end
