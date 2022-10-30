
;---------------------------------------------------------------------------
; core graphics routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; gbc screen setup routines
;---------------------------------------------------------------------------
;***************************************************************************

screen_setup_gbc:

	call palette_ram_load
	call palette_setup_main

	xor a
	ld ($ff4f),a

	ld hl,$9800
	ld a,$20
_screen_loop:
	push af
	ld de,pattern_gbc_map
	ld bc,$0114
	ld a,mapbank00
	call scr_copy_to_vram
	pop af
	dec a
	jr nz,_screen_loop

	ld hl,$9c00
	ld de,main_window_gbc_map
	ld bc,$0614
	ld a,mapbank00
	call scr_copy_to_vram

	ld hl,$9d00
	ld a,$18
_menu_screen_loop:
	push af
	ld de,menu_screen_gbc_map
	ld bc,$0114
	ld a,mapbank00
	call scr_copy_to_vram
	pop af
	dec a
	jr nz,_menu_screen_loop

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; dmg screen setup routines
;---------------------------------------------------------------------------
;***************************************************************************

screen_setup_dmg:

	ld a,%11100100			;"normal" colours
	ld ($ff47),a			; dmg bg palette

	ld a,%10010011			;"normal" colours
	ld ($ff48),a			; dmg obj 1 palette
	ld ($ff49),a			; dmg obj 2 palette

	ld hl,$9800			; when all bugs are fixed
	ld a,$20
_screen_loop:
	push af
	ld de,pattern_dmg_map
	ld bc,$0114
	ld a,mapbank00
	call scr_copy_to_vram_dmg
	pop af
	dec a
	jr nz,_screen_loop

	ld hl,$9c00
	ld de,main_window_dmg_map
	ld bc,$0614
	ld a,mapbank00
	call scr_copy_to_vram_dmg

	ld hl,$9d00			; when all bugs are fixed
	ld a,$18
_menu_screen_loop:
	push af
	ld de,menu_screen_dmg_map
	ld bc,$0114
	ld a,mapbank00
	call scr_copy_to_vram_dmg
	pop af
	dec a
	jr nz,_menu_screen_loop

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; screen copy to vram dmg mode
; call with:
; bank in a
; height in b
; width in c
; source address in hl
; dest address in de
;---------------------------------------------------------------------------
;***************************************************************************

scr_copy_to_vram_dmg:

	ld (current_rom_bank),a
	ld ($2666),a			; ROMB0 Set lowbyte for $4000-$7fff

	jr dmg_scr_copy_loop
	
;***************************************************************************
;---------------------------------------------------------------------------
; screen copy to vram gbc mode
; call with:
; bank in a
; height in b
; width in c
; source address in hl
; dest address in de
;---------------------------------------------------------------------------
;***************************************************************************

scr_copy_to_vram:
	
	ld (current_rom_bank),a
	ld ($2666),a			; ROMB0 Set lowbyte for $4000-$7fff

	push bc
	push hl
	ld a,c
	ld (grfx_temp),a

	ld a,$01			; set bank 1
	ld ($ff4f),a

_scr_copy_loop_1:

	ld a,($ff41)
	and 2
	jr nz,_scr_copy_loop_1

	ld a,(de)			; copy 1 byte of attribute data
	ld (hl),a			; put it where it belongs

	ld a,($ff41)
	and 2
	jr nz,_scr_copy_loop_1

	inc hl
	inc de				; fine increment
	dec c				; check for done line
	jr nz,_scr_copy_loop_1		; loop

	ld a,(grfx_temp)
	ld c,a
	push bc
	ld a,$20
	sub c
	ld c,a
	ld b,0
	add hl,bc
	pop bc

	dec b				; check for done image
	jr nz,_scr_copy_loop_1		; loop
	
	xor a				; set bank 0
	ld ($ff4f),a

	pop hl
	pop bc

;---------------------------------------------------------------------------
dmg_scr_copy_loop:

	ld a,c
	ld (grfx_temp),a

_scr_copy_loop_2:

	ld a,($ff41)
	and 2
	jr nz,_scr_copy_loop_2

	ld a,(de)			; copy 1 byte of attribute data
	ld (hl),a			; put it where it belongs

	ld a,($ff41)
	and 2
	jr nz,_scr_copy_loop_2

	inc hl
	inc de				; fine increment
	dec c				; check for done line
	jr nz,_scr_copy_loop_2		; loop

	ld a,(grfx_temp)
	ld c,a
	push bc
	ld a,$20
	sub c
	ld c,a
	ld b,0
	add hl,bc
	pop bc

	dec b				; check for done image
	jr nz,_scr_copy_loop_2		; loop

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; copy hl de vram
;---------------------------------------------------------------------------
;***************************************************************************

copy_hl_de_vram:

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	inc e
	inc hl

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; clear hl vram
;---------------------------------------------------------------------------
;***************************************************************************

clear_hl_vram:

_z_0:
	ld a,($ff41)
	and b
	jr nz,_z_0

	ld a,$20
	ld (hl),a

	ld a,($ff41)
	and b
	jr nz,_z_0

	inc l

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; wipe vram
;---------------------------------------------------------------------------
;***************************************************************************

wipe_vram:

	ld b,2

_z_0:
	ld a,($ff41)
	and b
	jr nz,_z_0

	xor a
	ld (hl),a

	ld a,($ff41)
	and b
	jr nz,_z_0

	inc hl
	ld a,h
	cp d
	jr nz,_z_0
	ld a,l
	cp e
	jr nz,_z_0

	ret

;***************************************************************************
; cset load
;***************************************************************************

cset_load:

	xor a
	ld ($4666),a
	ld a,$0a
	ld ($1666),a

	ld a,(font_index_save_ram)
	cp >last_cset_tileset
	jr nc,_bad
	cp >normal_cset_tileset
	jr nc,_good
_bad:
	ld a,>normal_cset_tileset
_good:
	ld (font_index),a

	xor a
	ld ($1666),a

cset_draw:	

	ld a,(current_rom_bank)
	push af

	ld a,csetbank00
	ld (current_rom_bank),a
	ld ($2666),a

	ld a,(font_index)
	ld d,a

	ld hl,$8000
	ld e,l

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,(de)
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	inc hl
	inc de

	ld a,h
	cp $84
	jr nz,_z_0

	ld a,(font_index)
	ld d,a

_copy_2:

	ld a,(de)
	ld b,a
	inc de

_z_2:
	ld a,($ff41)
	and 2
	jr nz,_z_2

	ld a,(de)
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_z_2

	inc hl
	inc de

_z_3:
	ld a,($ff41)
	and 2
	jr nz,_z_3

	ld a,b
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_z_3

	inc hl

	ld a,h
	cp $88
	jr nz,_copy_2

	pop af
	ld (current_rom_bank),a
	ld ($2666),a

	ret

;***************************************************************************
; cset save
;***************************************************************************
 
cset_save:

	xor a
	ld ($4666),a
	ld a,$0a
	ld ($1666),a

	ld a,(font_index)
	ld (font_index_save_ram),a

	xor a
	ld ($1666),a
	ret

;***************************************************************************

	end

