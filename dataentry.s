;---------------------------------------------------------------------------
; data entry menu routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; data_entry_menu_setup:
; data_entry_menu_cursor: wrapper
; data_entry_sel_copy_from_over: draws the text at the bottom of the window
; data_entry_redraw_data: redraws all data entries
; data_entry_draw_entry: redraws current data entry
;---------------------------------------------------------------------------
; de_draw_number_to_hl: checks to see if it is ok, and then draws it
; de_draw_b_to_hl: checks to see if it is ok, and then draws it
; copy_b_to_hl_vram: draws b to vram
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables
;
; data_entry_max_limit_table:
; data_entry_save_reset_data_table:
; data_entry_save_reset_sel_table:
;---------------------------------------------------------------------------
; text_off_on:
; text_up_dn:
; text_res:
; text_stop:
; text_p_u:
; text_o_l_r_c:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; data entry variables: 
;---------------------------------------------------------------------------

data_entry_sel_info_table	equ data_entry_ram+$00	; $02 bytes
data_entry_x_sel_num_table	equ data_entry_ram+$02	; $02 bytes
data_entry_sel_idx_table	equ data_entry_ram+$04	; $02 bytes
data_entry_data_type_table	equ data_entry_ram+$06	; $02 bytes
data_entry_work_byte_addr	equ data_entry_ram+$08	; $02 bytes

data_entry_x_menu_sel		equ data_entry_ram+$0e	; $01 bytes
data_entry_menu_cur_sel		equ data_entry_ram+$0f	; $01 bytes
data_entry_menu_mode		equ data_entry_ram+$10	; $01 bytes
data_entry_work_byte_type	equ data_entry_ram+$11	; $01 bytes
data_entry_work_byte_max	equ data_entry_ram+$12	; $01 bytes
data_entry_x_position		equ data_entry_ram+$13	; $01 bytes

data_entry_changed		equ data_entry_ram+$14	; $01 bytes
data_entry_data_mode		equ data_entry_ram+$15	; $01 bytes

data_entry_disp_flag_addr	equ data_entry_ram+$16	; $02 bytes
data_entry_disp_flag_bits	equ data_entry_ram+$18	; $02 bytes
data_entry_groups_table		equ data_entry_ram+$1a	; $02 bytes

data_entry_work_ram		equ data_entry_ram+$20	; $170 bytes (23 lines of 16 bytes)
data_entry_disp_bits		equ data_entry_ram+$2e	; it's interlaced with the above

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; data_entry_menu_setup: 
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_menu_setup:

	ld c,$93-8
	call setup_menu

	; stuff goes here

	xor a
	ld (data_entry_menu_mode),a
	ld (data_entry_changed),a

	jp data_entry_redraw_data

;***************************************************************************
;---------------------------------------------------------------------------
; data_entry_menu_cursor: cursor controls for data entry menus
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_menu_cursor:

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	jp data_entry_menu_cursor_over

;***************************************************************************
;---------------------------------------------------------------------------
; data_entry_sel_copy_from_over:
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_sel_copy_from_over:

	ld a,efxselbank00
	ld (current_rom_bank),a
	ld ($2666),a			; ROMB0 Set lowbyte for $4000-$7fff

	call dmg_scr_copy_loop

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a

	ret
	
;***************************************************************************
;---------------------------------------------------------------------------
; data_entry_redraw_data
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_redraw_data:

	; draw screen

	; loop through each entry and fill in the shit then call the draw routine
	; store menu_cur_sel & data_entry_x_menu_sel -> push to stack?

	ld de,$0000	; d = y, e = x

	; check loops first to avoid 0s

_loop_y:
	; loop y
	; use y->menu_num_sel

	ld a,(menu_num_sel)
	cp d
	jp z,_done_screen_draw

_loop_x:
	; loop x
	; use x->(data_entry_x_sel_num_table+y)

	ld b,0

	ld c,d

	ld a,(data_entry_x_sel_num_table)
	ld l,a
	ld a,(data_entry_x_sel_num_table+1)
	ld h,a
	add hl,bc ; data_entry_x_sel_num_table+y
	ld a,(hl) ; (data_entry_x_sel_num_table+y)
	cp e
	jp z,_done_line_draw

	; get data_entry_work_byte_type : ((data_entry_data_type_table+y*$02)+x)

	sla c
	rl b

	ld a,(data_entry_data_type_table)
	ld l,a
	ld a,(data_entry_data_type_table+1)
	ld h,a
	add hl,bc	; data_entry_data_type_table+y

	ld a,(hli)
	ld h,(hl)
	ld l,a
	ld b,0
	ld c,e
	add hl,bc	; (data_entry_data_type_table+y*$02)+x
	ld a,(hl)	; ((data_entry_data_type_table+y*$02)+x)
	
	ld (data_entry_work_byte_type),a

	; get data_entry_x_position : ((data_entry_sel_idx_table+y*$02)+x*$02)/8-2

	ld c,d
	sla c
	rl b

	ld a,(data_entry_sel_idx_table)
	ld l,a
	ld a,(data_entry_sel_idx_table+1)
	ld h,a

	add hl,bc	; data_entry_sel_idx_table+y*$02

	ld a,(hli)
	ld h,(hl)
	ld l,a		; (data_entry_sel_idx_table+y*$02)
	ld b,0
	ld c,e
	sla c
	add hl,bc	; (data_entry_sel_idx_table+y*$02)+x*$02

	ld a,(hl)	; ((data_entry_sel_idx_table+y*$02)+x*$02)
	srl a
	srl a
	srl a
	dec a
	dec a		; ((data_entry_sel_idx_table+y*$02)+x*$02)/8-2
	
	ld (data_entry_x_position),a

	; get data_entry_work_byte_addr : data_entry_work_ram+x+y*$10

	ld hl,data_entry_work_ram	; data_entry_work_ram

	ld c,d
	swap c
	ld a,c
	and $0f
	ld b,a
	ld a,c
	and $f0
	or e	; max 15		; x+y*$10
	ld c,a
	add hl,bc			; data_entry_work_ram+x+y*$10

	ld a,l
	ld (data_entry_work_byte_addr),a
	ld a,h
	ld (data_entry_work_byte_addr+1),a

	; now get our bit position

	ld a,c
	and $f0
	ld c,a

	ld hl,data_entry_disp_bits
	add hl,bc

	ld a,l
	ld (data_entry_disp_flag_addr),a
	ld a,h
	ld (data_entry_disp_flag_addr+1),a

	ld c,d
	sla c
	ld b,0

	ld a,(data_entry_groups_table)
	ld l,a
	ld a,(data_entry_groups_table+1)
	ld h,a

	add hl,bc

	ld a,(hli)
	ld h,(hl)
	ld l,a

	ld c,e
	sla c

	add hl,bc
	ld a,(hli)
	ld (data_entry_disp_flag_bits),a
	ld a,(hl)
	ld (data_entry_disp_flag_bits+1),a

	; call data_entry_draw_entry

	push de

	ld c,d
	call data_entry_draw_entry_2

	pop de

	; loop x

	inc e
	jp _loop_x

_done_line_draw:

	; loop y

	inc d
	ld e,0
	jp _loop_y

_done_screen_draw:

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; data_entry_draw_entry - draws a data entry to the screen
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_draw_entry:

	; calculate the onscreen position

	ld a,(menu_cur_sel)
	ld c,a

data_entry_draw_entry_2:

	;---------------------------------------------------------------------------
	; don't draw anything for buttons

	ld a,(data_entry_work_byte_type)
	cp data_entry_type_last_button+1
	ret c

	;---------------------------------------------------------------------------
	; calculate onscreen address

	ld b,0

	sla c
	rl b
	sla c
	rl b
	sla c
	rl b
	sla c
	rl b
	sla c
	rl b
	ld a,(data_entry_x_position)
	or c
	ld c,a

	ld hl,$9d01
	add hl,bc

	;---------------------------------------------------------------------------
	; check text

	ld a,(data_entry_work_byte_addr)
	ld c,a
	ld a,(data_entry_work_byte_addr+1)
	ld b,a

	ld a,(bc)
	ld c,a

	ld a,(data_entry_work_byte_type)
	cp data_entry_type_last_text+1
	jp nc,_not_text

	;---------------------------------------------------------------------------
	; draw text

	; off/on

	cp data_entry_type_off_on
	jr nz,_not_off_on

_draw_off:

	ld de,text_off_on
	jr _add_3_0_1

_not_off_on:

	; up/dn

	cp data_entry_type_up_dn
	jr nz,_not_up_dn

	ld de,text_up_dn

	ld a,c
	sla a

	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	jp _draw_2_text

_not_up_dn:

	; hol/res

	cp data_entry_type_res
	jr nz,_not_res

	ld de,text_res

_add_3_0_1:

	ld a,c
	sla a
	or c

	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	jr _draw_3_text	

_not_res:

	; p/u

	cp data_entry_type_p_u
	jr nz,_not_p_u

	ld de,text_p_u
	jr _add_1

	; o/l/r/c

_not_p_u:
	cp data_entry_type_o_l_r_c
	jr nz,_not_o_l_r_c

	ld de,text_o_l_r_c
_add_1:
	ld a,c

	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	jr _draw_1_text

_not_o_l_r_c:

	; note

	cp data_entry_type_note
	jr nz,_not_note

	ld de,note_names+3

;	ld a,c
;	sla a
;	ld a,d
;	adc a,0
;	ld d,0

	ld a,c
	sla a
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	ld a,c
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

	ld a,(de)
	or $40
	ld b,a
	call de_draw_b_to_hl
	inc hl
	inc de

	ld a,(de)
	or $40
	ld b,a
	call de_draw_b_to_hl
	inc hl
	inc de

	ld a,(de)
	or $40
	ld b,a
	jp de_draw_b_to_hl

_not_note:

	; text

	cp data_entry_type_text	
	jr nz,_not_text_type
	ld b,c
	jp de_draw_b_to_hl

_not_text_type:

	; stop/go

	cp data_entry_type_stop
;	jr nz,_not_stop
	ret nz

	ld de,text_stop

	ld a,c
	sla a
	sla a

	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a

_draw_4_text:

	ld a,(de)
	ld b,a
	call de_draw_b_to_hl
	inc hl
	inc de

_draw_3_text:

	ld a,(de)
	ld b,a
	call de_draw_b_to_hl
	inc hl
	inc de

_draw_2_text:

	ld a,(de)
	ld b,a
	call de_draw_b_to_hl
	inc hl
	inc de

_draw_1_text:

	ld a,(de)
	ld b,a
	jp de_draw_b_to_hl

;_not_stop_go:
;
;	ret

	;---------------------------------------------------------------------------
	; check bytes

_not_text:

	cp data_entry_type_last_byte+1
	jr nc,_not_byte

	;---------------------------------------------------------------------------
	; check for signed

	cp data_entry_type_signed_7f_80
	jr nz,_not_signed

	ld a,c
	sub $80
	ld c,a

	; check for negative

	ld b,107 ; +

	bit 7,c
	jr z,_positive
	ld a,c
	cpl
	inc a
	ld c,a

	ld b,109 ; -
_positive:

	call de_draw_b_to_hl
	inc hl
	jr _draw_byte

_not_signed:

	;---------------------------------------------------------------------------
	; draw byte

_draw_byte:

	ld a,c
	swap a

	call de_draw_number_to_hl

	inc hl

	ld a,c
	call de_draw_number_to_hl

	; check for off

	ld a,(data_entry_work_byte_type)
	cp data_entry_type_byte_off_ff
	jr z,_off
	cp data_entry_type_byte_off_7f
	ret nz

_off:

	ld a,c
	and a
	jr nz,_not_off_zero

	dec hl
	jp _draw_off

_not_off_zero:

	ld b,$60 ; space
	inc hl

	jp de_draw_b_to_hl

	;---------------------------------------------------------------------------
	; check nybbles

_not_byte:

	cp data_entry_type_last_nybble+1
;	jr nc,_not_byte
	ret nc

	;---------------------------------------------------------------------------
	; draw nybble

	cp data_entry_type_nybble_off_f
	jr z,_off_nybble
	cp data_entry_type_nybble_off_7
	jr nz,_not_off_nybble

_off_nybble:

	ld a,c
	and a
	jp z,_draw_off

	call de_draw_number_to_hl

	ld b,$60 ; space
	inc hl
	call de_draw_b_to_hl
	inc hl
	jp de_draw_b_to_hl

_not_off_nybble:
	
	ld a,c

;	jp de_draw_number_to_hl

;---------------------------------------------------------------------------
; de_draw_number_to_hl: checks to see if it is ok, and then draws it
;---------------------------------------------------------------------------

de_draw_number_to_hl:

	ld d,>numbers
	and $0f
	add a,<numbers
	ld e,a

	ld a,(de)
	or $40
	ld b,a

;---------------------------------------------------------------------------
; de_draw_b_to_hl: checks to see if it is ok, and then draws it
;---------------------------------------------------------------------------

de_draw_b_to_hl:

	ld a,(data_entry_data_mode)
	and a
	jr z,copy_b_to_hl_vram

	push de
	push hl

	ld a,(data_entry_disp_flag_addr)
	ld l,a
	ld a,(data_entry_disp_flag_addr+1)
	ld h,a

	ld a,(data_entry_disp_flag_bits)
	ld e,a
	ld a,(data_entry_disp_flag_bits+1)
	ld d,a

	ld a,(hli)
	and e
	ld e,a

	ld a,(hl)
	and d
	or e
	pop hl
	pop de

	jr nz,copy_b_to_hl_vram

	ld b,$1f

;---------------------------------------------------------------------------
; copy_b_to_hl_vram: draws b to vram
;---------------------------------------------------------------------------

copy_b_to_hl_vram:

	ld a,($ff41)
	and 2
	jr nz,copy_b_to_hl_vram

	ld (hl),b

	ld a,($ff41)
	and 2
	jr nz,copy_b_to_hl_vram

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; data entry lcdc irq
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_lcdc_irq:

	ld a,($ff44)
	cp $8d
	jp nc,c_wait_6
	cp $88
	jr nc,_wait_5
	cp $83-1
	jp nc,ef_wait_3q
	cp $35
	jr nc,_wait_3
	cp $33-1
	jr nc,_wait_2q
	cp $24
	jp nc,mc_wait_2
	cp $02
	jp c,c_wait_1
	jp logo_lcdc_irq
	
;---------------------------------------------------------------------------

_wait_5:
	ld a,$80+$38
	ld ($ff68),a

	ld a,(colour_table+$04)
	ld l,a
	ld a,(colour_table+$04+1)
	ld h,a

_wait_5a:		
	ld a,($ff44)
	cp $8b
	jr c,_wait_5a

	ld a,l
	ld ($ff69),a
	ld a,h
	ld ($ff69),a

	ld a,$b0
	ld ($ff42),a

	ld a,$90-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_2q:
	push bc

	ld a,(menu_raster_pos)
	ld c,a
	ld hl,cursor_colour_buffer
	ld b,8

	ld a,%10111110
	ld ($ff6a),a

_wait_2qa:
	ld a,($ff44)
	cp $33
	jr c,_wait_2qa

	ld a,(menu_ff42)
	ld ($ff42),a

	ld a,c
	cp $38
	jr c,_wait_3_go

	dec a
	ld ($ff45),a

	pop bc
	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_3:

	push bc

	ld a,(menu_raster_pos)
_wait_3_q:
	ld c,a

	ld hl,cursor_colour_buffer
	ld b,8

_wait_3d:
	ld a,%10111110
	ld ($ff6a),a

_wait_3q:
	ld a,($ff44)
	cp c
	jr c,_wait_3q

_wait_3_go:

	ld a,(hli)
	ld ($ff6b),a
	ld a,(hli)
	ld ($ff6b),a

	inc c
	dec b

	jr nz,_wait_3d
	
	ld a,c

	pop bc

	cp $8b-1-8
	jr nc,ef_wait_3q

	ld a,$8b-1-8
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

ef_wait_3q:
	ld a,$80+$38
	ld ($ff68),a

	ld a,(colour_table+$10)
	ld l,a
	ld a,(colour_table+$10+1)
	ld h,a

_ef_wait_3qa:
	ld a,($ff44)
	cp $83
	jr c,_ef_wait_3qa

	ld a,l
	ld ($ff69),a
	ld a,h
	ld ($ff69),a

	ld a,$6d+8
	ld ($ff42),a

	ld a,$8b-1
	ld ($ff45),a

	pop hl
	pop af
	reti




;***************************************************************************
;---------------------------------------------------------------------------
; data_entry vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_vbl_irq:

	ld a,(menu_cur_y_pos)
	ld ($fe00+(30*4)),a
	ld ($fe00+(31*4)),a
	ld ($fe00+(32*4)),a
	ld ($fe00+(33*4)),a
	ld ($fe00+(34*4)),a
	ld ($fe00+(35*4)),a
	ld ($fe00+(36*4)),a
	ld ($fe00+(37*4)),a
	ld ($fe00+(38*4)),a
	ld ($fe00+(39*4)),a

	jp menu_vbl_irq

;***************************************************************************
;---------------------------------------------------------------------------
; data entry lcdc irq dmg
;---------------------------------------------------------------------------
;***************************************************************************

;---------------------------------------------------------------------------
; main lcdc dmg controller

data_entry_lcdc_dmg:

	ld a,($ff44)
	cp $8d
	jp nc,d_wait_6
	cp $88
	jp nc,d_wait_5
	cp $83-1
	jr nc,ef_wait_3q
	cp $32-1
	jr nc,_wait_2q
	cp $24
	jp nc,md_wait_2
	cp $02
	jp c,d_wait_1
	pop hl
	pop af
	reti	

;---------------------------------------------------------------------------

_wait_2q:
	ld a,($ff44)
	cp $32
	jr c,_wait_2q

	ld a,%11010011
	ld ($ff47),a

_wait_02r:
	ld a,($ff44)
	cp $33
	jr c,_wait_02r

	ld a,(menu_ff42)
	ld ($ff42),a

	ld a,$83-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;***************************************************************************

;---------------------------------------------------------------------------
; data entry defines: 
;---------------------------------------------------------------------------

data_entry_type_null		equ $00
;data_entry_type_null_2		equ $01

data_entry_type_first_button	equ $02

data_entry_type_save_button	equ $02
data_entry_type_reset_button	equ $03
data_entry_type_user_button_0	equ $04
data_entry_type_user_button_1	equ $05
data_entry_type_user_button_2	equ $06
data_entry_type_user_button_3	equ $07
data_entry_type_user_button_4	equ $08
data_entry_type_user_button_5	equ $09
data_entry_type_user_button_6	equ $0a
data_entry_type_user_button_7	equ $0b
data_entry_type_user_button_8	equ $0c
data_entry_type_user_button_9	equ $0d
data_entry_type_user_button_a	equ $0e
data_entry_type_user_button_b 	equ $0f

data_entry_type_last_button	equ $0f
data_entry_type_first_text	equ $10

data_entry_type_off_on		equ $10
data_entry_type_up_dn		equ $11
data_entry_type_res		equ $12
data_entry_type_stop		equ $13
data_entry_type_p_u		equ $14
data_entry_type_o_l_r_c		equ $15
data_entry_type_note		equ $16
data_entry_type_text		equ $17
;data_entry_type_unused		equ $18
;data_entry_type_unused		equ $19
;data_entry_type_unused		equ $1a
;data_entry_type_unused		equ $1b
;data_entry_type_unused		equ $1c
;data_entry_type_unused		equ $1d
;data_entry_type_unused		equ $1e
;data_entry_type_unused		equ $1f

data_entry_type_last_text	equ $1f
data_entry_type_first_byte	equ $20

data_entry_type_byte_off_ff	equ $20
data_entry_type_byte_off_7f	equ $21
data_entry_type_byte_00_ff	equ $22
data_entry_type_byte_00_1f	equ $23
data_entry_type_byte_00_3f	equ $24
data_entry_type_signed_7f_80	equ $25
;data_entry_type_unused		equ $26
;data_entry_type_unused		equ $27
;data_entry_type_unused		equ $28
;data_entry_type_unused		equ $29
;data_entry_type_unused		equ $2a
;data_entry_type_unused		equ $2b
;data_entry_type_unused		equ $2c
;data_entry_type_unused		equ $2d
;data_entry_type_unused		equ $2e
;data_entry_type_unused		equ $2f

data_entry_type_last_byte	equ $2f
data_entry_type_first_nybble	equ $30

data_entry_type_nybble_0_f	equ $30
data_entry_type_nybble_0_3	equ $31
data_entry_type_nybble_0_1	equ $32
data_entry_type_nybble_0_7	equ $33
data_entry_type_nybble_off_7	equ $34
data_entry_type_nybble_off_f	equ $35

data_entry_type_last_nybble	equ $3f

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

data_entry_max_limit_table:
	db $00 ; data_entry_type_null		equ $00
	db $00 ; ;data_entry_type_null_2	equ $01
	db $00 ; data_entry_type_save_button	equ $02
	db $00 ; data_entry_type_reset_button	equ $03
	db $00 ; data_entry_type_user_button_0	equ $04
	db $00 ; data_entry_type_user_button_1	equ $05
	db $00 ; data_entry_type_user_button_2	equ $06
	db $00 ; data_entry_type_user_button_3	equ $07
	db $00 ; data_entry_type_user_button_4	equ $08
	db $00 ; data_entry_type_user_button_5	equ $09
	db $00 ; data_entry_type_user_button_6	equ $0a
	db $00 ; data_entry_type_user_button_7	equ $0b
	db $00 ; data_entry_type_user_button_8	equ $0c
	db $00 ; data_entry_type_user_button_9	equ $0d
	db $00 ; data_entry_type_user_button_a	equ $0e
	db $00 ; data_entry_type_user_button_b 	equ $0f

	db $01 ; data_entry_type_off_on		equ $10
	db $01 ; data_entry_type_up_dn		equ $11
	db $00 ; data_entry_type_res		equ $12
	db $00 ; data_entry_type_stop		equ $13
	db $00 ;$01 ; data_entry_type_p_u		equ $14
	db $03 ; data_entry_type_o_l_r_c	equ $15
	db $7f ; data_entry_type_note		equ $16
	db $7f ; ;data_entry_type_text		equ $17
	db $00 ; ;data_entry_type_unused	equ $18
	db $00 ; ;data_entry_type_unused	equ $19
	db $00 ; ;data_entry_type_unused	equ $1a
	db $00 ; ;data_entry_type_unused	equ $1b
	db $00 ; ;data_entry_type_unused	equ $1c
	db $00 ; ;data_entry_type_unused	equ $1d
	db $00 ; ;data_entry_type_unused	equ $1e
	db $00 ; ;data_entry_type_unused	equ $1f

	db $ff ; data_entry_type_byte_off_ff	equ $20
	db $7f ; data_entry_type_byte_off_7f	equ $21
	db $ff ; data_entry_type_byte_00_ff	equ $22
	db $1f ; data_entry_type_byte_00_1f	equ $23
	db $3f ; data_entry_type_byte_00_3f	equ $24
	db $ff ; data_entry_type_signed_7f_80	equ $25
	db $00 ; ;data_entry_type_unused	equ $26
	db $00 ; ;data_entry_type_unused	equ $27
	db $00 ; ;data_entry_type_unused	equ $28
	db $00 ; ;data_entry_type_unused	equ $29
	db $00 ; ;data_entry_type_unused	equ $2a
	db $00 ; ;data_entry_type_unused	equ $2b
	db $00 ; ;data_entry_type_unused	equ $2c
	db $00 ; ;data_entry_type_unused	equ $2d
	db $00 ; ;data_entry_type_unused	equ $2e
	db $00 ; ;data_entry_type_unused	equ $2f

	db $0f ; data_entry_type_nybble_0_f	equ $30
	db $03 ; data_entry_type_nybble_0_3	equ $31
	db $01 ; data_entry_type_nybble_0_1	equ $32
	db $07 ; data_entry_type_nybble_0_7	equ $33
	db $07 ; data_entry_type_nybble_off_7	equ $34
	db $0f ; data_entry_type_nybble_off_f	equ $35

;***************************************************************************

data_entry_save_reset_data_table:
	db data_entry_type_save_button
	db data_entry_type_reset_button

data_entry_save_reset_sel_table:
	db (1*8)+8,4  ; save
	db (6*8)+8,5  ; reset

;***************************************************************************

text_off_on:
	db 79,70,70 ; "off"
	db 79,78,96 ; "on "

text_up_dn:
	db 68,78 ; "dn"
	db 85,80 ; "up"

text_res:
	db 82,69,83 ; "res"

text_stop:
	db 83,84,79,80 ; "stop"

text_p_u:
	db 80 ; "p"
;	db 85 ; "u"

text_o_l_r_c:
	db 79 ; "o"
	db 82 ; "r"
	db 76 ; "l"
	db 67 ; "c"

	end

