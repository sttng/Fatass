;---------------------------------------------------------------------------
; data entry menu routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; data_entry_menu_cursor_over:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; routines
;---------------------------------------------------------------------------

;***************************************************************************
;---------------------------------------------------------------------------
; data_entry_menu_cursor: cursor controls for data entry menus
;---------------------------------------------------------------------------
;***************************************************************************

data_entry_menu_cursor_over:

	; check mode

	ld a,(data_entry_menu_mode)
	and a
	jp z,_select_entry_mode

	; make sure it is edit data mode because it could be wait for button mode, etc.

	cp 1
	ret nz

	; edit data

	call update_waveforms
	call joy_repeat_update

	ld a,(joy_held)
	bit joy_bit_a,a
	jp z,_done_altering

	; check for on/off mode

	ld a,(data_entry_data_mode)
	and a
	jr z,_not_fudup_mode

	; we are in on/off mode

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
	ld c,a

	ld a,(hld)
	and d
	or c
	jr nz,_check_for_turn_off

	; check for turn on
	; it is off, we need to see if we should turn it on

	ld a,(joy_held_repeat)
	and joy_up+joy_right
	ret z

	; turn it on

	ld a,(hl)
	or e
	ld (hli),a

	ld a,(hl)
	or d
	ld (hl),a

	jr _store_it_2

_check_for_turn_off:

	; it is on, we need to see if we should turn it off

	ld a,(joy_held_repeat)
	and joy_down+joy_left
	jr z,_not_fudup_mode

	push hl

	ld a,(data_entry_work_byte_addr)
	ld l,a
	ld a,(data_entry_work_byte_addr+1)
	ld h,a

	ld a,(hl)
	pop hl
	and a
	jr nz,_not_fudup_mode

	; we should turn it off

	ld a,e
	cpl
	ld e,a

	ld a,d
	cpl
	ld d,a

	ld a,(hl)
	and e
	ld (hli),a

	ld a,(hl)
	and d
	ld (hl),a

_store_it_2:


	xor a
	ld (data_entry_menu_mode),a

	inc a
	ld (data_entry_changed),a

	jp data_entry_redraw_data

_not_fudup_mode:

	ld a,(data_entry_work_byte_addr)
	ld l,a
	ld a,(data_entry_work_byte_addr+1)
	ld h,a

	ld e,(hl)

	ld a,(joy_held_repeat)
	and joy_up+joy_right
	jr z,_not_increase

	bit joy_bit_up,a
	jr z,_not_big_incr

	ld a,e
	add a,$10
	jr nc,_not_big_incr_carry
	ld a,$ff
_not_big_incr_carry:
	ld e,a
	ld a,(data_entry_work_byte_max)
	cp e
	jr nc,_store_it
	ld e,a
	
	jr _store_it

_not_big_incr:

	inc e
	ld a,(data_entry_work_byte_max)
	inc a
	cp e
	jr nz,_store_it
	dec a
	ld e,a
	
	jr _store_it

_not_increase:

	ld a,(joy_held_repeat)
	and joy_down+joy_left
	ret z

	bit joy_bit_down,a
	jr z,_not_big_decr
	
	ld a,e
	sub $10
	jr nc,_not_big_decr_carry
	xor a
_not_big_decr_carry:
	ld e,a
	
	jr _store_it

_not_big_decr:

	dec e
	ld a,e
	cp $ff
	jr nz,_store_it

	ld e,0

_store_it:

	ld (hl),e

	ld a,1
	ld (data_entry_changed),a

	; draw update
	jp data_entry_draw_entry

_done_altering:

	xor a
	ld (data_entry_menu_mode),a

	ret

	;---------------------------------------------------------------------------

_select_entry_mode:

	; select entry

	call menu_poop_loop	; get our y position taken care of

	;---------------------------------------------------------------------------
	; set up our indexes

	ld d,0

	;---------------------------------------------------------------------------
	; check number of selections, ensure valid

	call _check_num_selections

	;---------------------------------------------------------------------------
	; draw text in lower area

	; calculate indexes

	ld e,a
	sla c
	push bc

	ld a,(data_entry_sel_info_table)
	ld l,a
	ld a,(data_entry_sel_info_table+1)
	ld h,a
	add hl,bc

	; hl now points to the pointer to the line
	ld a,(hli)
	ld b,(hl)
	ld c,a
	; bc is now the address of the line
	; multiply x coord by 18 to get index of wanted text
	ld a,e
	sla e
	push de
	swap a
	ld d,a
	and $0f
	ld h,a
	ld a,d
	and $f0
	add a,e
	ld l,a
	ld a,h
	adc a,0
	ld h,a
	; hl is now index of wanted part
	add hl,bc
	; now hl is address of the data we want
	ld e,l
	ld d,h
	; now de is the address of the data we want

	;---------------------------------------------------------------------------
	; copy to screen

	ld hl,$9fe1
	ld bc,$0112
	xor a
	ld ($ff4f),a
	ld a,efxselbank00
	call data_entry_sel_copy_from_over
	
	;---------------------------------------------------------------------------
	; now set up for sprites

	pop de  ; x pos * 2
	pop bc	; y pos * 2

	; get index of sel_idx
	ld a,(data_entry_sel_idx_table)
	ld l,a
	ld a,(data_entry_sel_idx_table+1)
	ld h,a

	add hl,bc
	ld a,(hli)
	ld h,(hl)
	ld l,a
	add hl,de
	; hl is now the address of the important data
	ld a,(hli)
	ld b,a			; b is the x position
	srl a			; /8
	srl a
	srl a
	dec a
	dec a
	ld (data_entry_x_position),a
	ld c,(hl)		; c is the width

	;---------------------------------------------------------------------------
	; now set the sprites to the cursor x position we want

	ld hl,$fe79
	ld de,$0004

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld (hl),b

	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,8
	add a,b
	ld b,a
	add hl,de
	dec c
	jr nz,_z_0

	;---------------------------------------------------------------------------
	; now clear the unused sprites

	ld b,$e0

_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld (hl),b

	ld a,($ff41)
	and 2
	jr nz,_z_1

	add hl,de
	ld a,l
	cp $a0
	jr c,_z_1

	;---------------------------------------------------------------------------
	; now do the joy controls

	ld a,(joy_held)
	bit joy_bit_a,a
	jp z,_no_a_pressed

	; store our address of byte to modify

	ld a,(menu_cur_sel)
	ld e,a
	swap a
	ld c,a
	and $0f
	ld b,a
	ld a,c
	and $f0
	ld c,a

	ld a,(data_entry_x_menu_sel)
	ld d,a
	or c
	ld c,a
	
	ld hl,data_entry_work_ram
	add hl,bc

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

	ld c,e
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

	ld c,d
	sla c

	add hl,bc
	ld a,(hli)
	ld (data_entry_disp_flag_bits),a
	ld a,(hl)
	ld (data_entry_disp_flag_bits+1),a

	; now get the type of the byte we are working on

	sla e
	ld d,0

	ld a,(data_entry_data_type_table)
	ld l,a
	ld a,(data_entry_data_type_table+1)
	ld h,a
	add hl,de

	ld a,(hli)
	ld h,(hl)
	ld l,a

	ld a,(data_entry_x_menu_sel)
	ld e,a
	add hl,de

	ld a,(hl)
	ld e,a
	ld (data_entry_work_byte_type),a

	cp data_entry_type_last_button+1
	jr nc,_not_button
	
	cp data_entry_type_first_button
	ret c

	ld c,a

	ld a,(joy_pressed)
	bit joy_bit_a,a
	ret z

	ld a,c

	ld (data_entry_menu_mode),a
	ret	

_not_button:

	; now get the max limit

	ld hl,data_entry_max_limit_table
	add hl,de

	ld a,(hl)
	ld (data_entry_work_byte_max),a
		
	ld a,1
	ld (data_entry_menu_mode),a
	ret

_no_a_pressed:

	ld a,(joy_held_repeat)
	bit joy_bit_left,a
	jr z,_not_left

	; left:

	ld a,(data_entry_x_menu_sel)
	and a
	ret z

	dec a
	ld (data_entry_x_menu_sel),a
	ret

_not_left:
	bit joy_bit_right,a
	jr z,_not_right
	
	; right

	ld a,(data_entry_x_menu_sel)
	inc a
	ld (data_entry_x_menu_sel),a

	jr _check_num_selections

_not_right:

	ret

	;---------------------------------------------------------------------------
	; check number of selections, ensure valid

_check_num_selections:

	ld b,0
	ld a,(menu_cur_sel)
	ld (data_entry_menu_cur_sel),a
	ld c,a

	ld a,(data_entry_x_sel_num_table)
	ld l,a
	ld a,(data_entry_x_sel_num_table+1)
	ld h,a
	add hl,bc

	ld a,(data_entry_x_menu_sel)
	cp (hl)
	ret c

	ld a,(hl)
	dec a
	ld (data_entry_x_menu_sel),a

	ret	
