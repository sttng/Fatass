;---------------------------------------------------------------------------
; drawing graphics routines
;---------------------------------------------------------------------------
; contains the following functions:
;
; update_waveforms: redraws the waveform displays
; editor_init_block_buffer:
; editor_draw_block_buffer:
; editor_draw_block_line:
; editor_draw_block_line_up:
; editor_draw_track_buffer:
; editor_read_hl_from_song:
; editor_calc_waves:
; draw_square_wave:
; draw_noise_wave:
; draw_chip_wave:
;---------------------------------------------------------------------------
; editor_draw_block_buffer_4:
; editor_draw_block_buffer_3:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; contains the following tables
;
; bytes_per_effect:
; wait_name:
; note_names:
; note_names_4:
; render_bits:
; waveform_volume_trans:
; blank_waveform:
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; variables: 
;---------------------------------------------------------------------------

editor_volume_1		equ drawing_ram+$00	; $01 bytes
editor_volume_2		equ drawing_ram+$01	; $01 bytes
editor_volume_3		equ drawing_ram+$02	; $01 bytes
editor_volume_4		equ drawing_ram+$03	; $01 bytes

editor_last_line	equ drawing_ram+$04	; $01 bytes
editor_line_temp	equ drawing_ram+$05	; $01 bytes
editor_noise_rot	equ drawing_ram+$06	; $01 bytes

editor_display_1	equ drawing_ram+$07	; $01 bytes
editor_display_2	equ drawing_ram+$08	; $01 bytes

editor_line_count	equ drawing_ram+$09	; $01 bytes
editor_lines_used	equ drawing_ram+$0a	; $01 bytes
editor_lines_1		equ drawing_ram+$0b	; $01 bytes
editor_lines_2		equ drawing_ram+$0c	; $01 bytes
editor_lines_3		equ drawing_ram+$0d	; $01 bytes
editor_lines_4		equ drawing_ram+$0e	; $01 bytes

editor_wait_loop	equ drawing_ram+$0f	; $01 bytes
editor_macro_return	equ drawing_ram+$10	; $02 bytes
editor_note_names	equ drawing_ram+$12	; $02 bytes

grfx_temp		equ drawing_ram+$14	; $01 bytes

font_index		equ drawing_ram+$15	; $01 bytes		

editor_chip_render	equ drawing_ram+$16	; $80 bytes

;***************************************************************************
;---------------------------------------------------------------------------
; update waveforms
;---------------------------------------------------------------------------
;***************************************************************************

update_waveforms:

	ld a,(current_rom_bank)
	push af

	; track 1

	ld a,(editor_display_1)
	ld h,a

	ld c,0

	ld a,($ff25)
	and %00010001
	jr z,_ch1_off

	ld a,(editor_volume_1)
	and $e0
	ld c,a
_ch1_off:
	
	ld a,($ff11)
	ld de,$8ee0

	call draw_square_wave

	; do panning

	ld a,($ff25)
	and %00010001
	ld hl,$8f1a
	call _get_panning_addr	

	; track 2

	ld a,h
	ld (editor_display_1),a

	ld a,(editor_display_2)
	ld h,a

	ld c,0

	ld a,($ff25)
	and %00100010
	jr z,_ch2_off

	ld a,(editor_volume_2)
	and $e0
	ld c,a
_ch2_off:

	ld a,($ff16)
	ld de,$8ee0+$40

	call draw_square_wave

	; do panning

	ld a,($ff25)
	and %00100010
	rra
	ld hl,$8f5a
	call _get_panning_addr	

	; track 3

	ld a,h
	ld (editor_display_2),a

	call draw_chip_wave

	ld a,($ff25)
	and %01000100
	rra
	rra
	ld hl,$8fda
	call _get_panning_addr	

	; track 4

	call draw_noise_wave

	; do panning

	ld a,($ff25)
	and %10001000
	rra
	rra
	rra
	ld hl,$8ffa
	call _get_panning_addr	

	pop af
	ld (current_rom_bank),a
	ld ($2666),a

	ret


_get_panning_addr:

	; draw panning icon

	; which icon to use?

	; convert panning to 0-3

	ld b,a
	and $f0
	swap a
	sla a
	or b
	and $0f

	; * 4

	rla
	rla

	; get address

	ld de,panning_tileset
	add a,e
	ld e,a

	; and draw the damned thing

	ld a,wave_tileset_bank
	ld (current_rom_bank),a
	ld ($2666),a
	xor a
	ld ($ff4f),a

	ld b,3

_loop_4:

	ld a,(de)
	ld c,a
_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,c
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_z_0
	
	inc l
	ld a,c
	cpl
	ld c,a

_z_2:
	ld a,($ff41)
	and 2
	jr nz,_z_2

	ld a,(hl)
	or c
	ld (hl),a

	ld a,($ff41)
	and 2
	jr nz,_z_2

	inc l
	inc e
	dec b
	jr nz,_loop_4

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; editor init block buffer
;---------------------------------------------------------------------------
;***************************************************************************

editor_init_block_buffer:

	xor a
	ld (editor_display_1),a
	ld (editor_display_2),a
	dec a
	ld (editor_last_line),a
	ld (editor_redraw),a

	; do the sequence

	ld hl,editor_sequence_buffer
	ld a,$20
	ld (hli),a
	ld (hli),a
	ld (hli),a
	ld (hli),a
	ld c,l
	ld b,h

	; get address of sequence

	ld a,music_buffer_start_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,($d000)
	ld l,a
	ld a,($d001)
	add a,$d0
	ld h,a

	xor a
	ld (editor_max_seq),a

	ld e,$00
_seq_loop:
	ld a,h
	and $f0
	cp $d0
	jr z,_in_range

	ld a,(current_wram_bank)
	inc a
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,h
	sub $10
	ld h,a
	jr _seq_loop

_in_range:

	ld a,(hli)
	cp first_seq_effect
	jr c,_not_seq_effect

	cp ctrlbt_track_loop
	jr z,_done_seq_loop

	ld de,bytes_per_effect
	and $3f
	add a,e
	ld e,a
	ld a,d
	adc a,$00
	ld d,a
	ld a,(de)
	ld e,a
	ld d,$00
	add hl,de
	ld e,$40
	jr _seq_loop

_not_seq_effect:

	push hl
	ld l,e
	ld h,a

	ld d,>numbers
	swap a
	and $0f
	add a,<numbers
	ld e,a
	
	ld a,(de)
	or l
	ld (bc),a
	inc c

	ld a,h
	and $0f
	add a,<numbers
	ld e,a
	
	ld a,(de)
	or l
	ld (bc),a
	inc bc
	
	pop hl

	ld a,(editor_max_seq)
	inc a
	ld (editor_max_seq),a

	ld e,$00
	jr _seq_loop

_done_seq_loop:

	ld l,c
	ld h,b

	ld a,$20
	ld (hli),a
	ld (hli),a
	ld (hli),a
	ld (hli),a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld c,$00

	ld d,>numbers
	ld hl,$d000

_numbers:

	ld b,0

	ld a,c
	and $03
	jr nz,_no_b
	ld b,$40
_no_b:

	ld a,c
	swap a
	and $0f
	add a,<numbers
	ld e,a

	ld a,(de)
	or b
	ld (hli),a

	ld a,c
	and $0f
	add a,<numbers
	ld e,a

	ld a,(de)
	or b
	ld (hl),a

	ld a,$0f
	add a,l
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	inc c
	jr nz,_numbers

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; editor draw block buffer
;---------------------------------------------------------------------------
;***************************************************************************

editor_draw_block_buffer:

	ld a,(editor_seq)
	ld b,a
	ld e,a
	ld d,0
	sla e
	rl d
	ld hl,editor_sequence_buffer
	add hl,de

	ld de,$fe02
	ld c,$05

	dec b
	dec b

_seq_loop:
	ld a,(hl)
	cp $20
	; check for blank
	jr nz,_not_clear_sprite
	
	call clear_de_sprite_ram

	inc l
	inc l

	jr _clear_sprite
	
_not_clear_sprite:

	ld a,b
	ld (editor_line_temp),a

	call copy_b_number_de_sprite_ram

	ld b,2

	call copy_hl_de_sprite_ram

	ld a,(editor_line_temp)
	ld b,a

_clear_sprite:

	ld a,l
	and a
	jr nz,_seq_test_loop
	inc h
_seq_test_loop:
	
	inc b
	dec c
	jr nz,_seq_loop

	ld a,(editor_line)
	cp 5
	jr nc,_dont_clear_line

	ld hl,$9b61
	ld de,$20-18
	ld bc,$0205
	ld a,$20
_wipe_loop:
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	add hl,de
	dec c
	jr nz,_wipe_loop	

_dont_clear_line:

	ld a,<note_names
	ld (editor_note_names),a
	ld a,>note_names
	ld (editor_note_names+1),a

	ld a,(editor_sequence_1)
	ld l,a
	ld a,(editor_sequence_1+1)
	ld h,a

	ld a,(editor_last_sequence_1)
	cp l
	jr nz,_not_same_1

	ld a,(editor_last_sequence_1+1)
	cp h
	jr z,_same_1

_not_same_1:
	ld a,l
	ld (editor_last_sequence_1),a
	ld a,h
	ld (editor_last_sequence_1+1),a

	ld bc,$d002

	call editor_draw_track_buffer
	ld a,(editor_line_count)
	ld (editor_lines_1),a

_same_1:

	ld a,(editor_sequence_2)
	ld l,a
	ld a,(editor_sequence_2+1)
	ld h,a

	ld a,(editor_last_sequence_2)
	cp l
	jr nz,_not_same_2

	ld a,(editor_last_sequence_2+1)
	cp h
	jr z,_same_2

_not_same_2:
	ld a,l
	ld (editor_last_sequence_2),a
	ld a,h
	ld (editor_last_sequence_2+1),a

	ld bc,$d005

	call editor_draw_track_buffer
	ld a,(editor_line_count)
	ld (editor_lines_2),a

_same_2:

	ld a,(editor_sequence_3)
	ld l,a
	ld a,(editor_sequence_3+1)
	ld h,a

	ld a,(editor_last_sequence_3)
	cp l
	jr nz,_not_same_3

	ld a,(editor_last_sequence_3+1)
	cp h
	jr z,_same_3

_not_same_3:
	ld a,l
	ld (editor_last_sequence_3),a
	ld a,h
	ld (editor_last_sequence_3+1),a

	ld bc,$d008

	call editor_draw_track_buffer
	ld a,(editor_line_count)
	ld (editor_lines_3),a

_same_3:

	ld a,(editor_sequence_4)
	ld l,a
	ld a,(editor_sequence_4+1)
	ld h,a

	ld a,(editor_last_sequence_4)
	cp l
	jr nz,_not_same_4

	ld a,(editor_last_sequence_4+1)
	cp h
	jr z,_same_4

_not_same_4:
	ld a,l
	ld (editor_last_sequence_4),a 
	ld a,h
	ld (editor_last_sequence_4+1),a

	ld bc,$d00b

	ld a,<note_names_4
	ld (editor_note_names),a
	ld a,>note_names_4
	ld (editor_note_names+1),a

	call editor_draw_track_buffer
	ld a,(editor_line_count)
	ld (editor_lines_4),a

_same_4:

	ld a,(editor_lines_1)
	ld b,a
	ld a,(editor_lines_2)
	cp b
	jr c,_not_2
	ld b,a
_not_2:
	ld a,(editor_lines_3)
	cp b
	jr c,_not_3
	ld b,a
_not_3:
	ld a,(editor_lines_4)
	cp b
	jr c,_not_4
	ld b,a
_not_4:
	ld a,b
	ld (editor_lines_used),a

	ld a,(editor_line)
	cp b
	ret c
	ld a,b
	ld (editor_line),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; editor draw block line
;---------------------------------------------------------------------------
;***************************************************************************

editor_draw_block_line:

	xor a
	ld ($ff4f),a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(editor_redraw)
	and a
	jr nz,_is_new

	ld a,(editor_last_line)
	ld l,a
	ld a,(editor_line)
	ld (editor_last_line),a
	ld e,a

	sla a
	sla a
	sla a
	add a,$9d

	jr editor_draw_block_buffer_2

_is_new:

	xor a
	ld (editor_redraw),a	; @@@@ editor required

	call editor_draw_block_buffer

	ld a,(editor_line)
	ld (editor_last_line),a
	ld e,a
	sla a
	sla a
	sla a
	add a,$9d+8
;	add a,$9d
	ld (editor_ff42),a
	ld c,12
	ld a,e
	sub 6
	ld l,a
	ld e,a
	jp nc,editor_draw_block_buffer_4
	jp editor_draw_block_buffer_3

;***************************************************************************
;---------------------------------------------------------------------------
; editor draw block line up
;---------------------------------------------------------------------------
;***************************************************************************

editor_draw_block_line_up:

	xor a
	ld ($ff4f),a

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,(editor_last_line)
	ld l,a
	ld a,(editor_line)
	ld (editor_last_line),a
	ld e,a

	sla a
	sla a
	sla a
	add a,$9d+8

editor_draw_block_buffer_2:

	ld (editor_ff42),a

	ld a,e
	sub l
	ret z

	jr c,_up

; ok go down
;	jp editor_draw_block_line
	ld a,(editor_lines_used)
	ld b,a
	ld a,e
	sub l
	ld c,a
	ld a,l
	add a,6
	ld l,a		; start line to draw from
	ld e,a		; start line to draw to
	jr c,_problem_1
	dec a
	add a,c
	jr c,_problem_1
	dec a
	cp b
	; there could be a bad bug on multi line up here, but luckily that is currently
	; impossible
;	jr nc,_problem_1
	jp c,_no_problem
_problem_1:
	ld a,e
	dec a
	add a,c
	ld l,a
	ld e,a

	sla l

	ld a,l
	swap a
	and $03
	or $98
	ld h,a

	ld a,l
	swap a
	and $f0
	ld l,a
	inc l

	ld b,2

_wipe_loopa:
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	ld a,l
	sub $20-18
	ld l,a
	ld a,h
	sbc a,0
	ld h,a
	dec e
	ld a,e
	cp $ff
	jr z,_done_wipea
	dec c
	jr nz,_wipe_loopa
	ret

_done_wipea:
	dec c
	ret z
	ld l,e

	jr _no_problem

	

_up:

	ld a,l
	sub e

	ld c,a		; num lines to draw
	ld a,l
	sub 6
	ld l,a		; start line to draw from
	ld e,a		; start line to draw to

	jr nc,_no_problem
_editor_draw_block_buffer_3:
	sla l

	ld a,l
	swap a
	and $03
	or $98
	ld h,a

	ld a,l
	swap a
	and $f0
	ld l,a
	inc l

	ld b,2

_wipe_loop:
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	inc l
	call clear_hl_vram
	call clear_hl_vram
	call clear_hl_vram
	ld a,l
	add a,$20-18
	ld l,a
	ld a,h
	adc a,0
	and $9b
	ld h,a
	inc e
	jr z,_done_wipe
	dec c
	jr nz,_wipe_loop
	ret

_done_wipe:
	dec c
	ret z
	ld l,e

_no_problem:
_editor_draw_block_buffer_4:

	ld a,l

	swap a
	and $0f
	or $d0
	ld h,a

	ld a,l
	swap a
	and $f0
	ld l,a

	sla e

	ld a,e
	swap a
	and $03
	or $98
	ld d,a

	ld a,e
	swap a
	and $f0
	ld e,a

	inc e

	ld b,$02

_draw_loop:
	call copy_hl_de_vram
	call copy_hl_de_vram
	inc e
	call copy_hl_de_vram
	call copy_hl_de_vram
	call copy_hl_de_vram
	inc e
	call copy_hl_de_vram
	call copy_hl_de_vram
	call copy_hl_de_vram
	inc e
	call copy_hl_de_vram
	call copy_hl_de_vram
	call copy_hl_de_vram
	inc e
	call copy_hl_de_vram
	call copy_hl_de_vram
	call copy_hl_de_vram
	inc l
	inc hl

	ld a,e
	add a,$20-18
	ld e,a
	ld a,d
	adc a,0
	and $9b
	ld d,a

	; this crap is to fix the drawing numbers past bottom of area bug

	push hl
	ld a,l
	swap a
	and $0f
	ld l,a
	ld a,h
	swap a
	and $f0
	or l
	ld l,a

	ld a,h
	cp $e0
	jr nc,_do_wipe_damnit

	ld a,(editor_lines_used)
	cp l
	jr nc,_no_wipe_loop

_do_wipe_damnit:

	pop af
	dec c
	ret z

	ld l,e
	ld h,d
	ld e,0
	jp _wipe_loop

_no_wipe_loop:

	pop hl
	
	dec c
	jr nz,_draw_loop

	ret

editor_draw_block_buffer_4	equ _editor_draw_block_buffer_4
editor_draw_block_buffer_3	equ _editor_draw_block_buffer_3

;***************************************************************************
;---------------------------------------------------------------------------
; editor draw track buffer
;---------------------------------------------------------------------------
;***************************************************************************

editor_draw_track_buffer:

	; clear variables

	xor a
	ld (editor_wait_loop),a
	ld (editor_macro_return),a
	ld (editor_macro_return+1),a

	; set line count to max
	
	ld a,$ff
	ld (editor_line_count),a

	; clear effect flag

	ld e,0

_track_loop:

	; test for wait loop

	ld a,(editor_wait_loop)
	and a
	jr z,_not_wait_loop
	dec a
	jr _wait_loop	

_not_wait_loop:

	; read new value from song

	call editor_read_hl_from_song
	inc hl

	; test value to see what it is

	bit 7,a
	jp z,_note

	bit 6,a
	jr nz,_not_wait_c

	; it is a wait command

	and $3f
_wait_loop:
	ld (editor_wait_loop),a

	inc c
	inc c
	push hl

	ld a,e
	bit 1,a
	jp nz,_track_loop_3

	dec c
	dec c

	ld hl,wait_name
	jp _event

_not_wait_c:

	cp ctrlbt_track_loop
	jr nz,_not_track_loop

	; it is a track_loop command
	; check for macro exit

	ld a,(editor_macro_return)
	and a
	jr nz,_restore_macro
	ld a,(editor_macro_return+1)
	and a
	jp z,done_track

_restore_macro:

	; exit macro

	ld a,(editor_macro_return)
	ld l,a
	ld a,(editor_macro_return+1)
	ld h,a

	xor a
	ld (editor_macro_return),a
	ld (editor_macro_return+1),a

	jr _track_loop

_not_track_loop:

	cp ctrlbt_macro
	jr nz,_not_macro

	; it is a macro

	; update position to macro exit return

	inc hl
	inc hl

	; check for macro already, in which case skip

	ld a,(editor_macro_return)
	and a
	jp nz,_norm_effect
	ld a,(editor_macro_return+1)
	and a
	jp nz,_norm_effect

	; store macro exit return position

	ld a,l
	ld (editor_macro_return),a
	ld a,h
	ld (editor_macro_return+1),a

	; read jump address

	dec hl
	push de

	call editor_read_hl_from_song
	ld d,a
	dec hl

	call editor_read_hl_from_song
	ld e,a

	ld a,(mus_song_pat_address)
	ld l,a
	ld a,(mus_song_pat_address+1)
	ld h,a
	add hl,de

	pop de

	jr _norm_effect

_not_macro:

	cp ctrlbt_porta_from
	jr nz,_not_porta_from

	; it's porta from, so treat as effect, but also draw name

	call editor_read_hl_from_song
	inc hl
	ld e,a
	ld d,0

	push hl

	ld a,(editor_note_names)
	ld l,a
	ld a,(editor_note_names+1)
	ld h,a

	add hl,de
	sla e
	add hl,de

	ld a,(hli)
	or $40
	ld (bc),a
	inc c
	ld a,(hli)
	or $40
	ld (bc),a
	inc c
	ld a,(hl)
	or $40
	ld (bc),a
	dec c
	dec c
	pop hl

	set 1,e

	jp _track_loop

_not_porta_from:

	cp ctrlbt_sample
	jr nz,_not_sample

	; it's a sample

	; so treat as effect, but also draw name

	call editor_read_hl_from_song
	inc hl
	ld d,a

	;!!!! make it so that it cannot display
	;!!!! samples other that track 3

	ld a,e
	and $40
	ld e,a

	push hl

	ld a,$13
	or e
	ld (bc),a
	inc c

	ld h,>numbers
	ld a,d
	swap a
	and $0f
	add a,<numbers
	ld l,a
	ld a,(hl)
	or e
	ld (bc),a
	inc c

	ld a,d
	and $0f
	add a,<numbers
	ld l,a
	ld a,(hl)
	or e
	ld (bc),a
	dec c
	dec c
	pop hl

	set 1,e

	jp _track_loop

_not_sample:
_norm_effect:

	; add # of bytes to hl index

	push de

	and $3f
	ld de,bytes_per_effect
	add a,e
	ld e,a
	ld a,d
	adc a,0
	ld d,a
	ld a,(de)
	ld e,a
	ld d,0
	add hl,de

	pop de

	set 6,e

	jp _track_loop
_note:

	; draw a note

	; get name address into hl

	push hl
	push de
	ld e,a
	ld d,0

	ld a,(editor_note_names)
	ld l,a
	ld a,(editor_note_names+1)
	ld h,a

	add hl,de
	sla e
	add hl,de
	pop de

	res 1,e

_event:

	; draw name from hl

	bit 1,e
	jr nz,_track_loop_2

	ld a,e
	and $40
	ld e,a

	ld a,(hli)
	or e
	ld (bc),a
	inc c
	ld a,(hli)
	or e
	ld (bc),a
	inc c
	ld a,(hl)
	or e
	ld (bc),a
_track_loop_3:
	ld a,c
	add a,$0e
	ld c,a
	ld a,b
	adc a,$00
	ld b,a
_track_loop_2:

	ld a,(editor_line_count)
	inc a
	ld (editor_line_count),a

	pop hl
	ld a,b
	cp $e0
	ret nc
	
	ld e,0

	jp _track_loop

;---------------------------------------------------------------------------

done_track:	

	ld l,c
	ld h,b
	ld d,$20

_z_0:
	ld a,d
	ld (hli),a
	ld a,d
	ld (hli),a
	ld a,d
	ld (hl),a

	ld a,l
	add a,$0e
	ld l,a
	ld a,h
	adc a,$00
	ld h,a

	cp $e0
	jr c,_z_0	

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; editor_read_hl_from_song:
;---------------------------------------------------------------------------
;***************************************************************************

editor_read_hl_from_song:
	; get value from proper ram bank

	push hl
	push bc

	ld b,music_buffer_start_bank
	ld a,h
_bank_loop:
	and $f0
	cp $d0
	jr z,_got_bank

	inc b
	ld a,h
	sub $10
	ld h,a
	jr _bank_loop
	
_got_bank:

	ld a,b	
	ld (current_wram_bank),a
	ld ($ff70),a

	ld b,(hl)

	ld a,editor_block_buffer_bank
	ld (current_wram_bank),a
	ld ($ff70),a

	ld a,b
	pop bc
	pop hl

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; editor calc wave
;---------------------------------------------------------------------------
;***************************************************************************

editor_calc_waves:

	; calculate track 1

	ld d,0
	ld e,<mus_track_1
	ld a,(editor_initial_1)
	ld b,a
	xor a
	ld (editor_initial_1),a
	call _calc_wave

	; calculate track 2

	ld d,1
	ld e,<mus_track_2
	ld a,(editor_initial_2)
	ld b,a
	xor a
	ld (editor_initial_2),a
	call _calc_wave

	; calculate track 4

	ld d,3
	ld e,<mus_track_4
	ld a,(editor_initial_4)
	ld b,a
	xor a
	ld (editor_initial_4),a
	
_calc_wave:

	ld a,b	; a = editor_initial flag
	ld b,0
	ld c,e	; bc = player track offset
		; d = track editor data offset

	ld hl,mus_ram+o_envelope
	add hl,bc

	; check for new note

	ld c,d	; bc = editor track offset
	
	and a
	jr z,_not_new_note

	; new note
	; store initial volume

	ld a,(hl)	; track envelope
	and $f0		; initial volume
	ld e,a		; e = track initial volume

	ld hl,editor_volume_1
	add hl,bc	; hl = track editor volume
	ld (hl),e

	ret

_not_new_note:

	; update envelope for display

	ld a,(hl)	; track envelope
	ld e,a		; e = track envelope
	and $07
	ld d,a		; d = track envelope rate
	ret z		; envelope function is off if bottom 3 bits = 0

	ld hl,editor_volume_1
	add hl,bc	; hl = track editor volume

	ld a,d		; convert the rate to a usefull value
	xor $07
	inc a
	sla a
	dec a
	sla a
	ld c,a

	ld a,(hl)	; get the editor volume

	bit 3,e		; check for direction
	jr z,_sub

	add a,c		; add it
	jr nc,_no_carry_track
	ld a,$ff
	jr _no_carry_track
_sub:
	sub c		; subtract it
	jr nc,_no_carry_track
	xor a
_no_carry_track:
	ld (hl),a

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; draw square wave
;---------------------------------------------------------------------------
;***************************************************************************

draw_square_wave:

	; a = pw dir
	; de = $8ee0
	; c = volume and $e0
	; b = initial

	push de

	and $c0	

	rlca
	rlca
	ld b,a

	sla c
	rl b

	ld l,c
	ld h,b
	sla e
	rl d
	sla e
	rl d

	ld a,h
	cp d
	ret z

	pop de
	push hl

	ld hl,square_wave_tileset
	add hl,bc

	push hl
	push de
	inc hl
	inc de

	ld a,wave_tileset_bank
	ld (current_rom_bank),a
	ld ($2666),a

	xor a
	ld ($ff4f),a

	ld bc,$0220

_z_0:
	ld a,($ff41)
	and b
	jr nz,_z_0

	ld a,(hl)		
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_0

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_0

	ld bc,$000e

	pop hl
	add hl,bc
	ld e,l
	ld d,h

	pop hl
	add hl,bc

	ld bc,$0202

_z_1:
	ld a,($ff41)
	and b
	jr nz,_z_1

	ld a,(hl)		
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_1

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_1

	push hl
	ld l,e
	ld h,d

	ld bc,$001c
	add hl,bc
	ld e,l
	ld d,h
	pop hl
	add hl,bc

	ld bc,$0202

_z_2:
	ld a,($ff41)
	and b
	jr nz,_z_2

	ld a,(hl)		
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_2

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_2

	pop af

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; draw noise wave
;---------------------------------------------------------------------------
;***************************************************************************

draw_noise_wave:

	ld bc,$0000
	ld a,(mus_track_4+o_hold_mode)
	and mus_flag_tone_mode
	jr z,_not_tone
	inc b
_not_tone:

	ld c,0

	ld a,($ff25)
	and %10001000
	jr z,_ch4_off

	ld a,(editor_volume_4)
	and $e0
	ld c,a
_ch4_off:

	ld hl,noise_wave_tileset
	add hl,bc

	ld c,$10

	ld de,$8fe1

	ld a,(editor_noise_rot)
	inc a
	inc a
	inc a
	and 7
	ld (editor_noise_rot),a
	ld b,a

	ld a,wave_tileset_bank
	ld (current_rom_bank),a
	ld ($2666),a

	xor a
	ld ($ff4f),a

	push hl
	inc hl

_loop:

	ld a,(hli)
	ld b,a
	ld a,(editor_noise_rot)

_rot:
	rrc b
	dec a
	jr nz,_rot

_z_0:
	ld a,($ff41)
	and 2
	jr nz,_z_0

	ld a,b	
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_0

	inc hl
	inc de
	inc de
	dec c
	jr nz,_loop

	ld bc,$000e

	pop hl
	add hl,bc

	ld de,$8fee

	ld c,$02

_loop2:

	ld a,(hli)
	ld b,a
	ld a,(editor_noise_rot)

_rot2:
	rrc b
	dec a
	jr nz,_rot2

_z_1:
	ld a,($ff41)
	and 2
	jr nz,_z_1

	ld a,b	
	ld (de),a

	ld a,($ff41)
	and 2
	jr nz,_z_1

	inc hl
	inc de
	inc de
	dec c
	jr nz,_loop2

	ret

;***************************************************************************
;---------------------------------------------------------------------------
; draw chip wave
;---------------------------------------------------------------------------
;***************************************************************************

draw_chip_wave:

	ld b,$00
	
	ld a,(mus_track_3+o_envelope)
	and %01100000
	jr z,_gotit

	inc b

	cp %01100000
	jr z,_gotit

	inc b

	cp %01000000
	jr z,_gotit

	inc b

_gotit:

	swap b

	ld a,(sample_priority)
	and a
	jr z,_no_sample_v
	ld b,$30
_no_sample_v:

;	ld a,($ff1a)
;	and %10000000
;	jr z,_off

	ld a,($ff25)
	and %01000100
	jr nz,_not_off
;_off:
	ld b,$00	
_not_off:
	ld a,b
	ld (editor_volume_3),a

	ld hl,editor_chip_render
	ld c,$80
	ld a,$ff
_wipe_loop:
	ld (hli),a
	dec c
	jr nz,_wipe_loop

	ld de,$0000

_render_loop:

	push de

	; this might be more better

	ld hl,mus_waveform
	ld a,(sample_priority)
	and a
	jr z,_no_sample
	ld hl,$ff30
_no_sample:
	ld a,($ff25)
	and %01000100
	jr z,_trk_off
	ld a,($ff1a)
	and $80
	jr nz,_not_trk_off
_trk_off:
	ld hl,blank_waveform
_not_trk_off:

	srl e

	add hl,de
	ld b,(hl)
	
	push hl
	ld a,b
	swap a
	and $0f
	ld c,a
	ld a,(editor_volume_3)
	add a,c
	ld hl,waveform_volume_trans
	add a,l
	ld l,a
	ld a,h
	adc a,$00
	ld h,a
	ld a,(hl)
	swap a
	ld c,a

	ld a,b
	and $0f
	ld b,a
	ld a,(editor_volume_3)
	add a,b
	ld hl,waveform_volume_trans
	add a,l
	ld l,a
	ld a,h
	adc a,$00
	ld h,a
	ld a,(hl)
	or c
	ld c,a

	pop hl
	inc l
	ld a,e
	cp $1f/2

	jr nz,_not_cycle
	ld a,l
	sub $10
	ld l,a
_not_cycle:
	ld a,(hl)
	swap a
	and $0f

	ld b,a
	ld a,(editor_volume_3)
	add a,b
	ld hl,waveform_volume_trans
	add a,l
	ld l,a
	ld a,h
	adc a,$00
	ld h,a

	ld a,(hl)
	ld (editor_line_temp),a

	ld hl,editor_chip_render+1
	srl e
	srl e
	ld a,e
	and $0f
	swap a
	sla a
	ld e,a
	add hl,de

	pop de

	push hl

	ld hl,render_bits
	add hl,de
	ld b,(hl)

	pop hl

_line_loop:
	push hl

	ld a,c
	swap c
	cp c
	jr c,_less_than
	jr z,_equal
	swap c
	swap a
	and $0f
	sla a
	add a,l	
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	and b
	ld (hl),a

	ld a,c
	sub $10
	ld c,a

	pop hl	
	jr _line_loop

_less_than:
	swap c
	swap a
	and $0f
	sla a
	add a,l	
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	and b
	ld (hl),a

	ld a,c
	add a,$10
	ld c,a

	pop hl	
	jr _line_loop

_equal:
	swap c
	swap a
	and $0f
	sla a
	add a,l	
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	and b
	ld (hl),a
	
	inc e
	ld hl,render_bits
	add hl,de
	ld b,(hl)

_line_loop2:
	pop hl
	push hl

	ld a,c
	and $0f
	ld c,a

	sla a
	add a,l	
	ld l,a
	ld a,h
	adc a,0
	ld h,a

	ld a,(hl)
	and b
	ld (hl),a

	ld a,(editor_line_temp)
	cp c
	jr c,_less_than2
	jr z,_equal2

	inc c
	jr _line_loop2

_less_than2:

	dec c
	jr _line_loop2

_equal2:
	pop hl

	inc e
	ld a,e
	cp $20
	jp c,_render_loop	

;	ld hl,chip_wave_tileset
;	add hl,bc

	ld a,(editor_chip_render+$0f)
	cpl
	ld (editor_chip_render+$0e),a
	ld a,(editor_chip_render+$11)
	cpl
	ld (editor_chip_render+$10),a

	ld a,(editor_chip_render+$2f)
	cpl
	ld (editor_chip_render+$2e),a
	ld a,(editor_chip_render+$31)
	cpl
	ld (editor_chip_render+$30),a

	ld a,(editor_chip_render+$4f)
	cpl
	ld (editor_chip_render+$4e),a
	ld a,(editor_chip_render+$51)
	cpl
	ld (editor_chip_render+$50),a

	ld a,(editor_chip_render+$6f)
	cpl
	ld (editor_chip_render+$6e),a
	ld a,(editor_chip_render+$71)
	cpl
	ld (editor_chip_render+$70),a

	xor a
	ld (editor_chip_render+$0f),a
	ld (editor_chip_render+$11),a
	ld (editor_chip_render+$2f),a
	ld (editor_chip_render+$31),a
	ld (editor_chip_render+$4f),a
	ld (editor_chip_render+$51),a
	ld (editor_chip_render+$6f),a
	ld (editor_chip_render+$71),a

	ld hl,editor_chip_render+1
	ld bc,$0240

	ld de,$8ee0+$80+1

	xor a
	ld ($ff4f),a

_z_0:
	ld a,($ff41)
	and b
	jr nz,_z_0

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_0

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_0

	ld c,$02
	ld hl,editor_chip_render+$0e
	ld de,$8ee0+$80+$0e

_z_1:
	ld a,($ff41)
	and b
	jr nz,_z_1

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_1

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_1

	ld c,$02
	ld hl,editor_chip_render+$2e
	ld de,$8ee0+$80+$2e

_z_2:
	ld a,($ff41)
	and b
	jr nz,_z_2

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_2

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_2

	ld c,$02
	ld hl,editor_chip_render+$4e
	ld de,$8ee0+$80+$4e

_z_3:
	ld a,($ff41)
	and b
	jr nz,_z_3

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_3

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_3

	ld c,$02
	ld hl,editor_chip_render+$6e
	ld de,$8ee0+$80+$6e

_z_4:
	ld a,($ff41)
	and b
	jr nz,_z_4

	ld a,(hl)
	ld (de),a

	ld a,($ff41)
	and b
	jr nz,_z_4

	inc hl
	inc hl
	inc de
	inc de
	dec c
	jr nz,_z_4

	ret

;---------------------------------------------------------------------------
; tables
;---------------------------------------------------------------------------

bytes_per_effect:

	db 0,0,0,0	; $c0-$c3
	db 0,0,0,0	; $c4-$c7
	db 0,0,0,0	; $c8-$cb
	db 0,0,0,0	; $cc-$cf
	db 0,0,0,2	; $d0-$d3
	db 2,2,1,2	; $d4-$d7
	db 0,0,1,1	; $d8-$db
	db 1,3,1,1	; $dc-$df
	db 1,2,0,1	; $e0-$e3
	db 1,1,1,1	; $e4-$e7
	db 1,1,1,0	; $e8-$eb
	db 0,0,0,0	; $ec-$ef
	db 0,0,0,0	; $f0-$f3
	db 0,0,0,0	; $f4-$f7
	db 0,0,0,1	; $f8-$fb
	db 2,1,1,0	; $fc-$ff

wait_name:
	db $28,$29,$2a	; wait

note_names:
	db $1b,$1c,$1d	; note off
	db $03,$2d,$33	; c-3
	db $03,$23,$33	; c#3
	db $04,$2d,$33	; d-3
	db $04,$23,$33	; d#3
	db $05,$2d,$33	; e-3
	db $06,$2d,$33	; f-3
	db $06,$23,$33	; f#3
	db $07,$2d,$33	; g-3
	db $07,$23,$33	; g#3
	db $01,$2d,$33	; a-3
	db $01,$23,$33	; a#3
	db $02,$2d,$33	; b-3
	db $03,$2d,$34	; c-4
	db $03,$23,$34	; c#4
	db $04,$2d,$34	; d-4
	db $04,$23,$34	; d#4
	db $05,$2d,$34	; e-4
	db $06,$2d,$34	; f-4
	db $06,$23,$34	; f#4
	db $07,$2d,$34	; g-4
	db $07,$23,$34	; g#4
	db $01,$2d,$34	; a-4
	db $01,$23,$34	; a#4
	db $02,$2d,$34	; b-4
	db $03,$2d,$35	; c-5
	db $03,$23,$35	; c#5
	db $04,$2d,$35	; d-5
	db $04,$23,$35	; d#5
	db $05,$2d,$35	; e-5
	db $06,$2d,$35	; f-5
	db $06,$23,$35	; f#5
	db $07,$2d,$35	; g-5
	db $07,$23,$35	; g#5
	db $01,$2d,$35	; a-5
	db $01,$23,$35	; a#5
	db $02,$2d,$35	; b-5
	db $03,$2d,$36	; c-6
	db $03,$23,$36	; c#6
	db $04,$2d,$36	; d-6
	db $04,$23,$36	; d#6
	db $05,$2d,$36	; e-6
	db $06,$2d,$36	; f-6
	db $06,$23,$36	; f#6
	db $07,$2d,$36	; g-6
	db $07,$23,$36	; g#6
	db $01,$2d,$36	; a-6
	db $01,$23,$36	; a#6
	db $02,$2d,$36	; b-6
	db $03,$2d,$37	; c-7
	db $03,$23,$37	; c#7
	db $04,$2d,$37	; d-7
	db $04,$23,$37	; d#7
	db $05,$2d,$37	; e-7
	db $06,$2d,$37	; f-7
	db $06,$23,$37	; f#7
	db $07,$2d,$37	; g-7
	db $07,$23,$37	; g#7
	db $01,$2d,$37	; a-7
	db $01,$23,$37	; a#7
	db $02,$2d,$37	; b-7
	db $03,$2d,$38	; c-8
	db $03,$23,$38	; c#8
	db $04,$2d,$38	; d-8
	db $04,$23,$38	; d#8
	db $05,$2d,$38	; e-8
	db $06,$2d,$38	; f-8
	db $06,$23,$38	; f#8
	db $07,$2d,$38	; g-8
	db $07,$23,$38	; g#8
	db $01,$2d,$38	; a-8
	db $01,$23,$38	; a#8
	db $02,$2d,$38	; b-8

note_names_4:
	db $1b,$1c,$1d	; note off
	db $0f,$06,$06	; off
	db $0f,$06,$06	; off
	db $0f,$06,$06	; off
	db $0e,$30,$34	; n04
	db $0e,$30,$35	; n05
	db $0e,$30,$36	; n06
	db $0e,$30,$37	; n07
	db $0e,$30,$38	; n08
	db $0e,$30,$39	; n09
	db $0e,$30,$01	; n0a
	db $0e,$30,$02	; n0b
	db $0e,$30,$03	; n0c
	db $0e,$30,$04	; n0d
	db $0e,$30,$05	; n0e
	db $0e,$30,$06	; n0f
	db $0e,$31,$30	; n10
	db $0e,$31,$31	; n11
	db $0e,$31,$32	; n12
	db $0e,$31,$33	; n13
	db $0e,$31,$34	; n14
	db $0e,$31,$35	; n15
	db $0e,$31,$36	; n16
	db $0e,$31,$37	; n17
	db $0e,$31,$38	; n18
	db $0e,$31,$39	; n19
	db $0e,$31,$01	; n1a
	db $0e,$31,$02	; n1b
	db $0e,$31,$03	; n1c
	db $0e,$31,$04	; n1d
	db $0e,$31,$05	; n1e
	db $0e,$31,$06	; n1f
	db $0e,$32,$30	; n20
	db $0e,$32,$31	; n21
	db $0e,$32,$32	; n22
	db $0e,$32,$33	; n23
	db $0e,$32,$34	; n24
	db $0e,$32,$35	; n25
	db $0e,$32,$36	; n26
	db $0e,$32,$37	; n27
	db $0e,$32,$38	; n28
	db $0e,$32,$39	; n29
	db $0e,$32,$01	; n2a
	db $0e,$32,$02	; n2b
	db $0e,$32,$03	; n2c
	db $0e,$32,$04	; n2d
	db $0e,$32,$05	; n2e
	db $0e,$32,$06	; n2f
	db $0e,$33,$30	; n30
	db $0e,$33,$31	; n31
	db $0e,$33,$32	; n32
	db $0e,$33,$33	; n33
	db $0e,$33,$34	; n34
	db $0e,$33,$35	; n35
	db $0e,$33,$36	; n36
	db $0e,$33,$37	; n37
	db $0e,$33,$38	; n38
	db $0e,$33,$39	; n39
	db $0e,$33,$01	; n3a
	db $0e,$33,$02	; n3b
	db $0e,$33,$03	; n3c
	db $0e,$33,$04	; n3d
	db $0e,$33,$05	; n3e
	db $0e,$33,$06	; n3f

	db $0f,$06,$06	; off
	db $0f,$06,$06	; off
	db $0f,$06,$06	; off
	db $0f,$06,$06	; off
	
	db $04,$2d,$1a	; d-z
	db $05,$2d,$1a	; e-z
	db $07,$2d,$1a	; g-z
	db $01,$23,$19	; a#y
	db $04,$2d,$19	; d-y
	db $05,$2d,$19	; e-y
	db $07,$2d,$19	; g-y
	db $01,$23,$18	; a#x
	db $04,$2d,$18	; d-x
	db $05,$2d,$18	; e-x
	db $07,$2d,$18	; g-x
	db $01,$23,$30	; a#0
	db $04,$2d,$30	; d-0
	db $05,$2d,$30	; e-0
	db $07,$2d,$30	; g-0
	db $01,$23,$31	; a#1
	db $04,$2d,$31	; d-1
	db $05,$2d,$31	; e-1
	db $07,$2d,$31	; g-1
	db $01,$23,$32	; a#2
	db $04,$2d,$32	; d-2
	db $05,$2d,$32	; e-2
	db $07,$2d,$32	; g-2
	db $01,$23,$33	; a#3
	db $04,$2d,$33	; d-3
	db $05,$2d,$33	; e-3
	db $07,$2d,$33	; g-3
	db $01,$23,$34	; a#4
	db $04,$2d,$34	; d-4
	db $05,$2d,$34	; e-4
	db $07,$2d,$34	; g-4
	db $01,$23,$35	; a#5
	db $04,$2d,$35	; d-5
	db $05,$2d,$35	; e-5
	db $07,$2d,$35	; g-5
	db $01,$23,$36	; a#6
	db $04,$2d,$36	; d-6
	db $05,$2d,$36	; e-6
	db $07,$2d,$36	; g-6
	db $01,$23,$37	; a#7
	db $04,$2d,$37	; d-7
	db $05,$2d,$37	; e-7
	db $07,$2d,$37	; g-7
	db $01,$23,$38	; a#8
	db $04,$2d,$38	; d-8
	db $05,$2d,$38	; e-8
	db $07,$2d,$38	; g-8
	db $01,$23,$39	; a#9
	db $04,$2d,$39	; d-9
	db $05,$2d,$39	; e-9
	db $07,$2d,$39	; g-9
	db $01,$23,$01	; a#a
	db $04,$2d,$01	; d-a
	db $05,$2d,$01	; e-a
	db $07,$2d,$01	; g-a
	db $01,$23,$02	; a#b
	db $05,$2d,$02	; e-b
	db $01,$23,$03	; a#c
	db $01,$23,$04	; a#d
	db $01,$23,$05	; a#e

render_bits:
	db %01111111
	db %10111111
	db %11011111
	db %11101111
	db %11110111
	db %11111011
	db %11111101
	db %11111110
	db %01111111
	db %10111111
	db %11011111
	db %11101111
	db %11110111
	db %11111011
	db %11111101
	db %11111110
	db %01111111
	db %10111111
	db %11011111
	db %11101111
	db %11110111
	db %11111011
	db %11111101
	db %11111110
	db %01111111
	db %10111111
	db %11011111
	db %11101111
	db %11110111
	db %11111011
	db %11111101
	db %11111110

waveform_volume_trans:
	db $07,$07,$07,$07,$07,$07,$07,$07,$08,$08,$08,$08,$08,$08,$08,$08
	db $05,$05,$06,$06,$06,$07,$07,$07,$08,$08,$08,$09,$09,$09,$0a,$0a
	db $03,$03,$04,$04,$05,$05,$06,$07,$08,$09,$0a,$0a,$0b,$0b,$0c,$0c
	db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f

blank_waveform:
	db $78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78

;***************************************************************************

	end

	
