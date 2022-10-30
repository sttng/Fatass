;---------------------------------------------------------------------------
; gameboy color startup code
;
; contains:
; startup code
; main loop
; vbl_irq
; tim_irq
; lcdc_irq
; wait_vbl:	waits for a vbl
; setup_titlescreen:
;---------------------------------------------------------------------------

;***************************************************************************
; variables
;***************************************************************************

; control variables

; globals:

gbc			equ control_ram+$00	; $01 bytes
editor_num_redos	equ control_ram+$01	; $01 bytes

editor_mode		equ control_ram+$02	; $01 bytes

editor_ff42		equ control_ram+$03	; $01 bytes
editor_line		equ control_ram+$04	; $01 bytes
editor_seq		equ control_ram+$05	; $01 bytes
editor_redraw		equ control_ram+$06	; $01 bytes
editor_track		equ control_ram+$07	; $01 bytes

editor_sequence_1	equ control_ram+$08	; $02 bytes
editor_last_sequence_1	equ control_ram+$0a	; $02 bytes
editor_sequence_2	equ control_ram+$0c	; $02 bytes
editor_last_sequence_2	equ control_ram+$0e	; $02 bytes
editor_sequence_3	equ control_ram+$10	; $02 bytes
editor_last_sequence_3	equ control_ram+$12	; $02 bytes
editor_sequence_4	equ control_ram+$14	; $02 bytes
editor_last_sequence_4	equ control_ram+$16	; $02 bytes

; test against last sequence

fast_forward		equ control_ram+$18	; $01 bytes
editor_looping_mode	equ control_ram+$19	; $01 bytes
editor_max_seq		equ control_ram+$1a	; $01 bytes

editor_initial_1	equ control_ram+$1b	; $01 bytes
editor_initial_2	equ control_ram+$1c	; $01 bytes
editor_initial_4	equ control_ram+$1d	; $01 bytes

editor_ff_temp		equ control_ram+$1e	; $01 bytes

file_info_win_flag	equ control_ram+$1f	; $01 bytes

editor_num_undos	equ control_ram+$20	; $01 bytes
editor_copy_from	equ control_ram+$21	; $02 bytes

last_edit_track		equ control_ram+$23	; $01 bytes
last_edit_patt		equ control_ram+$24	; $01 bytes

song_time		equ control_ram+$25	; $05 bytes : ticks, seconds, minutes, hours, days
draw_time_flag		equ control_ram+$2a	; $01 bytes

editor_copy_patt	equ control_ram+$2b	; $08 bytes

;***************************************************************************

play_mode			equ $00
edit_pattern_mode		equ $02
edit_sequence_mode		equ $04
edit_effects_mode		equ $06
main_menu_mode			equ $08
help_menu_mode			equ $0a
text_display_mode		equ $0c
plasma_mode			equ $0e
configuration_mode		equ $10
load_from_rom_menu_mode		equ $12
load_from_sram_menu_mode	equ $14
save_to_sram_menu_mode		equ $16
delete_from_sram_menu_mode	equ $18
edit_seq_effects_mode		equ $1a
enter_filename_mode		equ $1c
null_mode			equ $1e
logo_mode			equ $20

;***************************************************************************
;---------------------------------------------------------------------------
; interrupt vectors
;---------------------------------------------------------------------------
;***************************************************************************

	org $000:$0000	; RST-0
	reti

	db "---fatass tracker! v0.14---"

	org $000:$0040	; VBlank IRQ
	jp vbl_irq	; do our lcdc status routine

	org $000:$0048	; LCDC Status IRQ
	jp lcdc_irq

	org $000:$0050	; Timer Owerflow
	jp tim_irq	; do our timer routine

	org $000:$0058	; Serial Transfer Completion
	reti		; Do nothing

	org $000:$0060	; Keypad interrupt
	reti		; Do nothing

;***************************************************************************
; startup code
;***************************************************************************

;***************************************************************************
;---------------------------------------------------------------------------
; main startup code
;---------------------------------------------------------------------------
;***************************************************************************

	org $000:$0150

numbers:
	db $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$01,$02,$03,$04,$05,$06

code_start:
reset: 

	di

;---------------------------------------------------------------------------
; check for gameboy color
;---------------------------------------------------------------------------

	; hard reset start code here

	cp $11			; check A against cgb code ($11)
	jr z,_setup_iscolor	; if we got it, go for it

	; do b/w gameboy no good loop

	xor a
	ld (gbc),a
	ld ($ff0f),a	; IE	Interupt Enable reset

	jr _setup_dmg

;---------------------------------------------------------------------------
; gameboy color startup
;---------------------------------------------------------------------------

_setup_iscolor:

	; set gbc flag

	ld a,1
	ld (gbc),a

	; check for doublespeed mode

	ld a,($ff4d)
	cp $80
	jr z,_setup_isdoublespeed

	ld a,$01
	ld ($ff4d),a	; set the doublespeed flag
	xor a		; a=0
	ld ($ff0f),a	; clear the interrupt request flag
	ld ($ffff),a	; clear the interrupt enable flag
	ld ($ff4f),a	; VBK	Vram Bank reset     (0)
	ld a,$30
	ld ($ff00),a	; set 1 in bit 4&5 of p1 register
	stop

_setup_isdoublespeed:
_setup_dmg:

;---------------------------------------------------------------------------
; initialize
;---------------------------------------------------------------------------

	ld sp,stack_ram		; Put the stack where the GB wants it

	; turn off screen

	call wait_vbl	; Must be in VBL before turning the screen off.
	xor a		
	ld ($ff40),a	; LCDC  Turn Screen Off
	ld ($ff41),a	; STAT 	LCDC Status info reset (off)
	ld ($ff4a),a	; WY	Window Y=0
	ld (current_wram_bank),a	; set wram bank holder
	ld ($ff70),a	; SVBK	Set bank for $d000-$dfff to 0
	ld ($ff56),a	; RP	Infrared port reset (off)
	ld ($ff24),a	; NR50	Zero Master Volume
	ld ($1666),a	; RAMG	Disable Save Ram Access
	ld ($3666),a	; ROMB1 Set hibyte for $4000-$7fff to 0
	ld ($4666),a	; RAMB	Set bank for $a000-$bfff to 0
	ld (current_sram_bank),a	; set sram bank holder

	inc a		; a=1 quick way
	ld (player_max),a

	ld a,codeoverbank
	ld (current_rom_bank),a
	ld ($2666),a	; ROMB0 Set lowbyte for $4000-$7fff to 1

	call init_save_ram	; format save ram if necessary

;---------------------------------------------------------------------------
; initialize routines
;---------------------------------------------------------------------------

	ld a,$33
	ld (menu_raster_pos),a

	xor a
	ld (editor_seq),a
	ld (efx_menu_sel),a
	ld (efx_x_menu_sel),a
	ld (seq_efx_menu_sel),a
	ld (seq_efx_x_menu_sel),a
	ld (conf_menu_sel),a
	ld (conf_x_menu_sel),a
	ld (main_menu_sel),a
	ld (help_menu_sel),a
	ld (load_from_rom_menu_sel),a
	ld (load_from_sram_menu_sel),a
	ld (save_from_sram_menu_sel),a
	ld (delete_from_sram_menu_sel),a
	ld (editor_track),a
	ld (editor_line),a
	ld (fast_forward),a
	ld (editor_looping_mode),a
	ld (file_info_win_flag),a
	ld (draw_time_flag),a
	ld (editor_last_sequence_1),a
	ld (editor_last_sequence_1+1),a
	ld (editor_last_sequence_2),a
	ld (editor_last_sequence_2+1),a
	ld (editor_last_sequence_3),a
	ld (editor_last_sequence_3+1),a
	ld (editor_last_sequence_4),a
	ld (editor_last_sequence_4+1),a
	ld ($ff45),a	; first lcdc int at y=0
	ld ($ff42),a	; SCY 	Screen scroll Y=0
	ld ($ff43),a	; SCX 	Screen scroll X=0

	ld hl,effects_copy_buffer
	ld c,$d0
_wipe_effects_copy_buffer:
	ld (hli),a
	dec c
	jr nz,_wipe_effects_copy_buffer

	call load_song_into_ram_from_rom

	call music_hard_reset			; setup music player

	call sam_setup				; setup sample player

	call music_reset
	xor a
	ld ($ff25),a

	call cset_load
	call plasma_load

	ld hl,$8800
	ld de,main_tileset
	ld bc,$4020
	ld a,tilebank00 
	call scr_copy_to_vram_dmg

	ld a,(gbc)
	cp $00
	call nz,screen_setup_gbc

	ld a,(gbc)
	cp $00
	call z,screen_setup_dmg

	call sprite_setup

	call editor_init_block_buffer
	call editor_draw_block_line
	call setup_edit_patt

;---------------------------------------------------------------------------
; and let's go!
;---------------------------------------------------------------------------

	ld a,%01000100		;set lcdc int
  	ld ($ff41),a

	ld a,%00000011  ; We want Vblank and LCDC Interrupts Enabled 
	ld ($ffff),a	

	call wait_vbl	; Must be in VBL before turning the screen On.
	ld a,%11011011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a
	ei

;***************************************************************************
;---------------------------------------------------------------------------
; main loop
;---------------------------------------------------------------------------
;***************************************************************************

poop:

	ld a,(editor_mode)
	add a,<poop_loop_table
	ld l,a
	ld a,>poop_loop_table
	adc a,0
	ld h,a
	ld a,(hli)
	ld h,(hl)
	ld l,a
	jp (hl)
	
;***************************************************************************
;---------------------------------------------------------------------------
; vbl_irq
;---------------------------------------------------------------------------
;***************************************************************************

vbl_irq:

	push af
	push bc
	push de
	push hl
	ld a,(current_rom_bank)
	ld b,a
	ld a,($ff4f)
	ld c,a
	push bc

	ld a,(editor_mode)
	add a,<vbl_irq_table
	ld l,a
	ld a,>vbl_irq_table
	adc a,0
	ld h,a
	ld a,(hli)
	ld h,(hl)
	ld l,a
	jp (hl)

done_vbl:

	; ticks

	ld a,(song_time)	
	inc a
	ld (song_time),a
	cp 60
	jr c,_done_time

	xor a
	ld (song_time),a

	call draw_time

	; seconds

	ld a,(song_time+1)
	inc a
	ld (song_time+1),a
	ld b,a
	and $0f
	cp $0a
	jr c,_done_time

	ld a,b
	and $f0
	add a,$10
	ld (song_time+1),a
	cp $60
	jr c,_done_time

	xor a
	ld (song_time+1),a

	; minutes

	ld a,(song_time+2)
	inc a
	ld (song_time+2),a
	ld b,a
	and $0f
	cp $0a
	jr c,_done_time

	ld a,b
	and $f0
	add a,$10
	ld (song_time+2),a
	cp $60
	jr c,_done_time

	xor a
	ld (song_time+2),a

	; hours

	ld a,(song_time+3)
	inc a
	ld (song_time+3),a
	ld b,a
	and $0f
	cp $0a
	jr c,_done_hour_small

	ld a,b
	and $f0
	add a,$10
	ld (song_time+3),a
	jr _done_time

_done_hour_small:
	ld a,b
	cp $24
	jr c,_done_time

	xor a
	ld (song_time+3),a

	; days

	ld a,(song_time+4)
	inc a
	ld (song_time+4),a

_done_time:

	pop bc
	ld a,b
	ld (current_rom_bank),a
	ld ($2666),a	; ROMB0 Set lowbyte for $4000-$7fff
	ld a,c
	ld ($ff4f),a
	pop hl
	pop de
	pop bc
	pop af
	reti

;***************************************************************************
;---------------------------------------------------------------------------
; lcdc_irq
;---------------------------------------------------------------------------
;***************************************************************************

lcdc_irq:

	push af
	push hl

	ld a,(gbc)
	and a
	jr nz,_color_lcdc

	ld a,(editor_mode)
	add a,<dmg_lcdc_irq_table
	ld l,a
	ld a,>dmg_lcdc_irq_table
	adc a,0
	ld h,a
	ld a,(hli)
	ld h,(hl)
	ld l,a
	jp (hl)

_color_lcdc:

	ld a,(editor_mode)
	add a,<lcdc_irq_table
	ld l,a
	ld a,>lcdc_irq_table
	adc a,0
	ld h,a
	ld a,(hli)
	ld h,(hl)
	ld l,a
	jp (hl)

done_lcdc:

	pop hl
	pop af
	reti

;***************************************************************************

poop_loop_table:

	dw play_mode_poop_loop			; $00 play mode
	dw edit_patt_poop_loop			; $02 edit pattern mode
	dw edit_seq_poop_loop			; $04 edit sequence mode
	dw edit_efx_poop_loop			; $06 edit effects mode
	dw main_menu_poop_loop			; $08 main menu mode
	dw help_menu_poop_loop			; $0a help menu mode
	dw text_poop_loop			; $0c text mode
	dw plasma_poop_loop			; $0e plasma mode
	dw config_poop_loop			; $10 configuration mode
	dw load_from_rom_menu_poop_loop		; $12 load from rom menu mode
	dw load_from_sram_menu_poop_loop	; $14 load from sram menu mode
	dw save_to_sram_menu_poop_loop		; $16 save to sram menu mode
	dw delete_from_sram_menu_poop_loop	; $18 delete from sram menu mode
	dw edit_seq_efx_poop_loop		; $1a edit seq effects mode
	dw enter_filename_poop_loop		; $1c enter filename mode
	dw poop					; $1e null mode
	dw logo_poop_loop			; $20 logo mode

vbl_irq_table:

	dw play_mode_vbl_irq	; $00 play mode
	dw edit_patt_vbl_irq	; $02 edit pattern mode
	dw edit_seq_vbl_irq	; $04 edit sequence mode
	dw data_entry_vbl_irq	; $06 edit effects mode
	dw menu_vbl_irq		; $08 main menu mode
	dw menu_vbl_irq		; $0a help menu mode
	dw text_vbl_irq		; $0c text mode
	dw plasma_vbl_irq	; $0e plasma mode
	dw data_entry_vbl_irq	; $10 configuration mode
	dw menu_vbl_irq		; $12 load from rom menu mode
	dw menu_vbl_irq		; $14 load from sram menu mode
	dw menu_vbl_irq		; $16 save to sram menu mode
	dw menu_vbl_irq		; $18 delete from sram menu mode
	dw data_entry_vbl_irq	; $1a edit seq effects mode
	dw data_entry_vbl_irq	; $1c enter filename mode
	dw done_vbl		; $1e null mode
	dw logo_vbl_irq		; $20 logo mode
	
lcdc_irq_table:

	dw play_mode_lcdc_irq	; $00 play mode
	dw play_mode_lcdc_irq	; $02 edit pattern mode
	dw play_mode_lcdc_irq	; $04 edit sequence mode
	dw data_entry_lcdc_irq	; $06 edit effects mode
	dw menu_lcdc_irq	; $08 main menu mode
	dw menu_lcdc_irq	; $0a help menu mode
	dw text_lcdc_irq	; $0c text mode
	dw done_lcdc		; $0e plasma mode
	dw data_entry_lcdc_irq	; $10 configuration mode
	dw menu_lcdc_irq	; $12 load from rom menu mode
	dw menu_lcdc_irq	; $14 load from sram menu mode
	dw menu_lcdc_irq	; $16 save to sram menu mode
	dw menu_lcdc_irq	; $18 delete from sram menu mode
	dw data_entry_lcdc_irq	; $1a edit seq effects mode
	dw data_entry_lcdc_irq	; $1c enter filename mode
	dw done_lcdc		; $1e null mode
	dw done_lcdc		; $20 logo mode

dmg_lcdc_irq_table:

	dw play_mode_lcdc_dmg	; $00 play mode
	dw play_mode_lcdc_dmg	; $02 edit pattern mode
	dw play_mode_lcdc_dmg	; $04 edit sequence mode
	dw data_entry_lcdc_dmg	; $06 edit effects mode
	dw menu_lcdc_dmg	; $08 main menu mode
	dw menu_lcdc_dmg	; $0a help menu mode
	dw text_lcdc_dmg	; $0c text mode
	dw done_lcdc		; $0e plasma mode
	dw data_entry_lcdc_dmg	; $10 edit effects mode
	dw menu_lcdc_dmg	; $12 load from rom menu mode
	dw menu_lcdc_dmg	; $14 load from sram menu mode
	dw menu_lcdc_dmg	; $16 save to sram menu mode
	dw menu_lcdc_dmg	; $18 delete from sram menu mode
	dw data_entry_lcdc_dmg	; $1a edit seq effects mode
	dw data_entry_lcdc_dmg	; $1c enter filename mode
	dw done_lcdc		; $1e null mode
	dw done_lcdc		; $20 logo mode

;***************************************************************************

	end
