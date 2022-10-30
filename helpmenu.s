;***************************************************************************
;---------------------------------------------------------------------------
; help menu setup
;---------------------------------------------------------------------------
;***************************************************************************

setup_help_menu:

	ld a,help_menu_mode
	ld (editor_mode),a

	ld a,(help_menu_sel)
	ld b,a
	ld c,$93

	ld a,$0c
	ld de,help_menu_gbc_map
	jp setup_menu

;***************************************************************************
;---------------------------------------------------------------------------
; help menu poop loop
;---------------------------------------------------------------------------
;***************************************************************************

help_menu_poop_loop:

	ld hl,poop
	push hl

	call menu_poop_loop
	ld a,(menu_cur_sel)
	ld (help_menu_sel),a

;---------------------------------------------------------------------------

	ld a,(joy_pressed)

	bit joy_bit_a,a
	jp z,_not_a_pressed

	ld hl,setup_text
	push hl
	
	ld a,(menu_cur_sel)	; process the selected item
	and a
	jr z,_setup_introduction_text00
	cp $01
	jr z,_setup_pattern_text00
	cp $02
	jr z,_setup_effects_text00
	cp $03
	jr z,_setup_sequence_text00
	cp $0a
	jr z,_setup_revision_text00
	cp $0b
	jr z,_setup_bugs_text00
	cp $0c
	jr z,_setup_contact_text00

	pop hl
	ret

_not_a_pressed:
	and joy_b+joy_select
	jp nz,setup_main_menu
	ret

;---------------------------------------------------------------------------

_setup_introduction_text00:
	ld hl,introduction_help_text_table ; text to display
	ret

_setup_pattern_text00:
	ld hl,pattern_editor_help_text_table ; text to display
	ret

_setup_effects_text00:
	ld hl,effects_editor_help_text_table ; text to display
	ret

_setup_sequence_text00:
	ld hl,sequence_editor_help_text_table ; text to display
	ret

_setup_revision_text00:
	ld hl,revision_history_help_text_table ; text to display
	ret

_setup_bugs_text00:
	ld hl,known_bugs_help_text_table ; text to display
	ret

_setup_contact_text00:
	ld hl,contact_info_help_text_table ; text to display
	ret

;***************************************************************************

	end
