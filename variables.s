
;---------------------------------------------------------------------------

mus_ram 		equ $c000	; $0100 $c000-$c0ff 	; must not cross a $100
pal_ram			equ $c100	; $001c $c100-$c11c	
joy_ram 		equ $c120	; $0006 $c120-$c126
data_entry_ram		equ $c130	; $0190 $c130-$c2c0
control_ram 		equ $c2d0	; $0033 $c2d0-$c303
text_ram		equ $c308	; $000e $c308-$c316
drawing_ram		equ $c320	; $0096 $c320-$c3b6
menu_ram		equ $c3c0	; $0012 $c3c0-$c3d2
play_mode_ram		equ $c3e0	; $0004 $c3e0-$c3e4
sprite_ram		equ $c3e8	; $0011 $c3e8-$c3f9

editor_sequence_buffer	equ $c400	; $0408 $c400-$c808
plasma_ram		equ $c810	; $00a9 $c810-$c8b9
editor_undo_buffer	equ $c8c0	; $0400 $c8c0-$ccbf ; max_undos*4 = size

effects_copy_buffer	equ $cdc0	; $00d0	$cdc0-$ce90

file_access_ram		equ $cf00	; $0014 $cf00-$cf14

stack_ram		equ $cffe	; $xxxx $cfxx-$cffe
sam_ram 		equ $ff80	; $0009 $ff80-$ff89 	; must not cross a $100

;---------------------------------------------------------------------------

music_buffer_start_bank		equ $01 ; $d000-$dfff	; uses banks 1-6
music_buffer_end_bank		equ $06 ; $d000-$dfff	; uses banks 1-6
editor_block_buffer_bank	equ $07 ; $d000-$dfff	; fills entire bank
