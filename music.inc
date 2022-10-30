;---------------------------------------------------------------------------

mus_def_speed	equ $30
mus_num_songs	equ 20

;***************************************************************************
musicbank00	equ	$10
music00		group	musicbank00		; Set group bank
		org 	musicbank00:$4000
this_bank	= 	musicbank00
;***************************************************************************

;***************************************************************************

	include music_effecttables.inc
	include music_player_2.s

;***************************************************************************

mus_song_address_table:
	dw s_header_00
	dw s_header_01
	dw s_header_02
	dw s_header_03
	dw s_header_04
	dw s_header_05
	dw s_header_06
	dw s_header_07
	dw s_header_08
	dw s_header_09
	dw s_header_0a
	dw s_header_0b
	dw s_header_0c
	dw s_header_0d
	dw s_header_0e
	dw s_header_0f
	dw s_header_10
	dw s_header_11
	dw s_header_12
	dw s_header_13

mus_song_bank_table:
	db s_bank___00
	db s_bank___01
	db s_bank___02
	db s_bank___03
	db s_bank___04
	db s_bank___05
	db s_bank___06
	db s_bank___07
	db s_bank___08
	db s_bank___09
	db s_bank___0a
	db s_bank___0b
	db s_bank___0c
	db s_bank___0d
	db s_bank___0e
	db s_bank___0f
	db s_bank___10
	db s_bank___11
	db s_bank___12
	db s_bank___13

;***************************************************************************
musicbank01	equ	musicbank00+$01
music01		group	musicbank01		; Set group bank
		org 	musicbank01:$4000
this_bank	=	musicbank01
;***************************************************************************

	include music_notetable.inc

s_bank___08	equ this_bank
s_header_08:
	include music\aintus.inc

s_bank___09	equ this_bank
s_header_09:
	include music\world1.inc

s_bank___0b	equ this_bank
s_header_0b:
	libbin music\song.bin
s_bank___0c	equ this_bank
s_header_0c:
	libbin music\song2.bin

;***************************************************************************
musicbank02	equ	musicbank00+$02
music02		group	musicbank02		; Set group bank
		org 	musicbank02:$4000
this_bank	=	musicbank02
;***************************************************************************

s_bank___0d	equ this_bank
s_header_0d:
	libbin music\song3.bin


s_bank___00	equ this_bank
s_header_00:
	include music\music2-blank.inc

s_bank___01	equ this_bank
s_header_01:
	include music\music2-bonerparty.inc

s_bank___04	equ this_bank
s_header_04:
	include music\seven8.inc


s_bank___05	equ this_bank
s_header_05:
	include music\orlee.inc

;***************************************************************************
musicbank03	equ	musicbank00+$03
music03		group	musicbank03		; Set group bank
		org 	musicbank03:$4000
this_bank	=	musicbank03
;***************************************************************************

s_bank___02	equ this_bank
s_header_02:
	include music\wanderin.inc

s_bank___07	equ this_bank
s_header_07:
	libbin music\dungsong.bin

s_bank___0a	equ this_bank
s_header_0a:
	include music\missiona.inc

;***************************************************************************
musicbank04	equ	musicbank00+$04
music04		group	musicbank04		; Set group bank
		org 	musicbank04:$4000
this_bank	=	musicbank04
;***************************************************************************

s_bank___06	equ this_bank
s_header_06:
	include music\faggags.inc


s_bank___03	equ this_bank
s_header_03:
	include music\missionc.inc

;***************************************************************************
musicbank05	equ	musicbank00+$05
music05		group	musicbank05		; Set group bank
		org 	musicbank05:$4000
this_bank	=	musicbank05
;***************************************************************************

s_bank___0e	equ this_bank
s_header_0e:
	include music\illusion.inc

;***************************************************************************
musicbank06	equ	musicbank00+$06
music06		group	musicbank06		; Set group bank
		org 	musicbank06:$4000
this_bank	=	musicbank06
;***************************************************************************

s_bank___0f	equ this_bank
s_header_0f:
	include music\journey.inc

;***************************************************************************
musicbank07	equ	musicbank00+$07
music07		group	musicbank07		; Set group bank
		org 	musicbank07:$4000
this_bank	=	musicbank07
;***************************************************************************

s_bank___10	equ this_bank
s_header_10:
	include music\shitsack.inc

s_bank___11	equ this_bank
s_header_11:
	include music\sophisto.inc

;***************************************************************************
musicbank08	equ	musicbank00+$08
music08		group	musicbank08		; Set group bank
		org 	musicbank08:$4000
this_bank	=	musicbank08
;***************************************************************************

s_bank___12	equ this_bank
s_header_12:
	include music\kaliedadope.inc

;***************************************************************************
musicbank09	equ	musicbank00+$09
music09		group	musicbank09		; Set group bank
		org 	musicbank09:$4000
this_bank	=	musicbank09
;***************************************************************************

s_bank___13	equ this_bank
s_header_13:
	include music\mididevil.inc

;***************************************************************************
;***************************************************************************

            
	end
