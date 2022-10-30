;---------------------------------------------------------------------------
; FATASS Main Header file
; (c) 1999,2000
; you touch it, i punch you
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; set compiler to dmg mode
;---------------------------------------------------------------------------

	errtag		; Required for outputting the error file 
	puball		; Specify public. 

;***************************************************************************
maincodebank	equ	$00
maincode00	group	maincodebank	; Specify bank group. 
		org	maincodebank:$0000
this_bank	=	maincodebank
;***************************************************************************

;---------------------------------------------------------------------------
;-required crap----------------------
;---------------------------------------------------------------------------

	include variables.s	; variable address location map
	include nintendo.s	; gameboy color header

;---------------------------------------------------------------------------
;-code-------------------------------
;---------------------------------------------------------------------------

	include startup.s		; startup and main controller
	include main.s			; main routines
	include saveram.s		; saveram routines
	include control.s		; joypad control
	include graphics.s		; main graphics
	include drawing.s		; main editor drawing
	include palettes.s		; palette control
	include sprites.s		; sprite control
	include playmode.s		; play mode control
	include dataentry.s		; data entry control
	include editpattern.s		; pattern editor control
	include editsequence.s		; sequence editor control
	include editeffects.s		; effects editor control
	include editseqeffects.s	; seq effects editor control
	include mainmenu.s		; main menu control
	include helpmenu.s		; help menu control
	include configmenu.s		; configuration menu control
	include menu.s			; menu core control
	include text.s			; text mode core control
	include music_foot.s		; music player bank 0 foot
	include sampleplay.s		; sample player
	include plasma.s		; plasma effect
	include logo.s		; plasma effect
	include fileaccess.s		; file access routines
	include fileaccessmenu.s	; file access menus
	include editorfunctions.s	; editor functions
	include enterfilename.s		; enter filename menu

;***************************************************************************
codeoverbank	equ	$01
codeover00	group	codeoverbank	; Specify bank group. 
		org	codeoverbank:$4000
this_bank	=	codeoverbank
;***************************************************************************

	include dataentry-over.s	; data entry control
	include fileaccess-over.s	; file access routines
	include editeffects-over.s	; effects edit routines
	include editseqeffects-over.s	; seq effects edit routines
	include configmenu-over.s	; configuration menu control
	include enterfilename-over.s	; enter filename menu
	include editorfunctions-over.s	; editor functions
	include fileaccessmenu-over.s	; file access menus

;---------------------------------------------------------------------------
;-data-------------------------------
;---------------------------------------------------------------------------

	include graphics.inc	; all graphics
	include text.inc	; all text
	include samples.inc	; all samples
	include music.inc	; all music

;***************************************************************************

	end
