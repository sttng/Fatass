
;***************************************************************************
graphicsbank00	equ	$03
graphics00	group	graphicsbank00		; Set group bank
		org 	graphicsbank00:$4000
this_bank	=	graphicsbank00
;***************************************************************************

csetbank00	equ	this_bank

normal_cset_tileset:
	libbin graphics\charsets\norm_charset_tileset.bin
	libbin graphics\charsets\lower_charset_tileset.bin
	libbin graphics\charsets\stink_charset_tileset.bin
	libbin graphics\charsets\slink_charset_tileset.bin
	libbin graphics\charsets\shadow_charset_tileset.bin
	libbin graphics\charsets\dither_charset_tileset.bin
last_cset_tileset:

;***************************************************************************

mapbank00 	equ this_bank

pattern_gbc_map:
	libbin graphics\maps\screen_pattern_attributes_x.bin
pattern_dmg_map:
	libbin graphics\maps\screen_pattern_x.bin

pattern_header_gbc_map:
	libbin graphics\maps\header_pattern_attributes_x.bin
pattern_header_dmg_map:
	libbin graphics\maps\header_pattern_x.bin

main_window_gbc_map:
	libbin graphics\maps\main_window_attributes_x.bin
main_window_dmg_map:
	libbin graphics\maps\main_window_x.bin

main_info_window_gbc_map:
	libbin graphics\maps\main_info_window_attributes_x.bin
main_info_window_dmg_map:
	libbin graphics\maps\main_info_window_x.bin

menu_header_gbc_map:
	libbin graphics\maps\header_menu_attributes_x.bin
menu_header_dmg_map:
	libbin graphics\maps\header_menu_x.bin

menu_screen_gbc_map:
	libbin graphics\maps\screen_menu_attributes_x.bin
menu_screen_dmg_map:
	libbin graphics\maps\screen_menu_x.bin

blank_menu_gbc_map:
	libbin graphics\maps\menu_blank_attributes_x.bin
blank_menu_dmg_map:
	libbin graphics\maps\menu_blank_x.bin

main_menu_gbc_map:
	libbin graphics\maps\menu_main_attributes_x.bin
main_menu_dmg_map:
	libbin graphics\maps\menu_main_x.bin

help_menu_gbc_map:
	libbin graphics\maps\menu_help_attributes_x.bin
help_menu_dmg_map:
	libbin graphics\maps\menu_help_x.bin

text_screen_gbc_map:
	libbin graphics\maps\screen_text_attributes_x.bin
text_screen_dmg_map:
	libbin graphics\maps\screen_text_x.bin

seq_effects_menu_gbc_map:
	libbin graphics\maps\menu_effects_seq_attributes_x.bin
seq_effects_menu_dmg_map:
	libbin graphics\maps\menu_effects_seq_x.bin

effects_menu_gbc_map:
	libbin graphics\maps\menu_effects_attributes_x.bin
effects_menu_dmg_map:
	libbin graphics\maps\menu_effects_x.bin

configuration_menu_gbc_map:
	libbin graphics\maps\menu_configuration_attributes_x.bin
configuration_menu_dmg_map:
	libbin graphics\maps\menu_configuration_x.bin

filename_menu_gbc_map:
	libbin graphics\maps\menu_filename_attributes_x.bin
filename_menu_dmg_map:
	libbin graphics\maps\menu_filename_x.bin

format_warning_gbc_map:
	libbin graphics\maps\format_warning_attributes_x.bin
format_warning_dmg_map:
	libbin graphics\maps\format_warning_x.bin

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

;***************************************************************************
graphicsbank01	equ	graphicsbank00+$01
graphics01	group	graphicsbank01		; Set group bank
		org 	graphicsbank01:$4000
this_bank	=	graphicsbank01
;***************************************************************************

plasmabank00 equ this_bank
plasma_sinus:
   db   32,  32,  33,  34,  35,  35,  36,  37
   db   38,  38,  39,  40,  41,  41,  42,  43
   db   44,  44,  45,  46,  46,  47,  48,  48
   db   49,  50,  50,  51,  51,  52,  53,  53
   db   54,  54,  55,  55,  56,  56,  57,  57
   db   58,  58,  58,  59,  59,  60,  60,  60
   db   61,  61,  61,  61,  62,  62,  62,  62
   db   62,  63,  63,  63,  63,  63,  63,  63
   db   63,  63,  63,  63,  63,  63,  63,  63
   db   62,  62,  62,  62,  62,  61,  61,  61
   db   61,  60,  60,  60,  59,  59,  59,  58
   db   58,  57,  57,  56,  56,  55,  55,  54
   db   54,  53,  53,  52,  52,  51,  50,  50
   db   49,  49,  48,  47,  47,  46,  45,  44
   db   44,  43,  42,  42,  41,  40,  39,  39
   db   38,  37,  36,  36,  35,  34,  33,  33
   db   32,  31,  30,  29,  29,  28,  27,  26
   db   26,  25,  24,  23,  23,  22,  21,  20
   db   20,  19,  18,  18,  17,  16,  16,  15
   db   14,  14,  13,  12,  12,  11,  11,  10
   db   9,  9,  8,  8,  7,  7,  6,  6
   db   6,  5,  5,  4,  4,  4,  3,  3
   db   3,  2,  2,  2,  1,  1,  1,  1
   db   1,  1,  0,  0,  0,  0,  0,  0
   db   0,  0,  0,  0,  0,  0,  0,  0
   db   1,  1,  1,  1,  1,  1,  2,  2
   db   2,  3,  3,  3,  4,  4,  4,  5
   db   5,  5,  6,  6,  7,  7,  8,  8
   db   9,  9,  10,  11,  11,  12,  12,  13
   db   14,  14,  15,  16,  16,  17,  18,  18
   db   19,  20,  20,  21,  22,  23,  23,  24
   db   25,  26,  26,  27,  28,  29,  29,  30

plasma_sinus_2:

   db   31,  33,  34, 36,  37,  38,  39,  41,  42,  43,  44
   db   46,  47,  48,  49,  50,  51,  52,  53
   db   54,  55,  56,  56,  57,  58,  59,  59
   db   60,  60,  61,  61,  62,  62,  62,  62
   db   63,  63,  63,  63,  63,  63,  63,  62
   db   62,  62,  62,  61,  61,  60,  60,  60
   db   59,  58,  58,  57,  56,  56,  55,  54
   db   54,  53,  52,  51,  50,  49,  49,  48
   db   47,  46,  45,  44,  43,  43,  42,  41
   db   40,  39,  38,  38,  37,  36,  35,  35
   db   34,  33,  33,  32,  32,  31,  30,  30
   db   30,  29,  29,  28,  28,  28,  28,  27
   db   27,  27,  27,  27,  27,  27,  27,  27
   db   27,  27,  27,  28,  28,  28,  28,  29
   db   29,  29,  30,  30,  30,  31,  31,  32
   db   32,  33,  33,  34,  34,  34,  35,  35
   db   36,  36,  37,  37,  37,  38,  38,  39
   db   39,  39,  40,  40,  40,  40,  40,  41
   db   41,  41,  41,  41,  41,  41,  41,  41
   db   41,  40,  40,  40,  40,  39,  39,  38
   db   38,  38,  37,  36,  36,  35,  35,  34
   db   33,  33,  32,  31,  30,  29,  28,  28
   db   27,  26,  25,  24,  23,  22,  21,  20
   db   19,  18,  17,  16,  16,  15,  14,  13
   db   12,  11,  10,  9,  9,  8,  7,  6
   db   6,  5,  4,  4,  3,  3,  2,  2
   db   1,  1,  1,  1,  0,  0,  0,  0
   db   0,  0,  0,  1,  1,  1,  1,  2
   db   2,  3,  3,  4,  4,  5,  6,  7
   db   7,  8,  9,  10,  11,  12,  13,  14
   db   15,  16,  18,  19,  20,  21,  22,  24
   db   25,  26,  27,  29,  30


plasma_zoomsin:
  db   217,  215,  213,  211,  209,  208,  206,  204
  db   202,  201,  199,  197,  196,  194,  192,  191
  db   189,  188,  186,  185,  183,  182,  180,  179
  db   177,  176,  175,  173,  172,  171,  170,  168
  db   167,  166,  165,  163,  162,  161,  160,  159
  db   158,  156,  155,  154,  153,  151,  150,  149
  db   147,  146,  145,  143,  142,  140,  139,  137
  db   135,  134,  132,  130,  128,  126,  124,  122
  db   120,  118,  116,  114,  111,  109,  107,  104
  db   102,  99,  97,  94,  92,  89,  86,  84
  db   81,  78,  75,  73,  70,  67,  64,  62
  db   59,  56,  53,  51,  48,  46,  43,  41
  db   38,  36,  34,  32,  30,  28,  26,  24
  db   22,  21,  19,  18,  17,  16,  15,  14
  db   13,  13,  12,  12,  12,  12,  12,  12
  db   13,  13,  14,  15,  16,  17,  18,  20
  db   21,  23,  25,  27,  29,  31,  33,  35
  db   37,  40,  42,  45,  48,  50,  53,  56
  db   59,  62,  64,  67,  70,  73,  76,  79
  db   81,  84,  87,  90,  92,  95,  97,  100
  db   102,  104,  106,  108,  110,  112,  114,  115
  db   117,  118,  120,  121,  122,  123,  123,  124
  db   124,  125,  125,  125,  125,  125,  125,  124
  db   124,  124,  123,  122,  121,  121,  120,  119
  db   117,  116,  115,  114,  113,  112,  110,  109
  db   108,  107,  105,  104,  103,  102,  101,  100
  db   99,  98,  98,  97,  97,  96,  96,  96
  db   96,  96,  96,  97,  97,  98,  99,  100
  db   101,  103,  104,  106,  108,  110,  112,  114
  db   117,  120,  123,  126,  129,  132,  136,  139
  db   143,  147,  151,  155,  159,  164,  168,  173
  db   177,  182,  187,  191,  196,  201,  206,  211

plasma_tileset:
	libbin graphics\plasma\plasma_1_tileset.bin
plasma_2_tileset:
	libbin graphics\plasma\plasma_2_tileset.bin

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

palettebank00 	equ this_bank

plasma_palette:
	libbin graphics\plasma\plasma_1_palette.bin
cursor_palette:
	libbin graphics\palettes\cursor_palette.bin
	libbin graphics\plasma\plasma_2_palette.bin
	libbin graphics\plasma\plasma_2_palette.bin
	libbin graphics\plasma\plasma_1_palette.bin

format_warning_palette:
	libbin graphics\palettes\format_warning_palette.bin

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

tilebank00 	equ this_bank
main_tileset:
	libbin graphics\tilesets\main_tileset.bin
format_warning_tileset:
	libbin graphics\tilesets\format_warning_tileset.bin

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

wave_tileset_bank	equ	this_bank
noise_wave_tileset:
	libbin graphics\tilesets\noise_tileset.bin
square_wave_tileset:
	libbin graphics\tilesets\square_tileset.bin
panning_tileset:
	include graphics\tilesets\panning_tileset.z80

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

insetbank00	equ this_bank

file_info_inset:
	libbin graphics\maps\inset_file_info_x.bin

optimize_info_inset:
	libbin graphics\maps\inset_optimize_x.bin

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

efxselbank00 	equ this_bank

effects_menu_1_gbc_attr:
	libbin graphics\maps\menu_effects_1_attributes_x.bin
effects_menu_2_gbc_attr:
	libbin graphics\maps\menu_effects_2_attributes_x.bin
effects_menu_3_gbc_attr:
	libbin graphics\maps\menu_effects_3_attributes_x.bin
effects_menu_4_gbc_attr:
	libbin graphics\maps\menu_effects_4_attributes_x.bin

efx_sel_pulse_info:
	libbin graphics\maps\effects_selections_pulse_x.bin
efx_sel_wave_info:
	libbin graphics\maps\effects_selections_wave_x.bin
efx_sel_volume_info:
	libbin graphics\maps\effects_selections_volume_x.bin
efx_sel_duration_info:
	libbin graphics\maps\effects_selections_duration_x.bin
efx_sel_panning_info:
	libbin graphics\maps\effects_selections_panning_x.bin
efx_sel_vibrato_info:
	libbin graphics\maps\effects_selections_vibrato_x.bin
efx_sel_portamento_info:
	libbin graphics\maps\effects_selections_portamento_x.bin
efx_sel_arpeggio_info:
	libbin graphics\maps\effects_selections_arpeggio_x.bin
efx_sel_sweep:
	libbin graphics\maps\effects_selections_sweep_x.bin
efx_sel_trigger_info:
	libbin graphics\maps\effects_selections_trigger_x.bin
efx_sel_speed_info:
	libbin graphics\maps\effects_selections_speed_x.bin
efx_sel_transpose_info:
	libbin graphics\maps\effects_selections_transpose_x.bin
efx_sel_sample_info:
	libbin graphics\maps\effects_selections_sample_x.bin
efx_sel_copy_paste_info:
	libbin graphics\maps\effects_selections_copy_paste_x.bin
efx_sel_song_loop_info:
	libbin graphics\maps\effects_selections_song_loop_x.bin

conf_colour_info:
	libbin graphics\maps\configuration_colours_x.bin
conf_preset_info:
	libbin graphics\maps\configuration_presets_x.bin
conf_charset_info:
	libbin graphics\maps\configuration_charsets_x.bin
conf_plasma_info:
	libbin graphics\maps\configuration_plasma_x.bin

filename_entry_info:
	libbin graphics\maps\filename_entries_x.bin

data_entry_save_reset_info:
	libbin graphics\maps\data_entry_save_reset_x.bin

;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;;-----;

;***************************************************************************
graphicsbank02	equ	graphicsbank00+$02
graphics02	group	graphicsbank02		; Set group bank
		org 	graphicsbank02:$4000
this_bank	=	graphicsbank02
;***************************************************************************

logobank00	equ this_bank

logo_tileset:
	libbin graphics\logo\image10cb_tileset.bin
logo_map_dmg:
	libbin graphics\logo\image10cb_x.bin
logo_palette:
	libbin graphics\logo\image10cb_palette.bin
logo_sprites:	
	libbin graphics\logo\logo_spr_tileset.bin

logo_sprite_table:

	db $40,$40,$00,$80
	db $50,$40,$02,$80
	db $60,$40,$04,$80
	db $70,$40,$06,$80
	db $40,$48,$08,$80
	db $50,$48,$0a,$80
	db $60,$48,$0c,$80
	db $70,$48,$0e,$80
	db $40,$50,$10,$80
	db $50,$50,$12,$80
	db $60,$50,$14,$80
	db $70,$50,$16,$80
	db $40,$58,$18,$80
	db $50,$58,$1a,$80
	db $60,$58,$1c,$80
	db $70,$58,$1e,$80
	db $40,$60,$20,$80
	db $50,$60,$22,$80
	db $60,$60,$24,$80
	db $70,$60,$26,$80
	db $40,$68,$28,$80
	db $50,$68,$2a,$80
	db $60,$68,$2c,$80
	db $70,$68,$2e,$80
	db $40,$70,$30,$80
	db $50,$70,$32,$80
	db $60,$70,$34,$80
	db $70,$70,$36,$80
	db $40,$78,$38,$80
	db $50,$78,$3a,$80
	db $60,$78,$3c,$80
	db $70,$78,$3e,$80
	db $40,$80,$40,$80
	db $50,$80,$42,$80
	db $60,$80,$44,$80
	db $70,$80,$46,$80
	db $40,$88,$48,$80
	db $50,$88,$4a,$80
	db $60,$88,$4c,$80
	db $70,$88,$4e,$80

	end
