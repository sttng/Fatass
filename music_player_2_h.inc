;***************************************************************************
; variables

;mus_song_number	equ mus_ram+$00	; $01 bytes ;##### required for standalone
;mus_song_bank		equ mus_ram+$01 ; $01 bytes ;##### required for standalone
mus_song_pat_address	equ mus_ram+$02 ; $02 bytes
mus_song_pat_counter	equ mus_ram+$04	; $02 bytes

mus_song_speed_counter 	equ mus_ram+$06	; $01 bytes
mus_song_speed	 	equ mus_ram+$07	; $01 bytes
mus_song_transpose	equ mus_ram+$08	; $01 bytes
mus_song_track		equ mus_ram+$09 ; $01 bytes
mus_song_shuffle	equ mus_ram+$0a	; $01 bytes
mus_song_shuffle_table	equ mus_ram+$0b	; $02 bytes
mus_song_shuffle_count  equ mus_ram+$0d ; $02 bytes
mus_song_flags		equ mus_ram+$0f	; $01 bytes
mus_sample_finished	equ mus_ram+$10	; $01 bytes
mus_temp		equ mus_ram+$11	; $01 bytes
mus_sample		equ mus_ram+$12	; $01 bytes

mus_waveform		equ mus_ram+$20 ; $10 bytes

mus_track_1		equ mus_ram+$30	; $34 bytes
mus_track_2		equ mus_ram+$64	; $34 bytes
mus_track_3		equ mus_ram+$98 ; $34 bytes
mus_track_4		equ mus_ram+$cc ; $34 bytes

o_seq_counter		equ $00	; $02 bytes ; must be first
o_play_note		equ $02 ; $02 bytes ; the current tone to play
o_dest_note		equ $04	; $02 bytes ; destination note for portamentos
o_source_note		equ $06	; $02 bytes ; buffer of source tone

o_panning		equ $08	; $01 bytes
o_pw_dur		equ $09	; $01 bytes
o_envelope		equ $0a	; $01 bytes
o_hold_mode		equ $0b ; $01 bytes
o_sweep			equ $0c ; $01 bytes

o_track_transpose	equ $0d	; $01 bytes ; must be in this order
o_source_transpose	equ $0e ; $01 bytes*

o_trigger_flag		equ $0f	; $01 bytes ; must be in this order
o_trigger_dly  		equ $10 ; $01 bytes
o_trigger_cnt  		equ $11 ; $01 bytes
o_retrig_dly		equ $12 ; $01 bytes

o_vibrato_table		equ $13 ; $02 bytes ; must be in this order
o_vibrato_rate		equ $15 ; $01 bytes

o_arpeggio		equ $16 ; $02 bytes
o_arpeggio_cnt		equ $18 ; $01 bytes ; must be in this order
o_arpeggio_dly		equ $19 ; $01 bytes ; must be in this order

o_porta_speed		equ $1a	; $01 bytes

o_fine_tune		equ $1b ; $01 bytes

o_auto_panner		equ $1c	; $01 bytes ; must be in this order
o_auto_panner_dly	equ $1d	; $01 bytes
o_auto_panner_cnt	equ $1e	; $01 bytes

o_auto_pwm		equ $1f	; $01 bytes ; must be in this order
o_auto_pwm_dly		equ $20	; $01 bytes
o_auto_pwm_cnt		equ $21	; $01 bytes

o_macro_return		equ $22 ; $02 bytes
o_wait_lines		equ $24	; $01 bytes

o_reg_flags		equ $27	; $01 bytes

;***************************************************************************
; flag defines
; song flags
mus_bit_pattern_end	equ 0
mus_flag_pattern_end	equ %00000001

; hold mode flags
mus_bit_glissando	equ 0
mus_bit_tone_mode	equ mus_bit_glissando	; noise mode has no glissando, might as well recycle
mus_bit_set_sample	equ 1
mus_bit_continuous	equ 6
mus_bit_set_initial	equ 7

mus_flag_glissando	equ %00000001
mus_flag_tone_mode	equ mus_flag_glissando
mus_flag_set_sample	equ %00000010
mus_flag_continuous	equ %01000000
mus_flag_set_initial	equ %10000000

; reg flags
mus_bit_set_note	equ 0
mus_bit_set_panning	equ 1
mus_bit_set_pw_dur	equ 2
mus_bit_set_envelope	equ 3
mus_bit_set_hold_mode	equ 4
mus_bit_set_sweep	equ 5
mus_bit_set_waveform	equ 5
mus_bit_set_transpose	equ 6
mus_bit_set_note_off	equ 7

mus_flag_set_note	equ %00000001
mus_flag_set_panning	equ %00000010
mus_flag_set_pw_dur	equ %00000100
mus_flag_set_envelope	equ %00001000
mus_flag_set_hold_mode	equ %00010000
mus_flag_set_sweep	equ %00100000
mus_flag_set_waveform	equ %00100000
mus_flag_set_transpose	equ %01000000
mus_flag_set_note_off	equ %10000000

;***************************************************************************
; controlbyte defines

; advance pointer effects 

ctrlbt_note_off		equ $00 ; note off + step
				;s$00

ctrlbt_wait		equ $80	; no action for this step *$80-$bf ->
				; and $3f = #of lines to wait
				;s$80


; pattern only effects

; definitely:

;filter sound 3 waveform - this is mostly done now.
;set other track panning

; likely:

;adsr envelope
;global volume
;force freq on chan 1,2,3
;randomizer
;tremolo
;strobe
;volume fade
;sequence loop to

; possibly:

;instruments?
;macrosounds?
;glissando_v
;delay
;sample offset?
;sample rate?
;pattern loop
;pattern delay
;hmm, should triggering samples take an event?
;cross modulation-one channel effects setting affects other channels

ctrlbt_macro		equ $d3 ; calls a macro.  DO NOT NEST MACROS!
				; a macro is basically a pattern.  you could even call
				; a pattern with this command.  however, if you call a
				; pattern that calls a macro, it will skip the macros in
				; that pattern.  what they are really useful for is for
				; setting up instruments that you have to set up a lot
				; or for little riffs that you repeat a lot, etc.
				; db $d3
				; dw xxxx-_pat
				; xxxx is the address of the macro to call
				; (xxxx-_pat = macro offset)

ctrlbt_auto_pwm		equ $d4 ; a very simple auto-pulse-width-modulator, very simple.
				; $d4 %aa_bb_cc_dd,xx
				; each effects tick, it steps through the chart
				; in the order aa,bb,cc,dd,aa,etc.
				; xx = speed, lower is faster, 0 is OFF

ctrlbt_auto_panner	equ $d5 ; a very simple auto-panner, very simple.
				; $d5 %aa_bb_cc_dd,xx
				; where 00 = off, 10 = left, 01 = right, 11 = center
				; each effects tick, it steps through the chart
				; in the order aa,bb,cc,dd,aa,etc.
				; xx = speed, lower is faster, 0 is OFF

ctrlbt_gain_waveform	equ $d6 ; QUICKLY applies a gain translation matrix to the waveform
				;3$d6 $ab = a = matrix($0-7 for now), b = gain ($0-f, 0 is silent, 8 is normal, f is full blast)
				; you can use this to create envelopes
				; heres how it is laid out, describing each transition from 0-8 and 8-f:
				; (x designates a fairly destructive matrix-one that will fuck the waveform up good, maybe put it out of tune)
				; also, most of these dont resemble the names too closely, but they do work
				; $00 nothing to normal, using smooth log matrix (ie, volume control in sound forge)
				; $08 normal to +6db using smooth log matrix  (ie, ultramaximizer, magneto)
				; $10 nothing to normal, using dirty triangular matrix (ie, sound forge dirty)
				;x$18 normal to fully clipping square using dirty triangular matrix (ie, sound forge dirty)
				; $20 nothing to normal, using smooth log+fuzz matrix (ie, sound forge fuzz)
				;x$28 normal to +6db using smooth log+fuzz matrix (ie, sound forge fuzz)
				;x$30 nothing to square wave, using square matrix (ie, sound forge hard clip)
				;x$38 square to more square, using square matrix (ie, sound forge hard clip)
				;x$40 sonic decimator type square waving (ie, bit depth reduction, lofi sims in vs880)
				;x$48 sonic decimator type square waving to square (ie, bit depth reduction, lofi sims in vs880)
				; $50 nothing to gated, using smooth log matrix (ie, sound forge compressor)
				; $58 gated to compressed (ie, sound forge hard knee compressor)
				;x$60 nothing to tube, using smooth log matrix (ie, similiar to wavewarm)
				;x$68 tube to +6db tube using smooth log matrix (ie, similiar to wavewarm)
				;x$70 nothing to noise distorted, using noise matrix (hmm, this really mangles things, it's something I made up)
				;x$78 noise distorted to various noise distortions, using noise matrix (hmm, this really mangles things, it's something I made up)
				;-there is room to add $80 more of these suckers, so I need some feedback as to
				; what works and what doesn't at some point.  we should be able to have a pretty good
				; variety - perhaps more manglers that don't affect the tune would be nice, or some
				; invertors that turn the waveform inside out.

ctrlbt_mix_waveform 	equ $d7 ; QUICKLY mixes the current waveform with another - this is fun!
				;3$d7 xx $ab - xx = waveform, ab = tonefactor!
				; -mixing waveforms uses the waveforms PAST the dest waveform if you pick a
				;  high tonefactor - this is a flaw in the algorithm, but makes things
				;  sound even more organic...
				;  it could cause problems with waveforms of greater#s, though... I have
				;  to decide what to do about it.
				;  for now I just waste $200 memory and copy part of the chart into the
				;  overflow area.
				;-It is recursive, IE, current waveform will be
				; the results of the last mix, unless you pick another waveform with
				; the ctrlbt_waveform command.  What this means is you can keep mixing the
				; same thing, but keep getting different results as the waveform morphs
				; from one to the other! - however, it's an averaging morph, so it will
				; start halfway there, and next time be halfway again (or 1/2+1/4+1/8, etc.)
				; THE OTHER REDICULOUS THING: the tonefactor - when you mix a new waveforms,
				; you can specify the rate of the second waveform to build chords or
				; even ringmod type things.  The range is pretty rediculous...
				; here's tonefactor, which is applied only to the new waveform:
				; $00 - -8 octaves, only the very first sample is applied. it's
				;			 basically silence, but makes shit crazy sometimes.
				; $01 - -4 octaves, but weird as it only has 1/16 a cycle
				; $02 - -3 octaves, but weird as it only has 1/8 a cycle
				; $04 - -2 octaves, but weird as it only has 1/4 a cycle
				; $08 - -1 octaves, but weird as it only has half a cycle
				; $10 - slight dither, no tone difference
				; $20 - +1 octaves
				; $40 - +2 octaves
				; $80 - +3 octaves
				; $ff - +4 octaves (actually just really close to 4 octaves, probably no
				;			noticeable difference)
				;-the only real bad thing is you can knock channel 3 out of tune with it
				; if you use a bunch of non-octave settings, which isn't a bad thing anyways.

ctrlbt_stop_sample	equ $d8	; stops the current sample
ctrlbt_vibrato_reset	equ $d9 ; resets the vibrato to be in tune
ctrlbt_waveform		equ $da ; followed by the index of the waveform
				;3$da xx - xx = waveform to load
ctrlbt_fine_tune	equ $db	; followed by the fine tune offset in 1/64th 1/2 steps (-127 to 128)
				;3$db xx - xx = fine tune offset
ctrlbt_sample		equ $dc	; followed by the index of the sample to trigger
				; $dc xx - xx = sample to trigger
ctrlbt_arpeggio		equ $dd ; arpeggio - this is a simple 4 step up only transpose arpeggio type effect * trigger delay is bypassed for this effect
				; $dd $ab $cd xx - 
				; transposes by one each time, in the order a b c d
				; 0 = off, 1-f Number of half-steps to transpose up
				; xx = rate, 0 = off
ctrlbt_glissando	equ $de ; sets glissando flag for portamentos
				; $de xx - xx = flag (0 = off, 1 = on)
ctrlbt_porta_from	equ $df ; portamento source note - * trigger delay is bypassed for this effect ?
				;3$df xx - xx = source_note
ctrlbt_portamento	equ $e0 ; portamento - track 1 and 2 only * trigger delay is bypassed for this effect
				; $e0 xx - xx = speed (bigger=faster), xx = 00 = off
ctrlbt_vibrato		equ $e1 ; vibrato - sets depth and speed- track 1 and 2 only
				; $e1 xx yy - xx = depth ($00-$1f), b = rate ($00-$7f) ($00=off)
ctrlbt_restart_envelope	equ $e2 ; turns the track on and restarts the envelope (sets initial)
				; $e2
ctrlbt_retrigger	equ $e3 ; retrigger - sets the delay for retriggering notes in ticks ($00 is off)
				; $e3 xx - xx = retrigger delay in ticks, $00 is default
ctrlbt_trigger_delay	equ $e4 ; trigger delay - sets the delay for triggering notes in ticks ($00 is no delay)
				; $e4 xx - xx = trigger delay in ticks, $00 is default
ctrlbt_hold_mode	equ $e5 ; set hold mode, 00 = on, 01 = off
				; $e5 xx
ctrlbt_sweep		equ $e6 ; sets the sweep (auto-slide) - track 1 only 
				;1$e6 xx - set track 1 "sweep" - (%0_xxx_y_zzz)
				;		this effect is not very musical, as it works on 
				;		frequencies, and not notes.
				;	   xxx 	= frequency, higher is slower, 0 is off (%000-%111)
				;	   y	= direction (0 = bend up, 1 = down)
				;	   zzz	= depth, lower is more (%000-%111)
ctrlbt_volume		equ $e7 ; sets the inital volume of the envelope (0-15)
				; $e7 xx - set track volume - this sets the initial volume of the envelope
				;	   xx = initial volume (higher is louder) (0-15)
				; **** special for track 3 ($00 - off, $20 - max, $40 - 1/2, $60 - 1/4)
ctrlbt_envelope		equ $e8 ; sets the envelope
				; $e8 xx - set track envelope - this one is weird (%0000_y_zzz)
				;	   y 	= direction (0 = down/decay, 1 = up/attack)
				;	   zzz  = speed of attack/decay (lower is quicker, 0 is off) (%000-%111)
ctrlbt_duration		equ $e9 ; sets the duration
				; $e9 xx - set track duration/sustain (0-63) (lower is longer - (64-t1)x(1/256)sec)
				; **** special for track 3 - (0-255) (lower is longer - (256-t1)x(1/256)sec)
ctrlbt_pulse_width	equ $ea	; sets the pulse-width - track 1 and 2 only
				;2$ea %xx_000000 - set track pulse width (0-3) (12.5%, 25%, 50%, 75%)
; sequence or pattern effects

ctrlbt_panning		equ $fb ; sets the track panning
				; $fb %dr_cr_br_ar_dl_cl_bl_al - set track panning -:
				; a = track 1, b = track 2, c = track 3, d = track 4
				; l is 1, left is on, r is 1, right is on
				; it's done this way so that you can reuse the same track in a 
				; different channel and get a different panning without changing
				; the command.  it's also sort of stupid, but that's ok.
ctrlbt_shuffle		equ $fc	; sets the song/track shuffle
				; $fc $xx $ab - xx = phase ($00-$ff) a = depth in ticks, b = rate in lines
ctrlbt_transpose	equ $fd ; sets the song/track transpose - track 1 and 2 only?
				;3$fd xx - set track transpose to xx 1/2 steps (-127 to 128)
ctrlbt_song_speed	equ $fe	; sets the song speed
				; $fe xx - change tempo (1-255) - controls tempo of all tracks
ctrlbt_track_loop	equ $ff	; marks the end of the track
				; $ff

first_seq_effect 	equ ctrlbt_panning

	; NOTE Oc3 Oc4 Oc5 Oc6 Oc7 Oc8
	;   c  $01 $0d $19 $25 $31 $3d
	;   c# $02 $0e $1a $26 $32 $3e
	;   d  $03 $0f $1b $27 $33 $3f
	;   d# $04 $10 $1c $28 $34 $40
	;   e  $05 $11 $1d $29 $35 $41
	;   f  $06 $12 $1e $2a $36 $42
	;   f# $07 $13 $1f $2b $37 $43
	;   g  $08 $14 $20 $2c $38 $44
	;   g# $09 $15 $21 $2d $39 $45
	;   a  $0a $16 $22 $2e $3a $46
	;   a# $0b $17 $23 $2f $3b $47
	;   b  $0c $18 $24 $30 $3c $48

ctrlbt_c_3 equ $01
ctrlbt_c#3 equ $02
ctrlbt_d_3 equ $03
ctrlbt_d#3 equ $04
ctrlbt_e_3 equ $05
ctrlbt_f_3 equ $06
ctrlbt_f#3 equ $07
ctrlbt_g_3 equ $08
ctrlbt_g#3 equ $09
ctrlbt_a_3 equ $0a
ctrlbt_a#3 equ $0b
ctrlbt_b_3 equ $0c
ctrlbt_c_4 equ $0d
ctrlbt_c#4 equ $0e
ctrlbt_d_4 equ $0f
ctrlbt_d#4 equ $10
ctrlbt_e_4 equ $11
ctrlbt_f_4 equ $12
ctrlbt_f#4 equ $13
ctrlbt_g_4 equ $14
ctrlbt_g#4 equ $15
ctrlbt_a_4 equ $16
ctrlbt_a#4 equ $17
ctrlbt_b_4 equ $18
ctrlbt_c_5 equ $19
ctrlbt_c#5 equ $1a
ctrlbt_d_5 equ $1b
ctrlbt_d#5 equ $1c
ctrlbt_e_5 equ $1d
ctrlbt_f_5 equ $1e
ctrlbt_f#5 equ $1f
ctrlbt_g_5 equ $20
ctrlbt_g#5 equ $21
ctrlbt_a_5 equ $22
ctrlbt_a#5 equ $23
ctrlbt_b_5 equ $24
ctrlbt_c_6 equ $25
ctrlbt_c#6 equ $26
ctrlbt_d_6 equ $27
ctrlbt_d#6 equ $28
ctrlbt_e_6 equ $29
ctrlbt_f_6 equ $2a
ctrlbt_f#6 equ $2b
ctrlbt_g_6 equ $2c
ctrlbt_g#6 equ $2d
ctrlbt_a_6 equ $2e
ctrlbt_a#6 equ $2f
ctrlbt_b_6 equ $30
ctrlbt_c_7 equ $31
ctrlbt_c#7 equ $32
ctrlbt_d_7 equ $33
ctrlbt_d#7 equ $34
ctrlbt_e_7 equ $35
ctrlbt_f_7 equ $36
ctrlbt_f#7 equ $37
ctrlbt_g_7 equ $38
ctrlbt_g#7 equ $39
ctrlbt_a_7 equ $3a
ctrlbt_a#7 equ $3b
ctrlbt_b_7 equ $3c
ctrlbt_c_8 equ $3d
ctrlbt_c#8 equ $3e
ctrlbt_d_8 equ $3f
ctrlbt_d#8 equ $40
ctrlbt_e_8 equ $41
ctrlbt_f_8 equ $42
ctrlbt_f#8 equ $43
ctrlbt_g_8 equ $44
ctrlbt_g#8 equ $45
ctrlbt_a_8 equ $46
ctrlbt_a#8 equ $47
ctrlbt_b_8 equ $48

ctrlbt_wait_1		equ $80
ctrlbt_wait_2		equ $81
ctrlbt_wait_3		equ $82
ctrlbt_wait_4		equ $83
ctrlbt_wait_5		equ $84
ctrlbt_wait_6		equ $85
ctrlbt_wait_7		equ $86
ctrlbt_wait_8		equ $87
ctrlbt_wait_9		equ $88
ctrlbt_wait_10		equ $89
ctrlbt_wait_11		equ $8a
ctrlbt_wait_12		equ $8b
ctrlbt_wait_13		equ $8c
ctrlbt_wait_14		equ $8d
ctrlbt_wait_15		equ $8e
ctrlbt_wait_16		equ $8f
ctrlbt_wait_17		equ $90
ctrlbt_wait_18		equ $91
ctrlbt_wait_19		equ $92
ctrlbt_wait_20		equ $93
ctrlbt_wait_21		equ $94
ctrlbt_wait_22		equ $95
ctrlbt_wait_23		equ $96
ctrlbt_wait_24		equ $97
ctrlbt_wait_25		equ $98
ctrlbt_wait_26		equ $99
ctrlbt_wait_27		equ $9a
ctrlbt_wait_28		equ $9b
ctrlbt_wait_29		equ $9c
ctrlbt_wait_30		equ $9d
ctrlbt_wait_31		equ $9e
ctrlbt_wait_32		equ $9f
ctrlbt_wait_33		equ $a0
ctrlbt_wait_34		equ $a1
ctrlbt_wait_35		equ $a2
ctrlbt_wait_36		equ $a3
ctrlbt_wait_37		equ $a4
ctrlbt_wait_38		equ $a5
ctrlbt_wait_39		equ $a6
ctrlbt_wait_40		equ $a7
ctrlbt_wait_41		equ $a8
ctrlbt_wait_42		equ $a9
ctrlbt_wait_43		equ $aa
ctrlbt_wait_44		equ $ab
ctrlbt_wait_45		equ $ac
ctrlbt_wait_46		equ $ad
ctrlbt_wait_47		equ $ae
ctrlbt_wait_48		equ $af
ctrlbt_wait_49		equ $b0
ctrlbt_wait_50		equ $b1
ctrlbt_wait_51		equ $b2
ctrlbt_wait_52		equ $b3
ctrlbt_wait_53		equ $b4
ctrlbt_wait_54		equ $b5
ctrlbt_wait_55		equ $b6
ctrlbt_wait_56		equ $b7
ctrlbt_wait_57		equ $b8
ctrlbt_wait_58		equ $b9
ctrlbt_wait_59		equ $ba
ctrlbt_wait_60		equ $bb
ctrlbt_wait_61		equ $bc
ctrlbt_wait_62		equ $bd
ctrlbt_wait_63		equ $be
ctrlbt_wait_64		equ $bf

;***************************************************************************

music_routines_table:
;	dw mus_set_note_off,mus_nop,mus_nop,mus_nop	; $80-$83
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $84-$87
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $88-$8b
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $8c-$8f
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $90-$93
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $94-$97
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $98-$9b
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $9c-$9f
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $a0-$a3
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $a4-$a7
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $a8-$ab
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $ac-$af
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $b0-$b3
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $b4-$b7
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $b8-$bb
;	dw mus_nop,mus_nop,mus_nop,mus_nop	; $bc-$bf

	dw mus_nop,mus_nop,mus_nop,mus_nop	; $c0-$c3
	dw mus_nop,mus_nop,mus_nop,mus_nop	; $c4-$c7
	dw mus_nop,mus_nop,mus_nop,mus_nop	; $c8-$cb
	dw mus_nop,mus_nop,mus_nop,mus_nop	; $cc-$cf
	dw mus_nop,mus_nop,mus_nop,mus_macro	; $d0-$d3
	dw mus_auto_pwm,mus_auto_panner,mus_gain_waveform,mus_mix_waveform	; $d4-$d7
	dw mus_stop_sample,mus_vibrato_reset,mus_set_waveform,mus_set_fine_tune	; $d8-$db
	dw mus_trigger_sample,mus_set_arpeggio,mus_set_glissando,mus_set_porta_from	; $dc-$df
	dw mus_set_portamento,mus_set_vibrato,mus_set_restart_envelope,mus_set_retrigger ; $e0-$e3
	dw mus_set_trigger_delay,mus_set_hold_mode,mus_set_sweep,mus_set_volume	; $e4-$e7
	dw mus_set_envelope,mus_set_duration,mus_set_pulse_width,mus_nop ; $e8-$eb
	dw mus_nop,mus_nop,mus_nop,mus_nop	; $ec-$ef
	dw mus_nop,mus_nop,mus_nop,mus_nop	; $f0-$f3
	dw mus_nop,mus_nop,mus_nop,mus_nop	; $f4-$f7
	dw mus_nop,mus_nop,mus_nop,mus_set_panning	; $f8-$fb
	dw mus_set_shuffle,mus_set_track_transpose,mus_set_song_speed,mus_track_loop	; $fc-$ff

	end
