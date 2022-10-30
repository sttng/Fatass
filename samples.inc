;---------------------------------------------------------------------------
; samples
; samples cannot cross banks
; samples must be truncated to 16byte lengths-gameman does this for you.
;---------------------------------------------------------------------------
;$4000-$7fff Bank $0c4-$0eb  40 banks  (640k) : Sample Data - 4 seconds of data per bank
;						this should be enough for
;						160 seconds of samples
;---------------------------------------------------------------------------

;***************************************************************************
;***************************************************************************
samplebank00	equ 	$08
sample00	group	samplebank00		; set group bank
		org 	samplebank00:$4000
this_bank	=	samplebank00
;***************************************************************************

sample_bank_table:

	db kick_00_sample_bank
	db kick_01_sample_bank
	db kick_02_sample_bank
	db kick_03_sample_bank
	db kick_04_sample_bank
	db kick_05_sample_bank
	db kick_06_sample_bank
	db kick_07_sample_bank
	db kick_08_sample_bank
	db kick_09_sample_bank
	db kick_0a_sample_bank
	db kick_0b_sample_bank
	db kick_0c_sample_bank
	db kick_0f_sample_bank;kick_0d_sample_bank
	db kick_0f_sample_bank;kick_0e_sample_bank
	db kick_0f_sample_bank

	db snare_00_sample_bank
	db snare_01_sample_bank
	db snare_02_sample_bank
	db snare_03_sample_bank
	db snare_04_sample_bank
	db snare_05_sample_bank
	db snare_06_sample_bank
	db snare_07_sample_bank
	db snare_08_sample_bank
	db snare_09_sample_bank
	db snare_0a_sample_bank
	db snare_0b_sample_bank
	db snare_0f_sample_bank;snare_0c_sample_bank
	db snare_0f_sample_bank;snare_0d_sample_bank
	db snare_0f_sample_bank;snare_0e_sample_bank
	db snare_0f_sample_bank

	db hat_00_sample_bank
	db hat_01_sample_bank
	db hat_02_sample_bank
	db hat_03_sample_bank
	db hat_04_sample_bank
	db hat_05_sample_bank
	db hat_06_sample_bank
	db hat_07_sample_bank
	db hat_08_sample_bank
	db hat_09_sample_bank
	db hat_0a_sample_bank
	db hat_0b_sample_bank
	db hat_0c_sample_bank
	db hat_0d_sample_bank
	db hat_0e_sample_bank
	db hat_0e_sample_bank;hat_0f_sample_bank

	db ride_00_sample_bank
	db ride_00_sample_bank;ride_01_sample_bank
	db ride_00_sample_bank;ride_02_sample_bank
	db ride_00_sample_bank;ride_03_sample_bank
	db ride_00_sample_bank;ride_04_sample_bank
	db ride_00_sample_bank;ride_05_sample_bank
	db ride_00_sample_bank;ride_06_sample_bank
	db ride_00_sample_bank;ride_07_sample_bank
	db ride_00_sample_bank;ride_08_sample_bank
	db ride_00_sample_bank;ride_09_sample_bank
	db ride_00_sample_bank;ride_0a_sample_bank
	db ride_00_sample_bank;ride_0b_sample_bank
	db ride_00_sample_bank;ride_0c_sample_bank
	db ride_00_sample_bank;ride_0d_sample_bank
	db ride_00_sample_bank;ride_0e_sample_bank
	db ride_00_sample_bank;ride_0f_sample_bank

	db crash_00_sample_bank
	db crash_01_sample_bank
	db crash_01_sample_bank;crash_02_sample_bank
	db crash_01_sample_bank;crash_03_sample_bank
	db crash_01_sample_bank;crash_04_sample_bank
	db crash_01_sample_bank;crash_05_sample_bank
	db crash_01_sample_bank;crash_06_sample_bank
	db crash_01_sample_bank;crash_07_sample_bank
	db crash_01_sample_bank;crash_08_sample_bank
	db crash_01_sample_bank;crash_09_sample_bank
	db crash_01_sample_bank;crash_0a_sample_bank
	db crash_01_sample_bank;crash_0b_sample_bank
	db crash_01_sample_bank;crash_0c_sample_bank
	db crash_01_sample_bank;crash_0d_sample_bank
	db crash_01_sample_bank;crash_0e_sample_bank
	db crash_01_sample_bank;crash_0f_sample_bank

	db tom_00_sample_bank
	db tom_01_sample_bank
	db tom_0d_sample_bank;tom_02_sample_bank
	db tom_0d_sample_bank;tom_03_sample_bank
	db tom_0d_sample_bank;tom_04_sample_bank
	db tom_0d_sample_bank;tom_05_sample_bank
	db tom_0d_sample_bank;tom_06_sample_bank
	db tom_0d_sample_bank;tom_07_sample_bank
	db tom_0d_sample_bank;tom_08_sample_bank
	db tom_0d_sample_bank;tom_09_sample_bank
	db tom_0d_sample_bank;tom_0a_sample_bank
	db tom_0d_sample_bank;tom_0b_sample_bank
	db tom_0d_sample_bank;tom_0c_sample_bank
	db tom_0d_sample_bank
	db tom_0e_sample_bank
	db tom_0f_sample_bank

	db scratch_00_sample_bank
	db scratch_01_sample_bank
	db scratch_02_sample_bank
	db scratch_03_sample_bank
	db scratch_04_sample_bank
	db scratch_05_sample_bank
	db scratch_06_sample_bank
	db scratch_07_sample_bank
	db scratch_08_sample_bank
	db scratch_09_sample_bank
	db scratch_0a_sample_bank
	db scratch_0a_sample_bank;scratch_0b_sample_bank
	db scratch_0a_sample_bank;scratch_0c_sample_bank
	db scratch_0a_sample_bank;scratch_0d_sample_bank
	db scratch_0a_sample_bank;scratch_0e_sample_bank
	db scratch_0a_sample_bank;scratch_0f_sample_bank

sample_start_table:

	dw kick_00_sample
	dw kick_01_sample
	dw kick_02_sample
	dw kick_03_sample
	dw kick_04_sample
	dw kick_05_sample
	dw kick_06_sample
	dw kick_07_sample
	dw kick_08_sample
	dw kick_09_sample
	dw kick_0a_sample
	dw kick_0b_sample
	dw kick_0c_sample
	dw kick_0f_sample;kick_0d_sample
	dw kick_0f_sample;kick_0e_sample
	dw kick_0f_sample

	dw snare_00_sample
	dw snare_01_sample
	dw snare_02_sample
	dw snare_03_sample
	dw snare_04_sample
	dw snare_05_sample
	dw snare_06_sample
	dw snare_07_sample
	dw snare_08_sample
	dw snare_09_sample
	dw snare_0a_sample
	dw snare_0b_sample
	dw snare_0f_sample;snare_0c_sample
	dw snare_0f_sample;snare_0d_sample
	dw snare_0f_sample;snare_0e_sample
	dw snare_0f_sample

	dw hat_00_sample
	dw hat_01_sample
	dw hat_02_sample
	dw hat_03_sample
	dw hat_04_sample
	dw hat_05_sample
	dw hat_06_sample
	dw hat_07_sample
	dw hat_08_sample
	dw hat_09_sample
	dw hat_0a_sample
	dw hat_0b_sample
	dw hat_0c_sample
	dw hat_0d_sample
	dw hat_0e_sample
	dw hat_0e_sample;hat_0f_sample

	dw ride_00_sample
	dw ride_00_sample;ride_01_sample
	dw ride_00_sample;ride_02_sample
	dw ride_00_sample;ride_03_sample
	dw ride_00_sample;ride_04_sample
	dw ride_00_sample;ride_05_sample
	dw ride_00_sample;ride_06_sample
	dw ride_00_sample;ride_07_sample
	dw ride_00_sample;ride_08_sample
	dw ride_00_sample;ride_09_sample
	dw ride_00_sample;ride_0a_sample
	dw ride_00_sample;ride_0b_sample
	dw ride_00_sample;ride_0c_sample
	dw ride_00_sample;ride_0d_sample
	dw ride_00_sample;ride_0e_sample
	dw ride_00_sample;ride_0f_sample

	dw crash_00_sample
	dw crash_01_sample
	dw crash_01_sample;crash_02_sample
	dw crash_01_sample;crash_03_sample
	dw crash_01_sample;crash_04_sample
	dw crash_01_sample;crash_05_sample
	dw crash_01_sample;crash_06_sample
	dw crash_01_sample;crash_07_sample
	dw crash_01_sample;crash_08_sample
	dw crash_01_sample;crash_09_sample
	dw crash_01_sample;crash_0a_sample
	dw crash_01_sample;crash_0b_sample
	dw crash_01_sample;crash_0c_sample
	dw crash_01_sample;crash_0d_sample
	dw crash_01_sample;crash_0e_sample
	dw crash_01_sample;crash_0f_sample

	dw tom_00_sample
	dw tom_01_sample
	dw tom_0d_sample;tom_02_sample
	dw tom_0d_sample;tom_03_sample
	dw tom_0d_sample;tom_04_sample
	dw tom_0d_sample;tom_05_sample
	dw tom_0d_sample;tom_06_sample
	dw tom_0d_sample;tom_07_sample
	dw tom_0d_sample;tom_08_sample
	dw tom_0d_sample;tom_09_sample
	dw tom_0d_sample;tom_0a_sample
	dw tom_0d_sample;tom_0b_sample
	dw tom_0d_sample;tom_0c_sample
	dw tom_0d_sample
	dw tom_0e_sample
	dw tom_0f_sample

	dw scratch_00_sample
	dw scratch_01_sample
	dw scratch_02_sample
	dw scratch_03_sample
	dw scratch_04_sample
	dw scratch_05_sample
	dw scratch_06_sample
	dw scratch_07_sample
	dw scratch_08_sample
	dw scratch_09_sample
	dw scratch_0a_sample
	dw scratch_0a_sample;scratch_0b_sample
	dw scratch_0a_sample;scratch_0c_sample
	dw scratch_0a_sample;scratch_0d_sample
	dw scratch_0a_sample;scratch_0e_sample
	dw scratch_0a_sample;scratch_0f_sample

sample_length_table:

	dw kick_00_sample_end
	dw kick_01_sample_end
	dw kick_02_sample_end
	dw kick_03_sample_end
	dw kick_04_sample_end
	dw kick_05_sample_end
	dw kick_06_sample_end
	dw kick_07_sample_end
	dw kick_08_sample_end
	dw kick_09_sample_end
	dw kick_0a_sample_end
	dw kick_0b_sample_end
	dw kick_0c_sample_end
	dw kick_0f_sample_end;kick_0d_sample_end
	dw kick_0f_sample_end;kick_0e_sample_end
	dw kick_0f_sample_end

	dw snare_00_sample_end
	dw snare_01_sample_end
	dw snare_02_sample_end
	dw snare_03_sample_end
	dw snare_04_sample_end
	dw snare_05_sample_end
	dw snare_06_sample_end
	dw snare_07_sample_end
	dw snare_08_sample_end
	dw snare_09_sample_end
	dw snare_0a_sample_end
	dw snare_0b_sample_end
	dw snare_0f_sample_end;snare_0c_sample_end
	dw snare_0f_sample_end;snare_0d_sample_end
	dw snare_0f_sample_end;snare_0e_sample_end
	dw snare_0f_sample_end

	dw hat_00_sample_end
	dw hat_01_sample_end
	dw hat_02_sample_end
	dw hat_03_sample_end
	dw hat_04_sample_end
	dw hat_05_sample_end
	dw hat_06_sample_end
	dw hat_07_sample_end
	dw hat_08_sample_end
	dw hat_09_sample_end
	dw hat_0a_sample_end
	dw hat_0b_sample_end
	dw hat_0c_sample_end
	dw hat_0d_sample_end
	dw hat_0e_sample_end
	dw hat_0e_sample_end;hat_0f_sample_end

	dw ride_00_sample_end
	dw ride_00_sample_end;ride_01_sample_end
	dw ride_00_sample_end;ride_02_sample_end
	dw ride_00_sample_end;ride_03_sample_end
	dw ride_00_sample_end;ride_04_sample_end
	dw ride_00_sample_end;ride_05_sample_end
	dw ride_00_sample_end;ride_06_sample_end
	dw ride_00_sample_end;ride_07_sample_end
	dw ride_00_sample_end;ride_08_sample_end
	dw ride_00_sample_end;ride_09_sample_end
	dw ride_00_sample_end;ride_0a_sample_end
	dw ride_00_sample_end;ride_0b_sample_end
	dw ride_00_sample_end;ride_0c_sample_end
	dw ride_00_sample_end;ride_0d_sample_end
	dw ride_00_sample_end;ride_0e_sample_end
	dw ride_00_sample_end;ride_0f_sample_end

	dw crash_00_sample_end
	dw crash_01_sample_end
	dw crash_01_sample_end;crash_02_sample_end
	dw crash_01_sample_end;crash_03_sample_end
	dw crash_01_sample_end;crash_04_sample_end
	dw crash_01_sample_end;crash_05_sample_end
	dw crash_01_sample_end;crash_06_sample_end
	dw crash_01_sample_end;crash_07_sample_end
	dw crash_01_sample_end;crash_08_sample_end
	dw crash_01_sample_end;crash_09_sample_end
	dw crash_01_sample_end;crash_0a_sample_end
	dw crash_01_sample_end;crash_0b_sample_end
	dw crash_01_sample_end;crash_0c_sample_end
	dw crash_01_sample_end;crash_0d_sample_end
	dw crash_01_sample_end;crash_0e_sample_end
	dw crash_01_sample_end;crash_0f_sample_end

	dw tom_00_sample_end
	dw tom_01_sample_end
	dw tom_0d_sample_end;tom_02_sample_end
	dw tom_0d_sample_end;tom_03_sample_end
	dw tom_0d_sample_end;tom_04_sample_end
	dw tom_0d_sample_end;tom_05_sample_end
	dw tom_0d_sample_end;tom_06_sample_end
	dw tom_0d_sample_end;tom_07_sample_end
	dw tom_0d_sample_end;tom_08_sample_end
	dw tom_0d_sample_end;tom_09_sample_end
	dw tom_0d_sample_end;tom_0a_sample_end
	dw tom_0d_sample_end;tom_0b_sample_end
	dw tom_0d_sample_end;tom_0c_sample_end
	dw tom_0d_sample_end
	dw tom_0e_sample_end
	dw tom_0f_sample_end

	dw scratch_00_sample_end
	dw scratch_01_sample_end
	dw scratch_02_sample_end
	dw scratch_03_sample_end
	dw scratch_04_sample_end
	dw scratch_05_sample_end
	dw scratch_06_sample_end
	dw scratch_07_sample_end
	dw scratch_08_sample_end
	dw scratch_09_sample_end
	dw scratch_0a_sample_end
	dw scratch_0a_sample_end;scratch_0b_sample_end
	dw scratch_0a_sample_end;scratch_0c_sample_end
	dw scratch_0a_sample_end;scratch_0d_sample_end
	dw scratch_0a_sample_end;scratch_0e_sample_end
	dw scratch_0a_sample_end;scratch_0f_sample_end

;***************************************************************************
; samples
;***************************************************************************

kick_02_sample_bank	equ this_bank
kick_02_sample:
	libbin samples\Kick02_sample.bin ; $0400
kick_02_sample_end:

kick_05_sample_bank	equ this_bank
kick_05_sample:
	libbin samples\Kick05_sample.bin ; $02f0
kick_05_sample_end:

kick_08_sample_bank	equ this_bank
kick_08_sample:
	libbin samples\Kick08_sample.bin ; $0320
kick_08_sample_end:

snare_01_sample_bank	equ this_bank
snare_01_sample:
	libbin samples\Snare01_sample.bin ; $0340
snare_01_sample_end:

snare_03_sample_bank	equ this_bank
snare_03_sample:
	libbin samples\Snare03_sample.bin ; $0370
snare_03_sample_end:

snare_06_sample_bank	equ this_bank
snare_06_sample:
	libbin samples\Snare06_sample.bin ; $0320
snare_06_sample_end:

snare_09_sample_bank	equ this_bank
snare_09_sample:
	libbin samples\Snare09_sample.bin ; $0240
snare_09_sample_end:

snare_0b_sample_bank	equ this_bank
snare_0b_sample:
	libbin samples\Snare0b_sample.bin ; $0340
snare_0b_sample_end:

hat_00_sample_bank	equ this_bank
hat_00_sample:
	libbin samples\Hat00_sample.bin ; $03b0
hat_00_sample_end:

hat_01_sample_bank	equ this_bank
hat_01_sample:
	libbin samples\Hat01_sample.bin ; $0360
hat_01_sample_end:

hat_05_sample_bank	equ this_bank
hat_05_sample:
	libbin samples\Hat05_sample.bin ; $0150
hat_05_sample_end:

hat_06_sample_bank	equ this_bank
hat_06_sample:
	libbin samples\Hat06_sample.bin ; $0290
hat_06_sample_end:

hat_07_sample_bank	equ this_bank
hat_07_sample:
	libbin samples\Hat07_sample.bin ; $03e0
hat_07_sample_end:

hat_08_sample_bank	equ this_bank
hat_08_sample:
	libbin samples\Hat08_sample.bin ; $0190
hat_08_sample_end:

hat_0a_sample_bank	equ this_bank
hat_0a_sample:
	libbin samples\Hat0a_sample.bin ; $0150
hat_0a_sample_end:

hat_0b_sample_bank	equ this_bank
hat_0b_sample:
	libbin samples\Hat0b_sample.bin ; $0230
hat_0b_sample_end:

hat_0c_sample_bank	equ this_bank
hat_0c_sample:
	libbin samples\Hat0c_sample.bin ; $0300
hat_0c_sample_end:

hat_0e_sample_bank	equ this_bank
hat_0e_sample:
	libbin samples\Hat0e_sample.bin ; $02b0
hat_0e_sample_end:

tom_01_sample_bank	equ this_bank
tom_01_sample:
	libbin samples\Dunno01_sample.bin ; $0330
tom_01_sample_end:

;***************************************************************************
samplebank01	equ 	samplebank00+$01
sample01	group	samplebank01		; set group bank
		org 	samplebank01:$4000
this_bank	=	samplebank01
;***************************************************************************

crash_00_sample_bank	equ this_bank
crash_00_sample:
	libbin samples\Crash00_sample.bin ; $1ef0
crash_00_sample_end:

scratch_03_sample_bank	equ this_bank
scratch_03_sample:
	libbin samples\Scratch03_sample.bin ; $0db0
scratch_03_sample_end:

scratch_06_sample_bank	equ this_bank
scratch_06_sample:
	libbin samples\Scratch06_sample.bin ; $0db0
scratch_06_sample_end:

scratch_08_sample_bank	equ this_bank
scratch_08_sample:
	libbin samples\Scratch08_sample.bin ; $05b0
scratch_08_sample_end:

;***************************************************************************
samplebank02	equ 	samplebank00+$02
sample02	group	samplebank02		; set group bank
		org 	samplebank02:$4000
this_bank	=	samplebank02
;***************************************************************************

hat_0d_sample_bank	equ this_bank
hat_0d_sample:
	libbin samples\Hat0d_sample.bin ; $02c0
hat_0d_sample_end:

snare_00_sample_bank	equ this_bank
snare_00_sample:
	libbin samples\Snare00_sample.bin ; $0ac0
snare_00_sample_end:

tom_0d_sample_bank	equ this_bank
tom_0d_sample:
	libbin samples\Tom0d_sample.bin ; $0b10
tom_0d_sample_end:

tom_0e_sample_bank	equ this_bank
tom_0e_sample:
	libbin samples\Tom0e_sample.bin ; $0c80
tom_0e_sample_end:

tom_0f_sample_bank	equ this_bank
tom_0f_sample:
	libbin samples\Tom0f_sample.bin ; $0d90
tom_0f_sample_end:

scratch_00_sample_bank	equ this_bank
scratch_00_sample:
	libbin samples\Scratch00_sample.bin ; $0d50
scratch_00_sample_end:

;***************************************************************************
samplebank03	equ 	samplebank00+$03
sample03	group	samplebank03		; set group bank
		org 	samplebank03:$4000
this_bank	=	samplebank03
;***************************************************************************

kick_00_sample_bank	equ this_bank
kick_00_sample:
	libbin samples\Kick00_sample.bin ; $07a0
kick_00_sample_end:

kick_01_sample_bank	equ this_bank
kick_01_sample:
	libbin samples\Kick01_sample.bin ; $0540
kick_01_sample_end:

snare_04_sample_bank	equ this_bank
snare_04_sample:
	libbin samples\Snare04_sample.bin ; $07f0
snare_04_sample_end:

snare_08_sample_bank	equ this_bank
snare_08_sample:
	libbin samples\Snare08_sample.bin ; $0790
snare_08_sample_end:

snare_0f_sample_bank	equ this_bank
snare_0f_sample:
	libbin samples\Snare0f_sample.bin ; $08d0
snare_0f_sample_end:

scratch_05_sample_bank	equ this_bank
scratch_05_sample:
	libbin samples\Scratch05_sample.bin ; $0980
scratch_05_sample_end:

scratch_09_sample_bank	equ this_bank
scratch_09_sample:
	libbin samples\Scratch09_sample.bin ; $08a0
scratch_09_sample_end:

scratch_0a_sample_bank	equ this_bank
scratch_0a_sample:
	libbin samples\Scratch0a_sample.bin ; $08a0
scratch_0a_sample_end:

;***************************************************************************
samplebank04	equ 	samplebank00+$04
sample04	group	samplebank04		; set group bank
		org 	samplebank04:$4000
this_bank	=	samplebank04
;***************************************************************************

kick_03_sample_bank	equ this_bank
kick_03_sample:
	libbin samples\Kick03_sample.bin ; $05c0
kick_03_sample_end:

kick_06_sample_bank	equ this_bank
kick_06_sample:
	libbin samples\Kick06_sample.bin ; $05a0
kick_06_sample_end:

kick_0a_sample_bank	equ this_bank
kick_0a_sample:
	libbin samples\Kick0a_sample.bin ; $0700
kick_0a_sample_end:

kick_0b_sample_bank	equ this_bank
kick_0b_sample:
	libbin samples\Kick0b_sample.bin ; $0570
kick_0b_sample_end:

kick_0f_sample_bank	equ this_bank
kick_0f_sample:
	libbin samples\Kick0f_sample.bin ; $06a0
kick_0f_sample_end:

hat_04_sample_bank	equ this_bank
hat_04_sample:
	libbin samples\Hat04_sample.bin ; $06c0
hat_04_sample_end:

ride_00_sample_bank	equ this_bank
ride_00_sample:
	libbin samples\Ride00_sample.bin ; $05f0
ride_00_sample_end:

crash_01_sample_bank	equ this_bank
crash_01_sample:
	libbin samples\Crash01_sample.bin ; $0700
crash_01_sample_end:

scratch_01_sample_bank	equ this_bank
scratch_01_sample:
	libbin samples\Scratch01_sample.bin ; $06f0
scratch_01_sample_end:

scratch_02_sample_bank	equ this_bank
scratch_02_sample:
	libbin samples\Scratch02_sample.bin ; $06f0
scratch_02_sample_end:

;***************************************************************************
samplebank05	equ 	samplebank00+$05
sample05	group	samplebank05		; set group bank
		org 	samplebank05:$4000
this_bank	=	samplebank05
;***************************************************************************

kick_04_sample_bank	equ this_bank
kick_04_sample:
	libbin samples\kick04_sample.bin ; $0470
kick_04_sample_end:

kick_07_sample_bank	equ this_bank
kick_07_sample:
	libbin samples\Kick07_sample.bin ; $0530
kick_07_sample_end:

kick_09_sample_bank	equ this_bank
kick_09_sample:
	libbin samples\Kick09_sample.bin ; $0440
kick_09_sample_end:

kick_0c_sample_bank	equ this_bank
kick_0c_sample:
	libbin samples\Kick0c_sample.bin ; $0510
kick_0c_sample_end:

snare_02_sample_bank	equ this_bank
snare_02_sample:
	libbin samples\Snare02_sample.bin ; $0460
snare_02_sample_end:

snare_05_sample_bank	equ this_bank
snare_05_sample:
	libbin samples\Snare05_sample.bin ; $0430
snare_05_sample_end:

snare_07_sample_bank	equ this_bank
snare_07_sample:
	libbin samples\Snare07_sample.bin ; $0480
snare_07_sample_end:

snare_0a_sample_bank	equ this_bank
snare_0a_sample:
	libbin samples\Snare0a_sample.bin ; $0490
snare_0a_sample_end:

hat_02_sample_bank	equ this_bank
hat_02_sample:
	libbin samples\Hat02_sample.bin ; $0480
hat_02_sample_end:

hat_03_sample_bank	equ this_bank
hat_03_sample:
	libbin samples\Hat03_sample.bin ; $0420
hat_03_sample_end:

hat_09_sample_bank	equ this_bank
hat_09_sample:
	libbin samples\Hat09_sample.bin ; $04b0
hat_09_sample_end:

scratch_04_sample_bank	equ this_bank
scratch_04_sample:
	libbin samples\Scratch04_sample.bin ; $04c0
scratch_04_sample_end:

scratch_07_sample_bank	equ this_bank
scratch_07_sample:
	libbin samples\Scratch07_sample.bin ; $04b0
scratch_07_sample_end:

tom_00_sample_bank	equ this_bank
tom_00_sample:
	libbin samples\Dunno00_sample.bin ; $0440
tom_00_sample_end:


	end
