;***************************************************************************
;---------------------------------------------------------------------------

blank_header:
	dw _seq-blank_header	; sequence offset
	dw _pat-blank_header	; patterns offset
	dw _end-blank_header	; end offset
	db 5	; # of patterns

;---------------------------------------------------------------------------

	db 'NEW SONG!'			; song name
	db $ff

;---------------------------------------------------------------------------

_seq:

	db 1
	db 2
	db 4
	db 3

	db ctrlbt_track_loop

;---------------------------------------------------------------------------

_pat:
	dw _track2-_pat,_track2-_pat,_track2-_pat,_track2-_pat
	dw _track3-_pat,_track3-_pat,_track3-_pat,_track3-_pat
	dw _track4-_pat,_track4-_pat,_track4-_pat,_track4-_pat
	dw _track5-_pat,_track5-_pat,_track5-_pat,_track5-_pat
	dw _track-_pat,_track-_pat,_track-_pat,_track-_pat

_track2:
	db ctrlbt_c_3
	db ctrlbt_c_3
	db ctrlbt_c_3
	db ctrlbt_c_3

_track:

	db ctrlbt_c_3
	db ctrlbt_macro
	dw _macro-_pat
	db ctrlbt_d_3

	db ctrlbt_wait_32
	db ctrlbt_track_loop

_track3:
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait

_macro:

	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_track_loop

_track4:
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_track_loop

_track5:
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_wait
	db ctrlbt_track_loop

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

_end:

	end
