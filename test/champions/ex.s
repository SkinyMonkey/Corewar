.name "zork"
.comment "just a basic living prog"

;:l2
; Registre : 1 byte
; Label : 2 bytes
; Direct : 2 byte
; 0b op 01,00 0f,00 01
l2:	sti	r1,%:live,%42

	  and	r1,%0,r1
live:	live	%1
	zjmp	%:live
