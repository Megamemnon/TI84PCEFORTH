;Standard Routines

;cleanup - call at beginning and ending of program
cleanup:
	call	_DelRes ; identify the statistics variables as invalid
	call	_Clrtxtshd
	call	_Clrscrn
	set		0,(iy+3)
	call	_HomeUp
	call	_DrawStatusBar
	ret

;getchar - get character from keyboard and reflect to console
;input: none
;output: ASCII in A (or 0FFh for EOL)		
getchar:
	call	_GetKey
	call	getascii
	cp		a,0
	jr		z,getchar
	cp		a,0dh
	jr		z,getchara
	cp		a,0ffh
	jr		z,getchara
	cp		a,0feh
	jr		z,getchara
	ld		(getcharb), a
	call	_Putc
	ld		a, (getcharb)
getchara:
	ret
getcharb:
	.db		0
		
;getch - get character from keyboard and don't reflect to console
;input: none
;output: ASCII in A (or 0FFh for EOL)		
getch:
	call	_GetKey
	call	getascii
	cp		a,0
	jr		z,getch
	ret

;getascii - translate key press code (_GetKey) into ASCII
;input: keyscan in A
;output: ASCII in A (or 0 if no ASCII)
getascii:
	push	hl
	ld		h, 0
	ld		l, a
	ld		de, key2ascii
	add		hl, de
	ld		a, (hl)
	pop		hl
	ret
		
; key2ascii table contains ascii values for printable characters beginning at key press code 80h "+"
key2ascii:
	;00h
	.db		0h, 0fah, 0fbh, 0fch, 0fdh, 0dh, 0h, 0h, 0h, 0feh, 0ffh, 0h, 040h, 0h, 0h, 0h
	;010h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
	;020h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 03dh, 03eh, 0h, 019h
	;030h
	.db		0h, 0h, 03ch, 017h, 0h, 0c2h, 027h, 0h, 0h, 018h, 0h, 0h, 0h, 0h, 0h, 0h
	;040h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
	;050h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
	;060h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
	;070h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
	;080h
	.db		02bh, 02dh, 02Ah, 02fh, 05eh, 028h, 029h, 0c1h, 05dh, 0h, 021h, 02ch, 020h, 02eh, 030h, 031h
	;090h
	.db		032h, 033h, 034h, 035h, 036h, 037h, 038h, 039h, 0h, 020h, 041h, 042h, 043h, 044h, 045h, 046h
	;0a0h
	.db		047h, 048h, 049h, 04ah, 04bh, 04ch, 04dh, 04eh, 04fh, 050h, 051h, 052h, 053h, 054h, 055h, 056h
	;0b0h
	.db		057h, 058h, 059h, 05ah, 0h, 0c4h, 011h, 021h, 023h, 040h, 025h, 026h, 02ah, 012h, 010h, 05fh
	;0c0h
	.db		05eh, 07eh, 0h, 0h, 0h, 03bh, 03ah, 0h, 0h, 0h, 03fh, 022h, 05bh, 0h, 0h, 0h 
	;0d0h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
	;0e0h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 07bh, 07dh, 0h, 0h
	;0f0h
	.db		0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h, 002h, 003h, 004h, 0h, 0h, 0h, 0h

		
