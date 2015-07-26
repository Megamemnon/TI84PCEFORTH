; TI 84 Plus CE Forth by Brian O'Dell
; Based on Itsy-Forth by John Metcalf http://www.retroprogramming.com/2012/03/itsy-forth-1k-tiny-compiler.html
.ASSUME ADL=1

.nolist 
#include "include\ti84pce.inc" 
.list 
	.org 	userMem-2 
	.db 	tExtTok,tAsm84CeCmp 

main:	
	call	_PushRealO1 ; save the program name for later use
	call	cleanup
	call	_RunIndicOff
	ld		hl,0
	ld	(curRow),hl

	; ld		hl, dtib
	; call	_DispHL
	; call	_NewLine
	; ld		hl, dspad
	; call	_DispHL
	; call	_NewLine
	
	ld		hl, titletxt+1
	call	_Puts
	pop		bc ; save 2 long words at the top of the stack to restore later
	pop		de
	ld		(ostackdata), de
	ld		(ostackdata+3),bc
	ld		(ostackdata+6), ix ; also save IX register to restore later
	push	de
	push	bc
 	push	ix
	ld		(origstack), sp ; save the current stack pointer to restore later
	ld		hl,interpreter - 3
	ld		(dip), hl ; set instruction stream to point at outer interpreter

; next - ( - ) jump to next instruction in instruction stream
next:
	ld		hl,(origstack)
	scf
	ccf
	sbc		hl,sp
	jp		M,nexterr
	ld		hl, (dip)
	inc		hl
	inc		hl
	inc		hl
	ld		(dip),hl
	ld		ix,(hl)
	ld		(dcxt), ix
	ld		ix, (ix)
	jp		(ix)
nexterr:
	call	_NewLine
	ld		hl, (dip)
	call	_Disphl
	ld		a,32
	call	_Putc
	ld		sp,(origstack)
	pop		hl
	pop		hl
	pop		hl
	ld		hl,(ostackdata)
	push	hl
	ld		hl,(ostackdata+3)
	push	hl
	ld		hl,(ostackdata+6)
	push	hl
	ld		hl,0
	ld		(dstate), hl
	ld		hl, stkunder
	call	_Puts
	ld		hl,interpreter
	ld		(dip), hl
	ld		ix,(hl)
	ld		(dcxt), ix
	ld		ix, (ix)	
	jp		(ix)



; outer interpreter (written in FORTH, of course)
interpreter:
	.dl		xprompt
	.dl		xtib
	.dl		xlit
	.dl		80
	.dl		xaccept
	.dl		xdrop
	.dl		xcr
intnextword:
	.dl		xlit
	.dl		32
	.dl		xword
	.dl		xcount
	.dl		xfind
	.dl		xstate
	.dl		xfetch
	.dl		xif
	.dl		xbranch
	.dl		compilemode
	.dl		xthen
	.dl		xif
	.dl		xexec
	.dl		xelse
	.dl		xtonum
	.dl		xthen
intnext:
	.dl		xtoin
	.dl		xfetch
	.dl		xnotib
	.dl		xfetch
	.dl		xequal
	.dl		xif
	.dl		xbranch
	.dl		interpreter
	.dl		xthen
	.dl		xbranch
	.dl		intnextword
compilemode:
	.dl		xif
	.dl		xbranch
	.dl		compilext
	.dl		xelse
	.dl		xtonum
	.dl		xdup
	.dl		xliteral
	.dl		xthen
	.dl		xbranch
	.dl		intnext
compilext:
	.dl		xdup
	.dl		ximq 
	.dl		xif
	.dl		xexec
	.dl		xelse
	.dl		xcoma 
	.dl		xthen
	.dl		xbranch
	.dl		intnext
 


titletxt:
	.db		7, "84+CE Forth", 0
abouttxt:
	.db		47,"84+CE Forth               Coded by Brian O'Dell",0
stkunder:
	.db		"Stack Underflow",0
origstack:
	.dl		0
ostackdata:
	.dl		0,0,0
temp:
	.dl		0
temp2:
	.dl		0
prompttxt:
	.db		"-OK>",0


#include "include\std84pce.asm"

sysdictionary:
#include "include\primitives.asm"

dictionary:		
	.fill	1024,0
dictionaryend:
	.end
