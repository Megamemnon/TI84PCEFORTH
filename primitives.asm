; TI 84 Plus CE Forth Primitives

; dovar - internal routine to place a variable on the stack
; the variable's execution token is expected to be in dcxt
dovar:
	ld		hl, (dcxt)
	inc		hl
	inc		hl
	inc		hl
	push	hl
	jp		next

; docolon - internal routine to store current instruction stream pointer,
; prepare new instruction stream and launch inner interpreter
docolon:
	call	puship
	ld		hl, (dcxt)
	ld		(dip), hl
	jp		next

; return stack push and pop
puship:
	ld		de,(dip)
	ld		hl, (drsp)
	ld		(hl), de
	inc		hl
	inc		hl
	inc		hl
	ld		(drsp), hl
	ret
popip:
	ld		hl, (drsp)
	dec		hl
	dec		hl
	dec		hl
	ld		de, (hl)
	ld		(drsp), hl
	ld		(dip), de
	ret

; control stack push and pop
pushde:
	ld		hl, (dcsp)
	ld		(hl), de
	inc		hl
	inc		hl
	inc		hl
	ld		(dcsp), hl
	ret
popde:
	ld		hl, (dcsp)
	dec		hl
	dec		hl
	dec		hl
	ld		de, (hl)
	ld		(dcsp), hl
	ret

; dictionary entry description
; link field - l*: - pointer to the previous dictionary entry's link field
; flag field - f*: - flag byte; defines immediate words
; name field - n*: - length-prefixed string containing name of word
; execution token - x*: - pointer to code to run when word is executed
; data field - d*: - code (primitive word), execution tokens (compound word)
;              memory space (variable)

; @ (fetch) - ( addr -- n ) put value at addr on top of stack
lfetch:
	.dl		0
ffetch:
	.db		0
nfetch:
	.db		1,"@"
xfetch:
	.dl		dfetch
dfetch:
	pop		hl
	ld		bc, (hl)
	push	bc
	jp		next

; C@ (char fetch) - ( addr -- char ) put character (byte) at addr on top of stack
lcfetch:
	.dl		lfetch
fcfetch:
	.db		0
ncfetch:
	.db		2,"C@"
xcfetch:
	.dl		dcfetch
dcfetch:
	pop		hl
	ld		bc,0
	ld		c, (hl)
	push	bc
	jp		next

; ! (store) - ( n addr -- ) Store n at addr
lstor:
	.dl		lcfetch
fstor:
	.db		0
nstor:
	.db		1,"!"
xstor:
	.dl		dstor
dstor:
	pop		hl
	pop		bc
	ld		(hl), bc
	jp		next

; C! (char store) - ( char addr -- ) store character (byte) at addr
lcstor:
	.dl		lstor
fcstor:
	.db		0
ncstor:
	.db		2,"C!"
xcstor:
	.dl		dcstor
dcstor:
	pop		hl
	pop		bc
	ld		(hl), c
	jp		next

; IP - Instruction Pointer; contains a ptr to the currently executing instruction in the instruction stream
lip:
	.dl		lcstor
fip:
	.db		0
nip:
	.db		2,"IP"
xip:
	.dl		dovar
dip:
	.dl		0

; CXT - Current Execution Token; contains a pointer to the current word's execution token
lcxt:
	.dl		lip
fcxt:
	.db		0
ncxt:
	.db		3,"CXT"
xcxt:
	.dl		dovar
dcxt:
	.dl		0

; STATE - compilation mode (1) or interpreter mode (0)
lstate:
	.dl		lcxt
fstate:
	.db		0
nstate:
	.db		5,"STATE"
xstate:
	.dl		dovar
dstate:
	.dl		0

; RSP - return stack pointer
lrsp:
	.dl		lstate
frsp:
	.db		0
nrsp:
	.db		3,"RSP"
xrsp:
	.dl		dovar
drsp:
	.dl		drsb

; RSB - Return Stack Buffer
lrsb:
	.dl		lrsp
frsb:
	.db		0
nrsb:
	.db		2,"RSB"
xrsb:
	.dl		dovar
drsb:
	.dl		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; CSP - control stack pointer
lcsp:
	.dl		lrsb
fcsp:
	.db		0
ncsp:
	.db		3,"CSP"
xcsp:
	.dl		dovar
dcsp:
	.dl		dcsb

; CSB - Control Stack Buffer
lcsb:
	.dl		lcsp
fcsb:
	.db		0
ncsb:
	.db		2,"CSB"
xcsb:
	.dl		dovar
dcsb:
	.dl		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; #TIB - ( - addr ) push address of #TIB (dnotib) on the stack
lnotib:
	.dl		lcsb
fnotib:
	.db		0
nnotib:
	.db		4,"#TIB"
xnotib:
	.dl		dovar
dnotib:
	.dl		0

; TIB - ( - addr ) push address of TIB on the stack
ltib:
	.dl		lnotib
ftib:
	.db		0
ntib
	.db		3,"TIB"
xtib:
	.dl		dovar
dtib:
	.db		0,"012345678901234567890123456789012345678901234567890123456789012345678901234567890123"

; SOURCE
lsource:
	.dl		ltib
fsource:
	.db		0
nsource:
	.db		6,"SOURCE"
xsource:
	.dl		docolon
dsource:
	.dl		xtib, xnotib, xfetch, xexit
	ld		hl,dtib
	push	hl
	ld		hl,0
	ld		a,(dnotib)
	ld		l,a
	push	hl
	jp		next

; >IN - index of the next available byte in tib
ltoin:
	.dl		lsource
ftoin:
	.db		0
ntoin:
	.db		3,">IN"
xtoin:
	.dl		dovar
dtoin:
	.dl		0

; BASE - number base (not currently used)
lbase:
	.dl		ltoin
fbase:
	.db		0
nbase:
	.db		4,"BASE"
xbase:
	.dl		dovar
dbase:
	.dl		10

; PAD - 84 byte buffer for programmer use
lpad:
	.dl		lbase
fpad:
	.db		0
npad:
	.db		3,"PAD"
xpad:
	.dl		dovar
dpad:
	.db		"012345678901234567890123456789012345678901234567890123456789012345678901234567890123"


; SPAD - another PAD
lspad:
	.dl		lpad
fspad:
	.db		0
nspad:
	.db		3,"SPAD"
xspad:
	.dl		dovar
dspad:
	.db		"012345678901234567890123456789012345678901234567890123456789012345678901234567890123"

; " - ( " ccc " - addr) - copies characters from input stream to a 
; length prefixed string in SPAD and puts the address of SPAD on the stack
ldquote:
	.dl		lspad
fdquote:
	.db		0
ndquote:
	.db		1,022h
xdquote:
	.dl		docolon
ddquote:
	.dl		xlit, 022h
	.dl		xword, xcount, xswap, x1m, xswap, xspad, xswap
	.dl		xcmove, xspad, xdup, xdup, xfetch, x1m, xswap, xstor, xexit

; ECS - ( addr -- addr ) Edit Counted String
lecs:
	.dl		ldquote
fecs:
	.db		0
necs:
	.db		3,"ECS"
xecs:
	.dl		docolon
decs:
	.dl		xclr,xdup,xcount,xtype,xecs2,xexit

; ecs2
lecs2:
	.dl		lecs
fecs2:
	.db		0
necs2:
	.db		4,"ECS2"
xecs2:
	.dl		decs2
decs2:
	pop		hl
	push	hl
	ld		a,(hl)
	ld		(ecs2len),a
	ld		hl,0
	ld		(ecs2row),hl
	ld		l,a
	dec		hl
	ld		a,26
	push	ix
	push	iy
	call	_Divhlbya
	pop		iy
	pop		ix
	ld		(ecs2cmax),a
	ld		a,l
	ld		(ecs2rmax),a
	ld		hl,0
	ld		(ecs2row),hl

decs2again:
	ld		a,(ecs2row)
	ld		(curRow),a
	ld		a,(ecs2col)
	ld		(curCol),a

	; display letter at cursor in inverse text mode
	pop		bc
	push	bc
	inc		bc
	ld		a,(ecs2col)
	ld		hl,0
	ld		l,a
	add		hl,bc
	ld		a,(ecs2row)
	ld		de,0
	ld		e,a
	ld		d,25
	mlt		de
	add		hl,de
	ld		a,(ecs2row)
	ld		(curRow),a
	ld		a,(ecs2col)
	ld		(curCol),a
	ld		a,(hl)
	set		textInverse,(IY+textFlags)
	call	_Putc

	push	ix
	push	iy
	call	getch
	pop		iy
	pop		ix
	ld		(ecs2char),a

	; display letter at cursor in regular text mode
	pop		bc
	push	bc
	inc		bc
	ld		a,(ecs2col)
	ld		hl,0
	ld		l,a
	add		hl,bc
	ld		a,(ecs2row)
	ld		de,0
	ld		e,a
	ld		d,25
	mlt		de
	add		hl,de
	ld		a,(ecs2row)
	ld		(curRow),a
	ld		a,(ecs2col)
	ld		(curCol),a
	ld		a,(hl)
	res		textInverse,(IY+textFlags)
	call	_Putc

	; display letter to left of cursor in regular text mode
	pop		bc
	push	bc
	inc		bc
	ld		a,(ecs2col)
	cp		a,0
	jr		z,ecs2handlechar
	dec		a
	ld		hl,0
	ld		l,a
	add		hl,bc
	ld		a,(ecs2row)
	ld		de,0
	ld		e,a
	ld		d,25
	mlt		de
	add		hl,de
	ld		a,(ecs2row)
	ld		(curRow),a
	ld		a,(ecs2col)
	dec		a
	ld		(curCol),a
	ld		a,(hl)
	res		textInverse,(IY+textFlags)
	call	_Putc

ecs2handlechar:
	ld		a,(ecs2char)
	cp		a,0dh
	jp		z,ecs2done
	cp		a,0ffh
	jr		z,ecs2del
	cp		a,0feh
	jr		z,ecs2clr
	cp		a,0fah
	jr		z,ecs2right
	cp		a,0fbh
	jr		z,ecs2left
	cp		a,0fch
	jp		z,ecs2up
	cp		a,0fdh
	jp		z,ecs2down
	pop		bc
	push	bc
	inc		bc
	ld		a,(ecs2col)
	ld		hl,0
	ld		l,a
	add		hl,bc
	ld		a,(ecs2row)
	ld		de,0
	ld		e,a
	ld		d,25
	mlt		de
	add		hl,de
	ld		a,(ecs2row)
	ld		(curRow),a
	ld		a,(ecs2col)
	ld		(curCol),a
	ld		a,(ecs2char)
	ld		(hl),a
	res		textInverse,(IY+textFlags)
	call	_Putc
	jp		ecs2right
ecs2del:
	jp		decs2again
ecs2clr:
	jp		decs2again
ecs2right:
	ld		a,(ecs2col)
	inc		a
	cp		26
	jr		nc,ecs2rtdn
	ld		(ecs2col),a
	jp		ecs2chk
ecs2rtdn:
	ld		a,0
	ld		(ecs2col),a
	ld		a,(ecs2row)
	inc		a
	ld		(ecs2row),a
	jp		ecs2chk
ecs2left:
	ld		a,(ecs2col)
	dec		a
	cp		26
	jr		nc,ecs2ltup
	ld		(ecs2col),a
	jp		ecs2chk
ecs2ltup:
	ld		a,25
	ld		(ecs2col),a
	ld		a,(ecs2row)
	dec		a
	ld		(ecs2row),a
	jp		ecs2chk
ecs2up:
	ld		a,(ecs2row)
	dec		a
	ld		(ecs2row),a
	jp		ecs2chk
ecs2down:
	ld		a,(ecs2row)
	inc		a
	ld		(ecs2row),a
	jp		ecs2chk

ecs2chk:
	ld		a,(ecs2rmax)
	inc		a
	ld		c,a
	ld		a,(ecs2row)
	cp		c
	jr		nc,ecs2rst
	dec		a
	cp		c
	jp		c,decs2again
	ld		a,(ecs2cmax)
	ld		c,a
	ld		a,(ecs2col)
	cp		c
	jr		nc,ecs2rst
	jp		decs2again
ecs2rst:
	ld		a,(ecs2rmax)
	ld		(ecs2row),a
	ld		a,(ecs2cmax)
	ld		(ecs2col),a
	jp		decs2again

ecs2done:
	res		textInverse,(IY+textFlags)
	jp		next
ecs2row:
	.db		0
ecs2col:
	.db		0
ecs2char:
	.db		0
ecs2len:
	.db		0
ecs2rmax:
	.db		0
ecs2cmax:
	.db		0

; CLR
lclr:
	.dl		lecs2
fclr:
	.db		0
nclr:
	.db		3,"CLR"
xclr:
	.dl		dclr
dclr:
	call	_Clrscrn
	ld		hl,0
	ld		(curRow),hl
	jp		next

; QUIT - launch outer interpreter
lquit:
	.dl		lclr
fquit:
	.db		0
nquit:
	.db		4,"QUIT"
xquit:
	.dl		dquit
dquit:
	ld		hl,interpreter
	ld		(dip), hl
	ld		ix,(hl)
	ld		(dcxt), ix
	ld		ix, (ix)	
	jp		(ix)

; = - ( n1  n2 - n3 ) if n1!=n2 then n3 will be zero; otherwise n3=1
lequal
	.dl		lquit
fequal:
	.db		0
nequal:
	.db		1,"="
xequal:
	.dl		dequal
dequal:
	pop		de
	pop		hl
	scf
	ccf
	sbc		hl,de
	jr		z, equala
	ld		hl, 0
	push	hl
	jp		next
equala:
	ld		hl, 1
	push	hl
	jp		next

; <
llst:
	.dl		lequal
flst:
	.db		0
nlst:
	.db		1,"<"
xlst:
	.dl		dlst
dlst:
	pop		hl
	pop		de
	scf
	ccf
	sbc		hl,de
	jp		m,lsta
	ld		bc,1
	push	bc
	jp		next
lsta:
	ld		bc,0
	push	bc
	jp		next

; >
lgrt:
	.dl		llst
fgrt:
	.db		0
ngrt:
	.db		1,">"
xgrt:
	.dl		dgrt
dgrt:
	pop		de
	pop		hl
	scf
	ccf
	sbc		hl,de
	jp		m,grta
	ld		bc,1
	push	bc
	jp		next
grta:
	ld		bc,0
	push	bc
	jp		next

; 0< - ( n - flag ) flag is true if n < 0
lzlt:
	.dl		lgrt
fzlt:
	.db		0
nzlt:
	.db		2,"0<"
xzlt:
	.dl		dzlt
dzlt:
	ld		bc,0
	push	bc
	jp		next

; 0= - ( n - flag ) flag is true if n = 0
lzeq:
	.dl		lzlt
fzeq:
	.db		0
nzeq:
	.db		2,"0="
xzeq:
	.dl		dzeq
dzeq:
	pop		bc
	ld		a,0
	cp		b
	jr		nz,zeqa
	cp		c
	jr		nz,zeqa
	ld		bc,1
	push	bc
	jp		next
zeqa:
	ld		bc,0
	push	bc
	jp		next

; 0> - ( n - flag ) flag is true if n > 0
lzgt:
	.dl		lzeq
fzgt:
	.db		0
nzgt:
	.db		2,"0>"
xzgt:
	.dl		dzgt
dzgt:
	pop		bc
	ld		a,0
	cp		b
	jr		nz,zgta
	cp		c
	jr		nz,zgta
	ld		bc,0
	push	bc
	jp		next
zgta:
	ld		bc,1
	push	bc
	jp		next

; + - ( n1 n2 - n3) - n3=n1+n2
lplus:
	.dl		lzgt
fplus:
	.db		0
nplus:
	.db		1,"+"
xplus
	.dl		dplus
dplus:
	pop		de
	pop		hl
	add		hl,de
	push	hl
	jp		next

; minus - ( n1 n2 - n3 ) n3 n1 - n2
lminus:
	.dl		lplus
fminus:
	.db		0
nminus:
	.db		1,"-"
xminus:
	.dl		dminus
dminus:
	pop		de
	pop		hl
	sbc		hl,de
	push	hl
	jp		next

; X ( n1 n2 - n3 ) where n3 = n1 * n2
lmult:
	.dl		lminus
fmult:
	.db		0
nmult:
	.db		1,"*"
xmult:
	.dl		dmult
dmult:
	pop		bc
	pop		hl
	ld		a,0
	cp		b
	jr		nz, multa
	cp		c
	jr		z, multb
	ld		a,1
	cp		c
	jr		z, multd
	ld		a,0
	dec		bc
	push	hl
	pop		de
multa:
	add		hl,de
	dec		bc
	cp		b
	jr		nz, multa
	cp		c
	jr		nz, multa
multd:
	push	hl
	jp		next
multb:
	push	bc
	jp		next

; / - ( n1 n2 - n3 ) where n3 = n1/n2 and n2 is an 8 bit value

ldiv:
	.dl		lmult
fdiv:
	.db		0
ndiv:
	.db		1,"/"
xdiv:
	.dl		ddiv
ddiv:
	pop		bc
	ld		a,c
	pop		hl
	push	ix
	call	_Divhlbya
	pop		ix
	push	hl
	jp		next

; /MOD - ( n1 n2 - n3 ) where n3 = n1/n2 and n2 is an 8 bit value

ldivmod:
	.dl		ldiv
fdivmod:
	.db		0
ndivmod:
	.db		4,"/MOD"
xdivmod:
	.dl		ddivmod
ddivmod:
	pop		bc
	ld		a,c
	pop		hl
	push	ix
	call	_Divhlbya
	pop		ix
	ld		bc,0
	ld		c,a
	push	bc
	push	hl
	jp		next

; 1+
l1p:
	.dl		ldivmod
f1p:
	.db		0
n1p:
	.db		2,"1+"
x1p:
	.dl		d1p
d1p:
	pop		bc
	inc		bc
	push	bc
	jp		next

; 1-
l1m:
	.dl		l1p
f1m:
	.db		0
n1m:
	.db		2,"1-"
x1m:
	.dl		d1m
d1m:
	pop		bc
	dec		bc
	push	bc
	jp		next

; 2+
l2p:
	.dl		l1m
f2p:
	.db		0
n2p:
	.db		2,"2+"
x2p:
	.dl		d2p
d2p:
	pop		bc
	inc		bc
	inc		bc
	push	bc
	jp		next

; 2-
l2m:
	.dl		l2p
f2m:
	.db		0
n2m:
	.db		2,"2-"
x2m:
	.dl		d2m
d2m:
	pop		bc
	dec		bc
	dec		bc
	push	bc
	jp		next

; rot - ( n1 n2 n3 - n2 n3 n1)
lrot:
	.dl		l2m
frot:
	.db		0
nrot:
	.db		3,"ROT"
xrot:
	.dl		drot
drot:
	pop		bc
	pop		de
	pop		hl
	push	de
	push	bc
	push	hl
	jp		next

; drop	- ( n - )
ldrop:
	.dl		lrot
fdrop:
	.db		0
ndrop:
	.db		4,"DROP"
xdrop:
	.dl		ddrop
ddrop:
	pop		hl
	jp		next

; 2drop	- ( n1 n2 - )
l2drop:
	.dl		ldrop
f2drop:
	.db		0
n2drop:
	.db		5,"2DROP"
x2drop:
	.dl		d2drop
d2drop:
	pop		hl
	pop		hl
	jp		next

; dup - ( n - n n )
ldup:
	.dl		l2drop
fdup:
	.db		0
ndup:
	.db		3,"DUP"
xdup:
	.dl		ddup
ddup:
	pop		hl
	push	hl
	push	hl
	jp		next

; 2dup - ( n1 n2 - n1 n2 n1 n2 )
l2dup:
	.dl		ldup
f2dup:
	.db		0
n2dup:
	.db		4,"2DUP"
x2dup:
	.dl		d2dup
d2dup:
	pop		bc
	pop		hl
	push	hl
	push	bc
	push	hl
	push	bc
	jp		next

; swap - ( n1 n2 - n2 n1 )
lswap:
	.dl		l2dup
fswap:
	.db		0
nswap:
	.db		4,"SWAP"
xswap:
	.dl		dswap
dswap:
	pop		de
	pop		hl
	push	de
	push	hl
	jp		next

; over
lover:
	.dl		lswap
fover:
	.db		0
nover:
	.db		4,"OVER"
xover:
	.dl		dover
dover:
	pop		hl
	pop		de
	push	de
	push	hl
	push	de
	jp		next

; and
land:
	.dl		lover
fand:
	.db		0
nand:
	.db		3,"AND"
xand:
	.dl		dand
dand:
	pop		hl
	pop		de
	ld		bc,0
	ld		a,h
	and		a,d
	ld		b,a
	ld		a,l
	and		a,e
	ld		c,a
	push	bc
	jp		next

; OR
lor:
	.dl		land
for:
	.db		0
nor:
	.db		2,"OR"
xor:
	.dl		dor
dor:
	pop		hl
	pop		de
	ld		bc,0
	ld		a,h
	or		a,d
	ld		b,a
	ld		a,l
	or		a,e
	ld		c,a
	push	bc
	jp		next

; OR
lxor:
	.dl		lor
fxor:
	.db		0
nxor:
	.db		3,"XOR"
xxor:
	.dl		dxor
dxor:
	pop		hl
	pop		de
	ld		bc,0
	ld		a,h
	xor		a,d
	ld		b,a
	ld		a,l
	xor		a,e
	ld		c,a
	push	bc
	jp		next

; if - ( n - ) if item on top of stack is 0, moves instruction pointer to instruction after THEN
lif:
	.dl		lxor
fif:
	.db		0
nif:
	.db		2,"IF"
xif:
	.dl		dif
dif:
	pop		bc

;	push	af
;	push	bc
;	push	de
;	push	hl
;	push	ix
;	push	iy
;
;	ld		hl,0
;	ld		l,c
;	call	_Disphl
;	call	_NewLine
;	call	_GetKey
;
;	pop		iy
;	pop		ix
;	pop		hl
;	pop		de
;	pop		bc
;	pop		af

	ld		a,0
	cp		b
	jr		nz,ifdone
	cp		c
	jr		nz,ifdone
	ld		c,0
	ld		a,0
ifcrawl:
	ld		hl, (dip) ; get the current instruction pointer
	inc		hl
	inc		hl
	inc		hl	; increment it to point at the next instruction
	ld		(dip),hl ; save the instruction pointer
	ld		de,(hl)	; load the instruction
	ld		hl, xif
	scf
	ccf
	sbc		hl,de
	jr		nz,ifnotif ; encountered a nested IF?
	inc		c ; yes, we did
	jr		ifcrawl
ifnotif:
	ld		hl, xelse 

;	push	af
;	push	bc
;	push	de
;	push	hl
;	push	ix
;	push	iy
;
;	ex		de,hl
;	call	_Disphl
;	call	_NewLine
;	call	_GetKey
;
;	pop		iy
;	pop		ix
;	pop		hl
;	pop		de
;	pop		bc
;	pop		af

	scf
	ccf
	sbc		hl,de
	jr		z,ifelse ; if the instruction is xelse, we may be done
	ld		hl, xthen
	scf
	ccf
	sbc		hl,de
	jr		nz,ifcrawl ; if the instruction is not xthen, do it again
	cp		c
	jr		z,ifdone ; is it our THEN?
	dec		c
	jr		ifcrawl
ifelse:
	cp		c
	jr		nz,ifcrawl ; is it our ELSE?
ifdone:
	jp		next

; else
lelse:
	.dl		lif
felse:
	.db		0
nelse:
	.db		4,"ELSE"
xelse:
	.dl		delse
delse:
	ld		a,0
	ld		c,0
elsecrawl:
	ld		hl, (dip)
	inc		hl
	inc		hl
	inc		hl
	ld		(dip),hl
	ld		de,(hl)
	ld		hl, xif
	scf
	ccf
	sbc		hl,de
	jr		nz,elsenext
	inc		c
	jr		elsecrawl
elsenext:
	ld		hl, xthen
	scf
	ccf
	sbc		hl,de
	jr		nz,elsecrawl
	cp		c
	jr		z,elsedone
	dec		c
elsedone:
	jp		next

; then - ( - ) does nothing; placeholder in instruction stream for IF 
lthen:
	.dl		lelse
fthen:
	.db		0
nthen:
	.db		4,"THEN"
xthen:
	.dl		dthen
dthen:
	jp		next

; DO
ldo:
	.dl		lthen
fdo:
	.db		0
ndo:
	.db		2,"DO"
xdo:
	.dl		ddo
ddo:
	ld		de,(dip)
	call	pushde ; store a ptr to this DO
	pop		hl ; get the counter start value
	pop		de ; get the counter final value
	push	hl ; pushde destroys HL so push it
	call	pushde ; push counter final value onto control stack
	pop		hl ; pop it
	ex		de,hl
	call	pushde ; push counter start value onto control stack
	jp		next

; LOOP
lloop:
	.dl		ldo
floop:
	.db		0
nloop:
	.db		4,"LOOP"
xloop:
	.dl		dloop
dloop:
	call	popde ; get counter
	inc		de ; increment counter
	ex		de,hl ; store counter in hl
	push	hl ; push counter to data stack
	call	popde ; get counter max value
	pop		hl ; get counter in hl
	ld		a,l ; load low byte of counter in a
	cp		e ; compare low byte of counter max
	jr		nc,loopdone ; if a>=e then we're done
	push	hl ; push counter
	push	de ; push counter max
	call	popde ; get ptr to corresponding DO
	ld		(dip),de ; go back to the DO
	call	pushde ; put the DO back in the control stack
	pop		de ; pop counter max off data stack
	call	pushde ; and put it back on control stack
	pop		hl ; pop counter off data stack
	ex		de,hl
	call	pushde ; and put it back on control stack
	jp 		next 
loopdone:
	call	popde ; pop the DO ptr off the control stack 
	jp		next

; +LOOP
lploop:
	.dl		lloop
fploop:
	.db		0
nploop:
	.db		5,"+LOOP"
xploop:
	.dl		dploop
dploop:
	call	popde ; get counter
	pop		bc ; get value to increment counter with
	ld		a,e
	add		a,c
	ld		e,a ; ok, so I'm only supporting 8 bits
	ex		de,hl ; store counter in hl
	push	hl ; push counter to data stack
	call	popde ; get counter max value
	pop		hl ; get counter in hl
	ld		a,l ; load low byte of counter in a
	cp		e ; compare low byte of counter max
	jr		nc,ploopdone ; if a>=e then we're done
	push	hl ; push counter
	push	de ; push counter max
	call	popde ; get ptr to corresponding DO
	ld		(dip),de ; go back to the DO
	call	pushde ; put the DO back in the control stack
	pop		de ; pop counter max off data stack
	call	pushde ; and put it back on control stack
	pop		hl ; pop counter off data stack
	ex		de,hl
	call	pushde ; and put it back on control stack
	jp 		next 
ploopdone:
	call	popde ; pop the DO ptr off the control stack 
	jp		next

; LEAVE
lleave:
	.dl		lploop
fleave:
	.db		0
nleave:
	.db		5,"LEAVE"
xleave:
	.dl		dleave
dleave:
	call	popde
	call	popde
	call	popde
leavecrawl:
	ld		hl, (dip)
	inc		hl
	inc		hl
	inc		hl
	ld		(dip),hl
	ld		de,(hl)
	ld		hl, xloop
	scf
	ccf
	sbc		hl,de
	jr		z,leavedone
	ld		hl, xploop
	scf
	ccf
	sbc		hl,de
	jr		nz,leavecrawl
leavedone:
	jp		next

; lit - ( - n ) pushes next 'instruction' in the instruction stream on the stack as a literal number
llit:
	.dl		lleave
flit:
	.db		0
nlit:
	.db		5,0c1h,"LIT]"
xlit:
	.dl		dlit
dlit:
	ld		hl, (dip)
	inc		hl
	inc		hl
	inc		hl
	ld		(dip),hl
	ld		bc,(hl)
	push	bc
	jp		next

; accept - ( addr n1 - n2 ) input n characters into tib
laccept:
	.dl		llit
faccept:
	.db		0
naccept:
	.db		6,"ACCEPT"
xaccept:
	.dl		daccept
daccept:
	pop		bc
	pop		hl
	push	hl
	push	bc	; just in case the user clears and we start over
	ld		b,c
	inc		hl
acceptwhile:
	push	bc
	push	de
	push	hl
	push	ix
	push	iy
	call	getchar
	pop		iy
	pop		ix
	pop		hl
	pop		de
	pop		bc
	cp		a,0dh
	jr		z,acceptdone
	cp		a,0ffh
	jr		z,acceptdel
	cp		a, 0feh
	jr		z,acceptclr
	cp		a,0fah
	jr		z,acceptwhile
	cp		a,0fbh
	jr		z,acceptwhile
	cp		a,0fch
	jr		z,acceptwhile
	cp		a,0fdh
	jr		z,acceptwhile
	jr		z,acceptwhile
	ld		(hl),a
	inc		hl
	djnz	acceptwhile
acceptdone:
	pop		bc
	pop		de
	scf
	ccf
	sbc		hl,de
	ld		a,l
	scf
	ccf
	sbc		a,1
	ld		(dtib),a
	ld		hl,0
	ld		l,a
	ld		(dnotib),hl
	push	hl
	ld		hl,0
	ld		(dtoin),hl
	jp 		next
acceptdel:
	ld		a,(curCol)
	dec		a
	ld		(curCol),a
	dec		hl
	ld		a,32
	push	bc
	push	de
	push	hl
	push	ix
	push	iy
	call	_Putc
	pop		iy
	pop		ix
	pop		hl
	pop		de
	pop		bc
	ld		a,(curCol)
	dec		a
	ld		(curCol),a
	jp		acceptwhile
acceptclr
	call	_NewLine
	ld		a,0
	ld		(curCol), a
	ld		hl, prompttxt
	call	_Puts
	jp		daccept

; space
lspc:
	.dl		laccept
fspc:
	.db		0
nspc:
	.db		5,"SPACE"
xspc:
	.dl		dspc
dspc:
	ld		a,32
	call	_Putc
	jp		next

; BL
lbl:
	.dl		lspc
fbl:
	.db		0
nbl:
	.db		2,"BL"
xbl:
	.dl		dbl
dbl: 
	ld		hl,32
	push	hl
	jp		next

; word ( char - addr) - get next word in tib
lword:
	.dl		lbl
fword:
	.db		0
nword:
	.db		4,"WORD"
xword:
	.dl		dword
dword:
	ld		hl,(dtoin)
	ld		c,l
	ld		hl, (dnotib)
	ld		a, l
	cp		c
	jr		z,wordf ; if >IN = #TIB then there are no more words in TIB
	sub		a,c
	jp		m,wordf ; if >IN > #TIB then there are really no more words in TIB
	pop		hl ; get the delimiter
	ld		a,l ; put it in a
	push	af ; A has delimiter so push it to use A for a compare
	ld		bc,0
	ld		hl,(dtoin)
	ld		c,l
	ld		hl, dtib
	add		hl,bc ; HL is now a ptr to the last char read previously
				  ; or beginning of TIB, which is a count
	pop		af ; get the delimiter
	ld		c,0 ; c will be the char count
wordc: ; ignore all delimiters before the next word (if there is one)
	push	hl ; store the current HL...
	pop		de ; in DE, it will be the ptr this function returns
	inc		hl ; now HL points at the next unread char
	ld		b,(hl) ; get next char in TIB
	cp		b  
	jr		nz,wordd ; if b is not the delimiter then we found a word
	push	hl	; b is the delimiter, so remember this address
	ld		hl,dtoin ; increment the variable >IN while stepping over...
	inc		(hl) ; preceding delimiters
	pop		hl
	jr		wordc
worda:
	inc		hl ; increment the pointer
	ld		b,(hl) ; get the next char
	cp		b ; check for a delimiter
	jr		z,wordb ; if B is a delimiter we've found all chars for the next word
wordd: ; we just found the first char of the next word
	inc		c ; increment the char counter
	push	af
	push	hl
	ld		hl, (dnotib)
	ld		a, l
	ld		hl, (dtoin)
	sub		a,l
	cp		c
	jr		z,worde ; are we at the end of TIB?
	pop		hl ; no
	pop		af
	jr		worda
worde: ; done counting chars for next word and at the end of TIB
	pop		hl ; remember to pop HL and AF
	pop		af
	ld		a,c
	ld		(de),a ; store the count in (DE), the byte prior to our current word
	push	de ; return a ptr to a length prefixed string
	ld		hl, (dnotib)
	ld		(dtoin), hl
	jp		next
wordb: ; done counting chars for the next word because B is now a delimiter
	ld		a,c
	ld		(de),a ; store the count in (DE), the byte prior to our current word
	push	de ; return a ptr to a length prefixed string
	ld		hl, (dtoin) ; update >IN to point to the next char
	ld		a,l
	add		a,c
	inc		a ; we didn't increment C after B became a delimiter
	ld		hl,0
	ld		l,a
	ld		(dtoin), hl ; now >IN points to the delimiter we just discovered
	jp		next
wordf:
	ld		a,0
	ld		de, dtib
	ld		(de),a
	push	de
	ld		hl, (dnotib)
	ld		(dtoin), hl
	jp		next

; find	( addr1 n1 - [addr2] n2) OR ( addr1 n1 - addr1 n1 0 )
;- if string at addr1 (with count n1) is found 
; in the primitives dictionary, addr2 is an execution token and n2=1, otherwise 
; n2=0 and addr2 is not provided
lfind:
	.dl		lword
ffind:
	.db		0
nfind:
	.db		4,"FIND"
xfind:
	.dl		dfind
dfind:
	pop		bc
	pop		de
	ld		hl,(dlast)
; first check the length of the next word in the dictionary
finda:
	push	de
	push	bc
	push	hl

	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; call	_Disphl

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	inc		hl
	inc		hl
	inc		hl ; skip link field
	inc		hl ; skip flag field

	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; inc		hl
	; ld		a,(hl)
	; call	_Putc
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	ld		a,(hl); get count of dictionary word
	cp		c	; compare to count of provided string
	jp		nz,findc ; not equal, get previous word in dict
	inc		hl
; compare letters in given word to current word
findb:
	ld		a,(de)

	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; call	_Putc
	; ld		a,(hl)
	; call	_Putc
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af
	
	cpi
	jp		nz,findc
	ld		a,0
	cp		c
	jp		z,findd
	inc		de
	jp		findb
; move to the previous word in dictionary
findc:
	pop		hl
	ld		de,(hl)
	ld		a,0
	cp		d
	jr		nz,findca
	cp		e
	jr		z,finde
findca:
	ex		de,hl
	pop		bc
	pop		de
	jp		finda
; found a match
findd:
	pop		hl
	ld		bc,0
	inc		hl
	inc		hl
	inc		hl ; skip link field
	inc		hl ; skip flag field
	ld		c,(hl)
	add		hl,bc
	inc		hl
	pop		bc
	pop		de
	ld		c,1
	push	hl
	push	bc

	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; call	_Disphl
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	jp		next
; did not find a match
finde:
	ld		bc,0
	push	bc
	
;	push	af
;	push	bc
;	push	de
;	push	hl
;	push	ix
;	push	iy
;
;	ld		a,023h
;	call	_Putc
;	call	_NewLine
;	call	_GetKey
;
;	pop		iy
;	pop		ix
;	pop		hl
;	pop		de
;	pop		bc
;	pop		af

	jp		next

; emit - ( char - ) send char to monitor
lemit:
	.dl		lfind
femit:
	.db		0
nemit
	.db		4,"EMIT"
xemit:
	.dl		demit
demit:
	pop		bc
	ld		a,c
	call	_Putc
	jp		next

; count - ( addr1 - int addr2 ) accept counted string as input and return ptr to string and count
lcount:
	.dl		lemit
fcount:
	.db		0
ncount:
	.db		5,"COUNT"
xcount:
	.dl		dcount
dcount:
	pop		hl
	ld		bc,0
	ld		c, (hl)
	inc		hl
	push	hl
	push	bc
	jp		next

; type - ( n addr - ) accept ptr to string and count and emit characters to monitor
ltype
	.dl		lcount
ftype:
	.db		0
ntype:
	.db		4,"TYPE"
xtype:
	.dl		dtype
dtype:
	pop		bc
	pop		hl
	ld		b,c
	ld		a,0
	cp		b
	jr		z,typeb
typea:
	ld		a,(hl)
	inc		hl
	push	hl
	call	_Putc
	pop		hl
	djnz	typea
typeb:
	jp		next

; . - ( n - ) sent n to monitor as a number
ldot:
	.dl		ltype
fdot:
	.db		0
ndot:
	.db		1,"."
xdot:
	.dl		ddot
ddot:
	pop		hl
	call	_Disphl
	jp		next

; U. 
ludot:
	.dl		ldot
fudot:
	.db		0
nudot:
	.db		2,"U."
xudot:
	.dl		dudot
dudot:
	jp		ddot

; prompt - ( - ) display prompt message		
lprompt:
	.dl		ludot
fprompt:
	.db		0
nprompt:
	.db		6,"PROMPT"
xprompt:
	.dl		dprompt		
dprompt:
	call	_NewLine
	ld		a,0
	ld		(curCol), a
	ld		hl, prompttxt+1
	call	_Puts
	jp		next

; newline - ( - ) call TI's _NewLine
lcr:
	.dl		lprompt
fcr:
	.db		0
ncr:
	.db		2,"CR"
xcr:	
	.dl		dcr
dcr:
	call	_NewLine
	ld		a,0
	ld		(curCol),a
	jp		next

; BYE - ( - ) quit FORTH
lbye:
	.dl		lcr
fbye:
	.db		0
nbye:
	.db		3,"BYE"
xbye:
	.dl		dbye
dbye:
	call	cleanup
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
	pop		ix
	ret

; execute - ( addr - ) executes execution token at addr
lexec:
	.dl		lbye
fexec:
	.db		0
nexec:
	.db		7,"EXECUTE"
xexec:
	.dl		dexec
dexec:
	pop		hl
	ld		(dcxt), hl

;	push	af
;	push	bc
;	push	de
;	push	hl
;	push	ix
;	push	iy
;
;	call	_Disphl
;	call	_GetKey
;
;	pop		iy
;	pop		ix
;	pop		hl
;	pop		de
;	pop		bc
;	pop		af

	ld		ix, (hl)
	jp		(ix)

; EXIT - quits executing compiled word and returns control to the calling routine
lexit:
	.dl		lexec
fexit:
	.db		0
nexit:
	.db		4,"EXIT"
xexit:
	.dl		dexit
dexit:
	call	popip
	jp		next

; CMOVE - ( addr1 addr2 n - ) moves n bytes from addr1 to addr2
lcmove:
	.dl		lexit
fcmove:
	.db		0
ncmove:
	.db		5,"CMOVE"
xcmove:
	.dl		dcmove
dcmove:
	pop		bc
	pop		de
	pop		hl
cmovea:
	ld		a,(hl)
	ex		de,hl
	ld		(hl), a
	dec		c
	inc		hl
	ex		de,hl
	inc		hl
	ld		a,0
	cp		c
	jr		nz,cmovea
	jp		next

; NTS - (addr -- addr+1) appends 0 to a count prefixed string at addr
; returns ptr to null terminated string (addr+1)
lnts:
	.dl		lcmove
fnts:
	.db		0
nnts:
	.db		3,"NTS"
xnts:
	.dl		dnts
dnts:
	pop		hl
	ld		a,(hl) ; get length
	inc		hl
	push	hl
	ld		de,0
	ld		e,a
	add		hl,de
	xor		a
	ld		(hl),a
	jp		next

; NTS.
lntsdot:
	.dl		lnts
fntsdot:
	.db		0
nntsdot:
	.db		4,"NTS."
xntsdot:
	.dl		dntsdot
dntsdot:
	pop		hl
	call	_Puts
	jp		next

; CRCODE
lcrcode:
	.dl		lntsdot
fcrcode:
	.db		0
ncrcode:
	.db		6,"CRCODE"
xcrcode:
	.dl		dcrcode
dcrcode:
	ld		hl,(dhere) ; get address of first available space
	ld		de, (dlast) ; get ptr to LAST
	ld		(hl), de ; populate link field of next word
	ld		(dlast), hl ; new word under construction is our new LAST
	inc		hl
	inc		hl
	inc		hl
	inc		hl ; now hl points to name portion of next word
	ld		(dhere), hl
	push	hl
	ld		hl, dstate
	ld		bc,1
	ld		(hl), bc ; set STATE to Compile
	jp		next

; CREATE - add next word to a new dictionary entry
lcreate:
	.dl		lcrcode
fcreate:
	.db		0
ncreate:
	.db		6,"CREATE"
xcreate:
	.dl		docolon
dcreate:
	.dl		xcrcode
	.dl		xlit
	.dl		32
	.dl		xword ; get name of new word from TIB
	.dl		xswap ; swap name and ptr to next mem space of new word
	.dl		xover ; copy name addr 
	.dl		xcount ; count name of new word
	.dl		x1p ; increment that by one to include the length byte, itself
	.dl		xswap 
	.dl		xdrop ; drop the ptr to the string returned by COUNT
	.dl		xdup ; duplicate length of bytes to copy
	.dl		xhere 
	.dl		xfetch
	.dl		xplus ; add length of bytes to copy to contents of HERE
	.dl		xhere
	.dl		xstor ; update here to point to xt of new word
	.dl		xcmove ; move bytes of length prefixed string to name field of new word
	.dl		xbcompile ; put the address of the docolon routine at HERE and inc HERE
	.dl		docolon
	.dl		xexit

; [COMPILE] - copies next Long (execution token) in current colon def to HERE 
; and increments HERE then increments the instruction pointer to skip it
lbcompile:
	.dl		lcreate
fbcompile:
	.db		0
nbcompile:
	.db		6,0c1h,"COMP]"
xbcompile:
	.dl		dbcompile
dbcompile:
	ld		hl,(dip)
	inc		hl
	inc		hl
	inc		hl
	ld		(dip), hl
	ld		de,(hl)
	ld		hl,(dhere)
	ld		(hl), de
	inc		hl
	inc		hl
	inc		hl
	ld		(dhere), hl
	jp		next

; , - ( xt - ) compiles execution token
lcoma:
	.dl		lbcompile
fcoma:
	.db		0
ncoma:
	.db		1,","
xcoma:
	.dl		dcoma
dcoma:
	pop		de
	ld		hl,(dhere)
	ld		(hl), de
	inc		hl
	inc		hl
	inc		hl
	ld		(dhere), hl
	jp		next

; LITERAL - ( n - ) compiles a number which will be placed 
; on the stack when the current word is executed
lliteral:
	.dl		lcoma
fliteral:
	.db		0
nliteral:
	.db		3,"LITERAL"
xliteral:
	.dl		dliteral
dliteral:
	ld		hl,(dhere)
	ld		de,xlit
	ld		(hl), de
	inc		hl
	inc		hl
	inc		hl
	pop		de
	ld		(hl), de
	inc		hl
	inc		hl
	inc		hl
	ld		(dhere), hl
	jp		next
	
; : begin compiling a new word to dictionary
lcoln:
	.dl		lliteral
fcoln:
	.db		0
ncoln:
	.db		1,":"
xcoln:
	.dl		docolon
dcoln:
	.dl		xcreate
	.dl		xexit

; ; - leave compile mode and complete latest word in dictionary
lsemicol:
	.dl		lcoln
fsemicol:
	.db		1
nsemicol:
	.db		1,";"
xsemicol:
	.dl		docolon
dsemicol: 
	.dl		xbcompile ; add EXIT to end of new word
	.dl		xexit
	.dl		xlit
	.dl		0
	.dl		xstate ; set STATE to interpret
	.dl		xstor
	.dl		xexit

; VARIABLE
lvar:
	.dl		lsemicol
fvar:
	.db		0
nvar:
	.db		8,"VARIABLE"
xvar:
	.dl		docolon
dvar
	.dl		xcrcode
	.dl		xlit
	.dl		32
	.dl		xword ; get name of new word from TIB
	.dl		xswap ; swap name and ptr to next mem space of new word
	.dl		xover ; copy name addr 
	.dl		xcount ; count name of new word
	.dl		x1p ; increment that by one to include the length byte, itself
	.dl		xswap 
	.dl		xdrop ; drop the ptr to the string returned by COUNT
	.dl		xdup ; duplicate length of bytes to copy
	.dl		xhere 
	.dl		xfetch
	.dl		xplus ; add length of bytes to copy to contents of HERE
	.dl		xhere
	.dl		xstor ; update here to point to xt of new word
	.dl		xcmove ; move bytes of length prefixed string to name field of new word
	.dl		xbcompile ; put the address of the dovar routine at HERE and inc HERE
	.dl		dovar
	.dl		xhere
	.dl		xfetch ; put the data field address on the stack
	.dl		xbcompile ; use [COMPILE] to put a long 0 in the data field
	.dl		0
	.dl		xlit
	.dl		0
	.dl		xstate
	.dl		xstor ; crcode set compile mode, so disable it
	.dl		xexit

; TRAV- - ( xt - addr ) addr is a ptr to the length pre-fixed string 
; containing the name of the word whose execution token is xt
ltrav:
	.dl		lvar
ftrav:
	.db		0
ntrav:
	.db		5,"TRAV-"
xtrav:
	.dl		dtrav
dtrav:
	pop		hl
	ld		a,0
	dec		a
trava:
	inc		a
	dec		hl
	ld		c,(hl)
	cp		c
	jr		nz,trava
	push	hl
	jp		next

; IM? - ( xt - flag ) returns Immediate flag field for a given execution token
limq:
	.dl		ltrav
fimq:
	.db		0
nimq:
	.db		3,"IM?"
ximq:
	.dl		dimq
dimq:
	pop		hl
	ld		a,0
	dec		a
imqa:
	inc		a
	dec		hl
	ld		c,(hl)
	cp		c
	jr		nz,imqa
	dec		hl
	ld		bc,0
	ld		c,(hl)
	push	bc
	jp		next
	

; >NUMBER - ( addr n - n2 )
ltonum:
	.dl		limq
ftonum:
	.db		0
ntonum:
	.db		7,">NUMBER"
xtonum:
	.dl		dtonum
dtonum:
	pop		bc
	pop		hl
	ld		e,0
tonuma:
	ld		a,(hl)
	inc		hl
	
	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; call	_Putc
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	dec		c
	sub		a,030h
	
	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; ld		hl,0
	; ld		l,a
	; call	_Disphl
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	cp		0ah
	jr		nc, tonumb
	push	hl
	ld		d,a
	ld		a,e
	call	x10
	ld		e,a
	ld		a,d
	
	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; ld		hl,0
	; ld		l,a
	; call	_Disphl
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	add		a,e
	ld		e,a
	
	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; ld		hl,0
	; ld		l,e
	; call	_Disphl
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	pop		hl
	ld		d,a
	ld		a,0
	cp		c
	jr		z,tonumb
	ld		a,d
	jp		tonuma
tonumb:
	ld		bc,0
	ld		c,e
	push	bc
	
	; push	af
	; push	bc
	; push	de
	; push	hl
	; push	ix
	; push	iy

	; ld		hl,0
	; ld		l,e
	; ld		h,d
	; call	_Disphl
	; call	_NewLine
	; call	_GetKey

	; pop		iy
	; pop		ix
	; pop		hl
	; pop		de
	; pop		bc
	; pop		af

	jp		next
x10:
	ld		l,a 
	sla		a
	sla		a
	sla		a
	ld		h,a
	ld		a,l
	sla		a
	add		a,h
	ret

; branch - ( addr - ) sets 
lbranch:
	.dl		ltonum
fbranch:
	.db		0
nbranch:
	.db		4,"GOTO"
xbranch:
	.dl		dbranch
dbranch:
	ld		hl, (dip)
	inc		hl
	inc		hl
	inc		hl
	ld		de,(hl)
	ld		(dip),de
	ex		de,hl
	ld		ix,(hl)
	ld		(dcxt), ix
	ld		ix, (ix)
	jp		(ix)
	jp		next


; KEY
lkey:
	.dl		lbranch
fkey:
	.db		0
nkey:
	.db		3,"KEY"
xkey:
	.dl		dkey
dkey:
	call	_GetKey
	ld		hl,0
	ld		l,a
	push	hl
	jp		next

; WORDS - ( - ) list words
lwords:
	.dl		lkey
fwords:
	.db		0
nwords:
	.db		5,"WORDS"
xwords:
	.dl		docolon
dwords:
	.dl		xlast
wordsa:
	.dl		xfetch, xdup, xzeq, xif, xdrop, xexit, xthen, xdup
	.dl		xlit, 4, xplus, xcount, xtype, xspc
	.dl		xbranch, wordsa

; FORGET
lforget:
	.dl		lwords
fforget:
	.db		0
nforget:
	.db		6,"FORGET"
xforget:
	.dl		docolon
dforget:
	.dl		xfind, xif, xtrav, x2m, x2m, xdup, xhere, xstor
	.dl		xfetch, xlast, xstor, xthen, xexit

; SEE
lsee:
	.dl		lforget
fsee:
	.db		0
nsee:
	.db		3,"SEE"
xsee:
	.dl		docolon
dsee:
	.dl		xfind, xif 
dseeagain:
	.dl		x2p, xdup, xfetch, xlit, xexit, xequal
	.dl		xif, xdrop, xexit, xthen
	.dl		xtrav, xcount, xtype, xspc, xbranch, dseeagain

; .S - ( n1 n2 n3 - n1 n2 n3 ) view top three items on stack in the order n1, n2, n3
ldots:
	.dl		lsee
fdots:
	.db		0
ndots:
	.db		2,".S"
xdots:
	.dl		ddots
ddots:
	pop		hl
	pop		de
	pop		bc

	push	bc
	push	de
	push	hl

	push	hl
	push	de
	push	bc

	pop		hl
	call	_NewLine
	call	_Disphl
	pop		hl
	call	_NewLine
	call	_Disphl
	pop		hl
	call	_NewLine
	call	_Disphl
	jp		next

; about
labout:
	.dl		ldots
fabout:
	.db		0
nabout:
	.db		5,"ABOUT"
xabout:
	.dl		dabout
dabout:
	jp		dforth

; OPEN - ( n addr -- n2 addr2 )opens APPVAR with name identified 
; by zero terminated string at addr (creates appvar if doesn't exist
; with length n bytes); returns address of appvar space at addr2 and 
; appvar size of n2 bytes
lopen:
	.dl		labout
fopen:
	.db		0
nopen:
	.db		4,"OPEN"
xopen:
	.dl		dopen
dopen:
	pop		hl
	push	hl
	call	_Mov9ToOP1 ; put it in OP1
	call	_ChkFindSym ; look for it in the VAT
	jr 		nc,VarFound 
CreateVar: 
	pop		hl
	push	hl
	call	_Mov9ToOP1
	pop		hl
	pop		bc
	push	bc
	push	hl
	push 	bc
	pop		hl
	call	_CreateAppVar 
	jr 		VarInRam 
VarFound: 
	call	_ChkinRam
	jr		z,VarInRam
	pop		hl
	push	hl
	call	_Mov9ToOP1 
	call	_Arc_Unarc
	jr		dopen ;find again in ram 
VarInRam: 
	ex		de,hl
	ld		de,0
	ld		a,(hl) ; get appvar size
	ld		e,a
	inc		hl
	ld		a,(hl)
	ld		d,a
	inc		hl
	pop		bc ; drop appvar name str ptr
	pop		bc ; drop length
	push	de ; appvar length
	push	hl ; appvar ptr
	jp		next

; SAVE
lsave:
	.dl		lopen
fsave:
	.db		0
nsave:
	.db		4,"SAVE"
xsave:
	.dl		dsave
dsave:
	call	_popRealO1 ; pushed OP1 (with prog name) when program started 
	call	_PushRealO1
	call	_ChkFindSym ; find it in the vat (sets DE as ptr to archive mem)
	push	de ; push start of archive prog mem to stack for later use
	ld		hl,dhere - userMem + 4  
	add		hl,de ; now HL points to dhere in the orig/archived program
	ld		de,(dhere) ; DE is the long value from dhere in the ram prog
	ld		(hl), de ; write DE to dhere in archive

	ld		de,(hl)
	ld		(temp), de

	pop		de ; get start of archive prog mem
	push	de ; push it for later use
	ld		hl,dlast - userMem + 4  
	add		hl,de ; now HL points to dlast in the orig/archived prog
	ld		de,(dlast) ; DE is long value from dlast in ram prog
	ld		(hl), de ; store DE in dlast in the archive prog

	pop		de ; one last time
	ld		hl,dictionary - userMem + 4  
	add		hl,de ; HL points to dictionary in archive prog
	ex		de,hl ; DE points to dictionary in archive prog
	ld		hl,dictionary ; HL points to dictionary in ram prog
	ld		bc,dictionaryend-dictionary ; BC is count of bytes to write
	ldir ; do it
	ld		de,(temp)
	push	de
	jp		next

; test - ( addr -- ) write 10 bytes with char A to addr
ltest:
	.dl		lsave
ftest:
	.db		0
ntest:
	.db		1,"T"
xtest:
	.dl		dtest
dtest:
	ld		a,65
	pop		hl
	ld		b,10
testagain:
	ld		(hl),a
	inc		hl
	djnz	testagain
	jp		next


; HERE - ptr to next available free memory location
lhere:
	.dl		ltest
fhere:
	.db		0
nhere:
	.db		4,"HERE"
xhere:
	.dl		dovar
dhere:
	.dl		dictionary

; LAST - ptr to last word in dictionary
llast:
	.dl		lhere
flast:
	.db		0
nlast:
	.db		4,"LAST"
xlast:
	.dl		dovar
dlast:
	.dl		lforth


; SP - print stack ptr
lpsp:
	.dl		llast
fpsp:
	.db 	0
npsp:
	.db		2,"SP"
xpsp:
	.dl		dpsp
dpsp:
	ld		(temp), sp
	ld		hl,(temp)	
	call	_Disphl
	jp		next

; GK
lgk:
	.dl		lpsp
fgk:
	.db		0
ngk:
	.db		2,"GK"
xgk:
	.dl		dgk
dgk:
	call	_GetKey
	jp		next

lemita:
	.dl		lgk
femita:
	.db		0
nemita:
	.db		1,"A"
xemita:
	.dl		demita
demita:
	ld		a,65
	call	_Putc
	jp		next


lemitb:
	.dl		lemita
femitb:
	.db		0
nemitb:
	.db		1,"B"
xemitb:
	.dl		demitb
demitb:
	ld		a,66
	call	_Putc
	jp		next


lemitc:
	.dl		lemitb
femitc:
	.db		0
nemitc:
	.db		1,"C"
xemitc:
	.dl		demitc
demitc:
	ld		a,67
	call	_Putc
	jp		next


lemitd:
	.dl		lemitc
femitd:
	.db		0
nemitd:
	.db		1,"D"
xemitd:
	.dl		demitd
demitd:
	ld		a,68
	call	_Putc
	jp		next


lemite:
	.dl		lemitd
femite:
	.db		0
nemite:
	.db		1,"E"
xemite:
	.dl		demite
demite:
	ld		a,69
	call	_Putc
	jp		next

; FORTH - displays forth version
lforth:
	.dl		lemite
fforth:
	.db		0
nforth:
	.db		5,"FORTH"
xforth:
	.dl		dforth
dforth:
	call	_NewLine
	ld		hl,abouttxt+1
	call	_Puts
	call	_NewLine
	jp		next

	

