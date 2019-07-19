; =================================================
;  HelFORTH - runtime core
;      File: runtime.asm
;   Version: 0.4
;    Author: Helmar Wodtke
;      Year: 2004
; Assembler: fasm
; -------------------------------------------------
CCT:				; INLINE state	| -- addr ||| Returns the address of current context table.
	sub esi,4
	mov [esi],eax
	mov eax,ebx
	ret
to_r:				; INLINE >r	| n -- ||| Store n to the return stack.
	push eax
	lodsd
	ret
from_r:				; INLINE r>	| -- n ||| Get top-most entry of return stack and remove it from there.
	sub esi,4
	mov [esi],eax
	pop eax
	ret
w_r:				; INLINE r	| -- n ||| Get top-most entry of return stack (don't remove it).
	sub esi,4
	mov [esi],eax
	mov eax,[esp]
	ret
here:				; INLINE here	| -- addr ||| Get current heap postition.
	sub esi,4
	mov [esi],eax
	mov eax,edi
	ret
question_dup:			; WORD ?dup	| n|0 -- (n n)|0
	or eax,eax
	jz w_dup.q
w_dup:				; INLINE dup	| n -- n n ||| Duplicate top of stack entry.
	sub esi,4
	mov [esi],eax
.q:
	ret
fetch:				; INLINE @	| addr -- n ||| Fetch cell value on addr position in memory.
	mov eax,[eax]
	ret
set:				; INLINE !	| n addr -- ||| Set cell value on addr.
	xchg eax,edx
	lodsd
	mov [edx],eax
	lodsd
	ret
c_fetch:			; INLINE c@	| addr -- c ||| Get (unsigned) byte value on addr.
	xchg eax,edx
	xor eax,eax
	mov al,[edx]
	ret
c_set:				; INLINE c!	| c addr -- ||| Set byte value c on addr.
	xchg eax,edx
	lodsd
	mov [edx],al
	lodsd
	ret
w_add:				; INLINE +	| n1 n2 -- sum ||| add n1 and n2, store the result sum to stack.
	add eax,[esi]
	add esi,4
	ret
w_negate:			; INLINE negate	| n1 -- -n1 ||| Negate top of stack value.
	neg eax
	ret
w_sub:				; INLINE -	| n1 n2 -- n1-n2 ||| Subtract n2 from n1 and store result to stack.
	sub [esi],eax
	lodsd
	ret
swap:				; INLINE swap	| n1 n2 -- n2 n1 ||| Exchange top of stack value with second value on stack.
	mov edx,eax
	mov eax,[esi]
	mov [esi],edx
	ret
one_comma:			; INLINE 1,	| c -- ||| Store byte c on heap.
	stosb
drop:				; INLINE drop	| n -- ||| Remove top-most stack entry.
	lodsd
	ret
two_comma:			; INLINE 2,	| w -- ||| Store word w on heap.
	stosw
	lodsd
	ret
thr_comma:			; WORD 3,	| n -- ||| Store 3 byte value n on heap.
	stosb
	shr eax,8
	stosw
	lodsd
	ret
comma:				; INLINE ,	| n -- ||| Store n on heap.
	stosd
	lodsd
	ret
allot:				; INLINE allot	| cnt -- ||| Allocate n bytes on heap.
	add edi,eax
	lodsd
	ret
sharp:			   DOER ; WORD (#)	| n -- ||| Output representation of n.
	xor ecx,ecx
.a:
	xor edx,edx
	div d[ebx + v_base]
	push edx
	inc ecx
	or eax,eax
	jnz .a
.b:
	pop eax
	add al,"0"
	cmp al,"9"+1
	jc .c
	add al,-"0" + "A" - 10
.c:
	push ecx
	call emit
	pop ecx
	sub esi,4
	mov [esi],eax
	loop .b
	lodsd
	ret
null_semi:			; WORD 0; | 0/n -- /n ||| Terminate word if top of stack is zero. Remove top of stack in this case.
	or eax,eax
	jnz not_null
	pop eax
	lodsd
not_null:
	ret
vector:			   DOER ; MACRO vector | -- || -- ||| Compile execution vector.
	push eax
	mov al,0e9h
	stosb
	xor eax,eax
	stosd
	pop eax
	ret
leftshift:			; INLINE <<	| n1 c -- n1' ||| Shift n1 c bits to the left.
	xchg eax,ecx
	lodsd
	shl eax,cl
	ret
rightshift:			; INLINE >>	| n1 c -- n1' ||| Shift n1 c bits to the right.
	xchg eax,ecx
	lodsd
	shr eax,cl
	ret
w_or:				; INLINE or	| -- ||| Output a line feed.
	xchg eax,ecx
	lodsd
	or eax,ecx
	ret
w_and:				; INLINE and	| n1 n2 -- n ||| Bitwise "and" n1 and n2.
	xchg eax,ecx
	lodsd
	and eax,ecx
	ret
w_xor:				; INLINE xor	| n1 n2 -- n ||| Bitwise "xor" n1 and n2.
	xchg eax,ecx
	lodsd
	xor eax,ecx
	ret
w_mul:				; INLINE *	| n1 n2 -- n ||| Multiply n1 and n2.
	xchg eax,ecx
	lodsd
	mul ecx
	ret
tuck:				; INLINE tuck	| n1 n2 -- n2 n1 n2 ||| Store top of stack value under the second value of the stack.
	mov ecx,eax
	xchg eax,[esi]
	sub esi,4
	mov [esi],eax
	xchg eax,ecx
	ret
w_cmove:			; WORD cmove	| addr1 addr2 c -- ||| Move the values in memory area addr1 to addr1 + c to memory area addr2. c is unsigned. The operation will be done byte by byte from left to right.
	xchg eax,ecx
	lodsd
	push edi
	xchg eax,edi
	lodsd
	push esi
	xchg eax,esi
	rep movsb
	pop esi
	pop edi
	lodsd
	ret
two_plus:			; INLINE 2+	| n -- n1 ||| Increment top of stack entry by 2.
	inc eax
one_plus:			; INLINE 1+	| n -- n1 ||| Increment top of stack entry by 1.
	inc eax
	ret
two_minus:			; INLINE 2-	| n -- n1 ||| Decrement top of stack entry by 2.
	dec eax
one_minus:			; INLINE 1-	| n -- n1 ||| Decrement top of stack entry by 1.
	dec eax
	ret
plus_set:			; INLINE +!	| n addr -- ||| Increment cell at addr by n.
	xchg eax,edx
	lodsd
	add [edx],eax
	lodsd
	ret
w_div:				; INLINE /	| n1 n2 -- n ||| Divide n1 by n2.
	xchg eax,ecx
	lodsd
	cdq
	idiv ecx
	ret
w_mod:				; INLINE mod	| n1 n2 -- n ||| Store reminder of the division of n1 and n2 to stack.
	xchg eax,ecx
	lodsd
	cdq
	idiv ecx
	xchg eax,edx
	ret
w_div_mod:			; WORD /MOD	| n1 n2 -- r n ||| Divide n1 by n2. Store result n and reminder r to stack.
	call w_div
	sub esi,4
	mov [esi],edx
	ret
w_on:				; INLINE on	| addr -- ||| Same as: -1 addr !
	or d[eax],-1
	lodsd
	ret
w_off:				; INLINE off	| addr -- ||| Same as: 0 addr !
	and d[eax],0
	lodsd
	ret
w_not:				; INLINE not	| n -- n^-1
	not eax
	ret
nip:				; INLINE nip	| n1 n2 -- n2 ||| Same as: swap drop
	add esi,4
	ret
two_swap:			; WORD 2swap	| n1 n2 n3 n4 -- n3 n4 n1 n2
	xchg [esi + 4],eax
	mov edx,[esi]
	xchg edx,[esi + 8]
	mov [esi],edx
	ret
scratch_pad:			; WORD pad	| -- addr ||| returns temporary buffer (256 bytes)
	sub esi,4
	mov [esi],eax
	mov eax,[ebx + v_spad]
	add eax,256
	mov edx,[ebx + v_strs]
	add edx,256
	cmp eax,edx
	jc roll_pad
	add edx,16 * 256	; allow 16 scratch pads
	cmp eax,edx
	jc scratch_pad_q
roll_pad:
	mov eax,[ebx + v_strs]
	add eax,256
scratch_pad_q:
	mov [ebx + v_spad],eax
	ret
fill:                          ; WORD fill
	push edi
	mov edi,[esi + 4]
	mov ecx,[esi]
	rep stosb
	pop edi
	add esi,8
	lodsd
	ret
w_sz:				; INLINE sz
	sub esi,4
	mov [esi],eax
	dec eax
.a:	inc eax
	cmp byte [eax],0
	jnz .a
	sub eax,[esi]
	ret
cell_plus:			; INLINE cell+
	add eax,4
	ret
five_plus:			; INLINE 5+
	add eax,5
	ret
fetch_fetch:			; INLINE @@
	sub esi,4
	mov edx,[eax]
	mov [esi],edx
	mov eax,[eax+4]
	ret
ndrop:				; INLINE ndrop
	lea esi,[eax*4+esi]
	lodsd
	ret
s_plus_plus:			; INLINE s++
	inc d[esi]
	dec eax
	ret
cell_minus:			; INLINE cell-
	sub eax,4
	ret
cells:				; INLINE cells
	shl eax,2
	ret
w_over:				; INLINE over
	sub esi,4
	mov [esi],eax
	mov eax,[esi+4]
	ret
w_rp:				; INLINE rp
	sub esi,4
	mov [esi],eax
	mov eax,esp
	ret
w_nop:				; INLINE nop
	nop
	ret
fetch_r:			; INLINE @r
	xchg eax,edx
	lea eax,[edx+4]
	add eax,[edx]
	ret
w_equal:			; WORD equal
	mov edx,[esi+4]
.a:
	sub d[esi],1
	jc .b
	mov cl,[eax]
	inc eax
	cmp [edx],cl
	lea edx,[edx+1]
	jz .a
	xor eax,eax
.b:
	add esi,8
	ret
to_number:			; WORD >number | d addr c -- d' addr' c'
	push edi
	mov edi,[esi]
.a:
	test eax,eax
	jz .q
	xor edx,edx
	mov dl,[edi]
	call cvdig
	jc .q
	inc edi
	dec eax
	push eax
	mov ecx,edx
	xor edx,edx
	mov eax,[esi+8]
	mul d[ebx+v_base]
	push eax
	xor edx,edx
	mov eax,[esi+4]
	mul d[ebx+v_base]
	add eax,ecx
	pop ecx
	adc edx,ecx
	mov [esi+4],eax
	mov [esi+8],edx
	pop eax
	jmp .a
.q:
	mov [esi],edi
	pop edi
	ret
; =========================================================
