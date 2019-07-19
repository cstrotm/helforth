; =================================================
;  HelFORTH - core
;      File: core.asm
;   Version: 0.4b
;    Author: Helmar Wodtke
;      Year: 2004
; Assembler: fasm
; -------------------------------------------------

; register conventions:
;	EAX = TOS
;	EBX = current context table
;	ESI = stack
;	EDI = heap

; ----- some useful macros ------------------------

d equ dword
b equ byte
w equ word
macro DOER { db $E9, 0, 0, 0, 0 }
macro Dict n,l {
	db n
local inq
inq equ (DicID + l - cct)
	db (inq / 256)
	db (inq and 255)
DicID equ 8000h
}

; -------------------------------------------------

	call init

; ------- link block ----------------------------------------

v_mode = 0
v_last = 4
v_base = 8
v_lkey = 12
v_tibb = 16
v_tibc = 20
v_dics = 24
v_strs = 28
v_dats = 32
v_stck = 36
v_lcal = 40
v_inli = 44
v_exep = 48
v_macp = 52
v_defp = 56
v_dic0 = 60
v_pref = 64
v_spad = 68

v_lsca = 72
v_lscn = 76
v_valu = 80
v_valh = 84
v_unkn = 88
v_unkh = 92
v_nowo = 96
v_nowh = 100
v_dici = 104

; --------------------------------------

; current context table

cct:
  dd 0			; mode	;   ; 0 = run 1 = compile
  dd 0			; last	;   ; pointer to current dict entry
  dd 10			; base	;   ; base of numbers
  dd 0			; lkey	;   ; next value returned by key or 0
  dd 0			; tibb	;   ; terminal input buffer
  dd 0			; tibc	;   ; terminal input counter
  dd dics		; dics	;   ; dictionary pointer
  dd strs		; strs	;   ; string pointer
  dd dats		; dats	; * ; heap pointer
  dd stck		; stck	; * ; stack pointer
  dd 0			; lcal	; c ; last compiled call
  dd question_inline	; inli	; c ; inline handler
  dd question_execute	; exep	; c ; word handler
  dd execute		; macp	; c ; macro handler
  dd question_execute	; defp	; c ; default : handler
  dd dics		; dic0	; c ; start of dictionary space
  dd 2			; pref  ; c ; preferences
  dd 0			; spad  ;   ; last scratch pad
  dd 0			; lsca  ;
  dd 0			; lscn  ;
  dd 0                  ; valu  ;
  dd question_literal_comma
  dd 0			; unkn	;
  dd word_question
  dd 0			; nowo	;
  dd noword

p_last_is_space = 4	; after reading from tib insert space

; dici... initial compressed dictionary

DICTIONARY

; ===========================================================
; ------- system interface ----------------------------------

INTERFACE

if ~HAS_CORE_INTERFACE

; --- Solaris interface:

s_bye:			   	; WORD (Sbye)	| -- ||| Exits FORTH.
	pop edx
	push eax
	push edx
	xor eax,eax
	inc eax
	int 91h
s_emit:			   	; WORD (Semit)	| c -- ||| Output character c.
	sub esi,4
	mov [esi],eax
	xor eax,eax
	inc eax
	push eax
	push esi
	push eax
	push eax ; dummy return
	mov al,4
	int 91h
	lodsd
_sysc_ex:
	add esp, 16
	lodsd
	ret
s_syskey:		  	; WORD (SSysKey)	| -- c ||| Get input character using system routine.
	sub esi,8
	mov [esi+4],eax
	xor eax,eax
	mov d[esi],eax
	inc eax
	push eax
	push esi
	dec eax
	push eax
	push eax ; dummy return
	mov al,3
	int 91h
	jmp _sysc_ex

; --- Linux interface:

bye:			  DOER ; WORD bye	| -- ||| Exits FORTH.
	xor ebx,ebx
	xor eax,eax
	inc eax
	int 80h

emit:			  DOER ; WORD emit	| c -- ||| Output character c.
	sub esi,4
	mov [esi],eax
	xor edx,edx
	inc edx
	mov ecx,esi
	push ebx
	mov ebx,edx
	xor eax,eax
	mov al,4
	int 80h
	pop ebx
	lodsd
	lodsd
	ret

syskey:			DOER ; WORD (SysKey)	| -- c ||| Get input character using system routine.
	sub esi,8
	mov [esi+4],eax
	and d[esi],0
	xor edx,edx
	inc edx
	mov ecx,esi
	push ebx
	xor ebx,ebx
	xor eax,eax
	mov al,3
	int 80h
	pop ebx
	lodsd
	ret

end if ; HAS_CORE_INTERFACE

; Linux system calls
; eax				syscall number
; ebx, ecx, edx, esi, edi	parameters (strings are asciiz)

; ecx, edx and ebp are defined as to be scratch registers in HelFORTH

w_syscall:			; WORD (SysCall)
	push ebx
	push edi
	push eax
	lodsd
	or eax,eax
	jz syscall_
	xchg eax,edi
	lodsd
	xchg eax,ebx
	dec edi
	jz syscall_
	lodsd
	xchg eax,ecx
	dec edi
	jz syscall_
	lodsd
	xchg eax,edx
	dec edi
	jz syscall_
	lodsd
	push eax
	dec edi
	jz syscall__
	lodsd
	push eax
	dec edi
	jz syscall___
	lodsd
	xchg eax,ebp
syscall___:
	pop edi
syscall__:
	pop eax
	xchg esi,[esp]
	xchg eax,esi
	int 80h
	pop esi
	pop edi
	pop ebx
	ret
syscall_:
	pop eax
	int 80h
	pop edi
	pop ebx
	ret

szcpy:
	push edi
	sub edx,ecx
	dec edx
	mov edi,edx
	rep movsb
	mov b[edi],0
	pop edi
	ret 

szsyscall:			; WORD (SZSysCall)
	lea edx,[esi - 32]
	mov ecx,[esi + 4]
	push esi
	mov esi,[esi + 8]
	call szcpy
	pop esi
	mov [esi + 8],edx
	push d[esi]
	add esi,4
	pop d[esi]
	jmp w_syscall

szszsyscall:			; WORD (SZSZSysCall)
	lea edx,[esi - 32]
	mov ecx,[esi + 4]
	push esi
	mov esi,[esi + 8]
	call szcpy
	pop esi
	mov ecx,edx
	xchg ecx,[esi + 12]
	push esi
	mov esi,[esi + 16]
	call szcpy
	pop esi
	mov [esi + 16],edx
	push d[esi]
	add esi,8
	pop d[esi]
	jmp w_syscall

; ===========================================================
; ------ runtime core ---------------------------------------

key:			   DOER ; WORD key	| -- c ||| Get key.
	cmp d[ebx + v_lkey],0
	jz key_
	sub esi,4
	mov [esi],eax
	mov eax,[ebx + v_lkey]
	and d[ebx + v_lkey],0
	ret
key_:
	cmp d[ebx + v_tibc],0
	jz key__
key_tib:
	sub esi,4
	mov [esi],eax
	mov edx,[ebx + v_tibb]
	inc d[ebx + v_tibb]
	dec d[ebx + v_tibc]
	xor eax,eax
	mov al,[edx]
	ret
key__:					DOER ; WORD (MoreInput)
MoreInputIndicator = $ - 4
	test d[ebx + v_pref],p_last_is_space
	jz key___
	and d[ebx + v_pref],-1 - p_last_is_space
	sub esi,4
	mov [esi],eax
	xor eax,eax
	mov al,32
	ret
key___:
	call bc_token
	mov [ebx + v_tibc],eax
	lodsd
	or eax,eax
	mov [ebx + v_tibb],eax
	lodsd
	jnz key_tib
	jmp syskey
s_comma:			; WORD s,	| addr cnt -- ||| Compile string (as counted string).
	stosb
	xchg eax,ecx
	push esi
	mov esi,[esi]
	rep movsb
	pop esi
two_drop:			; INLINE 2drop	| n1 n2 -- ||| Remove first two stack entries.
	lodsd
	lodsd
	ret
cvdig:
	or dl, 32
	sub dl, "0"
	jc .q
	cmp dl, 10
	jc .q1
	add dl, "0" - "a" + 10
.q1:
	cmp edx, [ebx + v_base]
	cmc
.q:
	ret
to_num:			   DOER ; WORD >num	| addr cnt -- 0/(n -1) ||| Convert string to number.
	push d[ebx + v_base]
	xchg eax,ecx
	lodsd
	push esi
	xchg esi,eax
	push esi
	cmp b[esi],"-"
	jnz no_sign
	inc esi
	dec ecx
no_sign:
	xor eax,eax
	cmp b[esi],"'"
	jnz no_char
	mov al,[esi + 1]
number_q:
	pop esi
	cmp b[esi], "-"
	jnz number_not_signed
	neg eax
number_not_signed:
	pop esi
	pop d[ebx + v_base]
	sub esi,4
	mov [esi],eax
	xor eax,eax
	dec eax
	ret
no_char:
	cmp b[esi],"$"
	jnz number_test_oct
	mov d[ebx + v_base], 16
	inc esi
	jmp number_do_loop
number_test_oct:
	cmp b[esi],"&"
	jnz number_convert
	mov d[ebx + v_base],8
	inc esi
	jmp number_do_loop
number_convert:
	mul d[ebx + v_base]
	xor edx,edx
	mov dl,[esi]
	inc esi
	call cvdig
	jc no_digit
	add eax,edx
number_do_loop:
	loop number_convert
	jmp number_q
no_digit:
	pop esi
	pop esi
	pop d[ebx + v_base]
	xor eax,eax
	ret
two_dup:			; INLINE 2dup	| n1 n2 -- n1 n2 n1 n2 ||| Duplicate first two stack entries.
	mov edx,[esi]
	sub esi,8
	mov [esi+4],eax
	mov [esi],edx
	ret
type:			   DOER ; WORD type	| addr cnt -- ||| Output string.
	or eax,eax
type_:
	jz two_drop
	mov edx,[esi]
	sub esi,4
	mov [esi],eax
	xor eax,eax
	mov al,[edx]
	call emit
	inc dword [esi]
	dec eax
	jmp type_

include "runtime.asm"

; =========================================================
; ---- compiler -------------------------------------------

parse_word:		  DOER ; WORD wsparse
	sub esi, 4
	mov [esi], eax
	mov eax, 32

parse:			   DOER ; WORD parse	| c -- addr cnt ||| Parse token from input (c will be skipped at begin and it terminates the token).
	mov edx,[ebx + v_strs]
	inc edx
	sub esi,4
	mov [esi],edx
parse___:
	call pkey
	jecxz parse_q
	cmp al,10
	jz parse_10
	cmp al,32
	jnz parse_10
	cmp cl,33
	jc parse___
parse__:
	call pput
	jecxz parse_q
	cmp cl,33
	jnc parse__
	cmp cl,10
	jnz parse_q
	mov [ebx + v_lkey],ecx
parse_q:
	xchg eax,edx
	mov edx,[esi]
	sub eax,edx
	mov b[edx-1],al
	ret
parse_:
	call pput
	jecxz parse_q
parse_10:
	cmp al,cl
	jnz parse_
	jmp parse_q
pput:
	mov [edx],cl
	inc edx
pkey:
	push edx
	call key
	cmp al,13
	jnz pkey_
	lodsd
	call key
	cmp al,10
	jz pkey_
	mov [ebx + v_lkey], eax
	xor eax,eax
	mov al,32
pkey_:
	pop edx
	xchg eax,ecx
	lodsd
	ret
question_inline:		; WORD ?inline | add -- ||| Execute or inline
	cmp d[ebx + v_mode],0
	jz execute
w_inline:			; WORD inline | add -- ||| inline the code
	mov d[ebx + v_lcal], edi
	xchg eax, esi
.a:
	cmp b[esi], 0c3h
	movsb
	jnz .a
	dec edi
	xchg eax, esi
	lodsd
	ret
question_execute:		; WORD ?execute	| addr -- ||| Execute or compile depending on mode.
	cmp d[ebx + v_mode],0
	jnz compile
execute:			; WORD execute	| addr -- ||| Execute code at addr.
	xchg eax,edx
	lodsd
	jmp edx
	nop
compile:		   DOER ; WORD compile	| addr -- ||| Compile call to addr.
	mov [ebx + v_lcal],edi
	mov b[edi],0E8h
	inc edi
	sub eax,edi
	sub eax,4
	stosd
	lodsd
	ret
create:			   DOER ; WORD create	| -- ||| Create word from next token.
 	sub esi,4
	mov [esi],eax
	xor eax,eax
	mov al,32
	call parse
creates:		   DOER ; WORD creates	| addr cnt -- ||| Create word from string.
	xchg edi,[ebx + v_dics]
	push eax
	mov eax,[ebx + v_last]
	mov [ebx + v_last],edi
	stosd				; link to previous
	pop eax
	call s_comma			; string
	sub esi,4
	mov [esi],eax
	mov eax,[ebx + v_dics]
	stosd				; "tick" information
	call create_
question_literal_comma:		; WORD ?literal	| n -- n/ ||| Compiles a literal depending on mode.
	cmp d[ebx + v_mode], 0
	jz ql_q							; call literal_comma
literal_comma:		   DOER ; MACRO literal	| n -- ||| Compile literal.
	call dup_comma
	mov b[edi], 0b8h
	inc edi
	stosd
	lodsd
ql_q:
	ret
dup_comma:		  DOER ; WORD dup, | -- ||| compile dup code
	mov d[edi], 08904ee83h
	mov b[edi+4], 06h
	add edi,5
	ret
create_:
	pop eax
	stosd				; link to handler
	mov eax,edi
	sub eax,[ebx + v_last]
	stosb				; count to header
	lodsd
	xchg edi,[ebx + v_dics]
	ret
ddot:			   DOER ; MACRO :	| -- ||| Create new word and turn on compilation mode.
	call align_
	call create
	mov edx,[ebx + v_last]
	xor ecx,ecx
	mov cl,[edx + 4]
	push d[ebx + v_defp]
	pop d[ecx + edx + 9]
mode_on:			; WORD ]	| -- ||| Turn on compilation mode,
	or d[ebx + v_mode],-1
	ret
mode_off:			; MACRO [	| -- || -- ||| Turn off compilation mode.
	and d[ebx + v_mode],0
	ret
set_forth:		   DOER ; WORD forth	| -- ||| Set : creation mode to default forth words.
	push d[ebx + v_exep]
	pop d[ebx + v_defp]
	ret
set_macro:		   DOER ; WORD macro	| -- ||| Set : creation mode to macro compilation.
	push d[ebx + v_macp]
	pop d[ebx + v_defp]
	ret
comment:			; MACRO |	| -- || -- ||| Ignore contents up to line end (comment).
	sub esi,4
	mov [esi],eax
	xor eax,eax
	mov al,10
	call parse
	jmp two_drop
semi_semi:		   DOER ; MACRO ;;	| -- || -- ||| Compile termination of word.
	lea ecx,[edi - 5]
	cmp [ebx + v_lcal], ecx
	jnz semi_semi_ret
	cmp byte [ecx], 0E8h
	jnz semi_semi_ret
	and d[ebx + v_lcal],0
	mov edx,[ebx + v_last]
	push eax
	xor eax,eax
	mov al,[edx + 4]
	add edx,eax
	pop eax
	cmp ecx,[edx+5]
	jnz semi_jump
	mov ecx,[edi - 4]
	add ecx,edi
	mov [edx+5],ecx
	sub edi,5
	ret
semi_jump:
	mov dl,0e9h
	cmp d[ecx+1],-128-3
	jc semi_j
	mov dl,0ebh
	add b[ecx+1],3
	sub edi,3
semi_j:
	mov b[ecx], dl
	jmp align_
semi_semi_ret:			; MACRO ;;ret	| -- || -- ||| Compile termination of word without optimization.
	mov b[edi],0c3h
	inc edi
align_:			   DOER ; WORD align	| -- || -- ||| align code
.a:
	test edi, 15
	jz .b
	mov b[edi],$90
	inc edi
	jmp .a
.b:
	ret
; ---------------------------------------------------------

expand_dictionary:	; called on startup to expand the dictionary
	push eax
	lea esi,[ebx + v_dici]
	mov edi,[ebx + v_dics]
	mov ecx,[ebx + v_last]
	lea edx,[ebx + v_inli]
.a:
	mov [edi],ecx
	mov ecx,edi
	add edi,5
.b:
	test b[esi],80h
	jnz .c
	movsb
	jmp .b
.c:
	lodsw
	test al,40h
	jz .d
	add edx,4
.d:
	xchg ah,al
	and eax,3FFFh
	add eax,ebx
	stosd
	mov eax,[edx]
	stosd
	mov eax,edi
	sub eax,ecx
	stosb
	sub al,13
	mov [ecx + 4],al
	cmp b[esi],0		; terminator
	jnz .a
	mov [ebx + v_dics],edi
	mov [ebx + v_last],ecx
	pop eax
	ret

; ---------------------------------------------------------

bc_token:			; -- (addr cnt)/(0 0)
	sub esi,8
	mov [esi+4],eax
	mov edx,[bc_i]
	xor eax,eax
	mov al,[edx]
	cmp al,0
	jnz .a
	mov [esi],eax
	ret
.a:
	inc d[bc_i]
	cmp al,255
	jnz .b
	mov al,[edx + 1]
	inc d[bc_i]
	push ecx
	push edi
	mov edi,bc_dlit
	mov [esi],edi
	inc edi
	push eax
	shr eax,4
	push eax
	mov ecx,2
.cv:
	pop eax
	and al,15
	add al,'0'
	cmp al,'9'+1
	jc .cva
	add al,-'0'+'a'-10
.cva:
	stosb
	loop .cv
	pop edi
	pop ecx
	mov al,3
	jmp .lspc
.b:
	cmp al,32
	jc .c
	mov edx,[eax * 4 + bc_cache]
	mov al,[edx - 1]
.setspc:
	mov [esi],edx
	cmp al,15
	jz .q
.lspc:
	or d[ebx + v_pref],p_last_is_space
.q:
	ret
.c:
	inc edx
	test al,16
	jnz .d
	push eax
	mov eax,[bc_ii]
	inc eax
	cmp al,255
	jc .noroll
	mov al,32
.noroll:
	mov [bc_ii],eax
	mov  [eax * 4 + bc_cache],edx
	and d[eax * 4 + bc_find],0
	pop eax
.d:
	and eax,15
	add [bc_i],eax
	jmp .setspc

; ------------------------------------------------------------

find:					DOER ; WORD find
	push esi
	push edi
	mov ebp, [esi]
	mov [ebx + v_lsca], ebp
	mov [ebx + v_lscn], eax
	mov ah, [ebp]
	lea edx, [ebx + v_last]
	xor ecx,ecx
.a:	mov edx, [edx]
	cmp edx, ecx ; ECX is always <= 255
	jna .q1
	cmp [edx + 4], ax
	jnz .a
	mov cl, al
	lea edi, [edx + 5]
	mov esi, ebp
	repz cmpsb
	jnz .a
.q:	xchg eax, edi
.q2:	pop edi
	pop esi
	add esi, 4
	ret
.q1:	xor eax,eax
	jmp .q2

question:				DOER ; WORD ?
	sub esi,4
	mov [esi],eax
	xor eax,eax
	cmp d[ebx + v_tibc], eax
	jnz .not_suitable
	cmp d[MoreInputIndicator], eax
	jnz .not_suitable
	mov edx,[bc_i]
	mov al,[edx]
	cmp al,0
	jz .not_suitable
	cmp al,15
	jc .new_token
	cmp al,32
	jc .not_suitable
	cmp al,255
	jz .not_suitable
	lea edx,[eax * 4 + bc_find]
	cmp d[edx],0
	jz .not_cached
	and d[ebx + v_pref],-1 - p_last_is_space
  	inc d[bc_i]
 	mov eax,[edx]
	ret
.new_token:
	mov eax,[bc_ii]
	inc eax
	cmp al,255
	jc .dont_roll
	mov al,32
.dont_roll:
	lea edx,[eax * 4 + bc_find]
.fetch_next:
	push edx
	lodsd
	call bc_token
	and d[ebx + v_pref],-1 - p_last_is_space
	call find
	pop edx
	mov [edx],eax
	jmp .cleanup
	ret
.not_cached:
	mov ecx,[edx + bc_cache - bc_find]
	cmp b[ecx - 1],15
	jc .fetch_next
.not_suitable:
	lodsd
	call parse_word
;	call xwsparse
        or eax, eax
	jz .noword1
	call find
.cleanup:
	or eax, eax
	jz .is_value
	ret
.noword1:
	lodsd
.noword:
	lea eax, [ebx + v_nowo]
	ret
.unknown:
	lea eax, [ebx + v_unkn]
	ret
.is_value:
	mov eax, [ebx + v_lscn]
	or eax, eax
	jz .noword
	mov ecx, [ebx + v_lsca]
	xor edx, edx
	mov dl, [ecx]
	cmp dl, "'"
	jz .number
	cmp dl, "-"
	jz .number
	cmp dl, "$"
	jz .number
	cmp dl, "&"
	jz .number
	call cvdig
	jc .unknown
.number:
	sub esi, 4
	mov [esi], ecx
	call to_num
	or eax, eax
	jz .unknown
  	lodsd
	mov [ebx + v_valu], eax
	lea eax, [ebx + v_valu]
	ret
tick:					DOER ; WORD '
	call question
	mov eax, [eax]
	ret

; =========================================================
; -------- interpret and init -----------------------------

init:
	pop ebx		; set up link block

	mov ecx,512
	xor eax,eax
	mov edi,bc_cache
	rep stosd

	call expand_dictionary
	mov esi,[ebx + v_stck]
	mov edi,[ebx + v_dats]
	call interpret
	jmp bye

word_question:				DOER ; WORD word?
	sub esi, 8
	mov eax, [ebx + v_lsca]
	mov [esi], eax
	mov eax, [ebx + v_lscn]
	call type
	mov eax, "?"
	call emit
cr:					DOER ; WORD cr	| -- ||| Output line feed.
	sub esi,4
	mov [esi],eax
	xor eax,eax
	mov al,10
	jmp emit
noword:					; INLINE rdrop
	pop edx
	ret
interpret:				DOER ; WORD interpret
	call question
	push interpret
exec:					; WORD exec
	xchg eax, edx
	mov eax, [edx]
	jmp d[edx + 4]

; =========================================================
; ---- data -----------------------------------------------

initial_bytecode: BOOTSTRAP

bc_dlit:	db "$00"
bc_i:		dd initial_bytecode
bc_ii:		dd 31

hfds_section

align 16
bc_cache:	rd 256			; Cache for bytecode
bc_find:	rd 256			; find Cache for bytecode

dics:	rb 128 * 1024			; 128 K dictionary data
dats:	rb 2024 * 1024			; 2024 K heap data
strs:	rb 128 * 1024			; 128 K string data
s1:	rb  4 * 1024			; 4 K stack
stck:	rb        40			; 10 entries underflow space for stack

; =========================================================
