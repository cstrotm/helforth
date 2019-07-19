\ This is an adaption of the matrix multiplication benchmark for using
\ run-time code generation (inspired by lee&leone96)

\ @InProceedings{lee&leone96,
\   author = 	 {Peter Lee and Mark Leone},
\   title = 	 {Optimizing ML with Run-Time Code Generation},
\   crossref =	 {sigplan96},
\   pages =	 {137--148}
\ }
\ @Proceedings{sigplan96,
\   booktitle = 	 "SIGPLAN '96 Conference on Programming Language
\ 		  Design and Implementation",
\   title = 	 "SIGPLAN '96 Conference on Programming Language
\ 		  Design and Implementation",
\   year = 	 "1996",
\   key = 	 "PLDI '96"
\ }

\ The original version is in comments.
\ The results with Gforth on a Nekotech Mach2 (300MHz 21064a) are very nice:
\ original program:		 6.2s user time
\ with run-time code generation: 3.9s user time
\ NOTE: This version needs 160,000+ cells data space
\	and a lot of code space, too.

\ A classical benchmark of an O(n**3) algorithm; Matrix Multiplication
\
\ Part of the programs gathered by John Hennessy for the MIPS
\ RISC project at Stanford. Translated to forth by  Marty Fraeman,
\ Johns Hopkins University/Applied Physics Laboratory.

\ MM forth2c doesn't have it !
: MYBOUNDS  OVER + SWAP ;
: UNDER+ ( A X B -- A+B X )
   ROT + SWAP ;

1 CELLS CONSTANT CELL

VARIABLE SEED

: INITIATE-SEED ( -- )  74755 SEED ! ;
: RANDOM  ( -- N )  SEED @ 1309 * 13849 + 65535 AND DUP SEED ! ;

200 CONSTANT ROW-SIZE
ROW-SIZE CELLS CONSTANT ROW-BYTE-SIZE

ROW-SIZE ROW-SIZE * CONSTANT MAT-SIZE
MAT-SIZE CELLS CONSTANT MAT-BYTE-SIZE

ALIGN CREATE IMA MAT-BYTE-SIZE ALLOT
ALIGN CREATE IMB MAT-BYTE-SIZE ALLOT
ALIGN CREATE IMR MAT-BYTE-SIZE ALLOT

: INITIATE-MATRIX ( M[ROW-SIZE][ROW-SIZE] -- )
  MAT-BYTE-SIZE MYBOUNDS DO
    RANDOM DUP 120 / 120 * - 60 - I !
  CELL +LOOP
;

: GEN-INNERPRODUCT ( A[ROW][*] -- XT )
\ XT IS OF TYPE ( B[*][COLUMN] -- N )
\ THIS WOULD BE A CANDIDATE FOR USING ]] ... [[
 >R :NONAME R>
 0 POSTPONE LITERAL POSTPONE SWAP
 ROW-SIZE 0 DO
   POSTPONE DUP POSTPONE @
   DUP @ POSTPONE LITERAL POSTPONE * POSTPONE UNDER+
   POSTPONE CELL+ ROW-BYTE-SIZE +
  LOOP
  DROP
 POSTPONE DROP POSTPONE ;
;

\ : INNERPRODUCT ( A[ROW][*] B[*][COLUMN] -- INT)
\   0 ROW-SIZE 0 DO ( A B INT )
\     >R OVER @ OVER @ * R> + >R
\     CELL+ SWAP ROW-BYTE-SIZE + SWAP
\     R>
\   LOOP
\   >R 2DROP R>
\ ;

: MAIN  ( -- )
  INITIATE-SEED
  IMA INITIATE-MATRIX
  IMB INITIATE-MATRIX 
  IMR IMA MAT-BYTE-SIZE MYBOUNDS DO
   I GEN-INNERPRODUCT SWAP
    IMB ROW-BYTE-SIZE MYBOUNDS DO ( R XT )
      I 2 PICK EXECUTE OVER ! CELL+
    CELL +LOOP
    NIP \ !! FORGET THE XT
  ROW-SIZE CELLS +LOOP
  DROP
;

\ : MAIN  ( -- )
\   INITIATE-SEED
\   IMA INITIATE-MATRIX
\   IMB INITIATE-MATRIX 
\   IMR IMA MAT-BYTE-SIZE MYBOUNDS DO
\     IMB ROW-BYTE-SIZE MYBOUNDS DO
\       J I INNERPRODUCT OVER ! CELL+ 
\     CELL +LOOP
\   ROW-SIZE CELLS +LOOP
\   DROP
\ ;


MAIN
BYE
