PFE = pfe
GFORTH = gforth-fast --dynamic
LINA = sh -c "cat linafix.fs - | ./lina" <

TESTS = bubble-sort.fs \
        fib.fs \
        matrix-mult.fs \
        sieve.fs \
        mm-rtcg.fs

all:
	@echo Running benchmarks...
	sh -c "time make bench"

allpfe:
	@echo Running benchmarks with PFE...
	@sh -c "time make pfebench"

allgforth:
	@echo Running benchmarks with GFORTH...
	@sh -c "time make gforthbench"

alllina:
	@echo Running benchmarks with LINA...
	@sh -c "time make linabench"


bench:
	for a in $(TESTS) ; do echo test: $$a ...; time ../hf-ans <$$a ; done

pfebench:
	for a in $(TESTS) ; do echo test: $$a ...; time $(PFE) $$a ; done

gforthbench:
	for a in $(TESTS) ; do echo test: $$a ...; time $(GFORTH) $$a ; done

linabench:
	for a in $(TESTS) ; do echo test: $$a ...; time $(LINA) $$a ; done

clean:
	-rm *~

distclean: clean
