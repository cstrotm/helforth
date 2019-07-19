ALLEXT = hf-nano hf-pico plain
ALL = hf hf-mega hf-ans
ASMFILES = core.asm runtime.asm
CFLAGS = -Os -Wall

all: $(ALL)

allext: $(ALL) $(ALLEXT)

%: %.h4 bin/fasm $(ASMFILES)
	bin/build -o $* $* footer.h4

hf-ans: hf-ans.h4 bin/fasm $(ASMFILES)
	bin/build -o hf-ans hf-ans

bin/fasm: bin/fasm.o
	gcc bin/fasm.o -o bin/fasm
	chmod +x bin/fasm

c: c.c cwords.o
	$(CC) $(CFLAGS) c.c cwords.o -o c
	strip c
cwords.c: cwords.ci 2source.pl
	perl 2source.pl <cwords.ci >cwords.c

clean:
	-rm *~ core

distclean: clean
	-rm $(ALL) $(ALLEXT)
	-rm bin/*~ bin/core DEADJOE bin/DEADJOE
	( cd examples ; make distclean )
#	( cd ANS ; make distclean )

distro:
	make distclean
	make all
	rm bin/fasm

depend:
	bin/mkdep.sh $(ALL) $(ALLEXT)

include Deps.mk
