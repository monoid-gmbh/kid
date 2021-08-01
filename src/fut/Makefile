.PHONY: pkgs clean

sources: Calculation.c

libKid.so: Calculation.c
	gcc Calculation.c -o libKid.so -fPIC -shared

Calculation.c:
	futhark c --library Calculation.fut

Calculation.py:
	futhark python --library Calculation.fut

pkgs:
	futhark pkg add github.com/diku-dk/cpprandom
	futhark pkg add github.com/diku-dk/statistics
	futhark pkg add github.com/diku-dk/linalg
	futhark pkg add github.com/monoid-gmbh/kid-annexes
	futhark pkg sync

clean:
	rm -f *.c *.h *.py *.o *.so
