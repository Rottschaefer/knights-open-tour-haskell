##Para rodar execute 'make run' no terminal

GHC = ghc

knight: main.hs
	$(GHC) -O2 -o knight main.hs

run: knight
	./knight exe.txt

clean:
	rm -f knight *.o *.hi
