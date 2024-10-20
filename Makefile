all:
	rm -rf generated
	mkdir generated
	cp src/*.hs generated/
	cd generated; ghc --make -package mtl Parser.hs -o ../main; ghc --make -package mtl UnificationTypeChecker.hs -o ../mainU; 

clean:
	rm -rf generated main mainU *.s syntax.txt
	cd tests; find . -name "*.out" -type f -delete
	cd unification_tests; find . -name "*.out" -type f -delete