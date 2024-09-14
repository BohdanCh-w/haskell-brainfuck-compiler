run:
	exefile=$$(mktemp -p build)
	mkdir -p build
	ghc BrainFuck.hs -outputdir build -o $(exefile) && $(exefile) ${@:2}; rm $(exefile)          
