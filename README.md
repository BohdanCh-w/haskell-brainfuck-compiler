# haskell-brainfuck-compiler

This is haskell implementation of brainfuck compiler. This version is very inefficient and is made for education purposes.

## execute program

- make sure that `build` and `bin` folders exist 

```bash
# compile
ghc BrainFuck.hs -outputdir build -o bin/bf

# execute
bin/bf examples/hello-world.bf
# $ Hello world!

# you can add input
echo 45 | bin/bf examples/add-two.bf
# $ 9

# execute string
bin/bf -c '>+++++++++[<++++++++>-]<.'
# $ H
```

### During implementation changes and/or for testing, bash function may be created

```bash
function ghr {                                                                             
    exefile=$(mktemp -p build)                                                             
                                                                                           
    mkdir -p build                                                                         
    ghc $1 -outputdir build -o $exefile && $exefile ${@:2}; rm $exefile                    
}

# it may be used as following
ghr BrainFuck.hs examples/exec.bf
```
