


```
h = here
mv = move

mv "a.txt" to "b.txt"
move "a.txt" to "b.txt"

copy "a.txt" to "b.txt"
cp "a.txt" to "b.txt"

a = read from "test.txt"
a = exec cat "test.txt"

write a to "test.txt"

"here / {** / * as f}foo / yeet.txt" 
~> "yeet" not in f   
|> move _ to "here / {f}bar / yeet.txt" 

print 3 + 3 / 5
write 3 + 3 / 5 to stdout


function compile 
    a
    "into" 
    b = replace {* as prefix}.c with {prefix}.o in a) {

    exec "gcc {a} -o {b}" 
}

compile test.c
compile test.c into something.o


```