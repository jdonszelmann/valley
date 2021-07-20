

```
types:
u8,
u16,
*u8 (=u16),
**u8 (=u16),
***u8 (=u16),
etc...

```


```
fn main() {
    let a = 3;
    
    let b = a + 3;
    a = a + 3;
    
    let c = &b;
    xstuff(*c);
    a++;
}

fn main() {
    let a: u8 = 3;
    let b: u8 = 3;
    test(a, b + b);
}

fn test(a: u8, b: u8) {
 
}

create_stackframe
* (sp + 1) = 3;
* (sp + 2) = 3;
repeat:
 calc_expr
 add nth-param - 1, sp
 push *(sp + 1)
 sub #nth-param, sp
 
 
 push *(sp + 3)
 call
 pop_params
 ret



fn xstuff();
fn xstuff(): 0xffee;
let a: 0xffee;

fn test(a: u8) {
    let a: u8 = 3;
    let a: u8 = 3;
}

```

```

---
stackframe

---

ret addr
ret addr
{
    local vars
    sp + 3
    sp + 2
    sp + 1
}

<---
sp = sp - (#local vars + *(loc))
tsa
sbc #local vars
tas
rts

---


    



jmp ($100)
lda ($80),Y
let x:u8 = *(*($81:$80) + y)

tsx
lda $80,X
lda $100,X
lda $100,X
lda $100,X
push
tsx
lda $100,X

jsr xstuff
inx

rts

ldx #$00
lda #$10
sta #$200,X
inx

```

```
3 + 3
lda #3
adc #3
```

```
a + 3
tsx
lda $1, X
adc #3
```

```
3 + a
tsx
lda $1, X
tay
lda #1
adc 
```