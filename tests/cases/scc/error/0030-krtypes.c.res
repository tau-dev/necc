void foo():
 entry0: { }
       ret
constant 1 (__func__):
"foo"
public extern bar
public int main():
 entry0: { mem: 2, 5, ordered: 2, 5, }
   0 /8 = global 0 +0
   1 /4 = const 0x0 (0)
   2 /0 = call 0 (1)
   3 /8 = global 2 +0
   4 /4 = const 0x0 (0)
   5 /0 = call 3 (4) after 2
   6 /4 = const 0x0 (0)
       ret 6
constant 4 (__func__):
"main"
exit code 0
