void foo():
 entry0: { }
       ret
constant 1 ([anon]):
"foo"
public extern bar
public int main():
 entry0: { mem: 2, 5, ordered: 2, 5, }
   0 = global 0 +0
   1 = const 0x0 (0)
   2 = call 0 (1) after 1
   3 = global 2 +0
   4 = const 0x0 (0)
   5 = call 3 (4) after 1
   6 = const 0x0 (0)
       ret 6
constant 4 ([anon]):
"main"
exit code 0
