public int f():
 entry0: { }
   0 = const 0x0 (0)
       ret 0
public int bar():
 entry0: { mem: 2, ordered: 2, }
   0 = global 0 +0
   1 = const 0x0 (0)
   2 = call 0 (1)
       ret 2
constant 2 ([anon]):
"bar"
constant 3 ([anon]):
"f"
public int z():
 entry0: { mem: 2, ordered: 2, }
   0 = global 0 +0
   1 = const 0x0 (0)
   2 = call 0 (1)
       ret 2
constant 5 ([anon]):
"z"
exit code 0
