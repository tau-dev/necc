public int f():
 entry0: { }
   0 /4 = const 0x0 (0)
       ret 0
public int bar():
 entry0: { mem: 2, ordered: 2, }
   0 /8 = global 0 +0
   1 /4 = const 0x0 (0)
   2 /4 = call 0 (1)
       ret 2
constant 2 (__func__):
"bar"
constant 3 (__func__):
"f"
public int z():
 entry0: { mem: 2, ordered: 2, }
   0 /8 = global 0 +0
   1 /4 = const 0x0 (0)
   2 /4 = call 0 (1)
       ret 2
constant 5 (__func__):
"z"
exit code 0
