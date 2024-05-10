constant 0 (x):
00 00 00 00 
public void f():
 entry0: { }
   0 /1 = const 0x1 (1)
       branch 0 ? if_true1 : if_else3
 if_true1: { mem: 9, ordered: 9, }
   7 /4 = const 0x4 (4)
   8 /8 = global 0 +0
   9 /4 = store [8] 7
       jmp if_join2
 if_join2: { }
   6 /8 = const 0x4 (4)
       switch 6: default => switch_join4
 switch_join4: { }
       ret
 if_else3: { mem: 3, 5, ordered: 3, 5, }
   1 /4 = const 0x0 (0)
   2 /8 = stack 4
   3 /4 = store [2] 1
   4 /4 = const 0x0 (0)
   5 /4 = store [2] 4 after 3
       jmp if_join2
constant 2 (__func__):
"f"
exit code 0
