public int main():
 entry0: { ordered: 1, }
   0 /4 = const 0x1 (1)
   1 /4 = out-phi 0 ?  true->6
   2 /8 = stack 4
   3 /4 = load 2
       branch 3 ? 2 : 1
 2: { mem: 7, ordered: 7, 9, }
   6 /4 = in-phi
   7 /4 = store [2] 6
   8 /4 = const 0x1 (1)
   9 /4 = out-phi 8 ? 
       branch 6 ? 4 : 3
 4: { mem: 17, ordered: 17, 19, }
  12 /4 = const 0x1 (1)
  13 /4 = const 0x0 (0)
  14 /4 = cmp== 12 13
  15 /4 = const 0x0 (0)
  16 /4 = cmp== 14 15
  17 /4 = store [2] 16
  18 /4 = const 0x0 (0)
  19 /4 = out-phi 18 ?  false->20
       branch 16 ? 5 : 6
 5: { ordered: 52, }
  51 /4 = const 0x0 (0)
  52 /4 = out-phi 51 ?  true->20
       jmp 6
 6: { mem: 21, ordered: 21, 23, }
  20 /4 = in-phi
  21 /4 = store [2] 20
  22 /4 = const 0x0 (0)
  23 /4 = out-phi 22 ? 
       branch 20 ? 7 : 8
 7: { ordered: 50, }
  49 /4 = const 0x1 (1)
  50 /4 = out-phi 49 ? 
       jmp 8
 8: { mem: 25, ordered: 25, }
  24 /4 = const 0x0 (0)
  25 /4 = store [2] 24
       branch 24 ? if_true9 : if_join10
 if_true9: { mem: 46, 48, ordered: 48, }
  45 /4 = const 0x1 (1)
  46 /4 = load 2
  47 /4 = sdiv 45 46
  48 /4 = store [2] 47 after 46
       jmp if_join10
 if_join10: { mem: 26, 33, ordered: 33, }
  26 /4 = load 2
  27 /4 = const 0xffffffffffffffff (18446744073709551615)
  28 /4 = and 26 27
  29 /4 = const 0xffffffffffffffff (18446744073709551615)
  30 /4 = and 28 29
  31 /4 = const 0x1 (1)
  32 /4 = smod 30 31
  33 /4 = store [2] 32 after 26
  34 /4 = const 0x0 (0)
       branch 34 ? if_true11 : if_join12
 if_true11: { mem: 41, 44, ordered: 44, }
  41 /4 = load 2
  42 /4 = const 0x0 (0)
  43 /4 = sdiv 41 42
  44 /4 = store [2] 43 after 41
       jmp if_join12
 if_join12: { }
  35 /4 = const 0x0 (0)
       branch 35 ? if_true13 : if_join14
 if_true13: { mem: 37, 40, ordered: 40, }
  37 /4 = load 2
  38 /4 = const 0x0 (0)
  39 /4 = smod 37 38
  40 /4 = store [2] 39 after 37
       jmp if_join14
 if_join14: { }
  36 /4 = const 0x0 (0)
       ret 36
 3: { ordered: 11, }
  10 /4 = const 0x1 (1)
  11 /4 = out-phi 10 ? 
       jmp 4
 1: { ordered: 5, }
   4 /4 = const 0x0 (0)
   5 /4 = out-phi 4 ?  true->6
       jmp 2
constant 1 (__func__):
"main"
exit code 0
