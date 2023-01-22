public int main():
 entry0: { ordered: 1, }
   0 /4 = const 0x1 (1)
   1 /4 = out-phi 0 ?  true->6
   2 /8 = stack 4
   3 /4 = load i32, [2]
       branch 3 ? 2 : 1
 2: { mem: 7, ordered: 7, 9, }
   6 /4 = in-phi
   7 /4 = store [2] 6
   8 /4 = const 0x1 (1)
   9 /4 = out-phi 8 ?  true->12
       branch 6 ? 4 : 3
 4: { mem: 13, 15, 20, ordered: 13, 15, 20, 22, }
  12 /4 = in-phi
  13 /4 = store [2] 12
  14 /4 = const 0x1 (1)
  15 /4 = store [2] 14 after 13
  16 /4 = const 0x0 (0)
  17 /4 = cmp== 14 16
  18 /4 = const 0x0 (0)
  19 /4 = cmp== 17 18
  20 /4 = store [2] 19 after 13
  21 /4 = const 0x0 (0)
  22 /4 = out-phi 21 ?  false->23
       branch 19 ? 5 : 6
 5: { ordered: 101, }
 100 /4 = const 0x0 (0)
 101 /4 = out-phi 100 ?  true->23
       jmp 6
 6: { mem: 24, ordered: 24, 26, }
  23 /4 = in-phi
  24 /4 = store [2] 23
  25 /4 = const 0x0 (0)
  26 /4 = out-phi 25 ?  false->27
       branch 23 ? 7 : 8
 7: { ordered: 99, }
  98 /4 = const 0x1 (1)
  99 /4 = out-phi 98 ?  true->27
       jmp 8
 8: { mem: 28, 33, 35, 38, 41, 44, 47, 48, 49, 50, 53, 56, 59, 62, 65, 67, 69, 70, 71, 72, ordered: 28, 33, 35, 38, 41, 44, 47, 48, 49, 50, 53, 56, 59, 62, 65, 67, 69, 70, 71, 72, }
  27 /4 = in-phi
  28 /4 = store [2] 27
  29 /4 = const 0x0 (0)
  30 /4 = cmp== 27 29
  31 /4 = const 0x0 (0)
  32 /4 = cmp== 30 31
  33 /4 = store [2] 32
  34 /4 = const 0x0 (0)
  35 /4 = store [2] 34 after 33
  36 /4 = const 0x0 (0)
  37 /4 = shift<< 34 36
  38 /4 = store [2] 37 after 33
  39 /4 = const 0x0 (0)
  40 /4 = shift<< 39 37
  41 /4 = store [2] 40 after 33
  42 /4 = const 0x0 (0)
  43 /4 = shift>> 40 42
  44 /4 = store [2] 43 after 33
  45 /4 = const 0x0 (0)
  46 /4 = shift>> 45 43
  47 /4 = store [2] 46 after 33
  48 /4 = store [2] 46 after 33
  49 /4 = store [2] 46 after 33
  50 /4 = store [2] 46 after 33
  51 /4 = const 0x0 (0)
  52 /4 = sub 51 46
  53 /4 = store [2] 52 after 33
  54 /4 = const 0x0 (0)
  55 /4 = or 52 54
  56 /4 = store [2] 55 after 33
  57 /4 = const 0x0 (0)
  58 /4 = or 55 57
  59 /4 = store [2] 58 after 33
  60 /4 = const 0x0 (0)
  61 /4 = xor 58 60
  62 /4 = store [2] 61 after 33
  63 /4 = const 0x0 (0)
  64 /4 = xor 61 63
  65 /4 = store [2] 64 after 33
  66 /4 = const 0x0 (0)
  67 /4 = store [2] 66 after 33
  68 /4 = const 0x0 (0)
  69 /4 = store [2] 68 after 33
  70 /4 = store [2] 68 after 33
  71 /4 = store [2] 68 after 33
  72 /4 = store [2] 68 after 33
       branch 68 ? if_true9 : if_join10
 if_true9: { mem: 95, 97, ordered: 97, }
  94 /4 = const 0x1 (1)
  95 /4 = load i32, [2]
  96 /4 = sdiv 94 95
  97 /4 = store [2] 96 after 95
       jmp if_join10
 if_join10: { mem: 73, 76, 79, 82, ordered: 76, 79, 82, }
  73 /4 = load i32, [2]
  74 /4 = const 0xffffffffffffffff (18446744073709551615)
  75 /4 = and 73 74
  76 /4 = store [2] 75 after 73
  77 /4 = const 0xffffffffffffffff (18446744073709551615)
  78 /4 = and 75 77
  79 /4 = store [2] 78 after 73
  80 /4 = const 0x1 (1)
  81 /4 = smod 78 80
  82 /4 = store [2] 81 after 73
  83 /4 = const 0x0 (0)
       branch 83 ? if_true11 : if_join12
 if_true11: { mem: 90, 93, ordered: 93, }
  90 /4 = load i32, [2]
  91 /4 = const 0x0 (0)
  92 /4 = sdiv 90 91
  93 /4 = store [2] 92 after 90
       jmp if_join12
 if_join12: { }
  84 /4 = const 0x0 (0)
       branch 84 ? if_true13 : if_join14
 if_true13: { mem: 86, 89, ordered: 89, }
  86 /4 = load i32, [2]
  87 /4 = const 0x0 (0)
  88 /4 = smod 86 87
  89 /4 = store [2] 88 after 86
       jmp if_join14
 if_join14: { }
  85 /4 = const 0x0 (0)
       ret 85
 3: { ordered: 11, }
  10 /4 = const 0x1 (1)
  11 /4 = out-phi 10 ?  true->12
       jmp 4
 1: { ordered: 5, }
   4 /4 = const 0x0 (0)
   5 /4 = out-phi 4 ?  true->6
       jmp 2
constant 1 ([anon]):
"main"
exit code 0
