public int main():
 entry0: { ordered: 1, }
   0 /4 = const 0x1 (1)
   1 /4 = phi 0 true->6
   2 /8 = stack 4
   3 /4 = load i32, [2]
       branch 3 ? 2 : 1
 2: { mem: 7, ordered: 7, 9, }
   6 /4 = ->phi
   7 /4 = store [2] 6
   8 /4 = const 0x1 (1)
   9 /4 = phi 8 true->12
       branch 6 ? 4 : 3
 4: { mem: 13, 15, 20, ordered: 13, 15, 20, 22, }
  12 /4 = ->phi
  13 /4 = store [2] 12
  14 /4 = const 0x1 (1)
  15 /4 = store [2] 14 after 13
  16 /4 = const 0x0 (0)
  17 /4 = cmp== 14 16
  18 /4 = const 0x0 (0)
  19 /4 = cmp== 17 18
  20 /4 = store [2] 19 after 13
  21 /4 = const 0x0 (0)
  22 /4 = phi 21 false->23
       branch 19 ? 5 : 6
 5: { ordered: 109, }
 108 /4 = const 0x0 (0)
 109 /4 = phi 108 true->23
       jmp 6
 6: { mem: 24, ordered: 24, 26, }
  23 /4 = ->phi
  24 /4 = store [2] 23
  25 /4 = const 0x0 (0)
  26 /4 = phi 25 false->27
       branch 23 ? 7 : 8
 7: { ordered: 107, }
 106 /4 = const 0x1 (1)
 107 /4 = phi 106 true->27
       jmp 8
 8: { mem: 28, 33, 35, 38, 41, 44, 47, 48, 49, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, ordered: 28, 33, 35, 38, 41, 44, 47, 48, 49, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, }
  27 /4 = ->phi
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
  67 /4 = smul 64 66
  68 /4 = store [2] 67 after 33
  69 /4 = const 0x0 (0)
  70 /4 = smul 69 67
  71 /4 = store [2] 70 after 33
  72 /4 = const 0x1 (1)
  73 /4 = smul 70 72
  74 /4 = store [2] 73 after 33
  75 /4 = const 0x1 (1)
  76 /4 = smul 75 73
  77 /4 = store [2] 76 after 33
  78 /4 = const 0x1 (1)
  79 /4 = sdiv 76 78
  80 /4 = store [2] 79 after 33
       branch 79 ? if_true9 : if_join10
 if_true9: { mem: 103, 105, ordered: 105, }
 102 /4 = const 0x1 (1)
 103 /4 = load i32, [2]
 104 /4 = sdiv 102 103
 105 /4 = store [2] 104 after 103
       jmp if_join10
 if_join10: { mem: 81, 84, 87, 90, ordered: 84, 87, 90, }
  81 /4 = load i32, [2]
  82 /4 = const 0xffffffffffffffff (18446744073709551615)
  83 /4 = and 81 82
  84 /4 = store [2] 83 after 81
  85 /4 = const 0xffffffffffffffff (18446744073709551615)
  86 /4 = and 83 85
  87 /4 = store [2] 86 after 81
  88 /4 = const 0x1 (1)
  89 /4 = smod 86 88
  90 /4 = store [2] 89 after 81
  91 /4 = const 0x0 (0)
       branch 91 ? if_true11 : if_join12
 if_true11: { mem: 98, 101, ordered: 101, }
  98 /4 = load i32, [2]
  99 /4 = const 0x0 (0)
 100 /4 = sdiv 98 99
 101 /4 = store [2] 100 after 98
       jmp if_join12
 if_join12: { }
  92 /4 = const 0x0 (0)
       branch 92 ? if_true13 : if_join14
 if_true13: { mem: 94, 97, ordered: 97, }
  94 /4 = load i32, [2]
  95 /4 = const 0x0 (0)
  96 /4 = smod 94 95
  97 /4 = store [2] 96 after 94
       jmp if_join14
 if_join14: { }
  93 /4 = const 0x0 (0)
       ret 93
 3: { ordered: 11, }
  10 /4 = const 0x1 (1)
  11 /4 = phi 10 true->12
       jmp 4
 1: { ordered: 5, }
   4 /4 = const 0x0 (0)
   5 /4 = phi 4 true->6
       jmp 2
constant 1 ([anon]):
"main"
exit code 0
