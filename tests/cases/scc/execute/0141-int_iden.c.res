public int main():
 entry0: { ordered: 1, }
   0 /4 = const 0x1 (1)
   1 /4 = out-phi 0 ?  true->10
   2 /8 = stack 4
   3 /4 = load 2
   4 /4 = const 0x0 (0)
   5 /4 = cmp== 3 4
   6 /4 = const 0x0 (0)
   7 /4 = cmp== 5 6
       branch 7 ? 2 : 1
 2: { mem: 11, ordered: 11, 13, }
  10 /4 = in-phi
  11 /4 = store [2] 10
  12 /4 = const 0x1 (1)
  13 /4 = out-phi 12 ? 
  14 /4 = const 0x0 (0)
  15 /4 = cmp== 10 14
  16 /4 = const 0x0 (0)
  17 /4 = cmp== 15 16
       branch 17 ? 4 : 3
 4: { mem: 25, ordered: 25, 27, }
  20 /4 = const 0x1 (1)
  21 /4 = const 0x0 (0)
  22 /4 = cmp== 20 21
  23 /4 = const 0x0 (0)
  24 /4 = cmp== 22 23
  25 /4 = store [2] 24
  26 /4 = const 0x0 (0)
  27 /4 = out-phi 26 ?  false->32
  28 /4 = const 0x0 (0)
  29 /4 = cmp== 24 28
  30 /4 = const 0x0 (0)
  31 /4 = cmp== 29 30
       branch 31 ? 6 : 7
 6: { ordered: 68, }
  67 /4 = const 0x0 (0)
  68 /4 = out-phi 67 ?  true->32
       jmp 7
 7: { mem: 33, ordered: 33, 35, }
  32 /4 = in-phi
  33 /4 = store [2] 32
  34 /4 = const 0x0 (0)
  35 /4 = out-phi 34 ? 
  36 /4 = const 0x0 (0)
  37 /4 = cmp== 32 36
  38 /4 = const 0x0 (0)
  39 /4 = cmp== 37 38
       branch 39 ? 8 : 9
 8: { ordered: 66, }
  65 /4 = const 0x1 (1)
  66 /4 = out-phi 65 ? 
       jmp 9
 9: { mem: 41, ordered: 41, }
  40 /4 = const 0x0 (0)
  41 /4 = store [2] 40
       branch 40 ? if_true11 : if_join12
 if_true11: { mem: 62, 64, ordered: 64, }
  61 /4 = const 0x1 (1)
  62 /4 = load 2
  63 /4 = sdiv 61 62
  64 /4 = store [2] 63 after 62
       jmp if_join12
 if_join12: { mem: 42, 49, ordered: 49, }
  42 /4 = load 2
  43 /4 = const 0xffffffff (4294967295)
  44 /4 = and 42 43
  45 /4 = const 0xffffffff (4294967295)
  46 /4 = and 44 45
  47 /4 = const 0x1 (1)
  48 /4 = smod 46 47
  49 /4 = store [2] 48 after 42
  50 /1 = const 0x0 (0)
       branch 50 ? if_true13 : if_join14
 if_true13: { mem: 57, 60, ordered: 60, }
  57 /4 = load 2
  58 /4 = const 0x0 (0)
  59 /4 = sdiv 57 58
  60 /4 = store [2] 59 after 57
       jmp if_join14
 if_join14: { }
  51 /1 = const 0x0 (0)
       branch 51 ? if_true15 : if_join16
 if_true15: { mem: 53, 56, ordered: 56, }
  53 /4 = load 2
  54 /4 = const 0x0 (0)
  55 /4 = smod 53 54
  56 /4 = store [2] 55 after 53
       jmp if_join16
 if_join16: { }
  52 /4 = const 0x0 (0)
       ret 52
 3: { ordered: 19, }
  18 /4 = const 0x1 (1)
  19 /4 = out-phi 18 ? 
       jmp 4
 1: { ordered: 9, }
   8 /4 = const 0x0 (0)
   9 /4 = out-phi 8 ?  true->10
       jmp 2
constant 1 (__func__):
"main"
exit code 0
