public int main():
 entry0: { mem: 5, 1, 4, 10, ordered: 10, }
   0 /8 = stack 8
   1 /8 = load 0
   2 /8 = const 0x8 (8)
   3 /8 = add 1 2
   4 /4 = load 3
   5 /8 = load 0
   6 /8 = const 0x8 (8)
   7 /8 = add 5 6
   8 /8 = const 0x4 (4)
   9 /8 = add 7 8
  10 /4 = store [9] 4 after 4
  11 /4 = const 0x0 (0)
       ret 11
constant 1 (__func__):
"main"
exit code 0
