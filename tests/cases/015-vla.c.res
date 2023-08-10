public extern memcpy
public extern memmove
public extern memccpy
public extern memset
public extern memcmp
public extern __memcmpeq
public extern memchr
public extern strcpy
public extern strncpy
public extern strcat
public extern strncat
public extern strcmp
public extern strncmp
public extern strcoll
public extern strxfrm
public extern strcoll_l
public extern strxfrm_l
public extern strdup
public extern strndup
public extern strchr
public extern strrchr
public extern strchrnul
public extern strcspn
public extern strspn
public extern strpbrk
public extern strstr
public extern strtok
public extern __strtok_r
public extern strtok_r
public extern strcasestr
public extern memmem
public extern __mempcpy
public extern mempcpy
public extern strlen
public extern strnlen
public extern strerror
public extern __xpg_strerror_r
public extern strerror_l
public extern bcmp
public extern bcopy
public extern bzero
public extern index
public extern rindex
public extern ffs
public extern ffsl
public extern ffsll
public extern strcasecmp
public extern strncasecmp
public extern strcasecmp_l
public extern strncasecmp_l
public extern explicit_bzero
public extern strsep
public extern strsignal
public extern __stpcpy
public extern stpcpy
public extern __stpncpy
public extern stpncpy
public extern strlcpy
public extern strlcat
public int main():
 entry0: { mem: 2, 4, 7, 5, 16, 19, 20, 21, ordered: 2, 4, 16, 20, 21, }
   0 /4 = const 0x0 (0)
   1 /8 = stack 4
   2 /4 = store [1] 0
   3 /4 = const 0x7d0 (2000)
   4 /4 = store [1] 3 after 2
   5 /4 = load 1 after 4
   6 /8 = global 3 +0
   7 /4 = load 1 after 4
   8 /8 = signex i64, 7
   9 /8 = const 0x4 (4)
  10 /8 = mul 8 9
  11 /8 = stack_vla 10
  12 /4 = const 0x2a (42)
  13 /8 = signex i64, 5
  14 /8 = const 0x4 (4)
  15 /8 = mul 13 14
  16 /8 = call 6 (11, 12, 15) after 5
  17 /8 = const 0x320 (800)
  18 /8 = add 11 17
  19 /4 = load 18 after 16
  20 /4 = store [1] 19 after 19
  21 /0 = discard 11
  22 /4 = load 1 after 21
       ret 22
constant 60 (__func__):
"main"
exit code 42
