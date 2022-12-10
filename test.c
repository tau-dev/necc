#include "test.h"

//#define A(X,Y) X##Y

#if __STDC_VERSION__ < 201112L && defined( __GNUC__)
#define _Alignas(t) __attribute__((__aligned__(t)))
#define _Alignof(t) __alignof__(t)
#endif


#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define A(X) puts(X # X)
#define B(X) puts(# X)
#define C(f) puts(# f)
#define APPEND(x, y) x y


int puts(const char *);


int ms(int (*)(void));

// static struct mystrucT {int a;};

typedef struct {int a; int mayx;} M;

struct incomp;

// extern struct incomp ms;

// static int mood(int);
const int x = 3;
static int y = x;

static int o;
static int o = 0;
// void o;

// int z = (int) &x;

int c = !(int*)&x;

// int xyz(void c);


// static int (*mime(void))(void);

int main(int argc, char **args) {
	(void) o;
	(void) y;
	typedef int z;
// 	register int *const *x(void)(int)(char);
	register int ohmy;
	(void) ohmy;
// 	int *p = &ohmy;
	double A;

// 	APPEND(A, ("ohno"));
// 	B("wow");
// 	B(B("wow"));
// 	C(B("me"));

	char x = sizeof(int);
	struct {int b;} typedef Woot;

// 	Woot wat = {0};
// 	int foo = Woot;

// 	(void) (wat == wat);
// 	(Woot) {.b=0} = (Woot) {.b=1};

	char *a = &x;
	char *p = &(1[&x]);
	long volatile long y;
	puts("Hello World!");
// 	(void) A;
	(void) x;
// 	(void) wat;
	(void) a;
	(void) y;
	(void) A;
	(void) ohmy;
// 	(void) x;
// 	printf("%d", sizeof(_Bool));
	ptrdiff_t ptd = 0;
// 	memcpy((char) 'a', &ptd, &ptd - &ptd);
// 	*args[argc-1] = 2;
	return strlen("wow!");
}

