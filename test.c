// #include "test.h"
//#define A(X,Y) X##Y
#include <stdio.h>

// #include <stddef.h>
// #include <string.h>
// #include <stdio.h>

#define A(X) puts(X # X)
#define B(X) puts(# X)
#define C(f) puts(# f)
#define APPEND(x, y) x y

// static struct mystrucT {int a;};

typedef struct {int a; int mayx;} M;

// struct incomp;

// extern struct incomp ms;

// static int mood(int);
const int x = 3;
static int y = x;

static int o;
static int o = 0;
// void o;

// int z = (int) &x;

int c = !(int*)&x;

int xyz(void c);


// static int (*mime(void))(void);

int main(int argc, char **args) {
	(void) o;
	(void) y;
	typedef int z;
// 	register int *const *x(void)(int)(char);
	register int ohmy;
	(void) ohmy;
// 	int *p = &ohmy;
// 	double A;

// 	APPEND(A, ("ohno"));
	B("wow");
// 	B(B("wow"));
// 	C(B("me"));

	char x = sizeof(int);
	struct {int b;} typedef Woot;

	Woot wat = {0};
// 	int foo = Woot;

// 	(void) (wat == wat);
	(Woot) {.b=0} = (Woot) {.b=1};

	char *a = &x;
	long volatile long y;
	(void) A;
	(void) x;
	(void) wat;
	(void) a;
	(void) y;
// 	char *y = &(1[&x]);
// 	(void) x;
// 	printf("%d", sizeof(_Bool));
// 	ptrdiff_t ptd = 0;
// 	memcpy((char) 'a', &ptd, &ptd - &ptd);
// 	*args[argc-1] = 2;
	return 0;
}

