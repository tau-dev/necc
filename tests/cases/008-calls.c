//!necc-dbg @ -run

void a(void) {}
void b(void) {a();}


int c(int w) {return w + 1;}
int d(int w) {return w + c(w);}

main() {
	b();
	return d(4);
}
