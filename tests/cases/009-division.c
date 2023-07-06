//!necc-dbg @ -run
#define h char
#define f(c) safe_##c

h f(div_func_int8_t_s_s)(h d, h e) {
   return d / e;
}
int a() {
    int g = 200;
    return safe_div_func_int8_t_s_s(g, 1);
}
int main() {
   return a();
}
