# Some random projects I tried which can be successfully compiled with Necc and pass their respective test suites

https://github.com/antirez/rax

https://github.com/cesanta/frozen

https://github.com/wooorm/stmr.c

http://runtimelegend.com/tech/lil

https://github.com/skejeton/raytracer

Necc doesn't have threading yet, you'll need this patch:
```diff
--- a/src/main.c
+++ b/src/main.c
@@ -99,7 +99,7 @@ int main() {
                 theatre_add(&theatre, actor_sphere(pos, radius));
         }
         */
-        const usize THREAD_N = 16;
+        const usize THREAD_N = 1;

         pthread_t threads[THREAD_N];
         int ret[THREAD_N];
```
