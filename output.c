#include <stdio.h>
#include <stdlib.h>

typedef union value value;
typedef struct closure closure;

struct closure {
   void *env;
   value (*func)(void *, value);
};

union value {
   int num;
   closure func;
};

value call(value clos, value arg) {
   return (*clos.func.func)(clos.func.env, arg);
}

value println(value arg) {
   printf("%d\n", arg.num);
   return (value) 0;
}


typedef struct {
   value f;
   value b;
} lam8_env;

value lam8(void *v_env, value x) {
   lam8_env *env = (lam8_env *) v_env;
   return call(call(env->f, env->f), x);
}

value init_lam8(value f, value b) {
   closure clos;
   lam8_env *env = malloc(sizeof(lam8_env));
   env->f = f;
   env->b = b;
   clos.env = env;
   clos.func = lam8;
   return (value) clos;
}


typedef struct {
   value b;
} lam7_env;

value lam7(void *v_env, value f) {
   lam7_env *env = (lam7_env *) v_env;
   return call(env->b, init_lam8(f, env->b));
}

value init_lam7(value b) {
   closure clos;
   lam7_env *env = malloc(sizeof(lam7_env));
   env->b = b;
   clos.env = env;
   clos.func = lam7;
   return (value) clos;
}


typedef struct {
   value f;
   value b;
} lam6_env;

value lam6(void *v_env, value x) {
   lam6_env *env = (lam6_env *) v_env;
   return call(call(env->f, env->f), x);
}

value init_lam6(value f, value b) {
   closure clos;
   lam6_env *env = malloc(sizeof(lam6_env));
   env->f = f;
   env->b = b;
   clos.env = env;
   clos.func = lam6;
   return (value) clos;
}


typedef struct {
   value b;
} lam5_env;

value lam5(void *v_env, value f) {
   lam5_env *env = (lam5_env *) v_env;
   return call(env->b, init_lam6(f, env->b));
}

value init_lam5(value b) {
   closure clos;
   lam5_env *env = malloc(sizeof(lam5_env));
   env->b = b;
   clos.env = env;
   clos.func = lam5;
   return (value) clos;
}


typedef struct {

} lam4_env;

value lam4(void *v_env, value b) {
   lam4_env *env = (lam4_env *) v_env;
   return call(init_lam5(b), init_lam7(b));
}

value init_lam4() {
   closure clos;
   lam4_env *env = malloc(sizeof(lam4_env));

   clos.env = env;
   clos.func = lam4;
   return (value) clos;
}


typedef struct {
   value fact;
   value y;
} lam3_env;

value lam3(void *v_env, value n) {
   lam3_env *env = (lam3_env *) v_env;
   return ((n.num <= 0) ? ((value) 1) : ((value) (n.num * call(env->fact, ((value) (n.num + ((value) -1).num))).num)));
}

value init_lam3(value fact, value y) {
   closure clos;
   lam3_env *env = malloc(sizeof(lam3_env));
   env->fact = fact;
   env->y = y;
   clos.env = env;
   clos.func = lam3;
   return (value) clos;
}


typedef struct {
   value y;
} lam2_env;

value lam2(void *v_env, value fact) {
   lam2_env *env = (lam2_env *) v_env;
   return init_lam3(fact, env->y);
}

value init_lam2(value y) {
   closure clos;
   lam2_env *env = malloc(sizeof(lam2_env));
   env->y = y;
   clos.env = env;
   clos.func = lam2;
   return (value) clos;
}


typedef struct {

} lam1_env;

value lam1(void *v_env, value y) {
   lam1_env *env = (lam1_env *) v_env;
   return call(y, init_lam2(y));
}

value init_lam1() {
   closure clos;
   lam1_env *env = malloc(sizeof(lam1_env));

   clos.env = env;
   clos.func = lam1;
   return (value) clos;
}


int main() {
   println(call(call(init_lam1(), init_lam4()), ((value) 6)));
   return 0;
}