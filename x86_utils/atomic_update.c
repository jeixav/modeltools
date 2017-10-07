#include <stdio.h>
#include <stdint.h>

  atomic_add(int *what,int n) {
  __asm__ __volatile__(
      "   lock       ;\n"
      "   addl %1,%0 ;\n"
      : "=m" (*what)
      : "ir" (n), "m" (*what)
      :
      );
}

atomic_and(int *what,int n) {
  __asm__ __volatile__(
      "   lock       ;\n"
      "   andl %1,%0 ;\n"
      : "=m" (*what)
      : "ir" (n), "m" (*what)
      :
      );
}

atomic_or(int *what,int n) {
  __asm__ __volatile__(
      "   lock       ;\n"
      "   orl %1,%0 ;\n"
      : "=m" (*what)
      : "ir" (n), "m" (*what)
      :
      );
}

atomic_xor(int *what,int n) {
  __asm__ __volatile__(
      "   lock       ;\n"
      "   xorl %1,%0 ;\n"
      : "=m" (*what)
      : "ir" (n), "m" (*what)
      :
      );
}

int atomic_fetch_and_add(int* what, int n)
{
  __asm__ volatile(
      "   lock       ;\n"
      "   xaddl %0, %1"
      : "+r" (n), "+m" (*what) // input+output
      : // No input-only
      : "memory"
    );
    return n;
}
#if defined(SELF_TEST)

uint64_t rdtsc(void) {   // version rapide "out of order"
#if defined(__x86_64__) || defined( __i386__ )
  uint32_t lo, hi;
  __asm__ volatile ("rdtscp"
      : /* outputs */ "=a" (lo), "=d" (hi)
      : /* no inputs */
      : /* clobbers */ "%rcx");
  return (uint64_t)lo | (((uint64_t)hi) << 32);
#else
  return time0++;
#endif
}

main(){
 int what = 0;
 int r, n;
 int i = 1;
 uint64_t t1, t2; 
 printf("what = %d\n",what);
 t1 = rdtsc();
#pragma omp parallel for
 for(r=0 ; r<1000000 ; r++) {
   n=atomic_fetch_and_add(&what,i);
//  atomic_add(&what,2);
//  atomic_xor(&what,3);
//  atomic_xor(&what,3);
//  atomic_add(&what,-4);
//  atomic_add(&what,3);
// printf("%d %3d what = %3d\n",omp_get_thread_num(),r,what);
 }
 t2 = rdtsc();
 printf("what = %10d, n= %10d, time = %10ld\n",what,n,t2-t1);
}
#endif
