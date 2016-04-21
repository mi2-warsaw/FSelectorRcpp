#ifndef FSELECTOR_INCLUDES
#define FSELECTOR_INCLUDES

#ifdef _OPENMP
   #include <omp.h>
#else
   #define omp_get_max_threads() 0
#endif

#endif
