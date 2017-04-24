#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP FSelectorRcpp_cutOff_k(SEXP, SEXP, SEXP);
extern SEXP FSelectorRcpp_discretize_cpp(SEXP, SEXP, SEXP);
extern SEXP FSelectorRcpp_fs_count_levels(SEXP);
extern SEXP FSelectorRcpp_fs_entropy1d(SEXP);
extern SEXP FSelectorRcpp_fs_order(SEXP);
extern SEXP FSelectorRcpp_fs_table_numeric2d(SEXP, SEXP);
extern SEXP FSelectorRcpp_fs_table1d(SEXP);
extern SEXP FSelectorRcpp_information_gain_cpp(SEXP, SEXP, SEXP);
extern SEXP FSelectorRcpp_sparse_information_gain_cpp(SEXP, SEXP);
extern void run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
  {"FSelectorRcpp_cutOff_k",                    (DL_FUNC) &FSelectorRcpp_cutOff_k,                    3},
  {"FSelectorRcpp_discretize_cpp",              (DL_FUNC) &FSelectorRcpp_discretize_cpp,              3},
  {"FSelectorRcpp_fs_count_levels",             (DL_FUNC) &FSelectorRcpp_fs_count_levels,             1},
  {"FSelectorRcpp_fs_entropy1d",                (DL_FUNC) &FSelectorRcpp_fs_entropy1d,                1},
  {"FSelectorRcpp_fs_order",                    (DL_FUNC) &FSelectorRcpp_fs_order,                    1},
  {"FSelectorRcpp_fs_table_numeric2d",          (DL_FUNC) &FSelectorRcpp_fs_table_numeric2d,          2},
  {"FSelectorRcpp_fs_table1d",                  (DL_FUNC) &FSelectorRcpp_fs_table1d,                  1},
  {"FSelectorRcpp_information_gain_cpp",        (DL_FUNC) &FSelectorRcpp_information_gain_cpp,        3},
  {"FSelectorRcpp_sparse_information_gain_cpp", (DL_FUNC) &FSelectorRcpp_sparse_information_gain_cpp, 2},
  {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 0},
  {NULL, NULL, 0}
};

void R_init_FSelectorRcpp(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
