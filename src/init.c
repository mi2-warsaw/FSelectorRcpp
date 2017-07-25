#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _FSelectorRcpp_cutOff_k(SEXP, SEXP, SEXP);
extern SEXP _FSelectorRcpp_discretize_cpp(SEXP, SEXP, SEXP);
extern SEXP _FSelectorRcpp_fs_count_levels(SEXP);
extern SEXP _FSelectorRcpp_fs_entropy1d(SEXP);
extern SEXP _FSelectorRcpp_fs_order(SEXP);
extern SEXP _FSelectorRcpp_fs_table_numeric2d(SEXP, SEXP);
extern SEXP _FSelectorRcpp_fs_table1d(SEXP);
extern SEXP _FSelectorRcpp_information_gain_cpp(SEXP, SEXP, SEXP);
extern SEXP _FSelectorRcpp_sparse_information_gain_cpp(SEXP, SEXP);
extern void run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
  {"_FSelectorRcpp_cutOff_k",                    (DL_FUNC) &_FSelectorRcpp_cutOff_k,                    3},
  {"_FSelectorRcpp_discretize_cpp",              (DL_FUNC) &_FSelectorRcpp_discretize_cpp,              3},
  {"_FSelectorRcpp_fs_count_levels",             (DL_FUNC) &_FSelectorRcpp_fs_count_levels,             1},
  {"_FSelectorRcpp_fs_entropy1d",                (DL_FUNC) &_FSelectorRcpp_fs_entropy1d,                1},
  {"_FSelectorRcpp_fs_order",                    (DL_FUNC) &_FSelectorRcpp_fs_order,                    1},
  {"_FSelectorRcpp_fs_table_numeric2d",          (DL_FUNC) &_FSelectorRcpp_fs_table_numeric2d,          2},
  {"_FSelectorRcpp_fs_table1d",                  (DL_FUNC) &_FSelectorRcpp_fs_table1d,                  1},
  {"_FSelectorRcpp_information_gain_cpp",        (DL_FUNC) &_FSelectorRcpp_information_gain_cpp,        3},
  {"_FSelectorRcpp_sparse_information_gain_cpp", (DL_FUNC) &_FSelectorRcpp_sparse_information_gain_cpp, 2},
  {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 0},
  {NULL, NULL, 0}
};

void R_init_FSelectorRcpp(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
