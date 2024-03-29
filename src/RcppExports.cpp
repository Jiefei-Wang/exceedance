// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "exceedance_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// C_get_range_by_bound
SEXP C_get_range_by_bound(SEXP R_sx, SEXP R_l, SEXP R_h);
RcppExport SEXP _exceedance_C_get_range_by_bound(SEXP R_sxSEXP, SEXP R_lSEXP, SEXP R_hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_sx(R_sxSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_l(R_lSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_h(R_hSEXP);
    rcpp_result_gen = Rcpp::wrap(C_get_range_by_bound(R_sx, R_l, R_h));
    return rcpp_result_gen;
END_RCPP
}
// C_GW_compute_FDR
double C_GW_compute_FDR(SEXP sorted_i, SEXP R_P, SEXP R_Q, int rj_num, int n);
RcppExport SEXP _exceedance_C_GW_compute_FDR(SEXP sorted_iSEXP, SEXP R_PSEXP, SEXP R_QSEXP, SEXP rj_numSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type sorted_i(sorted_iSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_P(R_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_Q(R_QSEXP);
    Rcpp::traits::input_parameter< int >::type rj_num(rj_numSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(C_GW_compute_FDR(sorted_i, R_P, R_Q, rj_num, n));
    return rcpp_result_gen;
END_RCPP
}
// C_get_range_by_bound2
SEXP C_get_range_by_bound2(SEXP R_sx, SEXP R_l, SEXP R_h);
RcppExport SEXP _exceedance_C_get_range_by_bound2(SEXP R_sxSEXP, SEXP R_lSEXP, SEXP R_hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_sx(R_sxSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_l(R_lSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_h(R_hSEXP);
    rcpp_result_gen = Rcpp::wrap(C_get_range_by_bound2(R_sx, R_l, R_h));
    return rcpp_result_gen;
END_RCPP
}
// general_GW_construct_subset
SEXP general_GW_construct_subset(SEXP pvalue_func, SEXP rho, NumericVector x);
RcppExport SEXP _exceedance_general_GW_construct_subset(SEXP pvalue_funcSEXP, SEXP rhoSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type pvalue_func(pvalue_funcSEXP);
    Rcpp::traits::input_parameter< SEXP >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(general_GW_construct_subset(pvalue_func, rho, x));
    return rcpp_result_gen;
END_RCPP
}
// general_GW_compute_FP
size_t general_GW_compute_FP(SEXP ptr, size_t m, NumericVector sorted_i, double alpha);
RcppExport SEXP _exceedance_general_GW_compute_FP(SEXP ptrSEXP, SEXP mSEXP, SEXP sorted_iSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type ptr(ptrSEXP);
    Rcpp::traits::input_parameter< size_t >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sorted_i(sorted_iSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(general_GW_compute_FP(ptr, m, sorted_i, alpha));
    return rcpp_result_gen;
END_RCPP
}
// print_subset_list
void print_subset_list(SEXP x);
RcppExport SEXP _exceedance_print_subset_list(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    print_subset_list(x);
    return R_NilValue;
END_RCPP
}
// compute_prob
double compute_prob(R_xlen_t m, SEXP R_g_value, SEXP R_h_value, R_xlen_t n_t, SEXP R_diff_t);
RcppExport SEXP _exceedance_compute_prob(SEXP mSEXP, SEXP R_g_valueSEXP, SEXP R_h_valueSEXP, SEXP n_tSEXP, SEXP R_diff_tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< R_xlen_t >::type m(mSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_g_value(R_g_valueSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_h_value(R_h_valueSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type n_t(n_tSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_diff_t(R_diff_tSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_prob(m, R_g_value, R_h_value, n_t, R_diff_t));
    return rcpp_result_gen;
END_RCPP
}
// compute_prob_fft
double compute_prob_fft(R_xlen_t m, SEXP R_g_value, SEXP R_h_value, R_xlen_t n_t, SEXP R_diff_t);
RcppExport SEXP _exceedance_compute_prob_fft(SEXP mSEXP, SEXP R_g_valueSEXP, SEXP R_h_valueSEXP, SEXP n_tSEXP, SEXP R_diff_tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< R_xlen_t >::type m(mSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_g_value(R_g_valueSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_h_value(R_h_valueSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type n_t(n_tSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_diff_t(R_diff_tSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_prob_fft(m, R_g_value, R_h_value, n_t, R_diff_t));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_exceedance_C_get_range_by_bound", (DL_FUNC) &_exceedance_C_get_range_by_bound, 3},
    {"_exceedance_C_GW_compute_FDR", (DL_FUNC) &_exceedance_C_GW_compute_FDR, 5},
    {"_exceedance_C_get_range_by_bound2", (DL_FUNC) &_exceedance_C_get_range_by_bound2, 3},
    {"_exceedance_general_GW_construct_subset", (DL_FUNC) &_exceedance_general_GW_construct_subset, 3},
    {"_exceedance_general_GW_compute_FP", (DL_FUNC) &_exceedance_general_GW_compute_FP, 4},
    {"_exceedance_print_subset_list", (DL_FUNC) &_exceedance_print_subset_list, 1},
    {"_exceedance_compute_prob", (DL_FUNC) &_exceedance_compute_prob, 5},
    {"_exceedance_compute_prob_fft", (DL_FUNC) &_exceedance_compute_prob_fft, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_exceedance(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
