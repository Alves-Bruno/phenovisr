// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// phenovis_read_mask
void phenovis_read_mask(std::string maskname);
RcppExport SEXP _phenovisr_phenovis_read_mask(SEXP masknameSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type maskname(masknameSEXP);
    phenovis_read_mask(maskname);
    return R_NilValue;
END_RCPP
}
// phenovis_get_gcc_histogram
DataFrame phenovis_get_gcc_histogram(StringVector names, int number_of_bins);
RcppExport SEXP _phenovisr_phenovis_get_gcc_histogram(SEXP namesSEXP, SEXP number_of_binsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type names(namesSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_bins(number_of_binsSEXP);
    rcpp_result_gen = Rcpp::wrap(phenovis_get_gcc_histogram(names, number_of_bins));
    return rcpp_result_gen;
END_RCPP
}
// phenovis_get_histogram
DataFrame phenovis_get_histogram(StringVector names, int number_of_bins);
RcppExport SEXP _phenovisr_phenovis_get_histogram(SEXP namesSEXP, SEXP number_of_binsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type names(namesSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_bins(number_of_binsSEXP);
    rcpp_result_gen = Rcpp::wrap(phenovis_get_histogram(names, number_of_bins));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_phenovisr_phenovis_read_mask", (DL_FUNC) &_phenovisr_phenovis_read_mask, 1},
    {"_phenovisr_phenovis_get_gcc_histogram", (DL_FUNC) &_phenovisr_phenovis_get_gcc_histogram, 2},
    {"_phenovisr_phenovis_get_histogram", (DL_FUNC) &_phenovisr_phenovis_get_histogram, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_phenovisr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
