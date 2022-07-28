// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// numUnpack
IntegerMatrix numUnpack(IntegerVector pack);
RcppExport SEXP _GGIRread_numUnpack(SEXP packSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type pack(packSEXP);
    rcpp_result_gen = Rcpp::wrap(numUnpack(pack));
    return rcpp_result_gen;
END_RCPP
}
// resample
NumericMatrix resample(NumericMatrix raw, NumericVector rawTime, NumericVector time, int stop, int type);
RcppExport SEXP _GGIRread_resample(SEXP rawSEXP, SEXP rawTimeSEXP, SEXP timeSEXP, SEXP stopSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type raw(rawSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rawTime(rawTimeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< int >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(resample(raw, rawTime, time, stop, type));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_GGIRread_numUnpack", (DL_FUNC) &_GGIRread_numUnpack, 1},
    {"_GGIRread_resample", (DL_FUNC) &_GGIRread_resample, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_GGIRread(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}