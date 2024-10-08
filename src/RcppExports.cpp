// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// isHeterozygous
arma::Col<double> isHeterozygous(const arma::field<arma::Cube<unsigned char> >& geno, const arma::Col<int>& lociPerChr, arma::uvec lociLoc, int nThreads);
RcppExport SEXP _SIMplyBee_isHeterozygous(SEXP genoSEXP, SEXP lociPerChrSEXP, SEXP lociLocSEXP, SEXP nThreadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::field<arma::Cube<unsigned char> >& >::type geno(genoSEXP);
    Rcpp::traits::input_parameter< const arma::Col<int>& >::type lociPerChr(lociPerChrSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type lociLoc(lociLocSEXP);
    Rcpp::traits::input_parameter< int >::type nThreads(nThreadsSEXP);
    rcpp_result_gen = Rcpp::wrap(isHeterozygous(geno, lociPerChr, lociLoc, nThreads));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SIMplyBee_isHeterozygous", (DL_FUNC) &_SIMplyBee_isHeterozygous, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_SIMplyBee(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
