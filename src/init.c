#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP _GGIRread_AxivityNumUnpack(SEXP);
extern SEXP _GGIRread_resample(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _GGIRread_GENEActivReader(SEXP, SEXP, SEXP, SEXP);
extern SEXP _GGIRread_find_matrix_packet_start(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_GGIRread_AxivityNumUnpack", (DL_FUNC) &_GGIRread_AxivityNumUnpack, 1},
    {"_GGIRread_GENEActivReader", (DL_FUNC) &_GGIRread_GENEActivReader, 4},
    {"_GGIRread_resample",  (DL_FUNC) &_GGIRread_resample,  5},
    {"_GGIRread_find_matrix_packet_start",  (DL_FUNC) &_GGIRread_find_matrix_packet_start,  3},
    {NULL, NULL, 0}
};

void R_init_GGIRread(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
