#include <Rcpp.h>
using namespace Rcpp;
//Most of the functions in this files should be C++ implementation of some R functions.

// [[Rcpp::export]]
SEXP C_get_range_by_bound(SEXP R_sx, SEXP R_l, SEXP R_h) {
	double* sx = REAL(R_sx);
	double* l = REAL(R_l);
	double* h = REAL(R_h);
	R_xlen_t nx = XLENGTH(R_sx);
	R_xlen_t n = XLENGTH(R_l);
	NumericVector L(n);
	NumericVector H(n);
	int index_x_l = 0;
	int index_x_h = nx-1;
	for (auto i = 0; i < n; i++) {
		//Rprintf("i:%d,l:%d\n", i, index_x_l);
		while (sx[index_x_l] <= l[i] || index_x_l < i) {
			index_x_l += 1;
			if (index_x_l == nx) {
				return(R_NilValue);
			}
		}
		L[i] = index_x_l + 1;

		auto j = n - i - 1;
		//Rprintf("j:%d,h:%d\n",j , index_x_h);
		while (sx[index_x_h] > h[j] || nx - index_x_h - 1 < i) {
			if (index_x_h == 0) {
				return(R_NilValue);
			}
			index_x_h -= 1;
		}
		H[j] = index_x_h + 1;
		index_x_h -= 1;
	}
	//Rf_PrintValue(L);
	//Rf_PrintValue(H);
	for (auto i = 0; i < n; i++) {
		if (L[i] > H[i]) {
			return(R_NilValue);
		}
	}
	return List::create(Named("L") = L, Named("H") = H);
}

// [[Rcpp::export]]
double C_GW_compute_FDR(SEXP sorted_i, SEXP R_H, SEXP R_L,int rj_num,int n) {
	double* H = REAL(R_H);
	double* L = REAL(R_L);
	//int* sorted_i = INTEGER(R_sorted_i);
	int i = 0;
	int j = 0;
	int FP = 0;
	while (i<rj_num&&j<n) {
		int cur_elt = INTEGER_ELT(sorted_i, i);
		if (cur_elt >= L[j] && cur_elt <= H[j]) {
			FP += 1;
			i += 1;
			j += 1;
		}
		else {
			if (cur_elt < L[j]) {
				i += 1;
			}
			else {
				if (cur_elt > H[j]) {
					j += 1;
				}
			}
		}
	}
	return (double)FP / rj_num;
}