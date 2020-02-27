#include <Rcpp.h>
#include "bit_class.h"
using namespace Rcpp;


void bitObj_finalizer(SEXP x) {
		delete (bitObj*)R_ExternalPtrAddr(x);
}

// [[Rcpp::export]]
SEXP get_bit_obj(int bit_size) {
	void* ptr = (void*)new bitObj(bit_size);
	SEXP R_ptr = Rf_protect(R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(R_ptr, bitObj_finalizer);
	Rf_unprotect(1);
	return R_ptr;
}


// [[Rcpp::export]]
void set_bit_obj(SEXP R_ptr, IntegerVector index) {
	bitObj* ptr = (bitObj*)R_ExternalPtrAddr(R_ptr);
	ptr->set_bit(index);
}
// [[Rcpp::export]]
int get_bit_count(SEXP R_ptr) {
	bitObj* ptr = (bitObj*)R_ExternalPtrAddr(R_ptr);
	return ptr->count();
}
// [[Rcpp::export]]
std::string print_bit(SEXP R_ptr) {
	bitObj* ptr = (bitObj*)R_ExternalPtrAddr(R_ptr);
	return ptr->to_string();
}

// [[Rcpp::export]]
int get_inter_number(SEXP R_ptr1, SEXP R_ptr2) {
	bitObj* ptr1 = (bitObj*)R_ExternalPtrAddr(R_ptr1);
	bitObj* ptr2 = (bitObj*)R_ExternalPtrAddr(R_ptr2);
	return ptr1->inter_sum(ptr2);
}






// [[Rcpp::export]]
NumericVector get_list_inter_number(List bit_list, IntegerVector list_index, SEXP bit) {
	NumericVector res(list_index.length());
	for (unsigned int i = 0; i < list_index.length(); i++) {
		res[i] = get_inter_number(bit_list[list_index[i]], bit);
	}
	return res;
}

// [[Rcpp::export]]
StringVector print_bit_list(List bit_list) {
	StringVector res(bit_list.length());
	for (unsigned int i = 0; i < bit_list.length(); i++) {
		res[i] = print_bit(bit_list[i]);
	}
	return res;
}




