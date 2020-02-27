#include "bit_class.h"
#include <cmath> 


bitObj::bitObj(int bit_size) {
	eltNum = bit_size;
	int maxElt = ceil(bit_size / 8.0);
	bit_list.reserve(maxElt);
	for (int i = 0; i < maxElt; i++) {
		bitset<8> * bit = new bitset<8>;
		bit_list.push_back(bit);
	}
}



int bitObj::inter_sum(bitObj* another) {
	if (another->eltNum != eltNum) {
		Rf_error("The element numbers are not equal");
	}
	int result=0;
	for (unsigned int i = 0; i < bit_list.size(); i++) {
		result += ((*bit_list[i]) & (*another->bit_list[i])).count();
	}
	return result;
}

void bitObj::set_bit(IntegerVector index) {
	for (int i = 0; i < (int)index.length(); i++) {
		int base_index = floor(index[i] / 8.0);
		int off = index[i] - base_index* 8.0;
		//Rprintf("%d,%d,%d,%d\n", i, index[i],base_index, off);
		bit_list[base_index]->set(off);
	}
}

bitObj::~bitObj() {
	for (unsigned int i = 0; i < bit_list.size(); i++) {
		delete bit_list[i];
	}
}



string bitObj::to_string() {
	string str;
	for (int i = (int)bit_list.size()-1; i >= 0; i--) {
		//Rprintf("%d:%s\n", i, bit_list[i]->to_string().c_str());
		str.append(bit_list[i]->to_string());
	}
	return str;
}


int bitObj::count() {
	int result = 0;
	for (unsigned int i = 0; i < bit_list.size(); i++) {
		result += bit_list[i]->count();
	}
	return result;

}





