#include <Rcpp.h>
#include <bitset>
#include <vector>
#include <string>
using namespace Rcpp;
using std::bitset;
using std::vector;
using std::string;


class bitObj
{
public:
	int eltNum;
	vector<bitset<8>*> bit_list;


	bitObj(int eltNum);
	int inter_sum(bitObj* another);
	int count();
	void set_bit(IntegerVector index);
	string to_string();
	~bitObj();
};

