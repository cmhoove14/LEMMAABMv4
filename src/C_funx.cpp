#include <Rcpp.h>
using namespace Rcpp;

//' @title GetCTVisit_cpp
//' @return Vector of CT indices visitied by agents
//' @export
// [[Rcpp::export]] 
NumericVector GetCTVisit_cpp(NumericVector agents, NumericVector s, NumericMatrix cdfs) {
  int n = agents.size(); 
  NumericVector out(n); 
  for (int i = 0; i < n; ++i) {
    int a = agents[i]-1;
    NumericVector cdf = cdfs(a, _ ); 
    int m = cdf.size(); 
    for (int j = 0; j < m; ++j) {
      if (s[i] < cdf[j]) {
        out[i] = j + 1;
        break;
      }
    }
  }
  return out;
}
