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

//' @title test_probs_fx
//' @return Vector of test probabilities for all agents to weight sampling to determine who is tested
//' @export
// [[Rcpp::export]] 

NumericVector test_probs_fx(NumericVector income, 
                            double income_mult,
                            NumericVector hpi, 
                            NumericVector essential, 
                            double essential_prob,
                            NumericVector t_symptoms, 
                            CharacterVector state, 
                            NumericVector t_since_contact, 
                            NumericVector res_inf, 
                            NumericVector adapt_site, 
                            double adapt_site_mult, 
                            int tests_avail,
                            double case_find_mult,
                            double symp_mult, 
                            double hpi_mult, 
                            double cont_mult, 
                            double res_mult, 
                            double nosymp_state_mult, 
                            double symp_state_mult, 
                            double hosp_mult) {
  
  int n = income.size(); // number agents
  double test_mult = 1+tests_avail*case_find_mult ; // test availability improves case identification, contact tracing, etc.
  
// Probabilities that are just product of input vectors  
  NumericVector adapt_prob = 1+adapt_site*adapt_site_mult;
  NumericVector income_prob = income*income_mult;
  NumericVector hpi_prob = hpi*hpi_mult;
  NumericVector symp_prob = 1+t_symptoms*symp_mult*adapt_prob*test_mult ;

// Probabilities that are conditional on agent characteristics loop through all agents    
  NumericVector ess_prob(n) ;
  NumericVector cont_prob(n) ; 
  NumericVector res_prob(n) ;
  NumericVector state_prob(n) ;  
    
  for (int i = 0; i < n; ++i) {
    ess_prob[i] = essential[i] == 1 ? essential_prob : 1 ; 
    ess_prob[i] = adapt_site[i] == 1 ? 1 : ess_prob[i]   ; // Adaptive site removes barriers to testing
    cont_prob[i] = t_since_contact[i] >= 3 ? cont_mult*adapt_prob[i]*test_mult : 1 ;
    res_prob[i] = res_inf[i] > 0 ? res_inf[i]*res_mult*adapt_prob[i]*test_mult : 1 ;
    state_prob[i] = ((state[i] == "Ia") || state[i] == "Ip") ? nosymp_state_mult*adapt_prob[i]*test_mult : 1 ;
    state_prob[i] = ((state[i] == "Im") || state[i] == "Imh") ? symp_state_mult*adapt_prob[i]*test_mult : 1 ;
    state_prob[i] = state[i] == "Ih" ? hosp_mult*adapt_prob[i] : 1 ;
  }
  
  NumericVector test_prob = income_prob*hpi_prob*ess_prob*(cont_prob+res_prob+state_prob+symp_prob) ;
    
  //normalize to 1
    test_prob = test_prob/max(test_prob) ;
    
    return test_prob ;
}