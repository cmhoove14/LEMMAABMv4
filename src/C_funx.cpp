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
                            NumericVector hpi_hc, 
                            double hpi_mult, 
                            NumericVector essential, 
                            double essential_prob,
                            NumericVector t_symptoms, 
                            double symp_mult, 
                            CharacterVector state, 
                            double nosymp_state_mult, 
                            double symp_state_mult, 
                            NumericVector t_since_contact, 
                            double cont_mult, 
                            NumericVector res_inf, 
                            double res_mult, 
                            NumericVector adapt_site, 
                            double adapt_site_mult, 
                            int tests_avail,
                            double case_find_mult,
                            double hosp_mult) {
  
  int n = income.size(); // number agents
  double test_mult = 1+tests_avail*case_find_mult ; // test availability improves case identification, contact tracing, etc.
  NumericVector norm_hpi = hpi_hc + 1 + abs(min(hpi_hc)) ; // normalize hpi access to 1 since some scres negative
  
// Probabilities that are just product of input vectors  
  NumericVector adapt_prob = 1+adapt_site*adapt_site_mult;
  NumericVector income_prob = income*income_mult;
  NumericVector hpi_prob = norm_hpi*hpi_mult;
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

//' @title q_prob_fx
//' @return Vector of quarantine probabilities for all agents to make quarantine decisions
//' @export
// [[Rcpp::export]] 

NumericVector q_prob_fx(NumericVector contact, 
                        NumericVector t_since_contact,
                        double q_prob_contact,
                        NumericVector res_inf, 
                        double q_prob_resinf, 
                        NumericVector t_symptoms, 
                        double q_prob_symptoms, 
                        NumericVector tested, 
                        NumericVector infector,
                        double q_prob_testpos, 
                        NumericVector essential,
                        double q_prob_essential) {
  
  int n = contact.size(); // number agents

  // Probabilities that are conditional on agent characteristics loop through all agents    
  NumericVector cont_prob(n) ; 
  NumericVector res_prob(n) ;
  NumericVector symp_prob(n) ;  
  NumericVector test_prob(n) ;  
  NumericVector ess_prob(n) ;
  
  for (int i = 0; i < n; ++i) {
    cont_prob[i] = t_since_contact[i] > 0 ? q_prob_contact*(7-t_since_contact[i])/7 : 0 ; // weights recent contact more heavily
    res_prob[i] = res_inf[i] > 0 ? res_inf[i]*q_prob_resinf : 0 ; // More res infections, higher q_prob
    symp_prob[i] = t_symptoms[i] > 0 ? t_symptoms[i]*q_prob_symptoms : 0 ;  // Longer symptoms, higher q_prob
    test_prob[i] = (tested[i] == 1 && infector[i] == 1) ? q_prob_testpos : 0 ;
    ess_prob[i] = essential[i] == 1 ? q_prob_essential : 1 ; 
  }
  
  NumericVector q_prob = (cont_prob + res_prob + symp_prob + test_prob)*ess_prob ;
  
  return q_prob ;
}
