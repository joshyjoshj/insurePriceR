#include <Rcpp.h>
#include <map>
using namespace Rcpp;

//' @rdname calcBasePremiumCpp
 // [[Rcpp::export]]
 DataFrame calcBasePremiumCpp(
     DataFrame df,
     double age_base,
     double age_factor,
     double bmi_base,
     double bmi_factor,
     double smoker_penalty,
     double arthritis_factor = 0.0,
     double diabetes_factor = 0.0,
     double heart_disease_factor = 0.0,
     double asthma_factor = 0.0,
     double hypertension_factor = 0.0,
     double none_factor = 0.0) {
   // # JB 20/01/2025 17:50:17 df columns to vectors for loop
   NumericVector age = df["age"];
   NumericVector bmi = df["bmi"];
   CharacterVector smoking_status = df["smoking_status"];
   CharacterVector conditions = df["pre_existing_conditions"];
   NumericVector annual_premium = df["annual_premium"];
   // # JB 20/01/2025 17:55:36 define loop size and new vectors
   int n = age.size();
   NumericVector new_premium(n);
   NumericVector risk_scores(n);

   // # JB 20/01/2025 18:20:47 create a map with conditions in
   std::map<std::string, double> condition_map = {
     {"arthritis", arthritis_factor},
     {"diabetes_type_2", diabetes_factor},
     {"heart_disease", heart_disease_factor},
     {"asthma", asthma_factor},
     {"hypertension", hypertension_factor},
     {"none", none_factor}
   };

   for(int i = 0; i < n; i++) {
     // # JB 20/01/2025 18:23:27 set condition factor and extract string from condition
     double condition_factor = 0.0;
     std::string cond = as<std::string>(conditions[i]);

     // # JB 20/01/2025 18:42:22 look up condition in map  anx define the factor i
     auto it = condition_map.find(cond);
     if(it != condition_map.end()) {
       condition_factor = it->second;
     }

     // # JB 20/01/2025 18:48:39 calculate risk score and add to vectors
     double risk = 1.0 +
       (age[i] - age_base) * age_factor +
       (bmi[i] - bmi_base) * bmi_factor +
       (smoking_status[i] == "y") * smoker_penalty +
       condition_factor;

     risk_scores[i] = risk;
     new_premium[i] = annual_premium[i] * risk;
   }

   // # JB 20/01/2025 19:00:45 add vectors to df
   df["base_premium_rcpp"] = new_premium;
   df["risk_score"] = risk_scores;
   return df;
 }
