
#include <Rcpp.h>
using namespace Rcpp;
//using namespace std;

// [[Rcpp::export]]
IntegerVector tri_digit_Rcpp(IntegerVector V, int rank){
  int n =V.length();
  IntegerVector V_mod (n);
  IntegerVector res (n);
  IntegerMatrix mat_stock (10,n);
  IntegerVector vect_count(10);
  
  int tamp=pow(10,rank-1);
  for(int i=0;i<n;i++) V_mod[i]=(V[i]/tamp)%10;
  
  for(int i=0;i<n;i++){
    mat_stock(V_mod[i],vect_count[V_mod[i]])=V[i];
    vect_count[V_mod[i]]++;
  }
  
  
  IntegerVector tamp2=cumsum(vect_count); // on pourrait juste remplacer tamp2 par une màj du vecteur vect_count.
  for (int i=9; i>=0;i--){
    while(vect_count[i]>0){
      //cout<<mat_stock(i,vect_count[i]-1)<<endl;
      res[tamp2[i]-1]=mat_stock(i,vect_count[i]-1);
      vect_count[i]--;
      tamp2[i]--;
      
    }
  }
  
  return res;
}
  
  
// [[Rcpp::export]]
IntegerVector tri_digit_Rcpp_opti(IntegerVector V, int rank){
  int n =V.length();
  IntegerVector res (n);
  IntegerVector vect_count(10);
  
  IntegerVector V_idx(n); // les modulos de chaque élt de V. Pour éviter de les recalculer à chaque fois (on évite n calculs).
  int tamp=pow(10,rank-1);
  for(int i=0;i<n;i++){
    V_idx[i]=(V[i]/tamp)%10;
    vect_count[V_idx[i]]++;
  }
  
  IntegerVector tamp2=cumsum(vect_count);
  for (int i=n-1; i>=0;i--){
      //int idx=(v[i]/tamp)%10;
      res[tamp2[V_idx[i]]-1]=V[i];
      tamp2[V_idx[i]]--;
  }
  
  return res;
}


// [[Rcpp::export]]
IntegerVector radix_sort_Rcpp(IntegerVector V){
  int elt_max=max(V);
  for(int i=1;i<=trunc(log10(elt_max)+1);i++){
    V=tri_digit_Rcpp_opti(V,i);
  }
  return V;
}



/*** R
tri_digit_Rcpp_opti(50:0,1)
*/

/*** R
radix_sort_Rcpp(100:1)
*/

