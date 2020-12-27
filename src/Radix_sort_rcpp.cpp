#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;
//using namespace std;

// [[Rcpp::export]]
IntegerVector modulo(NumericVector V, int m=10){
  IntegerVector res(V.length());
  //for(int i=0;i<V.length();i++) res[i]=V[i]-trunc(V[i]/m)*m;
  for(int i=0;i<V.length();i++) res[i]=(int)std::fmod(V[i],m);
  return(res);
}


/*
 // [[Rcpp::export]]
 NumericVector tri_digit_Rcpp(NumericVector V, int rank){
 int n =V.length();
 IntegerVector V_mod (n);
 NumericVector res (n);
 NumericMatrix mat_stock (10,n);
 IntegerVector vect_count(10);
 
 int tamp=pow(10,rank-1);
 //for(int i=0;i<n;i++) V_mod[i]=modulo(trunc(V[i]/tamp),10);
 V_mod=modulo(trunc(V/tamp),10);
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
 */

/*
 // [[Rcpp::export]]
 NumericVector radix_sort_Rcpp(NumericVector V){
 
 
 int elt_max=max(V);
 for(int i=1;i<=trunc(log10(elt_max)+1);i++){
 V=tri_digit_Rcpp_opti(V,i);
 }
 return V;
 }*/

/* CODE OPTI */

// [[Rcpp::export]]
NumericVector tri_digit_Rcpp_opti(NumericVector V, int rank){
  int n =V.length();
  NumericVector res (n);
  IntegerVector vect_count(10);
  
  IntegerVector V_idx(n); // les modulos de chaque élt de V. Pour éviter de les recalculer à chaque fois (on évite n calculs).
  int tamp=pow(10,rank-1);
  V_idx=modulo(trunc(V/tamp),10);
  
  for(int i=0;i<n;i++){
    //V_idx[i]=modulo(floor(V[i]/tamp),10);
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
NumericVector radix_sort_Rcpp(NumericVector V){
  
  double elt_max=0;
  NumericVector V_neg=abs(V[V<0]);
  
  if(V_neg.length()>1){
    elt_max=max(V_neg);
    for(int rank=1;rank<=trunc(log10(elt_max))+1;rank++) V_neg=tri_digit_Rcpp_opti(V_neg,rank);
  }
  
  NumericVector V_pos= V[V>=0];
  if(V_pos.length()>1){
    elt_max=max(V_pos);
    for(int rank=1;rank<=trunc(log10(elt_max))+1;rank++) V_pos=tri_digit_Rcpp_opti(V_pos,rank);
  }
  
  NumericVector output(V.length()); // Prend moins de temps que de réaffectr les valeurs de V.
  for(int i=0;i<V_neg.length();i++) output[i]=-V_neg[V_neg.length()-1-i];
  for(int i=0;i<V_pos.length();i++) output[i+V_neg.length()]=V_pos[i];
  return( output );
}


/* CODE Décimal */

// [[Rcpp::export]]
int nb_decim(double nb){
  int idx=0;
  while( (idx<9) && (trunc(nb*pow(10,idx))!=nb*pow(10,idx)) ){
    idx++;
  }
  return(idx);
}

/*
 // [[Rcpp::export]]
 IntegerVector nb_decim_vect(NumericVector V){
 //IntegerVector output(V);
 //for(i=0;i<V.length();i++) output[i]=nb_decim(V[i]);
 return(sapply(V,nb_decim));
 }
 */

// [[Rcpp::export]]
NumericVector tri_digit_Rcpp_decimal(NumericVector V, int rank){
  int n =V.length();
  int tamp;
  NumericVector res (n);
  IntegerVector vect_count(10);
  
  IntegerVector V_idx(n); // les modulos de chaque élt de V. Pour éviter de les recalculer à chaque fois (on évite n calculs).
  
  if(rank>0){tamp=pow(10,rank-1) ; V_idx = modulo(trunc(trunc(V)/tamp),10) ;}
  if(rank<=0){tamp=pow(10,-rank+1) ;V_idx = modulo(trunc( (V - trunc(V))* tamp),10);}
  
  for(int i=0;i<n;i++){
    //V_idx[i]=modulo(floor(V[i]/tamp),10);
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
NumericVector radix_sort_Rcpp_decimal(NumericVector V){
  int nb_decimal;
  double elt_max;
  NumericVector V_neg=abs(V[V<0]);
  
  if(V_neg.length()>1){
    nb_decimal=-max(sapply(V_neg,nb_decim));
    elt_max=max(V_neg);
    for(int rank=(nb_decimal+1);rank<=trunc(log10(elt_max))+1;rank++) V_neg=tri_digit_Rcpp_decimal(V_neg,rank);
  }
  
  NumericVector V_pos= V[V>=0];
  if(V_pos.length()>1){
    nb_decimal=-max(sapply(V_pos,nb_decim));
    elt_max=max(V_pos);
    for(int rank=(nb_decimal+1);rank<=trunc(log10(elt_max))+1;rank++) V_pos=tri_digit_Rcpp_decimal(V_pos,rank);
  }
  
  NumericVector output(V.length()); // Prend moins de temps que de réaffectr les valeurs de V.
  for(int i=0;i<V_neg.length();i++) output[i]=-V_neg[V_neg.length()-1-i];
  for(int i=0;i<V_pos.length();i++) output[i+V_neg.length()]=V_pos[i];

  return( output );
}
