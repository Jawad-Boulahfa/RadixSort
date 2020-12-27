#include <Rcpp.h>
using namespace Rcpp;
//using namespace std;


// [[Rcpp::export]]
List create_hoare_partition_Rcpp(NumericVector V_res)
{
  //NumericVector val = sample(V_res, 1);
  //double pivot = val[0];
  double pivot = sample(V_res, 1)[0];
  int i = 0;
  int j = V_res.length()-1;
  double tmp_i;
    while(true)
      {
        while(V_res[i] < pivot)
        {
          i += 1;
        }
        while(V_res[j] > pivot)
        {
          j -=  1;
        }
        if(i >= j)
        {
          return(List::create(Named("V")=V_res, Named("idx_pivot")=j));
          //return(List::create(Named("V")=V_res, Named("idx_pivot")=j,Named("pivot")=pivot));
        }

        tmp_i = V_res[i];
        V_res[i] = V_res[j];
        V_res[j] = tmp_i;
          if(V_res[i] == V_res[j])
          {
            j -= 1;
          }
      }
}


/*
create_three_way_partition <- function(V)
{
  
  V_res <- V
  

  pivot <- sample(V_res, 1)
    
    i <- 1

    j <- length(V_res)

      while(TRUE)
      {

        while(V_res[i] < pivot)
        {

          i <- i + 1
        while(V_res[j] > pivot)
        {
          j <- j - 1
        }
        
        
        if(i >= j)
        {
          return(list(new_pivot_index = j, new_V = V_res))
        }
        

        tmp_i <- V_res[i]
        V_res[i] <- V_res[j]
        V_res[j] <- tmp_i
          
          if(V_res[i] == V_res[j])
          {
            j <- j - 1
          }
      }
}
*/

// [[Rcpp::export]]
NumericVector quick_sort_Rcpp(NumericVector V)
{
  if(V.length() > 1){
    
    List partition = create_hoare_partition_Rcpp(V);
    NumericVector V=partition["V"];
    int pivot_index = partition["idx_pivot"];
    
    NumericVector V_left(pivot_index+1);
    for(int i=0;i<V_left.length();i++) V_left[i]=V[i];
    if(V_left.length() > 1) V_left = quick_sort_Rcpp(V_left);
    if(pivot_index == V.length())
    {
      return(V_left);
    }
    NumericVector V_right(V.length()-V_left.length());
    for(int i=0;i<V_right.length();i++) V_right[i]=V[V_left.length()+i];
    if(V_right.length() > 1) V_right = quick_sort_Rcpp(V_right);

    NumericVector output(V.length()); // Prend moins de temps que de réaffectr les valeurs de V.
    for(int i=0;i<V_left.length();i++) output[i]=V_left[i];
    for(int i=0;i<V_right.length();i++) output[i+V_left.length()]=V_right[i];
    
    return( output );
  }
  else
  {
    return(V);
  }
}

// [[Rcpp::export]]
NumericVector quick_sort_Rcpp_opti(NumericVector V)
{
  if(V.length() > 1){
    // A modifier
    //NumericVector tamp = sample(V, 1);
    //double pivot=tamp[0];
    //
    double pivot = sample(V, 1)[0];
    NumericVector V_left = V[V<pivot];

    if(V_left.length() > 1) V_left = quick_sort_Rcpp_opti(V_left);


    NumericVector V_right = V[V>pivot];
    if(V_right.length() > 1) V_right = quick_sort_Rcpp_opti(V_right);
      
    NumericVector V_middle = V[V==pivot];

    NumericVector output(V.length()); // Prend moins de temps que de réaffectr les valeurs de V.
    for(int i=0;i<V_left.length();i++) output[i]=V_left[i];
    for(int i=0;i<V_middle.length();i++) output[i+V_left.length()]=V_middle[i];
    for(int i=0;i<V_right.length();i++) output[i+V_left.length()+V_middle.length()]=V_right[i];
    
    return( output );
  }
  else
  {
    return(V);
  }
}


