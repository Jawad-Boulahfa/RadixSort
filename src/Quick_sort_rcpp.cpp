#include <Rcpp.h>
using namespace Rcpp;
//using namespace std;

/*
// [[Rcpp::export]]
int partition(double V[])
{
  double pivot = V[];
  int i = -1;
  int j = V.length();
  
  while(true)
  {
    do
    {
      i++;
    }
    while (V[i] < pivot);
    
    do
    {
      j--;
    }
    while (V[j] > pivot);
    
    if (i >= j)
      return j;
    
    std::swap(V[i], V[j]);
  }
}
*/


/*
// [[Rcpp::export]]
void quickSort(double V[])
{
  if (V.length() > 1)
  {
    int pivot_index = partition(V);
    

    quickSort(V);
    quickSort(V);
  }
}
*/

/*
create_hoare_partition <- function(V)
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
        }
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
    // A modifier
    NumericVector tamp = sample(V, 1);
    double pivot=tamp[0];
    //
  
    NumericVector V_left = V[V<=pivot];

    if(V_left.length() > 1) V_left = quick_sort_Rcpp(V_left);


    NumericVector V_right = V[V>pivot];
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


