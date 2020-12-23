#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

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

// [[Rcpp::export]]
void quickSort(double V[])
{
  
  /* A finir */
  if (V.length() > 1)
  {
    int pivot_index = partition(V);
    

    quickSort(V);
    quickSort(V);
  }
}
