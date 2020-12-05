tri_digit <- function(V,rank){
  n=length(V)
  V_mod=floor(V/10^(rank-1))%%10
  mat_stock=matrix(nrow=10,ncol=n)
  vect_count=rep(0,10)
  res=rep(0,n)
  
  for (i in 1:n){
    vect_count[V_mod[i]+1]=vect_count[V_mod[i]+1]+1 # Il y a un +1 dans le tableau car l'indicage commence à 1 en R.
    mat_stock[V_mod[i]+1,vect_count[V_mod[i]+1]]=V[i]
  }
  tamp=cumsum(vect_count)
  for (i in 10:1){
    while(vect_count[i]>0){
      res[tamp[i]]=mat_stock[i,vect_count[i]]
      vect_count[i]=vect_count[i]-1
      tamp[i]=tamp[i]-1
    }
  }
  return(res)
}


tri_digit_opti <- function(V,rank){
  n=length(V)
  vect_count=rep(0,10)
  res=rep(0,n)
  tamp=10^(rank-1)
  V_idx=floor(V/tamp)%%10+1 # les modulos de chaque élt de v +1, pour éviter de les recalculer à chaque fois. On évite n calculs
  #donc gain de temps de calcul, pour un vecteur de taille N en + à stocker.
  # Il y a un +1 dans le tableau car l'indicage commence à 1 en R.
  
  for (i in 1:n){ 
    #idx=floor(V[i]/tamp)%%10+1  
    vect_count[V_idx[i]]=vect_count[V_idx[i]]+1
  }
  tamp2=cumsum(vect_count)
  
  for (i in n:1){
    #idx=floor(V[i]/tamp)%%10 + 1;
    res[tamp2[V_idx[i]]]=V[i];
    tamp2[V_idx[i]]=tamp2[V_idx[i]]-1;
  }

  return(res)
}

Radixsort <- function(V){
  elt_max=max(V) 
  #print(mx)
  #print(trunc(log(mx, base=10)+1))
  for (i in (1: (trunc(log(elt_max, base=10))+1) ) ){
    V=tri_digit_opti(V,i)
  }
  return(V)
}