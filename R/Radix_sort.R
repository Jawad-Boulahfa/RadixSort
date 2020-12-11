tri_digit <- function(V, rank)
{
  # n = taille du vecteur V (nombre d'éléments à trier)
  n <- length(V)
  
  # V_mod = Vecteur qui contient à un indice j
  # le reste de la division euclidienne de la partie entière de V[j]/10^(rank-1) par 10.
  # Autrement dit, V_mod[j] chiffre des unités de V[j] si rank = 1, celui des dizaines si rank = 2, etc.
  V_mod = floor(V/10^(rank-1))%%10
  
  # Equivaut à 10 boites capable de stocker au maximum n valeurs.
  # On construit 10 boites pour les 10 chiffres possibles (0 à 9).
  # On stocke au maximum n valeurs si tout les nombres de la liste sont sur la même ligne de la matrice.
  #
  mat_stock <- matrix(nrow = 10, ncol = n) 
  
  # vect_count = compte le nombre d'éléments dans chacune des 10 boîtes
  # on initialise donc tout ses éléments à 0
  vect_count <- rep(0, 10)
  
  # res contiendra le tri des éléments de V selon le rank
  # (i.e. selon les unités si rank = 1, selon les dizaines et les unités si rank = 2, etc.)
  res <- rep(0, n) 
  
  for (i in 1:n)
  {
    # Il y a un +1 dans le tableau car l'indicage commence à 1 en R et non à 0 (les boites commencent à 0)
    vect_count[V_mod[i]+1] <- vect_count[V_mod[i] + 1] + 1 
    mat_stock[V_mod[i]+1,vect_count[V_mod[i]+1]]=V[i]
  }
  
  tamp <- cumsum(vect_count)
  # On parcourt du plus grand au plus petit chiffre possible
  for (i in 10:1)
  {
    # But de la boucle: se servir de tamp pour trier les éléments de V selon le chiffre rank
    while(vect_count[i]>0)
    {
      res[tamp[i]]=mat_stock[i,vect_count[i]]
      vect_count[i]=vect_count[i]-1
      tamp[i]=tamp[i]-1
    }
  }
  return(res)
}


tri_digit_opti <- function(V,rank){
  n <- length(V)
  vect_count <- rep(0,10)
  res <- rep(0,n)
  tamp <- 10^(rank-1)
  V_idx <- floor(V/tamp)%%10+1 # les modulos de chaque élt de v +1, pour éviter de les recalculer à chaque fois. On évite n calculs
  #donc gain de temps de calcul, pour un vecteur de taille N en + à stocker.
  # Il y a un +1 dans le tableau car l'indicage commence à 1 en R.
  
  for (i in 1:n){ 
    #idx=floor(V[i]/tamp)%%10+1  
    vect_count[V_idx[i]] <- vect_count[V_idx[i]]+1
  }
  tamp2 <- cumsum(vect_count)
  
  for (i in n:1){
    #idx=floor(V[i]/tamp)%%10 + 1;
    res[tamp2[V_idx[i]]]=V[i];
    tamp2[V_idx[i]]=tamp2[V_idx[i]]-1;
  }

  return(res)
}

radix_sort <- function(V)
{
  elt_max <- max(V) # élément maximum de la liste
  #print(elt_max)
  #print(trunc(log(elt_max, base=10)+1))
  
  # Le "rank" dans la boucle, c'est la place du digit qu'on cherche à regarder
  # Pour rank allant de 1 au nombre de digits maximum
  # effectuer le tri selon le chiffre à la position rank (1 = unités, 2 = dizaines, etc.).
  # Cette boucle permet de trier les éléments de V d'abord selon les unités,
  # puis selon les dizaines et les unités, puis selon les centaines, les dizaines et les unités, etc.
  for (rank in (1: (trunc(log(elt_max, base=10))+1) ) )
  {
    V <- tri_digit_opti(V, rank)
  }
  return(V)
}