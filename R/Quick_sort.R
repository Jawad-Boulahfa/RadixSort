# But : séparer V en une partie gauche où les éléments et inférieurs au pivot
# et une partie droite où les éléments sont supérieurs au pivot
# Utilise l'Hoare partitioning pour garder une complexité de O(nlog(n))
# même dans le cas où tout les éléments sont identiques
create_hoare_partition <- function(V, pivot_index)
{
  
  V_res <- V
  
  # i concerne la partie gauche du vecteur (à gauche du pivot)
  i <- 1
  # j concerne la partie droite du vecteur (à droite du pivot)
  j <- length(V_res)
  
  # On répète la procédure tant qu'on a pas placé tout les éléments strictement inférieurs au pivot
  # à sa gauche et les éléments strictement supérieurs au pivot à sa droite
  while(TRUE)
  {
    # Si un élément est supérieur ou égal au pivot on s'arrête
    # Si i est plus grand que j on s'arrête aussi car on est pas au bon endroit
    # (on n'est plus dans la partie gauche du vecteur)
    while(V_res[i] < V_res[pivot_index])
    {
      # Tant que les éléments qu'on regarde en partant du début du vecteur
      # sont plus petits que le pivot, on incrémente l'indice
      # qui permet de compter les éléments à gauche
      # le if sert à ne pas dépasser la taille du vecteur
      #if(i < length(V_res))
      i <- i + 1
    }
    # Tant que les éléments qu'on regarde en partant de la fin du vecteur
    # sont plus grands que le pivot, on décrémente l'indice
    # qui permet de compter les éléments à droite
    # Si un élément est plus petit que le pivot, on s'arrête
    # Si j est plus petit que i on s'arrête aussi car on est pas au bon endroit
    # (on n'est plus dans la partie droite du vecteur)
    while(V_res[j] > V_res[pivot_index])
    {
      # le if sert à ne pas être en dessous du premier élément du vecteur
      # si j = 0, le if qui suit ne marche pas car la première comparaison renvoie un NULL
      # vu que V_res[0] n'existe pas et donc le if ne fonctionne pas
      #if(j > 1)
      j <- j - 1
      
    }
    
    # Si i >= j, on renvoie j et le vecteur modifié
    
    if(i >= j)
    {
      
      return(list(new_pivot_index = j, new_V = V_res))
      
    }
    
    # Si i < j, on échange V[i] et V[j] puis on recommence la procédure
    # Il faut les échanger car si on s'est arrêté avant "le milieu" pour i ou pour j
    # ça veut dire que dans le(s) cas en question, l'élément où on s'est arrêté
    # était mal placé.
    tmp_i <- V_res[i]
    V_res[i] <- V_res[j]
    V_res[j] <- tmp_i
    
    
  }
}


quick_sort <- function(V)
{
  if(length(V) > 1)
  {
    # Choix du pivot (choisi aléatoirement parmi les éléments du vecteur à trier)
    # sample(length(V), 1) renvoie un indice compris entre 1 et length(V)
    pivot_index <- sample(length(V), 1)
    # Le pivot correspond alors à V[pivot_index], on le stocke pour l'utiliser à la fin
    pivot <- V[pivot_index]
    
    # Avec create_hoare_partition, on sélectionne un élément comme pivot,
    # et on le place au bon endroit du tableau
    partition <- create_hoare_partition(V = V, pivot_index = pivot_index)
    
    V_partition <- partition$new_V
    pivot_index <- partition$new_pivot_index
    
    # Partie gauche: éléments à gauche du pivot
    V_left <- V_partition[1:pivot_index]
    # Partie droite: éléments à droite du pivot
    V_right <- V_partition[pivot_index+1:length(V_partition)]
    
    # On trie les éléments à gauche du pivot
    if(length(V_left) > 1)
      V_left <- quick_sort(V = V_left)
    
    # On trie les éléments à droite du pivot
    if(length(V_right) > 1)
      V_right <- quick_sort(V = V_right)
    
    res <- c(V_left, pivot, V_right)
    return(res)
  }
  else
  {
    return(V)
  }
}




#quick_sort <- function(V)
#{
#  V_tmp <- V
#  
#  if(length(V_tmp) > 1)
#  {
#    # Choix du pivot (choisi aléatoirement parmi les éléments du vecteur à trier)
#    # sample(length(V), 1) renvoie un indice compris entre 1 et length(V)
#    pivot_index <- sample(length(V_tmp), 1)
#    # Le pivot correspond alors à V[pivot_index], on le stocke pour l'utiliser à la fin
#    pivot <- V_tmp[pivot_index]
#    
#    # Avec create_hoare_partition, on sélectionne un élément comme pivot,
#    # et on le place au bon endroit du tableau
#    partition <- create_hoare_partition(V = V_tmp, pivot_index = pivot_index)
#    
#    V_partition <- partition$new_V
#    pivot_index <- partition$new_pivot_index
#    
#    # On trie les éléments à gauche du pivot
#    V_left <- quick_sort(V_partition[1:pivot_index-1])
#    
#    # On trie les éléments à droite du pivot
#    V_right <- quick_sort(V_partition[pivot_index+1:length(V_partition)])
#    
#    # On rassemble les résultats
#    res <- c(V_left, pivot, V_right)
#    
#    return(res)
#  }
#  else
#  {
#    return(V_tmp)
#  }
#}