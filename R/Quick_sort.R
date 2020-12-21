# But : séparer V en une partie gauche où les éléments et inférieurs au pivot
# et une partie droite où les éléments sont supérieurs au pivot
# Utilise l'Hoare partitioning pour garder une complexité de O(nlog(n))
# même dans le cas où tout les éléments sont identiques
create_hoare_partition <- function(V, pivot_index)
{
  # On commence par échanger le pivot avec le premier élément du vecteur à trier
  tmp <- V[1]
  V[1] <- V[pivot_index]
  v[pivot_index] <- tmp
  
  i <- 0
  j <- length(V) + 1
  
  # On répète la procédure tant qu'on a pas placé tout les éléments strictement inférieurs au pivot
  # à sa gauche et les éléments strictement supérieurs au pivot à sa droite
  while(TRUE)
  {
    while(TRUE)
    {
      # Tant que les éléments qu'on regarde en partant du début du vecteur
      # sont plus petits que le pivot, on incrémente l'indice
      # qui permet de compter les éléments à gauche
      i <- i + 1
      # Si un élément est supérieur ou égal au pivot on s'arrête
      if(V[i] >= V[pivot_index])
      {
        break
      }
    }
    # Tant que les éléments qu'on regarde en partant de la fin du vecteur
    # sont plus grands que le pivot, on décrémente l'indice
    # qui permet de compter les éléments à droite
    while(TRUE)
    {
      j <- j - 1
      # Si un élément est plus petit que le pivot, on s'arrête
      if(V[j] <= V[pivot_index])
      {
        break
      }
      if(i < j)
      {
        # Si i < j, on échange V[i] et V[j] puis on recommence la procédure
        # Il faut les échanger car si on s'est arrêté avant "le milieu" pour i ou pour j
        # ça veut dire que dans le(s) cas en question, l'élément où on s'est arrêté
        # était mal placé.
        tmp_i <- V[i]
        V[i] <- V[j]
        V[j] <- tmp_i
      }
      else
      {
        # Sinon on renvoie j (ou i car i = j, i.e. on est "au milieu"
        # i et j pointent sur un élément égal au pivot 
        return(list(new_pivot_index = j, new_V = V))
      }
      
      
    }

  }
}





quick_sort <- function(V)
{
  if(length(V) > 1)
  {
    # Choix du pivot (choisi aléatoirement parmi les éléments du vecteur à trier)
    # sample(length(V), 1) renvoie un indice compris entre 1 et length(V)
    # Le pivot correspond alors à V[pivot_index]
    pivot_index <- sample(length(V), 1)
    
    # On sélectionne un élément comme pivot, et on le place au bon endroit du tableau
    partition <- create_hoare_partition(V = V, pivot_index = sample(length(V), 1))
    V <- partition$new_V

    pivot_index <- partition$new_pivot_index
    
    # On trie les éléments à gauche du pivot
    V_left <- quick_sort(V[1:pivot_index-1])
    
    # On trie les éléments à droite du pivot
    V_right <- quick_sort(V[pivot_index+1:length(V)])
    
    res <- c(V_left, V[pivot_index], V_right)
    
    return(res)
  }
  else
  {
    return(V)
  }
}