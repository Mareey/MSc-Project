V <- c(12,15,8,4,56,32,0,3,20,28,32,25,36,18)
S=32
l=1
h=length(V)

Bin_search_recursive <- function(V,S,l,h) {
  if ( h < l ) {
    return(FALSE)
  } else {
    m <- floor((l + h) / 2)
    
    if ( V[m] > S )
      Bin_search_recursive(V, S,l,m-1)
    else if ( V[m] < S )
      Bin_search_recursive(V, S, m+1, h)
    else
      return(m)
  }
}

Bin_search_recursive(V,S,1,14)

