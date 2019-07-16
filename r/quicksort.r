Quick_Sort <- function(V,n) {  
  if (n <= 1) return(V)
  left <- 0 ##start from left prior first element
  right <- n  ##start from rightmost element
  v <- V[n] ## initialize last element as pivot element
  
  ## Partition implementation
  repeat {
    while (left < n && V[left+1]  < v) left <- left+1
    while (right > 1 && V[right-1] >= v) right <- right-1
    if (left >= right-1) break
    ## Swap elements to put pivot in place
    temp <- V[left+1]
    V[left+1] <- V[right-1]
    V[right-1] <- temp
  }
  
  ## Recursive implementation of Quick sort
  if (left == 0) return(c(V[n], Quick_Sort(V[1:(n-1)],n=(n-1))))
  if (right == n) return(c(Quick_Sort(V[1:(n-1)],n=(n-1)), V[n]))
  return( c(Quick_Sort(V[1:left],n=left), V[n], Quick_Sort(V[(left+1):(n-1)],n=(n-left-1))))
}

Quick_Sort(V= c(20,12,65,8,10,16,43,35,23,88,2,56,41,27,67,55),n=16)
