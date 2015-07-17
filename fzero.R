fzero <- function(f, a, b, eps=1e-5){
  if (f(a)*f(b)>0)
    list(fail="finding root is fail!")
  else{
    while (abs(b-a)>eps){
      x <- (a+b)/2;
      if (f(a)*f(x)<0)
        b <- x  
      else
        a <- x
    }
    list(root=(a+b)/2, fun=f(x))
  }
}

