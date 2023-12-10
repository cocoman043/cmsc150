MullerMethod <- function(
    f,
    x0,
    x1,
    x2,
    macheps = 2^-23,
    max = 1000,
    verbose = TRUE
){
  given_x0 = x0
  given_x1 = x1
  given_x2 = x2
  
  num_iterations = 0
  ea = 100
  
  if (verbose) {
    # Print table headers
    cat(" Iter      x0          x1          x2          x3          f(x0)          f(x1)          f(x2)          f(x3)        ea\n")
  }
  
  while (ea >= macheps && num_iterations != max) {
    y0 = f(x0)
    y1 = f(x1)
    y2 = f(x2)
    
    h0 = x1 - x0
    h1 = x2 - x1
    
    d0 =(y1 - y0) / h0
    d1 =(y2 - y1) / h1
    
    a = (d1 - d0) / (h1 + h0)
    b = a*h1 + d1
    c = y2
    
    sign = if (abs(b - sqrt(b^2 - 4*a*c + 0i)) < abs(b + sqrt(b^2 - 4*a*c + 0i))) 1 else -1
    
    positive_form = x2 + -2 * c / (b + sqrt(b^2 - 4*a*c + 0i))
    negative_form = x2 + -2 * c / (b - sqrt(b^2 - 4*a*c + 0i))
    
    x3 = if (sign == 1) positive_form  else negative_form
    
    if (verbose) {
      # print iteration as a table row
      cat(sprintf(
        "%3d %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n",
        num_iterations,
        x0,
        x1,
        x2,
        x3,
        f(x0),
        f(x1),
        f(x2),
        f(x3),
        ea
      ))
    }
    
    ea = abs((x3 - x2) / x3) * 100
    x0 = x1
    x1 = x2
    x2 = x3
    
    num_iterations = num_iterations + 1
  }
  
  return(list(
    "f" = f,
    "given_x0" = given_x0,
    "given_x1" = given_x1,
    "given_x2" = given_x2,
    "x3" = x3,
    "iterations" = num_iterations,
    "ea" = ea
  ))
}