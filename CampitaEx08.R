FalsePositionMethod <- function(
    f,
    a,
    b,
    macheps = 2^-23,
    max = 1000,
    verbose = TRUE
){
  given_a = a
  given_b = b
  if (f(a) * f(b) < 0) {
    ea = 100
    num_iterations = 0
    c_old = 0
    
    if (verbose) {
      # Print table headers
      cat(" Iter      a          b          c          f(a)          f(b)          f(c)        ea\n")
    }
    
    
    while (ea >= macheps && num_iterations < max) {
      c = (b * f(a) - a * f(b)) / (f(a) - f(b))
      if (f(c) == 0) break 
      if (f(c) * f(a) < 0) b = c
      else a = c
      
      if (num_iterations != 0) {
        ea = abs((c - c_old) / c) * 100
        c_old = c
      }
      
      if (verbose) {
        # print iteration as a table row
        cat(sprintf(
          "%3d %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f \n",
          num_iterations,
          a,
          b,
          c,
          f(a),
          f(b),
          f(c),
          ea
        ))
      }
      
      num_iterations = num_iterations + 1
    }
    
    if (verbose) {
      # print iteration as a table row
      cat(sprintf(
        "%3d %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f \n",
        num_iterations,
        a,
        b,
        c,
        f(a),
        f(b),
        f(c),
        ea
      ))
    }
    
    return (list(
      "f" = f,
      "given_a" = given_a,
      "given_b" = given_b,
      "c" = c,
      "iterations" = num_iterations,
      "ea" = ea
    ))
  }
}

SecantMethod <- function(
    f,
    x0,
    x1,
    macheps = 2^-23,
    max = 1000,
    verbose = TRUE
){
  given_x0 = x0
  given_x1 = x1
  
  num_iterations = 0
  ea = 100
  
  if (verbose) {
    # Print table headers
    cat(" Iter      x0          x1          x2          f(x0)          f(x1)          f(x2)        ea\n")
  }
  
  while (ea >= macheps && num_iterations != max) {
    y0 = f(x0)
    y1 = f(x1)
    
    x2 = x1 - y1 * (x1 - x0) / (y1 - y0)
    
    ea = abs((x2 - x1) / x2) * 100
    
    x0 = x1
    x1 = x2
    
    num_iterations = num_iterations + 1
    
    if (verbose) {
      # print iteration as a table row
      cat(sprintf(
        "%3d %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f \n",
        num_iterations,
        x0,
        x1,
        x2,
        f(x0),
        f(x1),
        f(x2),
        ea
      ))
    }
  }
  
  return (list(
    "f" = f,
    "given_x0" = given_x0,
    "given_x1" = given_x1,
    "x2" = x2,
    "iterations" = num_iterations,
    "ea" = ea
  ))
}