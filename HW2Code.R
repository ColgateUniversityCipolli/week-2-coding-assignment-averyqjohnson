
###############################################################################
# Homework 2 
###############################################################################

###############################################################################
# Problem 1
###############################################################################

# create a function that computes the integrand
integrand <- function(x){
  f <- 7 - 2 * x^2
  return(f)
}
# set up an example choice of a, b, and the # of rectanges
a <- 0
b <- 2
n.rect <- 100
(delta.x <- (b-a)/n.rect)

# compute the area using the left rule
left.points <- a + 0:99*(delta.x)
(left.area <- sum(delta.x*(integrand(left.points))))

# compute the area using the right rule
right.points <- a + 1:100*(delta.x)
(right.area <- sum(delta.x*(integrand(right.points))))
# compute the area using the midpoint rule

# compute the area using the trapezoidal rule
mid.points <- (left.points+right.points)/2
(mid.area <- sum(delta.x*(integrand(mid.points))))

# write the code that computes the area using trapezoidal rule
(trapezoidal_area <- (delta.x/2) * sum(integrand(left.points) + integrand(right.points)))

# write a function that takes a, b, and the # of rectangles in as input and
# returns the trapezoidal rule by default
riemann.sums <- function(fnct,                        # function to integrate
                         a,                           # lower bound of integral
                         b,                           # upper bound of integral
                         n.rect,                      # number of  bound of integral
                         method = "Trapezoidial"){    # method to use (trap by default)
  ######################################
  # Check Input
  ######################################
  if(!is.numeric(a)){ # if a is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!is.numeric(b)){ # if b is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!(is.numeric(n.rect)) | (n.rect%%1!=0)){ # if n.rect is not a whole number
    stop("The number of rectangles must be a positive whole number.")
  }
  ######################################
  # Compute Area
  ######################################
  if(method == "Left"){
    left.points <- a + 0:99*(delta.x)
    area <- sum(delta.x*(integrand(left.points)))
  }else if(method == "Right"){
    right.points <- a + 1:100*(delta.x)
    area <- sum(delta.x*(integrand(right.points)))
  }else if(method == "Midpoint"){
    mid.points <- (left.points+right.points)/2
    area <- sum(delta.x*(integrand(mid.points)))
  }else if(method == "Trapezoidial"){
    area <- (delta.x/2) * sum(integrand(left.points) + integrand(right.points))
  }else{
    stop("Please select a valid method (e.g., 'Left', 'Right', 'Midpoint', 'Trapezoidial')")
  }
  ######################################
  # Return the area
  ######################################
  return(area)
}
######################################
# Test the function
######################################
riemann.sums(fnct = integrand,
             a = 0,
             b = 2,
             n.rect = 100)
######################################
# Compare to numerical integral
######################################
integrate(f = integrand, # integrate() is an R function
          lower = 0,     # that completes numerical
          upper = 2)     # integration