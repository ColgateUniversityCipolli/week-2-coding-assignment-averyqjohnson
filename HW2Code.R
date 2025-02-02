
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
