# start out with a number to test
x <- 53
# you'll want your function to return this number
x^2
square <- function(x) {
	thesquare <- x^2
	return(thesquare)
}
# test it out
square(x)
square(53)
53^2 # does this match?
