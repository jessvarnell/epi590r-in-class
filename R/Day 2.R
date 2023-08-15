#using renv


install.packages("renv")


renv::init()
renv::snapshot()
renv::status()

install.packages("praise")

library(praise)
praise()

install.packages("fortunes")
library(fortunes)
fortune()

emo::ji("banana")
emo::ji("dolphin")
emo::ji("doctor")

library(emo)
ji("mango")

ji("sad")
ji("giraffe")
ji("redhead")
ji("sushi")
ji("rubber duck")
ji("muffin")
ji("green_pepper")

renv::install("fortunes")
renv::install("hadley/emo")
renv::install("praise")

renv::snapshot()

emo::ji


#Functions

x <- c(1,3,5,7,9)
n <- length(x)
sum(x) / n
newmean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

x <- c(0,1,1)
multiplier <- 100

prop <- function(x, multiplier) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

prop(x = c(0,1,1,0,1))

#Functions.R

x <- 3
x^2

square <- function(x){
	value <- x^2
	return(value)
}

square(4)

raise <- function(x, power){
	expo <- x^power
	return(expo)
}

raise(x=2, power=3)

raise <- function(x, power=2){
	expo <- x^power
	return(expo)
}

raise(x=7)

raise(x=7, power=4)

raise(x=2, power=100)

raise(3)

