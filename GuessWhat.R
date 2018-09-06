library(purrr)

read.int <- compose(strtoi, readline)

pp <- compose(print, paste)

pair <- function(a, b) c(a, b)

continue <- partial(pair, a="continue")
win <- partial(pair, a="win")
lose <- partial(pair, a="lose")

main <- function() {

	pp("GUESS WHAT!!!!!!!!!!??!?!?!?!?!!??!!?")
	max.num <- 100
	rand.from.to <- function(from, to) runif(1, min=from, max=to)
	rand.0.to <- partial(rand.from.to, from=0)
	rand.0.max.num <- partial(rand.0.to, to=max.num)
	magic.number <- compose(floor, rand.0.max.num)

	tries <- 10
	lucky.number <- magic.number()

	loop <- function(tries.remaining) {
		next.state <- {
			if (tries.remaining <= 0) lose("you lose")
			else {
				guess <- read.int("Enter number: ")
				if (is.na(guess)) continue("invalid input")
				else if (guess == lucky.number) win("you win")
				else if (guess < lucky.number) continue("too low")
				else continue("too high")
			}
		}

		pp(next.state[2])
		if (next.state[1] == "continue") loop(tries.remaining - 1)
	}

	loop(tries)
}
