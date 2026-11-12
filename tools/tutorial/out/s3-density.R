# How many of the 1,560 possible ties exist at each survey?
tapply(ties$confides, ties$wave, sum)

# What happened to the ties and the non-ties of the first survey?
table(first = ties$confides[ties$wave == 1], second = ties$confides[ties$wave == 2])
