a <- function(num, vec) {
	return (num ^ (length(vec) * mean(vec)));
}


juliet <- readLines("data/tokenized-juliet.txt");
b <- juliet[5];


c <- c(mean(nchar(juliet)), sd(nchar(juliet)));


ttr <- function(x) { length(levels(factor(x)))/length(x) }

romeo <- readLines("data/tokenized-romeo.txt");
numSentencesJuliet <- length(juliet);
numSentencesRomeo <- length(romeo);
wordsJuliet <- unlist(strsplit(juliet, ' '));
wordsRomeo <- unlist(strsplit(romeo, ' '));
numWordsJuliet <- sum(length(wordsJuliet));
numWordsRomeo <- sum(length(wordsRomeo));
ttrJuliet <- ttr(wordsJuliet);
ttrRomeo <- ttr(wordsRomeo);

d <- data.frame(rbind(c('Romeo', numSentencesRomeo, numWordsRomeo, ttrRomeo), c('Juliet', numSentencesJuliet, numWordsJuliet, ttrJuliet)));
names(d) <- c('name', 'numSentences', 'numWords', 'ttr');

data <- read.csv("data/romeo-and-juliet.csv")
wordsSorted <- data[order(data$numOfWords, decreasing=TRUE),] 
e <- wordsSorted$name[1]


