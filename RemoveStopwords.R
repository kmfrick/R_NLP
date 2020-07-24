#!/usr/bin/env Rscript
library(datasets);

cars <- mtcars;


b <- mean(cars$cyl);

i <- 1;
readCorpus <- function(dirname = "data") {
	corpus = list();
	files <- list.files(path=dirname, full.names=TRUE, pattern="\\.txt", all.files=TRUE);
	for (file in files) {
		part <- scan(file, what=character(), sep="\n", strip.white=TRUE);
		part <- strsplit(part, " ");
		corpus[[i]] <- unlist(part)
			i <- i + 1;
	}
	print(corpus[[5]]);
	return(corpus);
}



defaultStopWords<-c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")


removeStopwords <- function(toStrip, stopWords = defaultStopWords) {
	removeStopwordsFromVector <- function(vecToStrip) {
		result <- vecToStrip[!(vecToStrip %in% stopWords)];
		return(result);
	}
	result <- lapply(toStrip, removeStopwordsFromVector);
	return(result);
}


removeStopwords <- function(toStrip, stopWords = defaultStopWords) {
	removeStopwordsFromVector <- function(vecToStrip) {
		result <- vector();
		for (word in vecToStrip) {
			if (!(word %in% stopWords)) {
				result <- unlist(c(result, word));
			}
		}
		return(result);
	}
	result <- lapply(toStrip, removeStopwordsFromVector);
	return(result);
}


