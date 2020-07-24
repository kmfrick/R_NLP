wwmat <- function(fileName, contextSize) {
	file <- tolower(scan(fileName, what=character(), quote=NULL,sep=" "));
	file <- file[!file %in% c("", ".", ",", "'", "\"", ";", ":")];
	tokens <- unique(file);

	wordWord <- list();
	length(wordWord) <- length(file);
	names(wordWord) <- file;

	for (i in 1:length(file)) {
		word <- file[i];
		# initialize empty rows
		if (is.null(wordWord[[word]])) {
			# create new named list
			wordWord[[word]] <- list();
			# set names = tokens
			length(wordWord[[word]]) <- length(tokens);
			names(wordWord[[word]]) <- tokens;
			# initialize to 0
			for (context in tokens) { wordWord[[word]][[context]] <- 0; }
		}
		# create word-word named list
		for (j in 1:contextSize) {
			if (i + j > length(file)) { break; }
			context <- file[i + j];
			wordWord[[word]][[context]] <- wordWord[[word]][[context]] + 1;
		}
	}

	return(wordWord);
}

print((wwmat("lol.txt", 2)));

