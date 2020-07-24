tfIdf <- function(dirname="data", fPattern="") {
	files <- dir(dirname, pattern=fPattern);
	idf <- list();
	tf <- list();
	for (file in files) {
		# Read the current file
		tokens <- scan(file.path(dirname,file), what=character(), quote=NULL,sep="");
		# Generate token counts
		counts <- table(tokens);
		# tf = token count / n. of tokens
		tf[[file]] <- counts / as.double(length(tokens));
		for (t in names(counts)) {
			# initialize elements of idf to 0
			if (is.null(idf[[t]])) { idf[[t]] <- 0; };
			# keep track of how many times a term is mentioned
			idf[[t]] <- idf[[t]] + 1;
		}

	}
	# ugly for loop to calculate idf
	for (x in idf) {
		x <- length(files) / x;
	}
	tfIdf <- list();
	# generate tf-idf as named list of named lists
	for (file in files) {
		# only get idf values for tokens in the current documents
		fileIdf <- idf[names(idf) %in% names(tf[[file]])]
		tempTfIdf <- as.numeric(tf[[file]]) * as.numeric(fileIdf);
		# restore names as they were deleted by as.numeric
		names(tempTfIdf) <- names(tf[[file]]);
		# use zeros for terms not appearing in a document
		tfIdf[[file]] <- list();
		for (term in names(idf)) {
			if (term %in% names(tempTfIdf)) {
				tfIdf[[file]][[term]] <- tempTfIdf[[term]];
			} else {
				tfIdf[[file]][[term]] <- 0;
			}
		}


	}
	# Use sapply to convert to matrix
	tfIdfMat <- sapply (tfIdf, function (x) { return (x)});
	return(tfIdfMat);
}



print(tfIdf());
