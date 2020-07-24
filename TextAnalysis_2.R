
readCorpus <- function(dirname="data", fPattern="*.txt", tokenize=TRUE, scanSep=ifelse(tokenize,"","\n")) {
	files <- dir(dirname, pattern=fPattern)
	lapply(files, function(x) {
		scan(file.path(dirname,x), what=character(), quote=NULL,sep=scanSep)
	})
}

corpus <- readCorpus(tokenize=FALSE);

file <- corpus[[10]];
get.eur <- function(x) { return(grep("EUR [0-9]*", x, value=FALSE)); };
b<- get.eur(file);
print(b);

num.indef <- function(x) { return(length(grep("^(A |An )",x,  value=FALSE))); };
b <- as.double(num.indef(file))/length(file);
print(b);

get.qst <- function(x) { return(length(grep("\\? $", x, value=FALSE))); };
c <- get.qst(file);
print(c);
get.qst.perc <- function(x) { return(as.double(get.qst(x))/length(x)); };
d<-mean(unlist(lapply(corpus, get.qst.perc)));
print(d);

get.eur.perc <- function(x) { return(as.double(length(get.eur(x)))/length(x)); };
vec.eur <- unlist(lapply(corpus, get.eur.perc));
e <- which.max(vec.eur);
print(e);

get.china <- function(x) { return(length(grep("(China|Chinese|Beijing)", x, value=FALSE)) > 0); };
f <- which(unlist(lapply(corpus, get.china)));
print(f);

print(corpus[[28]]);

warnings();
