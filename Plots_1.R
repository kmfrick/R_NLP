readCorpus <- function(dirname="data", fPattern="*.txt", tokenize=TRUE, scanSep=ifelse(tokenize,"","\n")) {
	files <- dir(dirname, pattern=fPattern)
	lapply(files, function(x) {
		scan(file.path(dirname,x), what=character(), quote=NULL,sep=scanSep)
	})
}


corpus <- readCorpus("data", tokenize=TRUE)

ttr <- function(x) { length(levels(factor(x)))/length(x) }
ucWords <- function(x) { length(grep("\\b[A-Z]+\\b", x)) }
sentences <- function(x) { length(grep("\\b\\.\\b", x)) }

df <- data.frame(
  tokens=unlist(lapply(corpus, length)),
  sentences=unlist(lapply(corpus, sentences)),
  ttr=unlist(lapply(corpus, ttr)),
  uc=unlist(lapply(corpus, ucWords))
)
low = df$uc < 100
print(low)

X11();
hist(df$tokens, xlab="Tokens", ylab="Frequency", nclass=20, col=c("red", "blue"), density=c(10, 20));
while(names(dev.cur()) !='null device') Sys.sleep(1);

X11();
boxplot(df$ttr[low], df$ttr[!low]);
while(names(dev.cur()) !='null device') Sys.sleep(1);

X11();
plot(df$tokens, type = "l",col = "red", xlab="Document", ylab="Number");
par(new=TRUE)
plot(df$sentences, type = "l",col = "black", axes=FALSE, xlab="", ylab="");
axis(side=4, at = pretty(range(df$sentences)))
mtext("z", side=4, line=3);
legend("topright", legend=c("Tokens", "Sentences"), pch=c(15,15), col=c("black","red"));

while(names(dev.cur()) !='null device') Sys.sleep(1);
