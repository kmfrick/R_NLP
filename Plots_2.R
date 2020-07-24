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
wc <- function(x, p) { length(grep(p, x)) / length(x)}

df <- data.frame(
  tokens=unlist(lapply(corpus, length)),
  sentences=unlist(lapply(corpus, sentences)),
  ttr=unlist(lapply(corpus, ttr)),
  uc=unlist(lapply(corpus, ucWords)),
  articles=unlist(lapply(corpus, wc, p="\\b(an?|the)\\b")),
  us=unlist(lapply(corpus, wc, p="\\bUSA?\\b")),
  colors=unlist(lapply(corpus, wc, p="\\b(green|blue|red|yellow|white|black)\\b")),
  green=unlist(lapply(corpus, wc, p="\\b(green)\\b")),
  red=unlist(lapply(corpus, wc, p="\\b(red)\\b")),
  blue=unlist(lapply(corpus, wc, p="\\b(blue)\\b"))
)

#X11();
#barplot(df$tokens, xlab="Document", ylab="Tokens");
#while(names(dev.cur()) !='null device') Sys.sleep(1);


#pos <- df$green > 0 | df$red > 0 | df$blue > 0;
#X11();
#barplot(rbind(df$red[pos], df$green[pos], df$blue[pos]), xlab="Document", ylab="Tokens", col = c("red", "green", "blue"));
#while(names(dev.cur()) !='null device') Sys.sleep(1);

#X11();
#heatmap(as.matrix(dist(df, "manhattan")), Colv=NA, Rowv=NA);
#while(names(dev.cur()) !='null device') Sys.sleep(1);


#library(cowplot)

X11();
layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths=c(2, 0.5), heights=c(1, 1))
barplot(df$sentences, yaxs="i", ylim=c(0, 1500));
boxplot(df$sentences, yaxs="i", frame=F, ylim=c(0, 1500));
while(names(dev.cur()) !='null device') Sys.sleep(1);

