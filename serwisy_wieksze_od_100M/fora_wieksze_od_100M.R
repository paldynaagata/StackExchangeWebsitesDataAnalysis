library(stringi)
plik <- read.csv("Zeszyt1.csv", head = TRUE, sep = ";")
head(plik)
x<-as.data.frame(plik)
colnames(x) <- c("str", "data", "rozmiar")
head(x)
x2 <- x[x$rozmiar == stri_match_all_regex(x$rozmiar, "^[0-9]+\\.?[0-9]*M?$"),]
x2$nowakol <- stri_replace_all_regex(x2$rozmiar, "^(.*?)[M]+$", "$1")
head(x2)
x2$nowakol <- as.double(x2$nowakol)
x3 <- x2[x2$nowakol > 100,]
x4 <- x3[!is.na(x3$nowakol),]
x4[order(x4$nowakol, decreasing = TRUE),]
length(x4$nowakol)
