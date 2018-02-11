rm(list = ls())
suppressWarnings(library(dplyr))
suppressWarnings(library(NLP))
suppressWarnings(library(tm))
suppressWarnings(library(wordcloud))
suppressWarnings(library(RColorBrewer))
filepath <- "D:/R/JHU/DSS Capstone Project/Coursera-SwiftKey/final/en_US/"

# BLOGS dataset
blogs <- readLines(paste(filepath, "en_US.blogs.txt", sep = ""))
blogs <- blogs[1:10000]
blogs_corpus <- VCorpus (VectorSource (blogs))
rm(blogs) # remove variable no longer needed

# NEWS dataset
# Warning message:
# In readLines(news) : incomplete final line found on 'en_US.news.txt'

# TWITTER dataset
# Warning messages:
# 1: In readLines(twitter) : line 167155 appears to contain an embedded nul
# and various other lines also

blogs <- "en_US.blogs.txt"
news <- "en_US.news.txt"
twitter <- "en_US.twitter.txt"
datalist <- list(blogs, news, twitter)
info <- function(file_x) {
        name <- strsplit(file_x, '[.,]')[[1]][2]
        file_x <- paste(filepath, file_x, sep = "")
	finfo <- file.info(file_x)[1]/(1024*1024)
	fdata <- readLines(file_x)
	nchars <- lapply(fdata, nchar)
	maxchars <- which.max(nchars)
	nwords <- sum(sapply(strsplit(fdata, "\\s+"), length))
	return (c(name, as.integer(finfo), length(fdata), maxchars, nwords))
}
df <- as.data.frame(lapply(datalist, info))
rownames(df) <- c("FILE", "FILE_SIZE", "LENGHT", "LONGEST_LINE", "TOTAL_WORDS")
colnames(df) <- NULL
print(df)

rm(df)
corpus <- blogs_corpus %>% tm_map(removePunctuation) %>% tm_map(removeNumbers) %>% tm_map(stripWhitespace) %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, stopwords("english")) %>% tm_map(PlainTextDocument)
rm(blogs_corpus)
Unigram <- function(x) {
	unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = F)
}
Bigram <- function(x) {
	unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = F)
}
Trigram <- function(x) {
	unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = F)
}
blogs_1 <- TermDocumentMatrix(corpus, control = list(tokenize = Unigram))
blogs_2 <- TermDocumentMatrix(corpus, control = list(tokenize = Bigram))
blogs_3 <- TermDocumentMatrix(corpus, control = list(tokenize = Trigram))


corpusToDF <- function(theCorpus) {
	m <- as.matrix(theCorpus)
	v <- sort(rowSums(m), decreasing = TRUE)
	return (data.frame(word = names(v), freq = v))
}

# ggplot(data[1:30,], aes(reorder(word, -freq), freq)) + labs(x = label, y = "Frequency")
# + theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1))
# + geom_bar(stat = "identity", fill = I("grey50"))

d1 <- corpusToDF(blogs_1)
barplot(d1[1:10, ]$freq, las = 2, names.arg = d1[1:10, ]$word, col ="lightblue", main = "Most frequent words", ylab = "Word frequencies")
wordcloud(words = d1$word, freq = d1$freq, min.freq = 40, max.words = 200, random.order = TRUE, rot.per = 0.35,  colors = brewer.pal(8, "Dark2"))
d2 <- corpusToDF(blogs_2)
d3 <- corpusToDF(blogs_3)