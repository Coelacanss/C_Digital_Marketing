### This script is to do text mining

library(tm)
library(SnowballC)
library(e1071)
library(caret)
library(ggplot2)

des.df = fans_twitter$description
all.reviews.raw = VCorpus(VectorSource(des.df))

all.reviews.raw = VCorpus(VectorSource(nonBrew.df))

#now clean up the data

#convert to all lowercase
# note start with all.reviews.raw but use just all.reviews henceforth
ptm = proc.time()
all.reviews = tm_map(all.reviews.raw, content_transformer(tolower))
# remove all numbers
all.reviews = tm_map(all.reviews, content_transformer(removeNumbers))
#remove all punctuation
all.reviews = tm_map(all.reviews, content_transformer(removePunctuation))
#remove all stopwords
all.reviews = tm_map(all.reviews, removeWords, stopwords("chinese"))
#now let us stem it
#library(SnowballC)
all.reviews = tm_map(all.reviews, stemDocument)
#remove whitespace
all.reviews = tm_map(all.reviews, content_transformer(stripWhitespace))
proc.time() - ptm
# lets take a look
writeLines(as.character(all.reviews[[5]]))
# save(all.reviews, file=file.choose())

# now make the document term matrix
dtm = DocumentTermMatrix(all.reviews)
dtm


dtm.99 = removeSparseTerms(dtm, 0.99)
dtm.99

dtm.df = as.data.frame(as.matrix(dtm.99))

#get full details on terms
term.freq =colSums(as.matrix(dtm.99))
term.freq[order(term.freq, decreasing=T)]  # in order of decreasing freq
# let us make a dataframe of it
term.df = data.frame(word=names(term.freq), freq = term.freq)
term.df$percentage = term.freq/9087

library(ggplot2)
ggplot(subset(term.df, freq>1), aes(word, freq)) + 
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle=45, hjust=1, size=20))


brew = grepl('brew', des.df, ignore.case = T)
index.brew = which(brew == F)
nonBrew.df = des.df[index.brew]


