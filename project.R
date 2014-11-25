install.packages("tm")
install.packages("SnowballC")
install.packages("slam")
install.packages("wordcloud")
require("wordcloud")
require("slam")
require("SnowballC")
require("tm")
require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
library("reshape2")
require("data.table")

getwd()
setwd("C:\\Workspace")

file <- xmlTreeParse("sms.xml" , useInternal = T)
root <-xmlRoot(file)
root
class(file)
xmlName(root)
xmlAttrs(root)
xmlSize(root)

s1 <- xpathSApply(root , "//sms[@protocol='0']")
cat("The number of messages received is:",length(s1))


s2 <- xpathSApply(root , "//sms[@contact_name='Mom']")
cat("The number of messages from contact name Mom is:",length(s2))

s3 <- xpathSApply(root , "//sms[@contact_name]/@contact_name")
s3

m <- unique(s3)
m

m2 <- count(s3)
m2

barplot(m2$freq,names.arg=m2$x, main="Number of messages of each contact", 
        xlab="Contacts", space=c(0,2), ylim=c(0,450))

s4 <- xpathSApply(root , "//sms[@body]/@body")
s4
class(s4)
mode(s4)




doc <- VCorpus(VectorSource(s4))
inspect(doc)

doc <- tm_map(doc, tolower)
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, stemDocument)
doc <- tm_map(doc, stripWhitespace)
doc <- Corpus(VectorSource(doc))
tdm <- TermDocumentMatrix(doc)
head(inspect(tdm))
tdm[1,1]

dtm <- DocumentTermMatrix(doc)

tdm$i
tdm$j
tdm$v
tdm$dimnames$Terms
tdm$dimnames
str(tdm)
tem <- unique(tdm$dimnames$Terms)
tem

findFreqTerms(tdm, 10)

findAssocs(tdm, "call", 0.5)


nm <- as.matrix(tdm, stringsAsFactors=F)
nm

dt <- rowSums(nm)
dt <- as.numeric(dt)
class(dt)
str(dt)
length(dt)
head(dt)


wordcloud(tem,dt)


r <- rownames(nm)
head(r)




l <- readline("Enter the message:")
l <- tolower(l)
l <- removePunctuation(l)
l <- removeNumbers(l)
l <- removeWords(l, stopwords("english"))
l <- stemDocument(l)
l <- stripWhitespace(l)
l

for(i in 1:1885){
  if(l == r[i]) {
    
    il <- nm[i,]
    li <- which(il == 1 )
    
  }
  
}
li

for(j in 1:length(li) ){
  ji <- li[j]
  cat(s3[ji])
  cat("\t")
}



rowTotals <-  row_sums(tdm)
tdm2 <- tdm[which(rowTotals > 2),]
tdm2
str(tdm2)
tdm2_matrix <- as.matrix(tdm2, stringsAsFactors=F)
str(tdm2_matrix)
tdm2_matrix <- sort(colSums(tdm2_matrix),decreasing=TRUE)
head(tdm2_matrix)
name <- names(tdm2_matrix)
d <- data.frame(word=words, freq=tdm2_matrix)

commonality.cloud(tdm2_matrix,comonality.measure=min,max.words=40,random.order=FALSE)
class(tdm2_matrix[[1]])
dtm2_matrix$Docs
un <- unique(dtm2_matrix$Terms)
cn <- count(dtm2_matrix)
name1 <- names(cn)
name1



name <- names(nm)
str(name)
d <- data.frame(words=as.character(name),freq=as.numeric(dt))
d[[1]] <- as.character(d[[1]])
commonality.cloud(nm,max.words=40,random.order=FALSE)
traceback()
help


inspect(dtm, list(dictionary=d))
