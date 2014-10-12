library(twitteR)
library(tm)

rdmTweets<-userTimeline("rDataMining",n=200)
(nDocs<-length(rdmTweets))

rdmTweets[11:15]

df<-do.call("rbind",lapply(rdmTweets,as.data.frame))

dim(df)

myCorpus<-Corpus(VectorSource(df$text))

myCorpus<-tm_map(myCorpus,tolower)

myCorpus<-tm_map(myCorpus,removePunctuation)

myCorpus<-tm_map(myCorpus,removeNumbers)

removeURL<-function(x)gsub("http[[:alnum:]]*","",x)

myCorpus<-tm_map(myCorpus,removeURL)

myStopwords<-c(stopwords('english'),"available","via")

myStopwords<setdiff(myStopwords,c("r","big"))


myCorpus<-tm_map(myCorpus,removeWords,myStopwords)

myCorpusCopy<-myCorpus

myCorpus<-tm_map(myCorpus,stemDocument)

for(i in 11:15){
 cat(paste("[[",i,"]] ",sep=""))
 writeLines(strwrap(myCorpus[i],width=73))
}

for(i in 11:15){
 cat(paste("[[",i,"]] ",sep=""))
 writeLines(strwrap(myCorpusCopy[i],width=73))
}

myCorpus<-tm_map(myCorpus,stemCompletion,dictionary=myCorpusCopy)

inspect(myCorpus[11:15])

myTdm<-TermDocumentMatrix(myCorpus,control=list(wordLengths=c(1,Inf)))
myTdm

