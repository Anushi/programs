library(twitteR)
library(plyr)
library(stringr)
library(RCurl)
# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
 
library(twitteR)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
apiKey <- "HXjYD1fFjb1izKBw0i3hj7QEU"
apiSecret <- "5jLMdOvfrzVfXGEPUvBe1B4KmbjRFD7dfvVkQ2IIFV1QErwTiQ"
 
twitCred <- OAuthFactory$new(
consumerKey = apiKey,
consumerSecret = apiSecret,
requestURL = reqURL,
accessURL = accessURL,
authURL = authURL
)
 
twitCred$handshake(
cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")
)

registerTwitterOAuth(twitCred)

delta.tweets = searchTwitter('@delta', n=1500)
tweet = delta.tweets[[1]]
delta.text = laply(delta.tweets, function(t) t$getText() )
delta.text = laply(delta.tweets, function(t) t$getText() )

hu.liu.pos = scan('D:\\Anushi\\R_programs\\positive-words.txt',what='character', comment.char=';')

hu.liu.neg = scan('D:\\Anushi\\R_programs\\negative-words.txt',what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
require(plyr)
require(stringr)

# we got a vector of sentences. plyr will handle a list
# or a vector as an "l" for us
# we want a simple array of scores back, so we use
# "l" + "a" + "ply" = "laply":
scores = laply(sentences, function(sentence, pos.words, neg.words) {

# clean up sentences with R's regex-driven global substitute, gsub():
sentence = gsub('[[:punct:]]', '', sentence)
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
# and convert to lower case:
sentence = tolower(sentence)

# split into words. str_split is in the stringr package
word.list = str_split(sentence, '\\s+')
# sometimes a list() is one level of hierarchy too much
words = unlist(word.list)

# compare our words to the dictionaries of positive & negative terms
pos.matches = match(words, pos.words)
neg.matches = match(words, neg.words)

# match() returns the position of the matched term or NA
# we just want a TRUE/FALSE:
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)

# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
score = sum(pos.matches) - sum(neg.matches)

return(score)
}, pos.words, neg.words, .progress=.progress )

scores.df = data.frame(score=scores, text=sentences)
return(scores.df)
}

delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')

delta.scores$airline = 'Delta'
delta.scores$code = 'DL'

hist(delta.scores$score)
