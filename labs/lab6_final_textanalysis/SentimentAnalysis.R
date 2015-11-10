# required pakacges
library(tm)

library(devtools)
install_github("mannau/tm.plugin.sentiment")
library(tm.plugin.sentiment)

example = c("I hate, HATE apples." , "I love oranges." , "I have a love hate love hate love relationship with plumbs")
exampleCorpus = Corpus(VectorSource(example))

meta(score(exampleCorpus))


studentFeedback <- read.csv("/home/dan/Desktop/GitRepo/COLL_100/labs/lab6_final_textanalysis/studentfeedback.csv")

View(studentFeedback)

allFeedbackCorpus = Corpus(VectorSource(studentFeedback$Feedback))
summary(meta(score(allFeedbackCorpus)))

femaleFeedback = subset(studentFeedback, Sex == "F")
femaleFeedbackCorpus = Corpus(VectorSource(femaleFeedback$Feedback))
summary(meta(score(femaleFeedbackCorpus)))

#dantestR2

#------------------------

library(twitteR)

api_key <- "8nPyy9wX50LsHbS955L5e1W8a"
api_secret <- "N8nmFLEv9uwuonLid70QF3F2F1BwgI7Qq7BMStCEQsLizX2pug"

access_token <- "4157847035-8RtwSilZ1UoEWjF0I7yd6TdyoBZjeLFQrnmzthT"
access_token_secret <- "G4ZJhIs0UfjPFc3fdshU4VjiY5dE3OnTmtGYUJ2LeM6zG"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

testTweetSearch = searchTwitter("William and Mary", n=100, lang="en")

tweetDF <- do.call("rbind", lapply(testTweetSearch, as.data.frame))

tweetCorpus = Corpus(VectorSource(tweetDF$text))
summary(meta(score(tweetCorpus)))

tweetDF$polarity <- meta(score(tweetCorpus))[,1]

lowPolarityTweets <- subset(tweetDF, polarity < -0.2)
neutralPolarityTweets <- subset(tweetDF, polarity < 0.2 & polarity > -0.2)
highPolarityTweets <- subset(tweetDF,  polarity > 0.2)



allPolarityTweets = rep("", 3)

allPolarityTweets[1] = highPolarityTweets
allPolarityTweets[2] = lowPolarityTweets
allPolarityTweets[3] = neutralPolarityTweets


library(tm)
library(wordcloud)

corpus = Corpus(VectorSource(allPolarityTweets))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(3, "Dark2"), scale = c(3,.5), random.order = FALSE)

#GeoTweetSearch
GeoTweetSearch = searchTwitter("Obama", n=1000, geocode='38.9047,-77.0164,100mi')
geoDF <- do.call("rbind", lapply(GeoTweetSearch, as.data.frame))

#Map it out:
library(ggmap)

geoDF_noNA <- geoDF[complete.cases(geoDF["latitude"]),]
geoDF_noNA$latitude <- as.numeric(geoDF_noNA$latitude)
geoDF_noNA$longitude <- as.numeric(geoDF_noNA$longitude)
qmplot(longitude, latitude, data = geoDF_noNA, colour = I('red'), source="google")

#------------------------------
library(igraph)
NetworkTweetSearch = searchTwitter("Hillary Clinton", n=1000, lang="en")
NT_DF <- do.call("rbind", lapply(NetworkTweetSearch, as.data.frame))

NT_DF_IDS <- NT_DF[c("replyToSN", "screenName")]
NT_DF_IDS <- NT_DF_IDS[complete.cases(NT_DF_IDS),]

Tweets_adj <- get.adjacency(graph.edgelist(as.matrix(NT_DF_IDS)))

MyGraph <- graph_from_adjacency_matrix(Tweets_adj)

plot(MyGraph,        		#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Twitter Network',	#specifies the title
     vertex.color = 'green',        #The color of the nodes
     vertex.label.dist=0.25,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=0.1,			#the font of the name labels
     vertex.label.cex=0.5,			#specifies the size of the font of the labels. can also be made to vary
     vertex.size = 0.1,
     vertex.shape = "circle",       #Try out some circles
     edge.arrow.size = 0,       #More reasonable size arrows
     edge.lty = 6, #Different edge types; 1-6 are options.
     edge.curved=0.5 #Make it look neat.  Different values make it more or less curvy.
)