setwd("~/Desktop/GitRepo/COLL_100/labs/lab4_socialnets")
ClassData = read.csv("SocialNetwork_R.csv")

#install.packages("igraph")
library(igraph) 

rownames(ClassData) = ClassData$Student.Name

ClassData = ClassData[-1]

ClassData_Matrix = as.matrix(ClassData)

MyGraph <- graph_from_adjacency_matrix(ClassData_Matrix)
plot.igraph(MyGraph)

plot.igraph(MyGraph, mark.shape=1, layout=layout_on_sphere)

#par(mai=c(0,0,1,0))     		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(MyGraph,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Class Social Network',	#specifies the title
     vertex.color = 'green',        #The color of the nodes
     vertex.label.dist=0.25,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=3,			#the font of the name labels
     vertex.label.cex=0.7,			#specifies the size of the font of the labels. can also be made to vary
     vertex.shape = "square",       #Try out some squares
     edge.arrow.size = 0.5,       #More reasonable size arrows
     edge.lty = 6, #Different edge types; 1-6 are options.
     edge.curved=0.5 #Make it look neat.  Different values make it more or less curvy.
     )


evcent(MyGraph)$vector

reciprocity(MyGraph)

graph.density(MyGraph)

average.path.length(MyGraph)

transitivity(MyGraph)

theTweets <- read.csv("twitter_data.csv")

library(wordcloud)
library(RColorBrewer)
library(tm)

tweetCorpus = Corpus(VectorSource(theTweets$Tweet))

tweetTerms = TermDocumentMatrix(tweetCorpus)

tweetMatrix = as.matrix(tweetTerms)
word_freqs = sort(rowSums(tweetMatrix), decreasing = TRUE) 

dm = data.frame(word = names(word_freqs), freq = word_freqs)
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#---------
tweetTerms = TermDocumentMatrix(tweetCorpus, control = list(stopwords = c("new","time","warner",stopwords("english"))))

tweetMatrix = as.matrix(tweetTerms)
word_freqs = sort(rowSums(tweetMatrix), decreasing = TRUE) 

dm = data.frame(word = names(word_freqs), freq = word_freqs)
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#---------------------

danTweets <- subset(theTweets, User_ID == "Tyler_F")

tweetCorpus = Corpus(VectorSource(danTweets$Tweet))

tweetTerms = TermDocumentMatrix(tweetCorpus, control = list(stopwords = c("new","time","warner",stopwords("english"))))

tweetMatrix = as.matrix(tweetTerms)
word_freqs = sort(rowSums(tweetMatrix), decreasing = TRUE) 

dm = data.frame(word = names(word_freqs), freq = word_freqs)
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#----------------------
tweetCorpus = Corpus(VectorSource(theTweets$Tweet))

tweetTerms = TermDocumentMatrix(tweetCorpus)

findAssocs(tweetTerms, "@dan_r", corlimit=0.2)

findAssocs(tweetTerms, "laptop", corlimit=0.2)

danTweets <- subset(theTweets, User_ID == "Dan_R" | User_ID == "Tyler_F")

#----------------------
theTweetsCopy <- read.csv("twitter_data.csv")

Tweet_Mentions <- theTweetsCopy[grep("^@",theTweetsCopy$Tweet),]

library(stringr)
Tweet_Mentions["Tweet"] <- lapply(Tweet_Mentions["Tweet"],FUN = function(x) word(x,1))

Tweet_Mentions["Tweet"] <- lapply(Tweet_Mentions["Tweet"],FUN = function(x) gsub("@","",x))

Tweet_Mentions <- Tweet_Mentions[c("User_ID", "Tweet")]

findBlanks <- which(Tweet_Mentions$Tweet == "")

Tweet_Mentions = Tweet_Mentions[-findBlanks,]

Tweets_adj <- get.adjacency(graph.edgelist(as.matrix(Tweet_Mentions), directed=TRUE))

MyGraph <- graph_from_adjacency_matrix(Tweets_adj)

plot(MyGraph,    			#the graph to be plotted
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


linked_actors <- as.data.frame(evcent(MyGraph)$vector)

names(linked_actors)[1] <- "centrality"

linked_actors_sub <- subset(linked_actors, centrality >= mean(centrality))

Important_Tweet_Mentions <- subset(Tweet_Mentions, User_ID %in% rownames(linked_actors_sub)) 

Tweets_adj <- get.adjacency(graph.edgelist(as.matrix(Important_Tweet_Mentions), directed=TRUE))

MyGraph <- graph_from_adjacency_matrix(Tweets_adj)


central_actors <- as.data.frame(evcent(MyGraph)$vector)
names(central_actors)[1] <- "centrality"

central_actors_sub <- subset(central_actors, centrality >= mean(centrality))
Important_Tweet_Mentions <- subset(Tweet_Mentions, User_ID %in% rownames(central_actors_sub)) 

Tweets_adj <- get.adjacency(graph.edgelist(as.matrix(Important_Tweet_Mentions), directed=TRUE))

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

