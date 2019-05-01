###Networks Analysis
#Spring 2019
library(igraph)
library(dplyr)


#Load in Data

exdata <- read.csv('C:/Users/swery/OneDrive/Desktop/Networks Project/External Support .csv')
mgep <- read.csv("C:/Users/swery/OneDrive/Desktop/Networks Project/MGEP.csv")

#Importing a key that was created in STATA for fuzzy matching
key <- read.csv(file = 'C:/Users/swery/OneDrive/Desktop/key.csv')



#Subset datasets for imporatant varibales
exdata <- exdata[c(1:3, 6,13)]

#Remove dyads where there is no sender
exdata <- exdata[exdata$Sender != '',]

#create a dyad variable to collapse the time-varying data
exdata$dyad <- paste(exdata$Receiver, exdata$Sender, sep = '%')

#Collapse and summarize by external sponsorship
exdata <- exdata %>%
                        group_by(dyad) %>%
                        summarize(external = sum(external_exists))

#Recreate the previous variables of sender and receiver
exdata$Receiver <- do.call(rbind, strsplit(exdata$dyad, split = '%', fixed = TRUE))[,1]
exdata$Sender <- do.call(rbind, strsplit(exdata$dyad, split = '%', fixed = TRUE))[,2]


#Collapse the rebel participation data, summarize by participation and winning
mgep <- mgep %>%
            group_by(name) %>%
            summarize(part = sum(part),
                     winner = sum(winner),
              )



#Establish which matches pass spot-checking
key$keep[which(is.na(key$keep) == TRUE)] <- 0

sum(key$keep)

keep <- key[which(key$keep == 1),]

nrow(keep)


#merge key with first dataset
mgep <- merge(mgep, keep, by = 'name', all = FALSE)


key$rec <- as.character(key$rec)


#merge key with second dataset
names(keep)[2] <- 'rec'
names(exdata)[3] <- 'rec'


exdata <- merge(exdata, keep, by = 'rec' , all = FALSE)


#Merge two datasets together
fulldata <- merge(mgep, exdata, by = 'name', all = FALSE )


fulldata$name <- as.character(fulldata$name)
basic <- fulldata


names(basic)[1] <- 'Receiver'


basic$Receiver <- as.character(basic$Receiver)
basic$Sender <- as.character(basic$Sender)

#Subset for the variables we actually care about
basicnew <- as.data.frame(cbind(basic$Receiver, basic$Sender, basic$part,
               basic$winner))

names(basicnew) <- c('receiver','sender', 'part','winner')


basicnew$receiver <- as.character(basicnew$receiver)
basicnew$sender <- as.character(basicnew$sender)

#Remove any self sending dyads
basicnew <- basicnew[which(basicnew$sender != basicnew$receiver),]

#basicnew$part <- as.numeric(basicnew$part)

#Create the adjacency matrix
actors <- unique(c(basicnew$receiver, basicnew$sender))

n = length(actors)

mat <- matrix(0, n, n, dimnames = list(actors, actors))

for(i in 1:nrow(basicnew)){
  #print(i)
  rowActor = as.character(basicnew$receiver[i])
  colActor = as.character(basicnew$sender[i])
  mat[rowActor, colActor] <- 1
  mat[colActor, rowActor] <- 1
}

library(igraph)

#Create the graph 

g <- graph_from_adjacency_matrix(mat, mode = 'undirected', 
                                      weighted = TRUE, 
                                      diag = FALSE)



#Make names attribute
names <- V(g)$name
names <- as.character(names)


V(g)$part <- NA

#Create an attribute whether an invidiual node is a sender, receiver, or both
V(g)$sendrec <- rep(0,length(V(g)))


for(i in actors){
  if(i %in% basicnew$sender){
    V(g)$sendrec[which(V(g)$name == i)] <- 2
  }
  if( i %in% basicnew$sender &
      i %in% basicnew$receiver){
    V(g)$sendrec[which(V(g)$name == i)] <- 1
  }
}

#save <- basicnew

#Adjust for some weird coding
basicnew$part <- as.integer(basicnew$part)
for(i in 1:nrow(basicnew)){
  basicnew$part[i] <- basicnew$part[i]-1
}

#Create dummy for post conflict participation
basicnew$part <- ifelse(basicnew$part >= 0, 1, 0)


#Identify those that participated, those that didn't
V(g)$part <- rep(0, length(V(g)))

for(i in 1:length(V(g))){
  x <- V(g)$name[i]
  if(x %in% basicnew$receiver){
    data <- as.data.frame(basicnew[which(basicnew$receiver == x),])
    V(g)$part[i] <- mean(na.omit(data$part))
    if(is.na(V(g)$part[i])){
      print(i)
      print('shit went wrong')
      break
    }
    #print(mean(na.omit(data$part)))
    if(V(g)$part[i] > 0){
    V(g)$part[i] <- 1
  }  
  }
}

#Identify those that won
basicnew$winner <- as.integer(basicnew$winner)

basicnew$winner <- as.integer(basicnew$winner)
for(i in 1:nrow(basicnew)){
  basicnew$winner[i] <- basicnew$winner[i]-1
}



V(g)$winner <- rep(0, length(V(g)))

for(i in 1:length(V(g))){
  x <- V(g)$name[i]
  if(x %in% basicnew$receiver){
    data <- as.data.frame(basicnew[which(basicnew$receiver == x),])
    if(i < 10){
      print(head(data))
    }
    V(g)$winner[i] <- mean(na.omit(data$winner))
    if(is.na(V(g)$winner[i])){
      print(i)
      print('shit went wrong')
      break
    }
    #print(mean(na.omit(data$winner)))
    if(V(g)$winner[i] > 0){
    V(g)$winner[i] <- 1
  }
  }
}

table(V(g)$winner)


#Create a color varaible that identifies senders and receivers (eventually thrown out of graph)
V(g)$color <- rep(NA, length(V(g)))

for(i in 1:length(V(g)$sendrec)){
  if(V(g)$sendrec[i] < 2){
    V(g)$color[i] <- 1
  }
  else{
    V(g)$color[i] == .75
  }
  if(V(g)$sendrec[i] == 2){
    V(g)$color[i] <- .75
  }
}


head(V(g)$color)


#Create degree data for descriptives

V(g)$degree <- degree(g, v = V(g), mode = 'all', loops = TRUE, normalized = FALSE)

degreedata <- as.data.frame(cbind(V(g)$name, V(g)$degree))

degreedata <- cbind(degreedata, V(g)$color)

length(V(g)$part)

ncol(degreedata)

head(degreedata)
names(degreedata) <- c('name', 'degree', 'direction')


degreedata <- degreedata[order(degreedata$degree, decreasing = TRUE),]

head(degreedata)



#Establish a direction to separate senders from receivers in descriptives tables
for(i in 1:nrow(degreedata)){
  if(is.na(degreedata$direction[i])){
    degreedata$direction[i] <- 0
  }
  if(degreedata$direction[i] == 1){
    degreedata$direction[i] <- 1
  }
  else{
    degreedata$direction[i] <- 0
  }
}

#table(degreedata$direction)

degreedatareceive <- degreedata[which(degreedata$direction == 1),]

degreedatasend <- degreedata[which(degreedata$direction != 1),]

nrow(degreedatareceive) + nrow(degreedatasend) == nrow(degreedata)


#Create a value in our participation attribute for those that won

for(i in 1:length(V(g))){
  if(V(g)$winner[i] == 1){
    V(g)$part[i] <- 2
  }
  if(V(g)$sendrec[i] == 2){
    V(g)$part[i] <- 4
  }
}

#Use the 4 possible outcomes to color the nodes

V(g)$part = gsub(0, 'cyan',V(g)$part) #groups that didn't participate
V(g)$part = gsub(1, 'orange', V(g)$part) #groups that did participate
V(g)$part = gsub(2, 'green', V(g)$part) #groups that particpated and won
V(g)$part = gsub(4, 'white', V(g)$part) #state sponsors



#***Plot 1***

set.seed(5678)


V(g)$label <- ifelse((V(g)$part != 'cyan' & V(g)$part != 'white') | (V(g)$degree > 6 & V(g)$sendrec == 2) , V(g)$name, NA)



plot(g,
     vertex.label.cex = .8,
     #vertex.frame.color= V(g)$part,
     vertex.size = 60,
     vertex.color = V(g)$part,
     vertex.label = V(g)$label,
     vertex.label.color = 'black',
     rescale = FALSE,
     ylim = c(-5,20),
     xlim = c(0, 20),
     asp = 1)






#***PLOT 2***






V(g)$label <- ifelse((V(g)$degree > 4 & V(g)$sendrec == 0) , V(g)$name, NA)

plot(g,
     vertex.label.cex = .8,
     #vertex.frame.color= V(g)$part,
     vertex.size = 60,
     vertex.color = V(g)$sendrec,
     #edge.width = 4,
     vertex.label = V(g)$label,
     vertex.label.color = 'black',
     rescale = FALSE,
     ylim = c(-5,20),
     xlim = c(0, 20),
     asp = 1)



