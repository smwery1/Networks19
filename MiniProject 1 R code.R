
#Subset for the events that take place in Syria, Turkey, and Iraq

library(dplyr)

ACLED <- Syria.ACLED.Networks.Data

#Aggregate data by actors, keeping a sum of events

eventAgg = ACLED %>% group_by(actor1, actor2) %>% summarize(count=n())
head(eventAgg)


eventAgg$id <- apply(cbind(eventAgg$actor1, 
                           eventAgg$actor2), 1, 
                           function(x) paste(sort(x), 
                           collapse = ' '))




eventAgg_agg <- eventAgg %>%
                         group_by(id) %>%
                         summarize(sum = sum(count))


#Merge the data back together by ID to give final dyad data

eventAgg_final <- merge(eventAgg, eventAgg_agg, by = 'id', all = F)

#Due to large numbers of events and actors, eliminate any dyads that interact less than 50 times

eventAgg <- eventAgg_final[eventAgg_final$sum > 50,]

#Remove any duplicated data

eventAgg <- eventAgg[!duplicated(eventAgg$id),]


#Create a list of unique actors

actorlist <- c(eventAgg$actor1,eventAgg$actor2)

actorlist <- unique(actorlist)


#Create an empty matrix to populate with event counts

eventmatrix <- matrix(0, 
                      nrow = length(actorlist), 
                      ncol = length(actorlist))

#ascribe names to the rows and columns

rownames(eventmatrix) <- actorlist

colnames(eventmatrix) <- actorlist


#Populate the adjacency matrix 

for(i in 1:nrow(eventAgg)){
  r = as.character(eventAgg$actor1[i])
  c = as.character(eventAgg$actor2[i])
  eventmatrix[r,c] <- eventAgg$sum[i]
  eventmatrix[c,r] <- eventAgg$sum[i]
}


#Check for degree measure of centrality 

centrality <- apply(eventmatrix, 1, sum, na.rm=TRUE)

#create a matrix with both the name of the groupa nd the degree (not necessary but for fun)
centralmatrix <- as.matrix(rbind(colnames(eventmatrix), degree))

#Find the group that has the most interactions and the number of interactions
which.max(apply(eventmatrix, 1, sum, na.rm=TRUE))

max(centrality)

#Most events occured with the syrian military



# Graph Network

library(igraph)
g = graph_from_adjacency_matrix(eventmatrix, 
                                mode='undirected', 
                                weighted=T,
                                diag=FALSE
)



# names for large actors only (eliminates graph clutter)
V(g)$label <- ifelse( centrality>=100, V(g)$name, NA )

#Set a color parameter to identify "major players" with more than 1000 interactions
V(g)$color <- ifelse( centrality>=1000, 'yellow', 'grey' )

#Create a size attribute to make nodes larger when interactions are larger
V(g)$size <- log(degree)


#Change the weight of the lines by total interactions
E(g)$weight <- log(E(g)$weight)/1.5


# final plot
par(mar=c(4,4,4,4))
plot(g,
     layout=layout_with_gem,
     vertex.label=V(g)$label, 
     vertex.size=V(g)$size,
     vertex.color = V(g)$color, # change color of nodes
     vertex.label.color = "black", # make labels a readable color
     vertex.label.cex = .5, #cut down label size
     edge.curved =.25, # make edges curved
     edge.color ="grey", 
     edge.width = E(g)$weight,
     vertex.size = 1,
     vertex.size2 = 1,
     asp = 0,
     margin = -0.1
     
)

#Find the Max Degree

which.max(degree(g))

max(degree(g))

