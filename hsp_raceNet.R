# library 
library(igraph)
library(plyr)

# data
load("./raceNetVisData.Rdata")

# pdf
pdf("raceNet_043018.pdf",width=15,height=18.0)

##################### original ###############

# Sociogram
mat = cbind(soc[,c("driver1_id", "driver2_id", "streq", "collmean")])
str(mat)

# driver_name = read.csv(file = "driver_name.csv")

mat = graph.data.frame(mat, directed = F)

# calculate layout
# 120417 added: repulserad=vcount(mat)^2.5
layout = layout.fruchterman.reingold(mat,  weights = E(mat)$streq, repulserad=vcount(mat)^2.8)


# ############### 1st sociogram ################
streqplot = plot(mat, layout=layout, vertex.size=3, vertex.color = adjustcolor("blue", alpha.f = .2),
                 vertex.frame.color= adjustcolor("black", alpha.f = .2),
                 vertex.label=NA, edge.width = E(mat)$streq,
                 edge.color = adjustcolor("steelblue", alpha.f = .2))
# dev.off()

# ############## 2nd sociogram ################
# remove edges which do not have any crashing ties using collmean
mat = delete.edges(mat, which(E(mat)$collmean == 0))

# more revulsion? 
# plot.igraph(mat, layout =  layout.fruchterman.reingold)


# #identify isolated nodes
isolate.vs<-V(mat)[degree(mat) == 0] 
# isolate.vs

# remove isolated nodes
# mat <- delete.vertices(mat, isolate.vs)
summary(mat)
# isolate vertex size 0, otherwise 3
V(mat)[-isolate.vs]$size = 3
V(mat)[isolate.vs]$size = 0

# isolate vertex color opacity as 0, otherwise 1
V(mat)[-isolate.vs]$color = adjustcolor("red", alpha.f = 0.9)
V(mat)[isolate.vs]$color = adjustcolor("red", alpha.f = 0)

V(mat)[-isolate.vs]$frame.color = adjustcolor("black", alpha.f = 0.9)
V(mat)[isolate.vs]$frame.color = adjustcolor("black", alpha.f = 0)



##### assign vertex name 
name = V(mat)$name
name = as.data.frame(name)

## using merge
# merged_name = merge(name, driver_name, by.x="name", by.y = "driver_id", all.x=T)
# V(mat)$label = as.character(merged_name$driver_name)

## using join method
# colnames(name) = "driver_id"
# joined_name = join(name, driver_name, by="driver_id")

V(mat)$label = as.character(joined_name$driver_name)
## ?????? ?????? ?????? ????????? ?????? (3->6)
V(mat)[!is.na(V(mat)$label)]$size = 6

# draw collmeanplot  with add option T
# plot(mat, layout=layout, vertex.size=V(mat)$size, vertex.color = V(mat)$color, vertex.frame.color= V(mat)$frame.color, 
#      vertex.label=NA, edge.width = E(mat)$streq,
#      edge.color = adjustcolor("red", alpha.f = .9), add=T)


## vertex.label.font = 2: bold face, vertex.label.font = 1: plain text, vertex.label.font = 3: ltalyic

# with label
# second_plot = plot(mat, layout=layout, vertex.size=V(mat)$size, vertex.color = V(mat)$color, vertex.frame.color= V(mat)$frame.color, vertex.label=V(mat)$label, vertex.label.font = 2, edge.width = E(mat)$streq, edge.color = adjustcolor("red", alpha.f = .9), add=T)

# without label
second_plot = plot(mat, layout=layout, vertex.size=V(mat)$size, vertex.color = V(mat)$color, 
                   vertex.frame.color= V(mat)$frame.color, vertex.label=V(mat)$label, edge.width = E(mat)$streq, 
                   edge.color = adjustcolor("red", alpha.f = .9), add=T)


dev.off()


##################### split ###
# streq_layer
pdf("streq_net_120617.pdf",width=15,height=18.0) 

library(igraph)
mat = cbind(soc[,c("driver1_id", "driver2_id", "streq", "collmean")])
str(mat)

driver_name = read.csv(file = "driver_name.csv")

mat = graph.data.frame(mat, directed = F)

# calculate layout
# 120417 added: repulserad=vcount(mat)^2.5
# layout = layout.fruchterman.reingold(mat,  weights = E(mat)$streq, repulserad=vcount(mat)^2.8)


# ############### 1st sociogram ################
streqplot = plot(mat, layout=layout, vertex.size=3, vertex.color = adjustcolor("blue", alpha.f = .2),
                 vertex.frame.color= adjustcolor("black", alpha.f = .2),
                 vertex.label=NA, edge.width = E(mat)$streq,
                 edge.color = adjustcolor("steelblue", alpha.f = .2))

dev.off()


# collision_layer
pdf("coll_net_120617.pdf",width=15,height=18.0) 

# ############## 2nd sociogram ################
# remove edges which do not have any crashing ties using collmean
mat = delete.edges(mat, which(E(mat)$collmean == 0))

# #identify isolated nodes
isolate.vs<-V(mat)[degree(mat) == 0] 
# isolate.vs

# remove isolated nodes
# mat <- delete.vertices(mat, isolate.vs)
summary(mat)
# isolate vertex size 0, otherwise 3
V(mat)[-isolate.vs]$size = 3
V(mat)[isolate.vs]$size = 0

# isolate vertex color opacity as 0, otherwise 1
V(mat)[-isolate.vs]$color = adjustcolor("red", alpha.f = 0.9)
V(mat)[isolate.vs]$color = adjustcolor("red", alpha.f = 0)

V(mat)[-isolate.vs]$frame.color = adjustcolor("black", alpha.f = 0.9)
V(mat)[isolate.vs]$frame.color = adjustcolor("black", alpha.f = 0)


##### assign vertex name 
name = V(mat)$name
name = as.data.frame(name)

## using join method
library(plyr)
colnames(name) = "driver_id"
joined_name = join(name, driver_name, by="driver_id")
V(mat)$label = as.character(joined_name$driver_name)
## ?????? ?????? ?????? ????????? ?????? (3->6)
V(mat)[!is.na(V(mat)$label)]$size = 6

collmean_plot = plot(mat, layout=layout, vertex.size=V(mat)$size, vertex.color = V(mat)$color, 
                     vertex.frame.color= V(mat)$frame.color, 
                     vertex.label=NA, edge.width = E(mat)$streq,
                     edge.color = adjustcolor("red", alpha.f = .7))

dev.off()

