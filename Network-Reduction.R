########################################
# This code reads a graphml file containing a directed document citation network
# exported from Sci2 version 0.5.2 (using database plugin)
# A csv file has keywords associated with the nodes of the network.
# The keywords are mapped onto respective nodes and added as a node attribute.
# The network is scaled down based on certain node attributes.
# Resulting network is then exported as a graphml file for visualization
########################################
# This is part of data visualization project for IVMOOC 2015
########################################

install.packages("igraph")
library("igraph")

## read graphml file saved from Sci2
graph<-read.graph("Document Citation Network (Core Only).graphml",format="graphml")
vcount(graph)   ## 5,382 nodes
ecount(graph)   ## 76,892 edges

## read keywords file saved from Sci2 and aggregate based on wos
keywords<-read.csv("keywords with wos id.csv")
names(keywords)<-c("keyword","wos")
keywords<-aggregate(keyword~wos, paste, collapse=",",data=keywords)

## create new logical variables for keywords of interest
## this step is optional if only interested in keyword count
keywords$mathematics <- grepl('MATHEMATICS',keywords$keyword)  # 227 true
keywords$numerical_estimation <- grepl('NUMERICAL ESTIMATION',keywords$keyword) # 123 true
keywords$enumeration <- grepl('ENUMERATION',keywords$keyword)  # 76 true
keywords$quantity <- grepl('QUANTITY',keywords$keyword)  # 160 true
keywords$numerosity <- grepl('NUMEROSITY',keywords$keyword)  # 239 true
keywords$magnitude <- grepl('MAGNITUDE',keywords$keyword)  # 564 true
keywords$number_sense <- grepl('NUMBER SENSE',keywords$keyword)  # 73 true
keywords$number_line <- grepl('NUMBER LINE',keywords$keyword)  # 142 true
keywords$calculation <- grepl('CALCULATION',keywords$keyword)  # 180 true
keywords$addition <- grepl('ADDITION',keywords$keyword)  # 181 true
keywords$subtraction <- grepl('SUBTRACTION',keywords$keyword)  # 142 true
keywords$multiplication <- grepl('MULTIPLICATION',keywords$keyword)  # 168 true
keywords$number_representation <- grepl('NUMBER REPRESENTATION',keywords$keyword)  # 77 true
keywords$numerical_abilities <- grepl('NUMERICAL ABILITIES',keywords$keyword)  # 69 true
keywords$intraparietal_sulcus <- grepl('INTRAPARIETAL SULCUS',keywords$keyword)  # 145 true
keywords$arithmetic <- grepl('ARITHMETIC',keywords$keyword)  # 181 true
keywords$nonsymbolic <- grepl('NONSYMBOLIC',keywords$keyword)  # 25 true
keywords$symbolic <- grepl('SYMBOLIC',keywords$keyword)  # 48 true
keywords$counting <- grepl('COUNTING',keywords$keyword)  # 76 true
keywords$cardinality <- grepl('CARDINALITY',keywords$keyword)  # 14 true

## map keywords to wos identifiers in the graph
wos<-as.data.frame(V(graph)$isi_unique_article_identifier)
names(wos)<-"wos"
mapped<-merge(x=wos,y=keywords, by="wos", all.x=TRUE)

## convert all NA values to 0
mapped[is.na(mapped)]<-0

## add a variable to show counts of keywords of interest
mapped$keyword_count <- rowSums(mapped[,c(3:22)])
table(mapped$keyword_count)  ## 3449 records have no keywords of interest

## add the additional attributes to the graph
V(graph)$keywords <- mapped$keyword
V(graph)$keyword_count <- mapped$keyword_count

## to see all attributes of a node
# list.vertex.attributes(graph)

## begin scaling-down the graph
graph <- delete.vertices(graph,V(graph)[V(graph)$language!="English"])  ## 5260 nodes now

fivenum(authority.score(graph)$vector)  ## .. 0.06 0.14 1.00
length(V(graph)[authority.score(graph)$vector<0.5&
                  V(graph)$publication_year<2012&
                  V(graph)$times_cited<500&
                  V(graph)$keyword_count==0])   ## 2382

graph <- delete.vertices(graph,V(graph)[authority.score(graph)$vector<0.5&
                                          V(graph)$publication_year<2012&
                                          V(graph)$times_cited<500&
                                          V(graph)$keyword_count==0])  ## 2878 nodes now

fivenum(page.rank(graph)$vector)  ## .. 0.0004 0.004
length(V(graph)[page.rank(graph)$vector<0.003&
                  V(graph)$times_cited<500&
                  V(graph)$keyword_count==0]) # 915
graph <- delete.vertices(graph,V(graph)[page.rank(graph)$vector<0.003&
                                          V(graph)$times_cited<500&
                                          V(graph)$keyword_count==0]) # 1963 nodes now


fivenum(V(graph)$times_cited)  ## .. 11 48 15983
length(V(graph)[V(graph)$times_cited<2&
                  V(graph)$publication_year<2013])   ## 159
graph <- delete.vertices(graph,V(graph)[V(graph)$times_cited<2&
                                          V(graph)$publication_year<2013]) ## 1804 nodes now


fivenum(page.rank(graph)$vector)  ## .. 0.0006 0.005
table(V(graph)$keyword_count)   ## 0:69,1:1091,2:460,3:146,4:28,5:8,6:2
length(V(graph)[page.rank(graph)$vector<0.001&
                  V(graph)$times_cited<500&
                  authority.score(graph)$vector<0.5&
                  V(graph)$keyword_count<3]) # 1373
graph <- delete.vertices(graph,V(graph)[page.rank(graph)$vector<0.001&
                                          authority.score(graph)$vector<0.5&
                                          V(graph)$times_cited<500&
                                          V(graph)$keyword_count<3]) # 760 nodes now

length(V(graph)[page.rank(graph)$vector<0.005&
                  V(graph)$document_type!="Article"&
                  #    authority.score(graph)$vector<0.5&
                  V(graph)$keyword_count<1]) # 23


## update authority score and pagerank attributes in the graph
V(graph)$authority_score <- authority.score(graph)$vector
V(graph)$page_rank <- page.rank(graph)$vector
V(graph)$hub_score <- hub.score(graph)$vector

## remove unnecessary attributes
list.vertex.attributes(graph)
graph <- remove.vertex.attribute(graph,"abstract_text")
graph <- remove.vertex.attribute(graph,"article_number")
graph <- remove.vertex.attribute(graph,"beginning_page")
graph <- remove.vertex.attribute(graph,"ending_page")
graph <- remove.vertex.attribute(graph,"cited_reference_count")
graph <- remove.vertex.attribute(graph,"cited_year")
graph <- remove.vertex.attribute(graph,"document_volume")
graph <- remove.vertex.attribute(graph,"funding_agency_and_grant_number")
graph <- remove.vertex.attribute(graph,"funding_text")
graph <- remove.vertex.attribute(graph,"isi_document_delivery_number")
graph <- remove.vertex.attribute(graph,"issue")
graph <- remove.vertex.attribute(graph,"page_count")
graph <- remove.vertex.attribute(graph,"part_number")
graph <- remove.vertex.attribute(graph,"special_issue")
graph <- remove.vertex.attribute(graph,"supplement")
graph <- remove.vertex.attribute(graph,"_x_nwb_id")
graph <- remove.vertex.attribute(graph,"id")
graph <- remove.vertex.attribute(graph,"isbn")
graph <- remove.vertex.attribute(graph,"digital_object_identifier")
graph <- remove.vertex.attribute(graph,"nwbWeightedPagerank")
graph <- remove.vertex.attribute(graph,"language")

###### statistics of the data ######

sum(degree(graph)==0)  # 25 isolates
# to delete isolates; not done since isolates have high keyword counts
# graph <- delete.vertices(graph, which(degree(graph) < 1)-1)

vcount(graph)  # 431
ecount(graph)  # 1360

table(V(graph)$keyword_count)   ## 0:69,1:128,2:50,3:146,4:28,5:8,6:2
table(V(graph)$document_type)  ## review:60, article:338

## to plot the distribution of document types
qplot(V(graph)$document_type)
x<-c(0,1,2,3,4,5,6)
y<-c(69,128,50,146,28,8,2)

## to plot the distribution of keywords
q<-qplot(x,y,geom="line",xlab="keyword counts", ylab="number of articles")
q+scale_x_continuous(breaks=0:7)


## export in graphml format
write.graph(graph,file="document_citation_network_scaled_down_431.graphml","graphml")

###############################################
## co-citation network scale down
###############################################
# !! make sure to remove isolates and run MST in Sci2 before loading here

## read graphml file saved from Sci2
graphco<-read.graph("co-citation_afterMST.graphml",format="graphml")
vcount(graphco)   ## 3425 nodes
ecount(graphco)   ## 3424 edges

## extract subgraph of nodes present in (reduced) direct network
## graph is the output from any step in the above direct network scale-down process
graphco<-induced.subgraph(graphco, which(V(graphco)$isi_unique_article_identifier 
                                         %in% V(graph)$isi_unique_article_identifier))

sum(degree(graphco)==0)  # 462 isolates
## instead of deleting isolates, extract largest weak component
gclust<-clusters(graphco, mode='weak')
graphco<-induced.subgraph(graphco, V(graphco)[which(gclust$membership == which.max(gclust$csize))])

vcount(graphco)   ## 582 nodes
ecount(graphco)   ## 581 edges

## remove unnecessary attributes
list.vertex.attributes(graph)
list.vertex.attributes(graphco)
graphco <- remove.vertex.attribute(graphco,"abstract_text")
graphco <- remove.vertex.attribute(graphco,"article_number")
graphco <- remove.vertex.attribute(graphco,"beginning_page")
graphco <- remove.vertex.attribute(graphco,"ending_page")
graphco <- remove.vertex.attribute(graphco,"cited_reference_count")
graphco <- remove.vertex.attribute(graphco,"cited_year")
graphco <- remove.vertex.attribute(graphco,"document_volume")
graphco <- remove.vertex.attribute(graphco,"funding_agency_and_grant_number")
graphco <- remove.vertex.attribute(graphco,"funding_text")
graphco <- remove.vertex.attribute(graphco,"isi_document_delivery_number")
graphco <- remove.vertex.attribute(graphco,"issue")
graphco <- remove.vertex.attribute(graphco,"page_count")
graphco <- remove.vertex.attribute(graphco,"part_number")
graphco <- remove.vertex.attribute(graphco,"special_issue")
graphco <- remove.vertex.attribute(graphco,"supplement")
graphco <- remove.vertex.attribute(graphco,"_x_nwb_id")
graphco <- remove.vertex.attribute(graphco,"isbn")
graphco <- remove.vertex.attribute(graphco,"digital_object_identifier")
graphco <- remove.vertex.attribute(graphco,"language")

## add attributes from direct citation graph
V(graphco)$keywords <- V(graph)$keywords
V(graphco)$keyword_count <- V(graph)$keyword_count
V(graphco)$page_rank <- V(graph)$page_rank
V(graphco)$authority_score <- V(graph)$authority_score
V(graphco)$hub_score <- V(graph)$hub_score

## export in graphml format
write.graph(graphco1,file="co-citation_582.graphml","graphml")
