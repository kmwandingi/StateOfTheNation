library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(igraph)
library(networkD3)
library(RColorBrewer)
library(data.table)
library(bbplot)
library(chorddiag)
library(reshape2)


myPalette <- brewer.pal(5, "Set2")

#data input--------
#SN <- read.csv("C:/Users/mwandingik/Box/Ergo Analytics/Projects/sotn/State Of The Nation_2.csv")
SN %>% dplyr::rename(Region = Where.do.you.reside.currently.,
                     Age = What.is.your.age.,
                     Sex = What.is.your.gender.,
                     Approval = Do.you.approve.or.disapprove.of.the.way.Hage.Geingob.is.handling.his.job.as.president.,
                     Issues = Which.one.of.the.following.issues.matters.MOST.to.you.right.now.,
                     Voted = Did.you.vote.in.the.2014.presidential.election..or.not.,
                     Candidate = If.the.2019.Namibia.general.election.were.being.held.today..which.candidate.would.you.vote.for.,
                     Education = What.is.your.education.level.) -> SN



SN %>% dplyr::group_by(Candidate) %>% dplyr::summarise(n = length(Candidate),
                                                      nweight = sum(Weight))-> C

SN %>% dplyr::group_by(Approval) %>% dplyr::summarise(n = length(Approval),
                                                       nweight = sum(Weight))-> A

SN %>% dplyr::group_by(Voted) %>% dplyr::summarise(n = length(Voted),
                                                      nweight = sum(Weight))-> V

SN %>% dplyr::group_by(Education) %>% dplyr::summarise(n = length(Education),
                                                          nweight = sum(Weight))-> E

SN %>% dplyr::group_by(Issues) %>% dplyr::summarise(n = length(Issues),
                                                       nweight = sum(Weight))-> I

SN %>% dplyr::count(Candidate)-> C
SN %>% dplyr::count(Region) -> R
SN %>% dplyr::count(Age) -> A
SN %>% dplyr::count(Sex) -> G
SN %>% dplyr::count(Approval) -> AP
SN %>% dplyr::count(Voted) -> V
SN %>% dplyr::count(Education) -> E
SN %>% dplyr::count(Issues) -> I

colnames(SN)
#Candidate by Age
SN %>% dplyr::count(Candidate,Age) -> AC
SN %>% dplyr::group_by(Candidate, Age) %>% dplyr::summarise(n = length(Candidate),
                                                       nweight = sum(Weight))-> AC


#Candidate By issue
SN %>% dplyr::count(Issues, Candidate) -> IC
SN %>% dplyr::group_by(Candidate, Issues) %>% dplyr::summarise(n = length(Candidate),
                                                            nweight = sum(Weight))-> IC
#candidate by Education
SN %>% dplyr::group_by(Candidate, Education) %>% dplyr::summarise(n = length(Candidate),
                                                               nweight = sum(Weight))-> EC


#Issues by Age
SN %>% dplyr::group_by(Issues, Age) %>% dplyr::summarise(n = length(Issues),
                                                               nweight = sum(Weight))-> IA




#Circular bar plot------
#Candidate by Region
SN %>% dplyr::count(Candidate,Region) -> CR
SN %>% dplyr::group_by(Candidate,Region) %>% dplyr::summarise(
                                                         n = ceiling(sum(Weight)))-> CR
CR <- data.frame(CR)


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame(matrix(NA, empty_bar*nlevels(CR$Region), ncol(CR)))
colnames(to_add) <- colnames(CR)
to_add$Region <- rep(levels(CR$Region), each=empty_bar)
CR <- rbind(CR, to_add)
CR <- CR %>% arrange(Region)
CR$id <- seq(1, nrow(CR))

#cut off the last empty cells
CR[1:82,] -> CR

# Get the name and the y position of each label
label_data <- CR
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- CR %>% 
  group_by(Region) %>% 
  dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(CR, aes(x=as.factor(id), y=n, fill=Region)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=n, fill=Region), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the n of each 100/75/50/25 lines
  annotate("text", x = rep(max(CR$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=n, fill=Region), stat="identity", alpha=0.5) +
  ylim(-20,50) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+5, label=Candidate, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.0, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -3, xend = end, yend = -3), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -4, label=Region), hjust=c(1,1,1,1,1,1,0,0,0,0,0,0,0), colour = "black", alpha=0.8, size=2.0, fontface="bold", inherit.aes = FALSE)

p

ggiraph::girafe(ggobj=p)

# Pie Charts----------
prsdt <- pie(C$nweight, labels = C$Candidate, border="white", col=myPalette )
ggiraph::girafe(pie(P$n, labels = P$Candidate))

# network ----------

# create a dataset:
SN %>% dplyr::select(Candidate,Education,Issues) %>%
  dplyr::rename(Candidate = Candidate, 
                Education = Education, 
                Issue = Issues) -> SNNet

SNNET <- SNNet[,1:2]
# Plot
p <- simpleNetwork(SNNet)

p <- simpleNetwork(SNNet, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)
p

#Create Nodes.list
# sources <- SN %>%
#   distinct(Issues) %>%
#   rename(label = Issues)

sources <- SN %>%
  group_by(Issues) %>%
  summarise(size = length(Issues))%>%
  rename(label = Issues)
sources <- sources %>% rowid_to_column("group")
# sources1 <- sources1 %>% rowid_to_column("group")
# sources1 <- mutate(sources1, group = group + 20)


# sources1 <- SN %>%
#   group_by(Education) %>%
#   summarise(size = length(Education))%>%
#   rename(label = Education)


# destinations <- SN %>%
#   distinct(Candidate) %>%
#   rename(label = Candidate)

destinations <- SN %>%
  group_by(Candidate) %>%
  summarise(size = length(Candidate))%>%
  rename(label = Candidate)
destinations$group <- 1000

nodes <- full_join(sources, destinations, by = c("label", "size","group"))
# nodes <- full_join(sources1, nodes, by = c("label", "size","group"))


nodes <- nodes %>% rowid_to_column("id")

#Create Edge.list
edges <- IC %>% 
  left_join(nodes, by = c("Issues" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Candidate" = "label")) %>% 
  rename(to = id, weight=nweight)

# edges1 <- EC %>% 
#   left_join(nodes, by = c("Education" = "label")) %>% 
#   rename(from = id)
# 
# edges1 <- edges1 %>% 
#   left_join(nodes, by = c("Candidate" = "label")) %>% 
#   rename(to = id, weight=nweight)


edges <- select(edges, from, to, weight)
# edges1 <- select(edges1, from, to, weight)
# 
# edges <- full_join(edges,edges1,by = c("Candidate", "from", "to","weight"))


#Make ids start with zero instead of one to be compatible with d3
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "group", Value = "weight", Nodesize = "size",
             opacity = 1,fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

#ggplots-----------------------------
Plot <- SN %>%
 filter(!(Region %in% "Zambezi")) %>%
 filter(!(Age %in% "under 18")) %>%
 filter(!(Approval %in% 
    "")) %>%
 filter(!(Issues %in% "")) %>%
 filter(!(Voted %in% "")) %>%
 filter(!(Candidate %in% 
    "")) %>%
 filter(!(Education %in% ""))

ggplot(Plot) +
 aes(x = Issues, fill = Age) +
 geom_bar() +
 scale_fill_brewer(palette = "GnBu") +
 labs(y = "Responses", title = "Issues By Age") +
 coord_flip() +
 theme_minimal(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Plot <- SN %>%
 filter(!(Region %in% "Zambezi")) %>%
 filter(!(Age %in% "under 18")) %>%
 filter(!(Approval %in% 
    "")) %>%
 filter(!(Issues %in% "")) %>%
 filter(!(Voted %in% "")) %>%
 filter(!(Candidate %in% 
    "")) %>%
 filter(!(Education %in% ""))

ggplot(Plot) +
 aes(x = Issues, fill = Candidate) +
 geom_bar() +
 scale_fill_brewer(palette = "GnBu") +
 labs(y = "Responses", title = "Issues By Candidate") +
 coord_flip() +
 theme_minimal()

Plot <- SN %>%
 filter(!(Region %in% "Zambezi")) %>%
 filter(!(Age %in% "under 18")) %>%
 filter(!(Approval %in% 
    "")) %>%
 filter(!(Issues %in% "")) %>%
 filter(!(Voted %in% "")) %>%
 filter(!(Candidate %in% 
    "")) %>%
 filter(!(Education %in% ""))

ggplot(Plot) +
 aes(x = Approval, fill = Age) +
 geom_bar() +
 scale_fill_brewer(palette = "GnBu") +
 labs(y = "Responses", title = "President age Geingob Current Approval By Age") +
 coord_flip() +
 theme_minimal()

Plot <- SN %>%
 filter(!(Region %in% "Zambezi")) %>%
 filter(!(Age %in% "under 18")) %>%
 filter(!(Approval %in% 
    "")) %>%
 filter(!(Issues %in% "")) %>%
 filter(!(Voted %in% "")) %>%
 filter(!(Candidate %in% 
    "")) %>%
 filter(!(Education %in% ""))

p <- ggplot(Plot) +
 aes(x = Approval, fill = Candidate) +
 geom_bar() +
  scale_fill_ergo(palette = "mixed") +
 labs(y = "Responses") +
 coord_flip() +
 theme_minimal() + bbc_style()

Plot <- SN %>%
  filter(!(Region %in% "Zambezi")) %>%
  filter(!(Age %in% "under 18")) %>%
  filter(!(Approval %in% 
             "")) %>%
  filter(!(Issues %in% "")) %>%
  filter(!(Voted %in% "")) %>%
  filter(!(Candidate %in% 
             "")) %>%
  filter(!(Education %in% ""))

p <- ggplot(Plot) +
  aes(x = factor(Approval, levels = level_order), fill = Age) +
  geom_bar() +
  scale_fill_ergo(palette = "mixed") +
  labs(y = "Responses", x="", title = "Approval by Age") +
  coord_flip() +
  theme_minimal() +
  bbc_style() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #plot.title = element_text(hjust = 0.5),
        legend.position = "right",legend.justification = "right") 

finalise_plot(plot_name = p,
              source = "Source: ergo analytics",
              save_filepath = "C:/Users/mwandingik/Box/Ergo Analytics/Projects/sotn/files/AC.png",
              width_pixels = 640,
              height_pixels = 450,
              logo_image_path = "C:/Users/mwandingik/Box/Ergo Analytics/Projects/sotn/files/LOGO.png")

#chorddiagram----
SN %>% dplyr::group_by(Candidate, Age) %>% dplyr::summarise(A = sum(Weight))-> ACn

SN %>% dplyr::group_by(Candidate, Issues) %>% dplyr::summarise(I = sum(Weight))-> ICn

SN %>% dplyr::group_by(Candidate, Education) %>% dplyr::summarise(E = sum(Weight))-> ECn

SN %>% dplyr::group_by(Candidate, Voted) %>% dplyr::summarise(V = sum(Weight))-> VCn

SN %>% dplyr::group_by(Candidate, Approval) %>% dplyr::summarise(Ap = sum(Weight))-> AACn


ACn <- recast(ACn, Candidate + variable ~ Age, id.var = c("Candidate", "Age")) %>% dplyr::select(-variable)

ICn <- recast(ICn, Candidate + variable ~ Issues, id.var = c("Candidate", "Issues"))%>% dplyr::select(-variable, -Var.3)

ECn <- recast(ECn, Candidate + variable ~ Education, id.var = c("Candidate", "Education"))%>% dplyr::select(-variable, -Var.3)

VCn <- recast(VCn, Candidate + variable ~ Voted, id.var = c("Candidate", "Voted"))%>% dplyr::select(-variable, -Var.3)

AACn <- recast(AACn, Candidate + variable ~ Approval, id.var = c("Candidate", "Approval"))%>% dplyr::select(-variable, -Var.3)

#combine

SNn <- ECn %>% 
  left_join(ICn, by = "Candidate") %>%
  left_join(AACn, by = "Candidate") %>%
  left_join(VCn, by = "Candidate") %>%
  left_join(ACn, by = "Candidate")
SNn <- SNn%>%dplyr::rename(`Economy` = `Jobs and the Economy`)
SNn <- SNn[-1,]
row.names(SNn) <- SNn$Candidate
SNn <- SNn[,-1]

SNn <- as.matrix(SNn)


chorddiag(SNn, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 5, margin = 90, width = "100%", height = "1048px")
# Make the circular plot

  

