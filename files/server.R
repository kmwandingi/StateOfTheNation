server <- function(input, output) { 
  
  output$circular <- renderPlot({
    
    #Candidate by Region
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
    CR[1:85,] -> CR
    
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

    })
  
  output$network <- renderForceNetwork({
    
    sources <- SN %>%
      group_by(Issues) %>%
      summarise(size = length(Issues))%>%
      rename(label = Issues)
    
    # sources1 <- SN %>%
    #   group_by(Education) %>%
    #   summarise(size = length(Education))%>%
    #   rename(label = Education)
    
    
    destinations <- SN %>%
      group_by(Candidate) %>%
      summarise(size = length(Candidate))%>%
      rename(label = Candidate)
    
    
    nodes <- full_join(sources, destinations, by = c("label", "size"))
    # nodes <- full_join(sources1, nodes, by = c("label", "size"))
    
    
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
    
    # edges <- full_join(edges,edges1,by = c("Candidate", "from", "to","weight"))
    
    
    #Make ids start with zero instead of one to be compatible with d3
    nodes_d3 <- mutate(nodes, id = id - 1)
    edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
    
    forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                 NodeID = "label", Group = "id", Value = "weight", Nodesize = "size",
                 opacity = 1,fontSize = 16, zoom = TRUE)
    
  })
  
  output$plotIA <- renderPlot({
    Plot <- SN %>%
      filter(!(Region %in% "Zambezi")) %>%
      filter(!(Age %in% "under 18")) %>%
      filter(!(Approval %in% 
                 "")) %>%
      filter(!(Issues %in% "")) %>%
      filter(!(Likely.Voter %in% "")) %>%
      filter(!(Candidate %in% 
                 "")) %>%
      filter(!(Education %in% ""))
    
    ggplot(Plot) +
      aes(x = Issues, fill = Age) +
      geom_bar() +
      scale_fill_brewer(palette = "GnBu") +
      labs(y = "Responses", title = "Issues By Age") +
      coord_flip() +
      theme_minimal()
    
  })
  
  output$plotIC <- renderPlot({
    Plot <- SN %>%
      filter(!(Region %in% "Zambezi")) %>%
      filter(!(Age %in% "under 18")) %>%
      filter(!(Approval %in% 
                 "")) %>%
      filter(!(Issues %in% "")) %>%
      filter(!(Likely.Voter %in% "")) %>%
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
    
  })
  
  output$plotAA <- renderPlot({
    Plot <- SN %>%
      filter(!(Region %in% "Zambezi")) %>%
      filter(!(Age %in% "under 18")) %>%
      filter(!(Approval %in% 
                 "")) %>%
      filter(!(Issues %in% "")) %>%
      filter(!(Likely.Voter %in% "")) %>%
      filter(!(Candidate %in% 
                 "")) %>%
      filter(!(Education %in% ""))
    
    ggplot(Plot) +
      aes(x = Approval, fill = Age) +
      geom_bar() +
      scale_fill_brewer(palette = "GnBu") +
      labs(y = "Responses", title = "President Hage Geingob Current Approval By Age") +
      coord_flip() +
      theme_minimal()
    
  })
  
  output$plotAC <- renderPlot({
    
    Plot <- SN %>%
      filter(!(Region %in% "Zambezi")) %>%
      filter(!(Age %in% "under 18")) %>%
      filter(!(Approval %in% 
                 "")) %>%
      filter(!(Issues %in% "")) %>%
      filter(!(Likely.Voter %in% "")) %>%
      filter(!(Candidate %in% 
                 "")) %>%
      filter(!(Education %in% ""))
    
    ggplot(Plot) +
      aes(x = Approval, fill = Candidate) +
      geom_bar() +
      scale_fill_brewer(palette = "GnBu") +
      labs(y = "Responses", title = "President Hage Geingob Current Approval by Candidate Support") +
      coord_flip() +
      theme_minimal()
  })
  
  }