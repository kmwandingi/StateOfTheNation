library(shiny)
library(shinybulma)


 ##Server--------
  server = function(input, output) {
    
    output$circular <- renderPlot({
      
      #Candidate by Region
      SN %>% dplyr::group_by(Candidate,Region) %>% dplyr::summarise(
        n = ceiling(sum(Weight)))-> CR
      CR[complete.cases(CR), ]
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
        ggplot2::annotate("text", x = rep(max(CR$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
        
        geom_bar(aes(x=as.factor(id), y=n, fill=Region), stat="identity", alpha=0.5) +
        ylim(-20,55) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm") 
        ) +
        coord_polar() + 
        geom_text(data=label_data, aes(x=id, y=n+3, label=Candidate, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -2, xend = end, yend = -2), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
        geom_text(data=base_data, aes(x = title, y = -3, label=Region), hjust=c(1,1,1,1,1,1,0,0,0,0,0,0,0), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)+
        bbc_style()
  
        
      p 

    })
    
    output$network <- renderForceNetwork({
      
      sources <- SN %>%
        dplyr::group_by(Issues) %>%
        dplyr::summarise(size = length(Issues))%>%
        dplyr::rename(label = Issues)
      sources <- sources %>% rowid_to_column("group")
      
      # sources1 <- SN %>%
      #   group_by(Education) %>%
      #   summarise(size = length(Education))%>%
      #   rename(label = Education)
      # sources1 <- sources1 %>% rowid_to_column("group")
      # sources1 <- mutate(sources1, group = group + 20)
      
      
      destinations <- SN %>%
        dplyr::group_by(Candidate) %>%
        dplyr::summarise(size = length(Candidate))%>%
        dplyr::rename(label = Candidate)
      destinations$group <- 1000
      
      nodes <- full_join(sources, destinations, by = c("label", "size","group"))
      
      # nodes <- full_join(sources1, nodes, by = c("label", "size", "group"))
      
      
      nodes <- nodes %>% rowid_to_column("id")
      
      #Create Edge.list
      edges <- IC %>% 
        dplyr::left_join(nodes, by = c("Issues" = "label")) %>% 
        dplyr::rename(from = id)
      
      edges <- edges %>% 
        dplyr::left_join(nodes, by = c("Candidate" = "label")) %>% 
        dplyr::rename(to = id, weight=nweight)
      
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
      
      #Create Colour scale
    
      YourColors <- 'd3.scaleOrdinal().range([ "##4a4e4d", "#019c86",  "#0297a0",
                                         "#04354b", "#96384e", "#eda48e"]);'
      
      
      forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                     NodeID = "label", Group = "group", charge = -300, Value = "weight", Nodesize = "size",
                     opacity = 2, fontSize = 20, zoom = FALSE, linkDistance = 200, bounded = T, opacityNoHover = 0.4,
                   colourScale = JS(YourColors))
        
    })
    
    output$plotIA <- renderPlot({
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
        scale_fill_ergo(palette = "mixed") +
        labs(y = "Responses", x="") +
        coord_flip() +
        theme_minimal() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      
      # plotly::ggplotly(p)%>% config(displayModeBar = F)
      
      
    })
    
    output$plotIC <- renderPlot({
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
        scale_fill_ergo(palette = "mixed") +
        labs(y = "Responses", x="") +
        coord_flip() +
        theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      # plotly::ggplotly(p)%>% config(displayModeBar = F)
      
      
    })
    
    output$plotAA <- renderPlot({
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
        aes(x = factor(Approval, levels = level_order), fill = Age) +
        geom_bar() +
        scale_fill_ergo(palette = "mixed") +
        labs(y = "Responses", x="") +
        coord_flip() +
        theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      # plotly::ggplotly(p)%>% config(displayModeBar = F)
      
      
    })
    
    output$plotAC <- renderPlot({
      
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
        aes(x = factor(Approval, levels = level_order), fill = Candidate) +
        geom_bar() +
        scale_fill_ergo(palette = "mixed") +
        labs(y = "Responses", x="") +
        coord_flip() +
        theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      # plotly::ggplotly(p)%>% config(displayModeBar = F)
      
    })
    
    output$plotSACM <- renderPlot({
      
      plot <- SN %>%
        filter(!(Age %in% "under 18")) %>%
        filter(Sex %in% "Male") %>%
        filter(!(Approval %in% 
                   "")) %>%
        filter(!(Issues %in% "")) %>%
        filter(!(Voted %in% "")) %>%
        filter(!(Candidate %in% 
                   "")) %>%
        filter(!(Education %in% ""))
      
      
       ggplot(plot) +
        aes(x = Candidate, fill = Age) +
        geom_bar(position = "fill") +
         scale_fill_ergo(palette = "mixed") +
         labs(y = "Responses", x="") +
        coord_flip() +
        theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
       
       # plotly::ggplotly(p)%>% config(displayModeBar = F)
    })
    
    output$plotSACF <- renderPlot({
      
      plot <- SN %>%
        filter(!(Age %in% "under 18")) %>%
        filter(Sex %in% "Female") %>%
        filter(!(Approval %in% 
                   "")) %>%
        filter(!(Issues %in% "")) %>%
        filter(!(Voted %in% "")) %>%
        filter(!(Candidate %in% 
                   "")) %>%
        filter(!(Education %in% ""))
      
      
      ggplot(plot) +
        aes(x = Candidate, fill = Age) +
        geom_bar(position = "fill") +
        scale_fill_ergo(palette = "mixed") +
        labs(y = "Responses", x="") +
        coord_flip() +
        theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      # plotly::ggplotly(p)%>% config(displayModeBar = F)
      
    })
    
    output$senthage <- renderHighchart({
      
      cth <- tweets_cleaner(tweet_df_hage)
      
      x <- data.frame(tweet_nbr = 1:length(cth), clean_tweet = cth)
      x$clean_tweet <- as.character(x$clean_tweet)
      
      df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
      df <- df %>% inner_join(afinn)
      # df <- df %>% dplyr::group_by(tweet_nbr) %>% dplyr::summarize(score = sum(df$value))
      df <- aggregate(df$value, by=list(tweet_nbr=df$tweet_nbr), FUN=sum)
      colnames(df) <- c('tweet_nbr','score')
      
      df$category_senti <- ifelse(df$score < 0, "Negative", ifelse(df$score > 0, "Positive", "Neutral"))
      
      df1 <- df %>% left_join(x)
      
      xs <- list()
      xs[[1]] <- as.data.frame(df1)
      # xs[[2]] <- as.character(df1[df1$score == max(df1$score),"clean_tweet"][1,1])
      # xs[[3]] <- as.character(df1[df1$score == min(df1$score),"clean_tweet"][1,1])
      
      senti_hage <- xs[[1]] %>% mutate(score_pct = as.numeric(xs[[1]]$score)/sum(as.numeric(xs[[1]]$score))*100, coloract = c("#d35400"))
      
      for (i in 1:nrow(senti_hage)){
        
        if (senti_hage[i,"score_pct"]>0){
          senti_hage[i,"coloract"] = c('#019c86')
        } else if (senti_hage[i,"score_pct"]==0){
          senti_hage[i,"coloract"] = c('#eed284')
        } else {
          senti_hage[i,"coloract"] = c('#96384e')
        }
      }
      
      
      hc <- highchart() %>%
        #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
        hc_chart(type = "bar") %>%
        #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
        hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                   formatter = JS(paste0("function() {
                                       console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
                   }")))%>%
        hc_xAxis(categories = senti_hage$category_senti,
                 #labels = list(rotation = 0, step=1), title =list(text="Brand")
                 labels = list(style = list(fontSize= '12px')) #max=20, scrollbar = list(enabled = T)
        )    %>%
        hc_colors(colors = senti_hage$coloract) %>% 
        hc_add_series(name="Sentiment", data = senti_hage$score_pct, colorByPoint = TRUE, 
                      type ="column",
                      #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                      color = "#4472c4", showInLegend= F) %>% 
        hc_yAxis(labels=list(format = '{value}%'), showFirstLabel = TRUE,showLastLabel=TRUE)
      #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
      
      hc
      
    })
    
    output$sentitula <- renderHighchart({
      
      cti <- tweets_cleaner(tweet_df_itula)
      
      x <- data.frame(tweet_nbr = 1:length(cti), clean_tweet = cti)
      x$clean_tweet <- as.character(x$clean_tweet)
      
      df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
      df <- df %>% inner_join(afinn)
      # df <- df %>% dplyr::group_by(tweet_nbr) %>% dplyr::summarize(score = sum(df$value))
      df <- aggregate(df$value, by=list(tweet_nbr=df$tweet_nbr), FUN=sum)
      colnames(df) <- c('tweet_nbr','score')
      
      df$category_senti <- ifelse(df$score < 0, "Negative", ifelse(df$score > 0, "Positive", "Neutral"))
      
      df1 <- df %>% left_join(x)
      
      xs <- list()
      xs[[1]] <- as.data.frame(df1)
      # xs[[2]] <- as.character(df1[df1$score == max(df1$score),"clean_tweet"][1,1])
      # xs[[3]] <- as.character(df1[df1$score == min(df1$score),"clean_tweet"][1,1])
      
      senti_itula <- xs[[1]] %>% mutate(score_pct = as.numeric(xs[[1]]$score)/sum(as.numeric(xs[[1]]$score))*100, coloract = c("#d35400"))
      
      for (i in 1:nrow(senti_itula)){
        
        if (senti_itula[i,"score_pct"]>0){
          senti_itula[i,"coloract"] = c('#019c86')
        } else if (senti_itula[i,"score_pct"]==0){
          senti_itula[i,"coloract"] = c('#eed284')
        } else {
          senti_itula[i,"coloract"] = c('#96384e')
        }
      }
      
      
      hc <- highchart() %>%
        #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
        hc_chart(type = "bar") %>%
        #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
        hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                   formatter = JS(paste0("function() {
                                       console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
                   }")))%>%
        hc_xAxis(categories = senti_itula$category_senti,
                 #labels = list(rotation = 0, step=1), title =list(text="Brand")
                 labels = list(style = list(fontSize= '12px')) #max=20, scrollbar = list(enabled = T)
        )    %>%
        hc_colors(colors = senti_itula$coloract) %>% 
        hc_add_series(name="Sentiment", data = senti_itula$score_pct, colorByPoint = TRUE, 
                      type ="column",
                      #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                      color = "#4472c4", showInLegend= F) %>% 
        hc_yAxis(labels=list(format = '{value}%'), showFirstLabel = TRUE,showLastLabel=TRUE)%>% 
        hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
      
        hc
      
    })
    
    output$chord <- renderChorddiag({
      chorddiag(SNn, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 5, 
                groupColors = c("##4a4e4d", "#019c86",  "#0297a0","#04354b", "#96384e", "#eda48e",
                                "#96ceb4", "#ff6f69","#4b3832","#88d8b0","#854442","#eed284"),margin = 90)
    })
    
    
  }
