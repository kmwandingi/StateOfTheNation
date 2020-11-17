library(rsconnect)
library(weights)
library(anesrake)
library(reshape2)
library(bbplot)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(igraph)
library(networkD3)
library(RColorBrewer)
library(data.table)
library(ggiraph)
library(plotly)
library(shiny)
library(shinybulma)
library(htmltools)
library(twitteR)
library(rtweet)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(syuzhet) # Sentiment
library(topicmodels)
library(tidytext)
library(slam)
library(highcharter)
library(textdata)
library(chorddiag)
library(Cairo)

options(shiny.usecairo=T)

myPalette <- brewer.pal(5, "Set2")

SN <- read.csv("State Of The Nation_2.csv")
# rapcnt <- read.csv("C:/Users/mwandingik/Box/Ergo Analytics/Projects/Polling/data/Age_Fraction.csv")
# rpcnt <- read.csv("C:/Users/mwandingik/Box/Ergo Analytics/Projects/Polling/data/Sex_Fraction.csv")
apcnt <- read.csv("Age_Fraction_2.csv")
spcnt <- read.csv("Sex_Fraction_2.csv")

afinn <- readr::read_tsv("AFINN-111.txt", col_names = F)
colnames(afinn) <- c("word","value")

SN %>% dplyr::rename(Region = Where.do.you.reside.currently.,
                     Age = What.is.your.age.,
                     Sex = What.is.your.gender.,
                     Approval = Do.you.approve.or.disapprove.of.the.way.Hage.Geingob.is.handling.his.job.as.president.,
                     Issues = Which.one.of.the.following.issues.matters.MOST.to.you.right.now.,
                     Voted = Did.you.vote.in.the.2014.presidential.election..or.not.,
                     Candidate = If.the.2019.Namibia.general.election.were.being.held.today..which.candidate.would.you.vote.for.,
                     Education = What.is.your.education.level.) -> SN

# target <- with(rapcnt, list(
#   Sex  = wpct(Sex, Age.Pcnt.per.Sex.Region),
#   Age  = wpct(Age, Age.Pcnt.per.Sex.Region),
#   Region  = wpct(Region, Age.Pcnt.per.Sex.Region)
# ))
# 
# 
# target <- with(rpcnt, list(
#   Sex  = wpct(Sex, Sex.Pcnt.Per.region),
#   Region  = wpct(Region, Sex.Pcnt.Per.region)
# ))

target <- with(apcnt, list(
  Sex  = wpct(Sex, Age.Pcnt.per.sex),
  Age  = wpct(Age, Age.Pcnt.per.sex)
))



names(target$Sex) <- levels(SN$Sex)
names(target$Age) <- levels(SN$Age)

# names(target$Region) <- levels(SN$Region)


raking <- anesrake(target,
                                       SN,
                                       SN$Resposnse.ID,
                                       cap = 5,                      # Maximum allowed weight per iteration
                                       choosemethod = "total",       # How are parameters compared for selection?
                                       type = "pctlim",              # What selection criterion is used?
                                       pctlim = 0.05                 # Threshold for selection
                                       )

SN$Weight <- raking$weightvec

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
#Candidate by Age
SN %>% dplyr::group_by(Candidate, Age) %>% dplyr::summarise(n = length(Candidate),
                                                            nweight = sum(Weight))-> AC

#Candidate By issue
SN %>% dplyr::group_by(Candidate, Issues) %>% dplyr::summarise(n = length(Candidate),
                                                               nweight = sum(Weight))-> IC
#candidate by Education
SN %>% dplyr::group_by(Candidate, Education) %>% dplyr::summarise(n = length(Candidate),
                                                                  nweight = sum(Weight))-> EC

#Issues by Age
SN %>% dplyr::group_by(Issues, Age) %>% dplyr::summarise(n = length(Issues),
                                                         nweight = sum(Weight))-> IA

level_order <- c('Strongly Disapprove', 'Disapprove', 'Neither Approve nor Disapprove', 'Approve','Strongly Approve')

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

SNn <- ICn %>% 
  left_join(AACn, by = "Candidate") %>%
  left_join(ECn, by = "Candidate") %>%
  left_join(VCn, by = "Candidate") %>%
  left_join(ACn, by = "Candidate") 

SNn <- SNn%>%dplyr::rename(`Economy` = `Jobs and the Economy`)

SNn <- SNn[-1,]
row.names(SNn) <- SNn$Candidate
SNn <- SNn[,-1]

SNn <- as.matrix(SNn)

# ergo corporate colors--------------
ergo_colors <- c(
  `dark`  = "#4a4e4d ",
  `green`      = "#019c86",
  `blue green` = "#0297a0",
  `blue`    = "#04354b",
  `red`     = "#96384e",
  `orange`  = "#eda48e",
  `yellow`  = "#eed284"
)

#' Function to extract ergo colors as hex codes
#'
#' @param ... Character names of ergo_colors 
#'
ergo_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ergo_colors)
  
  ergo_colors[cols]
}

#combine colors into palletes

ergo_palettes <- list(
  `main`  = ergo_cols("blue", "green", "blue green"),
  
  `cool`  = ergo_cols("blue", "green", "blue green"),
  
  `dark`   = ergo_cols("dark","blue"),
  
  `mixed` = ergo_cols("green", "blue","red", "orange", "yellow"),
  
  `light`  = ergo_cols("orange", "yellow")
)

#' Return function to interpolate a ergo color palette
#'
#' @param palette Character name of palette in ergo_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette
#'
ergo_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ergo_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#Scales for ggplot2

#' Color scale constructor for ergo colors
#'
#' @param palette Character name of palette in ergo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale or
#'            scale_color_gradientn, used respectively when discrete is TRUE or FALSE
#'
scale_color_ergo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ergo_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ergo_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for ergo colors
#'
#' @param palette Character name of palette in ergo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale or
#'            scale_fill_gradientn, used respectively when discrete is TRUE or FALSE
#'
scale_fill_ergo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ergo_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ergo_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#Sentiment----

#======== User defined functions ====================
tweets_cleaner <- function(tweet.df){
  
  tweets_txt <- unique(tweet.df$text)
  clean_tweet = gsub("&amp", "", tweets_txt) # Remove Amp
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) # Remove Retweet
  clean_tweet = gsub("@\\w+", "", clean_tweet) # Remove @
  clean_tweet = gsub("#", " ", clean_tweet) # Before removing punctuations, add a space before every hashtag
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet) # Remove Punct
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet) # Remove Digit/Numbers
  clean_tweet = gsub("http\\w+", "", clean_tweet) # Remove Links
  clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet) # Remove tabs
  clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) # Remove extra white spaces
  clean_tweet = gsub("^ ", "", clean_tweet)  # remove blank spaces at the beginning
  clean_tweet = gsub(" $", "", clean_tweet) # remove blank spaces at the end
  clean_tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", clean_tweet) # Remove Unicode Char
  
  
  clean_tweet <- str_replace_all(clean_tweet," "," ") #get rid of unnecessary spaces
  clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
  clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
  clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header, there is only one
  clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
  clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") # Get rid of references to other screennames
  
  clean_tweet
}

tweet_df_hage <- readRDS(file = "tweet_df_hage.rds")
tweet_df_itula <- readRDS(file = "tweet_df_itula.rds")



