library(shiny)
library(shinybulma)

##ui---------
ui = bulmaPage(
  bulmaHero(
    color = "white",
    fullheight = F,
    bulmaHeroBody(
      bulmaContainer(
        bulmaTitle(div(HTML(paste("<font color=\"#0297a0\"><b>","State", "</b></font>","<font color=\"#eda48e\"><b>"," Of", "</b></font>", 
                                  "<font color=\"#96384e\"><b>"," The", "</b></font>", "<font color=\"#04354b\"><b>"," Nation", "</b></font>")))),
        p("25 October - 29 October 2019 "),
        p("... "),
        p(div(HTML(paste("<p style='text-align:justify;'> Recent years have seen significant declines in economic well-being and growing levels of political mistrust. 
            Anecdotally, more people are growing disillusioned due to what they perceive as unkept promises and rampant corruption. 
            With the 2019 Namibian National elections approaching and hoping to quantify this sentiment, we developed a national survey designed to better undestand public opinion on national politics and related issues. 
            </p>")))),
        p("... "),
        p(div(HTML(paste("<p style='text-align:justify;'> The diagram below connects respondents' choice of candidates to their characteristics (Age, Education, Issues they care most about etc.). 
            Thickness of the chords represents the strength of connection. The Approve/Disapprove characteristic was derived from answers to the survey question:
            Do you approve or disapprove of the way Hage Geingob is handling his job as president? </p>"))))
      )
    )
  ),
  bulmaSection(
    bulmaContainer(
      chorddiagOutput("chord",width = "100%", height = "1048px"))
  ),
  bulmaSection(
    bulmaContainer(
      bulmaTileAncestor(
        bulmaTileParent(
          vertical = TRUE,
          bulmaTileChild(
            bulmaTitle("Issues By Age"),
            bulmaSubtitle("Topics that matter most to respondents by age"),
            plotOutput("plotIA"),
            color = "white"
          ),
          bulmaTileChild(
            bulmaTitle("Approval Rate By Age"),
            bulmaSubtitle("President Geingob's approval rate by age"),
            plotOutput("plotAA"),
            color = "white"
          ),
          bulmaTileChild(
            bulmaTitle("Candidate Preference By Age"),
            bulmaSubtitle("Proportion of candidate preference by age (male)"),
            plotOutput("plotSACM"),
            color = "white"
          ),
          bulmaTileChild(
            bulmaTitle("Twitter Sentiment Analysis"),
            bulmaSubtitle("General sentiment of tweets about Dr. Itula"),
            highchartOutput("sentitula", height=500),
            color = "white"
          )
          
        ),
        bulmaTileParent(
          vertical = TRUE,
          bulmaTileChild(
            bulmaTitle("Issues By Candidate"),
            bulmaSubtitle("Topics that matter most to respondents by candidate"),
            plotOutput("plotIC"),
            color = "white"
          ),
          bulmaTileChild(
            bulmaTitle("Approval Rate By Candidate"),
            bulmaSubtitle("President Geingob's approval rate by candidate"),
            plotOutput("plotAC"),
            color = "white"
          ),
          bulmaTileChild(
            bulmaTitle("Candidate Preference By Age"),
            bulmaSubtitle("Proportion of candidate preference by age (female)"),
            plotOutput("plotSACF"),
            color = "white"
          ),
          bulmaTileChild(
            bulmaTitle("Twitter Sentiment Analysis"),
            bulmaSubtitle("General sentiment of tweets about SWAPO and President Geingob"),
            highchartOutput("senthage", height=500),
            color = "white"
          )
        )
      )
    )
  ),
  # bulmaSection(
  #   bulmaContainer(
  #     bulmaTileAncestor(
  #       bulmaTileParent(
  #         vertical = F,
  #         bulmaTileChild(color = "white"),
  #         bulmaTileChild(
  #           bulmaTitle("Hover, Click And Drag"),
  #           p("See how candidates are connected to Issues that people care about the most"),
  #                        color = "white"),
  #         bulmaTileChild(color = "white")
  #         )))),
  # bulmaSection(
  #   bulmaContainer(
  #     bulmaTitle("Hover, Click And Drag"),
  #     bulmaSubtitle("See how candidates are connected to issues that people care about the most"),
  #     forceNetworkOutput("network", width = "100%", height = "500px")
  #   )
  # ),
  bulmaHero(
    color = "primary",
    bulmaHeroBody(
      # bulmaContainer(ppsrc,
      #                ppcode,
      #                ppid,
        bulmaSubtitle("State Of The Nation"),
        p("by Ergo Analytics"),
        p("... "),
        p(div(HTML(paste("<p style='text-align:justify;'>The Internet provides an alternative way to reach people and survey their opinions. 
                          It, however, requires a difficult technique if one is to get an accurate gauge of public opinion.
                          </p>")))),
        p("..."),
        p(div(HTML(paste("<p style='text-align:justify;'> Online, there is no way to randomly reach people. 
                         Instead, what we tried to do was get as big a sample size as possible (we got 421), 
                         and then winnow the sample to match known demographic measures, such as age, gender and region. 
                         Moreover, online polling can also under-sample groups of people who have less access to the Internet or who simply do not want to take surveys online. 
                         A significant population either cannot or will not take a survey online. 
                         They tend to be older, less educated and more likely to be rurally located than people who do take an online survey or are otherwise simply just not trustful of online surveys. 
                         We tried to adjust for that.</p>")))),
        p("... "),
        p(div(HTML(paste("<p style='text-align:justify;'> However, while demographic data was available and we could adjust for that and it was much harder to find the proportion of likely responders in Namibia. 
                        Therefore, what we have here is responses of people most likely to answer a survey randomly sent to them (i.e. it is likely biased towards people with the strongest opinion on politics), 
                        adjusted to known national demographics through Raking. The last section of diagrams visualises twitter sentiment of the top two candidates. 
                        We downloaded seven days worth of tweets that had each of their keywords and analysed their general sentiment.</p>")))),
        p("... "),
        p(div(HTML(paste("<p style='text-align:justify;'> For more info on methodology and the data collected contact kennethmwandingi@ergoanalyticscc.com</p>"))))
      )
    )
  )
