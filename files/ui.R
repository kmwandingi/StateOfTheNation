library(fullPage)
library(shiny)

ui <- pagePiling(
  center = TRUE,
  sections.color = c(
    "#4B97D2",
    "#FFE6F4",
    "#FFBDE1",
    "#FFBDE1",
    "#AEFFAC",
    "#C490F4",
    "#4B97D2"
  ),
  pageTheme("maroon"),
  menu = c(
    "Piling" = "intro",
    "Plots" = "backgrounds",
    "Grid" = "grid",
    "Grid1" = "grid1",
    "Grid2" = "grid2",
    "Grid3" = "grid3",
    "sisters" = "sisters"
  ),
  
  pageSection(
    menu = "intro",
    h1("Background Plot")
    ),
  pageSectionPlot(
    "circular",
    menu = "backgrounds",
    pageContainer(
      pageRow(
        pageColumn(),
        pageColumn(
          h1("Background Plot")
        ),
        pageColumn()
      )
    )
  ),
  pageSection(
    menu = "grid",
    h1("Grid"),
    pageRow(
      pageColumn(
        h2("Issues By Age"),
        p("The issues that respondents cared about the most by age.")
      ),
      pageColumn(
        plotOutput("plotIA")
      )
    )
  ),
  pageSection(
    menu = "grid1",
    h1("Grid1"),
    pageRow(
      pageColumn(
        h2("Issues By Candidate"),
        p("The issues that respondents cared about the most by candidate")
      ),
      pageColumn(
        plotOutput("plotIC")
      )
    )
  ),
  pageSection(
    menu = "grid2",
    h1("Grid2"),
    pageRow(
      pageColumn(
        h2("Approval By Age"),
        p("The approval/disapproval of Hage Geingob's job as president by age")
      ),
      pageColumn(
        plotOutput("plotAA")
      )
    )
  ),
  pageSection(
    menu = "grid3",
    h1("Grid3"),
    pageRow(
      pageColumn(
        h2("Approval By Canndidate"),
        p("The approval/disapproval of Hage Geingob's job as president by preffered candidate")
      ),
      pageColumn(
        plotOutput("plotAC")
      )
    )
  ),

  pageSection(
    menu = "sisters",
    pageContainer(
      h1("Sister functions", style = "color:#f3f3f3;"),
      verbatimTextOutput("sisters")
    )
  )
)

