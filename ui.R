# UI

library(shiny)
library(shinydashboard)
library(leaps)
library(downloader)
library(gclus)
library(DT)


shinyUI(dashboardPage(
  
  # 標題列
  dashboardHeader(title = "iModelSelection"),
  
  # 側邊欄
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Source", tabName = "DataSource", icon = icon("database")),
      menuItem("Data View", tabName = "DataView", icon = icon("table")),
      menuItem("Methods Review", tabName = "MethodReview", icon = icon("archive")),
      menuItem("References View", tabName = "ReferenceView", icon = icon("folder-open")),
      menuItem("MetaExperiments", tabName = "MetaExperiments", icon = icon("bar-chart-o")),
      menuItem("科技部申請計劃書", tabName = "Proposal", icon = icon("book"))
    )
  ),
  
  # 主體
  dashboardBody(
    tabItems(
      tabItem(tabName = "DataSource",
              wellPanel(
                uiOutput("OzDASL"),
                hr(),
                uiOutput('storySelect')
              )
      ),
      tabItem(tabName = "DataView",
              wellPanel(
                h4('Observations'),
                DT::dataTableOutput("OzDASLdataView"),
                br()
              )
      ),
      tabItem(tabName = "MethodReview",
              wellPanel(
                sliderInput("whichDocs",
                            "請挑選一篇手冊來觀賞",
                            min = 1,
                            max = 4,
                            value = 1,
                            width = '100%'
                ),
                uiOutput("leapsHelp")
              )
      ),
      tabItem(tabName = "ReferenceView",
              wellPanel(
                sliderInput("whichReference",
                            "請挑選一篇參考資料來觀賞",
                            min = 1,
                            max = 4,
                            value = 3,
                            width = '100%'
                ),
                uiOutput("ReferenceHelp")

              )
      ),
      tabItem(tabName = "Proposal",
              wellPanel(
                tags$iframe(src="Proposal.pdf", width="100%", height="688")
              )

      ),
      tabItem(tabName = "MetaExperiments",
              tabsetPanel(
                          tabPanel("step1. 了解數據",
                                   uiOutput("Mstep1")
                          ),
                          tabPanel("step2. 建立所有模型",
                                   uiOutput("Mstep2")
                          ),
                          tabPanel("step3. 蒐集模型超數據",
                                   uiOutput("Mstep3")
                          ),
                          tabPanel("step4. 超數據實驗設計",
                                   uiOutput("Mstep4")
                          ),
                          tabPanel("step5. 群集分析",
                                   uiOutput("Mstep5")
                          ),
                          tabPanel("step6. 實證",
                                   uiOutput("Mstep6")
                          )
              )
      )
    )
  )
))
