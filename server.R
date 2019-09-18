
library(shiny)
library(shinydashboard)
library(DT)
library(Hmisc)
library(leaps)
library(combinat)
library(DoE.base)
library(cluster)
library(plyr)
source('global.R')

shinyServer(function(input, output) {
  
  # DataSource
  output$OzDASL <- renderUI({
    download(paste(urlS, 
                   substr(z, 
                          dataLinkS[as.numeric(input$whichStory)]+6,
                          #dataLinkS[36]+6,
                          dataLinkE[as.numeric(input$whichStory)]-1
                          #dataLinkE[36]-1
                   ), 
                   urlE, 
                   sep = ""), 
             'z.txt')
    
    z <- readLines('z.txt')
    z <- paste(z, collapse = "")
    #z
    IntroS <- gregexpr(pattern = '<h1>',text = z)[[1]][1]
    IntroE <- gregexpr(pattern = '<h2>Analysis',text = z)[[1]][1]
    if (IntroE == -1) {
      IntroE <- gregexpr(pattern = '<p>&nbsp;</p>',text = z)[[1]][1]  
    }
    
    #HTML(paste(z[28:107], collapse = ""))
    HTML(substr(z, IntroS, IntroE-1))
  })
  

  # SelectStory(Data)
  output$storySelect <- renderUI({
    
    radioButtons("whichStory", label = h3("請挑選一個故事來觀賞"),
                 choices = as.list(setNames(1:dim(DataTable)[1], DataTable$title)), 
                 selected = 9, inline = TRUE, width = '100%')
  })
  
  OzDASLdata <- reactive({
  
    url <- paste(urlS, 
                 substr(z, 
                        dataLinkS[as.numeric(input$whichStory)]+6,
                        #dataLinkS[1]+6, 
                        dataLinkE[as.numeric(input$whichStory)]-1
                        #dataLinkE[1]-1
                 ), 
                 '.txt', 
                 sep = "")
    Data <- read.table(url, header = TRUE)
    Data
    
  })
  
  selectDATAix <- reactive({input$whichStory})
  
  
  # DataView
  output$OzDASLdataView <- DT::renderDataTable({
    
    dataName <- paste0('Dataset: ', as.character(DataTable$title[as.numeric(selectDATAix())]))
    
    datatable(OzDASLdata(), filter = 'top', options = list(
      pageLength = 25, autowidth = TRUE
    ),
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center', dataName
    )
    
    )
  })
  
  # MethodReview
  output$leapsHelp <- renderUI({

    docs <- c("leaps.html", "leaps.setup.html", "regsubsets.html", "plot.regsubsets.html")

    Dirurl <- "leaps/html/"
    url <- paste(Dirurl,
                 #docs[1],
                 docs[input$whichDocs],
                 sep = "")

    #download(url, 'z.txt')
    #HTML(paste(readLines('z.txt'), collapse = " "))
    HTML(paste(readLines(url), collapse = " "))
  })

  # ReferenceView
  observe({
    AllPDF <- list.files("www")[grep(".pdf", list.files("www"))]
    # "bestglm.pdf", "leaps.pdf", "R10.pdf", "Rbestsubsets.pdf"
    AllPDF <- AllPDF[c(1,4,8,9)]

    output$ReferenceHelp <- renderUI({
      wellPanel(
        tags$iframe(src = AllPDF[input$whichReference], width="100%", height="688")
      )
    })
  })

  # step1 了解數據
  observe({
    # OzDASLdata <- OzDASLdata()

    output$Mstep1 <- renderUI({
      wellPanel(
        fluidRow(
          column(12,
                 # verbatimTextOutput("DataDescribe")
                 plotOutput("CorrScatterM")

          ),
          column(12,
                 verbatimTextOutput("DataSummary")

          )
        )
      )
    })

    # 資料集的敘述統計量
    output$DataSummary <- renderPrint({
      summary(OzDASLdata())
    })

    output$CorrScatterM <- renderPlot({
      Data <- OzDASLdata()
      
      nums_vars <- unlist(lapply(Data, is.numeric))
      Data <- Data[,nums_vars]
      Data.r <- abs(cor(Data)) # get correlations
      Data.col <- dmat.color(Data.r) # get colors
      # reorder variables so those with highest correlation are closest to the diagonal
      Data.o <- order.single(Data.r)
      cpairs(Data, Data.o, panel.colors=Data.col, gap=.5,
             main=" Variables Ordered and Colored by Correlation" )
    })


  })
  
  # 建立所有可能模型
  Regsubset <- reactive({
    
    RegSubsets <- with(OzDASLdata(), regsubsets(OzDASLdata()[, input$varY] ~ .,
                                              data = OzDASLdata()[, -which(names(OzDASLdata()) == input$varY)],
                                              nbest = 100,
                                              really.big = T))
    RegSubsets

  })

  # step2 建立所有模型
  observe({
    
    # 建立所有模型
    output$Mstep2 <- renderUI({
      wellPanel(
        fluidRow(
          column(12,
                 selectInput(inputId = "varY",
                             label = "Select Y variable",
                             choices = names(OzDASLdata()))
          )
        ),
        fluidRow(
          column(12,
                 h3(helpText("All Possible Regression Model"), align = TRUE),
                 DT::dataTableOutput("Regsubsets")
          )
        ),
        fluidRow(
          column(12,
                 h3(helpText("Type of Metadata"), align = TRUE),
                 
                 a(h4('1. rsq: The r-squared for each model'), href = 'https://en.wikipedia.org/wiki/Coefficient_of_determination'),
                 a(h4('2. rss  : Residual sum of squares for each model'), href = 'https://en.wikipedia.org/wiki/Residual_sum_of_squares'),
                 a(h4('3. adjr2: Adjusted r-squared'), href = 'https://en.wikipedia.org/wiki/Coefficient_of_determination'),
                 a(h4("4. cp   : Mallows' Cp"), href = 'https://en.wikipedia.org/wiki/Mallows%27s_Cp'),
                 a(h4("5. bic  : Schwartz's information criterion"), href = 'https://en.wikipedia.org/wiki/Bayesian_information_criterion')
                 
          
                 # img(src = ("metadataType.png"), width = "100%")
          )
        )
      )
    })
    
    output$Regsubsets <- DT::renderDataTable({
      Regsubset <- Regsubset()

      a <- coef(Regsubset, 1:nrow(summary(Regsubset)$which))
      a <- ldply(a, rbind)

      colnames(a)[1] <- "Intercept"
      row.names(a) <- 1:nrow(a)
      a <- round(a, 4)
      AllModel <- datatable(data.frame("模型編號" = 1:nrow(a), a),
                            options = list(scrollX = TRUE, scrollCollapse = TRUE))
      
      AllModel
    })

    # 呈現超數據的類型
    # "rsq","rss","adjr2","cp","bic"
    output$MetaReg <- renderPrint({
      Regsubset <- Regsubset()
      names(summary(Regsubset))[c(2,3,4,5,6)]
    })

  })

  # 所有模型超數據的大表
  MetadataTable <- reactive({
    Regsubset <- Regsubset()
    Summary <- summary(Regsubset)
    d <- data.frame("rsq" = Summary$rsq,
                    "rss" = Summary$rss,
                    "adjr2" = Summary$adjr2,
                    "cp" = Summary$cp,
                    "bic" = Summary$bic)
    round(d, 4)
  })

  # step3 蒐集模型超數據
  observe({

    # 蒐集模型超數據
    output$Mstep3 <- renderUI({
      wellPanel(
        fluidRow(
          column(12,
                 h3(helpText("Original Metadata")),
                 DT::dataTableOutput("oriMetaRegTable"),
                 br(),

                 h3(helpText("Standardize Metadata")),
                 DT::dataTableOutput("MetaRegTable")
          )
        )
      )
    })

    # 所有模型超數據的大表
    output$oriMetaRegTable <- DT::renderDataTable({
      d <- MetadataTable()
      
      datatable(d, extensions = 'Scroller',
                options = list(
                  deferRender = TRUE,
                  dom = "frtiS",
                  scrollY = 500,
                  scrollCollapse = FALSE
                )) %>%
        formatStyle(
          'rsq',
          backgroundColor = styleInterval(c(.5,.999), c('', 'yellow', ''))
        ) %>%
        formatStyle(
          'rss',
          backgroundColor = styleInterval(min(d$rss), c('yellow', ''))
        ) %>%
        formatStyle(
          'adjr2',
          backgroundColor = styleInterval(c(.5,.999), c('', 'yellow', ''))
        ) %>%
        formatStyle(
          'bic',
          backgroundColor = styleInterval(min(d$bic), c('yellow', ''))
        )

    })


    # 所有標準化後模型超數據的大表
    output$MetaRegTable <- DT::renderDataTable({
      d <- MetadataTable()
      d <- as.data.frame(scale(d))
      d <- round(d, 4)
      datatable(d, extensions = 'Scroller',
                options = list(
                  deferRender = TRUE,
                  dom = "frtiS",
                  scrollY = 500,
                  scrollCollapse = FALSE
                )) %>%
        formatStyle(
          'rsq',
          backgroundColor = styleInterval(c(.5,.999), c('', 'yellow', ''))
        ) %>%
        formatStyle(
          'rss',
          backgroundColor = styleInterval(min(d$rss), c('yellow', ''))
        ) %>%
        formatStyle(
          'adjr2',
          backgroundColor = styleInterval(c(.5,.999), c('', 'yellow', ''))
        ) %>%
        formatStyle(
          'bic',
          backgroundColor = styleInterval(min(d$bic), c('yellow', ''))
        )

    })

  })

  DOXMatrix <- reactive({
    fac.design(nfactors = 5,
               replications = 1,
               repeat.only = FALSE,
               blocks = 1,
               randomize = FALSE,
               seed = 6860,
               nlevels = rep(2,5),
               factor.names = list("rsq" = c(TRUE, FALSE),
                                   "rss" = c(TRUE, FALSE),
                                   "adjr2" = c(TRUE, FALSE),
                                   "cp" = c(TRUE, FALSE),
                                   "bic" = c(TRUE, FALSE)))
  })

  # step4 超數據實驗設計
  observe({
    # 超數據實驗設計
    output$Mstep4 <- renderUI({
      wellPanel(
        fluidRow(
          column(6,
                 verbatimTextOutput("DesignMatrix")
          ),
          column(6,
                 sliderInput(inputId = "whichDesign", label = "選擇第幾個設計",
                             min = 1, max = 32, value = 1),
                 h3(helpText("實驗設計說明", align = "center")),
                 helpText("共有5種模型超數據(rsq、rss、adjr2、cp、bic)，
                          每種超數據都有「放進去」以及「不放進去」兩種可能，
                          所以建立了2^5的因子設計矩陣，共有32種可能的實驗。")


                 )
          )
      )
    })

    # 2^5所有可能實驗的設計矩陣
    output$DesignMatrix <- renderPrint({
      DOXMatrix()
    })


  })

  # step5 群集分析
  observe({
    output$Mstep5 <- renderUI({
      wellPanel(
        tabsetPanel(
                    tabPanel("Distance Matrix",
                             fluidRow(
                               column(12,
                                      h3(helpText(" Distance Matrix(euclidean)")),
                                      verbatimTextOutput("DistMatrix"),
                                      br()
                               )
                             )
                    ),
                    tabPanel("Kmeans",
                             fluidRow(
                               h3(helpText(" Kmeans分群結果")),
                               DT::dataTableOutput("clusterTable"),
                               br(),
                               sliderInput(inputId = "clusterNo", label = "分成幾群",
                                           min = 1, max = 10, value = 2),
                               verbatimTextOutput("KmeansOutput"),
                               br(),

                               h3(helpText(" Cluster Plot")),
                               plotOutput("clusPlot"),
                               br()

                             )
                    )
        )
      )
    })

    output$DistMatrix <- renderPrint({
      d <- MetadataTable()
      D <- DOXMatrix()
      d <- d[,which(D[input$whichDesign,] == "TRUE")]

      d <- as.data.frame(scale(d))
      mydist <- dist(d, method = "euclidean")
      mydist
    })

    output$KmeansOutput <- renderPrint({
      d <- MetadataTable()
      D <- DOXMatrix()
      d <- d[,which(D[input$whichDesign,] == "TRUE")]

      d <- as.data.frame(scale(d))
      mydist <- dist(d, method = "euclidean")
      set.seed(6666)
      kmeans(mydist, input$clusterNo)
    })

    output$clusPlot <- renderPlot({
      d <- MetadataTable()
      D <- DOXMatrix()
      d <- d[,which(D[input$whichDesign,] == "TRUE")]

      d <- as.data.frame(scale(d))
      mydist <- dist(d, method = "euclidean")
      set.seed(6666)
      fit <- kmeans(mydist, input$clusterNo)

      clusplot(d, fit$cluster, color = TRUE, shade = TRUE,
               labels = 2, lines = 0)
    })

    output$clusterTable <- DT::renderDataTable({
      d <- MetadataTable()
      D <- DOXMatrix()
      d <- d[,which(D[input$whichDesign,] == "TRUE")]
      # d <- as.data.frame(scale(d))
      mydist <- dist(d, method = "euclidean")
      set.seed(6666)
      fit <- kmeans(mydist, input$clusterNo)

      d$Group <- fit$cluster
      d <- d[order(d$Group),]
      d <- data.frame("模型編號" = row.names(d), d)

      # 加上模型細節(使用變數)
      Regsubset <- Regsubset()
      a <- summary(Regsubset)$which

      a[which(!a)] <- ""
      colnames(a)[1] <- "Intercept"
      d <- data.frame(d, a)
      ####################

      datatable(d, extensions = 'Scroller', options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE
      ))#%>%
      #formatStyle(
      #  'bic',
      #  backgroundColor = styleInterval(c(.5,.999), c('', 'yellow', ''))
      #)

    })


  })

  PE <- reactive({
    OzDASLdata <- OzDASLdata()
    RegSubsets <- Regsubset()
    d <- MetadataTable()
    q <- summary(RegSubsets)$which

    D <- DOXMatrix()
    d <- d[,which(D[input$whichDesign,] == "TRUE")]

    d <- as.data.frame(scale(d))
    mydist <- dist(d, method = "euclidean")
    set.seed(6666)
    Kmean <- kmeans(mydist, input$clusterNo)

    PE <- matrix(0, input$resampleTimes, nrow(q))

    for (Boot in 1:input$resampleTimes) {
      testing <- OzDASLdata[sample(nrow(OzDASLdata), nrow(OzDASLdata), replace = TRUE),]

      for(i in 1:nrow(q)) {
        w <- lapply(names(which(q[i,]))[-1], function(x) with(testing, get(x)))

        X <- matrix(1, nrow = length(w[[1]]), ncol = 1)
        w <- cbind(X, matrix(unlist(w), nrow = length(w[[1]]), ncol = length(w)))
        colnames(w) <- names(which(q[i,]))

        # 係數
        y <- coef(RegSubsets, 1:nrow(summary(RegSubsets)$which))
        coefficient <- ldply(y, rbind)

        # coefficient[is.na(coefficient)] <- 0

        new <- w %*% as.matrix(coefficient)[i,][!is.na(as.matrix(coefficient)[i,])]

        # 計算預測誤差
        yhat <- apply(new, 1, sum)
        y <- testing[, input$varY]
        PE[Boot,i] <- sum((y-yhat)^2)
      }
    }
    colnames(PE) <- Kmean$cluster
    PE
    # 計算10次再抽樣後預測誤差的平均(MPE)
    # MPETable <- aggregate(t(t(apply(PE, 2, mean))), list(row.names(t(t(apply(PE, 2, mean))))), mean)
    # colnames(MPETable) <- c("Group", "MPE")
    # MPETable <- as.data.frame(MPETable)
  })

  # step6 實證
  observe({
    output$Mstep6 <- renderUI({
      wellPanel(
        tabsetPanel(
                    tabPanel("第一階段",
                             fluidRow(
                               column(12,
                                      # helpText("實證"),
                                      numericInput(inputId = "resampleTimes", label = "再抽樣次數", value = 10),
                                      br(),

                                      h3(helpText("預測誤差平均的結果")),
                                      DT::dataTableOutput("MPETable"),
                                      br(),

                                      h3(helpText("再抽樣的預測誤差")),
                                      DT::dataTableOutput("PETable")
                               )
                             )
                    ),
                    tabPanel("第二階段",
                             fluidRow(
                               column(12,
                                      h3(helpText("選擇變數長度較短的模型")),
                                      DT::dataTableOutput("simplifyModel")
                               )
                             )
                    ),
                    tabPanel("第三階段",
                             fluidRow(
                               column(12,
                                      h3(helpText("最終模型")),
                                      verbatimTextOutput("lastModelFormula"),

                                      br(),
                                      h3(helpText("最簡模型的平均預測誤差")),
                                      verbatimTextOutput("lastModelPE"),

                                      br(),
                                      h3(helpText("最簡模型的平均預測誤差")),
                                      DT::dataTableOutput("lastModel")
                               )
                             )
                    )
        )

      )
    })
  })

  output$MPETable <- DT::renderDataTable({
    PE <- PE()

    # 計算10次再抽樣後預測誤差的平均(MPE)
    MPETable <- aggregate(t(t(apply(PE, 2, mean))), list(row.names(t(t(apply(PE, 2, mean))))), mean)
    colnames(MPETable) <- c("Group", "MPE")
    MPETable <- as.data.frame(MPETable)
    
    for(i in 1:length(MPETable$Group)) {
      MPETable$模型編號[i] <- paste0(which(MPETable$Group[i] == colnames(PE)), collapse = ",")
    }

    # MPETable

    datatable(MPETable, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      dom = "frtiS",
      scrollY = 350,
      scrollCollapse = FALSE
    ))

  })

  output$PETable <- DT::renderDataTable({
    PE <- PE()

    # PE <- rbind(PE, apply(PE, 2, mean))
    row.names(PE) <- 1:nrow(PE)
    PE <- round(PE, 4)

    datatable(PE, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      dom = "frtiS",
      scrollY = 350,
      scrollCollapse = FALSE
    ))
  })

  output$simplifyModel <- DT::renderDataTable({
    PE <- PE()
    Regsubset <- Regsubset()

    # 計算10次再抽樣後預測誤差的平均(MPE)
    MPETable <- aggregate(t(t(apply(PE, 2, mean))), list(row.names(t(t(apply(PE, 2, mean))))), mean)
    colnames(MPETable) <- c("Group", "MPE")
    MPETable <- as.data.frame(MPETable)
    MPETable

    for(i in 1:length(MPETable$Group)) {
      MPETable$模型編號[i] <- paste0(which(MPETable$Group[i] == colnames(PE)), collapse = ",")
    }

    # 預測誤差較小的那群的模型
    selectG <- unlist(strsplit(MPETable[which(MPETable$MPE == min(MPETable$MPE)), "模型編號"], split = ','))


    # 所有可能的模型長相
    d <- MetadataTable()
    d <- as.data.frame(scale(d))

    mydist <- dist(d, method = "euclidean")
    set.seed(6666)
    fit <- kmeans(mydist, input$clusterNo)

    d$Group <- fit$cluster
    d <- d[order(d$Group),]
    d <- data.frame("模型編號" = row.names(d), d)

    # 所有可能的模型長相
    # 加上模型細節(使用變數)
    a <- summary(Regsubset)$which
    colnames(a)[1] <- "Intercept"
    a <- data.frame(d, a)

    m <- a[which(a$模型編號 %in% selectG),]
    m <- data.frame(m, "變數個數" = apply(m[,-c(1:8)], 1, sum))
    # length(m)

    m <- m[,c(1,7:length(m))]


    datatable(m,
              options = list(
                scrollX = TRUE,
                scrollCollapse = TRUE
              ))
  })

  output$lastModelFormula <- renderPrint({
    PE <- PE()
    Regsubset <- Regsubset()

    # 計算10次再抽樣後預測誤差的平均(MPE)
    MPETable <- aggregate(t(t(apply(PE, 2, mean))), list(row.names(t(t(apply(PE, 2, mean))))), mean)
    colnames(MPETable) <- c("Group", "MPE")
    MPETable <- as.data.frame(MPETable)
    MPETable

    for(i in 1:length(MPETable$Group)) {
      MPETable$模型編號[i] <- paste0(which(MPETable$Group[i] == colnames(PE)), collapse = ",")
    }

    # 預測誤差較小的那群的模型
    selectG <- unlist(strsplit(MPETable[which(MPETable$MPE == min(MPETable$MPE)), "模型編號"], split = ','))

    # 所有可能的模型長相
    d <- MetadataTable()
    d <- as.data.frame(scale(d))

    mydist <- dist(d, method = "euclidean")
    set.seed(6666)
    fit <- kmeans(mydist, input$clusterNo)

    d$Group <- fit$cluster
    d <- d[order(d$Group),]
    d <- data.frame("模型編號" = row.names(d), d)

    # 所有可能的模型長相
    # 加上模型細節(使用變數)
    a <- summary(Regsubset)$which
    colnames(a)[1] <- "Intercept"
    a <- data.frame(d, a)


    m <- a[which(a$模型編號 %in% selectG),]
    m <- data.frame(m, "變數個數" = apply(m[,-c(1:8)], 1, sum))
    # length(m)

    m <- m[,c(1,7:length(m))]


    colnames(PE) <- 1:ncol(PE)
    row.names(PE) <- 1:nrow(PE)

    n <- PE[,which(colnames(PE) %in% m[which(m$變數個數 == min(m$變數個數)),"模型編號"])]

    test <- apply(n, 2, mean)
    names(test)[which(test == min(test))]

    lastModelT <- m[which(m$模型編號 == names(test)[which(test == min(test))]), -ncol(m)]

    a <- lastModelT[, 3:ncol(lastModelT)]

    c(paste0(input$varY, " ~ ", paste(names(a)[which(a == "TRUE")], collapse = " + ")), min(test))



  })

  output$lastModelPE <- renderPrint({
    PE <- PE()
    Regsubset <- Regsubset()

    # 計算10次再抽樣後預測誤差的平均(MPE)
    MPETable <- aggregate(t(t(apply(PE, 2, mean))), list(row.names(t(t(apply(PE, 2, mean))))), mean)
    colnames(MPETable) <- c("Group", "MPE")
    MPETable <- as.data.frame(MPETable)
    MPETable

    for(i in 1:length(MPETable$Group)) {
      MPETable$模型編號[i] <- paste0(which(MPETable$Group[i] == colnames(PE)), collapse = ",")
    }

    # 預測誤差較小的那群的模型
    selectG <- unlist(strsplit(MPETable[which(MPETable$MPE == min(MPETable$MPE)), "模型編號"], split = ','))

    # 所有可能的模型長相
    d <- MetadataTable()
    d <- as.data.frame(scale(d))

    mydist <- dist(d, method = "euclidean")
    set.seed(6666)
    fit <- kmeans(mydist, input$clusterNo)

    d$Group <- fit$cluster
    d <- d[order(d$Group),]
    d <- data.frame("模型編號" = row.names(d), d)

    # 所有可能的模型長相
    # 加上模型細節(使用變數)
    a <- summary(Regsubset)$which
    colnames(a)[1] <- "Intercept"
    a <- data.frame(d, a)


    m <- a[which(a$模型編號 %in% selectG),]
    m <- data.frame(m, "變數個數" = apply(m[,-c(1:8)], 1, sum))
    # length(m)

    m <- m[,c(1,7:length(m))]


    colnames(PE) <- 1:ncol(PE)
    row.names(PE) <- 1:nrow(PE)

    m <- PE[,which(colnames(PE) %in% m[which(m$變數個數 == min(m$變數個數)),"模型編號"])]

    apply(m, 2, mean)

  })

  output$lastModel <- DT::renderDataTable({
    PE <- PE()
    Regsubset <- Regsubset()

    # 計算10次再抽樣後預測誤差的平均(MPE)
    MPETable <- aggregate(t(t(apply(PE, 2, mean))), list(row.names(t(t(apply(PE, 2, mean))))), mean)
    colnames(MPETable) <- c("Group", "MPE")
    MPETable <- as.data.frame(MPETable)
    MPETable

    for(i in 1:length(MPETable$Group)) {
      MPETable$模型編號[i] <- paste0(which(MPETable$Group[i] == colnames(PE)), collapse = ",")
    }

    # 預測誤差較小的那群的模型
    selectG <- unlist(strsplit(MPETable[which(MPETable$MPE == min(MPETable$MPE)), "模型編號"], split = ','))

    # 所有可能的模型長相
    d <- MetadataTable()
    d <- as.data.frame(scale(d))

    mydist <- dist(d, method = "euclidean")
    set.seed(6666)
    fit <- kmeans(mydist, input$clusterNo)

    d$Group <- fit$cluster
    d <- d[order(d$Group),]
    d <- data.frame("模型編號" = row.names(d), d)

    # 所有可能的模型長相
    # 加上模型細節(使用變數)
    a <- summary(Regsubset)$which
    colnames(a)[1] <- "Intercept"
    a <- data.frame(d, a)


    m <- a[which(a$模型編號 %in% selectG),]
    m <- data.frame(m, "變數個數" = apply(m[,-c(1:8)], 1, sum))
    # length(m)

    m <- m[,c(1,7:length(m))]


    colnames(PE) <- 1:ncol(PE)
    row.names(PE) <- 1:nrow(PE)

    m <- PE[,which(colnames(PE) %in% m[which(m$變數個數 == min(m$變數個數)),"模型編號"])]

    m <- round(m, 4)

    datatable(m,
              options = list(
                scrollX = TRUE,
                scrollCollapse = TRUE
              ))

  })
  
  
})