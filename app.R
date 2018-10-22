library(shiny)
library(vegan)
library(labdsv)
library(MASS)
library(ggplot2)
library(ggbiplot)
library(graphics)
library(readr)
library(cluster)
library(DT)
library(dygraphs)
library(mclust)
library(pca3d)
library(vegan3d)
library(Cairo)
library(grDevices)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(logging)
library(shinyjs)

#basicConfig()
#options(shiny.error = function() {logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })

####Global####

distance <- c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "morisita", "horn", "mountford", "raup" , "binomial", "chao")
stand <- c("pa", "total", "max", "freq", "hellinger", "log", "chi", "range", "normalize", "standardize")

####UI Start####

ui <- shinyUI(
  dashboardPagePlus(
    #useShinyjs(),
    
    header = dashboardHeaderPlus(title = span(tagList(icon("grain" , lib = "glyphicon"), "VEGDESK")), enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
    
    ####LeftSideBar####
    
    sidebar =  
      dashboardSidebar( 
        tags$head(
          tags$style(HTML("
                        .sidebar { height: 90vh; overflow-y: auto; }
                        " )
          )
        ),
        sidebarMenu(
          menuItem("Data", icon = icon("database"), tabName = "Data"),
          menuItem("Explore", tabName = "Explore", icon = icon("dashboard")),
          menuItem("3-D Mode", tabName = "3D", icon = icon("linode")),
          menuItem("Metrics", tabName = "Metrics", icon = icon("th")),
          menuItem("Environmental", tabName = "env", icon = icon("leaf")),
          menuItem("Info", tabName = "Info", icon = icon("info-circle"),
                   menuSubItem("VEGDESK Guide", tabName = "guide", icon = icon("book")),
                   menuSubItem("Supplementary Information", tabName = "supp", icon = icon("university")),
                   menuSubItem("Contact & Support", tabName = "contact", icon = icon("thumbs-up")))
        )
      ),
    
    ####RightSideBar####
    
    rightsidebar = 
      rightSidebar(background = "dark",
                   rightSidebarTabContent(
                     id = 1,
                     title = "Data Transformation",
                     active = TRUE,
                     icon = NULL,
                     tags$hr(),
                     selectInput("Distance","Select dissimilartity/distance:", choices = distance,
                                 selected="kulczynski"),
                     checkboxInput("nonstandardised", "Apply Standardisation to Species Matrix"),
                     conditionalPanel(
                       condition = "input.nonstandardised == true",
                       selectInput("Standardisation", "Select Standardisation Method:", choices = stand,
                                   selected="pa"))
                   ),
                   
                   rightSidebarTabContent(
                     id = 2,
                     active = TRUE,
                     icon = "wrench",
                     title = "Clustering",
                     tags$hr(),
                     sliderInput("beta", "Flexible Beta:",min = -1, max = 1, value = -.25, step= 0.001),
                     numericInput("group", "Number of Clusters:", value=7, min=1, max=Inf, step=1)
                   ),
                   
                   rightSidebarTabContent(
                     id = 3,
                     active = TRUE,
                     icon = NULL,
                     title = "Plot Options",
                     tags$hr(),
                     checkboxInput("smooth", "Use Anisotropic Smoothing of Environmental Variables"),
                     conditionalPanel(condition = "input.smooth == true",
                                      sliderInput("knot", "Number of knots/degrees of freedom",min = 0, max = 10, value = 1, step= 1),
                                      selectInput("wat", "What", list("contour","persp","gam")),
                                      selectInput("smoothingMethod", "Method",list("GACV.Cp", "GCV.Cp", "REML", "P-REML", "ML", "P-ML")),
                                      selectInput("smoothingBasis", "Smoothing Basis", list("tp", "ts", "cr", "cs", "ds", "ps", "ad"))),
                     
                     checkboxInput("editpdf", "Edit PDF Plot Parameters"),
                     conditionalPanel(condition = "input.editpdf == true",
                                      numericInput("pointSize", "Point Size", value= 12, min=1, max=Inf, step=1),
                                      numericInput("long", "Length", value= 15, min=1, max=Inf, step=.1),
                                      numericInput("wide", "Width", value= 10, min=1, max=Inf, step=.1)))
      ),
    
    ####Body####
    
    body = dashboardBody(
      #tags$head(tags$script(HTML("window.onload = function() {resize();}window.onresize = function() {resize();}Shiny.addCustomMessageHandler ('triggerResize',function (val) {window.dispatchEvent(new Event('resize'));});function resize(){var h = window.innerHeight - $('.navbar').height() - 150; // Get dashboardBody height$('#box').height(h);}"))),
      tabItems(
        tabItem(tabName = "Data",
                
                fluidRow(
                  box(
                    title = "Data Upload", width = 6, status = "primary",
                    fileInput("file1", "Upload Species Data (.csv)",
                              multiple = F,
                              accept = ".csv"),
                    fileInput("file2", "Upload Environmental Data (.csv)",
                              multiple = F,
                              accept = ".csv"),
                    downloadButton("downloadData", label = "Download Example Data")
                  ),
                  box(
                    title = "Select Environmental Variables", width = 6, status = "success",
                    helpText(paste("Environmental variables to plot:")),
                    uiOutput("checkbox1"),
                    uiOutput("checkbox2")
                  )
                ),
                
                fluidRow(
                  column(width = 12,
                         box(
                           title = "Species Data", width = NULL, solidHeader = TRUE, status = "warning",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
                         ),
                         box(
                           title = "Environmental Data", width = NULL, solidHeader = TRUE, status = "success",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput('table2'))                       )
                  )
                )
        ),
        
        tabItem(tabName = "Metrics",
                fluidRow(
                  box(title = "Distance/Dissimilarity Metrics",width = 12, status = "info", collapsible = T, verbatimTextOutput("metrics"))
                ),
                fluidRow(
                  box(title = "Indicator Species Analysis", width = 12, status = "warning", collapsible = T, verbatimTextOutput("ind"))
                ),
                fluidRow(
                  tabBox(id = "diverse",
                         tabPanel(title = "Shannon's Diversity", DT::dataTableOutput("shannon")
                         ),
                         tabPanel(title = "Simpson's Diversity", DT::dataTableOutput("simpson")
                         ))
                )),      
        
        #tabItem(tabName = "Info"
        #),      
        
        tabItem(tabName = "env",
                fluidRow(
                  box(title = paste("Group Boxplot - Variable 1"), width = 12, status = "success", collapsible = T, plotOutput("envboxplot1"))
                ),
                fluidRow(
                  box(title = paste("Group Boxplot - Variable 2"), width = 12, status = "warning", collapsible = T, plotOutput("envboxplot2"))
                ),
                fluidRow(
                  box(title = "ANOSIM Plot", width = 6, status = "info", collapsible = T, plotOutput("anosimplot"))
                ),
                fluidRow(
                  box(title = "ANOSIM Summary", width = 6, status = "info", collapsible = T, verbatimTextOutput("anosimsum"))
                ),
                fluidRow(
                  tabBox(id="adonis", title = "ADONIS PERMANOVA", width = 12,
                  tabPanel(title = "Between Groups", verbatimTextOutput("permanovabetween")),
                  tabPanel(title = "Within Groups", verbatimTextOutput("permanovawithin")),
                  tabPanel(title = "Without Groups", verbatimTextOutput("permanovawithout"))
                )
                )
        ),
        
        tabItem(tabName = "3D",
                fluidRow(
                  box(title = "Dendrogram Ordination",width = 12, status = "info", collapsible = T, plotOutput("ThreeDend"))
                ),
                fluidRow(
                  box(title = "NM3DS", width = 12, status = "warning", collapsible = T,  plotOutput("ThreeDim"))
                ),
                fluidRow(
                  box(title = "Interactive 3D-NMDS", width = 12, status = "primary", collapsible = T, collapsed = T, plotOutput("ThreeDNMS"))
                )
        ),       
        
        tabItem(tabName = "Explore",
                fluidRow(
                  box(
                    title = "NMS Ordination", width = 12, status = "primary", collapsible = TRUE,
                    div(style = 'overflow-x: scroll',
                        plotOutput("ordplot", height = "1000px"),
                        downloadLink("downloadPlot", "Download Plot"))
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Dendrogram", width = 12, status = "warning", collapsible = TRUE,
                    div(style = 'overflow-x: scroll',
                        plotOutput("dendrogram")
                    )
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Shepard's Plot", width = 6, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                    plotOutput("sheplot")
                  ),
                  box(
                    title="Fuzzy Set Ordination", width = 6, background = "black", collapsible = TRUE,
                    plotOutput("fuzz")
                  )
                ),
                
                fluidRow(width = 6,
                         box(
                           title = "Silhouette Plot", width = 8, solidHeader = TRUE, status = "warning", collapsible = TRUE,
                           plotOutput("silplot", height = "600px")
                         ),
                         box(
                           title = "Mclust", width = 4, background = "teal", collapsible = TRUE,
                           plotOutput("mclust", height = "600px")
                         )
                ),
                
                fluidRow(
                  box(
                    title = "Cophenetic Correlation", width = 4, background = "green", collapsible = TRUE,
                    plotOutput("coph")
                  ),
                  box(
                    title = "Goodness of Fit", width = 8, solidHeader = TRUE, collapsible = TRUE, background = "yellow",
                    plotOutput("gofplot"))
                ),
                
                
                fluidRow(
                  box(
                    title = "Species Accumulation Curve", width = 6, solidHeader = TRUE, collapsible = TRUE, background = "aqua",
                    plotOutput("sac")),
                  box(
                    title = "Calinski Criterion", width = 6, background = "maroon", collapsible = TRUE,
                    plotOutput("calinski"))
                ),
                
                fluidRow(
                  box(
                    title = "Partitioning Around Medoids (PAM)", width = 12, solidHeader = TRUE, collapsible = TRUE,
                    div(style = 'overflow-x: scroll', plotOutput("demopam", height = "1000px")) 
                    
                    #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                    
                  )
                )
        )
      )
    )
  )
)

####Server####
server <- function(input, output, session){
  
  options(shiny.usecairo=T)
  #options(shiny.reactlog=TRUE)
  #options(shiny.error=browser)
  
  ####Sample Data Upload####
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("output", "zip", sep=".")
    },
    content <- function(file) {
      file.copy("out.zip", file)
    },
    contentType = "application/zip")
  
  Species_Matrix <- reactive({
    shiny::validate(need(input$file1, "Upload an Species Matrix!"))
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  Environmental_Matrix <- reactive({
    shiny::validate(need(input$file2, "Upload an Environmnetal Matrix!"))
    infile <- input$file2
    if (is.null(infile))
      # User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
      return(NULL)
    req(input$file2)
    read.csv(infile$datapath)
    #temp[order(temp[, 1]),]
  })
  
  FlexBeta <- reactive({1-(input$beta)})
  
  ####Data Tables####
  
  output$table <- renderDT(Species_Matrix())
  output$table2 <- renderDT(Environmental_Matrix())
  
  output$checkbox1 <- renderUI({
    selectInput(inputId = "var1", 
                label = "Select Environmental Variable 1",
                choices = colnames(Environmental_Matrix())
    )
  })
  
  output$checkbox2 <- renderUI({
    selectInput(inputId = "var2", 
                label = "Select Environmental Variable 2", 
                choices = colnames(Environmental_Matrix())
    )
  })
  
  ####Species Accumulation Curve####
  
  output$shannon <- renderUI({
    Species_Matrix <- Species_Matrix()
    as.data.frame(diversity(Species_Matrix, index = "shannon"))
  })
  
  output$simpson <- renderUI({
    Species_Matrix <- Species_Matrix()
    as.data.frame(diversity(Species_Matrix, index = "simpson"))
  })
  
  output$permanovabetween <- renderPrint({
    Species_Matrix <- Species_Matrix()
    Environmental_Matrix <- Environmental_Matrix()
    FlexBeta <- FlexBeta()
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    adonis(Species_Matrix ~ Environmental_Matrix[[input$var1]]*Environmental_Matrix[[input$var2]], strata=spe.bw.groups, permutations=999)
  })
  
  output$permanovawithin <- renderPrint({
    Species_Matrix <- Species_Matrix()
    Environmental_Matrix <- Environmental_Matrix()
    FlexBeta <- FlexBeta()
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    adonis(Species_Matrix ~ Environmental_Matrix[[input$var1]]*Environmental_Matrix[[input$var2]], permutations=999)
  })
  
  output$permanovawithout <- renderPrint({
    Species_Matrix <- Species_Matrix()
    Environmental_Matrix <- Environmental_Matrix()
    FlexBeta <- FlexBeta()
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    adonis(Species_Matrix ~ spe.bw.groups, permutations=999)
  })
  
  output$anosimplot <- renderPlot({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    ano <- anosim(Species_Matrix, spe.bw.groups, distance = input$distance)
    plot(ano, col=(1+c(1:length(grp.lev))), main= "ANOSIM", cex.axis=0.99)
  })
  
  output$anosimsum <- renderPrint({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    ano <- anosim(Species_Matrix, spe.bw.groups, distance = input$distance)
    summary(ano)
  })
  
  output$sac <- renderPlot({
    Species_Matrix <- Species_Matrix()
    sac <- specaccum(Species_Matrix)
    plot(sac, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main="Species Accumulation Curve", ylab="Species")
  })
  
  output$envboxplot1 <-
    renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      Environmental_Matrix <- Environmental_Matrix()
      if (input$nonstandardised){
        bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
      } else {
        bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
      }
      demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      if (input$nonstandardised){
        bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      } else {
        bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      }
      spe.bray.ward <- demoflex.hcl
      spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
      grp.lev <- levels(factor(spe.bw.groups))
      sit.sc <- scores(bci.mds)
      p1 <- ggplot(aes(y = Environmental_Matrix[[input$var1]], x = factor(spe.bw.groups)), data = Environmental_Matrix, fill= factor(spe.bw.groups)) + geom_boxplot(col=1+c(1:length(grp.lev))) + stat_summary(fun.y = mean, geom="point",colour="black", size=2)
      p1 + ggtitle(paste0(input$var1), "by Group") + xlab("Group") + ylab(paste0(input$var1))
      p1
    })
  
  output$envboxplot2 <-
    renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      Environmental_Matrix <- Environmental_Matrix()
      if (input$nonstandardised){
        bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
      } else {
        bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
      }
      demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      if (input$nonstandardised){
        bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      } else {
        bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      }
      spe.bray.ward <- demoflex.hcl
      spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
      grp.lev <- levels(factor(spe.bw.groups))
      sit.sc <- scores(bci.mds)
      p2 <- ggplot(aes(y = Environmental_Matrix[[input$var2]], x = factor(spe.bw.groups)), data = Environmental_Matrix, fill= factor(spe.bw.groups)) + geom_boxplot(col=1+c(1:length(grp.lev))) + stat_summary(fun.y = mean, geom="point",colour="black", size=2)
      p2 + ggtitle(paste0(input$var2), "by Group") + xlab("Group") + ylab(paste0(input$var2))
      p2
    })
  
  output$dendrogram <-
    renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      Environmental_Matrix <- Environmental_Matrix()
      if (input$nonstandardised){
        bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
      } else {
        bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
      }
      demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      if (input$nonstandardised){
        bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      } else {
        bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      }
      spe.bray.ward <- demoflex.hcl
      spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
      grp.lev <- levels(factor(spe.bw.groups))
      sit.sc <- scores(bci.mds)
      siteind<-as.data.frame(cbind(spe.bw.groups))
      siteind<-as.character(siteind)
      dend<-as.dendrogram(demoflex.hcl)
      labels_colors(dend) <- c(as.numeric(siteind) + 1)
      plot(dend, ylab = "Height", xlab="Site", cex=0.7)
      rect.hclust(demoflex.hcl, k= input$group, border = 1+c(1:length(grp.lev)))
    }
    )
  
  
  ####NMS Ordination####
  
  output$ordplot <-
    renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      Environmental_Matrix <- Environmental_Matrix()
      if (input$nonstandardised){
        bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
      } else {
        bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
      }
      demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      if (input$nonstandardised){
        bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      } else {
        bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      }
      spe.bray.ward <- demoflex.hcl
      spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
      grp.lev <- levels(factor(spe.bw.groups))
      sit.sc <- scores(bci.mds)
      p5 <- ordiplot(sit.sc, type="n", main="NMS Ordination", display = 'si', ylim=c(-2.3,2.3), xlim=c(-2.2,2.2),xlab="",ylab="")
      title(ylab="NMDS 2", line=2.5, cex.lab=0.7, xlab="NMDS 1")
      ###Environment
      ef1 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
      plot(ef1, col = "red", cex=0.5, labels = paste0(input$var1))
      if (input$smooth){
        ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Environmental_Matrix, knots = input$knot, add = TRUE, isotropic = FALSE, what = input$wat, method = input$smoothingMethod, bs = input$smoothingBasis)
      } else {
        ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Environmental_Matrix, knots = 1, add = TRUE)
      }
      ef2 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
      plot(ef2, col="green", cex=0.5, labels = paste0(input$var2))
      if (input$smooth){
        ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Environmental_Matrix, knots = input$knot, add = TRUE, col="green", isotropic = FALSE, what = input$wat, method = input$smoothingMethod, bs = input$smoothingBasis)
      } else {
        ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Environmental_Matrix, knots = 1, add = TRUE, col="green")
      }
      for (i in 1:length(grp.lev))
      {points(sit.sc[spe.bw.groups==i,], pch=(14+i), cex=2, col=i+1)}
      ordicluster(p5, spe.bray.ward, col="dark grey", lty=2)     # Add the dendrogram
      ### Add a legend interactively
      #legend(locator(1), paste("Group",c(1:length(grp.lev))), pch=14+c(1:length(grp.lev)), col=1+c(1:length(grp.lev)), pt.cex=0.75, cex = 0.5, ncol=3)
      points(bci.mds, display = "species", cex = 0.7, pch=21, col="red", bg="yellow")
      ordipointlabel(bci.mds, display = "species", add = TRUE, cex = 0.7, scaling = 8)
      ordihull (bci.mds, groups = spe.bw.groups, show.group = 1:input$group, col = c("red","green","blue","lightblue","pink","yellow","grey"), draw = 'polygon', label = T, lty=3, cex=0.4, alpha = 0.05)
    }, height = 1000, width = 1200)
  
  ####PDF####
  
  plotInput <- function(file){
    Species_Matrix <- Species_Matrix()
    Environmental_Matrix <- Environmental_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    sit.sc <- scores(bci.mds)
    p5 <- ordiplot(sit.sc, type="n", main="NMS Ordination", display = 'si', ylim=c(-2.3,2.3), xlim=c(-2.2,2.2),xlab="",ylab="")
    title(ylab="NMDS 2", line=2.5, cex.lab=0.7, xlab="NMDS 1")
    ###Environment
    ef1 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
    plot(ef1, col = "red", cex=0.5, labels = paste0(input$var1))
    if (input$smooth){
      ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Environmental_Matrix, knots = input$knot, add = TRUE, isotropic = FALSE, what = input$wat, method = input$smoothingMethod, bs = input$smoothingBasis)
    } else {
      ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Environmental_Matrix, knots = 1, add = TRUE)
    }
    ef2 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
    plot(ef2, col="green", cex=0.5, labels = paste0(input$var2))
    if (input$smooth){
      ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Environmental_Matrix, knots = input$knot, add = TRUE, col="green", isotropic = FALSE, what = input$wat, method = input$smoothingMethod, bs = input$smoothingBasis)
    } else {
      ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Environmental_Matrix, knots = 1, add = TRUE, col="green")
    }
    for (i in 1:length(grp.lev))
    {points(sit.sc[spe.bw.groups==i,], pch=(14+i), cex=2, col=i+1)}
    ordicluster(p5, spe.bray.ward, col="dark grey", lty=2)     # Add the dendrogram
    #legend(locator(1), paste("Group",c(1:length(grp.lev))), pch=14+c(1:length(grp.lev)), col=1+c(1:length(grp.lev)), pt.cex=0.75, cex = 0.5, ncol=3)
    points(bci.mds, display = "species", cex = 0.7, pch=21, col="red", bg="yellow")
    ordipointlabel(bci.mds, display = "species", add = TRUE, cex = 0.7, scaling = 8)
    ordihull (bci.mds, groups = spe.bw.groups, show.group = 1:input$group, col = c("red","green", "blue","lightblue","pink","yellow","grey"), draw = 'polygon', label = F, lty=3,cex=0.4, alpha = 0.1)
    #dev.off()
  }
  
  ####Download Plot####
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$dataset, '.pdf', sep = '')},
    
    content = function(file){
      cairo_pdf(filename = file,
                width = input$wide, height = input$long, pointsize = input$pointSize, family = "sans", bg = "transparent",
                antialias = c("default", "none", "gray", "subpixel") ,fallback_resolution = 330)
      plotInput(file)
      dev.off()
    },
    
    contentType = "application/pdf"
    
    #Error in downloadHandler(filename = function() { : unused argument (outputOptions(output, "downloadPlot", suspendWhenHidden = FALSE))
    #outputOptions(output, "downloadPlot", suspendWhenHidden=FALSE)
  )
  
  ####Shepherd Plot####
  
  output$sheplot <- renderPlot({
    Species_Matrix <- Species_Matrix()
    bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    stress <- bci.mds$stress
    sheplot <- stressplot(bci.mds, main= paste("NMS Shepard Plot (Stress =", stress,")"))
  })
  
  ####Demopam####
  
  output$demopam <- renderPlot({
    Species_Matrix <- Species_Matrix()
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    demopam <- pam(bci.mds$points,k=input$group)
    par(mfrow=c(2,1))
    plot(demopam)
    par(mfrow=c(1,1))
  }, height = 1000, width = 1500)
  
  ####Fuzzy Set Ordination####
  
  output$fuzz <- renderPlot({
    Species_Matrix <- Species_Matrix()
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    cfuz <- fanny(bci.mds$points, input$group, memb.exp=1.5)
    ordiplot(bci.mds, dis="si", type="n")
    stars(cfuz$membership, locatio=bci.mds$points[,-3], draw.segm=TRUE, add=TRUE, scale=FALSE, len=0.1)
    ordihull(bci.mds, cfuz$clustering, col="blue")
  })
  
  ####Calinski Criterion####
  
  output$calinski <- renderPlot({
    Species_Matrix <- Species_Matrix()
    ccash <- cascadeKM(decostand(Species_Matrix, method = input$Standardisation),input$group, input$group+10)
    ccas <- cascadeKM(Species_Matrix,input$group, input$group+10)
    plot(ccash, sortq=TRUE)
  })
  
  ####Cophenetic Correlation####
  
  output$coph <- renderPlot({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    corr <- cor(bc,cophenetic(demoflex.hcl))
    plot(bc, cophenetic(demoflex.hcl), asp=1, main = paste("Cophentic Correlation =", corr), ylab=NULL)
    abline(0, 1)
  })
  
  ####Goodness of Fit#### 
  
  output$gofplot <- renderPlot({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    gof = goodness(bci.mds)
    plot(bci.mds, type="n", main="Site IDs and Associated Goodness of Fit") 
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k= input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    sit.sc <- scores(bci.mds)
    for (i in 1:length(grp.lev))
    {points(sit.sc[spe.bw.groups==i,], pch=(14+i), cex=gof*110, col=i+1)}
    text(bci.mds, display="sites", cex=0.6, pos=1)
  })
  
  ####Indicator Species####
  
  output$ind <- renderPrint({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    spe.sc <- wascores(Booterstown_Vegan_Transect_Matrix, bc)
    cl <- cutree(demoflex.hcl, k=input$group)
    const(Booterstown_Vegan_Transect_Matrix, cl)
    importance(Booterstown_Vegan_Transect_Matrix, cl)
    mod <- indval(Booterstown_Vegan_Transect_Matrix, as.numeric(cl))
    #names(mod)
    #mod$maxcls
    #mod$pval
    print(summary(mod))
    print(summary(mod, type = "long"))
  })
  
  ####Dissim-Metrics####
  output$metrics <- renderPrint({
    Species_Matrix <- Species_Matrix()
    rankindex(scale(decostand(Species_Matrix, method=input$Standardisation)), Species_Matrix, distance)
  })
  
  ####3DNMDS####
  
  output$ThreeDNMS <- renderUI({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    sit.sc <- scores(bci.mds)
    pca3d(sit.sc, group=spe.bw.groups,show.plane=TRUE,fancy=TRUE)
  })
  
  output$ThreeDend <- renderPlot({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    orditree3d(bci.mds, demoflex.hcl, display = "sites", type = "t", col = as.numeric(spe.bw.groups+1), choices = 1:2, cex=0.9)
  })
  
  output$ThreeDim <- renderPlot({
    Species_Matrix <- Species_Matrix()
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    ordiplot3d(bci.mds, type='h', main= "NMS in 3 dimensions")
  })
  
  ####MClust####
  
  output$mclust <- renderPlot({
    Species_Matrix <- Species_Matrix()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    # Run the function to see how many clusters it finds to be optimal, set it to search for at least 1 model and up 20.
    d_clust <- Mclust(as.matrix(bc), G=1:20)
    m.best <- dim(d_clust$z)[2]
    cat("model-based optimal number of clusters:", m.best, "\n")
    plot(d_clust, main= "Model-Based Optimal Cluster Number Assessment", what="BIC")
    title(main = paste("Model-Based Optimal Cluster Number Assessment:\n Generalised Suggestion =", m.best,"Groups" ))
    
  })
  
  ####Silhouette Plot####
  
  output$silplot <- renderPlot({
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    if (input$nonstandardised){
      bc <-vegdist(decostand(Species_Matrix, method=input$Standardisa, na.rm= F), method=input$Distance, binary=F)
    } else {
      bc <-vegdist(Species_Matrix, method=input$Distance, binary=F)
    }
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    if (input$nonstandardised){
      bci.mds<-metaMDS(decostand(Species_Matrix, method=input$Standardisation, na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    } else {
      bci.mds<-metaMDS(Species_Matrix, distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    }
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    spe.sc <- wascores(Booterstown_Vegan_Transect_Matrix, bc)
    cl <- cutree(demoflex.hcl, k=input$group)
    si <- silhouette (cl, bc)
    silo<-sortSilhouette(si)
    order<-as.data.frame(spe.bw.groups)
    order<-tibble::rownames_to_column(order)
    order <- order[order(order$spe.bw.groups),] 
    rownames(order)<-order$rowname
    plot(silo, col=order$spe.bw.groups+1, main = expression(paste("Silhouette plot of Group Membership - Flexible"~beta~"=", paste(FlexBeta()), "Dissimilarity/Distance = ", paste(input$Distance))), cex.names=0.8, nmax.lab=100)
  })
}

#printLogJs <- function(x, ...) {logjs(x)
#  T
#} addHandler(printLogJs)}
#showReactLog()
####APP####
shinyApp(ui = ui, server = server)
