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

#THIS ONE MAY SUCK

#runGitHub("VEGDESK", "marsnone" )

distance <- c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "morisita", "horn", "mountford", "raup" , "binomial", "chao")
stand <- c("NULL", "pa", "total", "max", "freq", "hellinger", "log", "chi", "range", "normalize", "standardize")

ui <- fluidPage(
  # App title ----
  titlePanel("Vegetation Description and Analysis (VEGDESK)"), # Application title
  # Sidebar layout with input and output definitions ---- 
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      ###File Input  ----
      fileInput("file1", "Upload Species Data (.csv)",
                multiple = F,
                accept = ".csv"),
      fileInput("file2", "Upload Environmental Data (.csv)",
                multiple = F,
                accept = ".csv"),
      
      # Horizontal line ----
      tags$hr(),
      
      helpText(paste("Select dissimilartity/distance:" )),
      fluidRow(
        selectInput("Distance", NULL, choices = distance,
                    selected="kulczynski")),
      fluidRow(
        sliderInput("beta", "Flexible Beta:",
                    min = -1, max = 1, value = -.25, step= 0.001)),
      helpText(paste("Number of Clusters:" )),
      fluidRow(
        numericInput("group", NULL, value=2, min=1, max=Inf, step=1)),
      helpText(paste("Environmental variables to plot:")),
      fluidRow(
        uiOutput("checkbox1")),
      fluidRow(
        uiOutput("checkbox2")),
      p("Martin O'Neill - 2018")
    ),
    
    
    # Main Panels
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput("table")),
        tabPanel("EnviroData", DT::dataTableOutput("table2")),
        tabPanel("NMS Ordination", plotOutput("ordplot"),
                 downloadLink("downloadPlot", "Download Plot")),
        tabPanel("Shepard's Plot", plotOutput("sheplot")),
        tabPanel("Goodness of Fit", plotOutput("gofplot")),
        tabPanel("Silhouette Plot", plotOutput("silplot")),
        tabPanel("Mclust", plotOutput("mclust")),
        tabPanel("Cophenetic", plotOutput("coph")),
        tabPanel("Indicator Species", verbatimTextOutput("ind")),
        tabPanel("Metrics", verbatimTextOutput("metrics")),
        tabPanel("3D-Dendrogram", plotOutput("ThreeDend")),
        tabPanel("3D-NMDS", plotOutput("ThreeDim")),
        tabPanel("3D Interactive NMS", plotOutput("ThreeDNMS")),
        tabPanel("Partitioning Around Medoids (PAM)", plotOutput("demopam")),
        tabPanel("Species Accumulation Curve", plotOutput("sac")),
        tabPanel("Calinski Criterion", plotOutput("calinski")),
        tabPanel("Fuzzy Set Ordination", plotOutput("fuzz"))
      )
    )
  )
)
# end fluidPage

server <- function(input, output, session){
  
  library(Cairo)
  options(shiny.usecairo=T)
  
  Species_Matrix <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)})
  
  Environmental_Matrix <- reactive({
    infile <- input$file2
    if (is.null(infile))
      # User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
      return(NULL)
    temp <- read.csv(infile$datapath)
    temp[order(temp[, 1]),]
  })
  
  FlexBeta <- reactive({1-(input$beta)})
  
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
  
  ###Species Accumulation Curve
  output$sac <- renderPlot({
    Species_Matrix <- Species_Matrix()
    sac <- specaccum(Species_Matrix)
    plot(sac, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main="Species Accumulation Curve", ylab="Species")
  })
  
  ###NMS Ordination
  output$ordplot <- renderPlot({
    #render
    Species_Matrix <- Species_Matrix()
    FlexBeta <- FlexBeta()
    Environmental_Matrix <- Environmental_Matrix()
    bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
    demoflex <- agnes(bc,method='flexible',par.method= FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    sit.sc <- scores(bci.mds)
    p5 <- ordiplot(sit.sc, type="n", main="NMS Ordination", display = 'si', ylim=c(-2.3,2.3), xlim=c(-2.2,2.2),xlab="",ylab="")
    title(ylab="NMDS 2", line=2.5, cex.lab=0.7, xlab="NMDS 1")
    ###Environment
    ef1 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
    plot(ef1, col = "red", cex=0.5)
    ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Environmental_Matrix, knots = 1, add = TRUE)
    ef2 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
    plot(ef2, col="green", cex=0.5)
    ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Environmental_Matrix, knots = 1, add = TRUE, col="green")
    for (i in 1:length(grp.lev))
    {points(sit.sc[spe.bw.groups==i,], pch=(14+i), cex=2, col=i+1)}
    ordicluster(p5, spe.bray.ward, col="dark grey", lty=2)     # Add the dendrogram
    ### Add a legend interactively
    #legend(locator(1), paste("Group",c(1:length(grp.lev))), pch=14+c(1:length(grp.lev)), col=1+c(1:length(grp.lev)), pt.cex=0.75, cex = 0.5, ncol=3)
    points(bci.mds, display = "species", cex = 0.7, pch=21, col="red", bg="yellow")
    ordipointlabel(bci.mds, display = "species", add = TRUE, cex = 0.7, scaling = 8)
    ordihull (bci.mds, groups = spe.bw.groups, show.group = 1:input$group, col = c("red","green", "blue","lightblue","pink","yellow","grey"), draw = 'polygon', label = F, lty=3,cex=0.4, alpha = 0.2)
  })
  
  #output$ordplot <- renderPlot({
  #plotInput()
  #CairoX11("plot.pdf", 8.5, 14, bg="transparent")
  #X11(type="cairo")
  #cairo_pdf(filename = "plot.pdf",
  #  width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
  #  antialias = c("default", "none", "gray", "subpixel"),
  #  fallback_resolution = 300)
  #plotInput()
  #savePlot(filename = "plot.png",
  #type = "png",
  #device = dev.cur())
  #dev.off()
  # })
  
  
  plotInput <- function(file){
    #cairo_pdf(filename = file,
              #width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
              #antialias = "subpixel",fallback_resolution = 330)
    Species_Matrix <- Species_Matrix()
    Environmental_Matrix <- Environmental_Matrix()
    FlexBeta <- FlexBeta()
    bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
    demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
    demoflex.hcl <- as.hclust(demoflex)
    bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
    spe.bray.ward <- demoflex.hcl
    spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
    grp.lev <- levels(factor(spe.bw.groups))
    sit.sc <- scores(bci.mds)
    p5 <- ordiplot(sit.sc, type="n", main="NMS Ordination", display = 'si', ylim=c(-2.3,2.3), xlim=c(-2.2,2.2),xlab="",ylab="")
    title(ylab="NMDS 2", line=2.5, cex.lab=0.7, xlab="NMDS 1")
    ###Environment
    ef1 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
    plot(ef1, col = "red", cex=0.5)
    ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var1]]), Environmental_Matrix, knots = 1, add = TRUE)
    ef2 <- envfit(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Species_Matrix, permutations = 999, arrow.mul=0.6)
    plot(ef2, col="green", cex=0.5)
    ordisurf(bci.mds ~ as.vector(Environmental_Matrix[[input$var2]]), Environmental_Matrix, knots = 1, add = TRUE, col="green")
    for (i in 1:length(grp.lev))
    {points(sit.sc[spe.bw.groups==i,], pch=(14+i), cex=2, col=i+1)}
    ordicluster(p5, spe.bray.ward, col="dark grey", lty=2)     # Add the dendrogram
    ### Add a legend interactively
    #legend(locator(1), paste("Group",c(1:length(grp.lev))), pch=14+c(1:length(grp.lev)), col=1+c(1:length(grp.lev)), pt.cex=0.75, cex = 0.5, ncol=3)
    points(bci.mds, display = "species", cex = 0.7, pch=21, col="red", bg="yellow")
    ordipointlabel(bci.mds, display = "species", add = TRUE, cex = 0.7, scaling = 8)
    ordihull (bci.mds, groups = spe.bw.groups, show.group = 1:input$group, col = c("red","green", "blue","lightblue","pink","yellow","grey"), draw = 'polygon', label = F, lty=3,cex=0.4, alpha = 0.2)
    #dev.off()
    }
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$dataset, '.pdf', sep = '')},
      
      content = function(file){
        cairo_pdf(filename = file,
                  width = 18, height = 10, pointsize = 8, family = "sans", bg = "transparent",
                  antialias = c("default", "none", "gray", "subpixel") ,fallback_resolution = 330)
        plotInput(file)
        dev.off()
        },
      
      contentType = "application/pdf"
      
#Error in downloadHandler(filename = function() { : unused argument (outputOptions(output, "downloadPlot", suspendWhenHidden = FALSE))
     #outputOptions(output, "downloadPlot", suspendWhenHidden=FALSE)
    )
    
    ###Shepherd Plot
    output$sheplot <- renderPlot({
      Species_Matrix <- Species_Matrix()
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      stress <- bci.mds$stress
      sheplot <- stressplot(bci.mds, main= paste("NMS Shepard Plot (Stress =", stress,")"))
    })
    
    ##########fixed cluster algortithm or partitions#######
    ###demopam
    output$demopam <- renderPlot({
      Species_Matrix <- Species_Matrix()
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      demopam <- pam(bci.mds$points,k=input$group)
      par(mfrow=c(2,1))
      plot(demopam)
      par(mfrow=c(1,1))
    })
    
    output$fuzz <- renderPlot({
      Species_Matrix <- Species_Matrix()
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=T, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      cfuz <- fanny(bci.mds$points, input$group, memb.exp=1.5)
      ordiplot(bci.mds, dis="si", type="n")
      stars(cfuz$membership, locatio=bci.mds$points[,-3], draw.segm=TRUE, add=TRUE, scale=FALSE, len=0.1)
      ordihull(bci.mds, cfuz$clustering, col="blue")
    })
    
    #Calinski Criterion for choosing cluster number
    output$calinski <- renderPlot({
      Species_Matrix <- Species_Matrix()
      ccash <- cascadeKM(decostand(Species_Matrix,"hell"),input$group, input$group+10)
      ccas <- cascadeKM(Species_Matrix,input$group, input$group+10)
      plot(ccash, sortq=TRUE)
    })
    
    ###Cophenetic Correlation
    output$coph <- renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
      demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      corr <- cor(bc,cophenetic(demoflex.hcl))
      plot(bc, cophenetic(demoflex.hcl), asp=1, main = paste("Cophentic Correlation =", corr), ylab=NULL)
      abline(0, 1)
    })
    
    ###Goodness of Fit    
    output$gofplot <- renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      gof = goodness(bci.mds)
      plot(bci.mds, type="n", main="Site IDs and Associated Goodness of Fit") 
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
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
    
    output$ind <- renderPrint({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
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
      #summary(mod)
      print(summary(mod, type = "long"))
    })
    
    output$metrics <- renderPrint({
      Species_Matrix <- Species_Matrix()
      rankindex(scale(decostand(Species_Matrix, method="hellinger")), Species_Matrix, distance)
    })
    
    output$ThreeDNMS <- renderUI({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
      demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      spe.bray.ward <- demoflex.hcl
      spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
      grp.lev <- levels(factor(spe.bw.groups))
      sit.sc <- scores(bci.mds)
      pca3d(sit.sc, group=spe.bw.groups,show.plane=TRUE,fancy=TRUE)
    })
    
    output$ThreeDend <- renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
      demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      spe.bray.ward <- demoflex.hcl
      spe.bw.groups <- cutree(spe.bray.ward, k=input$group)
      orditree3d(bci.mds, demoflex.hcl, display = "sites", type = "t", col = as.numeric(spe.bw.groups+1), choices = 1:2, cex=0.9)
    })
    
    output$ThreeDim <- renderPlot({
      Species_Matrix <- Species_Matrix()
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
      ordiplot3d(bci.mds, type='h', main= "NMS in 3 dimensions")
    })
    
    output$mclust <- renderPlot({
      Species_Matrix <- Species_Matrix()
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
      # Run the function to see how many clusters it finds to be optimal, set it to search for at least 1 model and up 20.
      d_clust <- Mclust(as.matrix(bc), G=1:20)
      m.best <- dim(d_clust$z)[2]
      cat("model-based optimal number of clusters:", m.best, "\n")
      plot(d_clust, main= "Model-Based Optimal Cluster Number Assessment", what="BIC")
      title(main = paste("Model-Based Optimal Cluster Number Assessment - Generalised Suggestion =", m.best,"Groups" ))
      
    })
    
    ###Silhouette Plot    
    output$silplot <- renderPlot({
      Species_Matrix <- Species_Matrix()
      FlexBeta <- FlexBeta()
      bc <-vegdist(decostand(Species_Matrix, method="hellinger", na.rm= F), method=input$Distance, binary=F)
      demoflex <- agnes(bc,method='flexible',par.method=FlexBeta)
      demoflex.hcl <- as.hclust(demoflex)
      bci.mds<-metaMDS(decostand(Species_Matrix, method="hellinger", na.rm= F), distance =input$Distance, binary=F, k = 3 , trymax = 500, autotransform = F, noshare = F, expand = T, trace = 1, plot = FALSE, itr=1000)
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
      plot(silo, col=order$spe.bw.groups+1, main = expression(paste("Silhouette plot of Group Membership - Flexible"~beta~"= -0.25, Kulczynski Dissimilarity")), cex.names=0.8, nmax.lab=100)
    })
  }
shinyApp(ui=ui, server=server)