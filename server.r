# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
library(plyr)
library(reshape)
library(ggplot2)
shinyServer(function(input, output) {
  output$contents <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    a<-read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    sheep.m <- melt(a[,2:(2*input$Number+1)])
    sheep.m$variable <- as.character(sheep.m$variable)
    x <- unlist(strsplit(sheep.m$variable, "[.]"))
    n<-input$Number*input$Length*4
    sheep.m$class <- x[seq(1, n,2)]
    sheep.m$rep <- x[seq(2, n, 2)]
    sheep.m$WtNo <- rep(a$WtNo,input$Number*2)
    sheep.m.ave <- ddply(sheep.m,c("class","WtNo"),
                         summarise, av.value = mean(value))
 
    if(input$panel1 == "2") {
      dif<-0
      for (i in 1:input$Length){
        t1<-t.test(a[i,2:(input$Number+1)],a[i,(input$Number+2):(input$Number*2+1)]) 
        dif[i]<-t1[3]
      }
      wt<-as.numeric(a$WtNo)
      df<-rbind(wt[1:input$Length],dif[1:input$Length])
      row.names(df)<-c("wt","p.value")
      subset<-df[1,][df[2,]<0.05]
      sub <- subset(sheep.m.ave,sheep.m.ave$WtNo %in% subset==TRUE)
      plot <-qplot(WtNo,av.value,data=sub) }
    
    if (input$panel1 == "1") {
      plot <- qplot(WtNo, av.value, data=sheep.m.ave, geom="line", colour=class) }
    
    if (input$panel1 == "3") {
      sheep.pca <- prcomp(t(a[,2:(2*input$Number+1)]), scale=T, retx=T)
      plot<-qplot(PC1, PC2, data=data.frame(sheep.pca$x), 
            colour=c(rep("A", input$Number), rep("B", input$Number)))}
    
      print(plot)
   
  })
  
  output$summary <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    a<-read.csv(inFile$datapath, header = input$header,
                sep = input$sep, quote = input$quote)
    str(a)
  })
    
  output$downloadData <- downloadHandler(  
    
    filename ="PCA.csv",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      a<-read.csv(inFile$datapath, header = input$header,
                  sep = input$sep, quote = input$quote)
      sheep.pca <- prcomp(t(a[,2:(2*input$Number+1)]), scale=T, retx=T)
      # Write to a file specified by the 'file' argument
      write.table(sheep.pca$x, file, sep =",",
                  row.names = FALSE)
    }
    
  )
})



