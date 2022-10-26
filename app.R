library(shiny)
library(DT)
library(FSelector)
library(caret)
library(randomForest)
library(shinyWidgets)

shinyApp(
  ui = fluidPage(
    #from https://stackoverflow.com/questions/35251788/hide-values-of-sliderinput-in-shiny
    tags$head(tags$style(HTML(' .irs-to, .irs-min, .irs-max, .irs-single {
            visibility: hidden !important;
    }'))),
    sidebarLayout(
    sidebarPanel(
      #numericInput("lamda", label = "lamda - cost weighting", value = 0.01),
      sliderInput(
        
        inputId = "lamda",
        
        label="Cost Weighting- slide to left for cost-insensitive feature selection",
        #https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
        
        min=0,
        max=0.02,
        value=0,
        step=0.001,
        ticks=FALSE
        #width='250px'
      ),
      
      numericInput(
        inputId= "budget",
        label= "Maximum assessment time (seconds)",
        value =2000,
        min = 30,
        max = NA,
        
      ),
      sliderInput(
      inputId = "lamda2",
      
      label="Financial Cost weighting- slide to left for cost-insensitive feature selection",
      #https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
      
      min=0,
      max=0.0004,
      value=0,
      ticks=FALSE
      #width='250px'
    ),
      
      numericInput(
        inputId= "budget2",
        label= "Maximum financial cost (£)",
        value =3000,
        min = 300,
        max = NA,
        
      ),
      
      
      
      
      
      
      
      
      
      actionButton("go", "Run Tests")
   
    ), #sidebarPanel
      mainPanel(
        tabsetPanel(type = "tabs",
         tabPanel("Select Features",
           multiInput(
             inputId = "id", label = "Assessment items :",
             choices =  newcostscut[,1]
             ,
             selected = newcostscut[,1], width = "500px",
             options = list(
               enable_search = FALSE,
               non_selected_header = "Available items:",
               selected_header = "Currently Selected Items:"
             )#options
           )#multiinput
         ),#tabPanel
         tabPanel("Edit Costs",
        DTOutput('x1')
        ),#tabpanel
        tabPanel("Show Results",
           htmlOutput("x2")
        )#tabpanel
        )#tabsetpanel
        )#mainpanel
      )#sidebarlayout
    ),#ui
  server = function(input, output, session) {
    x = reactiveValues(df = NULL)
    
    observe({
      df <- newcostscut[newcostscut$Var %in% input$id,]
      colnames(df)<-c("Item","Assessment Time (seconds)","Financial Cost")
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- df
    })
    
    output$x1 = renderDT(x$df,
                         options = list( pageLength = 12, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
    
    proxy = dataTableProxy('x1')
    
    output$x2<-eventReactive(input$go, {
      #list1<-cfs(CDRSB~.,joinedcosts)
      
      #list1<-c(input$lamda,x$df[,1])
      times<-x$df[,2]
      names(times)<-x$df[,1]
      prices<-x$df[,3]
      names(prices)<-x$df[,1]
      tempdf<-joinednewcut[,c("CDRSB",names(times))]
      #list1<-cost_cfs(CDRSB ~.,lamda=input$lamda,costs=times,joinedcosts[,c("CDRSB",rownames(exportcosts))])
      list1=cost_cfs_2(CDRSB ~.,lamda=input$lamda,costs=times,joinednewcut[,c("CDRSB",names(prices))],budget=input$budget,costs2=prices,lamda2=input$lamda2,budget2=input$budget2)
      
      #text<-paste(list1,collapse="<br>")
      #paste("<b>Selected Features</b> <br>",text,sep="")
      
      
     
      
  
      
      tempdf<-tempdf[,c("CDRSB",list1)]
      yy<-createDataPartition(tempdf$CDRSB,p=0.8,list=FALSE)
      traindf<-tempdf[yy,]
      testdf<-tempdf[-yy,]
      
      
      #text<-paste(list1,collapse="<br>")
      
      model<-randomForest(CDRSB~.,traindf)
      vec2<-rownames(model$importance)[order(-model$importance)]
      predicted<-predict(model,testdf)
      auc<-multiclass.roc(as.ordered(testdf$CDRSB),as.ordered(predicted))
      val=round(auc$auc,3)
      text=paste("<table>")
      for (i in 1:length(vec2)){
        text=paste(text,"<tr><td>",list1[i],"</td><td>",round(model$importance[rownames(model$importance)==vec2[i]],2),"</td></tr>")
        
      }
      text=paste(text,"</table><br>")
      text=paste("<b>Feature Importance</b><br>",text)
      text=paste("<b>Financial cost: </b>£",sum(prices[list1])," <br>",text,sep="")
      text=paste("<b>Diagnosis Time: </b>",sum(times[list1])," seconds <br>",text,sep="")
      paste("<b>Multiclass AUC: </b>",val,"<br>",text,sep="")
    })
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # problem starts here
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })
    
    output$print <- renderPrint({
      x$df
    })
  }
)