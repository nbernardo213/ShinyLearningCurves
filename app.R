library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(rhandsontable)
library(stats)
library(rmarkdown)
library(rsconnect)

header<-dashboardHeader(title=img(src="LEGO_Image.png", height = 200, align = "left"),titleWidth = 300)
sidebar<-dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Introduction", tabName = "Intro",icon = icon("circle-info"),startExpanded = TRUE,
             HTML("<br>
                          A learning curve demonstrates how operaters <br> 
                          improve the speed at which they can complete <br>
                          a task through training.<br>
                          <br>
                          In this application, operators will build a <br>
                          Lego set 3 times and track the duration it <br>
                          takes to complete it. From this data, learning <br>
                          curves will be generated. <br>
                          On the <b>Analysis</b> tab, an operator selects <br>
                          the Lego set that they will build from the <br>
                          dropdown menu for each of the table columns.<br>
                          <br>
                          Data can be entered in the <b>Data Entry</b> tab. <br>
                          The data is then automatically plotted on the <br>
                          <b>Individual Plots</b> and <b>Combined Plots</b> tabs.<br>
                          Several graphs are available from the dropdown <br>
                          menu on each page. <br>
                          <br>")
    ),
    menuItem("Analysis",tabName = "Analysis",icon = icon("magnifying-glass-chart",class = "fa-solid fa-magnifying-glass-chart"),
             htmlOutput("BlankSpace"),
             selectInput(inputId = "NumSets", label = "Number of Sets:", 
                        choices = c(1:4), selected = 4),
             selectInput(inputId = "Rows", label = "Number of Trials:", 
                        choices = c(3:20), selected = 4),
             uiOutput("selectCol1"),
             uiOutput("selectCol2"),
             uiOutput("selectCol3"),
             uiOutput("selectCol4")
    )
  )
)

body<-dashboardBody(
  tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> LEGO Learning Curves </span>\');
      })
     ')),
  fluidRow(
    tabBox(title = "",
           id="tabbox1",width = 400,height = 400,
           tabPanel("Data Entry",
                    h4("Enter Data in Seconds:", style = "margin-top: 10px; margin-bottom: 15px;"),
                    rHandsontableOutput("HandTable", height = "600px")
           ),
           tabPanel("Individual Plot",
                    uiOutput("plotselect"),
                    plotOutput("Individual"),
                    htmlOutput("equationind")
           ),
           tabPanel("Combined Plot", 
                    selectInput(inputId = "selectCombined", #name of input
                                label = "Select a Plot:", #label displayed in ui
                                choices = c("Data Only","Fitted Curves","Data and Curves"),
                                # calls unique values from the State column in the previously created table
                                selected = c("Fitted Curves")), #default choice (not required)
                    plotOutput("Combined"),
                    htmlOutput("equation1"),
                    htmlOutput("equation2"),
                    htmlOutput("equation3"),
                    htmlOutput("equation4")
           )
    )
  )
)
##### Build UI ####
ui <- dashboardPage(
  header,
  sidebar,
  body,
  tags$head(
    tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: white;
                              height: 80px;
                            
                              }
        
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: white;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #FDB515;
                              max-height: 10px;
                              
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #1E3063;
        }
        
        /* active menu background */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu.menu-open {
          background-color: #1E3063;
        }    
        
        /* active menu background while moving*/
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu {
          background-color: #1E3063;
        }
        .skin-blue .sidebar {
                  background-color: #1E3063;
                  padding-top: 15%;
                }
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #808285;
                              }
        
        
                                  
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview {
                  background-color: black;
                }   
        
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #808285;
                              }
                              '))
  )
)
server <- function(input, output, session) {
  
  output$BlankSpace<-renderText({
    " <br> "
  })
  
  LegoSetColor=data.frame(Red = c("Windmill (1 Brick)","Car (2 Brick)","Crab (2 Brick)"),
                          Blue = c("House (1 Brick)","Whale (2 Brick)","Train (3 Brick)"),
                          Orange=c("Shop (1 Brick)","Lion (2 Brick)","Plane (2 Brick)"),
                          Green=c("Alligator (1 Brick)","Castle (1 Brick)","Tractor (3 Brick)"))
  
  RowLen<-reactive({
    req(input$Rows)
    as.numeric(input$Rows)
  })
  
  NumSets<-reactive({
    req(input$NumSets)
    as.numeric(input$NumSets)
  })
  
  DF<-reactive({
    req(input$Rows, input$NumSets)
    df <- data.frame(Trial = 1:input$Rows)
    for(i in 1:input$NumSets) {
      df[paste0("Set", i)] <- 0
    }
    df
  })
  
  output$selectCol1 = renderUI({
    if(NumSets() >= 1) {
      selectInput(inputId = "selectedCol1",
                  label = "Select a Lego Set for the first column:",
                  choices = LegoSetColor)
    }
  })
  
  output$selectCol2 = renderUI({
    if(NumSets() >= 2) {
      selectInput(inputId = "selectedCol2",
                  label = "Select a Lego Set for the second column:",
                  choices = LegoSetColor)
    }
  })
  
  output$selectCol3 = renderUI({
    if(NumSets() >= 3) {
      selectInput(inputId = "selectedCol3",
                  label = "Select a Lego Set for the third column:",
                  choices = LegoSetColor)
    }
  })
  
  output$selectCol4 = renderUI({
    if(NumSets() >= 4) {
      selectInput(inputId = "selectedCol4",
                  label = "Select a Lego Set for the fourth column:",
                  choices = LegoSetColor)
    }
  })
  
  output$HandTable <- renderRHandsontable({ 
    df_temp <- DF()
    
    # Dynamically rename columns based on number of sets
    if(NumSets() >= 1 && !is.null(input$selectedCol1)) {
      names(df_temp)[2] <- paste("(1)", as.character(input$selectedCol1))
    }
    if(NumSets() >= 2 && !is.null(input$selectedCol2)) {
      names(df_temp)[3] <- paste("(2)", as.character(input$selectedCol2))
    }
    if(NumSets() >= 3 && !is.null(input$selectedCol3)) {
      names(df_temp)[4] <- paste("(3)", as.character(input$selectedCol3))
    }
    if(NumSets() >= 4 && !is.null(input$selectedCol4)) {
      names(df_temp)[5] <- paste("(4)", as.character(input$selectedCol4))
    }
    
    rhandsontable(df_temp, width = NULL, height = NULL, stretchH = "all")
  })
  
  
  indat <- reactiveValues(data=NULL)
  
  # Initialize indat$data only once
  observe({
    if(is.null(indat$data)) {
      df <- data.frame(Trial = 1:4)
      for(i in 1:4) {
        df[paste0("Set", i)] <- 0
      }
      indat$data <- df
    }
  })
  
  # Update structure when NumSets or Rows changes, but preserve user data where possible
  observeEvent(c(input$NumSets, input$Rows), {
    req(input$NumSets, input$Rows)  # Only proceed if both inputs are valid
    if(!is.null(indat$data)) {
      tryCatch({
        new_df <- DF()
        # Try to preserve existing data
        for(i in 1:min(ncol(indat$data), ncol(new_df))) {
          for(j in 1:min(nrow(indat$data), nrow(new_df))) {
            new_df[j,i] <- indat$data[j,i]
          }
        }
        indat$data <- new_df
      }, error = function(e) {
        # Silently handle errors to prevent crashes
        NULL
      })
    }
  }, ignoreNULL = TRUE)
  
  output$plotselect <- renderUI({
    SpecificSet<-names(indat$data)[2:(NumSets()+1)]
    
    selectInput(inputId = "setselect",
                label = "Select a Plot:",
                choices = SpecificSet,
                selected = c(SpecificSet[1]))
  })
  
  observe({
    if(!is.null(input$HandTable))
      indat$data <- hot_to_r(input$HandTable)
    
  })  
  
  datalong<-reactive({
    gather(data = indat$data, key = "Lego", value = "Duration", 2:(NumSets()+1))
  })
  
  
  ##### Equations #####
  
  DF2<-reactive({
    df <- data.frame(x=indat$data[,1])
    for(i in 1:NumSets()) {
      df[paste0("y", i)] <- indat$data[,i+1]
    }
    df
  })
  
  SumCol1<-reactive({if(NumSets() >= 1) sum(indat$data[,2]) else 0})
  SumCol2<-reactive({if(NumSets() >= 2) sum(indat$data[,3]) else 0})
  SumCol3<-reactive({if(NumSets() >= 3) sum(indat$data[,4]) else 0})
  SumCol4<-reactive({if(NumSets() >= 4) sum(indat$data[,5]) else 0})
  
  output$equation1<-renderText({
    if(NumSets() < 1) return("")
    
    if(SumCol1()==0){
      paste(as.character(names(indat$data)[2]),
            ":  Enter Data  (Red) <br>")
    } else{
      paste(as.character(names(indat$data)[2]),
            ": y = ",
            round((indat$data[RowLen(),2])/(RowLen()^((log10(indat$data[RowLen(),2])-log10(indat$data[1,2]))/(log10(RowLen())-log10(1)))),4),
            "x^(",
            round(((log10(indat$data[RowLen(),2])-log10(indat$data[1,2]))/(log10(RowLen())-log10(1))),4),
            ")   (Red) <br>"
      )
    }
  })
  
  output$equation2<-renderText({
    if(NumSets() < 2) return("")
    
    if(SumCol2()==0){
      paste(as.character(names(indat$data)[3]),
            ": Enter Data   (Green) <br>")
    } else{
      paste(as.character(names(indat$data)[3]),
            ": y = ",
            round((indat$data[RowLen(),3])/(RowLen()^((log10(indat$data[RowLen(),3])-log10(indat$data[1,3]))/(log10(RowLen())-log10(1)))),4),
            "x^(",
            round(((log10(indat$data[RowLen(),3])-log10(indat$data[1,3]))/(log10(RowLen())-log10(1))),4),
            ")   (Green) <br>"
      )
    }
  })
  
  output$equation3<-renderText({
    if(NumSets() < 3) return("")
    
    if(SumCol3()==0){
      paste(as.character(names(indat$data)[4]),
            ": Enter Data     (Blue) <br>")
    } else{
      paste(as.character(names(indat$data)[4]),
            ": y = ",
            round((indat$data[RowLen(),4])/(RowLen()^((log10(indat$data[RowLen(),4])-log10(indat$data[1,4]))/(log10(RowLen())-log10(1)))),4),
            "x^(",
            round(((log10(indat$data[RowLen(),4])-log10(indat$data[1,4]))/(log10(RowLen())-log10(1))),4),
            ")   (Blue) <br>")
    }
  })
  
  output$equation4<-renderText({
    if(NumSets() < 4) return("")
    
    if(SumCol4()==0){
      paste(as.character(names(indat$data)[5]),
            ": Enter Data     (Purple) <br>")
    } else{
      paste(as.character(names(indat$data)[5]),
            ": y = ",
            round((indat$data[RowLen(),5])/(RowLen()^((log10(indat$data[RowLen(),5])-log10(indat$data[1,5]))/(log10(RowLen())-log10(1)))),4),
            "x^(",
            round(((log10(indat$data[RowLen(),5])-log10(indat$data[1,5]))/(log10(RowLen())-log10(1))),4),
            ")   (Purple) <br>")
    }
  })
  
  DF3<-reactive({
    data.frame(x=indat$data[,1],
               y=indat$data[,input$setselect])
  })
  
  output$equationind<-renderText({
    if(sum(DF3()[,2])==0){
      paste(as.character(input$setselect),
            ": Enter Data")
    } else{
      paste(as.character(input$setselect),
            ": y = ",
            round((indat$data[RowLen(),input$setselect])/(RowLen()^((log10(indat$data[RowLen(),input$setselect])-log10(indat$data[1,input$setselect]))/(log10(RowLen())-log10(1)))),4),
            "x^(",
            round(((log10(indat$data[RowLen(),input$setselect])-log10(indat$data[1,input$setselect]))/(log10(RowLen())-log10(1))),4),
            ")   (Magenta) <br>"
      )
    }
  })
  
  A_Var<-reactive({
    req(input$setselect)
    col_data <- indat$data[,input$setselect]
    if(sum(col_data) == 0 || col_data[1] <= 0 || col_data[RowLen()] <= 0) {
      return(1)
    }
    round((col_data[RowLen()])/(RowLen()^((log10(col_data[RowLen()])-log10(col_data[1]))/(log10(RowLen())-log10(1)))),4)
  })
  
  B_Var<-reactive({
    req(input$setselect)
    col_data <- indat$data[,input$setselect]
    if(sum(col_data) == 0 || col_data[1] <= 0 || col_data[RowLen()] <= 0) {
      return(1)
    }
    round(((log10(col_data[RowLen()])-log10(col_data[1]))/(log10(RowLen())-log10(1))),4)
  })
  ##### Generate Plots #####
  output$Individual<- renderPlot({
    req(input$setselect)
    
    eqind<-function(x){A_Var()*x^(B_Var())}
    
    ggplot(data = indat$data,aes(indat$data[,1],indat$data[,input$setselect]))+
      stat_function(fun = eqind, color = "magenta", inherit.aes = TRUE)+
      annotate("point", x = RowLen()+1, y = eqind(RowLen()+1), color="red", size=3)+
      annotate("text", label = paste0(round(eqind(RowLen()+1),0)," seconds"), 
               size=8, x = RowLen()+1, y=eqind(RowLen()+1)+eqind(RowLen()+1)*0.1)+
      geom_point()+
      geom_line()+
      ggtitle(paste(input$setselect))+
      theme(plot.title = element_text(size=20, hjust = .5),legend.position = "none")+
      xlab("Trials")+
      ylab("Duration")+
      scale_y_continuous(expand=c(.1,.1))+
      scale_x_continuous(breaks = seq(1,RowLen()+1,1))
  })
  
  eq1<-function(x){(indat$data[RowLen(),2])/(RowLen()^((log10(indat$data[RowLen(),2])-log10(indat$data[1,2]))/(log10(RowLen())-log10(1))))*x^((log10(indat$data[RowLen(),2])-log10(indat$data[1,2]))/(log10(RowLen())-log10(1)))}
  eq2<-function(x){(indat$data[RowLen(),3])/(RowLen()^((log10(indat$data[RowLen(),3])-log10(indat$data[1,3]))/(log10(RowLen())-log10(1))))*x^((log10(indat$data[RowLen(),3])-log10(indat$data[1,3]))/(log10(RowLen())-log10(1)))}
  eq3<-function(x){(indat$data[RowLen(),4])/(RowLen()^((log10(indat$data[RowLen(),4])-log10(indat$data[1,4]))/(log10(RowLen())-log10(1))))*x^((log10(indat$data[RowLen(),4])-log10(indat$data[1,4]))/(log10(RowLen())-log10(1)))}
  eq4<-function(x){(indat$data[RowLen(),5])/(RowLen()^((log10(indat$data[RowLen(),5])-log10(indat$data[1,5]))/(log10(RowLen())-log10(1))))*x^((log10(indat$data[RowLen(),5])-log10(indat$data[1,5]))/(log10(RowLen())-log10(1)))}
  
  output$Combined <-    renderPlot({ 
    
    # Create equation functions dynamically with validation
    # Use local() to properly capture the loop variable
    eq_list <- list()
    colors <- c("#FF6666", "#669900", "#56B4E9", "#9933FF")
    
    for(i in 1:NumSets()) {
      eq_list[[i]] <- local({
        col_idx <- i + 1
        function(x) {
          col_data <- indat$data[,col_idx]
          # Validate data to prevent log(0) errors
          if(sum(col_data) == 0 || col_data[1] <= 0 || col_data[RowLen()] <= 0) {
            return(NA)
          }
          a <- col_data[RowLen()]/(RowLen()^((log10(col_data[RowLen()])-log10(col_data[1]))/(log10(RowLen())-log10(1))))
          b <- (log10(col_data[RowLen()])-log10(col_data[1]))/(log10(RowLen())-log10(1))
          return(a * x^b)
        }
      })
    }
    
    if (input$selectCombined == "Data Only"){
      
      p <- ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
        geom_point(aes(shape = Lego))+geom_line()+
        ggtitle(paste(input$selected," Set: Data"))+
        theme(plot.title = element_text(size=20, hjust = .5))+
        xlab("Trials")+
        scale_y_continuous(expand=c(.1,.1),limits = c(0,NA))+
        scale_x_continuous(breaks = seq(1,RowLen()+1,1))+
        guides(size="none")
      
      # Add prediction points and labels dynamically
      for(i in 1:NumSets()) {
        eq_val <- eq_list[[i]](RowLen()+1)
        if(!is.na(eq_val)) {
          p <- p + 
            annotate("point", x=RowLen()+1, y=eq_val, size=3, color=colors[i]) +
            annotate("text", label = paste0(round(eq_val,0)," seconds"), 
                     size=6, x = RowLen()+1, y=eq_val+eq_val*0.1)
        }
      }
      p
      
    } else if (input$selectCombined == "Fitted Curves"){
      # Check if any column has no data
      any_empty <- any(sapply(1:NumSets(), function(i) sum(indat$data[,i+1]) == 0))
      
      if(any_empty){
        ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
          ggtitle("Please Completely Fill the Data Table")+
          theme(plot.title = element_text(size=20, hjust = .5))+
          xlab("Trials")+
          scale_y_continuous(expand=c(.1,.1),limits = c(0,NA))+
          scale_x_continuous(breaks = seq(1,RowLen()+1,1))
        
      } else {
        p <- ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
          ggtitle(paste(input$setselect," Set: Power Curves"))+
          theme(plot.title = element_text(size=20, hjust = .5))+
          xlab("Trials")+
          scale_y_continuous(expand=c(.1,.1),limits = c(0,NA))+
          scale_x_continuous(breaks = seq(1,RowLen()+1,1))+
          guides(size="none")
        
        # Add curves and annotations dynamically
        for(i in 1:NumSets()) {
          eq_val <- eq_list[[i]](RowLen()+1)
          if(!is.na(eq_val)) {
            p <- p +
              annotate("point", x=RowLen()+1, y=eq_val, size=3, color=colors[i]) +
              annotate("text", label = paste0(round(eq_val,0)," seconds"), 
                       size=6, x = RowLen()+1, y=eq_val+eq_val*0.1) +
              stat_function(fun = eq_list[[i]], color = colors[i])
          }
        }
        p
      }
    } else {
      # Check if any column has no data
      any_empty <- any(sapply(1:NumSets(), function(i) sum(indat$data[,i+1]) == 0))
      
      if(any_empty){
        ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
          geom_point(aes(shape = Lego))+
          geom_line()+
          ggtitle("Fill Data Table for Fitted Curves")+
          theme(plot.title = element_text(size=20, hjust = .5))+
          xlab("Trials")+
          scale_y_continuous(expand=c(.1,.1),limits = c(0,NA))+
          scale_x_continuous(breaks = seq(1,RowLen()+1,1))
        
      } else{
        p <- ggplot(data = datalong(),aes(datalong()[,1],Duration,color=Lego))+
          geom_point(aes(shape = Lego))+
          geom_line()+
          ggtitle(paste(input$setselect," Set: Data and Power Curves"))+
          theme(plot.title = element_text(size=20, hjust = .5))+
          xlab("Trials")+
          scale_y_continuous(expand=c(.1,.1),limits = c(0,NA))+
          scale_x_continuous(breaks = seq(1,RowLen()+1,1))+
          guides(size="none")
        
        # Add curves and annotations dynamically
        for(i in 1:NumSets()) {
          eq_val <- eq_list[[i]](RowLen()+1)
          if(!is.na(eq_val)) {
            p <- p +
              annotate("point", x=RowLen()+1, y=eq_val, size=3, color=colors[i]) +
              annotate("text", label = paste0(round(eq_val,0)," seconds"), 
                       size=6, x = RowLen()+1, y=eq_val+eq_val*0.1) +
              stat_function(fun = eq_list[[i]], color = colors[i])
          }
        }
        p
      }
    }
  })
  
  
  outputOptions(output, "selectCol1", suspendWhenHidden = FALSE)
  outputOptions(output, "selectCol2", suspendWhenHidden = FALSE)
  outputOptions(output, "selectCol3", suspendWhenHidden = FALSE)
  outputOptions(output, "selectCol4", suspendWhenHidden = FALSE)
  
  # Note: session$onSessionEnded with stopApp() removed to support multiple concurrent users
  # Each user session is isolated and will clean up automatically when they disconnect
}
shinyApp(ui = ui, server = server)
