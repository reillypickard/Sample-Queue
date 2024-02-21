library(shiny)
library(shinydashboard)
library(formattable)
library(glue)
library(shiny)
library(tidyverse)
library(DT)
library(dplyr)
library(shinyjs)
library(shinymanager)
library(microbenchmark)
library(shinyBS)
library("googledrive")
library(googlesheets4)
library(anytime)  

options(gargle_oauth_cache = ".secrets2")
drive_auth(cache = ".secrets2", email = "pickard1629@gmail.com")
gs4_auth(token = drive_token())
table1<- drive_get("Completed")
table2<-drive_get("Received")
rnames<- read.csv("rnames.csv")


ids <- read.csv("ids.csv")

colnames(ids)[1]<-"loc"
ids$loc <- as.factor(ids$loc)


navbar_js <- "@media (max-width: 3000px) {
    .navbar-header {
        float: none;
        background-color: #75AD86 ;
    }
      .navbar-footer {
        float: none;
        background-color: #75AD86 ;
    }
    .navbar-left,.navbar-right {
        float: none !important;
        background-color: #75AD86 ;
    }
    .navbar-toggle {
        display: block;
    }

    .navbar-fixed-top {
        top: 0;
        border-width: 0 0 1px;
    }    .navbar.navbar-default.navbar-static-top{ color: #7ACDDD; 
                                      font-size: 16px; 
                                      font-family: 'Segoe UI';
                                    
                                     
                                    
                                      background: linear-gradient(to right, 
                                      #75AD86,#75AD86,#344b5c);
        
      }
          
    .navbar-collapse.collapse {
        display: none!important;
    }
    .navbar-nav {
        float: none!important;
        margin-top: 7.5px;
          color: #5451F4; 
                           font-size: 12px; 
                           font-family: 'Segoe UI';
                           background-color: #75AD86  ;
                            overflow: hidden;
                           height:100px;
    }      .navbar-default .navbar-brand { color: white; 
                                      font-size: 36px; 
                                      font-family: 'Segoe UI';
                                     background-color: #75AD86  ;}
    .navbar-nav>li {
        float: none;
    }
    .navbar-nav>li>a {
        padding-top: 10px;
        padding-bottom: 10px;
        background-color: #75AD86 ;
    }
    .collapse.in{
        display:block !important;
    }

}"







#ui ----
ui <-   navbarPage( collapsible = TRUE,
  
  title = div(
    
    
    
    p(
      "ATDT Sample Queue", style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;
                        font-size: 40px;
      
      text-shadow: -0.5px -0.5px 0 white, 0.5px -0.5px 0 white, -0.5px 0.5px 0 white, 0.5px 0.5px 0 white;
      text-align:center;
      
      " )
  ),
  tags$head(
    tags$style(HTML(navbar_js))),
  tabPanel(
    p(p("",style = "
                        font-size: 32px; text-align: 'center';")),
    
    fluidPage(tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }
                             .block {
      border-color: #344b5c;
      border-style: solid;
      background-color:#344b5c ;
      text-align: center;
      font-family: Segoe UI;
      color:white;
      font-size: 18px;
      margin-top: 20x;
      margin-left: 2px;
      padding-left:5px;
      padding-right:5px;
      padding-top:5px;
      padding-bottom:5px;
      margin-right: 5px;
       border-radius: 5px 5px 5px 5px;
                             }
                             
                                                     .block2 {
      border-color: #344b5c;
      border-style: solid;
      background-color:#344b5c ;
      text-align: center;
      font-family: Segoe UI;
      font-weight: bold;
      color:white;
      width = 600px;
      font-size: 26px;
      margin-top: 50x;
      border-radius: 8px 8px 8px 8px;
       background: radial-gradient(ellipse farthest-corner at 90% 90%, #344b5c,  #344b5c, #75AD86);
       
                                                     }
                                                     
                                                     
                                                                                                          .block3 {
      border-color: white;
      border-style: solid;
      background-color:white ;
                                                                                                          }
                                                     
                                                            .block4 {
      border-color:#344b5c ;
      border-style: solid;
      background-color:white ;
      width:750px;
      text-align: right;
      padding-bottom:2px;
      padding-top:2px;
      padding-left:2px;
      padding-right:2px;
       border-radius: 8px 8px 8px 8px;
 
      
                                                            }
                                                            
                                                                                                                       .block5 {
      border-color:#344b5c ;
      border-style: solid;
      background-color:white ;
      width: 480px;
      text-align: right;
      padding-bottom:2px;
      padding-top:2px;
      padding-left:2px;
      padding-right:2px;
      margin-right:10px;
       border-radius: 8px 8px 8px 8px;
 
      
                                                            }
              
                            .block_input {
      border-color: white;
      border-style: solid;
      background-color:white ;
      text-align: left;
      font-size:20px;
      color:#344b5c;
      margin-left: 10px;
      margin-top: 0px;
      margin-bottom:5px;
      border-radius: 5px 5px 5px 5px;
              }
                                 
@media screen and (min-width:700px) {
nav ul{
display: flex;
justify-content: flex-end;
}
}
    

                        
                         
                         "
    ),shinyjs::useShinyjs(),
    
    fluidRow(
      column(width = 2,
             tags$style(
               "    .selectize-input{ font-size: 12px; font-family: 'Segoe UI''; line-height: 12px;} 
                           
                       .text-input{ font-size: 16px; font-family: 'Segoe UI''; line-height: 16px;}    
                       .selectize-input:hover{background: #75AD86 !important; border-color: #344b5c;}
                           .selectize-input:active{background: #75AD86 !important; border-color: #344b5c;}
                           .selectize-dropdown-content { font-size: 16px; 
                               line-height: 16px; font-family:'Segoe UI'; 
                               background: white !important;
                               color: #344b5c;
                               }
                             .selectize-dropdown-content .active {
                               background:#75AD86  !important;
                                 color: #344b5c !important;
                               font-size: 16px; line-height: 16px;  font-family: 'Segoe UI';
                               .checkbox{margin-top: -20px;  background-color:#344b5c ;
                               margin-left: 0px; margin-bottom: -5px;padding:-5px; margin-right: -800px;
                               }
                               
                             }

               
               "
             ),
             h1(""),
             h1(""),
             br(),
             br(),
             div(class= "block",
                 tags$style(
                   "    .selectize-input{ font-size: 12px; font-family: 'Segoe UI''; line-height: 12px;} 
                           
                       .text-input{ font-size: 16px; font-family: 'Segoe UI''; line-height: 16px;}    
                       .selectize-input:hover{background: #75AD86 !important; border-color: #344b5c;}
                           .selectize-input:active{background: #75AD86 !important; border-color: #344b5c;}
                           .selectize-dropdown-content { font-size: 16px; 
                               line-height: 16px; font-family:'Segoe UI'; 
                               background: white !important;
                               color: #344b5c;
                               }
                             .selectize-dropdown-content .active {
                               background:#75AD86  !important;
                                 color: #344b5c !important;
                               font-size: 16px; line-height: 16px;  font-family: 'Segoe UI';
                               .checkbox{margin-top: -20px;  background-color:#344b5c ;
                               margin-left: 0px; margin-bottom: -5px;padding:-5px; margin-right: -800px;
                               }
                               
                             }

               
               "
                 ),
                 
                 selectInput("q1", "I am reporting: ", choices = c("Received Samples", "Analyzed Samples")),
                 uiOutput("rorc"),
                 uiOutput("ref")
              
              
                 
                 )),
      column(width = 10, 
             tags$style(
               "    .selectize-input{ font-size: 12px; font-family: 'Segoe UI''; line-height: 12px;} 
                           
                       .text-input{ font-size: 16px; font-family: 'Segoe UI''; line-height: 16px;}    
                       .selectize-input:hover{background: #75AD86 !important; border-color: #344b5c;}
                           .selectize-input:active{background: #75AD86 !important; border-color: #344b5c;}
                           .selectize-dropdown-content { font-size: 16px; 
                               line-height: 16px; font-family:'Segoe UI'; 
                               background: white !important;
                               color: #344b5c;
                               }
                             .selectize-dropdown-content .active {
                               background:#75AD86  !important;
                                 color: #344b5c !important;
                               font-size: 16px; line-height: 16px;  font-family: 'Segoe UI';
                               .checkbox{margin-top: -20px;  background-color:#344b5c ;
                               margin-left: 0px; margin-bottom: -5px;padding:-5px; margin-right: -800px;
                               }
                               
                             }

               
               "
             ),
             fluidRow(
          uiOutput("b"), 
       

         fluidRow(div(class = "block_input", h5("To delete rows: select 'All' in the above dropdown, then
                   enter the range of rows. To delete one row, enter the same number in each."))),

             
             
             
             column(width = 6,
                    fluidRow(
                    column(width = 2,
                           textInput("delR", "Range Start", "")),
                    column(width =2,
                           textInput("delR2", "Range Finish", ""))),
                    
                    useShinyjs(),
                    inlineCSS(list("table" = "font-size: 12px; font-family: 'Segoe UI'")),
                    
                    uiOutput("title_table_r"),
                    
                    div(class = "block4",
                        tags$style(HTML(".dataTables_wrapper .dataTables_length {
                  float: right;}
                  .dataTables_wrapper .dataTables_filter {
                  float: left;
                  text-align: left;}"
                        )
                        ),
                        DT::dataTableOutput("received"),
                        h1(""),
                        h1(""),
                        uiOutput("vboxR"))),
          
             
             
             column(width = 2, h1("")),
             column(width = 4,
                    fluidRow(
                    column(width = 3,
                           textInput("delC", "Range Start", "")),
                    column(width =3,
                           textInput("delC2", "Range Finish", ""))),
                    
                    useShinyjs(),
                    inlineCSS(list("table" = "font-size: 12px; font-family: 'Segoe UI'")),
                   
                    uiOutput("title_table_c"),
                    div(class = "block5",
                        tags$style(HTML(".dataTables_wrapper .dataTables_length {
                  float: right;}
                  .dataTables_wrapper .dataTables_filter {
                  float: left;
                  text-align: left;}"
                        )
                        ),
                        DT::dataTableOutput("completed"),
                        h1(""),
                        h1(""),
                        
                        uiOutput("vboxC"))
                    
             )
             
      )
      
      
    )
    
    
    
    )),
  tags$footer(p(img(src = "footer3.png", height = '150px', width = '400px')), align = "center", style = "
              bottom:0;
              right:0;
              left:0;
              margin-top:80px;
              width:100%;
              height:160px;   /* Height of the footer */
              color: white;
              padding: 0px;
               background: linear-gradient(to right, 
                                      #75AD86,#75AD86,#344b5c);
              z-index: 1000;")
))


a.selected <- "All"
choice_list <- list(
  "a" = quote(selectInput("filtera", "Filter by Analysis Type: ", choices = list("All", "qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals" ), 
                          selected=a.selected)))

#server ----




# Check that the non-interactive authentication works by first deauthorizing:



# Authenticate using token. If no browser opens, the authentication works.



# Then pass the token to each drop_ function

server <- function(input, output, session) {
  observeEvent(input$preview, {
    showModal(modalDialog(
      div(class = "block4",
      h2("Entry Preview: If you like what you see, save and update!"),
      DT::dataTableOutput("preview")),
      footer = NULL,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$previewc, {
    showModal(modalDialog(
      h2("Entry Preview: If you like what you see, save and update!"),
      DT::dataTableOutput("previewc"),
      footer = NULL,
      easyClose = TRUE
    ))
  })
  


  
  output$b <- renderUI({
    eval(choice_list[["a"]])
  }) 
  observe({
    a.selected <<- input$filtera
  })
  
  output$title_table_r <-renderUI({
    h3("Sample Queue - ", input$filtera)
  })
  
  output$title_table_c <-renderUI({
    h3("Analyzed Samples - ", input$filtera)
  })
  
  rc<- eventReactive(input$q1,{
    if("Received Samples" %in% input$q1){
      
      rc <- div(tags$style(HTML("
                 .checkbox{   font-style: bold !important;
      color:#344b5c !important;}")),
                bsCollapsePanel(title = div(p("Sites of Received Samples  ", 
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       
                                       checkboxGroupInput(inputId = "site", 
                                                          NULL, choices = c(levels(ids$loc))),
                                       
                                       selectInput("sall_site","Select All" ,choices = c("Select All", "Manual Select",
                                                                                         "De-Select All"),
                                                   selected ="Manual Select"))),
                bsCollapsePanel(title = div(p("Analysis' to be completed  ", 
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       
                                       checkboxGroupInput("analysis", NULL, 
                                                          c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals" )),
                                       selectInput("sall_anal","Select All" ,choices = c("Select All", "Manual Select",
                                                                                         "De-Select All"),selected = "Manual Select"))),
                bsCollapsePanel(title = div(p("Sample types received",
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       
                                       checkboxGroupInput("sample", NULL,
                                                          c("Passive", "Grab" )))),
                bsCollapsePanel(title = div(p("Adsorbents to be used", 
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       checkboxGroupInput("adsorb",label = div(p("Please tick NA if you selected Grab",
                                                                                 style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;")),
                                                          c("GAC", "Filter", "C18", "NA" , "OASIS-HLB")),
                                       selectInput("sall_ads","Select All" ,choices = c("Select All", "Manual Select",
                                                                                        "De-Select All"), selected = "Manual Select"))),
                textInput("receiver", "Samples received by:", " " ),
                dateInput("dateD", "Deployment Date",value = Sys.Date()),
                dateInput("dateC", "Collection Date",value = Sys.Date()),
                dateInput("dateA", "Sample Received on:", value = Sys.Date()),
                actionButton("preview", "Preview"),
                actionButton("enter", "Save"))
    }else if("Analyzed Samples" %in% input$q1){
      rc <- div(tags$style(HTML("
                 .checkbox{   font-style: bold !important;
      color:#344b5c !important;}")),
                bsCollapsePanel(title = div(p("Sites of Analyzed Samples  ", 
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       
                                       checkboxGroupInput(inputId = "sitec", 
                                                          NULL, choices = c(levels(ids$loc))),
                                       selectInput("sall_sitec","Select All" ,choices = c("Select All", "Manual Select",
                                                                                          "De-Select All"), selected = "Manual Select"))),
                bsCollapsePanel(title = div(p("Analysis' completed  ", 
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       
                                       checkboxGroupInput("analysisc", NULL, 
                                                          c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals" )),
                                       selectInput("sall_analc","Select All" ,choices = c("Select All", "Manual Select",
                                                                                          "De-Select All"), selected = "Manual Select"))),
                bsCollapsePanel(title = div(p("Sample types analyzed",
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       
                                       checkboxGroupInput("samplec", NULL,
                                                          c("Passive", "Grab" )))),
                bsCollapsePanel(title = div(p("Adsorbents used", 
                                              style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;", img(src = "arrow.png", height = '30px', width = '30px'))), 
                                style = "info",
                                # style = "color:grey;",
                                
                                column(12,
                                       checkboxGroupInput("adsorbc",label = div(p("Please tick NA if you selected Grab",
                                                                                  style = "font-family: 'Segoe UI Black';
      font-style: bold;
      color:#344b5c;")),
                                                          c("GAC", "Filter", "C18", "NA" , "OASIS-HLB")),
                                       selectInput("sall_adsc","Select All" ,choices = c("Select All", "Manual Select",
                                                                                         "De-Select All"), selected = "Manual Select"))),
                dateInput("dateAc", "Sample Received on:", value = Sys.Date()),
                actionButton("previewc", "Preview"),
                actionButton("enterc", "Save"))
    }
  }
  )
  
  output$rorc<-renderUI({
    rc()
  })
  
  output$ref <- renderUI({
    actionButton("refresh", "Update")
  })
  
  
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  
  
  
  m <- eventReactive(input$enter, {
    
    m <- as.data.frame(expand.grid(unlist(strsplit(input$site, split=" ")),
                                   unlist(strsplit(input$analysis, split=" ")),
                                   unlist(strsplit(input$sample, split=" ")),
                                   unlist(strsplit(input$adsorb, split=" "))
    ) )
    
    
    m <- data.frame(
      Date_Dep = c(rep(input$dateD , nrow(m))),
      Date_Coll = c(rep(input$dateC , nrow(m))),
      Date_Rec = c(rep(input$dateA , nrow(m))),
      Receiver = c(rep(input$receiver, nrow(m))),
      Site = m[,1],
      Analysis = m[,2],
      Sample = m[,3],
      Adsorbent = m[,4]
    )
    m <- data.frame(
      Date_Dep = as.character(m$Date_Dep),
      Date_Coll =as.character( m$Date_Coll),
      Date_Rec = as.character(m$Date_Rec),
      Receiver = m$Receiver,
      Site = m$Site,
      Analysis = m$Analysis,
      Sample = m$Sample,
      Adsorbent = m$Adsorbent,
      code = as.factor(with(m,paste(Date_Rec,Site,Analysis, Sample, Adsorbent, sep = "_" )))
    )
    
    
    
    
    
    
  }          
  )
  
  
  
  m1 <- eventReactive( m(),{
    if(
      is.integer0(which((m()$Analysis == 'Metals' | m()$Analysis == 'Bulk-Chem') & m()$Sample == "Passive"))){
      m1 <- m()
    }else{
      m1 <- m()[-c(which((m()$Analysis == 'Metals' | m()$Analysis == 'Bulk-Chem') & m()$Sample == "Passive")), ]
    }
    m1
  }
  
  )
  
  m2 <- eventReactive( m1(),{
    if(
      is.integer0(which((m1()$Sample == 'Grab') & m1()$Adsorbent != "NA"))){
      m2 <- m1()
    }else{
      m2 <- m1()[-c(which((m1()$Sample == 'Grab') & m1()$Adsorbent != "NA")), ]
    }
    m2
  }
  )
  
  m3 <-  eventReactive( m2(),{
    if(
      is.integer0(which((m2()$Sample == 'Passive') & m2()$Adsorbent == "NA"))){
      m3 <- m2()
    }else{
      m3 <- m2()[-c(which((m2()$Sample == 'Passive') & m2()$Adsorbent == "NA")), ]
    }
    m3
  }
  )
  
  m4 <-  eventReactive( m3(),{
    if(
      is.integer0(which( m3()$Analysis == 'GC-MS' & m3()$Sample == 'Passive' & m3()$Adsorbent != "C18"))){
      m4 <- m3()
    }else{
      m4 <- m3()[-c(which( m3()$Analysis == 'GC-MS' & m3()$Sample == 'Passive' & m3()$Adsorbent != "C18")), ]
    }
    m4
  }
  )   
  
  m5 <-  eventReactive( m4(),{
    if(
      is.integer0(which( m4()$Analysis == 'LC-MS/MS' & m4()$Sample == 'Passive' & m4()$Adsorbent != "OASIS-HLB"))){
      m5 <- m4()
    }else{
      m5 <- m4()[-c(which( m4()$Analysis == 'LC-MS/MS' & m4()$Sample == 'Passive' & m4()$Adsorbent != "OASIS-HLB")), ]
    }
    m5
  }
  )
  
  m6 <-  eventReactive( m5(),{
    if(
      is.integer0(which( m5()$Analysis == 'qPCR' & m5()$Sample == 'Passive' & m5()$Adsorbent %in% c("OASIS-HLB", "C18")))){
      m6 <- m5()
    }else{
      m6 <- m5()[-c(which( m5()$Analysis == 'qPCR' & m5()$Sample == 'Passive' & m5()$Adsorbent %in% c("OASIS-HLB", "C18"))), ]
    }
    m6
  }
  ) 
  
  m7<-  eventReactive( m6(),{
    m7 <- m6()
    k=1
    while(k<=nrow(m7)){
    if( m7$Sample[k] == 'Grab'  ){
      m7$Date_Dep[k] <- NA
    }
     else{
       m7$Date_Dep[k] <- m7$Date_Dep[k]
    }
   k = k+1
    }
   m7

  }
  ) 
  
  
  mp <- eventReactive(input$preview, {
    
    mp <- as.data.frame(expand.grid(unlist(strsplit(input$site, split=" ")),
                                   unlist(strsplit(input$analysis, split=" ")),
                                   unlist(strsplit(input$sample, split=" ")),
                                   unlist(strsplit(input$adsorb, split=" "))
    ) )
    
    
    mp <- data.frame(
      Date_Dep = c(input$dateD, nrow(mp)),
      Date_Coll = c(rep(input$dateC, nrow(mp))),
      Date_Rec = c(rep(input$dateA , nrow(mp))),
      Receiver = c(rep(input$receiver, nrow(mp))),
      Site = mp[,1],
      Analysis = mp[,2],
      Sample = mp[,3],
      Adsorbent = mp[,4]
    )
    mp <- data.frame(
      Date_Dep = as.character(mp$Date_Dep),
      Date_Coll = as.character(mp$Date_Coll),
      Date_Rec = as.character(mp$Date_Rec),
      Receiver = mp$Receiver,
      Site = mp$Site,
      Analysis = mp$Analysis,
      Sample = mp$Sample,
      Adsorbent = mp$Adsorbent,
      code = as.factor(with(mp,paste(Date_Rec,Site,Analysis, Sample, Adsorbent, sep = "_" )))
    )
    
    
    
    
    
    
  }          
  )
  
  
  
  mp1 <- eventReactive( mp(),{
    if(
      is.integer0(which((mp()$Analysis == 'Metals' | mp()$Analysis == 'Bulk-Chem') & mp()$Sample == "Passive"))){
      mp1 <- mp()
    }else{
      mp1 <- mp()[-c(which((mp()$Analysis == 'Metals' | mp()$Analysis == 'Bulk-Chem') & mp()$Sample == "Passive")), ]
    }
    mp1
  }
  
  )
  
  mp2 <- eventReactive( mp1(),{
    if(
      is.integer0(which((mp1()$Sample == 'Grab') & mp1()$Adsorbent != "NA"))){
      mp2 <- mp1()
    }else{
      mp2 <- mp1()[-c(which((mp1()$Sample == 'Grab') & mp1()$Adsorbent != "NA")), ]
    }
    mp2
  }
  )
  
  mp3 <-  eventReactive( mp2(),{
    if(
      is.integer0(which((mp2()$Sample == 'Passive') & mp2()$Adsorbent == "NA"))){
      mp3 <- mp2()
    }else{
      mp3 <- mp2()[-c(which((mp2()$Sample == 'Passive') & mp2()$Adsorbent == "NA")), ]
    }
    mp3
  }
  )
  
  mp4 <-  eventReactive( mp3(),{
    if(
      is.integer0(which( mp3()$Analysis == 'GC-MS' & mp3()$Sample == 'Passive' & mp3()$Adsorbent != "C18"))){
      mp4 <- mp3()
    }else{
      mp4 <- mp3()[-c(which( mp3()$Analysis == 'GC-MS' & mp3()$Sample == 'Passive' & mp3()$Adsorbent != "C18")), ]
    }
    mp4
  }
  )   
  
  mp5 <-  eventReactive( mp4(),{
    if(
      is.integer0(which( mp4()$Analysis == 'LC-MS/MS' & mp4()$Sample == 'Passive' & mp4()$Adsorbent != "OASIS-HLB"))){
      mp5 <- mp4()
    }else{
      mp5 <- mp4()[-c(which( mp4()$Analysis == 'LC-MS/MS' & mp4()$Sample == 'Passive' & mp4()$Adsorbent != "OASIS-HLB")), ]
    }
    mp5
  }
  )
  
  mp6 <-  eventReactive( mp5(),{
    if(
      is.integer0(which( mp5()$Analysis == 'qPCR' & mp5()$Sample == 'Passive' & mp5()$Adsorbent %in% c("OASIS-HLB", "C18")))){
      mp6 <- mp5()
    }else{
      mp6 <- mp5()[-c(which( mp5()$Analysis == 'qPCR' & mp5()$Sample == 'Passive' & mp5()$Adsorbent %in% c("OASIS-HLB", "C18"))), ]
    }
    mp6
  }
  ) 
  
  mp7<-  eventReactive( mp6(),{
    mp7 <- mp6()
    k=1
    while(k<=nrow(mp7)){
      if( mp7$Sample[k] == 'Grab'  ){
        mp7$Date_Dep[k] <- NA
      }
      else{
        mp7$Date_Dep[k] <- mp7$Date_Dep[k]
      }
      k = k+1
    }
    mp7
  }
  )
  
  
  
  
  output$preview <-DT::renderDataTable({
    
    dtp<- mp7()
    dtp$Date_Rec <-format(dtp$Date_Rec, "%b %d, %Y")
    dtp$Date_Dep <-format(dtp$Date_Dep, "%b %d, %Y")
    dtp$Date_Coll <-format(dtp$Date_Coll, "%b %d, %Y")
    
    datatable(dtp[,1:8],
              rownames = FALSE,
              
              
              options = list(
                columnDefs = list(list(className = 'dt-center', targets = 0:7)),
                initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'color' : '#344b5c'});}")
              ))
  }
  )
  
  
  observeEvent(m7(), {
    saveDatar(m7())
  })
  

  
  saveDatar <- function(datar) {

  

    datar <- m7()
    
    
    
    if(TRUE%in% c( rbind(loadDatar(), datar)$code %in%loadDatac()$code  )){
      datar<-   rbind( loadDatar(), datar)[-c(which(c(rbind(loadDatar(), datar)$code %in%loadDatac()$code  ) == TRUE)), ]
    }else if(TRUE%in% c( datar$code %in%loadDatar()$code  )){
      datar <- rbind(loadDatar(), datar[-c(which(datar$code %in%loadDatar()$code  )),  ])
    }else{
      datar <- rbind( loadDatar(), datar)
      
    }
    
    
    datar
    
    sheet_write(datar, ss= table2, sheet = "Received")
    
    }

    

 ranger1<- reactive(
   {
     if("" %in% input$delR){
       ranger1 <- "0"
     }else{
       ranger1 <-as.numeric(input$delR)+1
     }
     ranger1
   }
 )
 ranger2<- reactive(
   {
     if("" %in% input$delR2){
       ranger2 <- "0"
     }else{
       ranger2 <-as.numeric(input$delR2)+1
     }
     ranger2
   }
 )
 
 ranger<- reactive(
   {
     if(ranger1() != "0" & ranger2() != "0"){

       ranger <-paste0(as.character(ranger1()), ":", as.character(ranger2() ))
     }else{
       ranger <- "0"
     }
}
 )

  observeEvent(c(ranger(),input$refresh),
               {

                   if(ranger() != "0"){
                   range_delete(table2, sheet = "Received", range = ranger())
                   }
                 
               }
               )

  
  loadDatar <- reactive({
    

    data<-  read_sheet(table2, sheet = "Received", col_names = T)
    

    
    
    if(TRUE%in% c(data$code %in%loadDatac()$code  )){
      data<- data[-c(which(c(data$code %in%loadDatac()$code  ) == TRUE)), ]
    }else{
      data<-data
      
    }
    data$Date_Coll <- as.Date(data$Date_Coll)
    data$Date_Rec <- as.Date(data$Date_Rec)
    
    data$Date_Dep<-as.Date(data$Date_Dep)
    data
    }
   
    
  ) 
  
  
  output$received <- DT::renderDataTable({
    dt<- loadDatar()
    dt$Date_Rec <-format(dt$Date_Rec, "%b %d, %Y")
    dt$Date_Dep <-format(dt$Date_Dep, "%b %d, %Y")
    dt$Date_Coll <-format(dt$Date_Coll, "%b %d, %Y")
    
    if("All" %in% input$filtera){
      datatable(dt[,1:8],
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = 0:7)),
                  initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'color' : '#344b5c'});}")
                ))
    }else{
      
      datatable(dt[,1:8]%>%filter(Analysis == input$filtera),
                rownames = FALSE,
                
                
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = 0:7)),
                  initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'color' : '#344b5c'});}")
                ))
    }
    
  })
  
  output$vboxR <-renderUI({
    if("All" %in% input$filtera){
      
      div(class= "block2",
          HTML(paste0("Samples in Entire Queue: ", "<br> <i> <p style='color:white;font-size:60px;'>",  
                      nrow(loadDatar()),  "</i> </p>"))
      )
    }else{
      div(class= "block2",
          HTML(paste0("Samples in ", input$filtera,  " Queue: ", "<br> <i> <p style='color:white;font-size:60px;'>",  
                      nrow(loadDatar()%>%filter(Analysis == input$filtera)),  "</i> </p>"))
      )
    }
  })
  

  
  
  
  mc <- eventReactive(input$enterc, {
    
    mc <- as.data.frame(expand.grid(unlist(strsplit(input$sitec, split=" ")),
                                    unlist(strsplit(input$analysisc, split=" ")),
                                    unlist(strsplit(input$samplec, split=" ")),
                                    unlist(strsplit(input$adsorbc, split=" "))
    ) )
    
    
    mc <- data.frame(
      Date_Rec = c(rep(input$dateAc , nrow(mc))),
      Site = mc[,1],
      Analysis = mc[,2],
      Sample = mc[,3],
      Adsorbent = mc[,4]
    )
    mc <- data.frame(
      Date_Rec = as.character(mc$Date_Rec),
      Site = mc$Site,
      Analysis = mc$Analysis,
      Sample = mc$Sample,
      Adsorbent = mc$Adsorbent,
      code = as.factor(with(mc,paste(Date_Rec,Site,Analysis, Sample, Adsorbent, sep = "_" )))
    )
    
    
    
    
    
    
  }
  )
  
  
  
  mc1 <- eventReactive( mc(),{
    if(
      is.integer0(which((mc()$Analysis == 'Metals' | mc()$Analysis == 'Bulk-Chem') & mc()$Sample == "Passive"))){
      mc1 <- mc()
    }else{
      mc1 <- mc()[-c(which((mc()$Analysis == 'Metals' | mc()$Analysis == 'Bulk-Chem') & mc()$Sample == "Passive")), ]
    }
    mc1
  }
  
  )
  
  mc2 <- eventReactive( mc1(),{
    if(
      is.integer0(which((mc1()$Sample == 'Grab') & mc1()$Adsorbent != "NA"))){
      mc2 <- mc1()
    }else{
      mc2 <- mc1()[-c(which((mc1()$Sample == 'Grab') & mc1()$Adsorbent != "NA")), ]
    }
    mc2
  }
  )
  
  mc3 <-  eventReactive( mc2(),{
    if(
      is.integer0(which((mc2()$Sample == 'Passive') & mc2()$Adsorbent == "NA"))){
      mc3 <- mc2()
    }else{
      mc3 <- mc2()[-c(which((mc2()$Sample == 'Passive') & mc2()$Adsorbent == "NA")), ]
    }
    mc3
  }
  )
  
  mc4 <-  eventReactive( mc3(),{
    if(
      is.integer0(which( mc3()$Analysis == 'GC-MS' & mc3()$Sample == 'Passive' & mc3()$Adsorbent != "C18"))){
      mc4 <- mc3()
    }else{
      mc4 <- mc3()[-c(which( mc3()$Analysis == 'GC-MS' & mc3()$Sample == 'Passive' & mc3()$Adsorbent != "C18")), ]
    }
    mc4
  }
  )
  
  mc5 <-  eventReactive( mc4(),{
    if(
      is.integer0(which( mc4()$Analysis == 'LC-MS/MS' & mc4()$Sample == 'Passive' & mc4()$Adsorbent != "OASIS-HLB"))){
      mc5 <- mc4()
    }else{
      mc5 <- mc4()[-c(which( mc4()$Analysis == 'LC-MS/MS' & mc4()$Sample == 'Passive' & mc4()$Adsorbent != "OASIS-HLB")), ]
    }
    mc5
  }
  )
  
  mc6 <-  eventReactive( mc5(),{
    if(
      is.integer0(which( mc5()$Analysis == 'qPCR' & mc5()$Sample == 'Passive' & mc5()$Adsorbent %in% c("OASIS-HLB", "C18")))){
      mc6 <- mc5()
    }else{
      mc6 <- mc5()[-c(which( mc5()$Analysis == 'qPCR' & mc5()$Sample == 'Passive' & mc5()$Adsorbent %in% c("OASIS-HLB", "C18"))), ]
    }
    mc6
  }
  )
  
  
  mcp <- eventReactive(input$previewc, {
    
    mcp <- as.data.frame(expand.grid(unlist(strsplit(input$sitec, split=" ")),
                                    unlist(strsplit(input$analysisc, split=" ")),
                                    unlist(strsplit(input$samplec, split=" ")),
                                    unlist(strsplit(input$adsorbc, split=" "))
    ) )
    
    
    mcp <- data.frame(
      Date_Rec = c(rep(input$dateAc , nrow(mcp))),
      Site = mcp[,1],
      Analysis = mcp[,2],
      Sample = mcp[,3],
      Adsorbent = mcp[,4]
    )
    mcp <- data.frame(
      Date_Rec = as.character(mcp$Date_Rec),
      Site = mcp$Site,
      Analysis = mcp$Analysis,
      Sample = mcp$Sample,
      Adsorbent = mcp$Adsorbent,
      code = as.factor(with(mcp,paste(Date_Rec,Site,Analysis, Sample, Adsorbent, sep = "_" )))
    )
    
    
    
    
    
    
  }
  )
  
  
  
  mcp1 <- eventReactive( mcp(),{
    if(
      is.integer0(which((mcp()$Analysis == 'Metals' | mcp()$Analysis == 'Bulk-Chem') & mcp()$Sample == "Passive"))){
      mcp1 <- mcp()
    }else{
      mcp1 <- mcp()[-c(which((mcp()$Analysis == 'Metals' | mcp()$Analysis == 'Bulk-Chem') & mcp()$Sample == "Passive")), ]
    }
    mcp1
  }
  
  )
  
  mcp2 <- eventReactive( mcp1(),{
    if(
      is.integer0(which((mcp1()$Sample == 'Grab') & mcp1()$Adsorbent != "NA"))){
      mcp2 <- mcp1()
    }else{
      mcp2 <- mcp1()[-c(which((mcp1()$Sample == 'Grab') & mcp1()$Adsorbent != "NA")), ]
    }
    mcp2
  }
  )
  
  mcp3 <-  eventReactive( mcp2(),{
    if(
      is.integer0(which((mcp2()$Sample == 'Passive') & mcp2()$Adsorbent == "NA"))){
      mcp3 <- mcp2()
    }else{
      mcp3 <- mcp2()[-c(which((mcp2()$Sample == 'Passive') & mcp2()$Adsorbent == "NA")), ]
    }
    mcp3
  }
  )
  
  mcp4 <-  eventReactive( mcp3(),{
    if(
      is.integer0(which( mcp3()$Analysis == 'GC-MS' & mcp3()$Sample == 'Passive' & mcp3()$Adsorbent != "C18"))){
      mcp4 <- mcp3()
    }else{
      mcp4 <- mcp3()[-c(which( mcp3()$Analysis == 'GC-MS' & mcp3()$Sample == 'Passive' & mcp3()$Adsorbent != "C18")), ]
    }
    mcp4
  }
  )
  
  mcp5 <-  eventReactive( mcp4(),{
    if(
      is.integer0(which( mcp4()$Analysis == 'LC-MS/MS' & mcp4()$Sample == 'Passive' & mcp4()$Adsorbent != "OASIS-HLB"))){
      mcp5 <- mcp4()
    }else{
      mcp5 <- mcp4()[-c(which( mcp4()$Analysis == 'LC-MS/MS' & mcp4()$Sample == 'Passive' & mcp4()$Adsorbent != "OASIS-HLB")), ]
    }
    mcp5
  }
  )
  
  mcp6 <-  eventReactive( mcp5(),{
    if(
      is.integer0(which( mcp5()$Analysis == 'qPCR' & mcp5()$Sample == 'Passive' & mcp5()$Adsorbent %in% c("OASIS-HLB", "C18")))){
      mcp6 <- mcp5()
    }else{
      mcp6 <- mcp5()[-c(which( mcp5()$Analysis == 'qPCR' & mcp5()$Sample == 'Passive' & mcp5()$Adsorbent %in% c("OASIS-HLB", "C18"))), ]
    }
    mcp6
  }
  )
  
  output$previewc <-DT::renderDataTable({
    dtcp<- mcp6()
    dtcp$Date_Rec <-format(dtcp$Date_Rec, "%b %d, %Y")
    
    datatable(dtcp[,1:5],
              rownames = FALSE,
              
              
              options = list(
                columnDefs = list(list(className = 'dt-center', targets = 0:4)),
                initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'color' : '#344b5c'});}")
              ))
  }
  )
  
  observeEvent(mc6(), {
    saveDatac(mc6())
  })
  
  saveDatac <- function(datac) {
    datac <- mc6()

    
    if(TRUE%in% c( datac$code %in%loadDatac()$code  )){
      datac <- rbind(loadDatac(), datac[-c(which(datac$code %in%loadDatac()$code  )),  ])
    }else{
      datac <- rbind( loadDatac(), datac)
      
    }
    
    
    # The data must be a dataframe rather than a named vector
    
    sheet_write(datac, ss= table1, sheet = "completed")
    
  }
  
  
  
  
  
  
  loadDatac <- function() {
    
    
    # Read all the files into a list
    data<- read_sheet(table1, sheet = "completed")
    data$Date_Rec <-as.Date(data$Date_Rec)
    
    data
    
    
  }
  
  output$completed<- DT::renderDataTable({
    dtc<- loadDatac()
    dtc$Date_Rec <-format(dtc$Date_Rec, "%b %d, %Y")
    
    if("All" %in% input$filtera){
      datatable(dtc[,1:5],
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = 0:4)),
                  initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'color' : '#344b5c'});}")
                ))
    }
    
    else {
      datatable(dtc[,1:5]%>%filter(Analysis == input$filtera),
                rownames = FALSE,
                
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = 0:4)),
                  initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'color' : '#344b5c'});}")
                ))
    }
    
  })
  
  
  
  output$vboxC <-renderUI({
    if("All" %in% input$filtera){
      div(class= "block2",
          HTML(paste0("Total Samples Analyzed: ", "<br> <i> <p style='color:white;font-size:60px;'>",  
                      nrow(loadDatac()),  "</i> </p>"))
      )
    }else{
      div(class= "block2",
          HTML(paste0(input$filtera, " Samples Analyzed: ", "<br> <i> <p style='color:white;font-size:60px;'>",  
                      nrow(loadDatac()%>%filter(Analysis == input$filtera)),  "</i> </p>")))
    }
  })
  
  rangec1<- reactive(
    {
      if("" %in% input$delC){
        rangec1 <- "0"
      }else{
        rangec1 <-as.numeric(input$delC)+1
      }
      rangec1
    }
  )
  rangec2<- reactive(
    {
      if("" %in% input$delC2){
        rangec2 <- "0"
      }else{
        rangec2 <-as.numeric(input$delC2)+1
      }
      rangec2
    }
  )
  
  rangec<- reactive(
    {
      if(rangec1() != "0" & rangec2() != "0"){
        
        ranger <-paste0(as.character(rangec1()), ":", as.character(rangec2() ))
      }else{
        ranger <- "0"
      }
    }
  )
  
  observeEvent(c(rangec(),input$refresh),
               {
                 
                 if(rangec() != "0"){
                   range_delete(table1, sheet = "completed", range = rangec())
                 }
                 
               }
  )
  
  
  observeEvent(input$refresh, {
    refresh()
  })

  

  
  observe({
    if ("Select All" %in% input$sall_site) {
      
      updateCheckboxGroupInput(
        inputId="site",
        choices = c(levels(ids$loc)),
        selected = c(levels(ids$loc)))
      
    }else if("De-Select All" %in% input$sall_site)  {
      updateCheckboxGroupInput(
        inputId="site",
        choices = c(levels(ids$loc)),
        selected = c())
    }
    
    else if("Manual Select" %in% input$sall_site) {
      updateCheckboxGroupInput(
        inputId="site",
        choices = c(levels(ids$loc)))
      
    }
  })
  
  observe({
    if ("Select All" %in% input$sall_sitec) {
      
      updateCheckboxGroupInput(
        inputId="sitec",
        choices = c(levels(ids$loc)),
        selected = c(levels(ids$loc)))
      
    }else if("De-Select All" %in% input$sall_sitec)  {
      updateCheckboxGroupInput(
        inputId="sitec",
        choices = c(levels(ids$loc)),
        selected = c())
    }
    
    else if("Manual Select" %in% input$sall_sitec) {
      updateCheckboxGroupInput(
        inputId="sitec",
        choices = c(levels(ids$loc)))
      
    }
  })
  
  observe({
    if ("Select All" %in% input$sall_anal) {
      
      updateCheckboxGroupInput(
        inputId="analysis",
        choices = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"),
        selected = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"))
      
    }else if("De-Select All" %in% input$sall_anal)  {
      updateCheckboxGroupInput(
        inputId="analysis",
        choices = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"),
        selected = c())
    }
    
    else if("Manual Select" %in% input$sall_anal) {
      updateCheckboxGroupInput(
        inputId="analysis",
        choices = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"))
      
    }
  })
  
  observe({
    if ("Select All" %in% input$sall_analc) {
      
      updateCheckboxGroupInput(
        inputId="analysisc",
        choices = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"),
        selected = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"))
      
    }else if("De-Select All" %in% input$sall_analc)  {
      updateCheckboxGroupInput(
        inputId="analysisc",
        choices = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"),
        selected = c())
    }
    
    else if("Manual Select" %in% input$sall_analc) {
      updateCheckboxGroupInput(
        inputId="analysisc",
        choices = c("qPCR", "GC-MS", "LC-MS/MS", "Bulk-Chem", "Metals"))
      
    }
  })
  
  observe({
    if ("Select All" %in% input$sall_ads) {
      
      updateCheckboxGroupInput(
        inputId="adsorb",
        choices = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"),
        selected = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"))
      
    }else if("De-Select All" %in% input$sall_ads)  {
      updateCheckboxGroupInput(
        inputId="adsorb",
        choices = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"),
        selected = c())
    }
    
    else if("Manual Select" %in% input$sall_ads) {
      updateCheckboxGroupInput(
        inputId="adsorb",
        choices = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"))
      
    }
  })
  
  observe({
    if ("Select All" %in% input$sall_adsc) {
      
      updateCheckboxGroupInput(
        inputId="adsorbc",
        choices = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"),
        selected = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"))
      
    }else if("De-Select All" %in% input$sall_adsc)  {
      updateCheckboxGroupInput(
        inputId="adsorbc",
        choices = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"),
        selected = c())
    }
    
    else if("Manual Select" %in% input$sall_adsc) {
      updateCheckboxGroupInput(
        inputId="adsorbc",
        choices = c("GAC", "Filter", "C18", "NA" , "OASIS-HLB"))
      
    }
  })
  
  
}


shinyApp(ui, server)
