cat("\014") 
graphics.off()
rm(list = ls())
options(java.parameters = "-Xmx8g")
options(encoding = 'UTF-8')
#1. LOAD HELPER,PACKAGES AND FUNCTIONS ----
#0.INSTALL/LOAD PACKAGES----
options(java.parameters = "-Xmx8g")
paquetes<-c("shiny","shinydashboard","data.table","dplyr","waiter","ggplot2","stringr","highcharter","shinyWidgets")
for(i in 1:length(paquetes))
{
  if(!require(paquetes[i],character.only = TRUE))
  {
    install.packages(paquetes[i])
  }
  library(paquetes[i],character.only = TRUE)
}
# Function

funtions_clean <- function(text){
  new_text <- tolower(text)
  new_text <- str_replace_all(new_text,"http\\S*", "")
  new_text <- str_replace_all(new_text,"[[:punct:]]", " ")
  new_text <- str_replace_all(new_text,"[[:digit:]]", " ")
  new_text <- str_replace_all(new_text,"[\\s]+", " ")
  return(new_text)
}


# Loader page
appCSS<-"#loader {
position: fixed;
z-index: 500;
margin-left: 0%;
margin-top: 0%;
height: 100%;
width: 100%;
background: rgba( 255, 255, 255, .8 ) url('AA.gif') 40% 25% no-repeat;
}"

# Load data
dta<-data.table::fread("~/Desktop/Supply_chain/DataCoSupplyChainDataset.csv",
                       colClasses = c('character','integer','integer','numeric','numeric','character','integer','integer','character','character','character','character','character','integer','character','character','character','character','character','integer','integer','character','numeric','numeric','character','character','character','integer','character','integer','integer','numeric','numeric','integer','numeric','numeric','integer','numeric','numeric','numeric','character','character','character','integer','integer','integer','logical','character','character','numeric','integer','character','character'))


newnames<-names(dta)
newnames<-funtions_clean(newnames)

newnames<-gsub(" ","_",newnames)
oldnames<-names(copy(dta))
data.table::setnames(dta,names(dta),newnames)

dta[,('quantity_order'):=length(unique(order_id)),by="category_name"]

#Define filter
desc<-data.table::fread("~/Desktop/Supply_chain/DescriptionDataCoSupplyChain.csv")
X=as.list(names(dta)[c(41,9,22)])
names(X)<-c(oldnames[c(41,9,22)])

Y=as.list(names(dta)[c(38,5,4,54)])
names(Y)<-c(oldnames[c(38,5,4)],"Orden Quantity")

XX=as.list(names(dta)[c(41,9,22)])
names(XX)<-oldnames[c(41,9,22)]

YY=as.list(c(names(dta)[c(6)],"time_delivery"))
names(YY)<-c(oldnames[c(6)],"Time Delivery")



###################
# Create the header for the ui.
###################
header <- shinydashboard::dashboardHeader(title = "Co Supply Chain")
###################
# Create the body for the ui. 

#########################
body <- shinydashboard::dashboardBody(
  shinyjs::useShinyjs(),
  waiter::useWaiter(),
  shinyjs::inlineCSS(appCSS),
  div(
    id = "loader",
  ),
  
  shinyjs::hidden(
    div(
      id = "main-page",
      shinydashboard::tabItems(
        
        ########################
        # First tab content
        ########################
        shinydashboard::tabItem(
          tabName = "plot",
          
          shiny::fluidRow(
            
            shinydashboard::box(status = "primary" ,width=12,
              highcharter::highchartOutput("plot1") 
              
            )
          )
          
          
          
        )
         ,
        
        
        
         
         shinydashboard::tabItem(
           tabName = "lm",
           
           shinydashboard::box(status = "primary" ,width=12,
                               highcharter::highchartOutput("plot2")) ,
           
           shinydashboard::box(status = "primary" ,width=12,
                               highcharter::highchartOutput("plot3")) 
           
           
           
           
         )
        
      )
      
    )
    
  ))      
###################
# Create the sidebar menu options for the ui.
###################
sidebar <- shinydashboard::dashboardSidebar(
  shinyjs::useShinyjs(),
  
  
  
  shinyjs::hidden(
    div(id="side_bar_tt",
        shinydashboard::sidebarMenu(id="sidebar",
                                    
                                    
                                    shinydashboard::menuItem("Order Analysis:", tabName = "plot", icon = shiny::icon("dashboard"))
                                     ,
                                    
                                     shinydashboard::menuItem("Late shipment Analysis:", tabName = "lm", icon = shiny::icon("dashboard"))
                                    
                                    
        )
        ,
        shiny::conditionalPanel(
          condition="input.sidebar == 'lm'",
          shinyWidgets::pickerInput("varxx","X", choices=XX,selected = XX[1] , options = list(`actions-box` = TRUE),multiple = FALSE)
          ,
          shinyWidgets::pickerInput("varyy","Y", choices=YY,selected = unlist(YY)[1], options = list(`actions-box` = TRUE),multiple = FALSE)
          ,
          shiny::conditionalPanel(
            condition="input.varyy == 'delivery_status'",
            
            shinyWidgets::pickerInput("top5","Select:", choices=NULL,selected = NULL, options = list(`actions-box` = TRUE),multiple = FALSE)
            
          )
          
          



        )
        ,
        shiny::conditionalPanel(
          condition="input.sidebar == 'plot'",
          shinyWidgets::pickerInput("varx","X", choices=X,selected = X[1] , options = list(`actions-box` = TRUE),multiple = FALSE)
          ,
          shinyWidgets::pickerInput("vary","Y", choices=Y,selected = Y[1], options = list(`actions-box` = TRUE),multiple = FALSE)
  
          
        )
        
    )
  )
)

#Server ----
# For all your server needs 
addResourcePath("tmpuser", getwd())
server <- function(input, output, session) {
  rv <- reactiveValues(html = NULL)
  loading_screen <-reactive({
    
    
    loading_screen <-   withTags(
      # col
      div(
        
        img(src = "AA.gif",width="200" ,  height="200"  ,alt = "User Avatar")
      )
      
    )
    
    loading_screen
  })
  
  observe({
    
    rv$html<-shiny::HTML(as.character(loading_screen() ))
    
    
  })
  
  

  output$plot1 <- highcharter::renderHighchart({
    
    #status of all the products 
    df1 <- dta %>% mutate(var1=dta[[as.name(input$varx)]], var2=dta[[as.name(input$vary)]])
    dfr <- df1 %>% group_by(var1,var2) %>% group_by(var1) %>% summarise_at(c(input$vary),sum)
    dfr<-data.table::setDT(dfr)
    data.table::setnames(dfr,c("var1","var2"),c(input$varx,input$vary),skip_absent=TRUE)
    data.table::setorderv(dfr,input$vary,-1)
    
      dfr=head(dfr,10)
    
    dat<-dfr
    
    column_1 <- rlang::sym(input$varx)
    column_2 <- rlang::sym(input$vary)
    
     hc<-dat %>% 
        highcharter::hchart("column", highcharter::hcaes(x=UQ(column_1), y =UQ(column_2),color=UQ(column_1)),showInLegend = FALSE
                            ,tooltip = list(valueDecimals=2,pointFormat = " Y: <b>{point.y}</b>"))
     hc
    })
  
  output$plot2 <- highcharter::renderHighchart({
    print(input$varyy)
    
    print(input$varxx)
    if(input$varyy=="delivery_status")
    {
      column_1 <- rlang::sym(as.character(input$varxx))
      column_2 <- rlang::sym(as.character(input$varyy))
      dfa=data.table::copy(dta)
      
      
      dfa<-dfa[,.(n=.N),by=c(input$varxx,input$varyy)]
      
      
      
        hc<-dfa %>% 
          highcharter::hchart("column",
                              highcharter::hcaes(x=UQ(column_1),  y = n, group = UQ(column_2)),showInLegend = FALSE
                             
                              )  
        
        
    }
    
    if(input$varyy=="time_delivery")
    {
      column_1 <- rlang::sym(as.character("days_for_shipping_real_"))
      column_2 <- rlang::sym(as.character("days_for_shipment_scheduled_"))
      column_3 <- rlang::sym(as.character(input$varxx))
      dfa=data.table::copy(dta)
      
      
      dfa<-dfa[,.(days_for_shipping_real_=sum(days_for_shipping_real_),days_for_shipment_scheduled_=sum(days_for_shipment_scheduled_)),by=c(input$varxx)]
      
      
      
      hc<-dfa %>% 
        highcharter::hchart('scatter',
                            highcharter::hcaes(x=UQ(column_1),  y = UQ(column_2), group = UQ(column_3)),showInLegend = FALSE
                            
        )  
    }
    hc
  })
  
  
  
  output$plot3 <- highcharter::renderHighchart({
    print(input$varyy)
    
    print(input$varxx)
    if(input$varyy=="delivery_status")
    {
      column_1 <- rlang::sym(as.character(input$varxx))
      column_2 <- rlang::sym(as.character(input$varyy))
      dfa=data.table::copy(dta)
      
      
      dfa<-dfa[,.(n=.N),by=c(input$varxx,input$varyy)]
      
      dfa[,aux:= .SD, .SDcols = c(input$varxx)]
      
      dfa=dfa[aux==input$top5]
      
      
      hc<-dfa %>% 
        highcharter::hchart("column",
                            highcharter::hcaes(x=UQ(column_1),  y = n, group = UQ(column_2)),showInLegend = FALSE
                            
        )  
      
      
    }
    
    if(input$varyy=="time_delivery")
    {
      column_1 <- rlang::sym(as.character("days_for_shipping_real_"))
      column_2 <- rlang::sym(as.character("days_for_shipment_scheduled_"))
      column_3 <- rlang::sym(as.character(input$varxx))
      dfa=data.table::copy(dta)
      
      
      dfa<-dfa[,.(range_of_lateness=sum(days_for_shipping_real_-days_for_shipment_scheduled_)),by=c(input$varxx)]
      
      data.table::setorderv(dfa,"range_of_lateness",-1)
      dfa<-head(dfa,10)
      
      hc<-dfa %>% 
        highcharter::hchart('column',
                            highcharter::hcaes(x=UQ(column_3),  y = "range_of_lateness", color = UQ(column_3)),showInLegend = FALSE
                            
        )  
    }
    hc
  })
  
  
  updateTop<-reactive({
    column_1 <- rlang::sym(as.character(input$varxx))
    column_2 <- rlang::sym(as.character(input$varyy))
    dfa=data.table::copy(dta)
    
    
    dfa<-dfa[,.(n=.N),by=c(input$varxx,input$varyy)]
    
    dfa[,aux:= .SD, .SDcols = c(input$varxx)]
    
    dfa[,N:= sum(n), by = c(input$varxx)]
    setorderv(dfa,"N",-1)
    dfa<-unique(dfa[,c(input$varxx,"N"),with=FALSE])
    
    dfa<-head(dfa[,1],5)
    dfa<-as.vector(t(dfa))
    
  })
  
  
  observeEvent(input$varxx,{
    shinyWidgets::updatePickerInput(session, "top5",choices = updateTop(),selected =updateTop()[1])
  })
  
  
  
  
  
  
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::hide(id = "loader", anim = TRUE, animType = "fade")    
  shinyjs::show("main-page")
  shinyjs::show("side_bar_tt")
  shinyjs::show(id = "sidebarCollapsed" )
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  
  
  
  
  
}
###################
# Initializes the ui. 
# Used to load in your header, sidebar, and body components.
###################

ui <- dashboardPage(
  header = header,
  sidebar =  sidebar,
  body = body)

shiny::shinyApp(ui, server)


