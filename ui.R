shinyUI(fluidPage(#theme = "bootstrap.css",

    tags$h3("Status of Social Determinants of Health Indicators and Disparities for California Counties"),
  
  
  
sidebarPanel( 
  
  conditionalPanel(condition = paste("input.ID == ",c(1,7,2,3,4,5,6),c(rep("|",6),""),  collapse=""), 
    selectInput("myV1","Indicator 1:",choices=list("Poverty"=1,"Parks"=2,"Healthy Food"=3,"Child Neglect"=4,"Alcohol Outlets"=5), selected=1),selectize=FALSE),
 
 conditionalPanel(condition = paste("input.ID == ",c(3),c(rep("|",0),""),  collapse=""),checkboxInput("myTwo", "TWO Indicators",value=FALSE)),
 
 
#  conditionalPanel(condition = paste0("input.ID == ",c(3),c(rep("|",0)),collapse=""), selectInput("myV2X","Indicator 2:",choices=list("Poverty"=1,"Parks"=2,"Healthy Food"=3,"Child Neglect"=4,"Alcohol Outlets"=5),selected=2)),  # "none"=0,
 

 conditionalPanel(condition = paste0("input.myTwo & (", paste0("input.ID == ",c(3),c(rep("|",0)),collapse=""),")",collapse=""), selectInput("myV2X","Indicator 2:",choices=list("Poverty"=1,"Parks"=2,"Healthy Food"=3,"Child Neglect"=4,"Alcohol Outlets"=5),selected=2)),  # "none"=0,

conditionalPanel(condition = paste0("input.myTwo  & (", paste0("input.ID == ",c(3),c(rep("|",0)),collapse=""),")",collapse=""), checkboxInput("mCI","Include CI?")),
                 
 
 conditionalPanel(condition = paste0("input.ID == ",c(1,7,2),c(rep("|",2),""),collapse=""), selectInput("myV2","Indicator 2:",choices=list("Poverty"=1,"Parks"=2,"Healthy Food"=3,"Child Neglect"=4,"Alcohol Outlets"=5),selected=2)),  
 
 
#conditionalPanel(condition = paste("input.ID == ",c(5,6),c(rep("|",1),""),  collapse=""),selectInput("myRace","Race/Ethnic Group",choices = r1name, selected="Total")),

conditionalPanel(condition = paste("input.ID == ",c(1,7),c(rep("|",1),""),  collapse=""),checkboxInput("mylim100", "full axis range",value=TRUE)),
conditionalPanel(condition = paste("input.ID == ",c(1,7,2),c(rep("|",2),""),  collapse=""),radioButtons("myGeo", "Geographic Level:",choices=c("CO","CT"))),

conditionalPanel(condition = paste("input.ID == ",c(1,7,3),c(rep("|",2),""),  collapse=""),selectInput("myGeoname", "Location:",choices = c("California",lhjL) )),

conditionalPanel(condition = paste("input.ID == ",c(2),c(rep("|",0),""),  collapse=""),radioButtons("myD", label=NULL,choices=c("Density","Boxplot"))),
                 
hr(),

helpText("Data Source: California Department of Public Health","Office of Health Equity",
         tags$a(href="https://www.cdph.ca.gov/Programs/OHE/Pages/Healthy-Communities-Data-and-Indicators-Cover-Page.aspx",
                "Healthy Communities Data and Indicators Project (HCI)")),

hr(),
includeText("Text1.txt"),    
hr(),
helpText(h6("for questions or suggestions please email: samfam921@gmail.com"))   #,style="color:blue",


),



mainPanel(
    
  
#  conditionalPanel(condition = "input.ID == 1",
#                   helpText("Two Indicator Scatterplot Showing Indicator Status by County and by Race/ethnicity.  This visualization allows the #exploration of two indicator value estimates simultaneously, by race-ethnicity and in relation to the state averages (cross lines in the plot). #Points in the upper right corner represent counties with estimates above state average that could be in higher need for interventions.")),   
  
  hr(), 
 
   tabsetPanel(type = "tabs", 
     tabPanel("Within-County Disparity",  plotOutput("bar1",height=500),     textOutput("barTxt"),    value=3),        #  ,width=800,height=600
     tabPanel("Scatterplot",              plotOutput("scatter1",height=500), textOutput("scatterTxt"), value=1),             
     tabPanel("Scatter Interative",       scatterD3Output("scatter2",height=500), value=7),      
     tabPanel("Distribition",             plotOutput("dist1"),  value=2),
     tabPanel("County Map",               plotOutput("map1"),  value=4),
     tabPanel("Census Tract Map",         plotOutput("map2"),  value=5),
     tabPanel("Census Tract Map - Zoom",  leafletOutput("map3",width=600,height=600),  value=6),
              id="ID")

  # could use includeText("Scatter.txt")textOutput("junk") for external file if more efficient
  
)))


#tabPanel("Scatterplot", plotOutput("scatter1", click = "scatter1_click"), verbatimTextOutput("click_info")),
