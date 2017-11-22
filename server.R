shinyServer(function(input, output) {
  
 output$scatter1  <- renderPlot(myScatter1(input$myV1,input$myV2,input$mylim100,input$myGeo,input$myGeoname)                      )
 output$scatter2  <- renderScatterD3(myScatter2(input$myV1,input$myV2,input$mylim100,input$myGeo,input$myGeoname)              )
 
 
 output$dist1     <- renderPlot(myDistX(   input$myV1,input$myV2,myGeo=input$myGeo,myD=input$myD),height = 600, width = 600)
 output$bar1      <- renderPlot(myBarX(    input$myV1,myTwo=input$myTwo,myGeoname=input$myGeoname,myV2=input$myV2X,mCI=input$mCI))  #,height = 600, width = "auto",height = 400, width = 600
 output$map1      <- renderPlot(myMap1(    input$myV1),                         height = 600, width = 600)
 output$map2      <- renderPlot(myMap2(    input$myV1           ), height = 600, width = 600)
 output$map3      <- renderLeaflet(myMap3( input$myV1           ))
 
output$scatterTxt  <- renderText("Two Indicator Scatterplot Showing Indicator Status by County and by Race/ethnicity.  This visualization allows the #exploration of two indicator value estimates simultaneously, by race-ethnicity and in relation to the state averages (cross lines in the plot). Points in the upper right corner represent counties with estimates above state average that could be in higher need for interventions.")
 
output$barTxt    <-renderText("Bar chart Showing Within-County Differences by Race/ethnicity.  Absolute and relative disparities using the White population as reference group are shown at the bottom of the chart.  Relative disparities can be used to compare magnitude of disparities across indicators.")


 
 
 #output$click_info <- renderPrint(nearPoints(junk[,1],junk[,2], input$scatter1_click))
 
 
   })

