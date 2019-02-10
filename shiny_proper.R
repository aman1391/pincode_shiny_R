library(sp)
library(leaflet)
library(dplyr)
library(geosphere)
library(htmltools)
library(shiny)


ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("Show the nearby pincodes"),
  textInput("Pincode", "Pincode", "744101"),
  verbatimTextOutput("value"),
  submitButton("Submit")
    ),
  mainPanel(
  leafletOutput("mymap")
  )
  )
))

df1=read.csv("/Users/Admin/Downloads/pin code data.csv",stringsAsFactors = F)
df1=df1[!duplicated(df1$postal.code),]
df1$label=paste("(",df1$place.name,":",df1$postal.code,")",sep="")

quakeIcons <- iconList(orange = makeIcon("/Users/Admin/Desktop/red.png", iconWidth = 24, iconHeight =32))
df2=df1 

server <- shinyServer(function(input, output, session) {
  output$mymap <- renderLeaflet({
    lat=df1$latitude[df1$postal.code==input$Pincode]
    long=df1$longitude[df1$postal.code==input$Pincode]
    pin=df1$label[df1$postal.code==input$Pincode]
    city=df1$admin.name1[df1$postal.code==input$Pincode]
    
    df2$dist=NA
    df2=df2 %>% filter(admin.name1==city)
    for(i in 1:nrow(df2)){
      df2$dist[i]=distm(c(long,lat), c(df2$longitude[i],df2$latitude[i]))
    }
    
    df2=df2 %>% arrange(dist) %>% filter(dist>0& dist<=10000)
    df_plot <- data.frame(longitude = df2$longitude,latitude = df2$latitude, pin=df2$label)
    
    leaflet() %>% addMarkers(data=df_plot,~longitude, ~latitude,label = ~as.character(pin)) %>% 
      addMarkers(lat=lat,lng=long,label = as.character(pin),icon=quakeIcons[1],labelOptions(noHide=T,permanent=T)) %>% addTiles()
    
  })
})

shinyApp(ui, server)
