#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(Hmisc)
library(rCharts)
library(shiny)
library(maps)
library(leaflet)
library(htmlwidgets)
load("data/dc2data.Rdata")
load("data/dc2data2.Rdata")
load("data/dc2data3.Rdata")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  stateFromLower <-function(x) {
    #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
    st.codes<-data.frame(
      state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                        "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                        "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                        "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                        "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
      full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                       "connecticut","district of columbia","delaware","florida","georgia",
                       "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                       "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                       "missouri","mississippi","montana","north carolina","north dakota",
                       "nebraska","new hampshire","new jersey","new mexico","nevada",
                       "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                       "rhode island","south carolina","south dakota","tennessee","texas",
                       "utah","virginia","vermont","washington","wisconsin",
                       "west virginia","wyoming"))
    )
    #create an nx1 data.frame of state codes from source column
    st.x<-data.frame(state=x)
    #match source codes with codes from 'st.codes' local variable and use to return the full state name
    refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
    #return the full state names in the same order in which they appeared in the original source
    return(refac.x)
    
  } 
  #Plot membership data
  output$chart1=renderChart({
    h=hPlot(x="day", y="value", data=DC2data, group="meetup", type="line", title="Meetup Membership Over the First Year")
    h$xAxis(title=list(text="Days"))
    h$yAxis(title=list(text="Members"))
    h$set(dom="chart1")
    h$chart(zoomType = 'xy', height=800)
    return(h)
    })
  output$chart2=renderChart({
    p5 <- rPlot(x = 'x', y = 'y', color = 'value', data = df4, type = 'tile', tooltip= "#! function(item){ 
              return item.value} 
                !#")
    p5$guides(x=list(levels=namesvec, title="", numticks=9))
    p5$guides(y=list(levels=namesvec, title="", numticks=9))
    p5$guides("{color: {scale: {type: gradient, lower: blue, upper: red}}}")
    p5$set(title = "Membership Overlap")
   p5$set(dom="chart2", width = session$clientData$output_plot1_width, height=800)
    return(p5)
  })
  output$map=renderLeaflet({
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
    mapStates <- map("state", fill = TRUE,
                     plot = FALSE,
                     region = as.character(statecountdf$wdsdcstates2))
    statenames=mapStates$names
    colvec=vector("character", length(statenames))
    textvec=vector("character", length(statenames))
    labelvec=vector("character", length(statenames))
    for(i in 1:length(statecountdf$wdsdcstates2)){
     colvec[grep(statecountdf$wdsdcstates2[i], statenames)]=statecountdf$color[i] 
     textvec[grep(statecountdf$wdsdcstates2[i], statenames)]=paste(simpleCap(as.character(statecountdf$wdsdcstates[i])), ":", "\n", as.character(statecountdf$Freq[i]), " Members", sep="")
     labelvec[grep(statecountdf$wdsdcstates2[i], statenames)]=simpleCap(as.character(statecountdf$wdsdcstates[i]))
    }
    your.map <- leaflet() %>% 
      addPolygons(data=mapStates, stroke = TRUE, color="#000000", fillColor=colvec, popup=textvec, opacity = 1.0, fillOpacity = 1.0, weight=3) %>%
      addLegend(position = 'bottomright', colors = col, labels = as.character(sort(cntu, decreasing = TRUE)), opacity = 1.0,
                title = 'Members') 
      
    your.map
    
    
    
  })
})
