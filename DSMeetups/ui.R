#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(rCharts)
library(leaflet)
# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "WDSDC Membership Data for the First Year", titleWidth=450),
  dashboardSidebar(
    width=450,
    sidebarMenu(
    menuItem("State Membership for the First Year of WDSDC", tabName = "tab3", icon = icon("map-o")),
    menuItem("First Year Membership for All DC2 Meetups", tabName = "tab1", icon = icon("line-chart")), menuItem("Overlap within DC2 Meetups", tabName = "tab2", icon = icon("cube")) )),
  dashboardBody(
    tags$head(tags$style(HTML(".leaflet-container {
    background: #000;
                              }"))),
    tabItems(
      tabItem(tabName="tab3", leafletOutput('map', height=800)),
      tabItem(tabName="tab1", showOutput('chart1', "highcharts")),
      tabItem(tabName="tab2", plotOutput("plot1", height = "1px"), showOutput('chart2', "polycharts"))
    )
    
    
              ),
    skin="blue")