#Fourier
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)   #datasets to accompany Chihara and Hesterberg
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))


#The user interface
header <- dashboardHeader(title = "Fourier Analysis (Point Of Sale Stock) ",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(id="menu",
                                        p("Stock prices from Nov 16, 2020,",HTML("</br>"), "to  Nov 16, 2021"),
                                        menuItem("LSPD", tabName = "air"),
                                        menuItem("Shopify", tabName = "sunspot"),
                                        menuItem("Square", tabName = "flu")
                            ),
                            sliderInput("nbasis","Basis Functions to Display", min = 0, max = 9, value = 0),
                            sliderInput("ndisplay","Coefficients to Display", min = 20, max = 100, value = 40),
                            actionButton("btncalc","Calculate Fourier Coefficients"),
                            sliderInput("nrecon","Components for reconstruction", min = 1, max = 100, value = 5),
                            h4("Data sourced", a(href="https://help.yahoo.com/kb/SLN2311.html", "from"))
                           
  
)
body <- dashboardBody(
  tabItems(

    tabItem(tabName = "air",
            h2("LSPD")
    ),
    tabItem(tabName = "flu",
            h2("Square")
    ),
    tabItem(tabName = "sunspot",
            h2("Shopify"),
    )
  ),
  fluidRow(plotOutput("dataplot")),
  fluidRow(plotOutput("coefplot")),
  fluidRow(stylesheet,
    column(width = 6,
           tableOutput("tbl")  #display the data set
    ),
    column(width = 6,
          
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "purple") #other colors available

source("fcalc.R")

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  havecoeff <- FALSE
  colors = c("red", "blue", "green", "orange", "purple")
  dset <- numeric(0)
  N <- 0
  coefA <- numeric(0)
  coefB <- numeric(0)
  coefabs <- numeric(0)
  datamean <- numeric(0)
  showData <- function() plot(1:N,dset,type = "l")
  observeEvent(input$menu, {
    if(input$menu == "sunspot") {
      dset <<- read.csv("SHOP.csv")$Close
      N <<- length(dset)

    }
    if(input$menu == "air") {
      dset <<- read.csv("LSPD.csv")$Close
      N <<- length(dset)               
    }
    if(input$menu == "flu"){
      dset <<- read.csv("SQ.csv")$Close
      N <<- length(dset)
    }
    havecoeff <<- FALSE
    output$coefplot  <<- NULL
    output$dataplot <- renderPlot({showData()})
  })
  observeEvent(input$btncalc,{
    f <- fa.makeCoefficients(dset)
    datamean <<- as.numeric(f$mean)
    coefA <<- f$cos
    coefB <<- f$sin
    coefabs <<- f$abs
    havecoeff <<- TRUE
    output$coefplot <- renderPlot(barplot(f$abs[1:input$ndisplay],names.arg = 1:input$ndisplay ))
  })

  observeEvent(input$nrecon,{
    if (havecoeff == FALSE) return()
    recon <- fa.reconstruct(datamean, coefA, coefB, input$nrecon)
    output$dataplot <- renderPlot({showData()
      points(1:N,recon, type = "l", col = "red")})
  })
  observeEvent(input$nbasis,{
    output$dataplot <- renderPlot({
      showData()
      n <- input$nbasis
      if (n == 0) return()
      dmean <- mean(dset)
      points(1:N,rep(dmean,N), type = "l")
      if (n == 1) return()
      for (i in 1:floor(n/2)){
        points(1:N,dmean+(dmean-min(dset))*myCos(i,N), type = "l", col = colors[i], lwd = 2)
      }
      if (n == 2) return()
      for (i in 1:floor((n-1)/2)){
        points(1:N,dmean+(dmean-min(dset))*mySin(i,N), type = "l", col = colors[i], lwd = 2, lty = 2)
      }
    })
  })
}

#Run the app
shinyApp(ui = ui, server = server)