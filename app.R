#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
fact<-function(n){
  f = 1
  l = c()
  for (i in (0:n)){
    p = range(1,n-i)
    l=append(l,p[2])
  }
  j = 1
  for (i in l){
    j =  j * i
  }
  return (j) 
} 
choose1<-function(n,k){
  b = n - k
  g = fact(b) 
  l=fact(n)
  p = fact(k)
  s=l/(p*g)
  return (s) 
}
bigfact<-function(n,k){
  l = c()
  for (i in (0:n)){
    p = range(1,n-i)
    l=append(l,p[2])
  }
  j = 1
  h = n - k
  G=l[0:h]
  for (i in G){
    j =  j * i
  }
  return(j)
}



choose<-function(n,k){
  b = n - k
  if (b >= k){
    d = bigfact(n,b)
    g = fact(k)
  }
  else{
    d = bigfact(n,k)
    g = fact(b)
  }
  s=d/(g)
  return(s) 
}


bi <- function(n,k,p){
  j <- choose(n,k)*(p^k)*((1-p)^(n-k))
  return(j)
}



bidist <- function(n,k,p){
  j = 0
  for (i in 0:k){
    j = bi(n,i,p) + j
  }
  return(j)
}

plotsss<-function(x,p){
  l <- function(i) {
    j<-bi(x,i,p)
    return(j)
  }
  u <- function(i) {
    s<-bidist(x,i,p)
    return(s)
  }
  Y<-sapply(0:x,l)
  X <- 0:x
  da<-data.frame(X,Y)
  p<-ggplot(data=da, aes(X,Y))+
    geom_col(aes(fill=Y)) +
    + 
    scale_colour_brewer(palette="BuPu", direction=-1)
    scale_y_continuous("f(x)") +
    scale_x_continuous("x") +
    ggtitle("Binomial PDF ")
  return(p)
}



CDF<-function(x,p){
  u <- function(i) {
    s<-bidist(x,i,p)
    return(s)
  }
  Y2<-sapply(0:x,u)
  X2 <- 0:x
  d <- data.frame(X2,Y2)
  l<-ggplot(d,aes(X2,Y2))+
    geom_line() +
    scale_y_continuous("F(x)") +
    scale_x_continuous("x") +
    ggtitle("Binomial CDF")
  return(l)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Binomial Distribution Calculator"),

    sidebarPanel(
      numericInput("obs1", "n:", 100, min = 1, max =500),
      numericInput("obs2", "p:", 0.5, min = 0, max =1),
      numericInput("obs3", "x:",50, min = 0, max =500),
      h5("f(x):", align = "left"),
      verbatimTextOutput("val2"),
      h5("p(X =< x):", align = "left"),
      verbatimTextOutput("val3")
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("Plot")
  
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      plotsss(input$obs1,input$obs2)
    })
    output$Plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      CDF(input$obs1,input$obs2)
    })
    output$val2 <- renderText({bi(input$obs1,input$obs3,input$obs2)})
    output$val3 <- renderText({bidist(input$obs1,input$obs3,input$obs2)})
}

# Run the application 
shinyApp(ui = ui, server = server)
