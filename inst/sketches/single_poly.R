# demonstrate non-robust production of a single polygon via clicks
# path might have intersecting edges, maybe that's ok

library(shiny)
library(ggplot2)

datp = data.frame(x=c(1,1,2,2), y=c(1,2,1,2))

pathdf = data.frame()
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("show path"),
   actionButton("close", "closepath"),
   actionButton("stopapp", "stopapp"),
   width=2
  ),
  mainPanel(
   plotOutput("forpath", click="click_inp")
  )
 )
)

server = function(input, output, session) {
 output$forpath = renderPlot({
  newx = input$click_inp$x
  newy = input$click_inp$y
  p = ggplot() + geom_point(data=datp, aes(x=x,y=y))  # initial display
  if (nrow(pathdf)==0) {   # initial path info
     pathdf <<- data.frame(x=newx, y=newy)
     }
  else {   # update path
      pathdf <<- rbind(pathdf, data.frame(x=newx, y=newy))
      if (input$close != 0) pathdf <<- rbind(pathdf, pathdf[1,])
      if (nrow(pathdf)>1)  p = p + geom_path(data=pathdf, aes(x=x,y=y))  # avoid "group" challenge message
      else  p = p + geom_point(data=pathdf, aes(x=x,y=y))  # avoid "group" challenge message
      }
  p
 })
 observeEvent(input$stopapp, {
  stopApp()
  })
}

runApp(list(ui=ui, server=server))
