
library(shiny)
library(SpatialData)
library(SpatialData.plot)
library(SpatialData.data)
library(ggmap)
library(ggplot2)
library(sf)
library(DT)
if (!exists("brdat")) brdat = Breast2fov_10x()

xs = NULL
ys = NULL
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("region selector app for Janesick Xenium Rep 1 example"),
   helpText("select center of circle"),
   DT::dataTableOutput("pathdat"),
   actionButton("usepath", "use"), 
   actionButton("clearpath", "clearpath"), 
   actionButton("stopapp", "stop app"), 
   width=2
   ),
  mainPanel(
   tabsetPanel(
    tabPanel("cells",
     plotOutput("cells", width="800px", height="800px", click="plot_click"),
     ),
    tabPanel("cropped",
     plotOutput("crop", width="800px", height="800px"),
     verbatimTextOutput("ans")
     )
    )
   )
  )
 )

server = function(input, output, session) {
 brsh = shape(brdat)
 # some points to form a subregion
 pick = structure(list(x = c(355.185319678255, 353.624236083554, 510.140679646861, 
    496.285253053229, 355.185319678255), y = c(-471.421489018199, 
    -591.83220661942, -601.010859954238, -467.21109758021, -471.421489018199
    )), row.names = c(NA, -5L), class = "data.frame")
 pick_poly = st_polygon(list(abs(data.matrix(pick))))
 brshpoly = st_as_sf(brsh@data)
 trim = st_intersects(brshpoly, pick_poly, sparse=FALSE)
 sh251 = brshpoly[i=which(trim[,1]),]
 picked = ShapeFrame(as.data.frame(sh251))
 if (!("pick" %in% shapeNames(brdat))) shapes(brdat)$pick = picked

 output$crop = renderPlot({
   plotSpatialData() + plotShape(brdat, "pick", c="black")
   })

 output$cells = renderPlot({
   input$plot_click
   input$clearpath
   p1 = plotSpatialData() + plotShape(brdat, c="black") + geom_path(data=pick, aes(x=x,y=abs(y)), colour="red",
      linewidth=3)  # full dataset
   if (length(xs)>1) {
     nd = data.frame(x=xs, y=ys)
     p1 = p1+geom_path(data=nd, aes(x=x,y=y), colour="red", linewidth=3)
     }
   p1
   })
 output$pathdat <- DT::renderDataTable({
           input$clearpath
           xs <<- c(xs, input$plot_click$x)
           ys <<- c(ys, input$plot_click$y)
           #cat(xs)
           pathdf()
         }, server=FALSE)
 observeEvent(input$stopapp, {
    stopApp()
    })
 pathdf <- eventReactive(input$usepath, {
    input$clearpath
    data.frame(x=xs, y=ys)
    })
 observeEvent(input$clearpath, {
    xs <<- NULL
    ys <<- NULL
    })
    
}

runApp(list(ui=ui, server=server))

