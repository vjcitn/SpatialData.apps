
library(shiny)
library(SpatialData)
library(SpatialData.plot)
library(SpatialData.data)
library(ggmap)
library(ggplot2)
library(sf)
library(DT)
if (!exists("brdat")) brdat = Breast2fov_10x()
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
 #    nd = data.frame(x=xs, y=ys)
 #    p1 = p1+geom_path(data=nd, aes(x=x,y=y), colour="red", linewidth=3)
 #    }

 if (!("pick" %in% shapeNames(brdat))) shapes(brdat)$pick = picked


docrop = function(spdat=brdat, cropview="pick") {
xs <<- NULL
ys <<- NULL
baseplot = plotSpatialData() + plotShape(spdat, c="black") + geom_path(data=pick, aes(x=x,y=abs(y)), colour="red", linewidth=2)  # FIXME pick not general
brshpoly = st_as_sf(shape(spdat)@data)
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("region selector app for Janesick Xenium Rep 1 example"),
   helpText("select center of circle"),
   DT::dataTableOutput("pathdat"),
   actionButton("usepath", "use"), 
   actionButton("closepath", "closepath"), 
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
     ),
    tabPanel("crop2",
     plotOutput("crop2", width="800px", height="800px"),
     )
    )
   )
  )
 )

server = function(input, output, session) {
 output$crop = renderPlot({
   plotSpatialData() + plotShape(spdat, cropview, c="black")
   })
 output$crop2 = renderPlot({
   shapes(spdat)$crop2 = closeddf()
   plotSpatialData() + plotShape(spdat, "crop2", c="black")
   })

 output$cells = renderPlot({
   input$plot_click
   input$clearpath
   input$closepath
#   p1 = plotSpatialData() + plotShape(spdat, c="black") + geom_path(data=pick, aes(x=x,y=abs(y)), colour="red",  # FIXME pick not general
#      linewidth=3)  # full dataset
   extra = NULL
   if (length(xs)>1) {
     nd = data.frame(x=xs, y=ys)
print(xs)
print(nd)
     extra = geom_path(data=nd, aes(x=x,y=y), colour="red", linewidth=3)
     }
   if (!is.null(extra)) baseplot + extra
   else baseplot
   })
 output$pathdat <- DT::renderDataTable({
           input$clearpath
           input$closepath
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
    input$closepath
    data.frame(x=xs, y=ys)
    })
 closeddf <- eventReactive(input$closepath, {
    input$clearpath
    ans = data.frame(x=xs, y=ys)
    ans = rbind(ans, ans[1,])
    xs <<- ans[,1]  # update vecs used for rendering on base plot
    ys <<- ans[,2]
    pick_poly = st_polygon(list(abs(data.matrix(ans))))
    trim = st_intersects(brshpoly, pick_poly, sparse=FALSE)
    shtrim = brshpoly[i=which(trim[,1]),]
    ShapeFrame(as.data.frame(shtrim))
    })
 observeEvent(input$clearpath, {
    xs <<- NULL
    ys <<- NULL
    })
    
}

runApp(list(ui=ui, server=server))
}

