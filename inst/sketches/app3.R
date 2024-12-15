

library(shiny)
library(SpatialData)
library(SpatialData.plot)
library(SpatialData.data)
library(ggmap)
library(ggplot2)
library(sf)
library(DT)

# set up spatial data

if (!exists("brdat")) brdat = Breast2fov_10x()
if (!exists("ludat")) ludat = Lung2fov_10x()

 if (!("pick" %in% shapeNames(brdat))) shapes(brdat)$pick = picked

docrop = function(spdat, cropview="pick") {
   baseplot = plotSpatialData() + plotShape(spdat, c="black")
   brshpoly = st_as_sf(shape(spdat)@data)
   pathdf <<- data.frame()
   
   ui = fluidPage(
    sidebarLayout(
     sidebarPanel(
      helpText("region selector app for Janesick Xenium Rep 1 example"),
      actionButton("usepath", "use"), 
      actionButton("closepath", "closepath"), 
      actionButton("clearpath", "clearpath"), 
      actionButton("stopapp", "stop app"), 
      width=2
      ),
     mainPanel(
      tabsetPanel(
       tabPanel("cells",
        plotOutput("cells", width="900px", height="900px", click="click_inp"),
        ),
       tabPanel("cropped",
        plotOutput("cropped", width="900px", height="900px"),
        )
       )
      )
     )
    )
   
   
   server = function(input, output, session) {
    output$cropped = renderPlot({
      plotSpatialData() + plotShape(spdat, cropview, c="black")
      })
   
    observeEvent(input$clearpath, {
       pathdf <<- data.frame()
       })
    observeEvent(input$stopapp, {
       stopApp(spdat)
       })
   
    output$cells = renderPlot({
     newx = input$click_inp$x
     newy = input$click_inp$y
     p = baseplot #+ geom_point(data=datp, aes(x=x,y=y))  # initial display
     if (nrow(pathdf)==0) {   # initial path info
        pathdf <<- data.frame(x=newx, y=newy)
        }
     else {   # update path
         pathdf <<- rbind(pathdf, data.frame(x=newx, y=newy))
         if (input$closepath != 0) {
            pathdf <<- rbind(pathdf, pathdf[1,])
            pick_poly = st_polygon(list(abs(data.matrix(pathdf))))
            trim = st_intersects(brshpoly, pick_poly, sparse=FALSE)
            shtrim = brshpoly[i=which(trim[,1]),]
            shapes(spdat)[[cropview]] <<- ShapeFrame(as.data.frame(shtrim))
            }
         if (nrow(pathdf)>1)  p = p + geom_path(data=pathdf, aes(x=x,y=y), colour="red", linewidth=2)  # avoid "group" challenge message
         else  p = p + geom_point(data=pathdf, aes(x=x,y=y))  # avoid "group" challenge message
         }
     p
    })
   
       
   }
   
   runApp(list(ui=ui, server=server))
}
   
