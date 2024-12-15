

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

docrop = function(spdat, shapeind = 1, tableind=1, cropview_name="pick", 
 shape_feature_name = "location_id", table_feature_id = "cell_id") {
   baseplot = plotSpatialData() + plotShape(spdat, i=shapeind, c="black")
   brshpoly = st_as_sf(shape(spdat, i=shapeind)@data)
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
#      nt = tables(spdat)[[ cropview_name ]]
#print(nt)
#      pts = data.frame(x=nt$xloc, y=nt$yloc)
#print(head(pts))
      plotSpatialData() + plotShape(spdat, cropview_name, c="black") # +
#           geom_point(data=pts, aes(x=x,y=y))
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
     else {   # update path, shape component, and table component
         pathdf <<- rbind(pathdf, data.frame(x=newx, y=newy))
         if (input$closepath != 0) {
            pathdf <<- rbind(pathdf, pathdf[1,])
            pick_poly = st_polygon(list(abs(data.matrix(pathdf))))
            trim = st_intersects(brshpoly, pick_poly, sparse=FALSE)
            shtrim = brshpoly[i=which(trim[,1]),]
            shapes(spdat)[[cropview_name]] <<- ShapeFrame(as.data.frame(shtrim))
            intab = tables(spdat)[[tableind]]
            tables(spdat)[[cropview_name]] <<- intab[, which(intab[[table_feature_id]] %in%
                 shapes(spdat)[[cropview_name]]@data[[shape_feature_name]]) ]
#            centers = shtrim |> st_centroid() |> st_coordinates()
#            tables(spdat)[[cropview_name]]$xloc <<- centers[,1]
#            tables(spdat)[[cropview_name]]$yloc <<- centers[,2]
            }
         if (nrow(pathdf)>1)  p = p + geom_path(data=pathdf, aes(x=x,y=y), colour="red", linewidth=2)  # avoid "group" challenge message
         else  p = p + geom_point(data=pathdf, aes(x=x,y=y))  # avoid "group" challenge message
         }
     p
    })
   
       
   }
   
   runApp(list(ui=ui, server=server))
}
   
