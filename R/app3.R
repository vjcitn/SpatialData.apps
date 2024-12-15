#
#
#
## set up spatial data
#
#if (!exists("brdat")) brdat = Breast2fov_10x()
#if (!exists("ludat")) ludat = Lung2fov_10x()
#
#
#' Use clicks on shape display of a SpatialData instance to define
#' a cropping region, produce a new ShapeFrame, visualize it, and
#' update the shapes and tables elements to respect the selected
#' shape elements.
#' @import shiny
#' @import SpatialData
#' @import SpatialData.plot
#' @import SpatialData.data
#' @import ggplot2
#' @import sf
#' @param spdat instance of SpatialData
#' @param shapeind numeric(1), index of shapes(spdat) to use as base display
#' @param tableind numeric(1) index of tables(spdat) to subset according to cropping
#' @param cropview_name character(1) name used for updating list elements
#' @param shape_feature_name character(1) string used to identify shape element identifiers in ShapeFrame,
#' defaults to "location_id"
#' @param table_feature_id character(1) strings used to identify element of colData that can be matched
#' by values of shape_feature_name in ShapeFrame of cropped SpatialData
#' @examples
#' brdat = SpatialData.data::Breast2fov_10x()
#' blk = crop_spd_app(brdat, shape_feature_name = "__index_level_0__")
#' blk
#' @export
crop_spd_app = function(spdat, shapeind = 1, tableind=1, cropview_name="pick", 
 shape_feature_name = "location_id", table_feature_id = "cell_id") {
   baseplot = plotSpatialData() + plotShape(spdat, i=shapeind, c="black")
   brshpoly = st_as_sf(shape(spdat, i=shapeind)@data)
   pathdf <<- data.frame()
   
   ui = fluidPage(
    sidebarLayout(
     sidebarPanel(
      helpText("region selector app"),
      helpText("Click on vertices of a region of interest.  The first click
          will not be visible, but additional clicks will produce a path.
          Do not close the region, but use the closepath button below to
          close.  The region will then be visible on the 'cropped' tab."),
      actionButton("closepath", "closepath"), 
      helpText("Stopping will return the cropped and updated instance to the session."),
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
        ),
       tabPanel("data",
        verbatimTextOutput("viewdat")
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
    output$viewdat = renderPrint({
      print(spdat)
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
