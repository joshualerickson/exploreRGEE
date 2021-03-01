#' AOI Draw
#' @description Draw an Area of Interest (AOI) interactively using a shiny app.
#' @param user_shape A provided sf object to view alongside map.
#' @param ... additional arguments for mapview.
#' @return An sf object.
#' @export
#' @importFrom shiny isolate observeEvent
#'
#' @examples \dontrun{
#' # Load Libraries
#'
#' library(rgee)
#' rgee::ee_intialize()
#' library(exploreRGEE)
#'
#' huc <- exploreRGEE::huc
#'
#' # without providing a sf object
#'
#' aoi_draw()
#'
#' # with
#'
#' aoi_draw(user_shape = huc)
#' }
#'
aoi_draw <- function(user_shape = NULL, ...) {


  shiny::shinyApp(

    ui = shiny::fluidPage(shiny::tags$head(
      shiny::tags$style(shiny::HTML("
                  .btn {
                    color: white;
                    background-color: #f44336;
                    display:inline-block;
                    font-size: 20px;
                    padding: 14px 40px;
                    margin-top: 10px;
                    margin-right: 10px;
                    border-radius: 12px;
                    border: 1px solid black;

                    }

                    "))
    ),shiny::tabPanel("Map", style = "height:92vh;",leaflet::leafletOutput('aoi', width = "100%", height = "100%"),
                          shiny::actionButton('clear', "Clear Map"),
                          shiny::actionButton('finish', "Finish Locations"))
    ),


    #server
    server = function(input, output, session) {

      output$aoi <- leaflet::renderLeaflet({

        if(is.null(user_shape)){

        viz_A() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4)%>%
            leaflet.extras::addMeasurePathToolbar()  %>%
            leaflet.extras::addStyleEditor(position = "topright",
                                           openOnLeafletDraw = F)  %>%
            leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(autoCollapse = TRUE, minLength = 2,
                                                                                 hideMarkerOnCollapse = TRUE, zoom = 14)) %>%
          leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
          leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                         rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                         markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                         polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
                                         editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = TRUE,allowIntersection = TRUE))
        } else {

            viz_A() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
            leaflet.extras::addMeasurePathToolbar()  %>%
            leaflet.extras::addStyleEditor(position = "topright",
                                           openOnLeafletDraw = F)  %>%
            leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(autoCollapse = TRUE, minLength = 2,
                                                                                 hideMarkerOnCollapse = TRUE, zoom = 14)) %>%
            leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
            leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                           rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                           markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                           polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
                                           editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = TRUE,allowIntersection = TRUE)) %>%
            clearLeafAOI(mapview::mapview(user_shape, ...))

          }

      })

    #store the sf in a reactiveValues
    values <- shiny::reactiveValues()
    values$sf <- sf::st_sf(sf::st_sfc(crs = 4326))

    #set to null if clear and reset map
    shiny::observeEvent(input$clear, {

            values$sf <- NULL

            leaflet::leafletProxy('aoi') %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


            map_update <- shiny::reactive({ viz_A() %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
            })


            output$aoi <- leaflet::renderLeaflet({

              if(is.null(user_shape)){

                map_update() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
                  leaflet.extras::addMeasurePathToolbar()  %>%
                  leaflet.extras::addStyleEditor(position = "topright",
                                                 openOnLeafletDraw = F)  %>%
                  leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(autoCollapse = TRUE, minLength = 2,
                                                                                       hideMarkerOnCollapse = TRUE, zoom = 14)) %>%
                  leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                                 rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                                 markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                                 polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
                                                 editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = TRUE,allowIntersection = TRUE))
              } else {

                map_update() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
                  leaflet.extras::addMeasurePathToolbar()  %>%
                  leaflet.extras::addStyleEditor(position = "topright",
                                 openOnLeafletDraw = F)  %>%
                  leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(autoCollapse = TRUE, minLength = 2,
                                                                                       hideMarkerOnCollapse = TRUE, zoom = 14)) %>%
                  leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                                 rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                                 markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                                 polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
                                                 editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = TRUE,allowIntersection = TRUE)) %>%
                  clearLeafAOI(mapview::mapview(user_shape, ...))

                }

            })

          })

     #update map with user input
     shiny::observeEvent(input$aoi_draw_new_feature, {

          feat <- input$aoi_draw_new_feature
          coords <- unlist(feat$geometry$coordinates)
          coords <- matrix(coords, ncol = 2, byrow = T)

          feature_type <- input$aoi_draw_new_feature$properties$feature_type

          if(feature_type %in% c("rectangle","polygon")){

            new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf())
            shiny::isolate(values$sf <- rbind(values$sf, new_sf))

          } else {

            new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf() %>% sf::st_cast("POINT"))
            shiny::isolate(values$sf <- rbind(values$sf, new_sf))

          }

          maps <- shiny::reactive(values$sf)

          #sf to return
          aoi <<- maps() %>% dplyr::mutate(ID = dplyr::row_number())

          #used to stop the app via button
          observeEvent(input$finish, {

          shiny::stopApp()

          })


        }
          )

        }

           ) #end shinyApp

}



# function to clear leaflet object and add mapview with aoi_draw()

clearLeafAOI <- function(leaf, data) {

  d_grp <- data@map$x$calls[[11]]$args[[1]]$group
  grp_names <- append(d_grp, leaf$x$calls[[8]]$args[[2]])

  calls <- data@map$x$calls
  app <- append(calls, leaf$x$calls)

  leaf$x$calls <- app

  leaf$x$calls[[19]]$args[[2]] <- grp_names

  leaf
}

# function to clear leaflet object and add mapview with viz()

clearLeafViz <- function(leaf, data) {

  d_grp <- data@map$x$calls[[11]]$args[[1]]$group
  grp_names <- append(d_grp, leaf$x$calls[[13]]$args[[2]])

  calls <- data@map$x$calls
  app <- append(calls, leaf$x$calls)

  leaf$x$calls <- app

  leaf$x$calls[[24]]$args[[2]] <- grp_names

  leaf
}


