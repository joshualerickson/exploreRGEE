#' AOI Draw
#' @description Draw an Area of Interest (AOI) interactively using a shiny app.
#' @return An sf object.
#' @export
#' @importFrom shiny isolate observeEvent
#'
#' @examples
aoi_draw <- function() {


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

    server = function(input, output, session) {

      output$aoi <- leaflet::renderLeaflet({

        viz_A() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
          leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
          leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = T,
                                         circleMarkerOptions = F, polygonOptions = T)


      })


      values <- shiny::reactiveValues()
      values$sf <- sf::st_sf(sf::st_sfc(crs = 4326))
 observeEvent(input$clear, {

            values$sf <- NULL

            leaflet::leafletProxy('aoi') %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


            map_update <- shiny::reactive({ viz_A() %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
            })


            output$aoi <- leaflet::renderLeaflet({

              map_update() %>% leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
                leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = T,
                                               circleMarkerOptions = F, polygonOptions = T)
            })

          })
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



  aoi <<- maps() %>% dplyr::mutate(ID = dplyr::row_number())


observeEvent(input$finish, {

  shiny::stopApp()

})


        }
          )

        }

           ) #end shinyApp

        }
