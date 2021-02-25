#' AOI Draw
#' @description Draw an Area of Interest (AOI) interactively using a shiny app.
#' @return An sf object.
#' @export
#'
#' @examples
aoi_draw <- function() {


  shiny::shinyApp(

    ui = shiny::fluidPage(leaflet::leafletOutput('aoi'), shiny::selectInput('type', 'Please select a drawing option', choices = c('Multiple' = 'multi', 'Single' = 'sing'))
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

      shiny::observeEvent(input$aoi_draw_new_feature, {


changing_type <- reactive(input$type)

        if(changing_type() == 'sing'){

        feat <- input$aoi_draw_new_feature
        coords <- unlist(feat$geometry$coordinates)
        coords <- matrix(coords, ncol = 2, byrow = T)

        feature_type <- input$aoi_draw_new_feature$properties$feature_type

        if(feature_type %in% c("rectangle","polygon")){

        shape <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf()

        } else {

          shape <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf() %>% sf::st_cast("POINT")

        }
        maps <- shiny::reactive(shape)



        leaflet::leafletProxy('aoi') %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


        map_update <- shiny::reactive({ viz_A() %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
            leaflet::addPolygons(data = maps() )})


        output$aoi <- leaflet::renderLeaflet({

          map_update() %>%
            leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = T, markerOptions = T,
                                           circleMarkerOptions = T, polygonOptions = T)
        })
        } else if (changing_type() == 'multi'){


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

          }


        aoi <<- maps() %>% dplyr::mutate(ID = dplyr::row_number())




      }
      )

      }

    ) #end shinyApp

}
