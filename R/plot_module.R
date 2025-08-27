#' UI module for interactive map of polygons
#'
#' @description
#' This function creates the UI part of a Shiny module displaying polygons on a Leaflet map.
#'
#' @param id Character. Shiny module id.
#'
#' @return A Shiny UI object (tabPanel with leafletOutput).
#' @export
plot_ui <- function(id) {

   ns <- NS(id)
   tagList(

      tabPanel('Map', leafletOutput(outputId = ns('plot_shp'), width = "100%", height = 1000))

   )

}


#' Server module for interactive map of polygons
#'
#' @description
#' This function handles the server logic for the Shiny module displaying polygons on a Leaflet map.
#' It compares two spatial datasets and highlights new, removed, or modified polygons.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @param dbx_shp sf object. Original polygons (e.g., from Dropbox).
#' @param new_shp sf object. New polygons to compare.
#' @param comparison data.frame. Comparison table with columns 'id_comp' and 'geom_comp' indicating new/removed/modified polygons.
#'
#' @return None. Creates a Leaflet map output in the module's UI.
#' @export
plot_server <- function(input, output, session, dbx_shp, new_shp, comparison) {

   st_geometry(dbx_shp) <- "geometry"
   st_geometry(new_shp) <- "geometry"

   new_shp <- new_shp %>% st_transform("WGS84")
   dbx_shp <- dbx_shp %>% st_transform("WGS84")

   new <- comparison %>% filter(id_comp == 'created') %>% .[['id']]
   removed <- comparison %>% filter(id_comp == 'removed') %>% .[['id']]

   geom_modif <- comparison %>% filter(geom_comp == 'polygon modified') %>% .[['id']]

   new_id <- new_shp %>% filter(id %in% new)
   geom_modif <- new_shp %>% filter(id %in% geom_modif)
   removed <- dbx_shp %>% filter(id %in% removed)

   output$plot_shp <-  renderLeaflet({

      leaflet() %>%
         addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%

         addPolygons(data = dbx_shp, color = 'green', group = 'Dropbox', label = ~paste(id,':',idtax_f)) %>%
         addPolygons(data = new_shp, color = 'red', group = 'My file', label = ~paste(id,':',idtax_f)) %>%
         addPolygons(data = new_id, color = 'blue', group = 'New crowns', label = ~paste(id,':',idtax_f)) %>%
         addPolygons(data = geom_modif, color = 'black', group = 'Geom change', label = ~paste(id,':',idtax_f)) %>%
         addPolygons(data = removed, color = 'white', group = 'Ids removed', label = ~paste(id,':',idtax_f)) %>%

         addLegend(color = "red", labels = 'myfile') %>%
         addLegend(color = "green", labels = 'dropbox file') %>%
         addLegend(color = "blue", labels = 'new crowns') %>%
         addLegend(color = "black", labels = 'Geom change') %>%
         addLegend(color = "white", labels = 'Ids removed') %>%

         addLayersControl(overlayGroups = c('Geom change',"Dropbox", "My file", 'New crowns', 'Ids removed'),
                          options = layersControlOptions(collapsed = FALSE))
   })

}
