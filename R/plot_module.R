####
#### MODULE DESCRIPTION
####

# This module plots the result of the comparison as an interactive map within the panel 'Map'

####
#### MODULE INPUTS
####

# dbx_shp from the module upload_dbx_data_server
# new_shp from the module upload_updated_data_server

####
#### MODULE OUTPUS
####

# This module shows the results of the comparison within the 'Comparison panel'
# and retrun the result of the compare_shp_files function

####
#### Packages and functions
####

# This module requires the package sf, leaflet and dplyr

plot_ui <- function(id) {

   ns <- NS(id)
   tagList(

      tabPanel('Map', leafletOutput(outputId = ns('plot_shp'), width = "100%", height = 1000))

   )

}

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
