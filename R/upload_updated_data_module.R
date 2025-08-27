####
#### MODULE DESCRIPTION
####

# This module requires the user to select the last version of the crown vector data.
# And return the data as sf object.

####
#### MODULE INPUTS
####

# First the module requires the user to select the new crown vector file

# Then the user will have to select the idtax_f new file (the column corresponding to istax_f)

# And check the 'Updated done' button, the names 'idtax_f' and 'geometry' will be given
# to the corresponding column

####
#### MODULE OUTPUS
####

# return the new renamed file as sf object

####
#### Packages and functions
####

# This module requires the package sf and dplyr

# and use the function 
# dplyr::rename
# sf::read_sf
# sf::st_geometry

upload_updated_data_ui <- function(id) {
   
   ns <- NS(id)
   
   tagList(
      
      fluidRow(
         
         column(
            width = 5,
            fileInput(
               inputId = ns('shp_path'),
               'Input shapefile',
               accept = '.gpkg',
               buttonLabel = "Browse...",
               placeholder = ".gpkg",
               multiple = FALSE
            )
         ),
         
         column(
            width = 5,
               selectInput(
                  inputId = ns("idtax_new"),
                  label = "idtax_f new file :",
                  choices = '',
                  selected = ''
               )
         ),
         column(
            width = 2,
            br(),
            checkboxInput(
               inputId = ns("check_new"),
               label = "Updated file done",
               value = FALSE
            )
         )
      )
   )
}



upload_updated_data_server <- function(input, output, session) {
   
   data <- reactive({
      
      if (class(input$shp_path) !=  'NULL') {
         
         dir <- dirname(input$shp_path[1, 4])
         
         for (i in 1:nrow(input$shp_path)) {
            file.rename(input$shp_path[i, 4], paste0(dir, "/", input$shp_path[i, 1]))
         }
         
         getshp <- list.files(dir, pattern = "*.gpkg", full.names = TRUE)
         shp <- sf::read_sf(getshp)
         st_geometry(shp) <- "geometry"
         return(shp)
         
         
      }else{NULL}
      
   })
   
      observeEvent(input$shp_path, {
      
      updateSelectInput(session,
                        "idtax_new",
                        choices = c('',  colnames(data())),
                        selected = '')
   })
      
      up_new <- reactive({
         
         if (input$idtax_new != ''){
            data() %>% dplyr::rename( idtax_f = input$idtax_new)
         }  else { NULL}
         
      })
      
      out <- reactive({
         
         if(input$check_new == TRUE) {
            
            up_new()

            } else { NULL }
         
      })
      
      return(out)

}
