####
#### MODULE DESCRIPTION
####

# This module requires the user to choose the site of the data 'Mbalmayo' or
# 'Bouamir'and return a list with $shp : the dropbox version of the crowns vector data as sf object
#                                 $site : the name of the site as character objet

####
#### MODULE INPUTS
####

# First the module requires the user to select the site. From the site name, the
# last dropbox version of tyhe crown will be load

# Then the user will have to select the idtax_f dbx file (the column corresponding to istax_f)

# And check the 'Dbx done' button, the names 'idtax_f' and 'geometry' will be given
# to the corresponding column

####
#### MODULE OUTPUS
####

# return a list with $shp : the dropbox version of the crowns vector data as sf object with renamed columns
#                    $site : the name of the site as character objet

####
#### Packages and functions
####

# This module requires the package sf and dplyr

# and use the function
   # dplyr::rename
   # sf::read_sf
   # sf::st_geometry


upload_dbx_data_ui <- function(id) {

   ns <- NS(id)

   tagList(

      fluidRow(

         column(
            width = 5,
            fileInput(
               inputId = ns('shp_path_old'),
               'Old shapefile',
               accept = '.gpkg',
               buttonLabel = "Browse...",
               placeholder = ".gpkg",
               multiple = FALSE
            )
         ),

         column(
            width = 5,
               selectInput(
                  inputId = ns("idtax_dbx"),
                  label = "idtax_f dbx file :",
                  choices = '',
                  selected = NULL
               )
         ),
         column(
            width = 2,
            br(),
            checkboxInput(
               inputId = ns("check_dbx"),
               label = "Dbx done",
               value = FALSE
            )
         )
      )
   )
}


upload_dbx_data_server <- function(input, output, session) {

   data <- reactive({

      if (class(input$shp_path_old) !=  'NULL') {

         dir <- dirname(input$shp_path_old[1, 4])

         for (i in 1:nrow(input$shp_path_old)) {
            file.rename(input$shp_path_old[i, 4], paste0(dir, "/", input$shp_path_old[i, 1]))
         }

         getshp <- list.files(dir, pattern = "*.gpkg", full.names = TRUE)
         dbx_sf <- sf::read_sf(getshp)
         st_geometry(dbx_sf) <- "geometry"
         return(dbx_sf)


      }else{NULL}

   })

   observeEvent(input$shp_path_old, {

      updateSelectInput(session,
                        "idtax_dbx",
                        choices = c('',  colnames(data())),
                        selected = '')
   })


   up_dbx <- reactive({

      if (input$idtax_dbx != ''){
         data() %>% dplyr::rename( idtax_f = input$idtax_dbx)

      }  else { NULL}

   })

   out <- reactive({

      if(input$check_dbx == TRUE){

         list(shp = data(),
              site = input$site)

      }else{NULL}
})

   return(out)

}
