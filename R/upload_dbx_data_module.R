#' UI module for uploading a Dbx shapefile
#'
#' @param id Shiny module id
#'
#' @return Shiny UI elements for uploading a shapefile and selecting a column
#' @export

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

#' Server module for uploading a Dbx shapefile
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return A reactive list containing the shapefile (`shp`) and selected site (`site`) when checkbox is checked
#' @export

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
