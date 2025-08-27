#' UI module for uploading an updated shapefile
#'
#' This module provides a UI to upload a shapefile, select a column,
#' and confirm that the updated file is ready.
#'
#' @param id A character string; the module's namespace ID.
#'
#' @return A Shiny UI element (tagList) for file input, selectInput, and checkbox.
#'
#' @export

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


#' Server module for uploading an updated shapefile
#'
#' This module handles the server-side logic for uploading a shapefile,
#' renaming a column, and returning the updated `sf` object.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return A reactive expression returning the updated `sf` object if the checkbox is checked, or NULL otherwise.
#'
#' @export
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
