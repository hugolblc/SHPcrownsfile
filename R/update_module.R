#' Update UI module for comparison of crown data
#'
#' @description
#' This Shiny module creates a UI to upload old comparison files, update data on Dropbox,
#' and download the updated comparison dataset.
#'
#' @param id Character. Module namespace id.
#'
#' @return A Shiny UI tagList.
#'
#' @export
#'
#' @import sf dplyr

update_ui <- function(id) {

   ns <- NS(id)
   tagList(

      textInput(ns("date"),
                label = 'Date (YYYY_MM_DD)',
                value = format(Sys.Date(), "%Y_%m_%d"),
                width = NULL,
                placeholder = format(Sys.Date(), "%Y_%m_%d")),

      fileInput(
         inputId = ns('comp_path_old'),
         'Old xlsx comparison file',
         accept = '.xlsx',
         buttonLabel = "Browse...",
         placeholder = ".xlsx",
         multiple = FALSE
      ),

      actionButton(ns('update'),'Update files on Dropbox'),

      downloadButton(ns('download'),"Download the comparison data"),

      DTOutput(output = ns('id'))
   )

}


#' Server logic for update module
#'
#' @description
#' Server logic for `update_ui`. Handles reading uploaded files, updating comparison data,
#' and providing a downloadable updated file.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param site Character. Name of the site to use in output file naming
#' @param new_shp `sf` object. New shapefile with updated crowns
#' @param comparison `sf` object. Comparison data for crowns
#'
#' @return None. Provides reactive download via Shiny.
#'
#' @export
#' @import shiny sf readxl dplyr openxlsx
update_server <-

   function(input, output, session, site, new_shp, comparison) {

      st_geometry(new_shp) <- "geometry"

      data_xlsx <- reactive({

         if (class(input$comp_path_old) !=  'NULL') {

            dir <- dirname(input$comp_path_old[1, 4])

            for (i in 1:nrow(input$comp_path_old)) {
               file.rename(input$comp_path_old[i, 4], paste0(dir, "/", input$comp_path_old[i, 1]))
            }

            getshp <- list.files(dir, pattern = "*.xlsx", full.names = TRUE)
            dbx_xlsx <- readxl::read_excel(getshp)
            return(dbx_xlsx)


         }else{NULL}

      })

      output$download <- downloadHandler(
         filename = function() {
            paste0(site, "_crowns_comparison_", input$date, ".xlsx")
         },
         content = function(file) {
            req(input$date)
            req(data_xlsx())

            comparison_out <- comparison %>%
               dplyr::mutate(date = input$date) %>%
               dplyr::select(-c(geometry_dbx, geometry_new)) %>%
               dplyr::select(date, id, id_dbx, id_new, id_comp,
                             idtax_f_dbx, idtax_f_new, idtax_f_comp, geom_comp)

            history.xlsx <- as.data.frame(rbind(data_xlsx(), comparison_out))

            openxlsx::write.xlsx(
               history.xlsx,
               file,
               col.names = TRUE,
               row.names = FALSE
            )
         }
      )

      }
