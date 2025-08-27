####
#### MODULE DESCRIPTION
####

# This module allow the user to update the data on dropbox and/or to download
# the results of the comparison

####
#### MODULE INPUTS
####

# dbx_shp from the module upload_dbx_data_server
# new_shp from the module upload_updated_data_server
# comparison$full_comp from the module comparison_server

# The date of the update is the current date by default, but the user can change it

####
#### MODULE OUTPUS
####

# The 'Update files on Dropbox' button will move files from the main folder to the archived folders
# and save the updated files (.gpkg for the crowns vector data and .xlsx for the comparison file)

# The "Download the comparison data" button will download the current comparion as .xlsx file


####
#### Packages and functions
####

# This module requires the package sf, xlsx and dplyr

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
