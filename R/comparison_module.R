#' UI module for comparing two shapefiles
#'
#' @description
#' Provides a Shiny UI for comparing two shapefiles, showing a summary and
#' a detailed comparison table.
#'
#' @param id Character. Shiny module id.
#'
#' @return A Shiny UI element (`tagList`) to be used in a Shiny app.
#' @export
comparison_ui <- function(id) {

   ns <- NS(id)
   tagList(

      verbatimTextOutput(output = ns('summary')),
      DTOutput(output = ns('comp'))

   )
}


#' Server module for comparing two shapefiles
#'
#' @description
#' Compares two sf objects (shapefiles) and provides outputs for a summary
#' and a detailed comparison table using DT.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param dbx_shp sf object. Reference shapefile.
#' @param new_shp sf object. New shapefile to compare.
#'
#' @return A list containing the comparison results.
#' @export
comparison_server <- function(input, output, session, dbx_shp, new_shp) {

   st_geometry(dbx_shp) <- "geometry"
   st_geometry(new_shp) <- "geometry"

   comparison <- compare_shp_files(dbx_shp = dbx_shp, new_shp = new_shp)

   output$summary <-  renderText({
      paste(comparison$summary$a,
            comparison$summary$b,
            comparison$summary$c,
            comparison$summary$d,
            comparison$summary$colomns_comp, sep="\n")
   })

   output$comp <- renderDT({

      datatable(
         comparison$full_comp %>% select(id, id_comp, idtax_f_dbx, idtax_f_new, idtax_f_comp, geom_comp),
         filter = 'top',
         style = "bootstrap4",
         options = list( pageLength = 10, autoWidth = TRUE,
         paging = FALSE, scrollY="300px", scrollX=TRUE, fixedColumns=TRUE
      ))
   })

   return(comparison)

}
