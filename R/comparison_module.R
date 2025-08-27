####
#### MODULE DESCRIPTION
####

# This module make the comparison with the compare_shp_files function

####
#### MODULE INPUTS
####

# dbx_shp from the module upload_dbx_data_server 
# new_shp from the module upload_updated_data_server 

####
#### MODULE OUTPUS
####

# This module shows the results of the comparison within the 'Comparison panel'
# and return the result of the compare_shp_files function

####
#### Packages and functions
####

# This module requires the package sf, DT and dplyr


comparison_ui <- function(id) {
   
   ns <- NS(id)
   tagList(
      
      verbatimTextOutput(output = ns('summary')),
      DTOutput(output = ns('comp'))
      
   )
}


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