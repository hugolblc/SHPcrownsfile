library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(sf)
library(tidyverse)
library(readxl)
library(DT)
library(leaflet)
library(openxlsx)
library(filesstrings)

ui <- fluidPage(
   theme = shinytheme("slate"),
   br(),
   br(),

   fluidPage(

      column(
         width = 10,
         upload_dbx_data_ui("upload_dbx_data")
      )
   ),

   fluidPage(

      column(
         width = 10,
         upload_updated_data_ui("upload_updated_data")
      ),

      column(
         width = 1,
         br(),
         actionButton("run", "Run")
      )
   ),

   navbarPage("Update crowns files",

              tabPanel("Comparison",
                       comparison_ui('summary')
              ),

              tabPanel("Map",
                       plot_ui('plot')),

              tabPanel("Updates files",
                       update_ui('update')),

              # inverse = T

   )

)


server <- function(input, output, session) {

      dbx_inputs <- callModule ( upload_dbx_data_server, 'upload_dbx_data') # automatical load dbx crowns file
      updated_inputs <- callModule ( upload_updated_data_server, 'upload_updated_data' ) # load selected last crown file version

      observeEvent(input$run, {

         # st_geometry(dbx_inputs()$shp) <- "geometry"
         # st_geometry(updated_inputs()) <- "geometry"

         comparison <- callModule(comparison_server,
                                  "summary",
                                  dbx_shp = dbx_inputs()$shp,
                                  new_shp = updated_inputs()) # make the comparison beetwen files


         callModule(plot_server,
                    'plot',
                    dbx_shp = dbx_inputs()$shp,
                    new_shp = updated_inputs(),
                    comparison = comparison$full_comp
         ) # plot the comparison

         callModule(update_server,
                    'update',
                    site = dbx_inputs()$site,
                    new_shp = updated_inputs(),
                    comparison = comparison$full_comp
         ) # updates dropbox folder or just download the comparison
      })
}

shinyApp(ui, server)
