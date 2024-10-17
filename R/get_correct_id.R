#' Correct the new file from the id find in the old version
#'
#' @description blabla
#'
#' @param new_shp blabla
#' @param old_shp blabla
#' @param crs blabla
#'
#'
#' @export
#'
#' @importFrom sf st_transform
#' @importFrom sf st_geometry
#' @importFrom sf st_centroid
#' @importFrom sf st_distance
#' @import dplyr

get_correct_id <- function(new_shp, old_shp, crs){

   # Fix crs
   old_shp <- old_shp %>% sf::st_transform(crs)
   new_shp <- new_shp %>% sf::st_transform(crs)

   # Fix the geometry names
   sf::st_geometry(old_shp) <- "geometry"
   sf::st_geometry(new_shp) <- "geometry"

   # Create new variables
   new_shp$idmodif_idn <- NA
   new_shp$idmodif_geom <- NA


   # Get corrected id from the id_n
   for(i in 1:nrow(new_shp)){

      if(!is.na(new_shp$id_n[i])){

         id <- old_shp$id[which(new_shp$id_n[i] == old_shp$id_n)]

         if(id != new_shp$id[i]){
            new_shp$id[i] = id
            new_shp$idmodif_idn[i] = 'OUI'
         }

      }

   }

   # Get corrected id from the centroid
   for(i in 1:nrow(new_shp)){

      dist <- sf::st_centroid(new_shp[i,'geometry']) %>%
         sf::st_distance( sf::st_centroid(old_shp$geometry)) %>%
         as.numeric()
      if(length(which(dist == 0)) == 1){same_geom <- old_shp$id[which(dist == 0)]}else{same_geom <- NULL}

      if(!is.null(same_geom)){

         if(!new_shp$id[i] == same_geom){
            new_shp$id[i] = same_geom
            new_shp$idmodif_geom[i] = 'OUI'
         }

      }

   }

   correct_id_by_idn <- new_shp %>% filter(idmodif_idn == 'OUI')
   correct_id_by_geom <- new_shp %>% filter(idmodif_geom == 'OUI')

   return(new_shp)

}
