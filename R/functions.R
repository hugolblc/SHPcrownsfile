#' compare_shp_files
#'
#' This function compares two versions of the same crowns vector data. Vectors as
#' to be sf object and the name for the 'id', 'idtax_f' and 'geometry' have to be
#' 'id', 'idtax_f' and 'geometry'. The function then return a list with the summary
#' of the comparison as character and the full comparison as tibble.
#'
#' @param dbx_shp the last Dropbox version
#' @param new_shp the update version
#'
#' @return a list with $summary and $full_comp. summary contains the summary of
#' the comparison as character and full_comp the full comparison as tibble
#'
#' @examples
#' st_geometry(dbx_shp) <- "geometry"
#' st_geometry(new_shp) <- "geometry"
#' compare_shp_files(dbx_shp, new_shp)
#'
#' @import dplyr sf
#' @export

compare_shp_files <- function(dbx_shp, new_shp){

################################################################################
####################### CREATE THE FULL COPARISON TIBBLE #######################

   st_geometry(dbx_shp) <- "geometry"
   st_geometry(new_shp) <- "geometry"

   dbx_shp <- dbx_shp %>% st_transform(st_crs(new_shp))


   dbx_shp <- dbx_shp %>% dplyr::mutate(centroid = st_centroid(geometry))
   new_shp <- new_shp %>% dplyr::mutate(centroid = st_centroid(geometry))


   full <- dplyr::full_join(as_tibble(dbx_shp),
                     as_tibble(new_shp),
                     by = 'id',
                     suffix = c("_dbx", "_new")) %>%

      dplyr::mutate(id_dbx = if_else(id %in% unique(dbx_shp$id), TRUE, FALSE),
             id_new = if_else(id %in% unique(new_shp$id), TRUE, FALSE),

             id_comp =
                dplyr::case_when(
                   id_dbx == FALSE & id_new == TRUE ~ 'created',
                   id_dbx == TRUE & id_new == FALSE ~ 'removed',
                   id_dbx == TRUE & id_new == TRUE ~ 'already present'
                   ),

             idtax_f_comp =
                dplyr::case_when(
                   id_comp == 'removed' ~ 'id removed',
                   id_comp == 'created' & idtax_f_new != 0 ~ 'new id identified',
                   id_comp == 'created' & idtax_f_new == 0 ~ 'new id indet',
                   idtax_f_dbx == 0 & idtax_f_new == 0 ~ 'remained indet',
                   id_comp == 'already present' & idtax_f_new == idtax_f_dbx ~ 'same identification',
                   idtax_f_dbx == 0 & idtax_f_new != 0 & id_dbx == TRUE ~ 'identification added',
                   idtax_f_dbx != 0 & idtax_f_new == 0 & id_dbx == TRUE ~ 'identification removed',
                   id_comp == 'already present' & idtax_f_new != idtax_f_dbx ~ 're-identified'),

                   geom_comp =
                dplyr::case_when(
                         id_comp == 'removed' ~ 'id removed',
                         id_comp == 'created' & st_is_empty(geometry_new) == FALSE ~ 'polygon created',
                         id_comp == 'created' & st_is_empty(geometry_new) == FALSE ~ 'new id emptypolygon',
                         id_comp == 'already present' & st_is_empty(geometry_dbx) & st_is_empty(geometry_new) == FALSE ~ 'polygon created',
                         st_is_empty(geometry_dbx) & st_is_empty(geometry_new) ~ 'empty polygon',
                         id_comp == 'already present' & st_is_empty(geometry_dbx) == FALSE & st_is_empty(geometry_new) ~ 'empty polygon',
                         (!is.na(as.numeric(st_distance(centroid_new, centroid_dbx, by_element = TRUE)) == 0)) & (as.numeric(st_distance(centroid_new, centroid_dbx, by_element = TRUE)) == 0)  ~ 'same polygon',
                         (!is.na(as.numeric(st_distance(centroid_new, centroid_dbx, by_element = TRUE)) > 0)) & as.numeric(st_distance(centroid_new, centroid_dbx, by_element = TRUE)) > 0  ~ 'polygon modified'
                         )) %>%

      dplyr::select(id, id_dbx, id_new, id_comp,
             idtax_f_dbx, idtax_f_new, idtax_f_comp,
             geometry_dbx, geometry_new, geom_comp)



   full[sapply(full, is.character)] <- lapply(full[sapply(full, is.character)], as.factor)

################################################################################
#######################   CREATE THE SUMMARY COMPARISON  #######################

   a <- paste('There is', nrow(full %>%
                                  dplyr::filter (id_comp == 'created')), 'new ids')
   b <- paste('There is', nrow(full %>%
                                  dplyr::filter (id_comp == 'removed')), 'ids removed')
   c <- paste('There is',  nrow(full %>%
                                   dplyr::filter (idtax_f_comp %in% c('re-identified', 'identification removed', 'identification added' ))), 'ids that have been re-identified')
   d <- paste('There is', nrow(full %>%
                                  dplyr::filter(geom_comp == 'polygon modified')), 'ids that have been re-delimited')


   if (ncol(new_shp) == ncol(dbx_shp)) {

      if (unique(colnames(new_shp) == colnames(new_shp)[colnames(new_shp) %in% colnames(dbx_shp)]) == TRUE) {

         colomns_comp <- 'Columns are the same between the 2 files'

      } else {

         a.col <- colnames(new_shp)[!(colnames(new_shp) %in% colnames(dbx_shp))]
         b.col <- colnames(dbx_shp)[!(colnames(dbx_shp) %in% colnames(new_shp))]
         colomns_comp <- paste("There is", length(a.col) + length(b.col), "differences between columns")

      }

   } else {

      a.col <- colnames(new_shp)[!(colnames(new_shp) %in% colnames(dbx_shp))]
      b.col <- colnames(dbx_shp)[!(colnames(dbx_shp) %in% colnames(new_shp))]
      colomns_comp <- paste("There is", length(a.col) + length(b.col), "differences between columns")

   }

################################################################################
#######################              OUTPUT              #######################

   comp <- list(
      summary = list(a =a,
                     b=b,
                     c=c,
                     d=d,
                     colomns_comp =colomns_comp),
      full_comp = full
         )

   return(comp)

}
