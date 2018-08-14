

#' An example dataset
#'
#' A dataframe containing multiple taxa occurrences
#'
#' @docType data
#' @keywords datasets
#' @name data.used
#' @usage data(data.used)
#' @format A data frame with 932 rows and 3 variables
NULL

# Multiple joining df
# Internal function for multiple joining. Obtained in stackoverflow https://stackoverflow.com/questions/34344214/how-to-join-multiple-data-frames-using-dplyr
.func <- function(...){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  col1 = colnames(df1)[1]
  col2 = colnames(df2)[1]
  xxx = dplyr::full_join(..., by = stats::setNames(col2,col1))
  return(xxx)
}

# Couting number of occurences within cell
#
# Internalfunction for counting occurrences within raster cell
#
.nbe_ind_cell <- function(x, rast_, rast_f) {
  rst <-
    raster::rasterize(sf::st_coordinates(x), rast_, fun='count')
  vals <-
    raster::getValues(rst)

  vals_tb <- dplyr::tibble(ID=1:raster::ncell(rast_f), val=vals)

  tb_filt <-
    dplyr::filter(vals_tb, !is.na(val))

  return(list(tb_filt))
}

# Multiple rasterinzing
#
# Internal function for rasterizing multiple df
.rast_sp <- function(x, rast_) raster::rasterize(sf::st_coordinates(x), rast_, fun='count')

#' Counting and rasterize multiple taxa occurrences
#'
#' Defining a grid of given resolution and counting occurrences for multiple taxa
#'
#' @param data A dataframe with taxa field, longitude and latitude
#' @param long A character string indicating the column name containing longitude in decimal degrees
#' @param lat A character string indicating the column name containing latitude in decimal degrees
#' @param sp A character string indicating the column name of taxa
#' @param resolution A numeric indicating the resolution in kilometres of the grid
#'
#' @export
grid_couting_tax <-
  function(data, long, lat, sp, resolution=10) {

    if(any(c(!is.character(long), !is.character(lat)))) stop("long and lat should be character strings")

    if(!is.character(sp)) stop("sp be character strings")

    if(!is.numeric(resolution)) stop("resolution should be numeric")

    data_sf <- sf::st_as_sf(data, coords = c(long, lat), crs = 4326)
    data_sf_proj <- sf::st_transform(x = data_sf, crs = 4087)
    Corners <- sf::st_bbox(data_sf_proj)

    ext = raster::extent(floor(Corners[1])-resolution*1000, floor(Corners[3])+resolution*1000,
                 floor(Corners[2])-resolution*1000, floor(Corners[4])+resolution*1000)
    r <-
      raster::raster(ext, resolution=resolution*1000, crs=sf::st_crs(data_sf_proj)$proj4string)

    ### all occurrences

    rast_full <-
      raster::rasterize(sf::st_coordinates(data_sf_proj), r, fun='count')

    rasterized_sp <-
      sapply(split(data_sf_proj, f=data_sf_proj$ID_Species), FUN = function(x) .rast_sp(x = x, rast_ = r))

    nbe_occ_cells_sp <-
      sapply(rasterized_sp, function(x) length(which(!is.na(raster::values(x)))))

    vals <-
      raster::getValues(rast_full)

    vals_tb <-
      dplyr::tibble(ID=1:raster::ncell(rast_full), val=vals)

    r.id <- r
    raster::values(r.id) <- 1:raster::ncell(r)
    ID_cell_all_occ <-
      raster::extract(r.id ,sf::st_coordinates(data_sf_proj))

    ### by species
    col <-
      data_sf_proj[,which(colnames(data_sf_proj)==sp)]
    sf::st_geometry(col) <- NULL

    nbe_ind_cell_sp <-
      sapply(split(data_sf_proj, f=dplyr::pull(col)), FUN = function(x) .nbe_ind_cell(x = x, rast_ = r, rast_f=rast_full))

    full_table <-
      Reduce(.func, nbe_ind_cell_sp)

    colnames(full_table) <-
      c("ID", names(nbe_ind_cell_sp))

    full_table <-
      dplyr::full_join(dplyr::filter(vals_tb, !is.na(val)), full_table, by=c("ID"="ID"))
    colnames(full_table)[2] <- "total_number_occ"

    return(list(rast_full, full_table, ID_cell_all_occ))

  }





