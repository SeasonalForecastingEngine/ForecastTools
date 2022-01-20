#' Function for upscaling data from a regular lon-lat grid to a coarser lon-lat grid
#'
#' @description
#' For validating predictions against gridded data and comparing different forecast systems, it is often necessary to map data to the same grid (usually the coarsest of the involved grids).
#' Gridded predictions and observations constitute grid point averages, see \href{https://rmets.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/met.78}{this paper}.
#' This means that the upscaled value assigned to a coarse grid cell should be a weighted average of the values of the fine grid cells overlapping the coarse cell, with the weighting accounting for the
#' area of overlap. This function does this for you, as long as both the fine grid and the coarse grid are regular grids in lon/lat (consisting of lon/lat rectangles). It addresses the following challenges:
#' \itemize{
#' \item The fine grid does not need to be nested in the coarse grid, creating different partial overlap scenarios. Therefore the value of each fine grid cell may contribute to multiple (up to four) coarse grid cells.
#' \item Grid cell area depends on latitude, grid cells at the equator are much larger than at the poles. This affects the contribution of each fine grid cell (grid cells closer to the pole contribute less to the coarse grid cell average).
#' \item Naive merging of data tables or distance-based matching of grid cells frequently results in unnecessary large lookup tables that may not fit into memory when both your fine and your coarse grid are high-resolution.
#' \item Frequently, you need to upscale \emph{repeated} data between the same grids, for example when you want to upscale observations for many different dates. In this case, the matching between the grids should only be derived once,
#' and not be repeated every time.
#' }
#' The function will still work when the coarse grid is of the same resolution as the fine grid (when the grids are just spatially shifted), but it won't work when the coarse grid is in fact finer than the fine grid
#' (in this case there might be coarse grid cells that are fully contained in a fine grid, which is not accounted for).
#'
#' In the current implementation, a coarse grid cell gets assigned a value if it overlaps with at least one fine grid cell containing a value. When your large grid spans a wider geographic area,
#' this can mean that the large grid cells at the border of your data get assigned a value even when they only have a fraction of overlap with a small grid cell. This can be problematic, in particular when your coarse grid is much coarser
#' than the fine grid.
#'
#' Even though the grids are assumed to be regular, the function allows for missing data in the form of missing grid points in dt (so you don't have to 'squarify' it, adding NAs before upscaling). In fact, the function is
#' faster when missing-data-grid-points are not contained in dt (since fewer grid points need to be matched).
#'
#'
#'
#' @param dt data table containing the data you want to upscale.
#' @param coarse_grid data table containing lons/lats of the grid you want to upscale to.
#' @param uscols column name(s) of the data you want to upscale (can take multiple columns at once, but assumes that the different columns have missing values at the same position).
#' @param bycols optional column names for grouping if you have repeated data on the same grid, e.g. use bycols = 'date' if your data table contains observations for many dates on the same grid (and the column specifying the date is in fact called 'date').
#' @param tol tolerance parameter used for grid matching, in order to deal with rounding errors present in the coordinates. The gridpoint areas are calculated with this precision, so the output has errors of this order of magnitude.
#'
#' @export

upscale_regular_lon_lat = function(dt,
                                   coarse_grid,
                                   uscols,
                                   bycols = NULL,
                                   tol = 1e-5)
{
  save_key_dt = key(dt)

  setkeyv(dt,c(bycols,'lon','lat'))

  # check whether for each coordinate and each instance of bycols only one value is provided, otherwise the user likely forgot to specify bycols correctly:
  if(is.null (bycols))
  {
    if(dt[,.N] != unique(dt[,.(lon,lat)])[,.N]) stop('The data table contains multiple values per coordinate. Did you forget to specify bycols?')
  } else {
    if(dt[,.N] != unique(dt[,.SD,.SDcols = c('lon','lat',bycols)])[,.N]) stop('The data table contains multiple values per coordinate and level of bycols. Maybe you forgot to include a grouping variables in bycols?')
  }

  # we need to 'squarify' the data table to make sure the grid is regular
  #
  # It makes things way less effective to squarify, that's a lot of data, so let's not do it for now...
  #
  # squarify = function (dt)
  # {
  #   lons = dt[, unique(lon)]
  #   lats = dt[, unique(lat)]
  #   DT_c = data.table(lon = rep(lons, each = length(lats)), lat = rep(lats,times = length(lons)))
  #   dt_squarify = merge(DT_c, dt, by = c("lon", "lat"), all = TRUE)
  #   return(dt_squarify)
  # }

  fine_grid = unique(dt[,.(lon,lat)])
  coarse_grid = unique(coarse_grid[,.(lon,lat)])

  setkey(fine_grid,lon,lat)
  setkey(coarse_grid,lon,lat)

  cg_lons = sort(unique(coarse_grid[,lon]))
  cg_lats = sort(unique(coarse_grid[,lat]))
  fg_lons = sort(unique(fine_grid[,lon]))
  fg_lats = sort(unique(fine_grid[,lat]))

  # here, we assume regular grids!

  # get grid size of fine grid
  d_lons = fg_lons[2:length(fg_lons)] - fg_lons[1:(length(fg_lons)-1)]
  d_lats = fg_lats[2:length(fg_lats)] - fg_lats[1:(length(fg_lats)-1)]

  # # check regularity (doesn't work, because we should not assume that every grid point contains a value. There is the option to 'squarify' by filling in missing values, but this is rather memory-expensive...)
  # if(max(abs(d_lons - d_lons[1])) + max(abs(d_lats - d_lats[1])) > tol)
  # {
  #   stop('your fine grid seems to be irregular, which is not allowed. If you are sure it is regular and this is just rounding errors, set `tol` to something larger.')
  # }

  d_lon = min(d_lons)
  d_lat = min(d_lats)

  # corner point coordinates for the fine grid:
  fg_lon_mins = fg_lons - d_lon/2
  fg_lon_maxs = fg_lons + d_lon/2
  fg_lat_mins = fg_lats - d_lat/2
  fg_lat_maxs = fg_lats + d_lat/2

  # same for coarse grid:

  D_lon = min(cg_lons[2:length(cg_lons)] - cg_lons[1:(length(cg_lons) - 1)])
  D_lat = min(cg_lats[2:length(cg_lats)] - cg_lats[1:(length(cg_lats) - 1)])

#   if(max(abs(D_lons - D_lons[1])) + max(abs(D_lats - D_lats[1])) > tol)
#   {
#     stop('your coarse grid seems to be irregular, which is not allowed. If you are sure it is in fact regular, this might be caused by rounding errors. In this case increase `tol` to something bigger.')
#   }



  # For the corner points of the coarse grids we expand each grid cell by `tol`.
  # This ensures that the area covered by the coarse grid does not get any 'holes' because of rounding errors.
  # Note however, that the area covered by the coarse grid might be smaller than the area covered by the fine grid,
  # so it is not required for each fine-grid-point to be contained in a coarse grid point...

  # upon rereading this code I'm wondering whether we actually need to do this?

  cg_lon_mins = cg_lons - D_lon/2
  cg_lon_maxs = cg_lons + D_lon/2
  cg_lat_mins = cg_lats - D_lat/2
  cg_lat_maxs = cg_lats + D_lat/2


  #### For each corner-point of the fine grid, find in which grid-cell of the coarse grid it is contained (if any). ###

  find_index_lon = function(fine_lons)
  {
    ret_list = lapply(fine_lons,FUN = function(x) which(x >= cg_lon_mins & x <= cg_lon_maxs))

    # if you just unlist this, the coordinates that don't match any coarse grid cell are getting lost, so we need to do the following:
    is_null = unlist(lapply(ret_list,length)) == 0
    ret_list[is_null] = 0
    ret_list= lapply(ret_list, `[[`, 1) # some fine-grid-corner-coordinates may be assigned to multiple coarse grid coordinates due to rounding errors, and the tol-expansion.
                                        # In this case it does not matter which of the grid cell we assign it to, this hack assigns it to the first one.

    return(unlist(ret_list))
  }

  find_index_lat = function(fine_lats)
  {
    ret_list = lapply(fine_lats,FUN = function(x) which(x >= cg_lat_mins & x <= cg_lat_maxs))

    # if you just unlist this, the coordinates that don't match any coarse grid cell are getting lost, so we need to do the following:
    is_null = unlist(lapply(ret_list,length)) == 0
    ret_list[is_null] = 0
    ret_list  =lapply(ret_list, `[[`, 1) # see above

    return(unlist(ret_list))
  }



  lon_min_index = find_index_lon(fg_lon_mins)
  lon_max_index = find_index_lon(fg_lon_maxs)

  lat_min_index = find_index_lat(fg_lat_mins)
  lat_max_index = find_index_lat(fg_lat_maxs)

  # Now, a small grid cell overlaps with the i-th large grid cell, if (and only if) at least one of its corner points lies in that grid cell.

  fg_full = as.data.table(expand.grid(fg_lon = fg_lons,fg_lat = fg_lats))
  setkey(fg_full,fg_lon,fg_lat)

  # add cornerpoints:
  fg_full[, fg_lon_min := rep(fg_lon_mins,each = length(fg_lats))]
  fg_full[, fg_lon_max := rep(fg_lon_maxs,each = length(fg_lats))]
  fg_full[, fg_lat_min := rep(fg_lat_mins,length(fg_lons))]
  fg_full[, fg_lat_max := rep(fg_lat_maxs,length(fg_lons))]


  fg_full[, lon_min_index := rep(lon_min_index,each = length(fg_lats))]
  fg_full[, lon_max_index := rep(lon_max_index,each = length(fg_lats))]
  fg_full[, lat_min_index := rep(lat_min_index,length(fg_lons))]
  fg_full[, lat_max_index := rep(lat_max_index,length(fg_lons))]

  fg_full[,fg_index := 1:.N]

  # now, we may only keep working the fine grid points for which we actually have data:
  setnames(fine_grid,c('lon','lat'),c('fg_lon','fg_lat'))
  fg_full = merge(fg_full,fine_grid,by = c('fg_lon','fg_lat'))


  # get the row-index of the cg_full (see below) associated with the coarse grid point in which the lower left corner point lies:
  fg_full_lower_left = copy(fg_full)[,index := (lon_min_index != 0 & lat_min_index != 0 )*(lat_min_index + (lon_min_index-1)*(length(cg_lats)))]
  fg_full_lower_left[,overlap_type := 1]
  # the overlap_type helps us keeping track of 'duplicates', i.e. fine grid cells for which multiple corner points lie in the same coarse grid cell
  # (but they still should only contribute once...).

  # lower right corner point:
  fg_full_lower_right = copy(fg_full)[,index := (lon_max_index != 0 & lat_min_index != 0 )*(lat_min_index + (lon_max_index-1)*(length(cg_lats)))]
  fg_full_lower_right[,overlap_type := 2]
  # upper left:
  fg_full_upper_left = copy(fg_full)[,index := (lon_min_index != 0 & lat_max_index != 0 )*(lat_max_index + (lon_min_index-1)*(length(cg_lats)))]
  fg_full_upper_left[,overlap_type := 3]
  # upper right:
  fg_full_upper_right = copy(fg_full)[,index := (lon_max_index != 0 & lat_max_index != 0 )*(lat_max_index + (lon_max_index-1)*(length(cg_lats)))]
  fg_full_upper_right[,overlap_type := 4]

  matched_grids = rbindlist(list(fg_full_lower_left,
                                 fg_full_lower_right,
                                 fg_full_upper_left,
                                 fg_full_upper_right))

  # get full coarse grid:
  cg_full = as.data.table(expand.grid(lon = cg_lons,lat = cg_lats))
  setkey(cg_full,lon,lat)
  # add cornerpoints to calculate area of overlap
  cg_full[, cg_lon_min := rep(cg_lon_mins,each = length(cg_lats))]
  cg_full[, cg_lon_max := rep(cg_lon_maxs,each = length(cg_lats))]
  cg_full[, cg_lat_min := rep(cg_lat_mins,length(cg_lons))]
  cg_full[, cg_lat_max := rep(cg_lat_maxs,length(cg_lons))]

  cg_full[,index := 1:.N] # this index is what matches with the index in matched_grids:
  matched_grids = merge(matched_grids,cg_full,by = 'index')

  # Each row in matched_grids resembles a corner point of the fine grid and matches it to the grid cell in coarse grid in which it is contained.
  # We want a data table that show us which fine cells overlap with which coarse cells. Overlapping means that at least one fine-grid-corner-point
  # is contained in the coarse grid cell, but there are coarse grid cells containing multiple corners of the same fine grid cell, so we need to do the following:

  matched_grids = matched_grids[,mt := min(overlap_type), by = .(index,fg_index)][overlap_type == mt][,mt := NULL]

  #### calculate area overlaps: ####

  # for a rectangle with corner coordinates lon_min, lon_max,lat_min,lat_max the area is (lon_max - lon_min)*(sin(lat_max) - sin(lat_min)), when lat is given in radians.
  # We thus just may convert lats into sin(lats), and then calculate areas of rectangles. (This is a one-to-one mapping, since the angle is between -90 and +90 degree, where sin is increasing).
  # Also note that we do not need dx and dy to be compatible (which would require to convert dx into proper angle distances...),
  # because we ultimately do not care about the AREA of overlap, but only about the FRACTION of overlap, like, (area of overlap)/(unit area), see below.

  # convert lats to sin(lats):
  matched_grids[,c('fg_lat_min','fg_lat_max','cg_lat_min','cg_lat_max') := lapply(.SD,function(x) sin(pi*x/180)),.SDcols = c('fg_lat_min','fg_lat_max','cg_lat_min','cg_lat_max')]
  # calculate fraction of overlap:
  matched_grids[,area := (pmin(cg_lon_max,fg_lon_max) - pmax(cg_lon_min,fg_lon_min)) * (pmin(cg_lat_max,fg_lat_max) - pmax(cg_lat_min,fg_lat_min))]
  # this should be non-negative, but just for robustness:
  matched_grids = matched_grids[area > 0]

  # area contribution per coarse grid cell:
  matched_grids[,area_contr:= area/sum(area),by = index] # this is why we don't need to care about the absolute unit of area, we only need it for our weights for averaging.

  matched_grids = matched_grids[,.(index,lon,lat,fg_index,fg_lon,fg_lat,area_contr)]

  # now, matched grids contains the weights for the weighted averages, and we can merge with the original data table:
  setnames(matched_grids,c('cg_index','cg_lon','cg_lat','fg_index','lon','lat','area_contr'))
  ret_dt = merge(dt,matched_grids,c('lon','lat'),allow.cartesian = TRUE)

  # return dt to its original key
  setkeyv(dt,save_key_dt)

  # take the weighted average for upscaling:
  ret_dt= ret_dt[!is.na(get(uscols[1]))] # remove missing values before taking weighted means: The function stats::weighted.means has three if-queries, making it probably pretty slow. Using sum with na.rm=TRUE is the fast alternative,
                                         # but if you do that without removing the missing values first, coarse grid cells that do not overlap with a single fine grid cells get the value 0 rather than NA.

  ret_dt = ret_dt[,lapply(.SD,FUN = function(x) sum(area_contr*x,na.rm = T)),.SDcols = uscols,by = c(bycols,'cg_lon','cg_lat')]


  setnames(ret_dt,c('cg_lon','cg_lat'),c('lon','lat'))
  return(ret_dt)
}

