
#' function for converting netcdfs to long data tables.
#' @param nc either character string with the name of the .nc file (including path), or an object of type ncdf4
#' @param subset_list named list for subsetting. The names must match dimension names of the nc file. The range of the vector contained on each page defines which subset of the data is read out.
#' @return long data table.
#'
#' @import ncdf4
#'
#' @examples {
#' fn = '/nr/user/claudio/bigdisk/SFE/ERA_monthly_nc/total_precipitation_era_1979_1.nc'
#' ncdf_to_dt(fn, subset_list = list('latitude' = -50:50)) # will read out everything between lat -50 and lat 50 (not only the latitudes matching -50:50)
#' }
#'
#' @author Claudio
#'
#' @export

ncdf_to_dt = function(nc,subset_list = NULL,printunits = TRUE)
{

  if(is.character(nc)) nc = nc_open(nc)

  dim_list = c()
  dim_ids = c()
  units = c()
  dim_starts = c()
  dim_counts = c()

  #read dimension variables:
  for(dim_ind in 1:nc$ndim)
  {
    nn = nc$dim[[dim_ind]]$name[1]

    dd = ncvar_get(nc,nn)

    if(nn %in% names(subset_list))
    {
      rel_indices = which(dd %between% range(subset_list[[which(names(subset_list) == nn)]]))

    } else {
      rel_indices = 1:length(dd)
    }

    dim_start = min(rel_indices)
    dim_starts = c(dim_starts,dim_start)
    names(dim_starts)[dim_ind] = nn

    dim_count = max(rel_indices) - min(rel_indices) + 1
    dim_counts = c(dim_counts,dim_count)

    dd = dd[dim_start:(dim_start + dim_count - 1)]     # NOT dd[rel_indices], since dd is not neccessarily sorted?


    dim_list = c(dim_list,list(dd))
    names(dim_list)[dim_ind] = nn
    dim_ids = c(dim_ids,nc$dim[[dim_ind]]$id)

    units = c(units,paste0(nc$dim[[dim_ind]]$name[1],':   ',nc$dim[[dim_ind]]$units))
  }

  ### sometimes the dimensions follow a different order in the variables as in nc$dim??? ###

  dim_order = nc$var[[1]]$dimids

  # check whether all variables have the same dimesnion ordering:
  if(nc$nvars > 1)
  {
    for(var_id in 2:nc$nvars)
    {
      if(!identical(dim_order,nc$var[[var_id]]$dimids))
      {
        stop('different variables have different dimension ordering, currently not supported.')
      }
    }
  }

  dim_list = dim_list[match(dim_ids,dim_order)]
  dim_starts = dim_starts[match(dim_ids,dim_order)]
  dim_counts = dim_counts[match(dim_ids,dim_order)]

  dim_dt = as.data.table(expand.grid(dim_list))



  # append variable values to dim_dt:

  return_dt = dim_dt


  for(var_ind in 1:nc$nvars)
  {

    temp = ncvar_get(nc, varid = nc$var[[var_ind]]$name,start = unname(dim_starts),count = unname(dim_counts))

    var_dt = data.table( as.vector(temp))
    setnames(var_dt,nc$var[[var_ind]]$name)

    return_dt = data.table(return_dt,var_dt)
    units = c(units,paste0(nc$var[[var_ind]]$name,':   ',nc$var[[var_ind]]$units))
  }

  # printout units:
  if(printunits)
  {
    catout = paste0(c('Units:',units),sep = '',collapse = "\n")
    cat(catout)
  }
  return(return_dt)
}
