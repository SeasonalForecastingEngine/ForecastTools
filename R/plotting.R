

#' plotting function for spatial data
#'
#' @description Plots spatial data from a data.table. The data table needs to contain columns named 'lon' and 'lat'. The grid needs to be regular.
#' The function runs a crude check whether the data is tempo-spatial. If it is, the time-slice of the first row is used.
#'
#' @param dt Data table containing the data for plotting.
#' @param data_col The name of the column in dt containing the data for plotting.
#' @param mn optional plot title
#' @param discrete_cs Logical. Should the color scale be discretized?
#' @param rr,low,mid,high,name,midpoint,na.value,oob,guide,... Arguments for the color scale, passed to scale_fill_gradient2/scale_fill_steps2 (depending on whether discrete_cs == TRUE).
#' rr replaces limits (specifying the range of the color scale) for consistency with the older plotting functions from the PostProcessing package.
#' Unlike for scale_fill_gradient2, the default midpoint is the center of the data range (or the center of rr, if provided), not 0.
#' Note that specifying the midpoint can often be a convenient way to force a color scale with only two colors (for example, by setting it
#' to the minimum or maximum of your data). \code{na.value} specifies the color of missing values. \code{oob} specifies the treatment of out-of-boundary values, i.e. values beyond the
#' limits \code{rr}.
#' The ... argument can in particular be used to customize the legend/colorbar using the function \code{guide_colorbar},
#' see https://ggplot2.tidyverse.org/reference/guide_colourbar.html. Moreover, when \code{discrete_cs == TRUE} you can pass the arguments \code{n.breaks,breaks} to customize the scale.
#' If you use n.breaks you might also want to set nice.breaks = FALSE, see ?scale_fill_steps2.
#' @param binwidth,bin_midpoint only used when \code{discrete_cs == TRUE}. Normally, the breaks for the discrete colorscale are specified by n.breaks (which is not reliable,
#' since they're adjusted to be 'nice'), or by specifying the breaks explicitly (which is often tedious). This gives you a third option, namely specifying how far the breaks
#' should be apart, and specifying the centerpoint for one of the bins (default is midpoint, or the center of rr if midpoint is not provided). For example, if your color scale shows
#' percentages and you'd like 4 categories, ranging from white to red, this is easiest achieved by \code{binwidth = 25, midpoint = 12.5}.
#'
#' @return a ggplot object.
#'
#' @import data.table
#' @import ggplot2
#'
#' @export
#'
#' @author Claudio Heinrich


ggplot_dt = function(dt,
                     data_col = colnames(dt)[3],
                     mn = NULL,
                     discrete_cs = FALSE,
                     rr = NULL,
                     low = "blue",
                     mid = "white",
                     high = "red",
                     name = data_col,
                     midpoint = NULL,
                     na.value = 'gray50',
                     oob = scales::squish,
                     guide = guide_colorbar(barwidth = 0.5, barheight = 10),
                     ...,
                     binwidth = NULL,bin_midpoint = midpoint)
{
  ####### transform data #######

  # if you have spatio-temporal data, plot only only the first time-slice of it (convenient for diagnostics).

  time_cols = intersect(names(dt),c('month','year','day','date','season'))

  dt_sm = dt[,.SD,.SDcols = c('lon','lat',data_col,time_cols)]

  if(length(time_cols)>0)
  {
    tc1 = dt_sm[1,.SD,.SDcols = time_cols]
    dt_sm = merge(dt_sm,tc1,by = time_cols)
  }

  if(dt_sm[,.N] > unique(dt_sm[,.(lon,lat)])[,.N])
  {
    warning('Your data has multiple entries per lon/lat coordinate. By default, the last value for each coordinate is plotted.' )
  }

  #### get map: ####

  world_map <- ggplot2::map_data(map = 'world',resolution = 0)
  # better maps are available with the rnaturalearth package and can be plotted using geom_sf.
  # However, this approach requires gdal, so it's not exactly easily accessible.

  #### fix range and set values outside of range to the range border ####

  if(is.null(rr))
  {
    rr = dt_sm[,range(get(data_col),na.rm = T)]
  }

  # set midpoint:
  if(is.null(midpoint))
  {
    midpoint = rr[1] + (rr[2]-rr[1])/2
    if(is.null(bin_midpoint))
    {
      bin_midpoint = midpoint
    }

  }

  # set colorscale:
  if(!discrete_cs)
  {
    colorscale = scale_fill_gradient2(low = low, mid = mid, high = high,
                                      name = name, limits = rr, midpoint = midpoint,
                                      na.value = na.value,oob = oob,
                                      guide = guide,...)
  }
  if(discrete_cs)
  {
    if(!is.null(binwidth))
    {
      nbinapprox = floor((rr[2] - rr[1])/binwidth)
      bins1 = binwidth*(1/2 + (0:nbinapprox)) + bin_midpoint
      bins2 = -binwidth*(1/2 + (0:nbinapprox)) + bin_midpoint
      bins=  sort(unique(c(bins2,bins1)))
      bins = round(bins[bins %between% rr],2)

      # for discrete scales there used to be an issue where the boundary bins are shown wider in the legend,
      # see https://github.com/tidyverse/ggplot2/issues/4019. This was resolved in ggplot2 version 2.3.4.

      colorscale = scale_fill_steps2(low = low, mid = mid, high = high,
                                     name = name, limits = rr, midpoint = midpoint,
                                     breaks = bins,
                                     na.value = na.value, oob = oob, guide = guide,...)
    }
    if(is.null(binwidth))
    {
      colorscale = scale_fill_steps2(low = low, mid = mid, high = high,
                                     name = name, limits = rr, midpoint = midpoint,
                                     na.value = na.value, oob = oob,
                                     guide = guide, ...)
    }
  }


  ### plotting ###
  pp = ggplot(data = dt_sm) +
    geom_raster(aes(x = lon,y = lat, fill = get(data_col))) +            # add data plot
    geom_polygon(data = world_map,
                 mapping = aes(x = long,y = lat,group = group),
                 color = 'black',fill = NA,size=0.25)  +               # add map
    colorscale +  # colorscale is specified above
    coord_cartesian(xlim = range(dt_sm[,lon],na.rm = T),
                    ylim = range(dt_sm[,lat],na.rm = T),
                    expand = FALSE) + # restricts the plot to exactly the considered area to avoid weird borders
    #coord_sf(xlim = lon_range,ylim = lat_range,expand = FALSE) +       # restricts the plot to exactly the considered area to avoid weird borders
    xlab('') + ylab('') +                                              # remove default labels and background grid...
    theme(panel.background = element_rect(fill =na.value), # this is required in case a data table is passed that has 'truely' missing locations, i.e. that is not rectangular
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  if(!is.null(mn)) pp = pp + ggtitle(mn)                               # add title, if given

  return(pp)
}



#' Turn a matrix with lat/lon idices into a data.table
#' @export
mat_to_dt = function(X, lons, lats, var_name = "X"){
  n_lat = length(lats)
  n_lon = length(lons)
  dt = data.table(lon = rep(lons, times = length(lats)),
                  lat = rep(lats, each = length(lons)),
                  X = as.vector(X))
  names(dt)[3] = var_name
  return(dt)
}



#' Quick diagnostic plotting function
#'
#' @description Takes a data table that contains cols Lon, Lat (or lon and lat) and a variable to plot.
#'
#' @param dt The data table.
#' @param var Character string. The name of the column containing the values for the plot. Default is third column, for subset data tables of the form .(Lon,Lat,var).
#' @param mn Title of the plot.
#' @param col_scheme Either "bwr" for blue - white - red, "wr" for white - red, or "wb" for white - blue. Specifies the color scheme of the plot.
#' @param set_white Forces the blue-white-red color scheme to center white at the set value if specified.
#' @param xlab,ylab Labeling.
#' @param save_pdf,save_dir,file_name Whether, where and under which name the plot should be saved.
#' @param stretch_par Numeric. Only used when save_pdf == TRUE. Stretches the pdf output. Default is NULL, where it is stretched to #Lons/#Lats.
#' @param legend Boolean. if FALSE, no legend is provided
#'
#' @return none
#'
#' @export
#'
#' @author Claudio Heinrich
#' @examples \dontrun{
#' dt = load_combined_wide()
#' plot_diagnostic(dt)
#' }
#'
#' @importFrom fields designer.colors image.plot
#' @importFrom maps map
plot_diagnostic = function( dt, var = colnames(dt)[3], mn = var,
                            rr = NULL,
                            col_scheme = "bwr", set_white = NULL,
                            xlab = "", ylab = "",
                            save_pdf = FALSE, save_dir = "./figures/", file_name = "diag_plot",stretch_par = NULL,
                            legend = TRUE)
{
  # sometimes lat and lon are given with lowercase letters:

  if('lat' %in% colnames(dt))
  {
    dt[,Lat:=lat][,Lon:=lon]
  }

  # prepare data table

  if("year" %in% colnames(dt))
  {
    if("month" %in% colnames(dt))
    {
      dt = dt[year == min(year)][month == min(month),.SD,.SDcols = c('Lon','Lat',var)][order(Lat,Lon)]
    } else {
      dt = dt[year == min(year),.SD,.SDcols = c('Lon','Lat',var)][order(Lat,Lon)]
    }
  } else {
    dt = dt[,.SD,.SDcols = c('Lon','Lat',var)][order(Lat,Lon)]
  }


  dt[Lon > 180, Lon := Lon-360]


  #--- get longitudes and latitudes

  Lons = sort(unique(dt[,Lon]))
  Lats = sort(unique(dt[,Lat]))

  n_lon = length(Lons)
  n_lat = length(Lats)

  # in case the grid is incomplete:

  if(n_lon * n_lat > dt[,.N])
  {
    setkey(dt,Lat,Lon) # doesn't change ordering

    full_coords = data.table(Lat = rep(Lats,each = n_lon) , Lon = rep(Lons,n_lat))
    setkey(full_coords,Lat,Lon)

    dt = merge(dt,full_coords,all.y = TRUE)
  }

  if(is.null(rr))  rr = range(dt[,3],na.rm=TRUE)
  if(!is.null(rr)){
    dt[dt[[3]]<min(rr),3] = min(rr)
    dt[dt[[3]]> max(rr),3] = max(rr)
  }

  A = matrix(dt[[3]],  n_lon, n_lat)

  # --- scaling and colors ---

  brk = seq(rr[1],rr[2],length = 500)
  brk.ind = round(seq(1,length(brk),length = 10))
  brk.lab = round(brk[brk.ind],2)
  brk.at = brk[brk.ind]

  if(col_scheme == "bwr"){
    if(is.null(set_white)){
      color <- fields::designer.colors(n=length(brk)-1, col = c("darkblue","white","darkred"))
    }else{
      zero.ind = min(which(brk > set_white))/length(brk)
      color <- fields::designer.colors(n=length(brk)-1, col = c("darkblue","white","darkred"), x = c(0,zero.ind,1))
    }
  }
  if(col_scheme == "wr"){
    color <- fields::designer.colors(n=length(brk)-1, col = c("white","darkred"))
  }
  if(col_scheme == "wb"){
    color <- fields::designer.colors(n=length(brk)-1, col = c("white","blue"))
  }

  # set up for coloring NAs gray

  newz.na <- rr[2]+(rr[2]-rr[1])/length(color) # new z for NA
  A[which(is.na(A))] <- newz.na
  rr[2] <- newz.na      #extend the range to include the new value
  color <- c(color, 'gray') # extend the color palette by 'gray'
  brk = c(brk,rr[2]) # extend the vector of breaks

  #--- plotting ---

  if(save_pdf)
  {
    if (is.null(stretch_par)) stretch_par = n_lat/n_lon

    par_0 = par() # allow to set par manually before calling the function

    pdf(paste0(save_dir,file_name,".pdf"),width = 7,height = stretch_par * 7)

    suppressWarnings(par(par_0))
  }

  if(legend)
  {

    fields::image.plot(Lons,Lats,A,
                       zlim=rr, main = mn,
                       xlim = range(Lons), xlab=xlab,
                       ylim = range(Lats), ylab=ylab,
                       breaks=brk,
                       col=color,
                       axis.args=list(at = brk.at,
                                      label = brk.lab))
  } else {
    image(Lons,Lats,A,
          zlim=rr, main = mn,
          xlim = range(Lons), xlab=xlab,
          ylim = range(Lats), ylab=ylab,
          breaks=brk,
          col=color)

  }


  # add world map
  maps::map("world", add = TRUE)

  if('lon' %in% names(dt))
  {
    dt[,c('Lon','Lat') := NULL]
  }

  if(save_pdf) dev.off()

}

#' Take a data.table on an incomplete grid and "squarify" so that the image function can work on it
#' @param dt A data.table with Lon and Lat plus another variable
#' @param var The name of the variable to be squarified
#' @export
squarify = function(dt, var = names(dt)[3])
{
  lons = dt[,unique(Lon)]
  lats = dt[,unique(Lat)]

  DT_c = data.table(Lon = rep(lons,each = length(lats)), Lat  = rep(lats, times = length(lons)))
  DT_plot = merge(DT_c,
                  dt[,.SD,.SDcols = c("Lon","Lat",var)],
                  by = c("Lon","Lat"),
                  all.x = TRUE,
                  all.y = FALSE)

  return(DT_plot)


}




#' smooth plotting function
#'
#' @description Takes a data table of the form .(Lon,Lat,value) or .(Lat,Lon,value) and plots values on the globe after applying a kernel smoothing
#'
#' @param dt The data table.
#' @param var Character string. The name of the column containing the values for the plot. Default is the name of the third column of dt, to be used for subset data tables of the form .(Lon,Lat,var).
#' @param mn,rr Title and range of the plot.
#' @param theta parameter for the Gaussian smoothing kernel
#' @param pixels Resolution of the plot
#' @param col_scheme Either "bwr" for blue - white - red, "wr" for white - red, or "wb" for white - blue. Specifies the color scheme of the plot.
#' @param set_white Forces the blue-white-red color scheme to center white at the set value if specified.
#' @param xlab,ylab Labeling.
#' @param save_pdf,save_dir,file_name Whether, where and under which name the plot should be saved.
#' @param stretch_par Numeric. Only used when save_pdf == TRUE. Stretches the pdf output. Default is NULL, where it is stretched to #Lons/#Lats.
#'@param exclude_land Should pixels on land be grayed out?
#' @param exclude_ocean Should pixels on ocean be grayed out?
#' @param legend Should the plot have a legend?
#' @return none
#'
#' @export
#'
#' @author Claudio Heinrich
#' @examples \dontrun{
#' dt = load_combined_wide()
#' plot_smooth(dt)
#' }
#'
#' @importFrom fields as.image designer.colors image.plot image.smooth
#' @importFrom maps map
#' @import maptools
#' @importFrom sp CRS over proj4string SpatialPoints


plot_smooth = function(dt_plot,
                       var = colnames(dt)[3],
                       mn = var,
                       rr = NULL,
                       theta = 0.5,
                       pixels = 256,
                       col_scheme = "bwr",
                       set_white = NULL,
                       xlab = "Lon",
                       ylab = "Lat",
                       save_pdf = FALSE,
                       save_dir = "./figures/",
                       file_name = "diag_plot",
                       stretch_par = NULL,
                       exclude_land = FALSE,
                       exclude_ocean = FALSE,
                       legend = TRUE)
{ # sometimes lat and lon are given with lowercase letters:

  dt = copy(dt_plot)

  if('lat' %in% colnames(dt)){
    dt[,Lat:=lat][,Lon:=lon]
  }
  if(any(dt[, Lon] > 180)){
    dt[Lon > 180, Lon := Lon - 360]
  }


  dt[Lon > 180, Lon := Lon-360]

  ## prepare data table
  if("year" %in% colnames(dt)){
    if("month" %in% colnames(dt)){
      dt = dt[year == min(year)][month == min(month),.SD,.SDcols = c('Lon','Lat',var)][order(Lat,Lon)]
    }else{
      dt = dt[year == min(year),.SD,.SDcols = c('Lon','Lat',var)][order(Lat,Lon)]
    }
  }else{
    dt = dt[,.SD,.SDcols = c('Lon','Lat',var)][order(Lat,Lon)]
  }

  dt = squarify(dt)
  ##--- create image ---


  x = dt[,.(Lon,Lat)]
  setnames(x,c("Lon","Lat"), c("lon","lat"))

  Lons = unique(dt[,Lon])
  Lats = unique(dt[,Lat])

  n_lon = length(Lons)
  n_lat = length(Lats)

  A = matrix(dt[[3]],  n_lon, n_lat)

  im_0 = fields::image.smooth(fields::as.image(A,x = x,nx = pixels,ny = pixels),theta = theta)

  ## Find the points that fall over land

  if(!exists("wrld_simpl")) data(wrld_simpl, package = 'maptools')

  all_loc = expand.grid(lat = im_0$x,lon = im_0$y)
  pts <- sp::SpatialPoints(all_loc, proj4string=sp::CRS(sp::proj4string(wrld_simpl)))
  ii <- !is.na(sp::over(pts, wrld_simpl)$FIPS)
  if(exclude_land){
    im_0$z[ii] = NA
  }
  if(exclude_ocean){
    im_0$z[-which(ii)] = NA
  }

  ## --- fix range of plot and fill in values for points out of range ---
  if(is.null(rr)){
    rr = range(im_0$z,na.rm=TRUE)
  }else{
    im_0$z[im_0$z< min(rr)] = min(rr)
    im_0$z[im_0$z> max(rr)] = max(rr)
  }

  ## --- Get the symmetric color scaling ---
  brk = seq(rr[1],rr[2],length = 500)
  if(all(brk < 0)){
    ## If everthing is negative
    cr = colorRamp(c("white", "darkblue"))
    bmin = min(brk) ## Find the coldest
    brk_fraction = brk / bmin
    brk_col = rgb(cr(brk_fraction), max = 255)
  }
  if(all(brk > 0)){
    ## If everthing is postive
    cr = colorRamp(c("white", "darkred"))
    bmax = max(brk) ## Find the warmest
    brk_fraction = brk / bmax
    brk_col = rgb(cr(brk_fraction), max = 255)
  }
  if(any(brk < 0) & any(brk > 0)){
    ## If we span 0, then make sure the two ends are equally shaded in magnitude
    r_max = max(abs(rr))
    cr_neg = colorRamp(c("white", "darkblue"))
    cr_pos = colorRamp(c("white", "darkred"))
    brk_col = rep(NA, length(brk))
    w_neg = which(brk < 0)
    brk_col[w_neg] = rgb(cr_neg(brk[w_neg] / -r_max), max = 255)
    brk_col[-w_neg] = rgb(cr_pos(brk[-w_neg] / r_max), max = 255)
  }
  brk_ind = round(seq(1,length(brk),length = 10))
  brk_lab = round(brk[brk_ind],2)
  brk_at = brk[brk_ind]
  ##--------------------------------------------------------------------------

  ## color NAs grey
  newz.na <- rr[2]+(rr[2]-rr[1])/length(brk_col) # new z for NA

  im_0$z[which(is.na(im_0$z))] <- newz.na
  rr[2] <- newz.na # extend the range to include the new value
  brk_col <- c(brk_col, grey(.7))
  brk = c(brk,rr[2]) # extend the vector of breaks

  ##--- plotting ---
  if(save_pdf){
    if (is.null(stretch_par)){
      stretch_par = n_lat/n_lon
    }
    pdf(paste0(save_dir,file_name,".pdf"),width = 7,height = stretch_par * 7)
  }

  if(legend){
    if(!exists('axis.args')){
      axis.args=list(at = brk_at,
                     label = brk_lab)
    }
    fields::image.plot(im_0,
                       zlim=rr, main = mn,
                       xlim = range(Lons),
                       xlab = xlab,
                       ylim = range(Lats),
                       ylab = ylab,
                       breaks=brk,
                       col=brk_col[-1])
  }else{
    image(im_0,
          zlim=rr, main = mn,
          xlim = range(Lons), xlab=xlab,
          ylim = range(Lats), ylab=ylab,
          breaks=brk,
          col=brk_col[-1])
  }
  ## add world map
  maps::map("world", add = TRUE)
  if(save_pdf) dev.off()

}
