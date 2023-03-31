


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
