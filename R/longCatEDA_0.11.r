setClass('longCat',
    representation(		data        = "matrix",
                      data.sorted = "matrix",
                      dim         = "integer",
                      times       = "matrix",
                      times.sorted= "matrix",
                      labels      = "character",
                      factors     = "numeric",
                      IndTime     = "logical",
                      nfactors    = "integer",
                      sorted      = "logical",
                      ascending	  = "logical",			   
                      group       = "matrix",
                      groupLabels = "character" ) 
)
setMethod("summary",
    signature(object = "longCat"),
    definition = function (object, ...) 
    {
      if( object$sorted) temp <- object[c(3,6:13)]
      if(!object$sorted) temp <- object[c(3,6:10 )]
      temp$group <- table(temp$group)
      print(temp)
    }
)


longContPlot <- function(y, times=NULL, ylim=NULL, xlim=NULL, ...)
{
  # check the inputs and set graphing parameters
  if( is.null(ylim) ){ ylim=range(y, na.rm=T) }
  if( is.null(times) )
  { 
      txx <- 1:ncol(y)
      xlim=range(txx) 
      times <- data.frame( matrix(txx, 1, ncol(y)) )
  }
  if( !is.null(times) & is.null(dim(times)) )
  { 
    times <- data.frame( matrix(times, 1, ncol(y)) ) 
  }
  if( all( dim(y)==dim(times) ) & !is.null(times) ){ txx <- times[1,] }
  if( any( dim(y)!=dim(times) ) & !is.null(times) ){ txx <- times[1,] }
  if( !is.null(times) & is.null(xlim) ){ xlim=range(times) }
  if(  is.null(times) & is.null(xlim) ){ xlim=c(1,ncol(y)) }
  
  # initiate a blank plot
  plot( unlist(txx), unlist(y[1,]), col='white', ylim=ylim, xlim=xlim, type='n', ...)
  
  # loop through subjects adding them to the plot
  if( any( dim(y)!=dim(times) ) )
  {
    for(r in 1:nrow(y)){ lines(txx, y[r,])  }
  }
  if( all( dim(y)==dim(times) ) ) 
  {
    for(r in 1:nrow(y))
    { 
      txx <- times[r,]
      lines(txx, y[r,])  
    }
  }
}

lunique <- function(y)
{
  u <- unique(y)
  sum(!is.na(u))
}
levelCheck <- function(y)
{
  lu <- apply(y, 2, lunique)
  maxlu <- max(lu)
  if( maxlu > 9 )
  {
    stop('One or more variables in y has 10 or more categories')
  }
  # determine unique values
  factors <- as.numeric(levels(factor(unlist(y))))
  return( factors )
}
dimCheck <- function(y, times)
{
  dimErr1 <- "The dimension of times does not equal the dimension of y"
  dimErr2 <- "The length of times does not equal ncol(y)"
  ry <- nrow(y)
  cy <- ncol(y)
  rt <- nrow(times)
  ct <- ncol(times)

  if( rt>1 & rt!=ry ) stop(dimErr1) 
  if( cy!=ct ) stop(dimErr2)
  if( ry==rt & cy==ct )
  {
    IndTime <- T
  }
  if( rt==1 & cy==ct )
  {
    IndTime <- F
  }
  return(IndTime)
}

longCat <- function(y, times=NULL, Labels=NULL, tLabels=NULL)
{
  # convert data frame to matrix
  y <- as.matrix(y)
  # rescale inputs to positive sequential integers from 1 to the maximum 
  # number of categories
  u <- unique(c(y))
  u <- u[ !is.na(u) ]
  u <- u[order(u)]
  if( !all(u == 1:length(u)) )
  {
    temp <- y
    for(i in 1:length(u))
    {
      temp[y==u[i]] <- i
    }
    y <- temp; rm(temp)
  }
  rm(u)
  
  # check the levels using levelCheck()
  factors <- levelCheck(y)
  
  # check the times input and force to data.frame
  if( is.null(times)){ times <- data.frame( matrix(1:ncol(y), 1, ncol(y)) ) }
  if(!is.null(times))
  { 
    if( is.null(dim(times))) times <- data.frame( matrix(times, 1, ncol(y)) )
    if(!is.null(dim(times))) times <- data.frame( times )  
  }
  
  # check the dimensions of the data (y) and the times input
  IndTime <- dimCheck(y, times)
  
  # create Labels if not provided
  if(is.null(Labels)){ Labels <- factors }
  
  # count the number of factors returned by leveCheck()
  nfactors <- length(factors)
  
  # check that Labels, nfactors, tLabels, and times conform
  if( length(Labels) != nfactors )
  {
    warning(paste('The number of labels in Labels does not equal the\n',
     'number of unique values in the data set.'))
  }
  if( !is.null(tLabels) & length(unique(times)) != length(tLabels)  )
  {
    warning(paste('The number of labels in tLabels does not equal the\n',
      'number of unique values in times.'))
  }
  
  # if individually varying times of observation, make sure times is a matrix
  if(!is.null(times) & IndTime) times <- as.matrix(times)

  # apply class and return output
  lc =  list(data=y,
        data.sorted=NULL,
        dim=dim(y),
        times=times,
		    times.sorted=NULL,
        labels=Labels,
        tLabels=tLabels,
        factors=factors,
        IndTime=IndTime,
        nfactors=nfactors,
        sorted=FALSE,
        ascending = NULL,					   
        group = NULL,
        groupLabels = NULL)
  class(lc) = 'longCat'
  return(lc) 
}


norpt <- function( alist = c(1,2,2,3,3,3,4,4,4,4,5) )
{
  outlist <- alist[1]
  for(i in 2:length(alist))
  {
    if( !is.na(alist[i-1]) & !is.na(alist[i]) )
    {
      if(alist[i-1] != alist[i]){ outlist <- c(outlist, alist[i]) }
    }
  }
  outlist <- c(outlist, rep(NA, (length(alist)-length(outlist))))
  outlist
}
makePatterns <- function(dat, times, num=TRUE, mindur=NULL, igrpt=FALSE)
{
  # first, reduce the effect of short durations
  if(!is.null(mindur) & length(dim(times))==2)
  {
    times <- times - times[,1]
    mintime <- times <= mindur & !is.na(times)
    dat[mintime] <- NA
  }
  # if desired, (ig)nore (r)e(p)ea(t) observations, 
  #   i.e., c(1, 2, 2, 3) becomes c(1, 2, 3)
  if(igrpt){ dat <- t( apply(dat, 1, norpt) )  }
  # concatenate rows into a string
  out <- apply(dat, 1, paste, collapse="")  
  # if desired, turn to numeric and reduce the scale
  if(num)
  {
    g    <- gsub('NA', '', out)
    nc   <- nchar(g)
    tens <- 10^nc
    out  <- ( as.numeric(g)/tens )*10
  }
  as.matrix(out, nrow(dat), 1)
}
sorter <- function(lc, ascending=TRUE, whichColumns=NULL, num=TRUE, mindur=NULL, 
             igrpt=FALSE, customSort=NULL, initFirst=FALSE, group=NULL, groupLabels=NULL, ggap=10)
{
  # check for missing values on the grouping variable
  if( !is.null(group) )
  {
    if( any(is.na(group)) ) 
    {
      lc$data <- lc$data[!is.na(group),]
      group <- group[!is.na(group)]
      w <- paste('WARNING: rOWS in ', quote(lc),
                 '$data with missing membership on group variable ', quote(group), 
                 ' have been deleted\n', sep='')
      cat(rep('*',40),'\n',w,rep('*',40),'\n')
    }
  }
  
  # check inputs and set additional sorting parameters
  if(is.null(whichColumns)) whichColumns = 1:ncol(lc$data)
  if( lc$IndTime) pats <- makePatterns(lc$data[,whichColumns], lc$times[,whichColumns], num, mindur, igrpt)
  if(!lc$IndTime) pats <- makePatterns(lc$data[,whichColumns], NULL, num, mindur, igrpt)
  if(lc$IndTime) tpat <- do.call(order, data.frame(lc$times[,whichColumns]) )
  if( is.null(group) & !lc$IndTime) o <- order(pats, decreasing = !ascending)
  if(!is.null(group) & !lc$IndTime) o <- order(group, pats, decreasing = !ascending)
  if( is.null(group) &  lc$IndTime) o <- order(pats, tpat, decreasing = !ascending)
  if(!is.null(group) &  lc$IndTime) o <- order(group, pats, tpat, decreasing = !ascending)
  # if there is a custom sorting variable
  if(!is.null(customSort) & !initFirst)
  {
    if( is.null(group) & !lc$IndTime) o <- order(customSort, pats, decreasing = !ascending)
    if(!is.null(group) & !lc$IndTime) o <- order(group, customSort, pats, decreasing = !ascending)
    if( is.null(group) &  lc$IndTime) o <- order(customSort, pats, tpat, decreasing = !ascending)
    if(!is.null(group) &  lc$IndTime) o <- order(group, customSort, pats, tpat, decreasing = !ascending) 
  }
  if(!is.null(customSort) &  initFirst)
  {
    if( is.null(group) & !lc$IndTime) o <- order(lc$data[,1], customSort, pats, decreasing = !ascending)
    if(!is.null(group) & !lc$IndTime) o <- order(lc$data[,1], group, customSort, pats, decreasing = !ascending)
    if( is.null(group) &  lc$IndTime) o <- order(lc$data[,1], customSort, pats, tpat, decreasing = !ascending)
    if(!is.null(group) &  lc$IndTime) o <- order(lc$data[,1], group, customSort, pats, tpat, decreasing = !ascending) 
  }
  data.sorted <- lc$data[o,]
  group <- group[o]
  if(lc$IndTime) times.sorted <- lc$times[o,]

  # check grouping parameters
  if( !is.null(group) )
  {
    if( nrow(as.matrix(group)) != nrow(lc$data) )
    {
      stop('group has a length that does not equal the number of rows in y')
    }
    group <- as.numeric(group)
    u <- unique(group)
    if( is.null(groupLabels) ) groupLabels <- paste('Group',u,sep='')
    if( length( u ) != length(groupLabels) )
    {
      stop(paste('The number of labels in groupLabels does not equal the\n',
        'number of unique values in the variable group.'))
    }
  }
  # if grouping/stratification is present, augment the data with empty rows
  # which will visually dilineate groups
  if( !is.null(group) )
  {
    temp <- vector('list', length(u) )
    gtemp <- vector('list', length(u) )
    ttemp <- vector('list', length(u) )
    blines <- matrix(NA, ggap, ncol(data.sorted))
    glines <- blines[,1]
    for(i in 1:length(u))
    {
      gdat <- data.sorted[group==u[i],]
      pats <- makePatterns(gdat[,whichColumns], NULL, num, mindur)
      o    <- order(pats, decreasing = !ascending)
      if(lc$IndTime)
      {
        tdat <- times.sorted[group==u[i],]
        pats <- makePatterns(gdat[,whichColumns], tdat[,whichColumns], num, mindur)
        tpat <- do.call(order, data.frame(tdat) )
        o    <- order(pats, tpat, decreasing = !ascending)
      }     
      temp[[i]]  <- rbind(blines, gdat[o,]) 
      gtemp[[i]] <- as.matrix(c(glines, group[group==u[i]]))
      if(lc$IndTime) 
      {
        ttemp[[i]] <- rbind(blines, tdat[o,]  )
      }
    }
    data.sorted <- do.call(rbind, temp); rm(temp)
    group <- do.call(rbind, gtemp); rm(gtemp)
    if(lc$IndTime){ times.sorted <- do.call(rbind, ttemp); rm(ttemp) }
  }                                          
  
  # assign NULL if not previously assigned
  if(!lc$IndTime) times.sorted = NULL

  # return modified lc object
  lc = list( data = lc$data,
        data.sorted = data.sorted,
        dim = lc$dim,
        times = lc$times,
		    times.sorted = times.sorted,
        labels = lc$labels,
        tLabels = lc$tLabels,
        factors = lc$factors,
        IndTime = lc$IndTime,
        nfactors = lc$nfactors,
        sorted = TRUE,
        ascending = ascending,			   
        group = group,
        groupLabels = groupLabels )
  class(lc) = 'longCat'
  return(lc)        
}


colChoose <- function(colScheme, nfactors, reverse=FALSE)
{
	if(colScheme==0) cols <- c(1:8, 'darkgreen')
  if(colScheme==1){
	cols <- c("darkred",
			  "darkorange",
			  "darkgoldenrod2",
			  "darkgreen",
			  "lightblue",
			  "darkblue",
			  "blueviolet",
			  "black",
			  "hotpink")}
	if(colScheme==2){
	cols <- c("orange4",
			  "orange",
			  "olivedrab",
			  "olivedrab1",
			  "mediumorchid4",
			  "mediumorchid1",
			  "royalblue4",
			  "royalblue1",
			  "black")}
	if(colScheme==3){
	cols <- c("forestgreen",
			  "green",
			  "deeppink4",
			  "hotpink1",
			  "chocolate4",
			  "darkorange",
			  "red4",
			  "orangered",
			  "black")}
  if(colScheme=='gray'){cols <- gray(seq(0,.9,len=nfactors))}
  if(colScheme=='oldheat'){
  cols <- c("purple4",
        "royalblue",
        "paleturquoise3",
        "palegreen",
        "yellow",
        "orange",
        "orangered",
        "maroon",
        "red4")
  }
  if(colScheme=='coldheat'){
  cols <- c("blue4",
        "dodgerblue2",
        "green4",
        "darkseagreen3",
        "yellow4",
        "tan1",
        "orange",
        "orangered",
        "red2")
  }
  if(colScheme=='rainbow'){cols <- rainbow(9)}
  if(colScheme=='heat'){cols <- heat.colors(9, alpha = 1)[9:1]}
  if(colScheme=='terrain'){cols <- terrain.colors(9, alpha = 1)}
  if(colScheme=='topo'){cols <- topo.colors(9, alpha = 1)}
  if(colScheme=='cm'){cols <- cm.colors(9, alpha = 1)}
             
  # some finessing to make sure contrast is sufficient when nFactors < 9
  if(reverse) cols <- cols[9:1]
  if( colScheme > 0 )
  {
    if(nfactors <= 5 & nfactors > 3) cols <- cols[c(1,3,5,7,9)]
    if(nfactors <= 3) cols <- cols[c(1,5,9)]
  }
	return(cols)
}

longCatPlot <- function(lc, xlab="Day",
                ylab="Each Line Represents a Participant", cols=NULL,
                colScheme='heat', reverse=FALSE, lwd=.5, lcex=1, llwd=1.5, 
                legendBuffer=.1, groupBuffer=.25, groupRotation=90, gcex=1, 
                bg='antiquewhite3', seg.len=1, xlas=0, xcex=1, ...)
{
  if( is(lc) != 'longCat' ){ stop('longCatPlot requires an object of class longCat.')  }
  if(is.null(cols)){ cols <- colChoose(colScheme, lc$nfactors, reverse) }
  if(legendBuffer < 0 | legendBuffer > 1){stop('legendBuffer must be in [0,1]')}

  # on the fly sorting
  if( !lc$sorted ) lc <- sorter(lc)

  # set up plot, pre-allocating a region for labels using ymax
  lo = min(lc$times, na.rm=T)
  up = max(lc$times, na.rm=T)
  xrange = lo:up
  
  # reps is used to automatically scale the x-axis
  reps = nrow(lc$data.sorted)-length(xrange)    
  # fix reps if negative, will occur when the number of cases is fewer than
  # the number of time points. This happens when plotting an individual
  if( reps < 0 ) reps <- 0
  
  # set additional plotting parameters
  xbuffer <- .5*mean(xrange[2:length(xrange)]-xrange[1:(length(xrange)-1)])
  tx <- c(lo-.5, xrange, rep( lo, reps), up+.5 )
  if( !is.null(lc$group) ){ tx[1] <- tx[1] - groupBuffer*xbuffer }
  ymax <- nrow(lc$data.sorted) + ceiling( legendBuffer*nrow(lc$data.sorted) )
  
  # set background color
  par(bg=bg)
  
  # initiate the empty plot
  plot(tx,y=rep(NA,length(tx)),col='white',ylim=c(0,ymax), 
        xlab=xlab,ylab=ylab, axes=FALSE, ...)

  # add axes
  if(!is.null(lc$tLabels)) axis( 1, at = unique(lc$times), 
                                 labels=lc$tLabels, las=xlas, cex.axis=xcex )
  if( is.null(lc$tLabels)) axis( 1, at = NULL )

  # plot loops
  for(r in 1:nrow(lc$data.sorted)) # loop over cases
  {
    # select plotting data for the r^th case
    pdat <- lc$data.sorted[r,]
    if( lc$IndTime & !lc$sorted) txx <- lc$times[r,]
	  if( lc$IndTime &  lc$sorted) txx <- lc$times.sorted[r,]
    if(!lc$IndTime) txx <- lc$times
    tempy <- rep(r,2)
    for(j in 1:length(pdat)) # loop over observations
    {
       if( !is.na( pdat[j] ) ) # if observation is NA, skip
       {
         # define the x-values for any but the last segment
         if( j <length(pdat) )
         { 
           tempx <- c(txx[j]-xbuffer, txx[j+1]-xbuffer)
           # correct for missing endpoint
           if(is.na(txx[j+1])) tempx[2] <- txx[j]+xbuffer 
         }
         # define the x-values for the last segment
         if( j==length(pdat) ){ tempx <- c(txx[j]-xbuffer, txx[j]+xbuffer) }
         # horizontal line plot
         lines(tempx, tempy, lwd=lwd, col=cols[ unlist(pdat[j]) ] )
       }
    }
  }

  # add legend
  legMax <- max(lc$times, na.rm=T)-(max(lc$times, na.rm=T)-min(lc$times, na.rm=T))/lc$nfactors
  legPoints <- seq(from=min(lc$times, na.rm=T), to=legMax, length.out=lc$nfactors)
  for(l in 1:length(legPoints))
  {
    legend(legPoints[l], ymax, legend=lc$labels[l], lty=1,
           cex=lcex, col=cols[l], bty='n', lwd=llwd, seg.len=seg.len)
  }
  
  # if there is grouping add group labels
  if( !is.null(lc$group) )
  {
    g <- cbind(lc$group, 1:length(lc$group))
    gag <- aggregate(g[,2] ~ g[,1], g, mean, na.rm=T)
    u <- unique(lc$group)
    for(i in 1:length(u))
    {
      text(tx[1], gag[i,2], labels = lc$groupLabels[i], srt=groupRotation, cex=gcex)
    }
  }
  # reset par
  par(bg='white')
}

