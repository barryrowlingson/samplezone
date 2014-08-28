##' jitter points but keep within regions
##'
##' jitter points in a uniform disk but keep them within a region
##' @title restricted jitter
##' @param pts a SpatialPointsDataFrame of locations 
##' @param polys a SpatialPolygonsDataFrame of regions
##' @param radius the radius of the disk
##' @param maxiterations limit of iterations
##' @return A SpatialPolygonsDataFrame like pts but with new coordinates
##' @author Barry Rowlingson
##' @export
rjitter <- function(pts, polys, radius, maxiterations = 100){
    original = sp::over(pts, as(polys, "SpatialPolygons"))
    if(any(is.na(original))){
        missed = which(is.na(original))
        stop(length(missed)," points not over any regions to start with")
    }

    xyOriginal = sp::coordinates(pts)
    xyNew = xyOriginal
    newRegion = rep(-1, length(original))

    ## keep going until all done
    ## or iterations exceeded
    iteration = 1
    while(any(newRegion!=original)){
        iteration = iteration + 1
        if(iteration > maxiterations){
            cat("Failed on ",which(newRegion!=original),"\n")
            stop("Couldn't jitter all after ",iteration-1, " steps")
        }
        todo = which(newRegion != original)
        xyNew[todo,] = diskjitter(xyOriginal[todo,,drop=FALSE], radius)
        newpts = sp::SpatialPoints(xyNew[todo,,drop=FALSE], sp::CRS(sp::proj4string(pts)))
        newRegion[todo] = sp::over(newpts, as(polys,"SpatialPolygons"))
        newRegion[is.na(newRegion)]=-1
    }

    sp::SpatialPointsDataFrame(coords=xyNew, data=pts@data, proj4string=sp::CRS(sp::proj4string(pts)))
    
}

diskjitter <- function(xy, r){
    n = nrow(xy)
    theta = runif(n, 0, 2*pi)
    ct = cos(theta)
    st = sin(theta)
    ## square-root needed here 
    r = r * sqrt(runif(n, 0, 1))
    cbind(xy[,1]+r*st, xy[,2]+r*ct)
}
