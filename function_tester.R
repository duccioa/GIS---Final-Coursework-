library(sp)
####### simple example, from scratch:
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
centroids <- coordinates(SpP)
x <- centroids[,1]
y <- centroids[,2]
z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
plot(SpP, col = 1:3, pbg="white")
spdf <- SpatialPolygonsDataFrame(SpP,
                                 data=data.frame(x=x, y=y, z=z, row.names=row.names(SpP)))


rm(Sr1, Sr2, Sr3, Sr4, Srs1, Srs2, Srs3, SpP, x, y, z, centroids)


####remove hole
####### simple example, from scratch:
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)))

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3), "s3")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
centroids <- coordinates(SpP)
x <- centroids[,1]
y <- centroids[,2]
z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
plot(SpP, col = 1:3, pbg="white")
spdf <- SpatialPolygonsDataFrame(SpP,
                                 data=data.frame(x=x, y=y, z=z, row.names=row.names(SpP)))


rm(Sr1, Sr2, Sr3, Sr4, Srs1, Srs2, Srs3, SpP, x, y, z, centroids)


####moment of inertia Jz


