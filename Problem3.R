data=read.table("kmeansData.txt")
ans2=kmeans(x=data[,c(2,3)],centers=3, iter.max=1000)
ans1=kmeans(x=data[,c(2,3)],centers=2, iter.max=1000)
print(ans1)
print(ans2)

pointsInEachCluster2=lapply(1:3,function(cluster){
  logicalVector=as.numeric(unlist(ans2[1]))==cluster
  return(data[logicalVector,c(2,3)])
});

print(pointsInEachCluster2)

print("Hello")

distances=lapply(1:3,function(cluster){  
   points=as.matrix((pointsInEachCluster2[cluster])[[1]])
   row=t(as.matrix(ans2$centers[cluster,]))
   distance=apply(points,1,function(point){
       point=t(as.matrix(point))
       return(sqrt(sum((point-row)^2)))
    })
    return(sum(distance)/nrow(points))
  })


print(distances)

d2=lapply(pointsInEachCluster2,function(points){
  return(sum(dist(points)))
})