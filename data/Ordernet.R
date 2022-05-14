order.net=function (mat=A){
	
	
	order.Col=order(apply(mat,2,sum), decreasing=T) # ordering the matrix according to species degree (columns)
mat.ordered=mat[,order.Col]
order.Row=order(apply(mat,1,sum), decreasing=T) # ordering the matrix according to species degree (rows)
mat.ordered=mat.ordered[order.Row,]
return(mat.ordered)
	
	}

