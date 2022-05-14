#Entropia do grau de redes bipartidas 
#Script de Flávi M. D. Marquitti

entropy= function(mat=A){
	r=dim(mat)[1]
 	c=dim(mat)[2]
 	
 	rmarg=apply(mat,1,sum)
 	cmarg=apply(mat,2,sum)
 	
 	Prop.R= matrix(0,c,1)
 	Prop.C= matrix(0,r,1)
 	
 	Log.K.R= matrix(0,c,1)
 	Log.K.C= matrix(0,r,1)
 	
 	U.R= matrix(0,c,1)
 	U.C= matrix(0,r,1)
 	

 	for(k in 1:c){
 		Prop.R[k,1]=sum(rmarg==k)/r
 		if(Prop.R[k,1]>0){
 		Log.K.R[k,1]=log2(Prop.R[k,1])
 		U.R[k,1]=Prop.R[k,1]*Log.K.R[k,1]
 		}
	}
	
	U.R[4,1]
	 	for(k in 1:r){
 		Prop.C[k,1]=sum(cmarg==k)/c
 		if(Prop.C[k,1]>0){
 		Log.K.C[k,1]=log2(Prop.C[k,1])
 		U.C[k,1]=Prop.C[k]*Log.K.C[k,1]
 		}
	}
	
	Entropy.value=numeric(2)
	
	Entropy.value[1]=-sum(U.R)
	Entropy.value[2]=-sum(U.C)
	values=data.frame(round(Entropy.value[1],4),round(Entropy.value[2],4), row.names=c("Values"))
	colnames(values)=c("Row.Entropy", "Col.Entropy")
	
	return(values)
	
}