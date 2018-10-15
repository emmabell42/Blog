allChanges <- cbind(
RegionSet1=c(-0.09543259,-0.17720883,-0.07194901,-0.06244325,-0.21434579,-0.73760726,-1.61031951,-2.08658316,-2.31103527,-1.90112607,-2.11555125,-2.36536705,-1.88156024,-1.35244766,-1.06747386),
RegionSet2=c(-3.09545648,-3.71080988,-3.59864198,-3.69171402,-3.20359125,-2.13559904,-1.0124959,-0.4366908,-0.35548768,-0.52212161,-0.43754211,-0.29075204,-0.37239281,-0.18810284,-0.07339725),
RegionSet3=c(-0.5132005,-0.3003632,-0.2428687,-0.3986333,-0.5946211,-0.4937398,-0.518903,-0.1722765,0.8079712,2.6267846,3.9429172,4.1655023,3.5955679,2.6160757,1.5748422))

largeChanges <- apply(allChanges,2,function(x) which(abs(x)>1))

is.sequential <- function(x){
  all(diff(x) == 1)
}  

sapply(largeChanges,is.sequential)

# Here's a fancier version of the is.sequential function that allows you to test for difference sequence directions and increment sizes.

is.sequential <- function(x,direction=NULL,incrementSize=NULL){

	if(missing(direction)){
		direction <- "up"
		cat("Argument 'direction' not specified. Defaulting to 'up'. Alternatively, set this argument to 'down' or 'either'.","\n")
	}
	
	if(missing(incrementSize)){
		incrementSize <- 1
		cat("Argument 'incrementSize' not specified. Defaulting to 1.","\n")
	}
	
	if(direction=="up"){
		return(all(diff(x) == incrementSize))
	}
	
	if(direction=="down"){
		return(all(diff(x) == incrementSize*-1))
	}
	
	if(direction=="either"){
		return(all(abs(diff(x)) == incrementSize))
	}
}
