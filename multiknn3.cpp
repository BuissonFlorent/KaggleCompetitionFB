#include <Rcpp.h>
using namespace Rcpp;


// Defining the current local bin of points to test
// [[Rcpp::export]]
NumericMatrix getArea3(NumericMatrix mat, int xi, int yi, int xbin_col, int ybin_col){
  LogicalVector condition1 = (mat(_,xbin_col)==xi)*(mat(_,ybin_col)==yi);
  int m1=mat.ncol();
  int n1=mat.nrow();
  int area_nr=sum(condition1);
  NumericMatrix area(area_nr,m1);
  for (int i = 0, j = 0; i < n1; i++) {
    if((mat(i,xbin_col)==xi) && (mat(i,ybin_col)==yi)){
      area(j,_) = mat(i,_);
      j = j+1;
    }
  }
  return(area);
}


// Selecting the points from the train set which are in the bin
// or in the neighboring bins
// [[Rcpp::export]]
NumericMatrix getNbhd3(NumericMatrix mat, int xi, int yi, int xbin_col, int ybin_col){
	int n2=mat.nrow();
	int m2=mat.ncol();
	LogicalVector condition2 = (mat(_,xbin_col)>=(xi-1))*(mat(_,xbin_col)<=(xi+1))*(mat(_,ybin_col)>=(yi-1))*(mat(_,ybin_col)<=(yi+1));
	int nbhd_nr=sum(condition2);
	NumericMatrix nbhd(nbhd_nr,m2);
		for (int i = 0, j = 0; i < n2; i++) {
			if( (mat(i,xbin_col)>=(xi-1)) && (mat(i,xbin_col)<=(xi+1)) && (mat(i,ybin_col)>=(yi-1)) && (mat(i,ybin_col)<=(yi+1)) ){
				nbhd(j,_) = mat(i,_);
				j = j+1;
			}
		}
	return(nbhd); 
}

// For each point in the test dataset, calculating the distance between
// the point and all the points in the neighborhood, and selecting the 
// closest neighbors
// [[Rcpp::export]]
NumericMatrix getClosest_Nb3(NumericVector area_line, NumericMatrix nbhd){
	int m2=nbhd.ncol();
	int nb_close_nbhrs = 20;
	int nbhd_nr=nbhd.nrow();
	NumericMatrix Closest_Neighbors(nb_close_nbhrs+1,m2);
	std::fill(Closest_Neighbors.begin(),Closest_Neighbors.end(),10000);
	//Looping through the neighborhood dataset
	for(int k=0; k < nbhd_nr; k++){
        Closest_Neighbors(0,_)=nbhd(k,_);
        //Calculating the distance between the test point and the neighbors
		// Starting with the columns for x and y
		double dist = pow(Closest_Neighbors(0,1)-area_line[1], 2.0)+pow(Closest_Neighbors(0,2)-area_line[2], 2.0);
		// Adding the other columns
		for(int d=5; d < 9; d++){
			dist+=pow(Closest_Neighbors(0,d)-area_line[d], 2.0);
		}
        Closest_Neighbors(0,10)=dist;
        //Immediately sorting the Closest_Neighbors set
        for(int l=0;l<nb_close_nbhrs;l++){
            if(Closest_Neighbors(l,10)<Closest_Neighbors(l+1,10)){
				NumericVector replace(m2);
				replace=Closest_Neighbors(l+1,_);
				Closest_Neighbors(l+1,_)=Closest_Neighbors(l,_);
				Closest_Neighbors(l,_)=replace;
            }
            else{break;}
          }
        }
    Closest_Neighbors=Closest_Neighbors(Range(1,nb_close_nbhrs),_);
	return(Closest_Neighbors); 
}

// Collapsing the Closest Neighbors matrix by place id
// [[Rcpp::export]]
NumericVector getPlaces3(NumericMatrix Closest_Neighbors){
	NumericMatrix places(Closest_Neighbors.nrow(),2);
	std::fill(places.begin(),places.end(),0);
	//Looping through the closest neighbors
	for(int i=0; i<Closest_Neighbors.nrow(); i++){
		for(int j=0; j<places.nrow(); j++){
			//checking whether one of the places already registered has the same place id as the neighbor
			if(places(j,0)==Closest_Neighbors(i,9)){
				places(j,1)++;
				//Immediately updating the order of the closest neighbors by count of check-ins
				for(int l=0;l<j;l++){
					if(places(j-l,1)>places(j-l-1,1)){
						NumericVector replace(2);
						replace=places(j-l-1,_);
						places(j-l-1,_)=places(j-l,_);
						places(j-l,_)=replace;
					}

				}
				break;
			}
			else if(places(j,0)==0){
				places(j,0)=Closest_Neighbors(i,9);
				places(j,1)++;
				break;
			}
		}
	}
	//Flattening the matrix to get a vector output
	NumericVector out(10);
	for(int m=0;m<5;m++){
		for(int n=0;n<2;n++){
			out[2*m+n]=places(m,n);
		}
	}
	return(out);
}
	
	
	




// [[Rcpp::export]]
NumericMatrix multiknn3(NumericMatrix test, NumericMatrix train){
	NumericMatrix Predict(test.nrow(),11);
	int Pred_row=0;
	int xbin_min=min(test.column(3));
	int xbin_max=max(test.column(3));
	int ybin_min=min(test.column(4));
	int ybin_max=max(test.column(4));
		  
  //Looping through the bins of the grid
  for(int xi=xbin_min; xi<=xbin_max; xi++){
    for(int yi=ybin_min; yi<=ybin_max; yi++){
      
      //int xi=xbin_min, yi=ybin_min;
      
      // Selecting the points from the test set which are in the bin
      NumericMatrix area=getArea3(test,xi,yi,3,4);
      int area_nr=area.nrow();
	  if(area_nr==0){continue;}
            
      // Selecting the points from the train set which are in the bin
      // or in the neighboring bins
	  NumericMatrix nbhd=getNbhd3(train,xi,yi,3,4);
	  //int nbhd_nr=nbhd.nrow();
	        
      
      // For each point in the test dataset, calculating the distance between
      // the point and all the points in the neighborhood, and selecting the 
      // closest neighbors
      
      //Looping through the test dataset
      for(int i = 0; i < area_nr; i++){
		//Getting the closest neighbors
		NumericVector area_line= area(i,_);
        NumericMatrix Closest_Neighbors=getClosest_Nb3(area_line,nbhd);
		
		//Collapsing the matrix of closest neighbors by place id and pushing the values to the Predict matrix
		NumericVector v=getPlaces3(Closest_Neighbors);
		Predict(Pred_row,0)=area(i,0);
		for(int u=1;u<11; u++){
			Predict(Pred_row,u)=v[u-1];
		}
		Pred_row++;
      }
    }
  }
  return(Predict);
}