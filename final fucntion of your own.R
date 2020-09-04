library(mice)
library(VIM)
dataclean<-function(dt,var)
{
  answer <-readline("1 for remove outlier,2 for outlier treatment,3 for NA impute") 
  if(answer==1)
  {
    if(sum(is.na(dt))>0)
    {
      stop("function cannot work with missing value")
    }
    var_name <- eval(substitute(var),eval(dt))
    if(is.numeric(var_name))
    {
      outlier <- boxplot.stats(var_name)$out
      su <- sum(outlier)
      if(is.na(outlier)){
        stop("variables has no outliers")
      }
      na1 <-sum(is.na(var_name))
      boxplot(var_name,main="boxplot with outliers")
      hist(var_name,main="histogram with outliers")
      m1<-mean(var_name)
      print(paste("mean before removal of outliers",m1))
      mo <- mean(outlier)
      print(paste("mean of outliers",mo))
      var_name1 <- ifelse(var_name %in% outlier, NA, var_name)
      m2 <-mean(var_name1)
      print(paste("mean after removal of outliers",m2))
      boxplot(var_name1,main="boxplot without outliers")
      hist(var_name1,main="histogram without outliers")
    } 
    return(var_name)
  }
  else if(answer==2)
  {
    if(sum(is.na(dt))>0)
    {
      stop("function cannot work with missing value")
    }
    var_name <- eval(substitute(var),eval(dt))
    if(is.numeric(var_name))
    {
      outlier <- boxplot.stats(var_name)$out
      su <- sum(outlier)
      if(is.na(outlier)){
        stop("variables has no outliers")
      }
      boxplot(var_name,main="boxplot with outliers")
      hist(var_name,main="histogram with outliers")
      m1 <-mean(var_name)
      print(paste("mean before removal of outliers",m1))
      Q3<-quantile(var_name,.75)
      Q1<-quantile(var_name,.25)
      bench = Q3+1.5*(Q3-Q1)
      var_name[var_name>bench] <-bench
      m2 <-mean(var_name)
      print(paste("mean before removal of outliers",m1))
      boxplot(var_name,main="boxplot without outliers")
      hist(var_name,main="histogram without outliers")
      
    
    }
    return(var_name) 
  }
  else if(answer==3){
     var_name<-matrix()
      if(sum(is.na(dt))>0)
      {
        tempData <- mice(dt,m=5,maxit=50,meth='pmm',seed=500)
        print(tempData)
       # summary(tempData)
        dt <- complete(tempData,1)
      }
      
     return(dt) 
  }
 
  
}
rs<-dataclean(df,sepal_length)
df=read.csv("/home/jayesh/Downloads/iris.csv")
View(df)
View(rs)
