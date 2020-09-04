#Exercise 5
compress<-function(dr,dr2)
{
  
  files <- list.files(pattern ="\\.jpeg$")
  files1 <- list.files(pattern ="\\.jpg$")
  f <- c(files,files1)
  files2 <- list.files(pattern = "\\.png$")
  
  if(length(files2) >0) {
    for(i in 1:length(files2)) ## enter only when file present in list
    {
      setwd(dr)
      image = readPNG(names(files2[i]))
      dim(image)
      r <- image[,,1]
      g <- image[,,2]
      b <- image[,,3]
      
      image.r.pca <- prcomp(r,center = F)
      image.g.pca <- prcomp(g,center = F)
      image.b.pca <- prcomp(b,center = F)
      
      rgb.pca <- list(image.r.pca,image.g.pca,image.b.pca)
      rgb.pca
      
      ncomp = 50
      
      #X = P*t(A)
      
      R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
      G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
      B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
      ### Exercise 2
      #Repeat step 1 with pixel correction. Observe and report the differences.
      R = ifelse(R<0, 0, R)
      R = ifelse(R>1, 1, R)
      G = ifelse(G<0, 0, G)
      G = ifelse(G>1, 1, G)
      B = ifelse(B<0, 0, B)
      B = ifelse(B>1, 1, B)
      setwd(dr2)   ## change directory to new folder
      img = array(c(R,G,B), dim=c(dim(image)[1:2],3))
      writePNG(img,names(files2[i]))
    }
    
  }
  
  if(length(f) >0) { ## enter only when file present in list
    for(i in 1:length(f))  
    {
      setwd(dr)
      image = readJPEG(f[i])
      dim(image)
      r <- image[,,1]
      g <- image[,,2]  ### code for jpeg file compression 
      b <- image[,,3]
      
      image.r.pca <- prcomp(r,center = F)
      image.g.pca <- prcomp(g,center = F)
      image.b.pca <- prcomp(b,center = F)
      
      rgb.pca <- list(image.r.pca,image.g.pca,image.b.pca)
      rgb.pca
      
      ncomp = 50
      
      #X = P*t(A)
      
      R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
      G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
      B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
      ### Exercise 2
      #Repeat step 1 with pixel correction. Observe and report the differences.
      R = ifelse(R<0, 0, R)
      R = ifelse(R>1, 1, R)
      G = ifelse(G<0, 0, G)
      G = ifelse(G>1, 1, G)
      B = ifelse(B<0, 0, B)
      B = ifelse(B>1, 1, B)
      setwd(dr2)  ## change directoryto new folder
      img = array(c(R,G,B), dim=c(dim(image)[1:2],3))
      writeJPEG(img,f[i])
    }
    
  }
  
  answer<-readline("yesno: wanna delete all orignal files?")
  if (answer=='yes') 
  {
    f1 <-c(f,files2)
    setwd(dr)
    for(i in 1:length(f1))   ## code to check delete orignal file or not
    { #Check its existence
      if (file.exists(f[i]))
        #Delete file if it exists
        file.remove(f[i])
    } 
  }
  else
  {
    print('okay')
    
    }
}





dr="/home/jayesh/Desktop"
dr2="/home/jayesh/Desktop/New Folder"
setwd(dr)
compress(dr,dr2)
