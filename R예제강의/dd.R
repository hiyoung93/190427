getwd()


setwd("C:/Users/1pc/Desktop/4.28���̽�� ������/R��������")

install.packages("jpeg")

library(jpeg)

#������ ����ó����

roadpot <- readJPEG("./image/roadpot.jpg")

roadpot
dim(roadpot)[1]
dim(roadpot)[2]
for(i in 1:dim(roadpot)[1]){
  for(j in 1 : dim(roadpot)[2]){
    if((i - j) %% 50 == 0){
      roadpot[i, j, 1 ] <-1
      roadpot[i, j, 2 ] <-1
      roadpot[i, j, 3 ] <-1
    }
  }
}

writeJPEG(roadpot, target="./image/roadpot_cross2.jpg")


#���� ���� ó�� - ������
#���� ���� ó��

pic_break <-function(data, right = TRUE) {
  for (i in 1:dim(data)[1]) {
    if(!right) {
      i = -i
    }
    for (j in 1:dim(data)[2]) {
      if ( (i - j) %% 50 == 0 ) {
        data[abs(i),j,1] <-1
        data[abs(i),j,2] <-1
        data[abs(i),j,3] <-1
      }
    }
  }
  return(data)
}
roadpot <- readJPEG("./image/roadpot.jpg")
cross_right <- pic_break(roadpot)
cross_diamond <- pic_break(cross_right, right = FALSE)
writeJPEG(cross_diamond, target = "./image/cross_diamond_1.jpg")


