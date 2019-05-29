getwd()
setwd("D:/04.학원강의/R예제강의")
install.packages("jpeg")
library(jpeg)

# readJPEG(): jpeg를 색상 Brightness 숫자로 바꾸어 주는 함수
# writeJPEG(): Brightness 숫자를 다시 이미지 파일로 엮어 주는 함ㅅ


# 오른쪽 빗금 처리
roadpot <- readJPEG("./image/roadpot.jpg") 
for (i in 1:dim(roadpot)[1]) {
  for (j in 1:dim(roadpot)[2]) {
    if ( (i - j) %% 50 == 0 ) {
      roadpot[i,j,1] <-1
      roadpot[i,j,2] <-1
      roadpot[i,j,3] <-1
    }
  }
}
writeJPEG(roadpot,target="./image/roadpot_cross2_1.jpg")

# 왼쪽 빗금 처리
roadpot <- readJPEG("./image/roadpot.jpg")
for (i in 1:dim(roadpot)[1]) {
  for (j in 1:dim(roadpot)[2]) {
    if ( (-i - j) %% 50 == 0 ) {
      roadpot[i,j,1] <-1
      roadpot[i,j,2] <-1
      roadpot[i,j,3] <-1
    }
  }
}
writeJPEG(roadpot,target="./image/roadpot_cross3_1.jpg")


# 양쪽 빗금 처리
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

