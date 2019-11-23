#install.packages("maptools")
library("maptools")
gpclibPermit()

getwd()
setwd("C:\\Users\\wldyd\\Desktop\\17-겨울\\인턴십\\CTPRVN_201703")
#area <- readShapePoly("TL_SCCO_CTPRVN.shp")

#install.packages("maptools")
library("maptools")

#install.packages("RColorBrewer")
library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

#install.packages("ggmap")
library(ggmap)
#ggmap(area)

#install.packages("rgdal");
#install.packages("ggplot2");
#install.packages("ggmap");
#install.packages("raster");
library(raster)
library(rgdal) # R wrapper around GDAL/OGR
library(ggplot2) # for general plotting
library(ggmap) # for fortifying shapefiles
library(CARBayes)
library(CARBayesdata)
library(sp)
library("rgdal")
library("foreign");
#install.packages("dplyr")
library("dplyr");
library("magrittr");
library("maps");
library("spdep");
#install.packages("spDataLarge")
library("spDataLarge")
library("mapproj");
library("ggplot2")


########


test_shp <- readShapePoly("TL_SCCO_CTPRVN.shp")
test_shp

test_dbf <- read.dbf("TL_SCCO_CTPRVN.dbf")
test_dbf

test_data <- read.csv("poisson_data.csv")
test_data

test_dbf <- cbind(test_dbf, test_data[,c(2:14)])
test_dbf

test_shp@data <- test_dbf;

test_shp@data[,c(3,17)]
##W

w_mat <- matrix(0,nrow = 17, ncol = 17);
#wmat[i,] <- c(s,p,d,i,g,d,u,s,k,g,c,c,j,j,k,k,j);
w_mat[1,] <- c(0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0);
w_mat[2,] <- c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0);
w_mat[3,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0);
w_mat[4,] <- c(1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0);
w_mat[5,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0);
w_mat[6,] <- c(0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0);
w_mat[7,] <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0);
w_mat[8,] <- c(0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0);
w_mat[9,] <- c(1,0,0,1,0,0,0,0,0,1,1,1,0,0,0,0,0);
#wmat[ij,] <- c(s,p,d,i,g,d,u,s,k,g,c,c,j,j,k,k,j);
w_mat[10,] <- c(0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0);
w_mat[11,] <- c(0,0,0,0,0,1,0,1,1,1,0,1,1,0,1,0,0);
w_mat[12,] <- c(0,0,0,0,0,1,0,1,1,0,1,0,1,0,0,0,0);
w_mat[13,] <- c(0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,1);
w_mat[14,] <- c(0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0);
w_mat[15,] <- c(0,0,1,0,0,0,1,0,0,1,1,0,1,0,0,1,0);
w_mat[16,] <- c(0,1,1,0,0,0,1,0,0,0,0,0,1,1,1,0,0);
w_mat[17,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
w_mat;

test_shp

test_data[1,3];test_data[1,2]

#SMR_plot_test
as.double(test_data[2,3])
test_data[,4]
SMR <- matrix(0, nrow = 17, ncol = 1);
for(i in 1: 17) {
  SMR[i,1] <- (as.double(test_data[i,3]) / as.double(test_data[i,2]));
}
SMR
test_data <- cbind(test_data, SMR)
test_data
test_shp@data <- test_data
test_shp@data[,c(3,15)]

northarrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(220000,647000),scale = 4000)
scalebar <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(225000, 647000), scale = 10000, fill = c("transparent", "black"));
text1 <- list("sp.text", c(225000,649000), "0");
text2 <- list("sp.text",c(230000, 649000), "5000m")
breakpoints <- seq(min(test_data$SMR) - 0.0005, max(test_data$SMR) + 0.0005, length.out = 10000);
spplot(test_shp, c("SMR"), sp.layout = list(northarrow, scalebar, text1, text2), scales = list(draw = TRUE), at=breakpoints, col.regions = terrain.colors(n = length(breakpoints)-1), col="black")

##variable change
test_shp@data$logSMR <- log(test_shp@data$SMR);
test_shp@data$scalegni <- (test_shp@data$gni)/1000;
test_shp@data$loggni <- log(as.double(test_shp@data$gni));
#test_shp@data$logwel <- log(as.double(test_shp@data$num_of_welfare));
#test_shp@data$logheal <- log(as.double(test_shp@data$subjective_health));
test_shp@data$logser <- log(as.double(test_shp@data$num_of_service));
test_shp@data$logE <- log(as.double(test_shp@data$E))
#test_shp@data$log_base <- log(as.double(test_shp@data$basic_income))
test_shp@data$gni
test_shp@data$scalegni
test_shp@data$loggni
test_shp@data$logser
test_shp@data$logE
#test_shp@data$per_mental_ins

#error term test
error_term_test <- lm(test_shp@data$logSMR ~ offset(test_shp@data$logE) + test_shp@data$scalegni + test_shp@data$per_mental_person)
error<-residuals(error_term_test)
summary(error_term_test)
test_data <- cbind(test_data,error)
test_shp@data <- test_data
test_shp@data

v <- test_shp@data$error;

w_sum = sum(w_mat)

w_sum
mean_error<-mean(v)
error_sum = 0;

for(i in 1:17) {
  error_sum <- error_sum + (v[i] - mean_error)^2
}
I_sum = 0;
for(i in 1:17) {
  for(j in 1:17) {
    I_sum = I_sum + w_mat[i,j]*(v[i]-mean_error)*(v[j]-mean_error)
  }
}
test_stat <- (17 * I_sum) / (w_sum * error_sum);
test_stat

moran_I = NULL

for(k in 1:10000) {
  v <- permute(v)
  mean_error<-mean(v)
  error_sum = 0;
  
  for(i in 1:17) {
    error_sum <- error_sum + (v[i] - mean_error)^2
  }
  I_sum = 0;
  for(i in 1:17) {
    for(j in 1:17) {
      I_sum = I_sum + w_mat[i,j]*(v[i]-mean_error)*(v[j]-mean_error)
    }
  }
  moran_I = c(moran_I, (17 * I_sum) / (w_sum * error_sum));
}

mean_ts <- mean(moran_I)
var_ts <- var(moran_I)

z_score<-(test_stat - mean_ts)/sqrt(var_ts)

z_score

hist(moran_I)

quantile(moran_I, 0.025)
quantile(moran_I, 0.975)
#model <- lm(test_shp@data$logSMR ~ test_shp@data$loggni + test_shp@data$num_of_welfare + test_shp@data$logheal, data = test_shp@data)

#W.nb <- poly2nb(test_shp);
#W.list <- nb2listw(W.nb, style = "B", zero.policy = TRUE);
#class(W.list)
#resid.model <- residuals(model);
#moran.mc(x = resid.model, w = w_mat, nsim = 1000, zero.policy = TRUE)
#summary(model)
#moran.mc
##intercept_only_carbayes

form <- test_shp@data$Y ~ offset(test_shp@data$logE) + test_shp@data$scalegni + test_shp@data$per_mental_person

#compare model
##intrinsic car
model_CAR_ler <- S.CARleroux(formula = form, data = test_shp@data, family = "poisson", W = w_mat, burnin = 20000, n.sample = 120000, thin = 10, fix.rho = TRUE, rho = 1);
print(model_CAR_ler)

#ler model
model_CAR_ler2 <- S.CARleroux(formula = form, data = test_shp@data, family = "poisson", W = w_mat, burnin = 20000, n.sample = 120000, thin = 10)
print(model_CAR_ler2)
##near has strong, further has weak correlation : overdispersion

##bym
model_CAR_bym <- S.CARbym(formula = form, data = test_shp@data, family = "poisson", W = w_mat, burnin = 20000, n.sample = 120000, thin = 10)
print(model_CAR_bym)
##strong nearby, weak further, but no overdispersion

##dissimiliar
#model_CAR_dis <- S.CARdissimilarity()
#print(model_CAR_dis)
##only nearby, may or may not correlated: need to use metric > 시간 남으면..

##glm
model_CAR_glm <- S.glm(formula = form, data = test_shp@data, family = "poisson", burnin = 20000, n.sample = 120000, thin = 10)
print(model_CAR_glm)
##no spatial correlation
test_shp@data$gni
plot(test_shp@data$gni, test_shp@data$SMR)
plot(test_shp@data$per_mental_person, test_shp@data$SMR, xlim = c(20, 70), ylim = c(0.5,1.5))

##남은 일
#다른 파일로 spatial dependence 유무 dic 비교 not yet;
#내생성 도구변수 선정;
#model selection;
