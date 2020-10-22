#!R --vanilla
##paquetes necesarios
require(dplyr)
require(tidyr)
require(raster)
##require(gdata)
#require(foreign)
#require(fractaldim)
#require(SDMTools)
require(chron)
require(detect)

## ubicación de la carpeta de trabajo y el repositorio local

if (Sys.getenv("WORKDIR") == "") {
   ## for Izza:
   work.dir <- "D:/PROJECTS/Gran Sabana/Metodologia"
   script.dir <- "???"
} else {
   ## for JR:
   work.dir <- Sys.getenv("WORKDIR")
   script.dir <- Sys.getenv("SCRIPTDIR")
}


args <- commandArgs(TRUE)
sp <- args[1]
k <- args[2]

## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
load(GIS.data)
mi.rda <- sprintf("%s/Rdata/padata-%s.rda",script.dir,sp)
if (file.exists(mi.rda))
   load(file=mi.rda)


exito <- "NOT"

linkfuns <- c("cloglog","probit")
detvars <- c("muestreo + (walk * cam)","muestreo + walk + cam","walk * cam","walk + cam")
occvars <- c("bsq + dcon + dcom + frs + dbsq",
"bsq + dcon + frs + dbsq",
"bsq + dcon + frs + dbsq",
"bsq + dcom + frs + dbsq",
"bsq + dcon + dcom + frs",
"bsq + dcon + dbsq",
"bsq + dcon + frs",
"bsq + frs + dbsq",
"bsq + dcon + dcom",
"bsq + dcom + dbsq",
"bsq + dcom + frs",
"bsq + dbsq",
"bsq + dfrs",
"bsq + dcon",
"bsq + dcom"
)

  ss <- switch(k,`1`=rep(T,nrow(pa.data)),`2`=pa.data$muestreo,`3`=pa.data$metodo,`4`=pa.data$metodo & pa.data$muestreo)

  for (occvar in occvars) {
     for(mi.link in linkfuns) {
        for (detvar in detvars) {
           if (exito != "OK") {
              fit <- svocc(formula(sprintf("pa ~ %s |  %s",occvar,detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
              if (all(abs(fit$coefficients$sta)<6) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<4) ) {
                 fit.null <- svocc(formula(sprintf("pa ~ bloque |  %s",detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
                 if (AIC(fit)<AIC(fit.null)) {
                   assign(sprintf("%s.full",sp),fit)
                   bfit <- bootstrap(fit, B=50)
                   assign(sprintf("%s.boot",sp),bfit)
                   bfit <- bootstrap(fit.null, B=50)
                   assign(sprintf("%s.null",sp),bfit)
                   exito <- "OK"
                 }
              }
           }
        }
     }
  }


  for (occvar in occvars) {
     for(mi.link in linkfuns) {
        for (detvar in detvars) {
           if (exito != "OK") {
              fit <- svocc(formula(sprintf("pa ~ %s |  %s",occvar,detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
              if (all(abs(fit$coefficients$sta)<10) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<6) ) {
                 fit.null <- svocc(formula(sprintf("pa ~ bloque |  %s",detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
                 if (AIC(fit)<AIC(fit.null)) {
                   assign(sprintf("%s.alt",sp),fit)
                   bfit <- bootstrap(fit, B=50)
                   assign(sprintf("%s.boot",sp),bfit)
                   bfit <- bootstrap(fit.null, B=50)
                   assign(sprintf("%s.null",sp),bfit)
                   exito <- "OK"
                 }
              }
           }
        }
     }
  }

  for (occvar in occvars) {
     for(mi.link in linkfuns) {
        for (detvar in detvars) {
           if (exito != "OK") {
              prefit <- svocc(formula(sprintf("pa ~ %s |  %s",occvar,detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
              fit <- svocc.step(prefit, model="sta")
              if (all(abs(fit$coefficients$sta)<6) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<4) ) {
                 fit.null <- svocc(formula(sprintf("pa ~ bloque |  %s",detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
                 if (AIC(fit)<AIC(fit.null)) {
                   assign(sprintf("%s.step",sp),fit)
                   bfit <- bootstrap(fit, B=50)
                   assign(sprintf("%s.boot",sp),bfit)
                   bfit <- bootstrap(fit.null, B=50)
                   assign(sprintf("%s.null",sp),bfit)
                   exito <- "OK"
                 }
              }
           }
        }
     }
  }

  for (occvar in occvars) {
     for(mi.link in linkfuns) {
        for (detvar in detvars) {
           if (exito != "OK") {
              prefit <- svocc(formula(sprintf("pa ~ %s |  %s",occvar,detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
              fit <- svocc.step(prefit, model="sta")
              if (all(abs(fit$coefficients$sta)<10) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<6) ) {
                 fit.null <- svocc(formula(sprintf("pa ~ bloque |  %s",detvar)), data=pa.data, subset=ss, link.sta = mi.link, link.det = "logit", penalized = FALSE, method = c( "optim"))
                 if (AIC(fit)<AIC(fit.null)) {
                   assign(sprintf("%s.step",sp),fit)
                   bfit <- bootstrap(fit, B=50)
                   assign(sprintf("%s.boot",sp),bfit)
                   bfit <- bootstrap(fit.null, B=50)
                   assign(sprintf("%s.null",sp),bfit)
                   exito <- "OK"
                 }
              }
           }
        }
     }
  }

  if (exito != "OK") {
     bfit <- bootstrap(fit.null, B=50)
     assign(sprintf("%s.null",sp),bfit)
  }
save(file=sprintf("%s/Rdata/svocc-%s-%s.rda",script.dir,sp,k),list=ls(pattern=".boot|.full|.null|.alt|.step"))
