setwd("C:/Users/Rodrigo/Documents/MAESTRÍA/Tesis/R/")
STDr<-read.table("STD2.txt",header=T,sep="\t")
names(STDr)
dim(STDr)

Precios<-read.table("Precios_Series_2008_70.txt",header=T,dec=",")
names(Precios)
dim(Precios)

library(ghyp)
library(GeneralizedHyperbolic)
library(fBasics)
library(rugarch)
library(copula)
library(scatterplot3d)
library(VineCopula)
library(CDVine)
library(vines)
library(rgl)
library(car)
library(copBasic)
library(lattice)
library(stats)
library(TSP)

PFBCOLOM<-ts(Precios[,2],start=1,end=1827,frequency=1)
ECOPETROL<-ts(Precios[,3],start=1,end=1827,frequency=1)
ISA<-ts(Precios[,4],start=1,end=1827,frequency=1)
ISAGEN<-ts(Precios[,5],start=1,end=1827,frequency=1)
EXITO<-ts(Precios[,6],start=1,end=1827,frequency=1)
BOGOTA<-ts(Precios[,7],start=1,end=1827,frequency=1)
BCOLOMBIA<-ts(Precios[,8],start=1,end=1827,frequency=1)
BVC<-ts(Precios[,9],start=1,end=1827,frequency=1)
CORFICOLCF<-ts(Precios[,10],start=1,end=1827,frequency=1)
GRUPOSURA<-ts(Precios[,11],start=1,end=1827,frequency=1)
NUTRESA<-ts(Precios[,12],start=1,end=1827,frequency=1)
GRUPOARGOS<-ts(Precios[,13],start=1,end=1827,frequency=1)
CELSIA<-ts(Precios[,14],start=1,end=1827,frequency=1)

rPFB<-diff(log(PFBCOLOM))
rECO<-diff(log(ECOPETROL))
rISA<-diff(log(ISA))
rGEN<-diff(log(ISAGEN))
rEXI<-diff(log(EXITO))
rBOG<-diff(log(BOGOTA))
rBCO<-diff(log(BCOLOMBIA))
rBVC<-diff(log(BVC))
rCOR<-diff(log(CORFICOLCF))
rGRS<-diff(log(GRUPOSURA))
rNUT<-diff(log(NUTRESA))
rGRA<-diff(log(GRUPOARGOS))
rCEL<-diff(log(CELSIA))

LogReturns<-cbind(rPFB,rECO,rISA,rGEN,rEXI,rBOG,rBCO,rBVC,rCOR,rGRS,rNUT,rGRA,rCEL)
LogRi<-as.matrix(LogReturns, ncol=13)
TauRi<-cor(x=LogRi, use="everything", method="kendall")

#=================================================================================================================
#=====================================================================================================================================================
### DEPENDENCE STRUCTURE MODELING

# TRANSFORMATION OF THE MARGINAL STANDARDIZED RESIDUALS OF DAILY LOG RETURNS TO
# UNIFORMLY DISTRIBUTED DATA (COPULA DATA)

basicStats(STDr[,1])
basicStats(STDr[,2])
basicStats(STDr[,3])
basicStats(STDr[,4])
basicStats(STDr[,5])
basicStats(STDr[,6])
basicStats(STDr[,7])
basicStats(STDr[,8])
basicStats(STDr[,9])
basicStats(STDr[,10])
basicStats(STDr[,11])
basicStats(STDr[,12])
basicStats(STDr[,13])

# PDFs STANDARDIZED RESIDUALS
PDF_PFBCOLOM<-ddist(distribution="ghyp",y=STDr[,1],mu=-0.001611,sigma=1.001558,lambda=2.602795,skew=0.004969,shape=0.251993)
PDF_ECOPETROL<-ddist(distribution="ghyp",y=STDr[,2],mu=0.002721,sigma=0.998721,lambda=-1.047574,skew=-0.122713,shape=1.033838)
PDF_ISA<-ddist(distribution="ghyp",y=STDr[,3],mu=0.003271,sigma=0.996752,lambda=1.451845,skew=-0.015722,shape=0.25)
PDF_ISAGEN<-ddist(distribution="ghyp",y=STDr[,4],mu=0.009449,sigma=1.033366,lambda=-1.713276,skew=0.148599,shape=0.25)
PDF_EXITO<-ddist(distribution="ghyp",y=STDr[,5],mu=-0.003683,sigma=0.999577,lambda=0.880668,skew=0.016197,shape=0.25)
PDF_BOGOTA<-ddist(distribution="ghyp",y=STDr[,6],mu=0.012211,sigma=1.055602,lambda=0.265185,skew=-0.001503,shape=0.25)
PDF_BCOLOMBIA<-ddist(distribution="ghyp",y=STDr[,7],mu=0.013834,sigma=0.999769,lambda=1.971627,skew=-0.038343,shape=0.250033)
PDF_BVC<-ddist(distribution="ghyp",y=STDr[,8],mu=-0.040195,sigma=1.013842,lambda=0.566383,skew=-0.009752,shape=0.25)
PDF_CORFICOLCF<-ddist(distribution="ghyp",y=STDr[,9],mu=0.003288,sigma=1.008330,lambda=0.749288,skew=-0.015556,shape=0.25)
PDF_GRUPOSURA<-ddist(distribution="ghyp",y=STDr[,10],mu=0.029949,sigma=0.994565,lambda=1.076872,skew=-0.037263,shape=0.25)
PDF_NUTRESA<-ddist(distribution="ghyp",y=STDr[,11],mu=0.007369,sigma=1.011311,lambda=1.369283,skew=-0.022049,shape=0.270325)
PDF_GRUPOARGOS<-ddist(distribution="ghyp",y=STDr[,12],mu=0.015561,sigma=0.999312,lambda=1.378296,skew=-0.053536,shape=0.25)
PDF_CELSIA<-ddist(distribution="ghyp",y=STDr[,13],mu=0.003452,sigma=1.004437,lambda=0.636637,skew=0.001499,shape=0.25)

# CDFs STANDARDIZED RESIDUALS
# Each time series is filltered using an ARMA(p,q)-GARCH(s,m) model with ghyp innovations and
# standardized residuals are transformed parametrically to copula data using the generalyzed
# hyperbolic distribution function

PFB<-pdist(distribution="ghyp",q=STDr[,1],mu=-0.001611,sigma=1.001558,lambda=2.602795,skew=0.004969,shape=0.251993)
ECO<-pdist(distribution="ghyp",q=STDr[,2],mu=0.002721,sigma=0.998721,lambda=-1.047574,skew=-0.122713,shape=1.033838)
ISA<-pdist(distribution="ghyp",q=STDr[,3],mu=0.003271,sigma=0.996752,lambda=1.451845,skew=-0.015722,shape=0.25)
GEN<-pdist(distribution="ghyp",q=STDr[,4],mu=0.009449,sigma=1.033366,lambda=-1.713276,skew=0.148599,shape=0.25)
EXI<-pdist(distribution="ghyp",q=STDr[,5],mu=-0.003683,sigma=0.999577,lambda=0.880668,skew=0.016197,shape=0.25)
BOG<-pdist(distribution="ghyp",q=STDr[,6],mu=0.012211,sigma=1.055602,lambda=0.265185,skew=-0.001503,shape=0.25)
BCO<-pdist(distribution="ghyp",q=STDr[,7],mu=0.013834,sigma=0.999769,lambda=1.971627,skew=-0.038343,shape=0.250033)
BVC<-pdist(distribution="ghyp",q=STDr[,8],mu=-0.040195,sigma=1.013842,lambda=0.566383,skew=-0.009752,shape=0.25)
COR<-pdist(distribution="ghyp",q=STDr[,9],mu=0.003288,sigma=1.008330,lambda=0.749288,skew=-0.015556,shape=0.25)
GRS<-pdist(distribution="ghyp",q=STDr[,10],mu=0.029949,sigma=0.994565,lambda=1.076872,skew=-0.037263,shape=0.25)
NUT<-pdist(distribution="ghyp",q=STDr[,11],mu=0.007369,sigma=1.011311,lambda=1.369283,skew=-0.022049,shape=0.270325)
GRA<-pdist(distribution="ghyp",q=STDr[,12],mu=0.015561,sigma=0.999312,lambda=1.378296,skew=-0.053536,shape=0.25)
CEL<-pdist(distribution="ghyp",q=STDr[,13],mu=0.003452,sigma=1.004437,lambda=0.636637,skew=0.001499,shape=0.25)

PFB<-pdist(distribution="norm",q=STDr[,1],mu=-0.001699,sigma=1.002793)
ECO<-pdist(distribution="norm",q=STDr[,2],mu=0.003046,sigma=1.001001)
ISA<-pdist(distribution="norm",q=STDr[,3],mu=0.001671,sigma=1.000217)
GEN<-pdist(distribution="norm",q=STDr[,4],mu=0.013274,sigma=1.000041)
EXI<-pdist(distribution="norm",q=STDr[,5],mu=0.000021,sigma=0.999659)
BOG<-pdist(distribution="norm",q=STDr[,6],mu=0.021887,sigma=1.000765)
BCO<-pdist(distribution="norm",q=STDr[,7],mu=0.013516,sigma=1.000383)
BVC<-pdist(distribution="norm",q=STDr[,8],mu=-0.030707,sigma=0.999621)
COR<-pdist(distribution="norm",q=STDr[,9],mu=-0.008299,sigma=1.000469)
GRS<-pdist(distribution="norm",q=STDr[,10],mu=0.033415,sigma=1.000296)
NUT<-pdist(distribution="norm",q=STDr[,11],mu=0.007330,sigma=0.995915)
GRA<-pdist(distribution="norm",q=STDr[,12],mu=0.015141,sigma=1.000984)
CEL<-pdist(distribution="norm",q=STDr[,13],mu=-0.006181,sigma=1.001236)

CDF_Join<-cbind(PFB, ECO, ISA, GEN, EXI, BOG, BCO, BVC, COR, GRS, NUT, GRA, CEL)
MCDF<-as.matrix(CDF_Join, ncol=13)

# EXPLORATORY DATA ANALYSIS

# EMPIRICAL KENDALL'S TAU MATRIX
TauCDF<-TauMatrix(data=MCDF)

# EMPIRICAL CONTOUR PLOTS
panel.contour<-function(x, y, bw=2, size=100){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(-3,3,-3,3), new=TRUE)
  BiCopMetaContour(x, y, bw, size, axes=FALSE)
}
windows()
EDApairwiseplot<-pairs(x=MCDF, lower.panel=panel.contour, gap=0);EDApairwiseplot

# EMPIRICAL BIVARIATE COPULAS SELECTION MATRIX

MBiCop<-matrix(nrow=13, ncol=13)
MParBiCop<<-matrix(nrow=13, ncol=13)
MPar2BiCop<-matrix(nrow=13, ncol=13)
M.UpperTailDep_BiCop<-matrix(nrow=13, ncol=13)
M.LowerTailDep_BiCop<-matrix(nrow=13, ncol=13)
M.Tau_BiCop<-matrix(nrow=13, ncol=13)


for (p in 1:13){
  for (q in 1:13){
    iU1<-MCDF[,p]
    iU2<-MCDF[,q]
    EDACop<-0
    if (p!=q){
      EDACop<-BiCopSelect(iU1, iU2, familyset=NA, selectioncrit="AIC", indeptest=FALSE)
      MBiCop[p,q]<-EDACop[[2]]
      MParBiCop[p,q]<-EDACop[[3]]
      MPar2BiCop[p,q]<-EDACop[[4]]
      BiCop.TailDep<-BiCopPar2TailDep(family=MBiCop[p,q],par=MParBiCop[p,q],par2=MPar2BiCop[p,q])
      BiCop.Tau<-BiCopPar2Tau(family=MBiCop[p,q],par=MParBiCop[p,q],par2=MPar2BiCop[p,q])
      M.UpperTailDep_BiCop[p,q]<-BiCop.TailDep$upper
      M.LowerTailDep_BiCop[p,q]<-BiCop.TailDep$lower
      M.Tau_BiCop[p,q]<-BiCop.Tau[[1]]
    }
    else{
      EDACop<-0}
  }
}


# EDA - PDF(SURFACE PLOTS) AND CONTOUR PLOTS

Student.BiCop<-BiCop(family=MBiCop[9,6], par=MParBiCop[9,6], par2=MPar2BiCop[9,6])

windows()
plot(x=Student.BiCop, type="surface", margins="norm")

windows()
plot(x=Student.BiCop, type="contour", margins="norm")

# DENSITY PLOTs
# windows()
# h<-hist(STDr[,4])
# d<-density(STDr[,1])
# windows()
# plot(CDF_PFBCOLOM, main="CDF_PFBCOLOM",col="blue",ylab='Log Returns', xlab='Días',type="l",las=1)
# media1<-mean(CDF_PFBCOLOM)
# abline(h=c(media1))
# polygon(d, col="red", border="blue")

# # CDF PLOTs
windows()
par(mfrow=c(4,4))
plot(ecdf(STDr[,1]), main="CDF_STD_PFBCOLOM")
plot(ecdf(STDr[,2]), main="CDF_STD_ECOPETROL")
plot(ecdf(STDr[,3]), main="CDF_STD_ISA")
plot(ecdf(STDr[,4]), main="CDF_STD_ISAGEN")
plot(ecdf(STDr[,5]), main="CDF_STD_EXITO")
plot(ecdf(STDr[,6]), main="CDF_STD_BOGOTA")
plot(ecdf(STDr[,7]), main="CDF_STD_BCOLOMBIA")
plot(ecdf(STDr[,8]), main="CDF_STD_BVC")
plot(ecdf(STDr[,9]), main="CDF_STD_CORFICOLCF")
plot(ecdf(STDr[,10]), main="CDF_STD_GRUPOSURA")
plot(ecdf(STDr[,11]), main="CDF_STD_NUTRESA")
plot(ecdf(STDr[,12]), main="CDF_STD_GRUPOARGOS")
plot(ecdf(STDr[,13]), main="CDF_STD_CELSIA")
#=================================================================================================================
#=================================================================================================================
# MEASURING DEPENDENCE

# R-VINES

# R-VINE TREE SELECTION (SEQUENTIAL TREE BY TREE SELECTION)

# data(daxreturns)
# RVMDax<-RVineStructureSelect(data=daxreturns, type="RVine")

AERVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="RVine", indeptest=FALSE, rotations=FALSE, progress=FALSE)
RotAERVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="RVine", indeptest=FALSE, rotations=TRUE, progress=FALSE)
iAERVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="RVine", indeptest=TRUE, rotations=FALSE, progress=FALSE)
iRotAERVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="RVine", indeptest=TRUE, rotations=TRUE, progress=FALSE)

# R-VINE TREE SELECTION (MATRICES | STRUCTURES - FAMILIES - PARAMETERS)

# RVMDax_Str<-RVMDax$Matrix;RVMDax_Str
# RVMDax_Families<-RVMDax$family;RVMDax_Families
# RVMDax_Par<-RVMDax$par;RVMDax_Par
  
AERVM_Str<-AERVM$Matrix;AERVM_Str
RotAERVM_Str<-RotAERVM$Matrix;RotAERVM_Str
iAERVM_Str<-iAERVM$Matrix;iAERVM_Str
iRotAERVM_Str<-iRotAERVM$Matrix;iRotAERVM_Str

#=================================================================================================================
# R-VINE PAIR COPULA SELECTION

# RVMDax_PCC<-RVineCopSelect(data=daxreturns, Matrix=RVMDax_StMatrix)
# RVMDax_PCC_FMatrix<-RVMDax_PCC$family;RVMDax_PCC_FMatrix

AERVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=AERVM_Str, indeptest=FALSE, rotations=FALSE)

RotAERVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=RotAERVM_Str, indeptest=FALSE, rotations=TRUE)

iAERVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=iAERVM_Str, indeptest=TRUE, rotations=FALSE)

iRotAERVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=iRotAERVM_Str, indeptest=TRUE, rotations=TRUE)

# R-VINE PARAMETER ESTIMATION

# SEQUENTIAL ESTIMATION
AERVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=AERVMPCC, method="mle", se=TRUE);AERVMPCC_SQT
AERVMPCC_SQTStr<-AERVMPCC_SQT$RVM$Matrix;AERVMPCC_SQTStr
AERVMPCC_SQTFam<-AERVMPCC_SQT$RVM$family;AERVMPCC_SQTFam
AERVMPCC_SQTPar<-AERVMPCC_SQT$RVM$par;AERVMPCC_SQTPar
AERVMPCC_SQTPar2<-AERVMPCC_SQT$RVM$par2;AERVMPCC_SQTPar2

RotAERVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=RotAERVMPCC, method="mle", se=TRUE);RotAERVMPCC_SQT
RotAERVMPCC_SQTStr<-RotAERVMPCC_SQT$RVM$Matrix;RotAERVMPCC_SQTStr
RotAERVMPCC_SQTFam<-RotAERVMPCC_SQT$RVM$family;RotAERVMPCC_SQTFam
RotAERVMPCC_SQTPar<-RotAERVMPCC_SQT$RVM$par;RotAERVMPCC_SQTPar
RotAERVMPCC_SQTPar2<-RotAERVMPCC_SQT$RVM$par2;RotAERVMPCC_SQTPar2

iAERVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=iAERVMPCC, method="mle", se=TRUE);iAERVMPCC_SQT
iAERVMPCC_SQTStr<-iAERVMPCC_SQT$RVM$Matrix;iAERVMPCC_SQTStr
iAERVMPCC_SQTFam<-iAERVMPCC_SQT$RVM$family;iAERVMPCC_SQTFam
iAERVMPCC_SQTPar<-iAERVMPCC_SQT$RVM$par;iAERVMPCC_SQTPar
iAERVMPCC_SQTPar2<-iAERVMPCC_SQT$RVM$par2;iAERVMPCC_SQTPar2

iRotAERVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=iRotAERVMPCC, method="mle", se=TRUE);iRotAERVMPCC_SQT
iRotAERVMPCC_SQTStr<-iRotAERVMPCC_SQT$RVM$Matrix;iRotAERVMPCC_SQTStr
iRotAERVMPCC_SQTFam<-iRotAERVMPCC_SQT$RVM$family;iRotAERVMPCC_SQTFam
iRotAERVMPCC_SQTPar<-iRotAERVMPCC_SQT$RVM$par;iRotAERVMPCC_SQTPar
iRotAERVMPCC_SQTPar2<-iRotAERVMPCC_SQT$RVM$par2;iRotAERVMPCC_SQTPar2

# JOINT MAXIMUM LIKELIHOOD ESTIMATION
# AERVMPCC_JMLE<-RVineMLE(data=MCDF, RVM=AERVMPCC, start=AERVMPCC_SQTPar, start2=AERVMPCC_SQTPar2, grad=FALSE, se=FALSE)
# AERVMPCC_JMLEPar<-AERVMPCC_JMLE$RVM$par;AERVMPCC_JMLEPar
# AERVMPCC_JMLEPar2<-AERVMPCC_JMLE$RVM$par2;AERVMPCC_JMLEPar2
# AERVMPCC_JMLE_LLH<-AERVMPCC_JMLE$value;AERVMPCC_JMLE_LLH

#=================================================================================================================
# R-VINE COPULA MODELS PLOTS

# windows()
# RVineTreePlot(data=MCDF, RVM=AERVMPCC_SQT$RVM, tree=1, edge.labels="emptau", legend=FALSE)

windows()
RVineTreePlot(RVM=AERVMPCC_SQT$RVM, tree=1, edge.labels="theotau", legend=FALSE)

# TPM<-c(28,43,53,-12,-7,-6,-48,-52,-37,-21,5,37,12,12,34,57,-10,17,37,22,-11,-48,-30,-49,-33,-1)
# TPMatrix<-matrix(TPM,13,2)
# windows()
# RVineTreePlot(RVM=AERVMPCC_SQT$RVM, tree=1, edge.labels=FALSE, legend=FALSE, P=TPMatrix)

#=================================================================================================================
# KENDALL'S TAU MATRIX R-VINES

AERVMPCC_SQTKTau<-RVinePar2Tau(RVM=AERVMPCC_SQT$RVM)
RotAERVMPCC_SQTKTau<-RVinePar2Tau(RVM=RotAERVMPCC_SQT$RVM)
iAERVMPCC_SQTKTau<-RVinePar2Tau(RVM=iAERVMPCC_SQT$RVM)
iRotAERVMPCC_SQTKTau<-RVinePar2Tau(RVM=iRotAERVMPCC_SQT$RVM)

#=================================================================================================================
# R-VINE COPULA MODELS SELECTION (BEST FIT)

# LOG LIKELIHOOD
AERVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=AERVMPCC, par=AERVMPCC_SQTPar, par2=AERVMPCC_SQTPar2);AERVMPCC_SQTLogLH$loglik

RotAERVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=RotAERVMPCC, par=RotAERVMPCC_SQTPar, par2=RotAERVMPCC_SQTPar2);RotAERVMPCC_SQTLogLH$loglik

iAERVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=iAERVMPCC, par=iAERVMPCC_SQTPar, par2=iAERVMPCC_SQTPar2);iAERVMPCC_SQTLogLH$loglik

iRotAERVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=iRotAERVMPCC, par=iRotAERVMPCC_SQTPar, par2=iRotAERVMPCC_SQTPar2);iRotAERVMPCC_SQTLogLH$loglik

# AIC
AIC_AERVMPCC<-RVineAIC(data=MCDF, RVM=AERVMPCC);AIC_AERVMPCC
AIC_RotAERVMPCC<-RVineAIC(data=MCDF, RVM=RotAERVMPCC);AIC_RotAERVMPCC
AIC_iAERVMPCC<-RVineAIC(data=MCDF, RVM=iAERVMPCC);AIC_iAERVMPCC
AIC_iRotAERVMPCC<-RVineAIC(data=MCDF, RVM=iRotAERVMPCC);AIC_iRotAERVMPCC

# BIC
BIC_AERVMPCC<-RVineBIC(data=MCDF, RVM=AERVMPCC);BIC_AERVMPCC
BIC_RotAERVMPCC<-RVineBIC(data=MCDF, RVM=RotAERVMPCC);BIC_RotAERVMPCC
BIC_iAERVMPCC<-RVineBIC(data=MCDF, RVM=iAERVMPCC);BIC_iAERVMPCC
BIC_iRotAERVMPCC<-RVineBIC(data=MCDF, RVM=iRotAERVMPCC);BIC_iRotAERVMPCC

#=================================================================================================================
#=================================================================================================================
# C-VINES

# C-VINE TREE SELECTION (SEQUENTIAL TREE BY TREE SELECTION)

# AECVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="CVine", indeptest=FALSE, rotations=FALSE, progress=FALSE)
# RotAECVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="CVine", indeptest=FALSE, rotations=TRUE, progress=FALSE)
# iAECVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="CVine", indeptest=TRUE, rotations=FALSE, progress=FALSE)
# iRotAECVM<-RVineStructureSelect(data=MCDF, familyset=c(1:10), type="CVine", indeptest=TRUE, rotations=TRUE, progress=FALSE)

# C-VINE TREE SELECTION (MATRICES | STRUCTURES - FAMILIES - PARAMETERS)

# AECVM_Str<-AECVM$Matrix;AECVM_Str
# RotAECVM_Str<-RotAECVM$Matrix;RotAECVM_Str
# iAECVM_Str<-iAECVM$Matrix;iAECVM_Str
# iRotAECVM_Str<-iRotAECVM$Matrix;iRotAECVM_Str

CVM_str<-c(3,2,8,12,6,5,9,4,1,13,11,7,10,
             0,2,8,12,6,5,9,4,1,13,11,7,10,
             0,0,8,12,6,5,9,4,1,13,11,7,10,
             0,0,0,12,6,5,9,4,1,13,11,7,10,
             0,0,0,0,6,5,9,4,1,13,11,7,10,
             0,0,0,0,0,5,9,4,1,13,11,7,10,
             0,0,0,0,0,0,9,4,1,13,11,7,10,
             0,0,0,0,0,0,0,4,1,13,11,7,10,
             0,0,0,0,0,0,0,0,1,13,11,7,10,
             0,0,0,0,0,0,0,0,0,13,11,7,10,
             0,0,0,0,0,0,0,0,0,0,11,7,10,
             0,0,0,0,0,0,0,0,0,0,0,7,10,
             0,0,0,0,0,0,0,0,0,0,0,0,10)

CVM_Str<-matrix(CVM_str,13,13)

#=================================================================================================================
# C-VINE PAIR COPULA SELECTION

AECVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=CVM_Str, indeptest=FALSE, rotations=FALSE)

RotAECVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=CVM_Str, indeptest=FALSE, rotations=TRUE)

iAECVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=CVM_Str, indeptest=TRUE, rotations=FALSE)

iRotAECVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=CVM_Str, indeptest=TRUE, rotations=TRUE)

GaussianCVMPCC<-RVineCopSelect(data=MCDF, familyset=1, Matrix=CVM_Str, indeptest=FALSE, rotations=FALSE)

# C-VINE PARAMETER ESTIMATION

# SEQUENTIAL ESTIMATION
AECVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=AECVMPCC, method="mle", se=TRUE);AECVMPCC_SQT
AECVMPCC_SQTStr<-AECVMPCC_SQT$RVM$Matrix;AECVMPCC_SQTStr
AECVMPCC_SQTFam<-AECVMPCC_SQT$RVM$family;AECVMPCC_SQTFam
AECVMPCC_SQTPar<-AECVMPCC_SQT$RVM$par;AECVMPCC_SQTPar
AECVMPCC_SQTPar2<-AECVMPCC_SQT$RVM$par2;AECVMPCC_SQTPar2

RotAECVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=RotAECVMPCC, method="mle", se=TRUE);RotAECVMPCC_SQT
RotAECVMPCC_SQTStr<-RotAECVMPCC_SQT$RVM$Matrix;RotAECVMPCC_SQTStr
RotAECVMPCC_SQTFam<-RotAECVMPCC_SQT$RVM$family;RotAECVMPCC_SQTFam
RotAECVMPCC_SQTPar<-RotAECVMPCC_SQT$RVM$par;RotAECVMPCC_SQTPar
RotAECVMPCC_SQTPar2<-RotAECVMPCC_SQT$RVM$par2;RotAECVMPCC_SQTPar2

iAECVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=iAECVMPCC, method="mle", se=TRUE);iAECVMPCC_SQT
iAECVMPCC_SQTStr<-iAECVMPCC_SQT$RVM$Matrix;iAECVMPCC_SQTStr
iAECVMPCC_SQTFam<-iAECVMPCC_SQT$RVM$family;iAECVMPCC_SQTFam
iAECVMPCC_SQTPar<-iAECVMPCC_SQT$RVM$par;iAECVMPCC_SQTPar
iAECVMPCC_SQTPar2<-iAECVMPCC_SQT$RVM$par2;iAECVMPCC_SQTPar2

iRotAECVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=iRotAECVMPCC, method="mle", se=TRUE);iRotAECVMPCC_SQT
iRotAECVMPCC_SQTStr<-iRotAECVMPCC_SQT$RVM$Matrix;iRotAECVMPCC_SQTStr
iRotAECVMPCC_SQTFam<-iRotAECVMPCC_SQT$RVM$family;iRotAECVMPCC_SQTFam
iRotAECVMPCC_SQTPar<-iRotAECVMPCC_SQT$RVM$par;iRotAECVMPCC_SQTPar
iRotAECVMPCC_SQTPar2<-iRotAECVMPCC_SQT$RVM$par2;iRotAECVMPCC_SQTPar2

GaussianCVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=GaussianCVMPCC, method="mle", se=TRUE);GaussianCVMPCC_SQT
GaussianCVMPCC_SQTStr<-GaussianCVMPCC_SQT$RVM$Matrix;GaussianCVMPCC_SQTStr
GaussianCVMPCC_SQTFam<-GaussianCVMPCC_SQT$RVM$family;GaussianCVMPCC_SQTFam
GaussianCVMPCC_SQTPar<-GaussianCVMPCC_SQT$RVM$par;GaussianCVMPCC_SQTPar
GaussianCVMPCC_SQTPar2<-GaussianCVMPCC_SQT$RVM$par2;GaussianCVMPCC_SQTPar2

# JOINT MAXIMUM LIKELIHOOD ESTIMATION
# AECVMPCC_JMLE<-RVineMLE(data=MCDF, RVM=AECVMPCC, start=AECVMPCC_SQTPar, start2=AECVMPCC_SQTPar2, grad=FALSE, se=FALSE)
# AECVMPCC_JMLEPar<-AECVMPCC_JMLE$RVM$par;AECVMPCC_JMLEPar
# AECVMPCC_JMLEPar2<-AECVMPCC_JMLE$RVM$par2;AECVMPCC_JMLEPar2
# AECVMPCC_JMLE_LLH<-AECVMPCC_JMLE$value;AECVMPCC_JMLE_LLH

#=================================================================================================================
# C-VINE COPULA MODELS PLOTS

windows()
RVineTreePlot(RVM=AECVMPCC_SQT$RVM, tree=1, edge.labels="theotau", legend=FALSE)

#=================================================================================================================
# KENDALL'S TAU MATRIX C-VINES

AECVMPCC_SQTKTau<-RVinePar2Tau(RVM=AECVMPCC_SQT$RVM)
RotAECVMPCC_SQTKTau<-RVinePar2Tau(RVM=RotAECVMPCC_SQT$RVM)
iAECVMPCC_SQTKTau<-RVinePar2Tau(RVM=iAECVMPCC_SQT$RVM)
iRotAECVMPCC_SQTKTau<-RVinePar2Tau(RVM=iRotAECVMPCC_SQT$RVM)

GaussianCVMPCC_SQTKTau<-RVinePar2Tau(RVM=GaussianCVMPCC_SQT$RVM)

#=================================================================================================================
# C-VINE COPULA MODELS SELECTION (BEST FIT)

# LOG LIKELIHOOD
AECVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=AECVMPCC, par=AECVMPCC_SQTPar, par2=AECVMPCC_SQTPar2);AECVMPCC_SQTLogLH$loglik

RotAECVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=RotAECVMPCC, par=RotAECVMPCC_SQTPar, par2=RotAECVMPCC_SQTPar2);RotAECVMPCC_SQTLogLH$loglik

iAECVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=iAECVMPCC, par=iAECVMPCC_SQTPar, par2=iAECVMPCC_SQTPar2);iAECVMPCC_SQTLogLH$loglik

iRotAECVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=iRotAECVMPCC, par=iRotAECVMPCC_SQTPar, par2=iRotAECVMPCC_SQTPar2);iRotAECVMPCC_SQTLogLH$loglik

GaussianCVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=GaussianCVMPCC, par=GaussianCVMPCC_SQTPar, par2=GaussianCVMPCC_SQTPar2);GaussianCVMPCC_SQTLogLH$loglik


# AIC
AIC_AECVMPCC<-RVineAIC(data=MCDF, RVM=AECVMPCC);AIC_AECVMPCC
AIC_RotAECVMPCC<-RVineAIC(data=MCDF, RVM=RotAECVMPCC);AIC_RotAECVMPCC
AIC_iAECVMPCC<-RVineAIC(data=MCDF, RVM=iAECVMPCC);AIC_iAECVMPCC
AIC_iRotAECVMPCC<-RVineAIC(data=MCDF, RVM=iRotAECVMPCC);AIC_iRotAECVMPCC
AIC_GaussianCVMPCC<-RVineAIC(data=MCDF, RVM=GaussianCVMPCC);AIC_GaussianCVMPCC

# BIC
BIC_AECVMPCC<-RVineBIC(data=MCDF, RVM=AECVMPCC);BIC_AECVMPCC
BIC_RotAECVMPCC<-RVineBIC(data=MCDF, RVM=RotAECVMPCC);BIC_RotAECVMPCC
BIC_iAECVMPCC<-RVineBIC(data=MCDF, RVM=iAECVMPCC);BIC_iAECVMPCC
BIC_iRotAECVMPCC<-RVineBIC(data=MCDF, RVM=iRotAECVMPCC);BIC_iRotAECVMPCC
BIC_GaussianCVMPCC<-RVineBIC(data=MCDF, RVM=GaussianCVMPCC);BIC_GaussianCVMPCC

#=================================================================================================================
#=================================================================================================================
# D-VINES

# D-VINE TREE SELECTION (SEQUENTIAL TREE BY TREE SELECTION)
# data(daxreturns)
d<-dim(MCDF)[2]
M<- 1 - abs(TauCDF)
hamilton<-insert_dummy(TSP(M), label = "cut")
sol<-solve_TSP(hamilton, method = "repetitive_nn")
order<-cut_tour(sol, "cut")

# D-VINE TREE SELECTION (MATRICES | STRUCTURES - FAMILIES - PARAMETERS)
DVM<-D2RVine(order, family = rep(0,d*(d-1)/2), par = rep(0, d*(d-1)/2))
DVM_Str<-DVM$Matrix;DVM_Str

#=================================================================================================================
# D-VINE PAIR COPULA SELECTION

AEDVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=DVM_Str, indeptest=FALSE, rotations=FALSE)

RotAEDVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=DVM_Str, indeptest=FALSE, rotations=TRUE)

iAEDVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=DVM_Str, indeptest=TRUE, rotations=FALSE)

iRotAEDVMPCC<-RVineCopSelect(data=MCDF, familyset=c(1:10), Matrix=DVM_Str, indeptest=TRUE, rotations=TRUE)

# D-VINE PARAMETER ESTIMATION

# SEQUENTIAL ESTIMATION
AEDVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=AEDVMPCC, method="mle", se=TRUE);AEDVMPCC_SQT
AEDVMPCC_SQTStr<-AEDVMPCC_SQT$RVM$Matrix;AEDVMPCC_SQTStr
AEDVMPCC_SQTFam<-AEDVMPCC_SQT$RVM$family;AEDVMPCC_SQTFam
AEDVMPCC_SQTPar<-AEDVMPCC_SQT$RVM$par;AEDVMPCC_SQTPar
AEDVMPCC_SQTPar2<-AEDVMPCC_SQT$RVM$par2;AEDVMPCC_SQTPar2

RotAEDVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=RotAEDVMPCC, method="mle", se=TRUE);RotAEDVMPCC_SQT
RotAEDVMPCC_SQTStr<-RotAEDVMPCC_SQT$RVM$Matrix;RotAEDVMPCC_SQTStr
RotAEDVMPCC_SQTFam<-RotAEDVMPCC_SQT$RVM$family;RotAEDVMPCC_SQTFam
RotAEDVMPCC_SQTPar<-RotAEDVMPCC_SQT$RVM$par;RotAEDVMPCC_SQTPar
RotAEDVMPCC_SQTPar2<-RotAEDVMPCC_SQT$RVM$par2;RotAEDVMPCC_SQTPar2

iAEDVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=iAEDVMPCC, method="mle", se=TRUE);iAEDVMPCC_SQT
iAEDVMPCC_SQTStr<-iAEDVMPCC_SQT$RVM$Matrix;iAEDVMPCC_SQTStr
iAEDVMPCC_SQTFam<-iAEDVMPCC_SQT$RVM$family;iAEDVMPCC_SQTFam
iAEDVMPCC_SQTPar<-iAEDVMPCC_SQT$RVM$par;iAEDVMPCC_SQTPar
iAEDVMPCC_SQTPar2<-iAEDVMPCC_SQT$RVM$par2;iAEDVMPCC_SQTPar2

iRotAEDVMPCC_SQT<-RVineSeqEst(data=MCDF, RVM=iRotAEDVMPCC, method="mle", se=TRUE);iRotAEDVMPCC_SQT
iRotAEDVMPCC_SQTStr<-iRotAEDVMPCC_SQT$RVM$Matrix;iRotAEDVMPCC_SQTStr
iRotAEDVMPCC_SQTFam<-iRotAEDVMPCC_SQT$RVM$family;iRotAEDVMPCC_SQTFam
iRotAEDVMPCC_SQTPar<-iRotAEDVMPCC_SQT$RVM$par;iRotAEDVMPCC_SQTPar
iRotAEDVMPCC_SQTPar2<-iRotAEDVMPCC_SQT$RVM$par2;iRotAEDVMPCC_SQTPar2

# JOINT MAXIMUM LIKELIHOOD ESTIMATION
# AEDVMPCC_JMLE<-RVineMLE(data=MCDF, RVM=AEDVMPCC, start=AEDVMPCC_SQTPar, start2=AEDVMPCC_SQTPar2, grad=FALSE, se=FALSE)
# AEDVMPCC_JMLEPar<-AEDVMPCC_JMLE$RVM$par;AEDVMPCC_JMLEPar
# AEDVMPCC_JMLEPar2<-AEDVMPCC_JMLE$RVM$par2;AEDVMPCC_JMLEPar2
# AEDVMPCC_JMLE_LLH<-AEDVMPCC_JMLE$value;AEDVMPCC_JMLE_LLH

#=================================================================================================================
# D-VINE COPULA MODELS PLOTS

windows()
RVineTreePlot(RVM=AEDVMPCC_SQT$RVM, tree=1, edge.labels="theotau", legend=FALSE)

#=================================================================================================================
# KENDALL'S TAU MATRIX D-VINES

AEDVMPCC_SQTKTau<-RVinePar2Tau(RVM=AEDVMPCC_SQT$RVM)
RotAEDVMPCC_SQTKTau<-RVinePar2Tau(RVM=RotAEDVMPCC_SQT$RVM)
iAEDVMPCC_SQTKTau<-RVinePar2Tau(RVM=iAEDVMPCC_SQT$RVM)
iRotAEDVMPCC_SQTKTau<-RVinePar2Tau(RVM=iRotAEDVMPCC_SQT$RVM)

#=================================================================================================================
# D-VINE COPULA MODELS SELECTION (BEST FIT)

# LOG LIKELIHOOD
AEDVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=AEDVMPCC, par=AEDVMPCC_SQTPar, par2=AEDVMPCC_SQTPar2);AEDVMPCC_SQTLogLH$loglik

RotAEDVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=RotAEDVMPCC, par=RotAEDVMPCC_SQTPar, par2=RotAEDVMPCC_SQTPar2);RotAEDVMPCC_SQTLogLH$loglik

iAEDVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=iAEDVMPCC, par=iAEDVMPCC_SQTPar, par2=iAEDVMPCC_SQTPar2);iAEDVMPCC_SQTLogLH$loglik

iRotAEDVMPCC_SQTLogLH<-RVineLogLik(data=MCDF, RVM=iRotAEDVMPCC, par=iRotAEDVMPCC_SQTPar, par2=iRotAEDVMPCC_SQTPar2);iRotAEDVMPCC_SQTLogLH$loglik

# AIC
AIC_AEDVMPCC<-RVineAIC(data=MCDF, RVM=AEDVMPCC);AIC_AEDVMPCC
AIC_RotAEDVMPCC<-RVineAIC(data=MCDF, RVM=RotAEDVMPCC);AIC_RotAEDVMPCC
AIC_iAEDVMPCC<-RVineAIC(data=MCDF, RVM=iAEDVMPCC);AIC_iAEDVMPCC
AIC_iRotAEDVMPCC<-RVineAIC(data=MCDF, RVM=iRotAEDVMPCC);AIC_iRotAEDVMPCC

# BIC
BIC_AEDVMPCC<-RVineBIC(data=MCDF, RVM=AEDVMPCC);BIC_AEDVMPCC
BIC_RotAEDVMPCC<-RVineBIC(data=MCDF, RVM=RotAEDVMPCC);BIC_RotAEDVMPCC
BIC_iAEDVMPCC<-RVineBIC(data=MCDF, RVM=iAEDVMPCC);BIC_iAEDVMPCC
BIC_iRotAEDVMPCC<-RVineBIC(data=MCDF, RVM=iRotAEDVMPCC);BIC_iRotAEDVMPCC

#=================================================================================================================

# VUONG TEST & CLARKE TEST

# RowVines<-c(AERVMPCC_SQT$RVM, RotAERVMPCC_SQT$RVM, iAERVMPCC_SQT$RVM,iRotAERVMPCC_SQT$RVM,
#           AECVMPCC_SQT$RVM,RotAECVMPCC_SQT$RVM,iAECVMPCC_SQT$RVM,iRotAECVMPCC_SQT$RVM,
#           AEDVMPCC_SQT$RVM,RotAEDVMPCC_SQT$RVM,iAEDVMPCC_SQT$RVM,iRotAEDVMPCC_SQT$RVM)
# 
# ColVines<-RowVines

ModelsVector<-cbind(AERVMPCC_SQT,RotAERVMPCC_SQT,iAERVMPCC_SQT,iRotAERVMPCC_SQT,
                    AECVMPCC_SQT,RotAECVMPCC_SQT,iAECVMPCC_SQT,iRotAECVMPCC_SQT,
                    AEDVMPCC_SQT,RotAEDVMPCC_SQT,iAEDVMPCC_SQT,iRotAEDVMPCC_SQT)

# dMV<-dim(ModelsVector)

MV<-matrix(nrow=12, ncol=12)
MC<-matrix(nrow=12, ncol=12)

# MV0<-mat.or.vec(nr=12, nc=12)

for(i in 1:12){
  for (j in 1:12){
    iRVM<-ModelsVector[,i]
    jRVM<-ModelsVector[,j]
    VuongSchwarzSt<-0
    ClarkeSchwarzSt<-0
    VuongSchwarzSt<-RVineVuongTest(data=MCDF, RVM1=iRVM$RVM, RVM2=jRVM$RVM)$statistic.Schwarz
    ClarkeSchwarzSt<-RVineClarkeTest(data=MCDF, RVM1=iRVM$RVM, RVM2=jRVM$RVM)$statistic.Schwarz
    MV[i,j]<-VuongSchwarzSt
    MC[i,j]<-ClarkeSchwarzSt
  }
}

# VALIDACIÓN MANUAL

Vuong_XY<-RVineVuongTest(data=MCDF, RVM1=AERVMPCC_SQT$RVM, RVM2=iRotAEDVMPCC_SQT$RVM);Vuong_XY

Clarke_XY<-RVineClarkeTest(data=MCDF, RVM1=iRotAERVMPCC_SQT$RVM, RVM2=AERVMPCC_SQT$RVM);Clarke_XY