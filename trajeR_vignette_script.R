library(trajeR)

##################################################
# CNORM
##################################################
load("yourPath/CNORM_data07")
load("yourPath/CNORM_data07Censored")
data <- as.matrix(data)
dataC <- as.matrix(dataC)
##################################################
solL <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    hessian = TRUE, ssigma = FALSE
)
solLs <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = TRUE, hessian = TRUE
)
solEM <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = FALSE, hessian = TRUE
)
solEMs <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = TRUE, hessian = TRUE
)
##################################################
# plot
plotrajeR(solL)
# colour's defintion
trans <- "70"
col1 <- "#034569"
col1.1 <- paste0("#64AAD0", trans)
col2 <- "#750062"
col2.1 <- paste0("#D962C7", trans)
col3 <- "#A68900"
col3.1 <- paste0("#FFE773", trans)
cols1 <- c(col1.1, col2.1, col3.1)
cols2 <- c(col1, col2, col3)
vcol <- c(cols1, cols2)
plotrajeR(solEM, Y = data[, 2:11], A = data[, 12:21], col = vcol)
##################################################
solLRisk <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 42:43],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = FALSE, hessian = TRUE
)
solLRisks <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 42:43],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = TRUE, hessian = TRUE
)
solEMRisk <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 42:43],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = FALSE, hessian = TRUE
)
solEMRisks <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 42:43],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = TRUE, hessian = TRUE
)
##################################################
paraminit <- c(solL$theta, solL$beta, solL$sigma, 0, 0, 0, 0, 0, 0)
solLTCOV2 <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 22:41],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = FALSE, hessian = TRUE,
    paraminit = paraminit
)
solLTCOV2s <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 22:41],
    degre = c(0, 3, 4), Model = "CNORM", Method = "L",
    ssigma = TRUE, hessian = TRUE,
    paraminit = paraminit
)
paraminit <- c(solEM$theta, solEM$beta, solEM$sigma, 0, 0, 0, 0, 0, 0)
solEMTCOV2 <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 22:41],
    degre = c(0, 3, 4), Model = "CNORM", Method = "EM",
    ssigma = FALSE, hessian = TRUE,
    paraminit = paraminit
)
solEMTCOV2s <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 22:41],
    degre = c(0, 3, 4), Model = "CNORM", Method = "EM",
    ssigma = TRUE, hessian = TRUE
)
##################################################
# plot
# colour's defintion
trans <- "70"
col1 <- "#034569"
col1.1 <- paste0("#64AAD0", trans)
col2 <- "#750062"
col2.1 <- paste0("#D962C7", trans)
col3 <- "#A68900"
col3.1 <- paste0("#FFE773", trans)
cols1 <- c(col1.1, col2.1, col3.1)
cols2 <- c(col1, col2, col3)
vcol <- c(cols1, cols2)
plotrajeR(solEM, Y = data[, 2:11], A = data[, 12:21], col = vcol)
plotcov <- c(c(rep(0, 10), 0.8, 0.4, 0.2, 0.5, 0.6, 0.7, 0.8, 0.8, 0.9, 1), c(rep(1, 10), 0., 0., 0., 0., 0., 0.2, 0.2, 0.5, 0.8, 0.8))
plotrajeR(solLTCOV2,
    Y = data[, 2:11], A = data[, 12:21],
    col = vcol, plotcov = plotcov
)
##################################################
solLC <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = FALSE, hessian = TRUE,
    ymin = 2, ymax = 23
)
solEMC <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = FALSE, hessian = TRUE,
    ymin = 2, ymax = 23
)
solLCs <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = TRUE, hessian = TRUE,
    ymin = 2, ymax = 23
)
solEMCs <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = TRUE, hessian = TRUE,
    ymin = 2, ymax = 23
)
##################################################
solLCTCOV <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21], TCOV = dataC[, 22:31],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = FALSE, hessian = TRUE,
    ymin = 2, ymax = 23
)
solEMCTCOV <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21], TCOV = dataC[, 22:31],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = FALSE, hessian = TRUE, itermax = 500,
    ymin = 2, ymax = 23
)
solLCTCOVs <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21], TCOV = dataC[, 22:31],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "L",
    ssigma = TRUE, hessian = TRUE,
    ymin = 2, ymax = 23
)
solEMCTCOVs <- trajeR(
    Y = dataC[, 2:11], A = dataC[, 12:21], TCOV = dataC[, 22:31],
    degre = c(0, 3, 4),
    Model = "CNORM", Method = "EM",
    ssigma = TRUE, hessian = TRUE,
    ymin = 2, ymax = 23
)

##################################################
# ZIP
##################################################
load("yourPath/ZIP_data01.Rdata")
data <- as.matrix(dataZIP)
##################################################
solL <- trajeR(
    Y = data[, 2:6], A = data[, 7:11],
    degre = c(2, 2), degre.nu = c(1, 1),
    Model = "ZIP", Method = "L", hessian = TRUE
)
solEM <- trajeR(
    Y = data[, 2:6], A = data[, 7:11],
    degre = c(2, 2), degre.nu = c(1, 1), itermax = 200,
    Model = "ZIP", Method = "EM", hessian = TRUE
)
solEMIRLS <- trajeR(
    Y = data[, 2:6], A = data[, 7:11],
    degre = c(2, 2), degre.nu = c(1, 1), itermax = 200,
    Model = "ZIP", Method = "EMIRLS", hessian = TRUE
)
##################################################
solLRisk <- trajeR(
    Y = data[, 2:6], A = data[, 7:11], Risk = data[, 12],
    degre = c(2, 2), degre.nu = c(1, 1),
    Model = "ZIP", Method = "L", hessian = TRUE
)
solEMRisk <- trajeR(
    Y = data[, 2:6], A = data[, 7:11], Risk = data[, 12],
    degre = c(2, 2), degre.nu = c(1, 1),
    Model = "ZIP", Method = "EM", hessian = TRUE
)
solEMIRLSRisk <- trajeR(
    Y = data[, 2:6], A = data[, 7:11], Risk = data[, 12],
    degre = c(2, 2), degre.nu = c(1, 1),
    Model = "ZIP", Method = "EMIRLS", hessian = TRUE
)
##################################################
solLTCOV <- trajeR(
    Y = data[, 2:6], A = data[, 7:11], TCOV = data[, 13:17],
    degre = c(2, 2), degre.nu = c(1, 1),
    Model = "ZIP", Method = "L", hessian = TRUE
)
solEMTCOV <- trajeR(
    Y = data[, 2:6], A = data[, 7:11], TCOV = data[, 13:17],
    degre = c(2, 2), degre.nu = c(1, 1), itermax = 500,
    Model = "ZIP", Method = "EM", hessian = TRUE
)
solEMIRLSTCOV <- trajeR(
    Y = data[, 2:6], A = data[, 7:11], TCOV = data[, 13:17],
    degre = c(2, 2), degre.nu = c(1, 1), itermax = 500,
    Model = "ZIP", Method = "EMIRLS", hessian = TRUE
)

##################################################
# LOGIT
##################################################
load(file = "yourPath/LOGIT_data01.Rdata")
data <- as.matrix(data)
##################################################
solL <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    ng = 3, degre = c(0, 3, 4),
    Model = "LOGIT", Method = "L", hessian = TRUE
)
solEM <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    ng = 3, degre = c(0, 3, 4),
    Model = "LOGIT", Method = "EM", hessian = TRUE, itermax = 300
)
solEMIRLS <- trajeR(
    Y = data[, 2:11], A = data[, 12:21],
    ng = 3, degre = c(0, 3, 4),
    Model = "LOGIT", Method = "EMIRLS", hessian = TRUE,
    itermax = 300
)
##################################################
solLRisk <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 22:23],
    ng = 3, degre = c(0, 3, 4),
    Model = "LOGIT", Method = "L", hessian = TRUE,
    itermax = 300
)
solEMRisk <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 22:23],
    degre = c(0, 3, 4),
    Model = "LOGIT", Method = "EM", hessian = TRUE,
    itermax = 500
)
solEMIRLSRisk <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], Risk = data[, 22:23],
    degre = c(0, 3, 4),
    Model = "LOGIT", Method = "EMIRLS", hessian = TRUE,
    itermax = 500
)
##################################################
solLTCOV <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 24:33],
    ng = 3, degre = c(0, 3, 4),
    Model = "LOGIT", Method = "L", hessian = TRUE,
    itermax = 300
)
paraminitEM <- c( # solution without time covariate
    exp(solL$theta) / sum(exp(solL$theta)), solL$beta,
    0, 0, 0 # initial values for TCOV
)
solEMIRLSTCOV <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 24:33],
    degre = c(0, 3, 4),
    Model = "LOGIT", Method = "EMIRLS", hessian = TRUE,
    itermax = 300, paraminit = paraminitEM
)
solEMTCOV <- trajeR(
    Y = data[, 2:11], A = data[, 12:21], TCOV = data[, 24:33],
    degre = c(0, 3, 4),
    Model = "LOGIT", Method = "EM", hessian = TRUE,
    itermax = 300, paraminit = paraminitEM
)
