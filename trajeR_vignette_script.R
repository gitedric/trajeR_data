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
