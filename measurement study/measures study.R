# Emotion Regulation Checklist (ERC) - measurement study ----
ERC_DF <- as.data.frame(read.csv(file="\\measurement study\\attachment_emotionregulation\\ERC_DF.csv"))

## Inverting items - items 16 and 18 at all 3 time points ----
sapply(ERC_DF, range, na.rm = T)
index <-
  which(startsWith(names(ERC_DF), prefix = "Erc_edc_16") |
          startsWith(names(ERC_DF), prefix = "Erc_edc_18"))
inv <- 5 - ERC_DF[, index]
names(inv) <- paste0("i_", names(inv))
ERC_DF <- cbind(ERC_DF, inv)

## Creating items parcels (with random allocation) ---- 
vars <-
  c(
    "Erc_edc_1",
    "Erc_edc_7",
    "Erc_edc_15",
    "i_Erc_edc_16",
    "i_Erc_edc_18",
    "Erc_edc_21"
  )
set.seed(2020)
vars_parcels <- sample(vars, length(vars), replace = FALSE)

# Assigning items to parcels
ER_P1.1 <- paste0(vars_parcels[1:2], ".1.2")
ER_P2.1 <- paste0(vars_parcels[3:4], ".1.2")
ER_P3.1 <- paste0(vars_parcels[5:6], ".1.2")
ER_P1.2 <- paste0(vars_parcels[1:2], ".2.2")
ER_P2.2 <- paste0(vars_parcels[3:4], ".2.2")
ER_P3.2 <- paste0(vars_parcels[5:6], ".2.2")
ER_P1.3 <- paste0(vars_parcels[1:2], ".3.2")
ER_P2.3 <- paste0(vars_parcels[3:4], ".3.2")
ER_P3.3 <- paste0(vars_parcels[5:6], ".3.2")
ER_P1.4 <- paste0(vars_parcels[1:2], ".4.2")
ER_P2.4 <- paste0(vars_parcels[3:4], ".4.2")
ER_P3.4 <- paste0(vars_parcels[5:6], ".4.2")

ERC_DF <- cbind(
  ERC_DF,
  data.frame(
    ER_P1.1 = create.parcel(as.data.frame(ERC_DF), ER_P1.1),
    ER_P2.1 = create.parcel(as.data.frame(ERC_DF), ER_P2.1),
    ER_P3.1 = create.parcel(as.data.frame(ERC_DF), ER_P3.1),
    ER_P1.2 = create.parcel(as.data.frame(ERC_DF), ER_P1.2),
    ER_P2.2 = create.parcel(as.data.frame(ERC_DF), ER_P2.2),
    ER_P3.2 = create.parcel(as.data.frame(ERC_DF), ER_P3.2),
    ER_P1.3 = create.parcel(as.data.frame(ERC_DF), ER_P1.3),
    ER_P2.3 = create.parcel(as.data.frame(ERC_DF), ER_P2.3),
    ER_P3.3 = create.parcel(as.data.frame(ERC_DF), ER_P3.3),
    ER_P1.4 = create.parcel(as.data.frame(ERC_DF), ER_P1.4),
    ER_P2.4 = create.parcel(as.data.frame(ERC_DF), ER_P2.4),
    ER_P3.4 = create.parcel(as.data.frame(ERC_DF), ER_P3.4)
  )
)

## Alpha coefficients ----
psych::alpha(ERC_DF[, c(
  "Erc_edc_1.1.2",
  "Erc_edc_7.1.2",
  "Erc_edc_15.1.2",
  "i_Erc_edc_16.1.2",
  "i_Erc_edc_18.1.2",
  "Erc_edc_21.1.2"
)])
psych::alpha(ERC_DF[, c(
  "Erc_edc_1.2.2",
  "Erc_edc_7.2.2",
  "Erc_edc_15.2.2",
  "i_Erc_edc_16.2.2",
  "i_Erc_edc_18.2.2",
  "Erc_edc_21.2.2"
)])
psych::alpha(ERC_DF[, c(
  "Erc_edc_1.3.2",
  "Erc_edc_7.3.2",
  "Erc_edc_15.3.2",
  "i_Erc_edc_16.3.2",
  "i_Erc_edc_18.3.2",
  "Erc_edc_21.3.2"
)])

## CFA & MI for Emotion Regulation - T1-T3 ----
model_ER_conf <-
  "
ER_T1 =~ ER_P1.1 + ER_P2.1 + ER_P3.1
ER_T2 =~ ER_P1.2 + ER_P2.2 + ER_P3.2
ER_T3 =~ ER_P1.3 + ER_P2.3 + ER_P3.3

ER_P1.1 ~~ ER_P1.2 + ER_P1.3 
ER_P1.2 ~~ ER_P1.3 
ER_P2.1 ~~ ER_P2.2 + ER_P2.3
ER_P2.2 ~~ ER_P2.3
ER_P3.1 ~~ ER_P3.2 + ER_P3.3
ER_P3.2 ~~ ER_P3.3

# Latent var.
ER_T1 ~~ 1*ER_T1
ER_T2 ~~ 1*ER_T2
ER_T3 ~~ 1*ER_T3

#intercepts & latent means
ER_P1.1 ~ 1
ER_P2.1 ~ 1
ER_P3.1 ~ 1
ER_P1.2 ~ 1
ER_P2.2 ~ 1
ER_P3.2 ~ 1
ER_P1.3 ~ 1
ER_P2.3 ~ 1
ER_P3.3 ~ 1

ER_T1 ~ 0*1
ER_T2 ~ 0*1
ER_T3 ~ 0*1
"
fit_ER_conf <- cfa(model_ER_conf, data = ERC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ER_conf, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ER_conf, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

model_ER_metric <-
  "
ER_T1 =~ l1*ER_P1.1 + l2*ER_P2.1 + l3*ER_P3.1
ER_T2 =~ l1*ER_P1.2 + l2*ER_P2.2 + l3*ER_P3.2
ER_T3 =~ l1*ER_P1.3 + l2*ER_P2.3 + l3*ER_P3.3

ER_P1.1 ~~ ER_P1.2 + ER_P1.3 
ER_P1.2 ~~ ER_P1.3 
ER_P2.1 ~~ ER_P2.2 + ER_P2.3
ER_P2.2 ~~ ER_P2.3
ER_P3.1 ~~ ER_P3.2 + ER_P3.3
ER_P3.2 ~~ ER_P3.3

# Latent var.
ER_T1 ~~ 1*ER_T1
ER_T2 ~~ NA*ER_T2
ER_T3 ~~ NA*ER_T3

#intercepts & latent means
ER_P1.1 ~ 1
ER_P2.1 ~ 1
ER_P3.1 ~ 1
ER_P1.2 ~ 1
ER_P2.2 ~ 1
ER_P3.2 ~ 1
ER_P1.3 ~ 1
ER_P2.3 ~ 1
ER_P3.3 ~ 1

ER_T1 ~ 0*1
ER_T2 ~ 0*1
ER_T3 ~ 0*1
"
fit_ER_metric <- cfa(model_ER_metric, data = ERC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ER_metric, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ER_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr_mplus"))

model_ER_strong <-
  "
ER_T1 =~ l1*ER_P1.1 + l2*ER_P2.1 + l3*ER_P3.1
ER_T2 =~ l1*ER_P1.2 + l2*ER_P2.2 + l3*ER_P3.2
ER_T3 =~ l1*ER_P1.3 + l2*ER_P2.3 + l3*ER_P3.3

ER_P1.1 ~~ ER_P1.2 + ER_P1.3 
ER_P1.2 ~~ ER_P1.3 
ER_P2.1 ~~ ER_P2.2 + ER_P2.3
ER_P2.2 ~~ ER_P2.3
ER_P3.1 ~~ ER_P3.2 + ER_P3.3
ER_P3.2 ~~ ER_P3.3

# Latent var.
ER_T1 ~~ 1*ER_T1
ER_T2 ~~ NA*ER_T2
ER_T3 ~~ NA*ER_T3

#intercepts & latent means
ER_P1.1 ~ i1*1
ER_P2.1 ~ i2*1
ER_P3.1 ~ i3*1
ER_P1.2 ~ i1*1
ER_P2.2 ~ i2*1
ER_P3.2 ~ i3*1
ER_P1.3 ~ i1*1
ER_P2.3 ~ i2*1
ER_P3.3 ~ i3*1

ER_T1 ~ 0*1
ER_T2 ~ NA*1
ER_T3 ~ NA*1
"
fit_ER_strong <- cfa(model_ER_strong, data = ERC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ER_strong, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ER_strong, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr_mplus"))

summary(compareFit(fit_ER_conf, fit_ER_metric, fit_ER_strong))

# Experiences in Close Relationship Scale (ECRS) - measurement study ----
SEC_DF <- as.data.frame(read.csv(file="\\measurement study\\attachment_emotionregulation\\SEC_DF.csv"))

## Inverting items - items 7, 9, & 12 at all 3 time points ----
index <- c(which(startsWith(names(SEC_DF), prefix="EREP_7")),
           which(startsWith(names(SEC_DF), prefix="EREP_9")),
           which(startsWith(names(SEC_DF), prefix="EREP_12")))
inv <- 8-SEC_DF[,index]
names(inv)
names(inv) <- paste0("i_", names(inv))
SEC_DF <- cbind(SEC_DF, inv)
names(SEC_DF)

## Creating items parcels (with random allocation) - Attachment-related Anxiety ---- 
vars <-
  c(
    "EREP_1", 
    "EREP_4",
    "EREP_6",
    "EREP_8",
    "EREP_10"
  )
set.seed(2020)
vars_parcels <- sample(vars, length(vars), replace = FALSE)

#Assigning items to parcels for father and mother
ANX_P1.1.1 <- paste0(vars_parcels[1], ".1.1")
ANX_P2.1.1 <- paste0(vars_parcels[2:3], ".1.1")
ANX_P3.1.1 <- paste0(vars_parcels[4:5], ".1.1")
ANX_P1.2.1 <- paste0(vars_parcels[1], ".2.1")
ANX_P2.2.1 <- paste0(vars_parcels[2:3], ".2.1")
ANX_P3.2.1 <- paste0(vars_parcels[4:5], ".2.1")
ANX_P1.3.1 <- paste0(vars_parcels[1], ".3.1")
ANX_P2.3.1 <- paste0(vars_parcels[2:3], ".3.1")
ANX_P3.3.1 <- paste0(vars_parcels[4:5], ".3.1")
ANX_P1.1.2 <- paste0(vars_parcels[1], ".1.2")
ANX_P2.1.2 <- paste0(vars_parcels[2:3], ".1.2")
ANX_P3.1.2 <- paste0(vars_parcels[4:5], ".1.2")
ANX_P1.2.2 <- paste0(vars_parcels[1], ".2.2")
ANX_P2.2.2 <- paste0(vars_parcels[2:3], ".2.2")
ANX_P3.2.2 <- paste0(vars_parcels[4:5], ".2.2")
ANX_P1.3.2 <- paste0(vars_parcels[1], ".3.2")
ANX_P2.3.2 <- paste0(vars_parcels[2:3], ".3.2")
ANX_P3.3.2 <- paste0(vars_parcels[4:5], ".3.2")

SEC_DF <- cbind(
  SEC_DF,
  data.frame(
    ANX_P1.1.1 = SEC_DF[, ANX_P1.1.1],
    ANX_P2.1.1 = create.parcel(as.data.frame(SEC_DF), ANX_P2.1.1),
    ANX_P3.1.1 = create.parcel(as.data.frame(SEC_DF), ANX_P3.1.1),
    ANX_P1.1.2 = SEC_DF[, ANX_P1.1.2],
    ANX_P2.1.2 = create.parcel(as.data.frame(SEC_DF), ANX_P2.1.2),
    ANX_P3.1.2 = create.parcel(as.data.frame(SEC_DF), ANX_P3.1.2),
    ANX_P1.2.1 = SEC_DF[, ANX_P1.2.1],
    ANX_P2.2.1 = create.parcel(as.data.frame(SEC_DF), ANX_P2.2.1),
    ANX_P3.2.1 = create.parcel(as.data.frame(SEC_DF), ANX_P3.2.1),
    ANX_P1.2.2 = SEC_DF[, ANX_P1.2.2],
    ANX_P2.2.2 = create.parcel(as.data.frame(SEC_DF), ANX_P2.2.2),
    ANX_P3.2.2 = create.parcel(as.data.frame(SEC_DF), ANX_P3.2.2),
    ANX_P1.3.1 = SEC_DF[, ANX_P1.3.1],
    ANX_P2.3.1 = create.parcel(as.data.frame(SEC_DF), ANX_P2.3.1),
    ANX_P3.3.1 = create.parcel(as.data.frame(SEC_DF), ANX_P3.3.1),
    ANX_P1.3.2 = SEC_DF[, ANX_P1.3.2],
    ANX_P2.3.2 = create.parcel(as.data.frame(SEC_DF), ANX_P2.3.2),
    ANX_P3.3.2 = create.parcel(as.data.frame(SEC_DF), ANX_P3.3.2)
  )
)

## Creating items parcels (with random allocation) - Attachment-related Avoidance ---- 
vars <-
  c(
    "EREP_2", 
    "EREP_3",
    "EREP_5",
    "i_EREP_9",
    "i_EREP_12"
  )
set.seed(2020)
vars_parcels <- sample(vars, length(vars), replace = FALSE)

#Assigning items to parcels for father and mother
AVOI_P1.1.1 <- paste0(vars_parcels[1], ".1.1")
AVOI_P2.1.1 <- paste0(vars_parcels[2:3], ".1.1")
AVOI_P3.1.1 <- paste0(vars_parcels[4:5], ".1.1")
AVOI_P1.2.1 <- paste0(vars_parcels[1], ".2.1")
AVOI_P2.2.1 <- paste0(vars_parcels[2:3], ".2.1")
AVOI_P3.2.1 <- paste0(vars_parcels[4:5], ".2.1")
AVOI_P1.3.1 <- paste0(vars_parcels[1], ".3.1")
AVOI_P2.3.1 <- paste0(vars_parcels[2:3], ".3.1")
AVOI_P3.3.1 <- paste0(vars_parcels[4:5], ".3.1")
AVOI_P1.1.2 <- paste0(vars_parcels[1], ".1.2")
AVOI_P2.1.2 <- paste0(vars_parcels[2:3], ".1.2")
AVOI_P3.1.2 <- paste0(vars_parcels[4:5], ".1.2")
AVOI_P1.2.2 <- paste0(vars_parcels[1], ".2.2")
AVOI_P2.2.2 <- paste0(vars_parcels[2:3], ".2.2")
AVOI_P3.2.2 <- paste0(vars_parcels[4:5], ".2.2")
AVOI_P1.3.2 <- paste0(vars_parcels[1], ".3.2")
AVOI_P2.3.2 <- paste0(vars_parcels[2:3], ".3.2")
AVOI_P3.3.2 <- paste0(vars_parcels[4:5], ".3.2")

SEC_DF <- cbind(
  SEC_DF,
  data.frame(
    AVOI_P1.1.1 = SEC_DF[, AVOI_P1.1.1],
    AVOI_P2.1.1 = create.parcel(as.data.frame(SEC_DF), AVOI_P2.1.1),
    AVOI_P3.1.1 = create.parcel(as.data.frame(SEC_DF), AVOI_P3.1.1),
    AVOI_P1.1.2 = SEC_DF[, AVOI_P1.1.2],
    AVOI_P2.1.2 = create.parcel(as.data.frame(SEC_DF), AVOI_P2.1.2),
    AVOI_P3.1.2 = create.parcel(as.data.frame(SEC_DF), AVOI_P3.1.2),
    AVOI_P1.2.1 = SEC_DF[, AVOI_P1.2.1],
    AVOI_P2.2.1 = create.parcel(as.data.frame(SEC_DF), AVOI_P2.2.1),
    AVOI_P3.2.1 = create.parcel(as.data.frame(SEC_DF), AVOI_P3.2.1),
    AVOI_P1.2.2 = SEC_DF[, AVOI_P1.2.2],
    AVOI_P2.2.2 = create.parcel(as.data.frame(SEC_DF), AVOI_P2.2.2),
    AVOI_P3.2.2 = create.parcel(as.data.frame(SEC_DF), AVOI_P3.2.2),
    AVOI_P1.3.1 = SEC_DF[, AVOI_P1.3.1],
    AVOI_P2.3.1 = create.parcel(as.data.frame(SEC_DF), AVOI_P2.3.1),
    AVOI_P3.3.1 = create.parcel(as.data.frame(SEC_DF), AVOI_P3.3.1),
    AVOI_P1.3.2 = SEC_DF[, AVOI_P1.3.2],
    AVOI_P2.3.2 = create.parcel(as.data.frame(SEC_DF), AVOI_P2.3.2),
    AVOI_P3.3.2 = create.parcel(as.data.frame(SEC_DF), AVOI_P3.3.2)
  )
)

## Alpha coefficients ----
# T1 - Anxiety father
psych::alpha(SEC_DF[, c("EREP_1.1.1",
                        "EREP_4.1.1",
                        "EREP_6.1.1",
                        "EREP_8.1.1",
                        "EREP_10.1.1")])
# T1 - Anxiety mother
psych::alpha(SEC_DF[, c("EREP_1.1.2",
                        "EREP_4.1.2",
                        "EREP_6.1.2",
                        "EREP_8.1.2",
                        "EREP_10.1.2")])
# T2 - Anxiety father
psych::alpha(SEC_DF[, c("EREP_1.2.1",
                        "EREP_4.2.1",
                        "EREP_6.2.1",
                        "EREP_8.2.1",
                        "EREP_10.2.1")])
# T2 - Anxiety mother
psych::alpha(SEC_DF[, c("EREP_1.2.2",
                        "EREP_4.2.2",
                        "EREP_6.2.2",
                        "EREP_8.2.2",
                        "EREP_10.2.2")])
# T3 - Anxiety father
psych::alpha(SEC_DF[, c("EREP_1.3.1",
                        "EREP_4.3.1",
                        "EREP_6.3.1",
                        "EREP_8.3.1",
                        "EREP_10.3.1")])
# T3 - Anxiety mother
psych::alpha(SEC_DF[, c("EREP_1.3.2",
                        "EREP_4.3.2",
                        "EREP_6.3.2",
                        "EREP_8.3.2",
                        "EREP_10.3.2")]) 


# T1 - Avoidance father
psych::alpha(SEC_DF[, c("EREP_2.1.1",
                        "EREP_3.1.1",
                        "EREP_5.1.1",
                        "i_EREP_9.1.1",
                        "i_EREP_12.1.1")])
# T1 - Avoidance mother
psych::alpha(SEC_DF[, c("EREP_2.1.2",
                        "EREP_3.1.2",
                        "EREP_5.1.2",
                        "i_EREP_9.1.2",
                        "i_EREP_12.1.2")])
# T2 - Avoidance father
psych::alpha(SEC_DF[, c("EREP_2.2.1",
                        "EREP_3.2.1",
                        "EREP_5.2.1",
                        "i_EREP_9.2.1",
                        "i_EREP_12.2.1")])
# T2 - Avoidance mother
psych::alpha(SEC_DF[, c("EREP_2.2.2",
                        "EREP_3.2.2",
                        "EREP_5.2.2",
                        "i_EREP_9.2.2",
                        "i_EREP_12.2.2")])
# T3 - Avoidance father
psych::alpha(SEC_DF[, c("EREP_2.3.1",
                        "EREP_3.3.1",
                        "EREP_5.3.1",
                        "i_EREP_9.3.1",
                        "i_EREP_12.3.1")])
# T3 - Avoidance mother
psych::alpha(SEC_DF[, c("EREP_2.3.2",
                        "EREP_3.3.2",
                        "EREP_5.3.2",
                        "i_EREP_9.3.2",
                        "i_EREP_12.3.2")]) 

## CFA & MI for Attachment-related Anxiety (T1-T3; Fathers & Mother) ----
model_ANX_conf <-
  "
ANX_T1_M =~ LM11*ANX_P1.1.2 + LM12*ANX_P2.1.2 + LM13*ANX_P3.1.2
ANX_T2_M =~ LM21*ANX_P1.2.2 + LM22*ANX_P2.2.2 + LM23*ANX_P3.2.2
ANX_T3_M =~ LM31*ANX_P1.3.2 + LM32*ANX_P2.3.2 + LM33*ANX_P3.3.2

ANX_T1_F =~ LF11*ANX_P1.1.1 + LF12*ANX_P2.1.1 + LF13*ANX_P3.1.1
ANX_T2_F =~ LF21*ANX_P1.2.1 + LF22*ANX_P2.2.1 + LF23*ANX_P3.2.1
ANX_T3_F =~ LF31*ANX_P1.3.1 + LF32*ANX_P2.3.1 + LF33*ANX_P3.3.1

# Longitudinal correlated residuals
ANX_P1.1.2 ~~ ANX_P1.2.2 + ANX_P1.3.2 
ANX_P1.2.2 ~~ ANX_P1.3.2
ANX_P2.1.2 ~~ ANX_P2.2.2 + ANX_P2.3.2
ANX_P2.2.2 ~~ ANX_P2.3.2
ANX_P3.1.2 ~~ ANX_P3.2.2 + ANX_P3.3.2
ANX_P3.2.2 ~~ ANX_P3.3.2

ANX_P1.1.1 ~~ ANX_P1.2.1 + ANX_P1.3.1 
ANX_P1.2.1 ~~ ANX_P1.3.1
ANX_P2.1.1 ~~ ANX_P2.2.1 + ANX_P2.3.1
ANX_P2.2.1 ~~ ANX_P2.3.1
ANX_P3.1.1 ~~ ANX_P3.2.1 + ANX_P3.3.1
ANX_P3.2.1 ~~ ANX_P3.3.1

#Diadic correlated residuals
ANX_P1.1.1 ~~ ANX_P1.1.2
ANX_P2.1.1 ~~ ANX_P2.1.2
ANX_P3.1.1 ~~ ANX_P3.1.2

ANX_P1.2.1 ~~ ANX_P1.2.2
ANX_P2.2.1 ~~ ANX_P2.2.2
ANX_P3.2.1 ~~ ANX_P3.2.2

ANX_P1.3.1 ~~ ANX_P1.3.2
ANX_P2.3.1 ~~ ANX_P2.3.2
ANX_P3.3.1 ~~ ANX_P3.3.2

# Latent var.
ANX_T1_M ~~ 1*ANX_T1_M
ANX_T2_M ~~ 1*ANX_T2_M
ANX_T3_M ~~ 1*ANX_T3_M

ANX_T1_F ~~ 1*ANX_T1_F
ANX_T2_F ~~ 1*ANX_T2_F
ANX_T3_F ~~ 1*ANX_T3_F

#intercepts & latent means
ANX_P1.1.2 ~ nuM11*1
ANX_P2.1.2 ~ nuM12*1
ANX_P3.1.2 ~ nuM13*1
ANX_P1.2.2 ~ nuM21*1
ANX_P2.2.2 ~ nuM22*1
ANX_P3.2.2 ~ nuM23*1
ANX_P1.3.2 ~ nuM31*1
ANX_P2.3.2 ~ nuM32*1
ANX_P3.3.2 ~ nuM33*1

ANX_P1.1.1 ~ nuF11*1
ANX_P2.1.1 ~ nuF12*1
ANX_P3.1.1 ~ nuF13*1
ANX_P1.2.1 ~ nuF21*1
ANX_P2.2.1 ~ nuF22*1
ANX_P3.2.1 ~ nuF23*1
ANX_P1.3.1 ~ nuF31*1
ANX_P2.3.1 ~ nuF32*1
ANX_P3.3.1 ~ nuF33*1

#Factor Means
ANX_T1_M ~ 0*1
ANX_T2_M ~ 0*1
ANX_T3_M ~ 0*1

ANX_T1_F ~ 0*1
ANX_T2_F ~ 0*1
ANX_T3_F ~ 0*1
"
fit_ANX_conf <- cfa(model_ANX_conf, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ANX_conf, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ANX_conf, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

model_ANX_metric <-
  "
ANX_T1_M =~ L11*ANX_P1.1.2 + L12*ANX_P2.1.2 + L13*ANX_P3.1.2
ANX_T2_M =~ L11*ANX_P1.2.2 + L12*ANX_P2.2.2 + L13*ANX_P3.2.2
ANX_T3_M =~ L11*ANX_P1.3.2 + L12*ANX_P2.3.2 + L13*ANX_P3.3.2

ANX_T1_F =~ L11*ANX_P1.1.1 + L12*ANX_P2.1.1 + L13*ANX_P3.1.1
ANX_T2_F =~ L11*ANX_P1.2.1 + L12*ANX_P2.2.1 + L13*ANX_P3.2.1
ANX_T3_F =~ L11*ANX_P1.3.1 + L12*ANX_P2.3.1 + L13*ANX_P3.3.1

# Longitudinal correlated residuals
ANX_P1.1.2 ~~ ANX_P1.2.2 + ANX_P1.3.2 
ANX_P1.2.2 ~~ ANX_P1.3.2
ANX_P2.1.2 ~~ ANX_P2.2.2 + ANX_P2.3.2
ANX_P2.2.2 ~~ ANX_P2.3.2
ANX_P3.1.2 ~~ ANX_P3.2.2 + ANX_P3.3.2
ANX_P3.2.2 ~~ ANX_P3.3.2

ANX_P1.1.1 ~~ ANX_P1.2.1 + ANX_P1.3.1 
ANX_P1.2.1 ~~ ANX_P1.3.1
ANX_P2.1.1 ~~ ANX_P2.2.1 + ANX_P2.3.1
ANX_P2.2.1 ~~ ANX_P2.3.1
ANX_P3.1.1 ~~ ANX_P3.2.1 + ANX_P3.3.1
ANX_P3.2.1 ~~ ANX_P3.3.1

#Diadic correlated residuals
ANX_P1.1.1 ~~ ANX_P1.1.2
ANX_P2.1.1 ~~ ANX_P2.1.2
ANX_P3.1.1 ~~ ANX_P3.1.2

ANX_P1.2.1 ~~ ANX_P1.2.2
ANX_P2.2.1 ~~ ANX_P2.2.2
ANX_P3.2.1 ~~ ANX_P3.2.2

ANX_P1.3.1 ~~ ANX_P1.3.2
ANX_P2.3.1 ~~ ANX_P2.3.2
ANX_P3.3.1 ~~ ANX_P3.3.2

# Latent var.
ANX_T1_M ~~ 1*ANX_T1_M
ANX_T2_M ~~ NA*ANX_T2_M
ANX_T3_M ~~ NA*ANX_T3_M

ANX_T1_F ~~ NA*ANX_T1_F
ANX_T2_F ~~ NA*ANX_T2_F
ANX_T3_F ~~ NA*ANX_T3_F

#intercepts & latent means
ANX_P1.1.2 ~ nuM11*1
ANX_P2.1.2 ~ nuM12*1
ANX_P3.1.2 ~ nuM13*1
ANX_P1.2.2 ~ nuM21*1
ANX_P2.2.2 ~ nuM22*1
ANX_P3.2.2 ~ nuM23*1
ANX_P1.3.2 ~ nuM31*1
ANX_P2.3.2 ~ nuM32*1
ANX_P3.3.2 ~ nuM33*1

ANX_P1.1.1 ~ nuF11*1
ANX_P2.1.1 ~ nuF12*1
ANX_P3.1.1 ~ nuF13*1
ANX_P1.2.1 ~ nuF21*1
ANX_P2.2.1 ~ nuF22*1
ANX_P3.2.1 ~ nuF23*1
ANX_P1.3.1 ~ nuF31*1
ANX_P2.3.1 ~ nuF32*1
ANX_P3.3.1 ~ nuF33*1

#Factor Means
ANX_T1_M ~ 0*1
ANX_T2_M ~ 0*1
ANX_T3_M ~ 0*1

ANX_T1_F ~ 0*1
ANX_T2_F ~ 0*1
ANX_T3_F ~ 0*1
"
fit_ANX_metric <- cfa(model_ANX_metric, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ANX_metric, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ANX_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

model_ANX_strong <-
  "
ANX_T1_M =~ L11*ANX_P1.1.2 + L12*ANX_P2.1.2 + L13*ANX_P3.1.2
ANX_T2_M =~ L11*ANX_P1.2.2 + L12*ANX_P2.2.2 + L13*ANX_P3.2.2
ANX_T3_M =~ L11*ANX_P1.3.2 + L12*ANX_P2.3.2 + L13*ANX_P3.3.2

ANX_T1_F =~ L11*ANX_P1.1.1 + L12*ANX_P2.1.1 + L13*ANX_P3.1.1
ANX_T2_F =~ L11*ANX_P1.2.1 + L12*ANX_P2.2.1 + L13*ANX_P3.2.1
ANX_T3_F =~ L11*ANX_P1.3.1 + L12*ANX_P2.3.1 + L13*ANX_P3.3.1

# Longitudinal correlated residuals
ANX_P1.1.2 ~~ ANX_P1.2.2 + ANX_P1.3.2 
ANX_P1.2.2 ~~ ANX_P1.3.2
ANX_P2.1.2 ~~ ANX_P2.2.2 + ANX_P2.3.2
ANX_P2.2.2 ~~ ANX_P2.3.2
ANX_P3.1.2 ~~ ANX_P3.2.2 + ANX_P3.3.2
ANX_P3.2.2 ~~ ANX_P3.3.2

ANX_P1.1.1 ~~ ANX_P1.2.1 + ANX_P1.3.1 
ANX_P1.2.1 ~~ ANX_P1.3.1
ANX_P2.1.1 ~~ ANX_P2.2.1 + ANX_P2.3.1
ANX_P2.2.1 ~~ ANX_P2.3.1
ANX_P3.1.1 ~~ ANX_P3.2.1 + ANX_P3.3.1
ANX_P3.2.1 ~~ ANX_P3.3.1

#Diadic correlated residuals
ANX_P1.1.1 ~~ ANX_P1.1.2
ANX_P2.1.1 ~~ ANX_P2.1.2
ANX_P3.1.1 ~~ ANX_P3.1.2

ANX_P1.2.1 ~~ ANX_P1.2.2
ANX_P2.2.1 ~~ ANX_P2.2.2
ANX_P3.2.1 ~~ ANX_P3.2.2

ANX_P1.3.1 ~~ ANX_P1.3.2
ANX_P2.3.1 ~~ ANX_P2.3.2
ANX_P3.3.1 ~~ ANX_P3.3.2

# Latent var.
ANX_T1_M ~~ 1*ANX_T1_M
ANX_T2_M ~~ NA*ANX_T2_M
ANX_T3_M ~~ NA*ANX_T3_M

ANX_T1_F ~~ NA*ANX_T1_F
ANX_T2_F ~~ NA*ANX_T2_F
ANX_T3_F ~~ NA*ANX_T3_F

#intercepts & latent means
ANX_P1.1.2 ~ nu11*1
ANX_P2.1.2 ~ nu12*1
ANX_P3.1.2 ~ nu13*1
ANX_P1.2.2 ~ nu11*1
ANX_P2.2.2 ~ nu12*1
ANX_P3.2.2 ~ nu13*1
ANX_P1.3.2 ~ nu11*1
ANX_P2.3.2 ~ nu12*1
ANX_P3.3.2 ~ nu13*1

ANX_P1.1.1 ~ nu11*1
ANX_P2.1.1 ~ nu12*1
ANX_P3.1.1 ~ nu13*1
ANX_P1.2.1 ~ nu11*1
ANX_P2.2.1 ~ nu12*1
ANX_P3.2.1 ~ nu13*1
ANX_P1.3.1 ~ nu11*1
ANX_P2.3.1 ~ nu12*1
ANX_P3.3.1 ~ nu13*1

#Factor Means
ANX_T1_M ~ 0*1
ANX_T2_M ~ NA*1
ANX_T3_M ~ NA*1

ANX_T1_F ~ NA*1
ANX_T2_F ~ NA*1
ANX_T3_F ~ NA*1
"
fit_ANX_strong <- cfa(model_ANX_strong, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ANX_strong, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ANX_strong, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

summary(compareFit(fit_ANX_conf, fit_ANX_metric, fit_ANX_strong))

model_ANX_strong_pt <-
  "
ANX_T1_M =~ L11*ANX_P1.1.2 + L12*ANX_P2.1.2 + L13*ANX_P3.1.2
ANX_T2_M =~ L11*ANX_P1.2.2 + L12*ANX_P2.2.2 + L13*ANX_P3.2.2
ANX_T3_M =~ L11*ANX_P1.3.2 + L12*ANX_P2.3.2 + L13*ANX_P3.3.2

ANX_T1_F =~ L11*ANX_P1.1.1 + L12*ANX_P2.1.1 + L13*ANX_P3.1.1
ANX_T2_F =~ L11*ANX_P1.2.1 + L12*ANX_P2.2.1 + L13*ANX_P3.2.1
ANX_T3_F =~ L11*ANX_P1.3.1 + L12*ANX_P2.3.1 + L13*ANX_P3.3.1

# Longitudinal correlated residuals
ANX_P1.1.2 ~~ ANX_P1.2.2 + ANX_P1.3.2 
ANX_P1.2.2 ~~ ANX_P1.3.2
ANX_P2.1.2 ~~ ANX_P2.2.2 + ANX_P2.3.2
ANX_P2.2.2 ~~ ANX_P2.3.2
ANX_P3.1.2 ~~ ANX_P3.2.2 + ANX_P3.3.2
ANX_P3.2.2 ~~ ANX_P3.3.2

ANX_P1.1.1 ~~ ANX_P1.2.1 + ANX_P1.3.1 
ANX_P1.2.1 ~~ ANX_P1.3.1
ANX_P2.1.1 ~~ ANX_P2.2.1 + ANX_P2.3.1
ANX_P2.2.1 ~~ ANX_P2.3.1
ANX_P3.1.1 ~~ ANX_P3.2.1 + ANX_P3.3.1
ANX_P3.2.1 ~~ ANX_P3.3.1

#Diadic correlated residuals
ANX_P1.1.1 ~~ ANX_P1.1.2
ANX_P2.1.1 ~~ ANX_P2.1.2
ANX_P3.1.1 ~~ ANX_P3.1.2

ANX_P1.2.1 ~~ ANX_P1.2.2
ANX_P2.2.1 ~~ ANX_P2.2.2
ANX_P3.2.1 ~~ ANX_P3.2.2

ANX_P1.3.1 ~~ ANX_P1.3.2
ANX_P2.3.1 ~~ ANX_P2.3.2
ANX_P3.3.1 ~~ ANX_P3.3.2

# Latent var.
ANX_T1_M ~~ 1*ANX_T1_M
ANX_T2_M ~~ NA*ANX_T2_M
ANX_T3_M ~~ NA*ANX_T3_M

ANX_T1_F ~~ NA*ANX_T1_F
ANX_T2_F ~~ NA*ANX_T2_F
ANX_T3_F ~~ NA*ANX_T3_F

#intercepts & latent means
ANX_P1.1.2 ~ nuM11*1
ANX_P2.1.2 ~ nuM12*1
ANX_P3.1.2 ~ nuM13*1
ANX_P1.2.2 ~ nuM11*1
ANX_P2.2.2 ~ nuM12*1
ANX_P3.2.2 ~ nuM13*1
ANX_P1.3.2 ~ nuM11*1
ANX_P2.3.2 ~ nuM12*1
ANX_P3.3.2 ~ nuM13*1

ANX_P1.1.1 ~ nuM11*1
ANX_P2.1.1 ~ nuF12*1
ANX_P3.1.1 ~ nuM13*1
ANX_P1.2.1 ~ nuM11*1
ANX_P2.2.1 ~ nuF12*1
ANX_P3.2.1 ~ nuM13*1
ANX_P1.3.1 ~ nuM11*1
ANX_P2.3.1 ~ nuF12*1
ANX_P3.3.1 ~ nuM13*1

#Factor Means
ANX_T1_M ~ 0*1
ANX_T2_M ~ NA*1
ANX_T3_M ~ NA*1

ANX_T1_F ~ NA*1
ANX_T2_F ~ NA*1
ANX_T3_F ~ NA*1
"
fit_ANX_strong_pt <- cfa(model_ANX_strong_pt, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_ANX_strong_pt, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_ANX_strong_pt, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

summary(compareFit(fit_ANX_conf, fit_ANX_metric, fit_ANX_strong_pt))

## CFA & MI for Attachment-related Avoidance - (T1-T3; Fathers & Mother) ----
model_AVOI_conf <-
  "
AVOI_T1_M =~ LM11*AVOI_P1.1.2 + LM12*AVOI_P2.1.2 + LM13*AVOI_P3.1.2
AVOI_T2_M =~ LM21*AVOI_P1.2.2 + LM22*AVOI_P2.2.2 + LM23*AVOI_P3.2.2
AVOI_T3_M =~ LM31*AVOI_P1.3.2 + LM32*AVOI_P2.3.2 + LM33*AVOI_P3.3.2

AVOI_T1_F =~ LF11*AVOI_P1.1.1 + LF12*AVOI_P2.1.1 + LF13*AVOI_P3.1.1
AVOI_T2_F =~ LF21*AVOI_P1.2.1 + LF22*AVOI_P2.2.1 + LF23*AVOI_P3.2.1
AVOI_T3_F =~ LF31*AVOI_P1.3.1 + LF32*AVOI_P2.3.1 + LF33*AVOI_P3.3.1

# Longitudinal correlated residuals
AVOI_P1.1.2 ~~ AVOI_P1.2.2 + AVOI_P1.3.2 
AVOI_P1.2.2 ~~ AVOI_P1.3.2
AVOI_P2.1.2 ~~ AVOI_P2.2.2 + AVOI_P2.3.2
AVOI_P2.2.2 ~~ AVOI_P2.3.2
AVOI_P3.1.2 ~~ AVOI_P3.2.2 + AVOI_P3.3.2
AVOI_P3.2.2 ~~ AVOI_P3.3.2

AVOI_P1.1.1 ~~ AVOI_P1.2.1 + AVOI_P1.3.1 
AVOI_P1.2.1 ~~ AVOI_P1.3.1
AVOI_P2.1.1 ~~ AVOI_P2.2.1 + AVOI_P2.3.1
AVOI_P2.2.1 ~~ AVOI_P2.3.1
AVOI_P3.1.1 ~~ AVOI_P3.2.1 + AVOI_P3.3.1
AVOI_P3.2.1 ~~ AVOI_P3.3.1

#Diadic correlated residuals
AVOI_P1.1.1 ~~ AVOI_P1.1.2
AVOI_P2.1.1 ~~ AVOI_P2.1.2
AVOI_P3.1.1 ~~ AVOI_P3.1.2

AVOI_P1.2.1 ~~ AVOI_P1.2.2
AVOI_P2.2.1 ~~ AVOI_P2.2.2
AVOI_P3.2.1 ~~ AVOI_P3.2.2

AVOI_P1.3.1 ~~ AVOI_P1.3.2
AVOI_P2.3.1 ~~ AVOI_P2.3.2
AVOI_P3.3.1 ~~ AVOI_P3.3.2

# Latent var.
AVOI_T1_M ~~ 1*AVOI_T1_M
AVOI_T2_M ~~ 1*AVOI_T2_M
AVOI_T3_M ~~ 1*AVOI_T3_M

AVOI_T1_F ~~ 1*AVOI_T1_F
AVOI_T2_F ~~ 1*AVOI_T2_F
AVOI_T3_F ~~ 1*AVOI_T3_F

#intercepts & latent means
AVOI_P1.1.2 ~ nuM11*1
AVOI_P2.1.2 ~ nuM12*1
AVOI_P3.1.2 ~ nuM13*1
AVOI_P1.2.2 ~ nuM21*1
AVOI_P2.2.2 ~ nuM22*1
AVOI_P3.2.2 ~ nuM23*1
AVOI_P1.3.2 ~ nuM31*1
AVOI_P2.3.2 ~ nuM32*1
AVOI_P3.3.2 ~ nuM33*1

AVOI_P1.1.1 ~ nuF11*1
AVOI_P2.1.1 ~ nuF12*1
AVOI_P3.1.1 ~ nuF13*1
AVOI_P1.2.1 ~ nuF21*1
AVOI_P2.2.1 ~ nuF22*1
AVOI_P3.2.1 ~ nuF23*1
AVOI_P1.3.1 ~ nuF31*1
AVOI_P2.3.1 ~ nuF32*1
AVOI_P3.3.1 ~ nuF33*1

#Factor Means
AVOI_T1_M ~ 0*1
AVOI_T2_M ~ 0*1
AVOI_T3_M ~ 0*1

AVOI_T1_F ~ 0*1
AVOI_T2_F ~ 0*1
AVOI_T3_F ~ 0*1
"
fit_AVOI_conf <- cfa(model_AVOI_conf, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_AVOI_conf, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_AVOI_conf, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

model_AVOI_metric <-
  "
AVOI_T1_M =~ L11*AVOI_P1.1.2 + L12*AVOI_P2.1.2 + L13*AVOI_P3.1.2
AVOI_T2_M =~ L11*AVOI_P1.2.2 + L12*AVOI_P2.2.2 + L13*AVOI_P3.2.2
AVOI_T3_M =~ L11*AVOI_P1.3.2 + L12*AVOI_P2.3.2 + L13*AVOI_P3.3.2

AVOI_T1_F =~ L11*AVOI_P1.1.1 + L12*AVOI_P2.1.1 + L13*AVOI_P3.1.1
AVOI_T2_F =~ L11*AVOI_P1.2.1 + L12*AVOI_P2.2.1 + L13*AVOI_P3.2.1
AVOI_T3_F =~ L11*AVOI_P1.3.1 + L12*AVOI_P2.3.1 + L13*AVOI_P3.3.1

# Longitudinal correlated residuals
AVOI_P1.1.2 ~~ AVOI_P1.2.2 + AVOI_P1.3.2 
AVOI_P1.2.2 ~~ AVOI_P1.3.2
AVOI_P2.1.2 ~~ AVOI_P2.2.2 + AVOI_P2.3.2
AVOI_P2.2.2 ~~ AVOI_P2.3.2
AVOI_P3.1.2 ~~ AVOI_P3.2.2 + AVOI_P3.3.2
AVOI_P3.2.2 ~~ AVOI_P3.3.2

AVOI_P1.1.1 ~~ AVOI_P1.2.1 + AVOI_P1.3.1 
AVOI_P1.2.1 ~~ AVOI_P1.3.1
AVOI_P2.1.1 ~~ AVOI_P2.2.1 + AVOI_P2.3.1
AVOI_P2.2.1 ~~ AVOI_P2.3.1
AVOI_P3.1.1 ~~ AVOI_P3.2.1 + AVOI_P3.3.1
AVOI_P3.2.1 ~~ AVOI_P3.3.1

#Diadic correlated residuals
AVOI_P1.1.1 ~~ AVOI_P1.1.2
AVOI_P2.1.1 ~~ AVOI_P2.1.2
AVOI_P3.1.1 ~~ AVOI_P3.1.2

AVOI_P1.2.1 ~~ AVOI_P1.2.2
AVOI_P2.2.1 ~~ AVOI_P2.2.2
AVOI_P3.2.1 ~~ AVOI_P3.2.2

AVOI_P1.3.1 ~~ AVOI_P1.3.2
AVOI_P2.3.1 ~~ AVOI_P2.3.2
AVOI_P3.3.1 ~~ AVOI_P3.3.2

# Latent var.
AVOI_T1_M ~~ 1*AVOI_T1_M
AVOI_T2_M ~~ NA*AVOI_T2_M
AVOI_T3_M ~~ NA*AVOI_T3_M

AVOI_T1_F ~~ NA*AVOI_T1_F
AVOI_T2_F ~~ NA*AVOI_T2_F
AVOI_T3_F ~~ NA*AVOI_T3_F

#intercepts & latent means
AVOI_P1.1.2 ~ nuM11*1
AVOI_P2.1.2 ~ nuM12*1
AVOI_P3.1.2 ~ nuM13*1
AVOI_P1.2.2 ~ nuM21*1
AVOI_P2.2.2 ~ nuM22*1
AVOI_P3.2.2 ~ nuM23*1
AVOI_P1.3.2 ~ nuM31*1
AVOI_P2.3.2 ~ nuM32*1
AVOI_P3.3.2 ~ nuM33*1

AVOI_P1.1.1 ~ nuF11*1
AVOI_P2.1.1 ~ nuF12*1
AVOI_P3.1.1 ~ nuF13*1
AVOI_P1.2.1 ~ nuF21*1
AVOI_P2.2.1 ~ nuF22*1
AVOI_P3.2.1 ~ nuF23*1
AVOI_P1.3.1 ~ nuF31*1
AVOI_P2.3.1 ~ nuF32*1
AVOI_P3.3.1 ~ nuF33*1

#Factor Means
AVOI_T1_M ~ 0*1
AVOI_T2_M ~ 0*1
AVOI_T3_M ~ 0*1

AVOI_T1_F ~ 0*1
AVOI_T2_F ~ 0*1
AVOI_T3_F ~ 0*1
"
fit_AVOI_metric <- cfa(model_AVOI_metric, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_AVOI_metric, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_AVOI_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

model_AVOI_strong <-
  "
AVOI_T1_M =~ L11*AVOI_P1.1.2 + L12*AVOI_P2.1.2 + L13*AVOI_P3.1.2
AVOI_T2_M =~ L11*AVOI_P1.2.2 + L12*AVOI_P2.2.2 + L13*AVOI_P3.2.2
AVOI_T3_M =~ L11*AVOI_P1.3.2 + L12*AVOI_P2.3.2 + L13*AVOI_P3.3.2

AVOI_T1_F =~ L11*AVOI_P1.1.1 + L12*AVOI_P2.1.1 + L13*AVOI_P3.1.1
AVOI_T2_F =~ L11*AVOI_P1.2.1 + L12*AVOI_P2.2.1 + L13*AVOI_P3.2.1
AVOI_T3_F =~ L11*AVOI_P1.3.1 + L12*AVOI_P2.3.1 + L13*AVOI_P3.3.1

# Longitudinal correlated residuals
AVOI_P1.1.2 ~~ AVOI_P1.2.2 + AVOI_P1.3.2 
AVOI_P1.2.2 ~~ AVOI_P1.3.2
AVOI_P2.1.2 ~~ AVOI_P2.2.2 + AVOI_P2.3.2
AVOI_P2.2.2 ~~ AVOI_P2.3.2
AVOI_P3.1.2 ~~ AVOI_P3.2.2 + AVOI_P3.3.2
AVOI_P3.2.2 ~~ AVOI_P3.3.2

AVOI_P1.1.1 ~~ AVOI_P1.2.1 + AVOI_P1.3.1 
AVOI_P1.2.1 ~~ AVOI_P1.3.1
AVOI_P2.1.1 ~~ AVOI_P2.2.1 + AVOI_P2.3.1
AVOI_P2.2.1 ~~ AVOI_P2.3.1
AVOI_P3.1.1 ~~ AVOI_P3.2.1 + AVOI_P3.3.1
AVOI_P3.2.1 ~~ AVOI_P3.3.1

#Diadic correlated residuals
AVOI_P1.1.1 ~~ AVOI_P1.1.2
AVOI_P2.1.1 ~~ AVOI_P2.1.2
AVOI_P3.1.1 ~~ AVOI_P3.1.2

AVOI_P1.2.1 ~~ AVOI_P1.2.2
AVOI_P2.2.1 ~~ AVOI_P2.2.2
AVOI_P3.2.1 ~~ AVOI_P3.2.2

AVOI_P1.3.1 ~~ AVOI_P1.3.2
AVOI_P2.3.1 ~~ AVOI_P2.3.2
AVOI_P3.3.1 ~~ AVOI_P3.3.2

# Latent var.
AVOI_T1_M ~~ 1*AVOI_T1_M
AVOI_T2_M ~~ NA*AVOI_T2_M
AVOI_T3_M ~~ NA*AVOI_T3_M

AVOI_T1_F ~~ NA*AVOI_T1_F
AVOI_T2_F ~~ NA*AVOI_T2_F
AVOI_T3_F ~~ NA*AVOI_T3_F

#intercepts & latent means
AVOI_P1.1.2 ~ nu11*1
AVOI_P2.1.2 ~ nu12*1
AVOI_P3.1.2 ~ nu13*1
AVOI_P1.2.2 ~ nu11*1
AVOI_P2.2.2 ~ nu12*1
AVOI_P3.2.2 ~ nu13*1
AVOI_P1.3.2 ~ nu11*1
AVOI_P2.3.2 ~ nu12*1
AVOI_P3.3.2 ~ nu13*1

AVOI_P1.1.1 ~ nu11*1
AVOI_P2.1.1 ~ nu12*1
AVOI_P3.1.1 ~ nu13*1
AVOI_P1.2.1 ~ nu11*1
AVOI_P2.2.1 ~ nu12*1
AVOI_P3.2.1 ~ nu13*1
AVOI_P1.3.1 ~ nu11*1
AVOI_P2.3.1 ~ nu12*1
AVOI_P3.3.1 ~ nu13*1

#Factor Means
AVOI_T1_M ~ 0*1
AVOI_T2_M ~ NA*1
AVOI_T3_M ~ NA*1

AVOI_T1_F ~ NA*1
AVOI_T2_F ~ NA*1
AVOI_T3_F ~ NA*1
"
fit_AVOI_strong <- cfa(model_AVOI_strong, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_AVOI_strong, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_AVOI_strong, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

summary(compareFit(fit_AVOI_conf, fit_AVOI_metric, fit_AVOI_strong))

model_AVOI_strong_pt <-
  "
AVOI_T1_M =~ L11*AVOI_P1.1.2 + L12*AVOI_P2.1.2 + L13*AVOI_P3.1.2
AVOI_T2_M =~ L11*AVOI_P1.2.2 + L12*AVOI_P2.2.2 + L13*AVOI_P3.2.2
AVOI_T3_M =~ L11*AVOI_P1.3.2 + L12*AVOI_P2.3.2 + L13*AVOI_P3.3.2

AVOI_T1_F =~ L11*AVOI_P1.1.1 + L12*AVOI_P2.1.1 + L13*AVOI_P3.1.1
AVOI_T2_F =~ L11*AVOI_P1.2.1 + L12*AVOI_P2.2.1 + L13*AVOI_P3.2.1
AVOI_T3_F =~ L11*AVOI_P1.3.1 + L12*AVOI_P2.3.1 + L13*AVOI_P3.3.1

# Longitudinal correlated residuals
AVOI_P1.1.2 ~~ AVOI_P1.2.2 + AVOI_P1.3.2 
AVOI_P1.2.2 ~~ AVOI_P1.3.2
AVOI_P2.1.2 ~~ AVOI_P2.2.2 + AVOI_P2.3.2
AVOI_P2.2.2 ~~ AVOI_P2.3.2
AVOI_P3.1.2 ~~ AVOI_P3.2.2 + AVOI_P3.3.2
AVOI_P3.2.2 ~~ AVOI_P3.3.2

AVOI_P1.1.1 ~~ AVOI_P1.2.1 + AVOI_P1.3.1 
AVOI_P1.2.1 ~~ AVOI_P1.3.1
AVOI_P2.1.1 ~~ AVOI_P2.2.1 + AVOI_P2.3.1
AVOI_P2.2.1 ~~ AVOI_P2.3.1
AVOI_P3.1.1 ~~ AVOI_P3.2.1 + AVOI_P3.3.1
AVOI_P3.2.1 ~~ AVOI_P3.3.1

#Diadic correlated residuals
AVOI_P1.1.1 ~~ AVOI_P1.1.2
AVOI_P2.1.1 ~~ AVOI_P2.1.2
AVOI_P3.1.1 ~~ AVOI_P3.1.2

AVOI_P1.2.1 ~~ AVOI_P1.2.2
AVOI_P2.2.1 ~~ AVOI_P2.2.2
AVOI_P3.2.1 ~~ AVOI_P3.2.2

AVOI_P1.3.1 ~~ AVOI_P1.3.2
AVOI_P2.3.1 ~~ AVOI_P2.3.2
AVOI_P3.3.1 ~~ AVOI_P3.3.2

# Latent var.
AVOI_T1_M ~~ 1*AVOI_T1_M
AVOI_T2_M ~~ NA*AVOI_T2_M
AVOI_T3_M ~~ NA*AVOI_T3_M

AVOI_T1_F ~~ NA*AVOI_T1_F
AVOI_T2_F ~~ NA*AVOI_T2_F
AVOI_T3_F ~~ NA*AVOI_T3_F

#intercepts & latent means
AVOI_P1.1.2 ~ nuM11*1
AVOI_P2.1.2 ~ nuM12*1
AVOI_P3.1.2 ~ nuM13*1
AVOI_P1.2.2 ~ nuM11*1
AVOI_P2.2.2 ~ nuM12*1
AVOI_P3.2.2 ~ nuM13*1
AVOI_P1.3.2 ~ nuM11*1
AVOI_P2.3.2 ~ nuM12*1
AVOI_P3.3.2 ~ nuM13*1

AVOI_P1.1.1 ~ nuF11*1
AVOI_P2.1.1 ~ nuF12*1
AVOI_P3.1.1 ~ nuF13*1
AVOI_P1.2.1 ~ nuF11*1
AVOI_P2.2.1 ~ nuF12*1
AVOI_P3.2.1 ~ nuF13*1
AVOI_P1.3.1 ~ nuF11*1
AVOI_P2.3.1 ~ nuF12*1
AVOI_P3.3.1 ~ nuF13*1

#Factor Means
AVOI_T1_M ~ 0*1
AVOI_T2_M ~ NA*1
AVOI_T3_M ~ NA*1

AVOI_T1_F ~ NA*1
AVOI_T2_F ~ NA*1
AVOI_T3_F ~ NA*1
"
fit_AVOI_strong_pt <- cfa(model_AVOI_strong_pt, data = SEC_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_AVOI_strong_pt, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_AVOI_strong_pt, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr"))

summary(compareFit(fit_AVOI_conf, fit_AVOI_metric, fit_AVOI_strong_pt))

# Parenting Relationship Questionnaire (PRQ) - measurement study ----
ATT_DF <- as.data.frame(read.csv(file="\\measurement study\\attachment_emotionregulation\\ATT_DF.csv"))

## Creating items parcels (with random allocation) - Attachment ----
#  PRQ_1, PRQ_2, PRQ_3, PRQ_16, PRQ_21, PRQ_26, PRQ_31, PRQ_36, PRQ_40, PRQ_42, PRQ_43

# CREATING PARCELS WITH RANDOM ALLOCATION
vars <-
  c(
    "PRQ_1",
    "PRQ_2",
    "PRQ_3",
    "PRQ_16",
    "PRQ_21",
    "PRQ_26",
    "PRQ_31",
    "PRQ_36",
    "PRQ_40",
    "PRQ_42",
    "PRQ_43"
  )
set.seed(2020)
vars_parcels <- sample(vars, length(vars), replace = FALSE)

#FOR FATHERS
ATTACH_P1.1.1 <- paste0(vars_parcels[1:4], ".1.1")
ATTACH_P2.1.1 <- paste0(vars_parcels[5:8], ".1.1")
ATTACH_P3.1.1 <- paste0(vars_parcels[9:11], ".1.1")
ATTACH_P1.2.1 <- paste0(vars_parcels[1:4], ".2.1")
ATTACH_P2.2.1 <- paste0(vars_parcels[5:8], ".2.1")
ATTACH_P3.2.1 <- paste0(vars_parcels[9:11], ".2.1")
ATTACH_P1.3.1 <- paste0(vars_parcels[1:4], ".3.1")
ATTACH_P2.3.1 <- paste0(vars_parcels[5:8], ".3.1")
ATTACH_P3.3.1 <- paste0(vars_parcels[9:11], ".3.1")
ATTACH_P1.4.1 <- paste0(vars_parcels[1:4], ".4.1")
ATTACH_P2.4.1 <- paste0(vars_parcels[5:8], ".4.1")
ATTACH_P3.4.1 <- paste0(vars_parcels[9:11], ".4.1")

#FOR MOTHERS
ATTACH_P1.1.2 <- paste0(vars_parcels[1:4], ".1.2")
ATTACH_P2.1.2 <- paste0(vars_parcels[5:8], ".1.2")
ATTACH_P3.1.2 <- paste0(vars_parcels[9:11], ".1.2")
ATTACH_P1.2.2 <- paste0(vars_parcels[1:4], ".2.2")
ATTACH_P2.2.2 <- paste0(vars_parcels[5:8], ".2.2")
ATTACH_P3.2.2 <- paste0(vars_parcels[9:11], ".2.2")
ATTACH_P1.3.2 <- paste0(vars_parcels[1:4], ".3.2")
ATTACH_P2.3.2 <- paste0(vars_parcels[5:8], ".3.2")
ATTACH_P3.3.2 <- paste0(vars_parcels[9:11], ".3.2")
ATTACH_P1.4.2 <- paste0(vars_parcels[1:4], ".4.2")
ATTACH_P2.4.2 <- paste0(vars_parcels[5:8], ".4.2")
ATTACH_P3.4.2 <- paste0(vars_parcels[9:11], ".4.2")

ATT_DF <- cbind(
  ATT_DF,
  data.frame(
    ATTACH_P1.1.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.1.1),
    ATTACH_P2.1.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.1.1),
    ATTACH_P3.1.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.1.1),
    ATTACH_P1.2.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.2.1),
    ATTACH_P2.2.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.2.1),
    ATTACH_P3.2.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.2.1),
    ATTACH_P1.3.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.3.1),
    ATTACH_P2.3.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.3.1),
    ATTACH_P3.3.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.3.1),
    ATTACH_P1.4.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.4.1),
    ATTACH_P2.4.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.4.1),
    ATTACH_P3.4.1 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.4.1),
    ATTACH_P1.1.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.1.2),
    ATTACH_P2.1.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.1.2),
    ATTACH_P3.1.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.1.2),
    ATTACH_P1.2.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.2.2),
    ATTACH_P2.2.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.2.2),
    ATTACH_P3.2.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.2.2),
    ATTACH_P1.3.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.3.2),
    ATTACH_P2.3.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.3.2),
    ATTACH_P3.3.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.3.2),
    ATTACH_P1.4.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P1.4.2),
    ATTACH_P2.4.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P2.4.2),
    ATTACH_P3.4.2 = create.parcel(as.data.frame(ATT_DF), ATTACH_P3.4.2)
  )
)

## Alpha coefficients ----
# T1 - father
psych::alpha(ATT_DF[, c(
  "PRQ_1.1.1",
  "PRQ_2.1.1",
  "PRQ_3.1.1",
  "PRQ_16.1.1",
  "PRQ_21.1.1",
  "PRQ_26.1.1",
  "PRQ_31.1.1",
  "PRQ_36.1.1",
  "PRQ_40.1.1",
  "PRQ_42.1.1",
  "PRQ_43.1.1"
)])
# T1 - mother
psych::alpha(ATT_DF[, c(
  "PRQ_1.1.2",
  "PRQ_2.1.2",
  "PRQ_3.1.2",
  "PRQ_16.1.2",
  "PRQ_21.1.2",
  "PRQ_26.1.2",
  "PRQ_31.1.2",
  "PRQ_36.1.2",
  "PRQ_40.1.2",
  "PRQ_42.1.2",
  "PRQ_43.1.2"
)])
# T2 - father
psych::alpha(ATT_DF[, c(
  "PRQ_1.2.1",
  "PRQ_2.2.1",
  "PRQ_3.2.1",
  "PRQ_16.2.1",
  "PRQ_21.2.1",
  "PRQ_26.2.1",
  "PRQ_31.2.1",
  "PRQ_36.2.1",
  "PRQ_40.2.1",
  "PRQ_42.2.1",
  "PRQ_43.2.1"
)])
# T2 - mother
psych::alpha(ATT_DF[, c(
  "PRQ_1.2.2",
  "PRQ_2.2.2",
  "PRQ_3.2.2",
  "PRQ_16.2.2",
  "PRQ_21.2.2",
  "PRQ_26.2.2",
  "PRQ_31.2.2",
  "PRQ_36.2.2",
  "PRQ_40.2.2",
  "PRQ_42.2.2",
  "PRQ_43.2.2"
)])
# T3 - father
psych::alpha(ATT_DF[, c(
  "PRQ_1.3.1",
  "PRQ_2.3.1",
  "PRQ_3.3.1",
  "PRQ_16.3.1",
  "PRQ_21.3.1",
  "PRQ_26.3.1",
  "PRQ_31.3.1",
  "PRQ_36.3.1",
  "PRQ_40.3.1",
  "PRQ_42.3.1",
  "PRQ_43.3.1"
)])
# T3 - mother
psych::alpha(ATT_DF[, c(
  "PRQ_1.3.2",
  "PRQ_2.3.2",
  "PRQ_3.3.2",
  "PRQ_16.3.2",
  "PRQ_21.3.2",
  "PRQ_26.3.2",
  "PRQ_31.3.2",
  "PRQ_36.3.2",
  "PRQ_40.3.2",
  "PRQ_42.3.2",
  "PRQ_43.3.2"
)])

## CFA & MI for Attachment (T1-T3; Fathers & Mother) ----
model_attach_conf <-
  "
ATTACH_T1_F =~ LF11*ATTACH_P1.1.1 + LF12*ATTACH_P2.1.1 + LF13*ATTACH_P3.1.1
ATTACH_T2_F =~ LF21*ATTACH_P1.2.1 + LF22*ATTACH_P2.2.1 + LF23*ATTACH_P3.2.1
ATTACH_T3_F =~ LF31*ATTACH_P1.3.1 + LF32*ATTACH_P2.3.1 + LF33*ATTACH_P3.3.1

ATTACH_T1_M =~ LM11*ATTACH_P1.1.2 + LM12*ATTACH_P2.1.2 + LM13*ATTACH_P3.1.2
ATTACH_T2_M =~ LM21*ATTACH_P1.2.2 + LM22*ATTACH_P2.2.2 + LM23*ATTACH_P3.2.2
ATTACH_T3_M =~ LM31*ATTACH_P1.3.2 + LM32*ATTACH_P2.3.2 + LM33*ATTACH_P3.3.2

# Correlated residual - repeated measures
ATTACH_P1.1.1 ~~ ATTACH_P1.2.1 + ATTACH_P1.3.1
ATTACH_P1.2.1 ~~ ATTACH_P1.3.1
ATTACH_P2.1.1 ~~ ATTACH_P2.2.1 + ATTACH_P2.3.1
ATTACH_P2.2.1 ~~ ATTACH_P2.3.1
ATTACH_P3.1.1 ~~ ATTACH_P3.2.1 + ATTACH_P3.3.1
ATTACH_P3.2.1 ~~ ATTACH_P3.3.1

ATTACH_P1.1.2 ~~ ATTACH_P1.2.2 + ATTACH_P1.3.2 
ATTACH_P1.2.2 ~~ ATTACH_P1.3.2
ATTACH_P2.1.2 ~~ ATTACH_P2.2.2 + ATTACH_P2.3.2
ATTACH_P2.2.2 ~~ ATTACH_P2.3.2
ATTACH_P3.1.2 ~~ ATTACH_P3.2.2 + ATTACH_P3.3.2
ATTACH_P3.2.2 ~~ ATTACH_P3.3.2

#Diadic correlated residuals
ATTACH_P1.1.1 ~~ ATTACH_P1.1.2
ATTACH_P2.1.1 ~~ ATTACH_P2.1.2
ATTACH_P3.1.1 ~~ ATTACH_P3.1.2
ATTACH_P1.2.1 ~~ ATTACH_P1.2.2
ATTACH_P2.2.1 ~~ ATTACH_P2.2.2
ATTACH_P3.2.1 ~~ ATTACH_P3.2.2
ATTACH_P1.3.1 ~~ ATTACH_P1.3.2
ATTACH_P2.3.1 ~~ ATTACH_P2.3.2
ATTACH_P3.3.1 ~~ ATTACH_P3.3.2

# Latent var.
ATTACH_T1_M ~~ 1*ATTACH_T1_M
ATTACH_T2_M ~~ 1*ATTACH_T2_M
ATTACH_T3_M ~~ 1*ATTACH_T3_M

ATTACH_T1_F ~~ 1*ATTACH_T1_F
ATTACH_T2_F ~~ 1*ATTACH_T2_F
ATTACH_T3_F ~~ 1*ATTACH_T3_F

#intercepts & latent means
ATTACH_P1.1.1 ~ nuF11*1
ATTACH_P2.1.1 ~ nuF12*1
ATTACH_P3.1.1 ~ nuF13*1
ATTACH_P1.2.1 ~ nuF21*1
ATTACH_P2.2.1 ~ nuF22*1
ATTACH_P3.2.1 ~ nuF23*1
ATTACH_P1.3.1 ~ nuF31*1
ATTACH_P2.3.1 ~ nuF32*1
ATTACH_P3.3.1 ~ nuF33*1

ATTACH_P1.1.2 ~ nuM11*1
ATTACH_P2.1.2 ~ nuM12*1
ATTACH_P3.1.2 ~ nuM13*1
ATTACH_P1.2.2 ~ nuM21*1
ATTACH_P2.2.2 ~ nuM22*1
ATTACH_P3.2.2 ~ nuM23*1
ATTACH_P1.3.2 ~ nuM31*1
ATTACH_P2.3.2 ~ nuM32*1
ATTACH_P3.3.2 ~ nuM33*1

ATTACH_T1_F ~ 0*1
ATTACH_T2_F ~ 0*1
ATTACH_T3_F ~ 0*1

ATTACH_T1_M ~ 0*1
ATTACH_T2_M ~ 0*1
ATTACH_T3_M ~ 0*1
"
fit_attach_conf <- cfa(model_attach_conf, data = ATT_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_attach_conf, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_attach_conf, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr_mplus"))


model_attach_metric <-
  "
ATTACH_T1_M =~ L11*ATTACH_P1.1.2 + L12*ATTACH_P2.1.2 + L13*ATTACH_P3.1.2
ATTACH_T2_M =~ L11*ATTACH_P1.2.2 + L12*ATTACH_P2.2.2 + L13*ATTACH_P3.2.2
ATTACH_T3_M =~ L11*ATTACH_P1.3.2 + L12*ATTACH_P2.3.2 + L13*ATTACH_P3.3.2

ATTACH_T1_F =~ L11*ATTACH_P1.1.1 + L12*ATTACH_P2.1.1 + L13*ATTACH_P3.1.1
ATTACH_T2_F =~ L11*ATTACH_P1.2.1 + L12*ATTACH_P2.2.1 + L13*ATTACH_P3.2.1
ATTACH_T3_F =~ L11*ATTACH_P1.3.1 + L12*ATTACH_P2.3.1 + L13*ATTACH_P3.3.1

# Longitudinal correlated residuals
ATTACH_P1.1.2 ~~ ATTACH_P1.2.2 + ATTACH_P1.3.2 
ATTACH_P1.2.2 ~~ ATTACH_P1.3.2
ATTACH_P2.1.2 ~~ ATTACH_P2.2.2 + ATTACH_P2.3.2
ATTACH_P2.2.2 ~~ ATTACH_P2.3.2
ATTACH_P3.1.2 ~~ ATTACH_P3.2.2 + ATTACH_P3.3.2
ATTACH_P3.2.2 ~~ ATTACH_P3.3.2

ATTACH_P1.1.1 ~~ ATTACH_P1.2.1 + ATTACH_P1.3.1 
ATTACH_P1.2.1 ~~ ATTACH_P1.3.1
ATTACH_P2.1.1 ~~ ATTACH_P2.2.1 + ATTACH_P2.3.1
ATTACH_P2.2.1 ~~ ATTACH_P2.3.1
ATTACH_P3.1.1 ~~ ATTACH_P3.2.1 + ATTACH_P3.3.1
ATTACH_P3.2.1 ~~ ATTACH_P3.3.1

#Diadic correlated residuals
ATTACH_P1.1.1 ~~ ATTACH_P1.1.2
ATTACH_P2.1.1 ~~ ATTACH_P2.1.2
ATTACH_P3.1.1 ~~ ATTACH_P3.1.2

ATTACH_P1.2.1 ~~ ATTACH_P1.2.2
ATTACH_P2.2.1 ~~ ATTACH_P2.2.2
ATTACH_P3.2.1 ~~ ATTACH_P3.2.2

ATTACH_P1.3.1 ~~ ATTACH_P1.3.2
ATTACH_P2.3.1 ~~ ATTACH_P2.3.2
ATTACH_P3.3.1 ~~ ATTACH_P3.3.2

# Latent var.
ATTACH_T1_M ~~ 1*ATTACH_T1_M
ATTACH_T2_M ~~ NA*ATTACH_T2_M
ATTACH_T3_M ~~ NA*ATTACH_T3_M

ATTACH_T1_F ~~ NA*ATTACH_T1_F
ATTACH_T2_F ~~ NA*ATTACH_T2_F
ATTACH_T3_F ~~ NA*ATTACH_T3_F

#intercepts & latent means
ATTACH_P1.1.2 ~ nuM11*1
ATTACH_P2.1.2 ~ nuM12*1
ATTACH_P3.1.2 ~ nuM13*1
ATTACH_P1.2.2 ~ nuM21*1
ATTACH_P2.2.2 ~ nuM22*1
ATTACH_P3.2.2 ~ nuM23*1
ATTACH_P1.3.2 ~ nuM31*1
ATTACH_P2.3.2 ~ nuM32*1
ATTACH_P3.3.2 ~ nuM33*1

ATTACH_P1.1.1 ~ nuF11*1
ATTACH_P2.1.1 ~ nuF12*1
ATTACH_P3.1.1 ~ nuF13*1
ATTACH_P1.2.1 ~ nuF21*1
ATTACH_P2.2.1 ~ nuF22*1
ATTACH_P3.2.1 ~ nuF23*1
ATTACH_P1.3.1 ~ nuF31*1
ATTACH_P2.3.1 ~ nuF32*1
ATTACH_P3.3.1 ~ nuF33*1

#Factor Means
ATTACH_T1_M ~ 0*1
ATTACH_T2_M ~ 0*1
ATTACH_T3_M ~ 0*1

ATTACH_T1_F ~ 0*1
ATTACH_T2_F ~ 0*1
ATTACH_T3_F ~ 0*1
"
fit_attach_metric <- cfa(model_attach_metric, data = ATT_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_attach_metric, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_attach_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr_mplus"))

model_attach_strong <-
  "
ATTACH_T1_M =~ L11*ATTACH_P1.1.2 + L12*ATTACH_P2.1.2 + L13*ATTACH_P3.1.2
ATTACH_T2_M =~ L11*ATTACH_P1.2.2 + L12*ATTACH_P2.2.2 + L13*ATTACH_P3.2.2
ATTACH_T3_M =~ L11*ATTACH_P1.3.2 + L12*ATTACH_P2.3.2 + L13*ATTACH_P3.3.2

ATTACH_T1_F =~ L11*ATTACH_P1.1.1 + L12*ATTACH_P2.1.1 + L13*ATTACH_P3.1.1
ATTACH_T2_F =~ L11*ATTACH_P1.2.1 + L12*ATTACH_P2.2.1 + L13*ATTACH_P3.2.1
ATTACH_T3_F =~ L11*ATTACH_P1.3.1 + L12*ATTACH_P2.3.1 + L13*ATTACH_P3.3.1

# Longitudinal correlated residuals
ATTACH_P1.1.2 ~~ ATTACH_P1.2.2 + ATTACH_P1.3.2 
ATTACH_P1.2.2 ~~ ATTACH_P1.3.2
ATTACH_P2.1.2 ~~ ATTACH_P2.2.2 + ATTACH_P2.3.2
ATTACH_P2.2.2 ~~ ATTACH_P2.3.2
ATTACH_P3.1.2 ~~ ATTACH_P3.2.2 + ATTACH_P3.3.2
ATTACH_P3.2.2 ~~ ATTACH_P3.3.2

ATTACH_P1.1.1 ~~ ATTACH_P1.2.1 + ATTACH_P1.3.1 
ATTACH_P1.2.1 ~~ ATTACH_P1.3.1
ATTACH_P2.1.1 ~~ ATTACH_P2.2.1 + ATTACH_P2.3.1
ATTACH_P2.2.1 ~~ ATTACH_P2.3.1
ATTACH_P3.1.1 ~~ ATTACH_P3.2.1 + ATTACH_P3.3.1
ATTACH_P3.2.1 ~~ ATTACH_P3.3.1

#Diadic correlated residuals
ATTACH_P1.1.1 ~~ ATTACH_P1.1.2
ATTACH_P2.1.1 ~~ ATTACH_P2.1.2
ATTACH_P3.1.1 ~~ ATTACH_P3.1.2

ATTACH_P1.2.1 ~~ ATTACH_P1.2.2
ATTACH_P2.2.1 ~~ ATTACH_P2.2.2
ATTACH_P3.2.1 ~~ ATTACH_P3.2.2

ATTACH_P1.3.1 ~~ ATTACH_P1.3.2
ATTACH_P2.3.1 ~~ ATTACH_P2.3.2
ATTACH_P3.3.1 ~~ ATTACH_P3.3.2

# Latent var.
ATTACH_T1_M ~~ 1*ATTACH_T1_M
ATTACH_T2_M ~~ NA*ATTACH_T2_M
ATTACH_T3_M ~~ NA*ATTACH_T3_M

ATTACH_T1_F ~~ NA*ATTACH_T1_F
ATTACH_T2_F ~~ NA*ATTACH_T2_F
ATTACH_T3_F ~~ NA*ATTACH_T3_F

#intercepts & latent means
ATTACH_P1.1.2 ~ nu11*1
ATTACH_P2.1.2 ~ nu12*1
ATTACH_P3.1.2 ~ nu13*1
ATTACH_P1.2.2 ~ nu11*1
ATTACH_P2.2.2 ~ nu12*1
ATTACH_P3.2.2 ~ nu13*1
ATTACH_P1.3.2 ~ nu11*1
ATTACH_P2.3.2 ~ nu12*1
ATTACH_P3.3.2 ~ nu13*1

ATTACH_P1.1.1 ~ nu11*1
ATTACH_P2.1.1 ~ nu12*1
ATTACH_P3.1.1 ~ nu13*1
ATTACH_P1.2.1 ~ nu11*1
ATTACH_P2.2.1 ~ nu12*1
ATTACH_P3.2.1 ~ nu13*1
ATTACH_P1.3.1 ~ nu11*1
ATTACH_P2.3.1 ~ nu12*1
ATTACH_P3.3.1 ~ nu13*1

#Factor Means
ATTACH_T1_M ~ 0*1
ATTACH_T2_M ~ NA*1
ATTACH_T3_M ~ NA*1

ATTACH_T1_F ~ NA*1
ATTACH_T2_F ~ NA*1
ATTACH_T3_F ~ NA*1
"
fit_attach_strong <- cfa(model_attach_strong, data = ATT_DF, estimator = "MLR", missing = "fiml", std.lv = TRUE, cluster="cluster")
summary(fit_attach_strong, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_attach_strong, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust", "cfi.robust", "srmr_mplus"))

summary(compareFit(fit_attach_conf, fit_attach_metric, fit_attach_strong))


