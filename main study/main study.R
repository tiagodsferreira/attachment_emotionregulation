###
data <- read.csv(file="main_data.csv")

# Descriptive statistics ----
describe(data[,-1])

## Correlations (Results) ----
corr.test(data[,6:23])

corr <- round(cor(data[,6:23], use="pairwise"), 2)
p.mat <- cor_pmat(data[,6:23])
library(ggcorrplot)
ggcorrplot(
  corr,
  p.mat = p.mat,
  hc.order = FALSE,
  type = "lower",
  outline.col = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#E46726", "white", "#6D9EC1")
)

## Descriptive statistics - by child sex ----
describeBy(data[, c(12:20, 23)], group="child_sex")

corr.test(data[data$child_sex==0, 12:20])
corr.test(data[data$child_sex==1, 12:20])


# Structural equation modeling - single group ----
## Model1: fully constrained (time, actor-partner, and child sex) ----

names(data)
model1 <- 
  "
AXF =~ 1*f_sec_t3 + 1*f_sec_t2 + 1*f_sec_t1
AXM =~ 1*m_sec_t3 + 1*m_sec_t2 + 1*m_sec_t1

# Regression paths
AXM ~ duraxm*rel_length
AXF ~ duraxm*rel_length

c_er_t3 ~ ct13*child_age + ct23*child_sex + r13*c_er_t2 + r33*mc_att_t2 + r33*fc_att_t2 
c_er_t2 ~ ct13*child_age + ct23*child_sex + r13*c_er_t1 + r33*mc_att_t1 + r33*fc_att_t1
c_er_t1 ~ ct11*child_age + ct21*child_sex 

mc_att_t3 ~ ct43*child_age + ct53*child_sex + r63*AXM + r73*c_er_t2 + r83*mc_att_t2 + r93*fc_att_t2 
mc_att_t2 ~ ct43*child_age + ct53*child_sex + r63*AXM + r73*c_er_t1 + r83*mc_att_t1 + r93*fc_att_t1
mc_att_t1 ~ ct41*child_age + ct51*child_sex + r61*AXM 

fc_att_t3 ~ ct43*child_age + ct53*child_sex + r63*AXF + r73*c_er_t2 + r93*mc_att_t2 + r83*fc_att_t2
fc_att_t2 ~ ct43*child_age + ct53*child_sex + r63*AXF + r73*c_er_t1 + r93*mc_att_t1 + r83*fc_att_t1 
fc_att_t1 ~ ct41*child_age + ct51*child_sex + r61*AXF

# Covariances
AXF ~~ cov_axmaxf*AXM 

f_sec_t3 ~~ cov_er3*m_sec_t3 
f_sec_t2 ~~ cov_er2*m_sec_t2 
f_sec_t1 ~~ cov_er1*m_sec_t1 

c_er_t3 ~~ cov_ater32*mc_att_t3 + cov_ater32*fc_att_t3
mc_att_t3 ~~ cov_atfatm2*fc_att_t3

c_er_t2 ~~ cov_ater33*mc_att_t2 + cov_ater33*fc_att_t2
mc_att_t2 ~~ cov_atfatm2*fc_att_t2

c_er_t1 ~~ cov_ater11*mc_att_t1 + cov_ater11*fc_att_t1
mc_att_t1 ~~ cov_atfatm1*fc_att_t1
" 
fit1 <- sem(model1, data=data, estimator="MLR", 
            cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(fit1, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit1, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                    "cfi", "tli", "srmr"))


## Model2: Unconstrained time ----
model2 <- 
  '
AXF =~ 1*f_sec_t3 + 1*f_sec_t2 + 1*f_sec_t1
AXM =~ 1*m_sec_t3 + 1*m_sec_t2 + 1*m_sec_t1

# Regression paths
AXM ~ duraxm*rel_length
AXF ~ duraxm*rel_length

c_er_t3 ~ ct13*child_age + ct23*child_sex + r13*c_er_t2 + r33*mc_att_t2 + r33*fc_att_t2 
c_er_t2 ~ ct12*child_age + ct22*child_sex + r12*c_er_t1 + r32*mc_att_t1 + r32*fc_att_t1
c_er_t1 ~ ct11*child_age + ct21*child_sex 

mc_att_t3 ~ ct43*child_age + ct53*child_sex + r63*AXM + r73*c_er_t2 + r83*mc_att_t2 + r93*fc_att_t2 
mc_att_t2 ~ ct42*child_age + ct52*child_sex + r62*AXM + r72*c_er_t1 + r82*mc_att_t1 + r92*fc_att_t1
mc_att_t1 ~ ct41*child_age + ct51*child_sex + r61*AXM 

fc_att_t3 ~ ct43*child_age + ct53*child_sex + r63*AXF + r73*c_er_t2 + r93*mc_att_t2 + r83*fc_att_t2
fc_att_t2 ~ ct42*child_age + ct52*child_sex + r62*AXF + r72*c_er_t1 + r92*mc_att_t1 + r82*fc_att_t1 
fc_att_t1 ~ ct41*child_age + ct51*child_sex + r61*AXF

# Covariances
AXF ~~ cov_axmaxf*AXM 

f_sec_t3 ~~ cov_er3*m_sec_t3 
f_sec_t2 ~~ cov_er2*m_sec_t2 
f_sec_t1 ~~ cov_er1*m_sec_t1 

c_er_t3 ~~ cov_ater32*mc_att_t3 + cov_ater32*fc_att_t3
mc_att_t3 ~~ cov_atfatm3*fc_att_t3

c_er_t2 ~~ cov_ater31*mc_att_t2 + cov_ater31*fc_att_t2
mc_att_t2 ~~ cov_atfatm2*fc_att_t2

c_er_t1 ~~ cov_ater11*mc_att_t1 + cov_ater11*fc_att_t1
mc_att_t1 ~~ cov_atfatm1*fc_att_t1
' 
fit2 <- sem(model2, data=data, estimator="MLR", 
            cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(fit2, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit2, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                    "cfi", "tli", "srmr"))


## Model3: unconstrained mother and father ----
model3 <- 
  "
AXF =~ 1*f_sec_t3 + 1*f_sec_t2 + 1*f_sec_t1
AXM =~ 1*m_sec_t3 + 1*m_sec_t2 + 1*m_sec_t1

# Regression paths
AXM ~ duraxm*rel_length
AXF ~ duraxf*rel_length

c_er_t3 ~ ct13*child_age + ct23*child_sex + r13*c_er_t2 + r33*mc_att_t2 + r33*fc_att_t2 
c_er_t2 ~ ct13*child_age + ct23*child_sex + r13*c_er_t1 + r33*mc_att_t1 + r33*fc_att_t1
c_er_t1 ~ ct11*child_age + ct21*child_sex 

mc_att_t3 ~ ct33*child_age + ct53*child_sex + r53*AXM + r73*c_er_t2 + r93*mc_att_t2 + r113*fc_att_t2 
mc_att_t2 ~ ct33*child_age + ct53*child_sex + r53*AXM + r73*c_er_t1 + r93*mc_att_t1 + r113*fc_att_t1
mc_att_t1 ~ ct31*child_age + ct51*child_sex + r51*AXM 

fc_att_t3 ~ ct43*child_age + ct63*child_sex + r63*AXF + r83*c_er_t2 + r103*mc_att_t2 + r123*fc_att_t2
fc_att_t2 ~ ct43*child_age + ct63*child_sex + r63*AXF + r83*c_er_t1 + r103*mc_att_t1 + r123*fc_att_t1 
fc_att_t1 ~ ct41*child_age + ct61*child_sex + r61*AXF

# Covariances
AXF ~~ cov_axmaxf*AXM 

f_sec_t3 ~~ cov_er3*m_sec_t3 
f_sec_t2 ~~ cov_er2*m_sec_t2 
f_sec_t1 ~~ cov_er1*m_sec_t1 

c_er_t3 ~~ cov_ater32*mc_att_t3 + cov_ater31*fc_att_t3
mc_att_t3 ~~ cov_atfatm3*fc_att_t3

c_er_t2 ~~ cov_ater32*mc_att_t2 + cov_ater31*fc_att_t2
mc_att_t2 ~~ cov_atfatm2*fc_att_t2

c_er_t1 ~~ cov_ater11*mc_att_t1 + cov_ater12*fc_att_t1
mc_att_t1 ~~ cov_atfatm1*fc_att_t1

" 
fit3 <- sem(model3, data=data, estimator="MLR", 
            cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(fit3, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit3,  c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                     "cfi", "tli", "srmr"))


## Model4: Final model ----

model4 <- 
  "
AXF =~ 1*f_sec_t3 + 1*f_sec_t2 + 1*f_sec_t1
AXM =~ 1*m_sec_t3 + 1*m_sec_t2 + 1*m_sec_t1

# Regression paths
AXM ~ duraxm*rel_length
AXF ~ duraxm*rel_length

c_er_t3 ~ ct13*child_age + ct23*child_sex + r13*c_er_t2 + r31*mc_att_t2 + r31*fc_att_t2
c_er_t2 ~ ct12*child_age + ct22*child_sex + r12*c_er_t1 + r32*mc_att_t1 + r32*fc_att_t1
c_er_t1 ~ ct11*child_age + ct21*child_sex 

mc_att_t3 ~ ct43*child_age + ct63*child_sex + r43*AXM + r53*c_er_t2 + r63*mc_att_t2 + r83*fc_att_t2 
mc_att_t2 ~ ct42*child_age + ct63*child_sex + r43*AXM + r52*c_er_t1 + r63*mc_att_t1 + r82*fc_att_t1
mc_att_t1 ~ ct31*child_age + ct61*child_sex + r41*AXM 

fc_att_t3 ~ ct33*child_age + ct63*child_sex + r43*AXF + r53*c_er_t2 + r83*mc_att_t2 + r63*fc_att_t2
fc_att_t2 ~ ct33*child_age + ct63*child_sex + r43*AXF + r52*c_er_t1 + r82*mc_att_t1 + r63*fc_att_t1 
fc_att_t1 ~ ct31*child_age + ct71*child_sex + r41*AXF

# Covariances
AXF ~~ cov_axmaxf*AXM 

f_sec_t3 ~~ cov_er3*m_sec_t3 
f_sec_t2 ~~ cov_er2*m_sec_t2 
f_sec_t1 ~~ cov_er1*m_sec_t1 

c_er_t3 ~~ cov_ater32*mc_att_t3 + cov_ater32*fc_att_t3
mc_att_t3 ~~ cov_atfatm3*fc_att_t3

c_er_t2 ~~ cov_ater31*mc_att_t2 + cov_ater31*fc_att_t2
mc_att_t2 ~~ cov_atfatm2*fc_att_t2

c_er_t1 ~~ cov_ater11*mc_att_t1 + cov_ater11*fc_att_t1
mc_att_t1 ~~ cov_atfatm1*fc_att_t1
" 
fit4 <- sem(model4, data=data, estimator="MLR", 
            cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(fit4, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit4,  c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                     "cfi", "tli", "srmr"))

lavTestLRT(fit1, fit4)
lavTestLRT(fit2, fit4)
lavTestLRT(fit3, fit4)

## Model5: Final, trimmed model ----
model5 <- 
  "
AXF =~ 1*f_sec_t3 + 1*f_sec_t2 + 1*f_sec_t1
AXM =~ 1*m_sec_t3 + 1*m_sec_t2 + 1*m_sec_t1

# Regression paths
AXM ~ duraxm*rel_length
AXF ~ duraxm*rel_length
duraxm==0
c_er_t3 ~ ct13*child_age + ct23*child_sex + r13*c_er_t2 + r31*mc_att_t2 + r31*fc_att_t2
c_er_t2 ~ ct12*child_age + ct23*child_sex + r12*c_er_t1 + r32*mc_att_t1 + r32*fc_att_t1
c_er_t1 ~ ct11*child_age + ct21*child_sex 
ct13==0
ct12==0
ct11==0


mc_att_t3 ~ ct43*child_age + ct62*child_sex + r43*AXM + r53*c_er_t2 + r63*mc_att_t2 + r83*fc_att_t2
mc_att_t2 ~ ct42*child_age + ct62*child_sex + r43*AXM + r52*c_er_t1 + r63*mc_att_t1 + r82*fc_att_t1
mc_att_t1 ~ ct31*child_age + ct61*child_sex + r41*AXM 


r82==0
r52==0
ct31==0
ct61==0
ct62==0

fc_att_t3 ~ ct33*child_age + ct63*child_sex + r43*AXF + r53*c_er_t2 + r83*mc_att_t2 + r63*fc_att_t2
fc_att_t2 ~ ct33*child_age + ct63*child_sex + r43*AXF + r52*c_er_t1 + r82*mc_att_t1 + r63*fc_att_t1
fc_att_t1 ~ ct31*child_age + ct71*child_sex + r41*AXF
ct33==0


# Covariances
AXF ~~ cov_axmaxf*AXM 

f_sec_t3 ~~ cov_er3*m_sec_t3 
f_sec_t2 ~~ cov_er2*m_sec_t2 
f_sec_t1 ~~ cov_er1*m_sec_t1 

c_er_t3 ~~ cov_ater32*mc_att_t3 + cov_ater32*fc_att_t3
mc_att_t3 ~~ cov_atfatm3*fc_att_t3

c_er_t2 ~~ cov_ater31*mc_att_t2 + cov_ater31*fc_att_t2
mc_att_t2 ~~ cov_atfatm2*fc_att_t2

c_er_t1 ~~ cov_ater11*mc_att_t1 + cov_ater11*fc_att_t1
mc_att_t1 ~~ cov_atfatm1*fc_att_t1
" 
fit5 <- sem(model5, data=data, estimator="MLR", 
            cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(fit5, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit5,  c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                     "cfi", "tli", "srmr"))
lavTestLRT(fit4, fit5)






# Structural equation modeling - multigroup (child sex) (supplementary analyses) ----

## Model1: fully constrained (time, actor-partner, and child sex) ----
model1 <-
  "
c_er_t3 ~ c(ct12, ct12)*child_age + c(b11, b11)*c_er_t2 + c(b31, b31)*mc_att_t2 + c(b31, b31)*fc_att_t2
c_er_t2 ~ c(ct12, ct12)*child_age + c(b11, b11)*c_er_t1 + c(b31, b31)*mc_att_t1 + c(b31, b31)*fc_att_t1
c_er_t1 ~ c(ct11, ct11)*child_age

mc_att_t3 ~ c(ct22, ct22)*child_age +
          c(b41, b41)*c_er_t2 + c(b21, b21)*mc_att_t2 + c(b51, b51)*fc_att_t2
mc_att_t2 ~ c(ct22, ct22)*child_age +
          c(b41, b41)*c_er_t1 + c(b21, b21)*mc_att_t1 + c(b51, b51)*fc_att_t1
mc_att_t1 ~ c(ct21, ct21)*child_age

fc_att_t3 ~ c(ct22, ct22)*child_age +
          c(b41, b41)*c_er_t2 + c(b51, b51)*mc_att_t2 + c(b21, b21)*fc_att_t2
fc_att_t2 ~ c(ct22, ct22)*child_age +
          c(b41, b41)*c_er_t1 + c(b51, b51)*mc_att_t1 + c(b21, b21)*fc_att_t1
fc_att_t1 ~ c(ct21, ct21)*child_age


# Covariances
c_er_t3 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t3 + c(g.cov_ater3, g.cov_ater3)*fc_att_t3
mc_att_t3 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t3

c_er_t2 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t2 + c(g.cov_ater3, g.cov_ater3)*fc_att_t2
mc_att_t2 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t2

c_er_t1 ~~ c(g.cov_ater1, g.cov_ater1)*mc_att_t1 + c(g.cov_ater1, g.cov_ater1)*fc_att_t1
mc_att_t1 ~~ c(g.cov_atfatm1, g.cov_atfatm1)*fc_att_t1
" 
fit1 <- sem(model1, data=data, estimator="MLR", 
            group= "child_sex", cluster="school_id", 
            missing = "fiml", meanstructure=TRUE)
summary(fit1, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit1, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                     "cfi", "tli", "srmr"))

## Model2: Unconstrained time ----
model2 <-
  "
c_er_t3 ~ c(ct13, ct13)*child_age + c(b13, b13)*c_er_t2 + c(b33, b33)*mc_att_t2 + c(b33, b33)*fc_att_t2
c_er_t2 ~ c(ct12, ct12)*child_age + c(b12, b12)*c_er_t1 + c(b32, b32)*mc_att_t1 + c(b32, b32)*fc_att_t1
c_er_t1 ~ c(ct11, ct11)*child_age

mc_att_t3 ~ c(ct23, ct23)*child_age +
          c(b43, b43)*c_er_t2 + c(b23, b23)*mc_att_t2 + c(b53, b53)*fc_att_t2
mc_att_t2 ~ c(ct22, ct22)*child_age +
          c(b42, b42)*c_er_t1 + c(b21, b21)*mc_att_t1 + c(b51, b51)*fc_att_t1
mc_att_t1 ~ c(ct21, ct21)*child_age

fc_att_t3 ~ c(ct23, ct23)*child_age +
          c(b43, b43)*c_er_t2 + c(b53, b53)*mc_att_t2 + c(b23, b23)*fc_att_t2
fc_att_t2 ~ c(ct22, ct22)*child_age +
          c(b42, b42)*c_er_t1 + c(b51, b51)*mc_att_t1 + c(b21, b21)*fc_att_t1
fc_att_t1 ~ c(ct21, ct21)*child_age


# Covariances
c_er_t3 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t3 + c(g.cov_ater3, g.cov_ater3)*fc_att_t3
mc_att_t3 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t3

c_er_t2 ~~ c(g.cov_ater2, g.cov_ater2)*mc_att_t2 + c(g.cov_ater2, g.cov_ater2)*fc_att_t2
mc_att_t2 ~~ c(g.cov_atfatm2, g.cov_atfatm2)*fc_att_t2

c_er_t1 ~~ c(g.cov_ater1, g.cov_ater1)*mc_att_t1 + c(g.cov_ater1, g.cov_ater1)*fc_att_t1
mc_att_t1 ~~ c(g.cov_atfatm1, g.cov_atfatm1)*fc_att_t1
" 
fit2 <- sem(model2, data=data, estimator="MLR", group= "child_sex",
            cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(fit2, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit2, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                    "cfi", "tli", "srmr"))

lavTestLRT(fit1, fit2)

## Model3: unconstrained mother and father ----
model3 <- 
  "
c_er_t3 ~ c(ct12, ct12)*child_age + c(b11, b11)*c_er_t2 + c(b31, b31)*mc_att_t2 + c(b32, b32)*fc_att_t2 
c_er_t2 ~ c(ct12, ct12)*child_age + c(b11, b11)*c_er_t1 + c(b31, b31)*mc_att_t1 + c(b32, b32)*fc_att_t1
c_er_t1 ~ c(ct11, ct11)*child_age

mc_att_t3 ~ c(ct222, ct222)*child_age + 
          c(b41, b41)*c_er_t2 + c(b22, b22)*mc_att_t2 + c(b52, b52)*fc_att_t2 
mc_att_t2 ~ c(ct222, ct222)*child_age +  
          c(b41, b41)*c_er_t1 + c(b22, b22)*mc_att_t1 + c(b52, b52)*fc_att_t1
mc_att_t1 ~ c(ct212, ct212)*child_age 

fc_att_t3 ~ c(ct221, ct221)*child_age + 
          c(b42, b42)*c_er_t2 + c(b51, b51)*mc_att_t2 + c(b21, b21)*fc_att_t2
fc_att_t2 ~ c(ct221, ct221)*child_age + 
          c(b42, b42)*c_er_t1 + c(b51, b51)*mc_att_t1 + c(b21, b21)*fc_att_t1 
fc_att_t1 ~ c(ct211, ct211)*child_age


# Covariances
c_er_t3 ~~ c(g.cov_ater32, g.cov_ater32)*mc_att_t3 + c(g.cov_ater31, g.cov_ater31)*fc_att_t3
mc_att_t3 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t3

c_er_t2 ~~ c(g.cov_ater32, g.cov_ater32)*mc_att_t2 + c(g.cov_ater31, g.cov_ater31)*fc_att_t2
mc_att_t2 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t2

c_er_t1 ~~ c(g.cov_ater12, g.cov_ater12)*mc_att_t1 + c(g.cov_ater11, g.cov_ater11)*fc_att_t1
mc_att_t1 ~~ c(g.cov_atfatm1, g.cov_atfatm1)*fc_att_t1
" 
fit3 <- sem(model3, data=data, estimator="MLR", 
            group= "child_sex", cluster="school_id", 
            missing = "fiml", meanstructure=TRUE)
summary(fit3, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit3, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                    "cfi", "tli", "srmr"))
lavTestLRT(fit1, fit3)

## Model4: Unconstrained child sex ----
model4 <- 
  "
c_er_t3 ~ c(ct12, b.ct12)*child_age + c(b11, b.b11)*c_er_t2 + c(b31, b.b31)*mc_att_t2 + c(b31, b.b31)*fc_att_t2 
c_er_t2 ~ c(ct12, b.ct12)*child_age + c(b11, b.b11)*c_er_t1 + c(b31, b.b31)*mc_att_t1 + c(b31, b.b31)*fc_att_t1
c_er_t1 ~ c(ct11, b.ct11)*child_age

mc_att_t3 ~ c(ct22, b.ct22)*child_age + 
          c(b41, b.b41)*c_er_t2 + c(b21, b.b21)*mc_att_t2 + c(b51, b.b51)*fc_att_t2 
mc_att_t2 ~ c(ct22, b.ct22)*child_age +  
          c(b41, b.b41)*c_er_t1 + c(b21, b.b21)*mc_att_t1 + c(b51, b.b51)*fc_att_t1
mc_att_t1 ~ c(ct21, b.ct21)*child_age 

fc_att_t3 ~ c(ct22, b.ct22)*child_age + 
          c(b41, b.b41)*c_er_t2 + c(b51, b.b51)*mc_att_t2 + c(b21, b.b21)*fc_att_t2
fc_att_t2 ~ c(ct22, b.ct22)*child_age + 
          c(b41, b.b41)*c_er_t1 + c(b51, b.b51)*mc_att_t1 + c(b21, b.b21)*fc_att_t1 
fc_att_t1 ~ c(ct21, b.ct21)*child_age


# Covariances
c_er_t3 ~~ c(g.cov_ater3, b.cov_ater3)*mc_att_t3 + c(g.cov_ater3, b.cov_ater3)*fc_att_t3
mc_att_t3 ~~ c(g.cov_atfatm3, b.cov_atfatm3)*fc_att_t3

c_er_t2 ~~ c(g.cov_ater3, b.cov_ater3)*mc_att_t2 + c(g.cov_ater3, b.cov_ater3)*fc_att_t2
mc_att_t2 ~~ c(g.cov_atfatm3, b.cov_atfatm3)*fc_att_t2

c_er_t1 ~~ c(g.cov_ater1, b.cov_ater1)*mc_att_t1 + c(g.cov_ater1, b.cov_ater1)*fc_att_t1
mc_att_t1 ~~ c(g.cov_atfatm1, b.cov_atfatm1)*fc_att_t1
" 
fit4 <- sem(model4, data=data, estimator="MLR", 
            group= "child_sex", cluster="school_id",
            missing = "fiml", meanstructure=TRUE)
summary(fit4, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit4, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                    "cfi", "tli", "srmr"))
lavTestLRT(fit1, fit4)

## Model5: Final model ----
model5 <- 
  "
c_er_t3 ~ c(ct12, ct12)*child_age + c(b11, b11)*c_er_t2 + c(b33, b31)*mc_att_t2 + c(b31, b32)*fc_att_t2 
c_er_t2 ~ c(ct12, ct12)*child_age + c(b11, b12)*c_er_t1 + c(b31, b31)*mc_att_t1 + c(b31, b31)*fc_att_t1
c_er_t1 ~ c(ct11, ct11)*child_age

mc_att_t3 ~ c(ct25, ct25)*child_age + 
          c(b41, b41)*c_er_t2 + c(b21, b21)*mc_att_t2 + c(b52, b51)*fc_att_t2 
mc_att_t2 ~ c(ct23, ct24)*child_age +  
          c(b42, b42)*c_er_t1 + c(b21, b21)*mc_att_t1 + c(b52, b51)*fc_att_t1
mc_att_t1 ~ c(ct21, ct21)*child_age 

fc_att_t3 ~ c(ct22, ct22)*child_age + 
          c(b41, b41)*c_er_t2 + c(b52, b51)*mc_att_t2 + c(b22, b21)*fc_att_t2
fc_att_t2 ~ c(ct22, ct22)*child_age + 
          c(b42, b42)*c_er_t1 + c(b52, b51)*mc_att_t1 + c(b21, b21)*fc_att_t1 
fc_att_t1 ~ c(ct21, ct21)*child_age


# Covariances
c_er_t3 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t3 + c(g.cov_ater3, g.cov_ater3)*fc_att_t3
mc_att_t3 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t3

c_er_t2 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t2 + c(g.cov_ater3, g.cov_ater3)*fc_att_t2
mc_att_t2 ~~ c(g.cov_atfatm2, g.cov_atfatm2)*fc_att_t2

c_er_t1 ~~ c(g.cov_ater1, g.cov_ater1)*mc_att_t1 + c(g.cov_ater1, g.cov_ater1)*fc_att_t1
mc_att_t1 ~~ c(g.cov_atfatm1, b.cov_atfatm1)*fc_att_t1
"
fit5 <- sem(model5, data=data, estimator="MLR", 
            group= "child_sex", cluster="school_id", 
            missing = "fiml", meanstructure=TRUE)
summary(fit5, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit5, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                    "cfi", "tli", "srmr"))

lavTestLRT(fit1, fit5, nested=TRUE)
lavTestLRT(fit1, fit2, nested=TRUE)
lavTestLRT(fit1, fit3, nested=TRUE)
lavTestLRT(fit1, fit4, nested=TRUE)

## Model6: Final trimmed model ----
model6 <- 
  "
c_er_t3 ~ c(ct12, ct12)*child_age + c(b11, b11)*c_er_t2 + c(b33, b31)*mc_att_t2 + c(b31, b32)*fc_att_t2 
c_er_t2 ~ c(ct12, ct12)*child_age + c(b11, b12)*c_er_t1 + c(b31, b31)*mc_att_t1 + c(b31, b31)*fc_att_t1
c_er_t1 ~ c(ct11, ct11)*child_age

ct12==0
b52==0

mc_att_t3 ~ c(ct25, ct25)*child_age + 
          c(b41, b41)*c_er_t2 + c(b21, b21)*mc_att_t2 + c(b52, b51)*fc_att_t2 
mc_att_t2 ~ c(ct23, ct24)*child_age +  
          c(b42, b42)*c_er_t1 + c(b21, b21)*mc_att_t1 + c(b52, b51)*fc_att_t1
mc_att_t1 ~ c(ct21, ct21)*child_age 

fc_att_t3 ~ c(ct22, ct22)*child_age + 
          c(b41, b41)*c_er_t2 + c(b52, b51)*mc_att_t2 + c(b22, b21)*fc_att_t2
fc_att_t2 ~ c(ct22, ct22)*child_age + 
          c(b42, b42)*c_er_t1 + c(b52, b51)*mc_att_t1 + c(b21, b21)*fc_att_t1 
fc_att_t1 ~ c(ct21, ct21)*child_age


b42==0
ct21==0
ct22==0
ct24==0

# Covariances
c_er_t3 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t3 + c(g.cov_ater3, g.cov_ater3)*fc_att_t3
mc_att_t3 ~~ c(g.cov_atfatm3, g.cov_atfatm3)*fc_att_t3

c_er_t2 ~~ c(g.cov_ater3, g.cov_ater3)*mc_att_t2 + c(g.cov_ater3, g.cov_ater3)*fc_att_t2
mc_att_t2 ~~ c(g.cov_atfatm2, g.cov_atfatm2)*fc_att_t2

c_er_t1 ~~ c(g.cov_ater1, g.cov_ater1)*mc_att_t1 + c(g.cov_ater1, g.cov_ater1)*fc_att_t1
mc_att_t1 ~~ c(g.cov_atfatm1, b.cov_atfatm1)*fc_att_t1
"
fit6 <- sem(model6, data=data, estimator="MLR", 
            group= "child_sex", cluster="school_id",
            missing = "fiml", meanstructure=TRUE)
summary(fit6, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit6, c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "tli", "srmr"))

lavTestLRT(fit5, fit6, nested=TRUE)
lavTestLRT(fit1, fit2, nested=TRUE)
lavTestLRT(fit1, fit3, nested=TRUE)
lavTestLRT(fit1, fit4, nested=TRUE)


# Additional analysis proposed during review ----
## growth curve analysis ----
Model1 <- "
intercept_er =~ 1*c_er_t1 + 1*c_er_t2 + 1*c_er_t3
slope_er =~ 0*c_er_t1 + 1*c_er_t2 + 2*c_er_t3
intercept_m =~ 1*mc_att_t1 + 1*mc_att_t2 + 1*mc_att_t3
slope_m =~ 0*mc_att_t1 + 1*mc_att_t2 + 2*mc_att_t3
intercept_f =~ 1*fc_att_t1 + 1*fc_att_t2 + 1*fc_att_t3
slope_f =~ 0*fc_att_t1 + 1*fc_att_t2 + 2*fc_att_t3

## fixar os intercepts das variáveis observadas a zero e libertar as médias latentes (intecept e crescimento médio)
intercept_er ~ 1
slope_er ~ 1
intercept_m ~ 1
slope_m ~ 1
intercept_f ~ 1
slope_f ~ 1

c_er_t1 ~ 0*1
c_er_t2 ~ 0*1
c_er_t3 ~ 0*1
mc_att_t3 ~ 0*1
mc_att_t2 ~ 0*1
mc_att_t1 ~ 0*1
fc_att_t3 ~ 0*1
fc_att_t2 ~ 0*1
fc_att_t1 ~ 0*1

"
Fit.Model1 <- sem(Model1, data, estimator="MLR",  
               cluster="school_id", missing = "fiml", meanstructure=TRUE)
summary(Fit.Model1, standardized=T, fit.measures=T, rsquare=TRUE)
fitMeasures(Fit.Model1, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                          "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                          "cfi", "tli", "srmr"))
