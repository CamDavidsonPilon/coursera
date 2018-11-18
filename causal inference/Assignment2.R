COVARIATES = c('age', 'educ', 'black', 'hispan', 'married', 'nodegree', 're74', 're75')


logit <- glm(treat ~ age + educ + black + hispan + married + nodegree + re75 + re74, data = lalonde, family = "binomial")
prospensity_scores = predict(logit, newdata=lalonde, type = "response")

ipt_weights = ifelse(lalonde$treat==1, 1/prospensity_scores, 1/(1-prospensity_scores) )
max(ipt_weights)
min(ipt_weights)

weighed_data = svydesign(ids = ~1, data=lalonde, weights=~ipt_weights)
weighted_table = svyCreateTableOne(vars=COVARIATES, strata='treat', data=weighed_data, test=FALSE)
print(weighted_table, smd=TRUE)


msm <- svyglm(re78 ~ treat, design=weighed_data)

coef(msm)
confint(msm)




weight_model = ipwpoint(exposure=treat, family="binomial", link="logit",
                        denominator= ~ age + educ + black + hispan + married + nodegree + re75 + re74,
                        data=lalonde, trunc=0.02)
summary(weight_model$weights.trunc)

weighed_data = svydesign(ids = ~1, data=lalonde, weights=~weight_model$weights.trunc)
weighted_table = svyCreateTableOne(vars=COVARIATES, strata='treat', data=weighed_data, test=FALSE)
print(weighted_table, smd=TRUE)


msm <- svyglm(re78 ~ treat, design=weighed_data)

coef(msm)
confint(msm)



