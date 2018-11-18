
install.packages("Matching")
install.packages("MatchIt")
library(tableone)
library(Matching)
library(MatchIt)
data(lalonde)

TREATMENT = 'treat'
OUTCOME = 're78'
COVARIATES = c('age', 'educ', 'black', 'hispan', 'married', 'nodegree', 're74', 're75')
table1 = CreateTableOne(vars=COVARIATES, data=lalonde, strata = TREATMENT)
print(table1, smd=TRUE)


aggregate(re78 ~ treat, lalonde, mean)



logit <- glm(treat ~ age + educ + black + hispan + married + nodegree + re75 + re74, data = lalonde, family = "binomial")
prospensity_scores = predict(logit, newdata=lalonde, type = "response")
min(prospensity_scores)
max(prospensity_scores)

set.seed(931139)
ps_matched = Match(Tr=lalonde$treat, X=prospensity_scores, replace=FALSE, M=1)
matched_data = lalonde[unlist(ps_matched[c("index.treated", "index.control")]),] 
table1_ps_matched = CreateTableOne(vars=COVARIATES, data=matched_data, strata = TREATMENT)
print(table1_ps_matched, smd=TRUE)
dim(matched_data)


set.seed(931139)
ps_matched = Match(Tr=lalonde$treat, X=prospensity_scores, replace=FALSE, M=1,caliper = 0.1)
matched_data = lalonde[unlist(ps_matched[c("index.treated", "index.control")]),] 
table1_ps_matched = CreateTableOne(vars=COVARIATES, data=matched_data, strata = TREATMENT)
print(table1_ps_matched, smd=TRUE)
dim(matched_data)

aggregate(re78 ~ treat, matched_data, mean)


t.test(matched_data$re78[matched_data$treat==1], matched_data$re78[matched_data$treat==0])
