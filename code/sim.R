N_control = 50
male = c(0.25, 0.5, 0.75)
age_mean = c(40, 50, 60)
age_sd = c(20, 30, 40)
male_effect = c(-0.1, 0, 0.1)
age_effect = c(0, 0.05, 0.1)
treatment_effect = c(-0.5, 0, 0.5, 1)
settings = expand.grid( male=male,
                        age_mean=age_mean,
                        age_sd=age_sd,
                        male_effect=male_effect,
                        age_effect=age_effect,
                        treatment_effect=treatment_effect
                      )


age = c("same", "rev", "even")
sex = c("same", "rev", "even")
settings2 = expand.grid(age=age, sex=sex)
settings2$age = age[settings2$age]
settings2$sex = sex[ settings2$sex ]

B = 1000

age = c( "20-29", "30-39", "40-49", "50-59", "60-69", "70+" )
fem = c(19, 38, 54, 84, 120, 93)
mal = c(40, 75, 78, 125, 171, 150)

#   Age Female Male
# 20-29     19   40
# 30-39     38   75
# 40-49     54   78
# 50-59     84  125
# 60-69    120  171
# 70+       93  150

sex = data.frame( age, fem, mal )
sex$age_grp = seq_len(nrow(sex)) + 1


# expand the treatment group into a list of subjects
tmp = pivot_longer( sex, cols=c("mal", "fem"), values_to="n", names_to="sex" )
tmp$id = 1:nrow(tmp)
id = with(tmp, rep(id, n))
indx = match( id, tmp$id )
trt = tmp[indx, c("age", "age_grp", "sex")]
trt$male = with(trt, ifelse(sex=="mal", 1, 0))

p = matrix(NA, nrow=B, ncol=nrow(settings2))

for (i in 1:nrow(settings2)) for (j in 1:B) {

  # expand the control group into a list of subjects
  mod_subj = sex
  
  # reverse the sexes if necessary
  if (settings2$sex[[i]] == "rev") {
    tmp = mod_subj$fem
    mod_subj$fem = mod_subj$mal
    mod_subj$mal = tmp
  }
  
  if (settings2$sex[[i]] == "even") {
    mod_subj$fem = mod_subj$mal
  }
  
  if (settings2$age[[i]] == "rev") {
    mod_subj$fem = rev(mod_subj$fem)
    mod_subj$mal = rev(mod_subj$mal)
  }
  
  if ( settings2$age[[i]] == "even" ) {
    mod_subj$fem = 1
    mod_subj$mal = 1
  }
  
  
    
  tmp = pivot_longer( mod_subj, cols=c("mal", "fem"), values_to="n", names_to="sex" )
  tmp$n = with( tmp, round(50 * n / sum(n)) )
  tmp$id = 1:nrow(tmp)
  id = with(tmp, rep(id, n))
  indx = match( id, tmp$id )
  control = tmp[indx, c("age", "age_grp", "sex")]
  control$male = with(control, ifelse(sex=="mal", 1, 0))
  
  
  p[[j, i]] = run_sim(trt[, c('age_grp', 'male')], control[, c('age_grp', 'male')],
                      age_effect=0.1, male_effect=0.5, trt_effect=1)
}
