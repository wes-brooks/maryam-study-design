run_sim = function( trt, ctl, age_effect, male_effect, trt_effect, B=1 ) {
  # n_male = round( N*male_prop )
  # 
  # sex = c( rep(1, n_male), rep(0, N-n_male) )
  # age = rlnorm( N, age_mean, age_sd )
  
  
  
  trt$trt = 1
  ctl$trt = 0
  
  # intercept = -8.25
  target = binomial()$linkfun(0.12)
  
  subjects = rbind(trt, ctl)
  subjects$linpred = with( subjects, 
                trt * trt_effect +
                age_grp * age_effect +
                male * male_effect #+ 
                # intercept
              )
  subjects$linpred = subjects$linpred + (target - mean(subjects$linpred))
  
  subjects$event = rbinom( nrow(subjects),
                           size=1,
                           prob=binomial()$linkinv(subjects$linpred)
                           )
  
  mod = glm( event ~ age_grp + sex + trt, data=subjects, family="binomial")
  
  summary(mod)$coefficients['trt', 'Pr(>|z|)']
}