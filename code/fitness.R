fitness = function( bits, trt, age_effect, male_effect, trt_effect, B=1 ) {
  ctl = decode(bits)
  
  # pp = numeric(B)
  zz = numeric(B)
  
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
  
  for (i in seq_len(B)) {
    subjects$event = rbinom( nrow(subjects),
                             size=1,
                             prob=binomial()$linkinv(subjects$linpred)
    )
    
    mod = glm( event ~ age_grp + male + trt, data=subjects, family="binomial")
    
    # pp[[i]] = summary(mod)$coefficients['trt', 'Pr(>|z|)']
    zz[[i]] = summary(mod)$coefficients['trt', 'z value']
  }
  
  # mean(pp<0.05)
  mean(zz)
}