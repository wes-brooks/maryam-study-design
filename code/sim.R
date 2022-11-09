library( tidyr )
library( GA )

# import scripts
source( "code/fitness.R" )
source( "code/decode.R" )

# treatment group age/sex table
age = c( "20-29", "30-39", "40-49", "50-59", "60-69", "70+" )
fem = c(19, 38, 54, 84, 120, 93)
mal = c(40, 75, 78, 125, 171, 150)

# make table into a data.frame and convert age groups to numbers
sex = data.frame( age, fem, mal )
sex$age_grp = seq_len(nrow(sex)) + 1


# expand the treatment age/sex table into a list of subjects
tmp = pivot_longer( sex, cols=c("mal", "fem"), values_to="n", names_to="sex" )
tmp$id = 1:nrow(tmp)
id = with(tmp, rep(id, n))
indx = match( id, tmp$id )
trt = tmp[indx, c("age", "age_grp", "sex")]
trt$male = with(trt, ifelse(sex=="mal", 1, 0))


# run the genetic algorithm - age, sex, and treatment effects are hard-coded
system.time( GA1 <- ga(type = "binary",
                          fitness = fitness,
                          nBits = 200,
                          trt=trt_short,
                          age_effect=0.2,
                          male_effect=0.3,
                          trt_effect=1,
                          B=100,
                          popSize = 400,
                          maxiter = 1200,
                          parallel=TRUE)
             )

