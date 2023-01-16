# working out space
comod = simulate_cosinor(1000,mesor = 50,amp = 7,acro = 1,beta.mesor = 0.1,beta.amp = 0.2, beta.acro = 0.3, dist = "2_component")
m <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 12)), data = comod)
comod$pred <- predict(m$fit, type="response")

ggplot() +
  geom_point(data=comod, aes(times, Y, col=as.factor(group)), alpha=0.1) +
  geom_line(data=comod, aes(times, pred, col=as.factor(group)))
# test when you put a browser() in at the simulate data stage with multicomponent (above)
data.frame(l1=lambda1, l2=lambda2, y=y, time=ttt) %>%
  mutate(lsum = l1+l2) %>%
  pivot_longer(!time) %>%
  ggplot(aes(time, value, col=name)) +
  geom_point()


#Example showing how cosinor.glmm works with a group of multiple levels
newgroup <-
  vitamind %>%
  filter(X==1) %>%
  mutate(X=2, time = time + 4)

dat <- rbind(vitamind, newgroup) %>%
  mutate(X = as.factor(X))

dat %>%
  ggplot(aes(time, Y, col=X)) + geom_point()

m <- cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2, group = "X", period = c(12,10)), data = dat)

# Examples for meeting: 15/12/2022
# Evidence that the multi-component estimation works. In this example, amp2 = amp1 + 3, acr2 = acr1-1.
comod = simulate_cosinor(100,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 0.2, beta.acro = 0.3, dist = "2_component")
cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#
# You can now have different groups assigned to different components. For example:
vitamind$Z = rbinom(length(vitamind$X),1,prob = 0.5)
cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)

dat2 = dat
dat2$Z = (rbinom(length(dat2$X),5,prob = 0.5))
cosinor.glmm(Y~ X + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat2)


#Meeting for 19/12/2022
#Added comments to the cosinor.glmm script for everything except the update_covnames function
  #Do we need the update_covnames? Currently, it is not being used in the script.
#Added the ability to have groups be factors with more than 2 levels.Example:
cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2, group = "X", period = c(12,10)), data = dat)
#Added some error messages. For example, if n_components is not an integer, or it is less than 1,
#an error will appear with a relevant message. Examples:
cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2.1, group = "X", period = c(12,10)), data = dat)
cosinor.glmm(Y ~ group+amp.acro(time, n_components = 0, group = "X", period = c(12,10)), data = dat)
#Changed some variable names to be more descriptive and informative.
#If for whatever reason the user names their grouping variables 'rrr' or 'sss', there will be
#problems when getting the terms for the formula. Should we add a simple error message if the user
#uses these names for their groups? The code in question is this:
  r.coef[[i]] <- !is.na(str_extract(names(coefs),vec_rrr[i]))
  #Str_extract will search in the coef names for anything with rrr[number] in it, and
  #return NA for everything else. Overall, what you get is a Boolean truth vector with TRUE for everything
  #containing rrr[number]. For example if the number (i) = 1, the script will get rrr1, X:rrr1, Z:rrr1 and so
  #on. Same for s.coef[[i]].


#Next meeting
#Code now works when group argument is not specified:
cosinor.glmm(Y ~ amp.acro(time, n_components = 2, period = c(12,10)), data = dat)
#If you name your grouping variable anything with 'rrr' or 'sss', an error will appear
#with relevant information.
cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2, group = "rrr", period = c(12,10)), data = dat)

#Updated the update.covnames function so that now it prints the group factors in a nicer way.
  #Example 1:
  dat2 = dat
  dat2$Z = as.factor(rbinom(length(dat$X),5,prob = 0.5)+2)
  cosinor.glmm(Y~ group + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat2)

  #Example 2:
  cosinor.glmm(Y ~ X+amp.acro(time, n_components = 2, group = "X", period = c(12,10)), data = vitamind)
#Addressed all of the 'TODO' lists
#Ask about the time_col string thing




#Meeting 30/12/2022
  #Separated the cosinor.glmm function into data_utils.R and data_processor.R
    #We could also move the update_covnames.R and get_varnames.R functions elsewhere
  #If the user specifies no intercept term, no intercept term will be displayed*
  #If there are multiple levels to a factor, then ALL levels will be displayed in the output.
    #- Previously, this was not the case for the mesor output.
    #- This invovled changing the contrasts argument in glmmTMB. By default, it ignores the
    #  reference level for each factor. For this to work, the groups needed to be formatted
    #  as factors. I've implemented code that does this automatically, but we could also
    #  present an error message if the group columns in the dataframe are not factors
    #- *A consequence of this change is that now, intercept terms are displayed for each parameter
    #  and each component.

  #In this example, X has 3 levels, Z has 2 levels.
  dat$Z = rbinom(length(dat$X),1,prob = 0.5)
  cosinor.glmm(Y~ 0 + X + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
  cosinor.glmm(Y~ 0 + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)

  #Might need to address the warning message

#Meeting on 3/01/2023
  #Addressed the issues with the no-intercept implementation:
  dat$Z = rbinom(length(dat$X),1,prob = 0.5)
  cosinor.glmm(Y~ 0 + X + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
  cosinor.glmm(Y~ 0 + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)

  #In the cosinor.lm() package, the update_covnames() function would not work
  #if there is no intercept. I've since adjusted the code so that it is
  #as flexible as possible. This is reflected in the naming of the transformed
  #coefficients

  #A few other things to note:
    #There's a bug (or a feature?) in the glmmTMB function that involves rearranging
    #the order of interaction terms depending on whether that term has already
    #appeared in the formula. For example:
    cosinor.glmm(Y~ 0 + X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
    #note the way the interaction terms are arranged:
    cosinor.glmm(Y~ 0 + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)

    #In addition to giving the interaction term components, the model will give
    #estimates without any interaction terms (which I believe is an aggregate estimate
    #across the group terms...)

    #I'm not sure how to address the issue indicated by the warning message

#Meeting on 5/01/2023
    #Addressed the issues from the previous meeting
    data(vitamind)
    vitamind$Z = rbinom(length(vitamind$X),1,prob = 0.5)
    cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)

    #Added a verbose argument to cosinor.glmm()
    data(vitamind)
    vitamindmod <- vitamind
    vitamindmod$Y[2] <- NA
    cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamindmod)

    #Testing the predict function()
    data(vitamind)
    form = Y ~ X + amp.acro(time, group = "X")
    fit <- cosinor.glmm(form, data = vitamind)
    predict(fit)

    vitamind_no_response_v <- vitamind
    vitamind_no_response_v["Y"]<-NULL
    predict(fit, vitamind_no_response_v)

    #One issue: I implemented a test that the family argument is of class family,
    #but this doesn't work with the predict function:
      #stopifnot(inherits(family, "family"))   #in data_utils.R

    #Made progress towards the SRR standards

#Next meeting
    #Made progress towards SRR standards
    #summary.cosinor.glmm() now works, but needs to be validated further. Seems
    #to be a probelm with amp1?
    #Ex. 1
    object1 <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind, family = "poisson")
    ggplot.cosinor.glmm(object1, x_str = "X") #geom_ribbon for confidence interval plotting or dotted lines above and below (add as argument)
    summary.cosinor.glmm(object1)
    test_cosinor(object, x_str = "X", param = "amp", ref_level = 0, comp_level =1, component_index = 1)

    #Ex. 2
    data(vitamind)
    vitamind$Z = rbinom(length(vitamind$X),4,prob = 0.5)
    object2 <-    cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)
    ggplot.cosinor.glmm(object2, x_str = "Z")
    ggplot.cosinor.glmm(object2, x_str = c("X","Z"))
    summary.cosinor.glmm(object2)

    #issuw with this: need to address the inconsistent order of object2$coefficients
    test_cosinor(object2, x_str = "X", param = "amp")

    #Ex. 3
    comod = simulate_cosinor(1000,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 1, beta.acro = 0.3, dist = "2_component")
    object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
    ggplot.cosinor.glmm(object3, x_str = "group")
    summary.cosinor.glmm(object3)

    #added around 15 tests to the test-amp-acro.R function
    #Testing dispersion formula behaviour:

    data(vitamind)
    vitamind$Z = rbinom(length(vitamind$X),4,prob = 0.5)
    cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind,
                 dispformula = ~ X + amp.acro(time, n_components = 3, group = c("Z",NA,"X"), period = c(12,11,8)),
                 ziformula = ~ X + amp.acro(time, n_components = 2, group = c("Z","X"), period = c(12,8)))



##notes from meeting 14/01/2023
#Use: usethis::use_package("ellipse")
#Use: check() and make sure there are no warnings or errors
