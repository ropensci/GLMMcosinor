# # working out space
# comod = simulate_cosinor(1000,mesor = 50,amp = 7,acro = 1,beta.mesor = 0.1,beta.amp = 0.2, beta.acro = 0.3, family = "2_component")
# m <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 12)), data = comod)
# comod$pred <- predict(m$fit, type="response")
#
# ggplot() +
#   geom_point(data=comod, aes(times, Y, col=as.factor(group)), alpha=0.1) +
#   geom_line(data=comod, aes(times, pred, col=as.factor(group)))
# # test when you put a browser() in at the simulate data stage with multicomponent (above)
# data.frame(l1=lambda1, l2=lambda2, y=y, time=ttt) %>%
#   mutate(lsum = l1+l2) %>%
#   pivot_longer(!time) %>%
#   ggplot(aes(time, value, col=name)) +
#   geom_point()
#
#
# #Example showing how cosinor.glmm works with a group of multiple levels
# newgroup <-
#   vitamind %>%
#   filter(X==1) %>%
#   mutate(X=2, time = time + 4)
#
# dat <- rbind(vitamind, newgroup) %>%
#   mutate(X = as.factor(X))
#
# dat %>%
#   ggplot(aes(time, Y, col=X)) + geom_point()
#
# m <- cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2, group = "X", period = c(12,10)), data = dat)
#
# # Examples for meeting: 15/12/2022
# # Evidence that the multi-component estimation works. In this example, amp2 = amp1 + 3, acr2 = acr1-1.
# comod = simulate_cosinor(100,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 0.2, beta.acro = 0.3, family = "2_component")
# cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
# #
# # You can now have different groups assigned to different components. For example:
# vitamind$Z = rbinom(length(vitamind$X),1,prob = 0.5)
# cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)
#
# dat2 = dat
# dat2$Z = (rbinom(length(dat2$X),5,prob = 0.5))
# cosinor.glmm(Y~ X + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat2)
#
#
# #Meeting for 19/12/2022
# #Added comments to the cosinor.glmm script for everything except the update_covnames function
#   #Do we need the update_covnames? Currently, it is not being used in the script.
# #Added the ability to have groups be factors with more than 2 levels.Example:
# cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2, group = "X", period = c(12,10)), data = dat)
# #Added some error messages. For example, if n_components is not an integer, or it is less than 1,
# #an error will appear with a relevant message. Examples:
# cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2.1, group = "X", period = c(12,10)), data = dat)
# cosinor.glmm(Y ~ group+amp.acro(time, n_components = 0, group = "X", period = c(12,10)), data = dat)
# #Changed some variable names to be more descriptive and informative.
# #If for whatever reason the user names their grouping variables 'rrr' or 'sss', there will be
# #problems when getting the terms for the formula. Should we add a simple error message if the user
# #uses these names for their groups? The code in question is this:
#   r.coef[[i]] <- !is.na(str_extract(names(coefs),vec_rrr[i]))
#   #Str_extract will search in the coef names for anything with rrr[number] in it, and
#   #return NA for everything else. Overall, what you get is a Boolean truth vector with TRUE for everything
#   #containing rrr[number]. For example if the number (i) = 1, the script will get rrr1, X:rrr1, Z:rrr1 and so
#   #on. Same for s.coef[[i]].
#
#
# #Next meeting
# #Code now works when group argument is not specified:
# cosinor.glmm(Y ~ amp.acro(time, n_components = 2, period = c(12,10)), data = dat)
# #If you name your grouping variable anything with 'rrr' or 'sss', an error will appear
# #with relevant information.
# cosinor.glmm(Y ~ group+amp.acro(time, n_components = 2, group = "rrr", period = c(12,10)), data = dat)
#
# #Updated the update.covnames function so that now it prints the group factors in a nicer way.
#   #Example 1:
#   dat2 = dat
#   dat2$Z = as.factor(rbinom(length(dat$X),5,prob = 0.5)+2)
#   cosinor.glmm(Y~ group + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat2)
#
#   #Example 2:
#   cosinor.glmm(Y ~ X+amp.acro(time, n_components = 2, group = "X", period = c(12,10)), data = vitamind)
# #Addressed all of the 'TODO' lists
# #Ask about the time_col string thing
#
#
#
#
# #Meeting 30/12/2022
#   #Separated the cosinor.glmm function into data_utils.R and data_processor.R
#     #We could also move the update_covnames.R and get_varnames.R functions elsewhere
#   #If the user specifies no intercept term, no intercept term will be displayed*
#   #If there are multiple levels to a factor, then ALL levels will be displayed in the output.
#     #- Previously, this was not the case for the mesor output.
#     #- This invovled changing the contrasts argument in glmmTMB. By default, it ignores the
#     #  reference level for each factor. For this to work, the groups needed to be formatted
#     #  as factors. I've implemented code that does this automatically, but we could also
#     #  present an error message if the group columns in the dataframe are not factors
#     #- *A consequence of this change is that now, intercept terms are displayed for each parameter
#     #  and each component.
#
#   #In this example, X has 3 levels, Z has 2 levels.
#   dat$Z = rbinom(length(dat$X),1,prob = 0.5)
#   cosinor.glmm(Y~ 0 + X + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
#   cosinor.glmm(Y~ 0 + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
#
#   #Might need to address the warning message
#
# #Meeting on 3/01/2023
#   #Addressed the issues with the no-intercept implementation:
#   dat$Z = rbinom(length(dat$X),1,prob = 0.5)
#   cosinor.glmm(Y~ 0 + X + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
#   cosinor.glmm(Y~ 0 + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
#
#   #In the cosinor.lm() package, the update_covnames() function would not work
#   #if there is no intercept. I've since adjusted the code so that it is
#   #as flexible as possible. This is reflected in the naming of the transformed
#   #coefficients
#
#   #A few other things to note:
#     #There's a bug (or a feature?) in the glmmTMB function that involves rearranging
#     #the order of interaction terms depending on whether that term has already
#     #appeared in the formula. For example:
#     cosinor.glmm(Y~ 0 + X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
#     #note the way the interaction terms are arranged:
#     cosinor.glmm(Y~ 0 + Z + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = dat)
#
#     #In addition to giving the interaction term components, the model will give
#     #estimates without any interaction terms (which I believe is an aggregate estimate
#     #across the group terms...)
#
#     #I'm not sure how to address the issue indicated by the warning message
#
# #Meeting on 5/01/2023
#     #Addressed the issues from the previous meeting
#     data(vitamind)
#     vitamind$Z = rbinom(length(vitamind$X),1,prob = 0.5)
#     cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)
#
#     #Added a verbose argument to cosinor.glmm()
#     data(vitamind)
#     vitamindmod <- vitamind
#     vitamindmod$Y[2] <- NA
#     cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamindmod)
#
#     #Testing the predict function()
#     data(vitamind)
#     form = Y ~ X + amp.acro(time, group = "X")
#     fit <- cosinor.glmm(form, data = vitamind)
#     predict(fit)
#
#     vitamind_no_response_v <- vitamind
#     vitamind_no_response_v["Y"]<-NULL
#     predict(fit, vitamind_no_response_v)
#
#     #One issue: I implemented a test that the family argument is of class family,
#     #but this doesn't work with the predict function:
#       #stopifnot(inherits(family, "family"))   #in data_utils.R
#
#     #Made progress towards the SRR standards
#
# #Next meeting
#     #Made progress towards SRR standards
#     #summary.cosinor.glmm() now works, but needs to be validated further. Seems
#     #to be a probelm with amp1?
#     #Ex. 1
#     object1 <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
#     ggplot.cosinor.glmm(object1, x_str = "X") #geom_ribbon for confidence interval plotting or dotted lines above and below (add as argument)
#     summary.cosinor.glmm(object1)
#     test_cosinor(object1, x_str = "X", param = "amp", ref_level = 0, comp_level =1, component_index = 1)
#
#     #Ex. 2
#     data(vitamind)
#     vitamind$Z = rbinom(length(vitamind$X),4,prob = 0.5)
#     object2 <-    cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)
#     ggplot.cosinor.glmm(object2, x_str = "Z")
#     ggplot.cosinor.glmm(object2, x_str = c("X","Z"))
#     summary.cosinor.glmm(object2)
#
#
#     #issuw with this: need to address the inconsistent order of object2$coefficients
#     test_cosinor(object2, x_str = "X", param = "amp")
#
#     #Ex. 3
#     comod = simulate_cosinor(100,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 1, beta.acro = 0.3, family = "2_component")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#     ggplot.cosinor.glmm(object3, x_str = "group")
#     summary.cosinor.glmm(object3)
#
#     #added around 15 tests to the test-amp-acro.R function
#     #Testing dispersion formula behaviour:
#
    data(vitamind)
    vitamind$Z = rbinom(length(vitamind$X),4,prob = 0.5)
    cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind,
                 dispformula = ~ X + amp.acro(time, n_components = 3, group = c("Z",NA,"X"), period = c(12,11,8)),
                 ziformula = ~ X + amp.acro(time, n_components = 2, group = c("Z","X"), period = c(12,8)))

#
#
# ##notes from meeting 14/01/2023
# #Use: usethis::use_package("ellipse")
# #Use: check() and make sure there are no warnings or errors
# #Testing out the updated plot function:
#     comod = simulate_cosinor(1000,mesor = 1,amp = 2,acro = 2, beta.mesor = 0.5,beta.amp = 1, beta.acro = 0.3, family = "2_component")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#     summary.cosinor.glmm(object3)
#     ggplot.cosinor.glmm(object3, x_str = "group", transpose_data = TRUE)
#
#     comod = simulate_cosinor(1000,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 1, beta.acro = 0.3, family = "poisson")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 1, group = "group", period = 12), data = comod, family = "poisson")
#     ggplot.cosinor.glmm(object3, x_str = "group", transpose_data = TRUE)
#
#     comod = simulate_cosinor(1000,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 1, beta.acro = 0.3, family = "gamma")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 1, group = "group", period = 12), data = comod, family = "Gamma"(link = "log"))
#     ggplot.cosinor.glmm(object3, x_str = "group", transpose_data = TRUE, data_opacity = 0.1)
#
#
#
   #polar plots in progress: (proof of concept)
   object1 <- cosinor.glmm(Y ~ 0 + amp.acro(time), data = vitamind)
   summary.cosinor.glmm(object1)
   #ggplot.cosinor.glmm.polar(object1)
   test_cosinor(object1, x_str = "X")

   object1 <- cosinor.glmm(Y ~ 1 + X +amp.acro(time, group = "X"), data = vitamind)
   summary.cosinor.glmm(object1)
   ggplot.cosinor.glmm.polar(object1)
   test_cosinor(object1, x_str = "X")
#
#
     data(vitamind)
     vitamind$Z = rbinom(length(vitamind$X),3,prob = 0.5)
     object2 <-    cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("Z",NA,"X"), period = c(12,10,8)),data = vitamind)
     ggplot.cosinor.glmm.polar(object2)

#
#     comod = simulate_cosinor(100,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.5,beta.amp = 1, beta.acro = 0.3, family = "2_component")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#     ggplot.cosinor.glmm.polar(object3)
#
#     #currently, the component specification doesn't correspond nicely if the user specifies NA
#     #it would be beneficial to plot components even if they have no group-assignment
#       #if group = NA, then do the same thing but without the multiple levels etc
#
#
#     #UPDATE: polar plots are much more refined now
#     comod = simulate_cosinor(500,mesor = 1,amp = 2,acro = -1.5,beta.mesor = 0.5,beta.amp = 1, beta.acro = -1, family = "2_component")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#     ggplot.cosinor.glmm(object3, x_str = "group", superimpose.data = TRUE, data_opacity = 0.5)
#     ggplot.cosinor.glmm.polar(object3, x_str = "group")
#
#     comod = simulate_cosinor(50,mesor = 1,amp = 2,acro = 0.5,beta.mesor = 1,beta.amp = 2, beta.acro = 0.4, family = "poisson")
#     object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 1, group = "group", period = 12), data = comod, family = "poisson")
#     ggplot.cosinor.glmm(object3, x_str = "group", superimpose.data = TRUE)
#     ggplot.cosinor.glmm.polar(object3, x_str = "group", grid_angle_segments = 12)
#
#
#
# ##Updated  polar plots
# #add degrees symbol to polar plots (DONE)
# #add zoom independent of show_polar_grid argument: 'focus_quadrant' to zoom in on quadrant (DONE)
# #remove show_polar_grid as an argument (DONE)
# #add top,left,bottom,right as arguments to dictate where plot starts, and keep clockwise but make it just affect the direction (DONE)
# #let the user specify the level too if component_index is specified, and cowplot is FALSE
# #if component index supplied but cowplot TRUE, override cowplot to be FALSE (DONE)
# #allow user to specify confidence level
# # get rid of the sss and rrr and axes labels - remove ticks and grid, use theme_bw() (DONE)
# # add hjust and vjust to the labels of the outer ring as vector corresponding to length rather than multipliers
# #codecov
# #vdiffr - check out this package to see tests for plots
# #check out the error when restarting R and loading (error in ggplot ...)



#Meeting for Wednesday Morning:
#Significant improvements to polar plots:
#comod = simulate_cosinor(50,mesor = 0.4,amp = 3,acro = 1.2,beta.mesor = 1,beta.amp = 2, beta.acro = 2, family = "2_component")
#object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#ggplot.cosinor.glmm.polar(object3,
#                          contour_interval = 1,
#                          make_cowplot = TRUE,
#                          component_index = 1,
#                          grid_angle_segments = 8,
#                          radial_units = "radians",
#                          clockwise = TRUE,
#                          text_size = 3,
#                          text_opacity = 0.5,
#                          fill_colours = c("red" ,"green", "blue", "purple", "pink", "yellow", "orange", "black"),
#                          ellipse_opacity = 0.3,
#                          circle_linetype = "dotted",
#                          start = "bottom",
#                          view = "zoom_origin",
#                          overlay_parameter_info = FALSE,
#                          quietly = TRUE)

#Added around 25 tests (specifically for the plot script)
#The package now works when there is no group assigned (ie, analysis of a single group)

#summary.cosinor.glmm() now deals with dispersion formula and ziformula appropriately
  #Ex. 1:
comod = simulate_cosinor(500,mesor = 0.4,amp = 3,acro = 1.2,beta.mesor = 1,beta.amp = 2, beta.acro = 2, family = "2_component")
object3 <- cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod,
                        dispformula = ~ group + amp.acro(times, n_components = 2, group = "group", period = c(12, 8)),
                        ziformula = ~ group + amp.acro(times, n_components = 2, group = "group", period = c(12, 8)))
summary.cosinor.glmm(object3)
#Ex. 2: notice how because of the non-convergence warning, NaN is produced in the summary. This is normal behaviour (according to a post I read)
object4 <- cosinor.glmm(Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12), data = vitamind,
                        dispformula = ~ X + amp.acro(time, n_components= 1, group = "X", period = 12),
                        ziformula = ~ X + amp.acro(time, n_components= 1, group = "X", period = 12))
summary.cosinor.glmm(object4)

#The p-values currently aren't comparing to the reference group. They are comparing to 0 (ie, the parameter = 0)
#that's why the p-values for the estimates are 0. Seems like an easy fix though



#TODO: make sure acrophase units match radial units (DONE)
#TODO: make sure zoom_origin maintains eqaul x and y scales (avoid familyortion) (DONE)
#TODO: use vdiffr to compare image output see https://github.com/RWParsons/predictNMB/blob/master/tests/testthat/test-plot.R
#TODO: if group_level = NULL, set fill and colour to grey outside of main aesthetic (will have to create a separate line)



#The simulate_cosinor() function is now much more robust. YOu can specify any number of components now,
testdata = simulate_cosinor(1000,
                            mesor = c(1,2,4),
                            amp = c(0.1,0.4,0.5),
                            acro = c(1,1.5, 0.1),
                            beta.mesor = c(0.4,1, 3),
                            beta.amp = c(2,1, 0.4),
                            beta.acro = c(1,-1.5, -1),
                            family = "gamma",
                            period = c(12,6,8),
                            n_components = 3,
                            alpha = 5)
object = cosinor.glmm(Y ~ group + amp.acro(times, n_components = 3, period = c(12,6, 8), group = 'group'), data = testdata, family = Gamma(link = "log"))
ggplot.cosinor.glmm(object, superimpose.data = TRUE, x_str = "group", predict.ribbon = FALSE)

testdata = simulate_cosinor(1000,
                            mesor = c(1,2,4),
                            amp = c(2,1,0.5),
                            acro = c(1,1.5, 0.1),
                            beta.mesor = c(1,2, 3),
                            beta.amp = c(2,1, 0.4),
                            beta.acro = c(1,1.5, -1),
                            family = "binomial",
                            period = c(12,6,8),
                            n_components = 3)
object = cosinor.glmm(Y ~ group + amp.acro(times, n_components = 3, period = c(12,6, 8), group = 'group'), data = testdata, family = binomial())
ggplot.cosinor.glmm(object, superimpose.data = TRUE, x_str = "group", predict.ribbon = FALSE)

testdata = simulate_cosinor(1000,
                            mesor = 4,
                            amp = c(1,0.4,0.5),
                            acro = c(1,1.5, 0.1),
                            beta.mesor = 3,
                            beta.amp = c(1,1, 0.4),
                            beta.acro = c(1,-1.5, -1),
                            family = "gaussian",
                            period = c(12,6,8),
                            n_components = 3,
                            sd = 1)
object = cosinor.glmm(Y ~ group + amp.acro(times, n_components = 3, period = c(12,6, 8), group = 'group'), data = testdata, family = gaussian())
ggplot.cosinor.glmm(object, superimpose.data = TRUE, x_str = "group", predict.ribbon = FALSE)
ggplot.cosinor.glmm.polar(object)

testdata = simulate_cosinor(20000,
                            mesor = 7,
                            amp = c(0.1,0.4,0.5),
                            acro = c(1,1.5, 0.1),
                            beta.mesor = 4.4,
                            beta.amp = c(2,1, 0.4),
                            beta.acro = c(1,-1.5, -1),
                            family = "gamma",
                            period = c(12,8,9),
                            n_components = 3)
object = cosinor.glmm(Y ~ group + amp.acro(times,
                                           n_components = 3,
                                           period = c(12,8,9), group = 'group'
                                           ),data = testdata, family = Gamma(link = "log"))
plot.cosinor.glmm(object, superimpose.data = TRUE, x_str = "group", predict.ribbon = FALSE)
polar_plot.cosinor.glmm(object, view = "full")


#Testing test_cosinor
testdata = simulate_cosinor(100,
                            mesor = 4,
                            amp = c(2,3),
                            acro = c(0.4,0.4),
                            beta.mesor = 4,
                            beta.amp = c(4,3),
                            beta.acro = c(2, 3),
                            family = "gaussian",
                            period = c(10,12),
                            n_components =2,
                            sd = 1)
object = cosinor.glmm(Y ~ group + amp.acro(times,n_components = 2, period = c(10,12), group = 'group'), data = testdata, family = gaussian())
test_cosinor(object, x_str ="group", param = "acr")
plot.cosinor.glmm(object, predict.ribbon = FALSE, superimpose.data = TRUE)

#Can now compare components for a given level
test_cosinor(object, x_str ="group", param = "amp", comparison_A = 1, comparison_B = 2, comparison_type = "components", level_index = 0)

#Or, can compare levels for a given component
test_cosinor(object, x_str ="group", param = "amp", comparison_A = 0, comparison_B = 1, comparison_type = "levels", component_index = 1)


#Links from meeting 1/02/2023
#Create a feature-table:
#https://link.springer.com/article/10.1186/s12859-020-03830-w
#https://www.sciencedirect.com/science/article/pii/S1877750322001429
#https://arxiv.org/pdf/2105.10168.pdf
#https://link.springer.com/article/10.1007/s10928-021-09748-x

#pkgcheck::pkgcheck
#goodpractice::goodpractice
#Change the 1:x to 1:seq_along()
#Add some more tests


#Opensci websites and other resources from meeting 8/02/2023
#https://www.biorxiv.org/content/10.1101/2023.02.02.526897v1.full.pdf
#https://docs.ropensci.org/dynamite/
#https://github.com/ropensci/software-review

#For creating the feature-table in the Vignette
#flextable::flextable()
#https://stackoverflow.com/questions/65925855/placing-literature-references-in-a-table-in-rmd
#Add .bib file for references (and add in text-tag)
#Or, in markdown: [link text here](https://link...)

#To add to feature table: Parameters, and difference between groups,
#exponential decay term to parameters, mixed models (feature table)
#Move get_new_coefs to data_utils as a function
#Rework mixed model specification (check out: https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf)

#Meeting 16/03/2023: TODO
#Add package version as a column
#exponential decay term to parameters, mixed models (feature table)
#Rework mixed model specification (check out: https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf)
#SRR standards



#Meeting 30/03/2023
#Add https://tbiomed.biomedcentral.com/articles/10.1186/1742-4682-11-16 citation in readme and vignette
#
#> obj1 <- cosinor.glmm(Y ~ X + amp.acro(time,
#                                        +                               n_components = 3,
#                                        +                               group = "X",
#                                        +                               period = c(12, 9, 10)
#                                        + ), data = vitamind, randef =  .~. + (1|X))
