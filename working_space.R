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

m <- cosinor.glmm(Y ~ group+amp.acro(time, n_components = 1, group = "X"), data = dat)

# Examples for meeting: 15/12/2022
# Evidence that the multi-component estimation works. In this example, amp2 = amp1 + 2, acr2 = acr1-1.
# comod = simulate_cosinor(1000,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.1,beta.amp = 0.2, beta.acro = 0.3, dist = "2_component")
# cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#
# You can now have different groups assigned to different components. For example:
# vitamind$Z = rbinom(length(vitamind$X),1,prob = 0.5)
# cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)
