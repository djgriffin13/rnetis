actors <- plot_data(fit, what = "actor_effects")
head(actors[order(-actors$receiver), c("node", "sender", "receiver")])
