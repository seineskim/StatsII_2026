## Replication file: Liu, Wang, Xu (2022)
# Plot simulation results

library(grid)
library(gridExtra)
library(ggplot2)

##################################################################
## Comparing Power of F and Equivalence Tests
##################################################################

## N = 300
load("results/sim_tests_n300.RData")
f_size_seq <- c(seq(0, 0.3, by = 0.05), seq(0.4, 0.8, by = 0.1))
bias <- abs(apply(sim.att.avg, 2, mean, na.rm = TRUE))
res.sd <- apply(sim.res.sd, 2, mean, na.rm = TRUE)
bias <- bias / res.sd 
F <- apply(sim.F.p >= 0.05, 2, mean, na.rm = TRUE)
equiv <- apply(sim.equiv.p < 0.05, 2, mean, na.rm = TRUE)
d <- cbind.data.frame(f_size_seq, bias, F, equiv)
pdf('graph/sim_tests_n300.pdf', width = 6, height = 6)
par(mar = c(4,4,1,1))
plot(1, type = "n",  xlim = c(0,1.2), ylim= c(0,1),  cex.lab = 1.3,
  xlab = "Bias / SD(Res. Y)", ylab= "Proportion Declared Balance", main = "")
lines(d$bias, d$F, type = "b", col = "gray30", cex = 0.7, lwd = 1.5, lty = 2)
points(d$bias, d$F, pch = 15, col = "gray30", cex = 0.9)
lines(d$bias, d$equiv, col = 1, type = "b", cex = 0.8)
points(d$bias, d$equiv, pch = 16, cex = 0.9)
text(1.02,0.47,"TOST Test", cex = 1.2)
text(0.4,0.47,"F Test", cex = 1.2)
graphics.off()

## N = 100

load("results/sim_tests_n100.RData")
f_size_seq <- c(seq(0, 0.3, by = 0.05), seq(0.4, 0.8, by = 0.1))
bias <- abs(apply(sim.att.avg, 2, mean, na.rm = TRUE))
res.sd <- apply(sim.res.sd, 2, mean, na.rm = TRUE)
bias <- bias / res.sd 
F <- apply(sim.F.p >= 0.05, 2, mean, na.rm = TRUE)
equiv <- apply(sim.equiv.p < 0.05, 2, mean, na.rm = TRUE)
d <- cbind.data.frame(f_size_seq, bias, F, equiv)
pdf('graph/sim_tests_n100.pdf', width = 6, height = 6)
par(mar = c(4,4,1,1))
plot(1, type = "n",  xlim = c(0,1.2), ylim= c(0,1),  cex.lab = 1.3,
  xlab = "Bias / SD(Res. Y)", ylab= "Proportion Declared Balance", main = "")
lines(d$bias, d$F, type = "b", col = "gray30", cex = 0.7, lwd = 1.5, lty = 2)
points(d$bias, d$F, pch = 15, col = "gray30", cex = 0.9)
lines(d$bias, d$equiv, col = 1, type = "b", cex = 0.8)
points(d$bias, d$equiv, pch = 16, cex = 0.9)
text(0.25,0.47,"TOST Test", cex = 1.2)
text(0.85,0.47,"F Test", cex = 1.2)
graphics.off()


##################################################################
## Comparing estimators: IFE vs MC
##################################################################

load("results/sim_ife_mc.RData")

## data
mean_mse_ife <- apply(ife_mse, 2, mean, na.rm = TRUE)
mean_mse_ifeo <- apply(ifeo_mse, 2, mean, na.rm = TRUE)
mean_mse_mc <- apply(mc_mse, 2, mean, na.rm = TRUE)
(mspe <- c(mean_mse_ifeo, mean_mse_ife, mean_mse_mc))

## parameters
library(RColorBrewer)
mycol<-brewer.pal(2,"Set1")
set.labels = c("IFEct (known r)", "IFEct (unknown r)", "MC")
set.colors = c("black", "gray40", "red")
set.linetypes = c("solid","dotdash","longdash")
set.linewidth = rep(1,3)
font.size <- 5
y1 <- c(9:1)/9*2.5
data <- cbind.data.frame(mspe, rep(1:9, 3), rep(set.labels, each = 9), 
  c(1:9)-0.3, c(1:9)+0.3, rep(0,27), rep(y1, 3))
names(data) <- c("per", "r", "type", "x0", "x1", "y0","y1")
data

## ggplot
ylim <- c(0,6.5)
p <- ggplot(data) + xlab("Number of Factors in the DGP") +  
	ylab("MSPE") + coord_cartesian(ylim = ylim) +
	geom_line(aes(r, per, colour = type, linetype = type, size = type)) 
p <- p + scale_x_continuous(breaks = 1:9) + scale_y_continuous()
p <- p + geom_rect(aes(xmin=x0, xmax=x1, ymin=y0, ymax=y1), alpha=0.1)
p <- p + theme(legend.position = "none", text = element_text(size=15))
p <- p + scale_colour_manual(values =set.colors)
p <- p + scale_linetype_manual(values = set.linetypes) 
p <- p + scale_size_manual(values = set.linewidth)
p <- p + annotate("text", x = 7.5, y = 2, label = "IFEct (known r)", size = font.size)
p <- p + annotate("text", x = 7.1, y = 5.5, label = "IFEct (CV-ed r)", size = font.size)
p <- p + annotate("text", x = 3, y = 3.5, label = "MC", size = font.size, colour = "red")
p <- p + annotate("text", x = 5, y = 0.5, label = "Variance of each factor", size = font.size, colour = "gray20")
p <- p + theme_bw() + theme(legend.position = "none", 
	axis.text = element_text(size = 15), axis.title = element_text(size = 18)) 
p
ggsave('graph/sim_compare.pdf', p, width = 8, height = 6)

##################################################################
## QQ plot for inferential methods
##################################################################


## N = 50
load("results/qqplots_N50.RData")

pdata_lmboot <- as.data.frame(pdata_lmboot)
names(pdata_lmboot) <- "y"

pdata_boot <- as.data.frame(pdata_boot)
names(pdata_boot) <- "y"

pdata_jack <- as.data.frame(pdata_jack)
names(pdata_jack) <- "y"

p_boot_lm <- ggplot(pdata_lmboot, aes(sample = y), distribution = stats::qnorm)  + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray") + stat_qq() + 
  ylim(c(-4, 4)) + xlim(c(-4, 4)) + theme_bw() + xlab("Theoretical") + ylab("Sample") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  annotate("text", x = 0, y = 3.5, label = 'atop(bold("Twoway FE"),"(bootstrap)")', hjust = 0.5, size = 6, parse = TRUE)

p_boot <- ggplot(pdata_boot, aes(sample = y), distribution = stats::qnorm) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray") + stat_qq() + 
  ylim(c(-4, 4)) + xlim(c(-4, 4)) + theme_bw() + xlab("Theoretical") + ylab("Sample") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  annotate("text", x = 0, y = 3.5, label = 'atop(bold("FEct"),"(bootstrap)")', hjust = 0.5, size = 6, parse = TRUE)

p_jack <- ggplot(pdata_jack, aes(sample = y), distribution = stats::qnorm) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") + stat_qq() + 
  ylim(c(-4, 4)) + xlim(c(-4, 4)) + theme_bw() + xlab("Theoretical") + ylab("Sample") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  annotate("text", x = 0, y = 3.5, label = 'atop(bold("FEct"),"(jackknife)")', hjust = 0.5, size = 6, parse = TRUE)

margin = theme(plot.margin = unit(c(1,1,1,1), "line"))
p <- grid.arrange(grobs = lapply(list(p_boot_lm, p_boot, p_jack), "+", margin),
  ncol = 3, widths = c (1, 1, 1))
ggsave('graph/sim_infer_n50.pdf', p, width = 17, height = 6)


## N = 100
load("results/qqplots_N100.RData")

pdata_lmboot <- as.data.frame(pdata_lmboot)
names(pdata_lmboot) <- "y"

pdata_boot <- as.data.frame(pdata_boot)
names(pdata_boot) <- "y"

pdata_jack <- as.data.frame(pdata_jack)
names(pdata_jack) <- "y"


p_boot_lm <- ggplot(pdata_lmboot, aes(sample = y), distribution = stats::qnorm)  + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray") + stat_qq() + 
  ylim(c(-4, 4)) + xlim(c(-4, 4)) + theme_bw() + xlab("Theoretical") + ylab("Sample") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  annotate("text", x = 0, y = 3.5, label = 'atop(bold("Twoway FE"),"(bootstrap)")', hjust = 0.5, size = 6, parse = TRUE)

p_boot <- ggplot(pdata_boot, aes(sample = y), distribution = stats::qnorm) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray") + stat_qq() + 
  ylim(c(-4, 4)) + xlim(c(-4, 4)) + theme_bw() + xlab("Theoretical") + ylab("Sample") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  annotate("text", x = 0, y = 3.5, label = 'atop(bold("FEct"),"(bootstrap)")', hjust = 0.5, size = 6, parse = TRUE)

p_jack <- ggplot(pdata_jack, aes(sample = y), distribution = stats::qnorm) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") + stat_qq() + 
  ylim(c(-4, 4)) + xlim(c(-4, 4)) + theme_bw() + xlab("Theoretical") + ylab("Sample") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16)) +
  annotate("text", x = 0, y = 3.5, label = 'atop(bold("FEct"),"(jackknife)")', hjust = 0.5, size = 6, parse = TRUE)

margin = theme(plot.margin = unit(c(1,1,1,1), "line"))
p <- grid.arrange(grobs = lapply(list(p_boot_lm, p_boot, p_jack), "+", margin),
  ncol = 3, widths = c (1, 1, 1))
ggsave('graph/sim_infer_n100.pdf', p, width = 17, height = 6)




