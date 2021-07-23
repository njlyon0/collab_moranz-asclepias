
### Is the number of shrubs within 1 meter *affected by* TSF/Stocking?
# Distribution
multi.hist(mkwd$Shrub.Abun.1m)

# Test
summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
## Stocking = marginal (IES ≠ SLS p = 0.0501) <- basically sig

summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
## NS



### Total bitten stems
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
## Stocking = sig (IES ≠ SLS)

summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
## NS

### Ratio bitten vs. total stems
summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
## Stocking = sig (IES ≠ None/SLS)

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))
## Stocking = sig (None ≠ IES)

### Average height of three longest stems
summary(glmmTMB(Avg.Height ~  TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = gaussian()))
## Stocking = sig (IES ≠ None)

summary(glmmTMB(Avg.Height ~  TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
## Stocking = sig & marginal (None vs. IES p = 0.016 | None vs. SLS p = 0.0504) <- basically sig
## Ixn = marginal (TSF*SLS p = 0.057)

### Total axillary shoots
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
## Stocking = sig (IES ≠ None/SLS)

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
## Stocking = sig & marginal (None vs. SLS p = 0.077 | None vs. IES p = 0.010)
## Stocking * Shrubs ixn = marginal! (SLS*Shrubs p = 0.072)

### Finally, do **shrubs alone** affect those characteristics?
