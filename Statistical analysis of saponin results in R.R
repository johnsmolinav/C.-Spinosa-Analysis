#my analysis

####data
  {
  sample   <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12")
  altitude <- c(2024, 2080, 1992, 2222, 2559, 2101, 2381, 2740, 2697, 2644, 2230, 2744)
  # Husk (cáscara)
  husk_2min  <- c(12,  9, 10,  6,  7, 10,  6, 15, 20,  5,  1,  5)
  husk_10min <- c( 7,  5,  5,  4,  4,  7,  2, 11,  4,  2,  0,  4)
  # Seed (semilla)
  seed_2min  <- c( 1,  0,  1,  1,  6,  1, 14,  4,  1,  5,  4,  3)
  seed_10min <- c( 0,  0,  0,  0,  6,  0, 13,  2,  1,  4,  3,  2)
  df <- data.frame(sample, altitude, husk_2min, husk_10min, seed_2min, seed_10min)
  print(df)
  }
  str(df)#type of data from data.frame
  
####ANOVA
  #An analysis of variance "ANOVA" is not logical with only two variables that are not categorical; it is the same as linear regression.
  # However, it is included for reader analysis
  mod1<-aov(husk_2min ~ altitude, data = df) 
  summary (mod1)
  mod3<-aov(husk_10min ~ altitude, data = df) 
  summary (mod3)
  mod5<-aov(seed_2min ~ altitude, data = df)
  summary (mod5)
  mod7<-aov(seed_10min ~ altitude, data = df)
  summary (mod7)
  

###Linear regression (foam height as response, altitude as predictor)
  mod2 <- lm(husk_2min~altitude, data = df)
  summary(mod2)
  mod4 <- lm(husk_10min~altitude, data = df)
  summary(mod4)
  mod6 <- lm(seed_2min~altitude, data = df)
  summary(mod6)
  mod8 <- lm(seed_10min~altitude, data = df)
  summary(mod8)

###Spearman correlation (rank-based, detects monotonic trends)
  #The warning message appears because there are repeated values. 
  #It's not an error; it simply means that R uses an approximation instead of the exact p. The result is still valid.
  sp2 <- cor.test(df$altitude, df$husk_2min,  method = "spearman")
  sp2
  sp4 <- cor.test(df$altitude, df$husk_10min, method = "spearman")
  sp4
  sp6 <- cor.test(df$altitude, df$seed_2min,  method = "spearman")
  sp6
  sp8 <- cor.test(df$altitude, df$seed_10min, method = "spearman")
  sp8
  {
  rho_2 <- round(sp2$estimate, 3)
  rho_4 <- round(sp4$estimate, 3)
  rho_6 <- round(sp6$estimate, 3)
  rho_8 <- round(sp8$estimate, 3)
  }
  
#Plots
  {
  par(mfrow = c(2,2))
  
  #husk 2 min
  plot(husk_2min~altitude, data = df, pch=19, main="Husk 2 min",
       xlab="Altitude (m)", ylab="Foam height (mm)")
  text(df$altitude, df$husk_2min, labels=df$sample, pos=3, cex=0.7)
  abline(mod2, col="red", lwd=0.5)
  r2_2 <- round(summary(mod2)$r.squared, 3)
  p_2  <- round(summary(mod2)$coefficients[2,4], 4)
  b0_2 <- round(coef(mod2)[1], 3)
  b1_2 <- round(coef(mod2)[2], 5)
  eq_2 <- paste("y =", b0_2, "+", b1_2, "x")
  legend("topleft", bty="n", cex=0.8,
         legend=c(eq_2, paste("Pearson R² =", r2_2), paste("p=", p_2), paste("Spearman ρ=", rho_2)))
  
  #husk 10 min
  plot(husk_10min~altitude, data = df, pch=19, main="Husk 10 min",
       xlab="Altitude (m)", ylab="Foam height (mm)")
  text(df$altitude, df$husk_10min, labels=df$sample, pos=3, cex=0.7)
  abline(mod4, col="red", lwd=0.5)
  r2_4 <- round(summary(mod4)$r.squared, 3)
  p_4  <- round(summary(mod4)$coefficients[2,4], 4)
  b0_4 <- round(coef(mod4)[1], 3)
  b1_4 <- round(coef(mod4)[2], 5)
  eq_4 <- paste("y =", b0_4, "+", b1_4, "x")
  legend("topleft", bty="n", cex=0.8,
         legend=c(eq_4, paste("Pearson R² =", r2_4), paste("p =", p_4), paste("Spearman ρ =", rho_4)))
  
  #seed 2 min
  plot(seed_2min~altitude, data = df, pch=19, main="Seed 2 min",
       xlab="Altitude (m)", ylab="Foam height (mm)")
  text(df$altitude, df$seed_2min, labels=df$sample, pos=3, cex=0.7)
  abline(mod6, col="red", lwd=0.5)
  r2_6 <- round(summary(mod6)$r.squared, 3)
  p_6  <- round(summary(mod6)$coefficients[2,4], 4)
  b0_6 <- round(coef(mod6)[1], 3)
  b1_6 <- round(coef(mod6)[2], 5)
  eq_6 <- paste("y =", b0_6, "+", b1_6, "x")
  legend("topleft", bty="n", cex=0.8,
         legend=c(eq_6, paste("Pearson R² =", r2_6), paste("p =", p_6), paste("Spearman ρ =", rho_6)))
  
  #seed 10 min
  plot(seed_10min~altitude, data = df, pch=19, main="Seed 10 min",
       xlab="Altitude (m)", ylab="Foam height (mm)")
  text(df$altitude, df$seed_10min, labels=df$sample, pos=3, cex=0.7)
  abline(mod8, col="red", lwd=0.5)
  r2_8 <- round(summary(mod8)$r.squared, 3)
  p_8  <- round(summary(mod8)$coefficients[2,4], 4)
  b0_8 <- round(coef(mod8)[1], 3)
  b1_8 <- round(coef(mod8)[2], 5)
  eq_8 <- paste("y =", b0_8, "+", b1_8, "x")
  legend("topleft", bty="n", cex=0.8,
         legend=c(eq_8, paste("Pearson R² =", r2_8), paste("p =", p_8), paste("Spearman ρ =", rho_8)))
  }

###Stability ratio (foam persistence: 10min / 2min)
###data
  df$husk_stab <- ifelse(df$husk_2min > 0, df$husk_10min / df$husk_2min, NA)
  df$seed_stab <- ifelse(df$seed_2min > 0, df$seed_10min / df$seed_2min, NA)
  print(df)

          #Note:
            #This idea stems from expanding the analysis of variance, which is incorrect due low amount of data therefore the increase in error.
              #mod_a <- aov(altitude ~ husk_2min*husk_10min, data = df)
              #summary(mod_a)
              #mod_b <- aov(altitude ~ seed_10min*seed_2min, data = df)
              #summary(mod_b)
            #However, there is some relationship between the times since the factor tends to have a lower P_value

###Linear regression of stability vs altitude    
  mod9  <- lm(husk_stab~altitude, data = df)
  summary(mod9)
  mod10 <- lm(seed_stab~altitude, data = df)
  summary(mod10)

###Spearman correlation for stability ratios
  sp9  <- cor.test(df$altitude, df$husk_stab, method = "spearman")
  sp9
  sp10 <- cor.test(df$altitude, df$seed_stab, method = "spearman")
  sp10
  {
  rho_9  <- round(sp9$estimate, 3)
  rho_10 <- round(sp10$estimate, 3)
  }

###Plots stability ratio
  {
  par(mfrow = c(1,2))
  
  #husk stability
  plot(husk_stab~altitude, data = df, pch=19, main="Husk foam stability (10min/2min)",
       xlab="Altitude (m)", ylab="Stability ratio")
  text(df$altitude, df$husk_stab, labels=df$sample, pos=3, cex=0.7)
  abline(mod9, col="red", lwd=2)
  r2_9 <- round(summary(mod9)$r.squared, 3)
  p_9  <- round(summary(mod9)$coefficients[2,4], 4)
  b0_9 <- round(coef(mod9)[1], 3)
  b1_9 <- round(coef(mod9)[2], 6)
  eq_9 <- paste("y =", b0_9, "+", b1_9, "x")
  legend("topleft", bty="n", cex=0.8,
         legend=c(eq_9, paste("Pearson R² =", r2_9), paste("p =", p_9), paste("Spearman ρ =", rho_9)))
  
  #seed stability
  plot(seed_stab~altitude, data = df, pch=19, main="Seed foam stability (10min/2min)",
       xlab="Altitude (m)", ylab="Stability ratio")
  text(df$altitude, df$seed_stab, labels=df$sample, pos=3, cex=0.7)
  abline(mod10, col="red", lwd=2)
  r2_10 <- round(summary(mod10)$r.squared, 3)
  p_10  <- round(summary(mod10)$coefficients[2,4], 4)
  b0_10 <- round(coef(mod10)[1], 3)
  b1_10 <- round(coef(mod10)[2], 6)
  eq_10 <- paste("y =", b0_10, "+", b1_10, "x")
  legend("topleft", bty="n", cex=0.8,
         legend=c(eq_10, paste("Pearson R² =", r2_10), paste("p =", p_10), paste("Spearman ρ =", rho_10)))
  }




################ assumptions check  
  #Counts for the assumtions
  df$Counts <- 1:12
  
#verifying assumptions with husk2mm
  {
  par(mfrow = c(2,2))
  plot(mod2,which = 5)#standardized residuals vs. factor levels
  plot(mod2,which = 1)#residuals vs. cell means of fitted values
  plot(mod2,which = 2)#normality plot of the standardized residuals vs. EU
  plot(residuals(mod2)~Counts,main="Residuals vs Exp units", data = df)#plot residuals in "time" sequence
  }
#verifying assumptions with husk10mm
  {
  par(mfrow = c(2,2))
  plot(mod4,which = 5)#standardized residuals vs. factor levels
  plot(mod4,which = 1)#residuals vs. cell means of fitted values
  plot(mod4,which = 2)#normality plot of the standardized residuals vs. EU
  plot(residuals(mod4)~Counts,main="Residuals vs Exp units", data = df)#plot residuals in "time" sequence
  }
#verifying assumptions with seed2mm
  {
  par(mfrow = c(2,2))
  plot(mod6,which = 5)#standardized residuals vs. factor levels
  plot(mod6,which = 1)#residuals vs. cell means of fitted values
  plot(mod6,which = 2)#normality plot of the standardized residuals vs. EU
  plot(residuals(mod6)~Counts,main="Residuals vs Exp units", data = df)#plot residuals in "time" sequence
  }
#verifying assumptions with seed10mm
  {
  par(mfrow = c(2,2))
  plot(mod8,which = 5)#standardized residuals vs. factor levels
  plot(mod8,which = 1)#residuals vs. cell means of fitted values
  plot(mod8,which = 2)#normality plot of the standardized residuals vs. EU
  plot(residuals(mod8)~Counts,main="Residuals vs Exp units", data = df)#plot residuals in "time" sequence
  }
#verifying assumptions with husk ratio
  {
  par(mfrow = c(2,2))
  plot(mod9,which = 5)#standardized residuals vs. factor levels
  plot(mod9,which = 1)#residuals vs. cell means of fitted values
  plot(mod9,which = 2)#normality plot of the standardized residuals vs. EU
  plot(residuals(mod9)~Counts,main="Residuals vs Exp units", data = df)#plot residuals in "time" sequence
  }
#verifying assumptions with seed ratio
  {
  par(mfrow = c(2,2))
  plot(mod10,which = 5)#standardized residuals vs. factor levels
  plot(mod10,which = 1)#residuals vs. cell means of fitted values
  plot(mod10,which = 2)#normality plot of the standardized residuals vs. EU
  plot(residuals(mod10)~Counts,main="Residuals vs Exp units", data = df)#plot residuals in "time" sequence
  }
  #NOTE=Because the divisions for zero were closed as zero as a value, it is not possible to quantify the randomization test; 
  #however, with the previous tests, it is clear that the randomization was satisfactory.