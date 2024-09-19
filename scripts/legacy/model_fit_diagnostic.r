##Model fit diagnostics fors Reanalyis_random_effects_vv.r
#model diagnostics
linkscale<-ggplot(data.frame(eta=predict(model_annual,type="link"),pearson=residuals(model_annual,type="pearson")),
                  aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()
ggplot(data.frame(x1=annualPrecipData$Year,pearson=residuals(model_annual,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
qqnorm(residuals(model_annual))
qqnorm(residuals(model_jfma))

ggplot(data.frame(lev=hatvalues(model_annual),pearson=residuals(model_annual,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()
levId <- which(hatvalues(model_annual) >= .172)
cat(paste(shQuote(levId, type="cmd"), collapse=", "))

sensitive_plots<-annualPrecipData[c(levId),]  ##NOTE: THE MODEL IS SENSITIVE TO SITES WITH 6 OR LESS YEARS OF DATA, I AM REMOVING THEM AND RERUNNING THE MODEL

ggplot(annualPrecipData, aes(x=fitted_annual,y=residual_annual) ) +
  geom_point() +
  theme_bw() 
plot(hatvalues(model_annual), residuals(model_annual, type= "deviance", scaled= TRUE))

#REMOVE LEVERAGE ABOVE .145
rerun2<- subset(annualPrecipData, nyears>6)
model_annual_rerun2<- lmer(annualPrecip~ site +(1|Year), data=rerun2)
summary(model_annual_rerun2)
plot(model_annual_rerun2)

rerun2$fitted_annual_rerun2<- fitted(model_annual_rerun2)
ranef<- ranef(model_annual_rerun2, condVar=TRUE)$Year
ranef$Year <- dimnames(ranef)[[1]]
ranef<- rename(ranef, random_effect_rerun2="(Intercept)")
rerun2$Year<- as.character(annualPrecipData$Year)
rerun2<- merge(rerun2,ranef, by.x="Year", by.y= "Year")
rerun2<- subset(rerun2, Year>= MINYEAR)

rerun2$fixef_rerun2<- rerun2$fitted_annual_rerun2-rerun2$random_effect_rerun2
rerun2$year_site<- paste(rerun2$Year,rerun2$site,sep="_")
rerun3<- rerun2[,c("fixef_rerun2","year_site")]
rerun4<- annualPrecipData %>% mutate(year_site= paste(Year, site, sep="_")) %>% select(c("fixef_annual","year_site"))
merged<- merge(rerun3, rerun4, by.x= "year_site", by.y= "year_site", all= TRUE)
merged<- merged%>% subset(!is.na(fixef_rerun2))%>% mutate(change= fixef_rerun2-fixef_annual, site= substring(year_site, 6,20))


se<-sqrt(diag(vcov(model_annual)))  
se<- as.data.frame(se)
se$site<- dimnames(se)[[1]]
se$site<- substring(se$site, 5, 20)
merged<- merge(merged, se, by.x="site", by.y="site")


merged$multiples <- abs(merged$change / merged$se)               ## not significant enough to justify removing the sites