# This scripts sets up the workflow for SSF/iSSF with individual models

rm(list=ls()) 

library(amt)
library(sf)
library(plyr)
library(dplyr)
library(multidplyr)
library(ggplot2)
library(tibble)
###############################################################################
# Set up data
###############################################################################

dat<-readRDS("03_data/Code_Neri/DataToUseTimeToNext.rds")

# Sample data to make sure the script runs
#dat<-dat[dat$animal_id%in%unique(dat$animal_id)[1:5],]

# Only use the january and february
dat$month<-as.numeric(format(dat$acquisition_time, "%m"))
dat<-dat[dat$month%in%c(1:3, 12),]

dat<-st_transform(dat, 32633)
dat$animal_id<-as.factor(dat$animal_id)
dat$X33<-st_coordinates(dat)[,1]
dat$Y33<-st_coordinates(dat)[,2]

dat.stat<-ddply(as.data.frame(dat), .(animal_id), summarise, 
                meanY=mean(Y33),
                meanX=mean(X33))

dat.stat.sf<-st_as_sf(dat.stat, coords=c("meanX", "meanY"), crs=32633)


dat_trk<-make_track(tbl=dat, .x=X33, .y=Y33, .t=acquisition_time, 
                    id=animal_id, crs=32633)

dat_nest <- dat_trk %>% nest(data = -"id")

dat_nest2 <- dat_nest %>%   mutate(steps = map(data, function(x) 
  x %>% track_resample(rate = minutes(60), tolerance = minutes(5)) %>% steps_by_burst()))

#dat_nest2 %>% select(id, steps) %>% unnest(cols = steps) %>% 
#  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4) + ylim(c(0,1)) + xlim(c(0,1000))

hist(dat_nest2[1,]$steps[[1]]$sl_, breaks=100)

# Exclude individuals with wrong fix rate
# Do some visual inspection of step lengths, turning angles and time of the day for movement here. 
# Should also have a look at some seasonal patterns

# Only include individuals with data
dat<-dat[dat$animal_id%in%dat_nest2[which(unlist(lapply(1:nrow(dat_nest2), function(x){nrow(dat_nest2[x,]$steps[[1]])>10}))),]$id,]

#dat_test<-dat[dat$animal_id%in%unique(dat$animal_id)[1:10],]

# Draw random steps based on estimated parameters from the same individual
min.sl<-15 # Minimum step length
n.control=10
library(parallel)
library(foreach)
cl <- makeCluster(10)
tictoc::tic()

out<-foreach (i=unique(dat$animal_id), combine=rbind) %dopar% {
  dat[dat$animal_id%in%i,] %>% 
      mk_track(.x=X33, .y=Y33, .t=acquisition_time, crs = 32633) %>%
        track_resample(rate = minutes(60), tolerance = minutes(6)) %>%
        filter_min_n_burst(min_n = 3) %>%
        steps_by_burst(degrees=FALSE) %>% 
        filter(sl_ > min.sl)  %>% # Exclude step lengths shorter than minimum step length
        random_steps(n_control=n.control) %>% 
        add_column(animal_id=i)
}
stopCluster(cl)
tictoc::toc() # 173

out2<-bind_rows(out)

# Renaming the bursts so that each burst is unique and not sheared by several moose
start.moose<-as.character(unique(out2[out2$burst_%in%max(out2$burst_),]$animal_id))

moose.to.use<-as.character(unique(out2$animal_id))
moose.to.use<-moose.to.use[-which(moose.to.use%in%start.moose)]

for(moose in moose.to.use){
  max.burst<-max(out2$burst_)
  out2[out2$animal_id%in%moose,"burst_"]<-out2[out2$animal_id%in%moose,"burst_"]+max.burst
}

# Create unique step_ids
out2$step_id_2<-paste0(out2$animal_id, "_", out2$step_id_)

# Save the final output
saveRDS(out2, "03_data/Code_Neri/DataToUseSSFAllInd_Individual_DecMarch.rds")
