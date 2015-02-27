library('randomForest')
library('ROCR')
library('ggplot2')
options(scipen = 16)
### Get data
###
### http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html


kddcupdata <- read.table('kddcup.data', sep = ',', header = FALSE, stringsAsFactors = FALSE)

### Find column names - http://kdd.ics.uci.edu/databases/kddcup99/kddcup.names
kddnames <- scan('./kddcup.names', what = character(), sep = '\n')
### Set correct names to dataframe
names(kddcupdata) <- c(unlist(lapply(kddnames[2:length(kddnames)], function(x) {strsplit(x, split = ':')[[1]][1]})), 'type')

### Summary statistics

summary(kddcupdata)

### Most events are icmp & all non-trivial services are tcp - something seems articial about dataset - many services has slightly over 1000 connections

with(kddcupdata, table(service, protocol_type))

### We see immediately outliers - very rare events

mean(kddcupdata$duration == 0) # Only few events over 1 second

table(kddcupdata$land) # Host to host connections very rare

table(kddcupdata$root_shell) # root shells very rare

table(kddcupdata$su_attempted) # Super user attempt very rare

table(kddcupdata$flag) # Mostly SF - normal status


hist(kddcupdata$count) # maximum connections to most hosts seem to be 511

###
### EXPLORATORY analysis
###

kddcupdata$mail <- with(kddcupdata, ifelse(service == 'smtp', 1, 0))

summary(subset(kddcupdata, mail > 0))

### Random assignment

kddcupdata$random <- runif(nrow(kddcupdata))


### Do model on subset to see patterns 
###
### We do a random forest to see patterns


###
###  Can't handle text variables so we treat them slightly different

mail.model <- randomForest(x = subset(kddcupdata[, -match(c('mail', 'type', 'service', 'flag', 'protocol_type'), names(kddcupdata))],random < 0.1), y = factor(subset(kddcupdata, random < 0.1)$mail), ntree = 50, verbose = TRUE, importance = TRUE)

varImpPlot(mail.model)

imp <- data.frame(names = dimnames(importance(mail.model))[[1]], importance(mail.model))[order(importance(mail.model)[,3]), ]
imp$names <- with(imp, factor(names, levels = names, ordered = TRUE))

ggplot(data = imp) + geom_point(aes(x = MeanDecreaseAccuracy, y = names), position = 'identity')

ggsave('importance.pdf')

### Histogram

ggplot(data = subset(kddcupdata, random < 0.1 & dst_bytes > 1), aes(x = dst_bytes, fill = service)) + geom_histogram() + scale_x_log10()
ggsave('dst_bytes.pdf')

ggplot(data = subset(kddcupdata, random < 0.1), aes(x = srv_diff_host_rate, fill = service)) + geom_histogram() + scale_x_log10()
ggsave('srv_diff_host_rate.pdf')

### Check logged in

y1 <- with(kddcupdata, tapply(logged_in, service, mean))

ggplot(data = data.frame(y = y1, names = names(y1))) + geom_point(aes(x = names, y = y, col = y > 0.5)) + coord_flip()

### Try prediction on rest of set.

kddcupdata$prob <- predict(mail.model, subset(kddcupdata[, -match(c('mail', 'type', 'service', 'flag', 'protocol_type'), names(kddcupdata))]), type = 'prob')[, 2]

dotchart(with(subset(kddcupdata, random > 0.9), tapply(prob, factor(service), mean)))

with(subset(kddcupdata, random > 0.9), mean((prob - mail)**2)) # MSE really small!

###

similarity <- with(subset(kddcupdata, random > 0.9), tapply(prob, factor(service), mean))
similarity <- data.frame(similarity, names = names(similarity))[order(similarity, decreasing = TRUE),]
similarity$names <- with(similarity, factor(names, levels = names, ordered = TRUE))

ggplot(data = similarity[1:20, ], aes(x = similarity, , y = names)) + geom_point() + xlab('mean probability')
ggsave('prob_service.pdf')
### Plots

ggplot(data = subset(kddcupdata, random < 0.1), aes(x = dst_host_same_srv_rate, y = mail)) + geom_smooth()

ggplot(data = subset(kddcupdata, random < 0.1), aes(x = dst_host_srv_count, y = mail)) + geom_smooth()

### We see that smtp can be predicted pretty well! But how well?

### Interesting to see 

ggplot(data = subset(kddcupdata, service == 'smtp'), aes(x = dst_bytes + 1, fill = prob[, 2] > 0.5)) + geom_histogram() + scale_x_log10()


### It is clear that almost all "small size" smtp messages are predicted wrong.


dotchart(with(subset(kddcupdata, random > 0.5 & dst_bytes == 0), tapply(prob[, 2], factor(service), mean)))


### Lets rebuild model on this population


mail.model2 <- randomForest(x = subset(kddcupdata[, -match(c('mail', 'type', 'service', 'flag', 'protocol_type'), names(kddcupdata))],random < 0.1 & dst_bytes == 0), y = factor(subset(kddcupdata, random < 0.1 & dst_bytes == 0)$mail), ntree = 50, verbose = TRUE, importance = TRUE)



kddcupdata$prob2 <- predict(mail.model2, kddcupdata, type = 'prob')[, 2]

### Random is most important variable! Sign that something is strange

pred <- with(subset(kddcupdata, random > 0.5 & dst_bytes == 0), prediction(prob2, factor(mail)))

perf <- performance(pred, "tpr", "fpr")

plot(perf)


###
### What is this private thing?
###

kddcupdata$private <- with(kddcupdata, ifelse(service == 'private', 1, 0))

private.model <- randomForest(x = subset(kddcupdata[, -match(c('prob', 'prob2', 'mail', 'type', 'service', 'flag', 'protocol_type', 'private'), names(kddcupdata))],random < 0.1), y = factor(subset(kddcupdata, random < 0.1)$private), ntree = 100, verbose = TRUE, importance = TRUE)

