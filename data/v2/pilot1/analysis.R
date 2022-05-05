
# Setup -------------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(combinat)

theme_update(strip.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             plot.background = element_blank(),
             axis.text=element_text(size=30, colour = "black"),
             axis.title=element_text(size=18, face = "bold"),
             axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"),
             legend.text = element_text(size = 20),
             plot.title = element_text(size = 26, face = "bold", vjust = 1),
             panel.margin = unit(1.0, "lines"), 
             plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
             axis.line = element_line(colour = "black", size = 2),
             axis.ticks = element_line(color = 'black', size = 3),
             axis.ticks.length = unit(.25, 'cm')
)

theme_black = function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "white"),  
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}
se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
as.string.vector = function(x) {
  return(strsplit(x,',')[[1]])
}
as.numeric.vector = function(x) {
  return(as.numeric(strsplit(x,',')[[1]]))
}
as.string = function(x) {
  return(paste(x, collapse = ','))
}
dodge <- position_dodge(width=0.9)

# Only works in RStudio -- otherwise you have to set the path manually
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ---------------------------------------------------------------
df.demo.raw = read.csv('demo.csv', stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.s1.raw = read.csv('s1.csv', stringsAsFactors = F) %>% arrange(subject)
df.s2.raw = read.csv('s2.csv', stringsAsFactors = F) %>% arrange(subject)


# Do filtering ---------------------------------------------------------

df.demo = df.demo.raw %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))
df.s1 = df.s1.raw %>% filter(practice == 0) %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))
df.s2 = df.s2.raw %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))


# Clean data --------------------------------------------------------------


ds.reversed = 6:10
mindfulness.reversed = c(3,4,7,8,9,13)
sk.reversed = c(4,5,6,7,8,9,10,11,12)

df.demo$decisionstyle = numeric(nrow(df.demo))
df.demo$mindfulness = numeric(nrow(df.demo))
df.demo$sk = numeric(nrow(df.demo))

for (i in 1:nrow(df.demo)) {
  ds = as.numeric.vector(df.demo$decisionstyle_responses[i])
  ds[ds.reversed] = 100 - ds[ds.reversed]
  df.demo$decisionstyle[i] = mean(ds)
  
  mindfulness = as.numeric.vector(df.demo$mindfulness_responses[i])
  mindfulness[mindfulness.reversed] = 100 - mindfulness[mindfulness.reversed]
  df.demo$mindfulness[i] = mean(mindfulness)
  
  sk = as.numeric.vector(df.demo$selfknowledge_responses[i])
  sk[sk.reversed] = 100 - sk[sk.reversed]
  df.demo$sk[i] = mean(sk)
}

df.demo = df.demo %>%
  mutate(choice.domain.fac = factor(choice_domain > median(choice_domain), c(F,T), c('Low', 'High')),
         decisionstyle.fac = factor(decisionstyle > median(decisionstyle), c(F,T), c('Low', 'High')),
         mindfulness.fac = factor(mindfulness > median(mindfulness), c(F,T), c('Low', 'High')),
         sk.fac = factor(sk > median(sk), c(F,T), c('Low', 'High')))

df.s1$choice.domain = numeric(nrow(df.s1))
df.s1$decisionstyle = numeric(nrow(df.s1))
df.s1$mindfulness = numeric(nrow(df.s1))
df.s1$sk = numeric(nrow(df.s1))

for (i in 1:nrow(df.s1)) {
  demo.row = as.character(df.demo$subject) == as.character(df.s1$subject[i])
  if (any(demo.row)) {
    df.s1$choice.domain[i] = df.demo$choice_domain[demo.row]
    df.s1$decisionstyle[i] = df.demo$decisionstyle[demo.row]
    df.s1$mindfulness[i] = df.demo$mindfulness[demo.row]
    df.s1$sk[i] = df.demo$sk[demo.row]
  } else {
    df.s1$choice.domain[i] = NA
    df.s1$decisionstyle[i] = NA
    df.s1$mindfulness[i] = NA
    df.s1$sk[i] = NA
  }
}

df.s1 = df.s1 %>%
  mutate(choice.domain.fac = factor(choice.domain > median(choice.domain, na.rm = T), c(F,T), c('Low', 'High')),
         decisionstyle.fac = factor(decisionstyle > median(decisionstyle, na.rm = T), c(F,T), c('Low', 'High')),
         mindfulness.fac = factor(mindfulness > median(mindfulness, na.rm = T), c(F,T), c('Low', 'High')),
         sk.fac = factor(sk > median(sk,na.rm = T), c(F,T), c('Low', 'High')))


df.s2$choice.domain = numeric(nrow(df.s2))
df.s2$decisionstyle = numeric(nrow(df.s2))
df.s2$mindfulness = numeric(nrow(df.s2))
df.s2$sk = numeric(nrow(df.s2))

for (i in 1:nrow(df.s2)) {
  demo.row = as.character(df.demo$subject) == as.character(df.s2$subject[i])
  if (any(demo.row)) {
    df.s2$choice.domain[i] = df.demo$choice_domain[demo.row]
    df.s2$decisionstyle[i] = df.demo$decisionstyle[demo.row]
    df.s2$mindfulness[i] = df.demo$mindfulness[demo.row]
    df.s2$sk[i] = df.demo$sk[demo.row]
  } else {
    df.s2$choice.domain[i] = NA
    df.s2$decisionstyle[i] = NA
    df.s2$mindfulness[i] = NA
    df.s2$sk[i] = NA
  }
}

df.s2 = df.s2 %>%
  mutate(choice.domain.fac = factor(choice.domain > median(choice.domain, na.rm = T), c(F,T), c('Low', 'High')),
         decisionstyle.fac = factor(decisionstyle > median(decisionstyle, na.rm = T), c(F,T), c('Low', 'High')),
         mindfulness.fac = factor(mindfulness > median(mindfulness, na.rm = T), c(F,T), c('Low', 'High')),
         sk.fac = factor(sk > median(sk,na.rm = T), c(F,T), c('Low', 'High')))


## Stage 1 choices
atts = c('Number of Bedrooms','Size of Garage','Amount of Crime in Neighborhood','Proximity to Parks','Proximity to Waterfront/Beaches',
         'Proximity to Cafes/Restaurants','Noise Pollution','Reputation of Closest School','Amount of Natural Light','Age of Building','Washer/Dryer','Size of Yard','Fireplace',
         'Central AC','Climate of Area','Hardwood Floors','Freshly Painted Exterior','Size of Home')

att.nums = 1:length(atts)
att.nums.str = as.character(att.nums)

atts.opt1 = paste0(atts,'.opt1')
atts.opt2 = paste0(atts,'.opt2')
atts.opt1.enclosed = paste0('`',atts.opt1,'`')
atts.opt2.enclosed = paste0('`',atts.opt2,'`')
atts.opt.diff = paste0(atts,'.diff')
atts.opt.diff.enclosed = paste0('`',atts.opt.diff,'`')

#df.s1.modeling = df.s1

df.s1[,atts.opt1] = NA
df.s1[,atts.opt2] = NA
#df.s1.modeling[,att.nums.str] = NA

df.avail.atts = data.frame(matrix(0,nrow = nrow(df.s1), ncol = length(atts)+1))
colnames(df.avail.atts) = c('subject.num', atts.opt1)
df.avail.atts$subject.num = df.s1$subject.num

for (i in 1:nrow(df.s1)) {
  cur.atts = as.string.vector(df.s1$attributes[i])
  cur.opt1.vals = as.string.vector(df.s1$opt1_values[i])
  cur.opt2.vals = as.string.vector(df.s1$opt2_values[i])
  
  cur.att.nums = numeric(length(cur.atts))
  for (j in 1:length(cur.atts)) {
    cur.att.nums[j] = which(cur.atts[j] == atts)
    df.s1[i,atts.opt1[cur.att.nums[j]]] = cur.opt1.vals[j]
    df.s1[i,atts.opt2[cur.att.nums[j]]] = cur.opt2.vals[j]
    df.avail.atts[i,atts.opt1[cur.att.nums[j]]] = 1
  }
  
  df.s1$att.nums[i] = as.string(cur.att.nums)
}

# clean up data
scale1 = c('None', 'Small', 'Medium', 'Large')
scale2 = c('Very Low', 'Low', 'Moderate', 'High', 'Very High')
scale3 = c('Very Bad', 'Bad', 'Moderate', 'Good', 'Very Good')
scale4 = c('Old', 'Medium', 'New')
scale5 = c('No', 'Yes')
scale6 = c('No natural light', 'A little natural light', 'Moderate natural light', 'A lot of natural light')
scale7 = c('Low', 'Medium', 'High')

for (i in 1:length(atts)) {
  cur.att.opt1 = paste0(atts[i],'.opt1')
  cur.att.opt2 = paste0(atts[i],'.opt2')
  
  if (atts[i] %in% c('Number of Bedrooms')) {
    df.s1[,cur.att.opt1] = as.numeric(df.s1[,cur.att.opt1])
    df.s1[,cur.att.opt2] = as.numeric(df.s1[,cur.att.opt2])
  } else if (atts[i] %in% c('Size of Home', 'Proximity to Parks', 'Proximity to Waterfront/Beaches', 'Proximity to Cafes/Restaurants')) {
    df.s1[,cur.att.opt1] = as.numeric(sub('\\ .*', '', df.s1[,cur.att.opt1]))
    df.s1[,cur.att.opt2] = as.numeric(sub('\\ .*', '', df.s1[,cur.att.opt2]))
  } else if (atts[i] %in% c('Amount of Crime in Neighborhood')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale2))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale2))
  } else if (atts[i] %in% c('Central AC', 'Fireplace', 'Hardwood Floors', 'Freshly Painted Exterior', 'Washer/Dryer')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale5))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale5))
  } else if (atts[i] %in% c('Size of Garage', 'Size of Yard')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale1))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale1))
  } else if (atts[i] %in% c('Age of Building')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale4))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale4))
  } else if (atts[i] %in% c('Amount of Natural Light')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale6))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale6))
  } else if (atts[i] %in% c('Reputation of Closest School', 'Climate of Area')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale3))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale3))
  } else if (atts[i] %in% c('Noise Pollution')) {
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], scale7))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], scale7))
  }
}

# compute diffs
for (i in 1:length(atts)) {
  df.s1[,paste0(atts[i], '.diff')] = df.s1[,atts.opt2[i]] - df.s1[,atts.opt1[i]]
}

# normalize attribute values
df.s1.scaled = df.s1
for (i in df.s1$subject.num) {
  subj.rows = df.s1$subject.num == i
  df.s1.scaled[subj.rows, c(atts.opt1, atts.opt2, atts.opt.diff)] = scale(df.s1[subj.rows, c(atts.opt1, atts.opt2, atts.opt.diff)])
}

df.s1.scaled.nonan = df.s1.scaled
for (i in 1:ncol(df.s1.scaled)) {
  df.s1.scaled.nonan[is.na(df.s1.scaled.nonan[,i]),i] = 0
}

# Check out data ----------------------------------------------------------

hist(df.demo$total_time_real)

df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(total.time = sum(rt) / 60000)
hist(df.s1.subj$total.time)
num.subj = nrow(df.s1.subj)

# run analysis!
formula = paste0('choice ~ ',
                 paste(atts.opt.diff.enclosed, collapse = " + "))
for (i in df.s1.scaled.nonan$subject.num) {
  cur.df.s1 = df.s1.scaled.nonan %>% filter(subject.num == i)
  m = glm(as.formula(formula), cur.df.s1, family = 'binomial')
  m.coefs = m$coefficients[-1]
  for (att in 1:length(m.coefs)) {
    df.s2$fitted.weight.lm[df.s2$subject.num == i & df.s2$attribute == atts[att]] = m.coefs[att]
  }
}

# get data for modeling ---------------------------------------------------

write.table(df.s1.scaled %>% select(subject.num, all_of(atts.opt1)), 'modeling_opts1.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1.scaled %>% select(subject.num, all_of(atts.opt2)), 'modeling_opts2.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% select(subject.num, choice) %>% mutate(choice = choice + 1), 'modeling_choice.csv', row.names = F, col.names = F, sep = ",")
write.table(df.avail.atts, 'modeling_avail_atts.csv', row.names = F, col.names = F, sep = ",")

# import modeling results -------------------------------------------------

df.fitted = read.csv('fitted_empirical_weights_priors.csv', header = F)
colnames(df.fitted) = atts

for (i in 1:nrow(df.s2)) {
  df.s2$fitted.weight[i] = df.fitted[df.s2$subject.num[i],df.s2$attribute[i]]
}

df.s2$fitted.weight.abs = abs(df.s2$fitted.weight)

df.s2.subj = df.s2 %>%
  group_by(subject) %>%
  summarize(accuracy = cor(fitted.weight.abs, rating))
for (i in 1:nrow(df.demo)) {
  df.demo$accuracy[i] = df.s2.subj$accuracy[as.character(df.s2.subj$subject) == as.character(df.demo$subject[i])]
}

df.s2.subj.split = df.s2 %>% mutate(trial_half = factor(trial %in% combs[,i], c(F,T), c('First Half', 'Second Half'))) %>%
  group_by(subject, trial_half) %>%
  summarize(accuracy = cor(fitted.weight.abs, rating))

combs = combn(1:18,9)
df.s2.grouped = df.s2 %>% group_by(subject)
shr = numeric(ncol(combs))
for (i in 1:ncol(combs)) {
  acc1 = (df.s2.grouped %>% filter(trial %in% combs[,i]) %>% summarize(accuracy = cor(fitted.weight.abs, rating)))$accuracy
  acc2 = (df.s2.grouped %>% filter(!(trial %in% combs[,i])) %>% summarize(accuracy = cor(fitted.weight.abs, rating)))$accuracy
  shr[i] = cor(acc1, acc2)
}

# get normalized per-subject
subjects = unique(df.s2$subject.num)
for (i in subjects) {
  cur.rows = df.s2$subject.num == i
  cur.ratings = df.s2[cur.rows,]$rating
  cur.weights = df.s2[cur.rows,]$fitted.weight.abs
  df.s2$rating.scaled[cur.rows] = scale(cur.ratings)
  df.s2$fitted.weight.abs.scaled[cur.rows] = scale(cur.weights)
}


# test modeling results ---------------------------------------------------



ggplot(df.s2, aes(x = fitted.weight, y = fitted.weight.lm)) +
  geom_point()
ggplot(df.s2, aes(x = fitted.weight.abs, y = rating)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.s2, aes(x = fitted.weight.abs.scaled, y = rating.scaled)) +
  geom_point() +
  geom_smooth(method='lm')

m1 = lmer(fitted.weight.abs ~ rating + (rating | subject), data = df.s2)
summary(rePCA(m1))
m2 = lmer(fitted.weight.abs ~ rating + (1 | subject), data = df.s2)
summary(rePCA(m2))
summary(m2)
m3 = lmer(fitted.weight.abs.scaled ~ rating.scaled + (1 | subject), data = df.s2)
summary(rePCA(m3))
summary(m3)

ggplot(df.s2.subj, aes(x = accuracy)) +
  geom_histogram()
ggplot(df.s2.subj, aes(x = trial_half, y = accuracy, color = subject, group = subject)) +
  geom_point() +
  geom_line() +
  guides(color = F, group = F)
m.retest = lmer()

# moderators --------------------------------------------------------------

ggplot(df.s2, aes(x = fitted.weight.abs, y = rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~choice.domain.fac)
m.choicedomain = lmer(fitted.weight.abs ~ rating * choice.domain + (1 | subject), data = df.s2)
summary(m.choicedomain)

ggplot(df.demo, aes(x = choice_domain, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')
m.choicedomain2 = lm(accuracy ~ choice_domain, data = df.demo)
summary(m.choicedomain2)


ggplot(df.demo, aes(x = decisionstyle, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')
m.ds = lm(accuracy ~ decisionstyle, data = df.demo)
summary(m.ds)


ggplot(df.demo, aes(x = mindfulness, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')
m.mindfulness = lm(accuracy ~ mindfulness, data = df.demo)
summary(m.mindfulness)



ggplot(df.demo, aes(x = sk, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')
m.mods = lm(accuracy ~ decisionstyle + mindfulness + sk, data = df.demo)
summary(m.mods)

ggplot(df.demo, aes(x = decisionstyle, y = mindfulness)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo, aes(x = sk, y = mindfulness)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo, aes(x = decisionstyle, y = sk)) +
  geom_point() +
  geom_smooth(method='lm')


# save data ---------------------------------------------------------------

save.image('analysis.rdata')
