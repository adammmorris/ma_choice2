
# Setup -------------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(combinat)
require(effectsize)
require(RColorBrewer)

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
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
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

# filter out anyone who didn't finish
subjlist = unique(df.demo.raw$subject)

df.s1.raw = read.csv('s1.csv', stringsAsFactors = F) %>% filter(subject %in% subjlist) %>% arrange(subject) %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))
df.s2.raw = read.csv('s2.csv', stringsAsFactors = F) %>% filter(subject %in% subjlist) %>% arrange(subject) %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))
df.attributes.raw = read.csv('attributes.csv', stringsAsFactors = F) %>% filter(subject %in% subjlist) %>% arrange(subject) %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))

# Do filtering ---------------------------------------------------------

df.demo = df.demo.raw %>%
  mutate(subject = factor(subject), subject.num = as.numeric(subject),
         signed_weights = ifelse(signed_weights == 2, F, T),
         signed_attributes = ifelse(signed_attributes == 2, F, T))
df.s1 = df.s1.raw %>% filter(practice == 0)
df.s2 = df.s2.raw
df.attributes = df.attributes.raw

# Clean data --------------------------------------------------------------


## Stage 1 choices
# make wide df, put each attribute in its own column
atts = unique(df.attributes$attribute)

att.nums = 1:length(atts)
att.nums.str = as.character(att.nums)

atts.opt1 = paste0(atts,'.opt1')
atts.opt2 = paste0(atts,'.opt2')
atts.opt1.enclosed = paste0('`',atts.opt1,'`')
atts.opt2.enclosed = paste0('`',atts.opt2,'`')
atts.opt.diff = paste0(atts,'.diff')
atts.opt.diff.enclosed = paste0('`',atts.opt.diff,'`')

df.s1[,atts.opt1] = NA
df.s1[,atts.opt2] = NA

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

# convert attribute values to numerics
for (i in 1:length(atts)) {
  cur.att.opt1 = atts.opt1[i]
  cur.att.opt2 = atts.opt2[i]
  cur.scale = unique(df.attributes$scale[df.attributes$attribute == atts[i]])
  
  if (cur.scale == "") {
    df.s1[,cur.att.opt1] = as.numeric(sub('\\ .*', '', df.s1[,cur.att.opt1]))
    df.s1[,cur.att.opt2] = as.numeric(sub('\\ .*', '', df.s1[,cur.att.opt2]))
  } else {
    cur.scale = unlist(strsplit(cur.scale, split = ","))
    df.s1[,cur.att.opt1] = as.numeric(factor(df.s1[,cur.att.opt1], cur.scale))
    df.s1[,cur.att.opt2] = as.numeric(factor(df.s1[,cur.att.opt2], cur.scale))
  }
}

# compute diffs
for (i in 1:length(atts)) {
  df.s1[,paste0(atts[i], '.diff')] = df.s1[,atts.opt2[i]] - df.s1[,atts.opt1[i]]
}

# create scaled df.s1 with normalized attribute values (per subject)
df.s1.scaled = df.s1
for (i in df.s1$subject.num) {
  subj.rows = df.s1$subject.num == i
  df.s1.scaled[subj.rows, c(atts.opt1, atts.opt2, atts.opt.diff)] = scale(df.s1[subj.rows, c(atts.opt1, atts.opt2, atts.opt.diff)])
}

for (i in atts.opt1)

# same thing, except get rid of nan's
df.s1.scaled.nonan = df.s1.scaled
for (i in 1:ncol(df.s1.scaled)) {
  df.s1.scaled.nonan[is.na(df.s1.scaled.nonan[,i]),i] = 0
}

## do individual difference scales
ds.reversed = 6:10
mindfulness.reversed = c(3,4,7,8,9,13)
sk.reversed = c(4,5,6,7,8,9,10,11,12)
bidr.reversed = c(1,3,5,8,9,11,12,13)
acs.reversed = c(1,2,3,5,6,8)

df.demo$decisionstyle = numeric(nrow(df.demo))
df.demo$mindfulness = numeric(nrow(df.demo))
df.demo$sk = numeric(nrow(df.demo))
df.demo$bidr = numeric(nrow(df.demo))
df.demo$acs = numeric(nrow(df.demo))
df.demo$acs.focusing = numeric(nrow(df.demo))
df.demo$acs.shifting = numeric(nrow(df.demo))

for (i in 1:nrow(df.demo)) {
  ds = as.numeric.vector(df.demo$decisionstyle_responses[i])
  ds[ds.reversed] = 100 - ds[ds.reversed]
  df.demo$decisionstyle[i] = mean(ds)
  df.s1$decisionstyle[df.s1$subject == df.demo$subject[i]] = mean(ds)
  df.s2$decisionstyle[df.s2$subject == df.demo$subject[i]] = mean(ds)
  
  mindfulness = as.numeric.vector(df.demo$mindfulness_responses[i])
  mindfulness[mindfulness.reversed] = 100 - mindfulness[mindfulness.reversed]
  df.demo$mindfulness[i] = mean(mindfulness)
  df.s1$mindfulness[df.s1$subject == df.demo$subject[i]] = mean(mindfulness)
  df.s2$mindfulness[df.s2$subject == df.demo$subject[i]] = mean(mindfulness)
  
  sk = as.numeric.vector(df.demo$selfknowledge_responses[i])
  sk[sk.reversed] = 100 - sk[sk.reversed]
  df.demo$sk[i] = mean(sk)
  df.s1$sk[df.s1$subject == df.demo$subject[i]] = mean(sk)
  df.s2$sk[df.s2$subject == df.demo$subject[i]] = mean(sk)
  
  bidr = as.numeric.vector(df.demo$bidr_responses[i])
  bidr[bidr.reversed] = 100 - bidr[bidr.reversed]
  df.demo$bidr[i] = mean(bidr)
  df.s1$bidr[df.s1$subject == df.demo$subject[i]] = mean(bidr)
  df.s2$bidr[df.s2$subject == df.demo$subject[i]] = mean(bidr)
  
  acs = as.numeric.vector(df.demo$acs_responses[i])
  acs[acs.reversed] = 100 - acs[acs.reversed]
  df.demo$acs[i] = mean(acs)
  df.demo$acs.focusing[i] = mean(acs[1:5])
  df.demo$acs.shifting[i] = mean(acs[6:10])
  df.s1$acs[df.s1$subject == df.demo$subject[i]] = mean(acs)
  df.s2$acs[df.s2$subject == df.demo$subject[i]] = mean(acs)
}

models = c('WAD', 'WP', 'EW', 'TAL')
df.demo = df.demo %>%
  mutate(choice.domain.fac = factor(choice_domain > median(choice_domain), c(F,T), c('Low', 'High')),
         decisionstyle.fac = factor(decisionstyle > median(decisionstyle), c(F,T), c('Low', 'High')),
         mindfulness.fac = factor(mindfulness > median(mindfulness), c(F,T), c('Low', 'High')),
         sk.fac = factor(sk > median(sk), c(F,T), c('Low', 'High')),
         bidr.fac = factor(bidr > median(bidr), c(F,T), c('Low', 'High')),
         acs.fac = factor(acs > median(acs), c(F,T), c('Low', 'High')),
         chosen.model.num = ifelse(!signed_weights,
                               ifelse(!signed_attributes, 1, 2),
                               ifelse(!signed_attributes, 3, 4)),
         chosen.model = models[chosen.model.num],
         chosen.model.fac = factor(chosen.model, models))

## do s2 stuff
for (i in 1:nrow(df.s2)) {
  cur.att = df.s2$attribute[i]
  cur.scale = unique(df.attributes$scale[df.attributes$attribute == cur.att])
  
  if (cur.scale == "") {
    least = as.numeric(sub('\\ .*', '', df.s2$least_preferred[i]))
    most = as.numeric(sub('\\ .*', '', df.s2$most_preferred[i]))
    bounds = c(unique(df.attributes$lb[df.attributes$attribute == cur.att]), unique(df.attributes$ub[df.attributes$attribute == cur.att]))
  } else {
    cur.scale = unlist(strsplit(cur.scale, ","))
    least = which(df.s2$least_preferred[i] == cur.scale)
    most = which(df.s2$most_preferred[i] == cur.scale)
    bounds = c(1, length(cur.scale))
  }
  
  df.s2$direction[i] = sign(most - least)
  df.s2$linear[i] = least %in% bounds & most %in% bounds
  df.s2$chosen.model[i] = df.demo$chosen.model[df.demo$subject == df.s2$subject[i]]
  df.s2$chosen.model.num[i] = df.demo$chosen.model.num[df.demo$subject == df.s2$subject[i]]
  df.s2$chosen.model.fac[i] = df.demo$chosen.model.fac[df.demo$subject == df.s2$subject[i]]
}

df.s2 = df.s2 %>% mutate(rating.signed = rating * direction)

# Check out data ----------------------------------------------------------

hist(df.demo$total_time_real)

df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(total.time = sum(rt) / 60000)
hist(df.s1.subj$total.time)
num.subj = nrow(df.s1.subj)

# run linear model
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

# y variable: choice (0=left or 1=right)
# x variables: number of bedrooms for the left option, size of house for the left option, ...
glm(choice ~ `Number of Bedrooms.opt1` + `Size of Home.opt1` + ..., data = df.s1)

# who selected which model?
ggplot(df.demo, aes(x = chosen.model.fac)) +
  geom_histogram(stat = 'count')

# linearity
mean(df.s2$linear)

# import modeling results -------------------------------------------------

df.fitted.wad = read.csv('fitted_empirical_weights_WAD.csv', header = F)
df.fitted.wp = read.csv('fitted_empirical_weights_WP.csv', header = F)
df.fitted.ew = read.csv('fitted_empirical_weights_EW.csv', header = F)
df.fitted.tal = read.csv('fitted_empirical_weights_TAL.csv', header = F)
colnames(df.fitted.wad) = atts
colnames(df.fitted.wp) = atts
colnames(df.fitted.ew) = atts
colnames(df.fitted.tal) = atts
fitted.weights = list(df.fitted.wad, df.fitted.wp, df.fitted.ew, df.fitted.tal)

for (i in 1:nrow(df.s2)) {
  df.s2$fitted.weight[i] = fitted.weights[[df.s2$chosen.model.num[i]]][df.s2$subject.num[i],df.s2$attribute[i]]
  df.s2$fitted.weight.wad[i] = fitted.weights[[1]][df.s2$subject.num[i],df.s2$attribute[i]]
  df.s2$fitted.weight.wp[i] = fitted.weights[[2]][df.s2$subject.num[i],df.s2$attribute[i]]
  df.s2$fitted.weight.ew[i] = fitted.weights[[3]][df.s2$subject.num[i],df.s2$attribute[i]]
  df.s2$fitted.weight.tal[i] = fitted.weights[[4]][df.s2$subject.num[i],df.s2$attribute[i]]
}

# get normalized per-subject fitted weights and ratings
subjects = unique(df.s2$subject.num)
for (i in subjects) {
  cur.rows = df.s2$subject.num == i
  df.s2$rating.signed.scaled[cur.rows] = scale(df.s2$rating.signed[cur.rows])
  df.s2$fitted.weight.scaled[cur.rows] = scale(df.s2$fitted.weight[cur.rows])
  df.s2$fitted.weight.lm.scaled[cur.rows] = scale(df.s2$fitted.weight.lm[cur.rows])
}

# get subject-level accuracies
df.s2.subj = df.s2 %>%
  group_by(subject.num) %>%
  summarize(accuracy = cor(fitted.weight, rating.signed))
for (i in 1:nrow(df.demo)) {
  df.demo$accuracy[i] = df.s2.subj$accuracy[df.s2.subj$subject.num == df.demo$subject.num[i]]
}

test = df.s2 %>% group_by(attribute) %>%
  summarize(linear = mean(linear))

# these are my weird attempts at doing split-half reliability estimates
# df.s2.subj.split = df.s2 %>% mutate(trial_half = factor(trial %in% combs[,i], c(F,T), c('First Half', 'Second Half'))) %>%
#   group_by(subject, trial_half) %>%
#   summarize(accuracy = cor(fitted.weight.abs, rating))

# combs = combn(1:18,9)
# df.s2.grouped = df.s2 %>% group_by(subject)
# shr = numeric(ncol(combs))
# for (i in 1:ncol(combs)) {
#   acc1 = (df.s2.grouped %>% filter(trial %in% combs[,i]) %>% summarize(accuracy = cor(fitted.weight.abs, rating)))$accuracy
#   acc2 = (df.s2.grouped %>% filter(!(trial %in% combs[,i])) %>% summarize(accuracy = cor(fitted.weight.abs, rating)))$accuracy
#   shr[i] = cor(acc1, acc2)
# }

# get cross-validation results
df.cv = read.csv('cv_results.csv', header = F)
colnames(df.cv) = models
df.cv$subject.num = df.demo$subject.num
df.cv$chosen.model = df.demo$chosen.model
df.cv$chosen.model.fac = df.demo$chosen.model.fac
df.cv$chosen.model.num = df.demo$chosen.model.num

df.cv$WAD[df.cv$WAD > 1] = NA
df.cv$WP[df.cv$WP > 1] = NA
df.cv$EW[df.cv$EW > 1] = NA
df.cv$TAL[df.cv$TAL > 1] = NA

df.cv.model = df.cv %>% group_by(chosen.model.fac) %>%
  summarize(wad.mean = mean(WAD), wp.mean = mean(WP),
            ew.mean = mean(EW), tal.mean = mean(TAL))

for (i in 1:nrow(df.demo)) {
  cv.row = df.cv$subject.num == df.demo$subject.num[i]
  df.demo$best.model.num[i] = which(df.cv[cv.row,1:4] == 1)
  df.demo$chosen.model.ll[i] = df.cv[cv.row,df.demo$chosen.model.num[i]]
}
df.demo = df.demo %>% mutate(chose.correct.model = chosen.model.num == best.model.num,
                             best.model = models[best.model.num],
                             best.model.fac = factor(best.model, models),
                             signed_attributes_real = best.model %in% c('WP', 'TAL'),
                             signed_weights_real = best.model %in% c('EW', 'TAL'))

# test modeling results ---------------------------------------------------

## process awareness
pct.correct = mean(df.demo$chose.correct.model)
pct.correct.se = se.prop(df.demo$chose.correct.model)
c(pct.correct - 1.96 * pct.correct.se, pct.correct + 1.96 * pct.correct.se)

ggplot(df.demo, aes(x = best.model.fac, fill = chosen.model.fac)) +
  geom_bar(position = 'dodge') +
  labs(x = '', y = '') +
  guides(fill = guide_legend(title = 'Reported model', title.position = 'top', title.hjust = .5)) +
  theme(legend.position = 'top') +
  scale_y_continuous(breaks = NULL)

# do heatmap
df.demo.heat = df.demo %>% group_by(chosen.model.fac, best.model.fac) %>%
  summarize(num.subj = n()) %>%
  ungroup() %>%
  mutate(num.subj.norm = num.subj / max(num.subj),
         num.subj.fac = as.factor(num.subj))
ggplot(df.demo.heat, aes(x = best.model.fac, y = chosen.model.fac, fill = num.subj.fac, alpha = num.subj.fac)) +
  geom_tile() +
  labs(x = '', y = '') +
  scale_fill_brewer(palette = 'YlOrRd')

ggplot(df.demo, aes(x = signed_weights_real, fill = signed_weights)) +
  geom_bar(position = 'dodge')
ggplot(df.demo, aes(x = signed_attributes_real, fill = signed_attributes)) +
  geom_bar(position = 'dodge')

pct.correct = mean(df.demo$chosen.model.ll)
pct.correct.se = se(df.demo$chosen.model.ll)
c(pct.correct - 1.96 * pct.correct.se, pct.correct + 1.96 * pct.correct.se)

ggplot(df.demo, aes(x = chosen.model.ll)) +
  geom_histogram(color = 'black') +
  #labs(x = "Scaled cross-validated likelihood of\nobserved choices given reported model",
  #     y = "Number of\nsubjects\n") +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = NULL) +
  #geom_segment(aes(x = 0, y = 0, xend = 0, yend = 24), color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = 1, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = mean(df.demo$chosen.model.ll), size = 1.5, linetype = 'longdash')
ggplot(df.demo, aes(x = chosen.model.ll, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')


## parameter awareness
# all
ggplot(df.s2 %>% mutate(rating.signed = rating.signed / 100), aes(x = fitted.weight, y = rating.signed)) +
  geom_point() +
  #geom_smooth() +
  labs(x = '', y = '') +
  scale_x_continuous(breaks = c(-1, 0, 1), limits = c(-1, 1.05)) +
  scale_y_continuous(breaks = c(-1, 0, 1), limits = c(-1, 1.05))
m = lmer(fitted.weight ~ rating.signed + (1 | subject), data = df.s2)
summary(m)
r2beta(m, method = 'kr')
standardize_parameters(m)

ggplot(df.s2, aes(x = fitted.weight.scaled, y = rating.signed.scaled)) +
  geom_point() +
  geom_smooth(method='lm')
# wad & wp
ggplot(df.s2 %>% filter(chosen.model %in% c('WAD', 'WP')), aes(x = fitted.weight, y = rating.signed)) +
  geom_point() +
  geom_smooth(method='lm')# +
  #facet_wrap(~chosen.model.fac, scales = 'free')
ggplot(df.s2 %>% filter(chosen.model %in% c('WAD', 'WP')), aes(x = fitted.weight.scaled, y = rating.signed.scaled)) +
  geom_point() +
  geom_smooth(method='lm')# +
#facet_wrap(~chosen.model.fac, scales = 'free')

ggplot(df.s2 %>% filter(chosen.model %in% c('WP')), aes(x = fitted.weight.wp, y = rating.signed)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.s2 %>% filter(chosen.model %in% c('WP')), aes(x = fitted.weight.wad, y = rating.signed)) +
  geom_point() +
  geom_smooth(method='lm')
m = lmer(fitted.weight.wad ~ rating.signed + (1 | subject), data = df.s2 %>% filter(chosen.model == 'WP'))
summary(m)
standardize_parameters(m)
m2 = lmer(fitted.weight.wp ~ rating.signed + (1 | subject), data = df.s2 %>% filter(chosen.model == 'WP'))
summary(m2)
standardize_parameters(m2)

ggplot(df.s2, aes(x = fitted.weight.wad, y = fitted.weight.wp)) +
  geom_point() +
  geom_smooth(method='lm')
cor.test(df.s2$fitted.weight.wad, df.s2$fitted.weight.wp)

# ew & tal
ggplot(df.s2 %>%
         filter(chosen.model %in% c('EW', 'TAL')),
       aes(x = fitted.weight, y = rating.signed)) +
  geom_jitter(width = .15, height = .15) +
  geom_smooth(method='lm') #+
  #facet_wrap(~chosen.model.fac, scales = 'free')
ggplot(df.s2 %>%
         filter(chosen.model %in% c('EW', 'TAL')),
       aes(x = fitted.weight.scaled, y = rating.signed.scaled)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.s2 %>%
         filter(chosen.model %in% c('EW', 'TAL')) %>%
         mutate(rating.signed.fac = factor(rating.signed)),
       aes(x = fitted.weight, fill = rating.signed.fac)) +
  geom_bar(position = 'dodge')#+
  facet_wrap(~chosen.model.fac, scales = 'free')

m1 = lmer(fitted.weight.abs ~ rating + (rating | subject), data = df.s2)
summary(rePCA(m1))
m3 = lmer(fitted.weight.abs.scaled ~ rating.scaled + (1 | subject), data = df.s2)
summary(rePCA(m3))
summary(m3)

# plot subject-level accuracies
ggplot(df.s2.subj, aes(x = accuracy)) +
  geom_histogram(color = 'black') +
  geom_vline(xintercept = mean(df.s2.subj$accuracy, na.rm = T), size = 1.5, linetype = 'longdash') +
  labs(x = '', y = '') +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1))
accuracy.mean = mean(df.s2.subj$accuracy, na.rm = T)
accuracy.se = se(df.s2.subj$accuracy)
c(accuracy.mean - 1.96 * accuracy.se, accuracy.mean, accuracy.mean + 1.96*accuracy.se)
ggplot(df.s2.subj, aes(x = trial_half, y = accuracy, color = subject, group = subject)) +
  geom_point() +
  geom_line() +
  guides(color = F, group = F)

# moderators --------------------------------------------------------------

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
m.mindfulness = glm(chose.correct.model ~ mindfulness, data = df.demo, family = 'binomial')
summary(m.mindfulness)

ggplot(df.demo, aes(x = sk, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo, aes(x = bidr, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')
m.bidr = lm(accuracy ~ bidr, data = df.demo)
summary(m.bidr)

ggplot(df.demo, aes(x = acs.shifting, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm', color = 'black') +
  labs(x = "Attentional Control Scale\nscore", y = "Parameter awareness\nscore") +
  scale_x_continuous(breaks = c(20,100), limits = c(20,100)) +
  scale_y_continuous(breaks = c(0,1), limits = c(0,1))
m.acs1 = lm(accuracy ~ acs, data = df.demo)
summary(m.acs1)
standardize_parameters(m.acs1)
ggplot(df.demo, aes(x = acs, y = chosen.model.ll)) +
  geom_point() +
  geom_smooth(method='lm', color = 'black') +
  labs(x = "Attentional Control Scale\nscore", y = "Process awareness\nscore") +
  scale_x_continuous(breaks = c(20,100), limits = c(20,100)) +
  scale_y_continuous(breaks = c(-2,-1,0,1), limits = c(-2,1.4))
m.acs2 = glm(chose.correct.model ~ acs, data = df.demo, family = 'binomial')
summary(m.acs2)
standardize_parameters(m.acs2)

# power analysis
p_vals = numeric(100)
df.pwr = df.demo
for (i in 1:100) {
  print(i)
  resampled <-
    df.pwr %>%
    distinct(subject) %>%
    slice_sample(n = 250, replace = T) %>%
    group_by(subject) %>%
    mutate(instance = row_number()) %>%
    ungroup() %>%
    left_join(df.pwr,
              by = "subject") %>%
    mutate(subject = str_c(subject, instance))
  
  resampled_model <- glm(chose.correct.model ~ acs, family = 'binomial',
                        data = resampled)
  p_vals[i] <- summary(resampled_model)$coefficients[2,4]
}

m.mods = lm(accuracy ~ decisionstyle + mindfulness + sk + bidr + acs, data = df.demo)
summary(m.mods)


# save data ---------------------------------------------------------------

save.image('analysis.rdata')

# get data for modeling ---------------------------------------------------

write.table(df.s1 %>% select(subject.num, all_of(atts.opt1)), 'modeling_opts1.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% select(subject.num, all_of(atts.opt2)), 'modeling_opts2.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% select(subject.num, choice) %>% mutate(choice = choice + 1), 'modeling_choice.csv', row.names = F, col.names = F, sep = ",")
write.table(df.avail.atts, 'modeling_avail_atts.csv', row.names = F, col.names = F, sep = ",")
