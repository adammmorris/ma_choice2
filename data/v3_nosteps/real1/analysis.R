# Setup -------------------------------------------------------------------
require(ggplot2)
require(lme4)
require(lmerTest)
require(combinat)
require(effectsize)
require(RColorBrewer)
require(dplyr)
require(tidyr)
require(forcats)

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
get.ci = function(x) {return(c(mean(x,na.rm = T) - 1.96*se(x), mean(x, na.rm = T), mean(x, na.rm = T) + 1.96*se(x)))}
as.string.vector = function(x) {
  return(strsplit(x,',')[[1]])
}
as.numeric.vector = function(x) {
  return(as.numeric(strsplit(gsub('\\[|\\]','',x),',')[[1]]))
}
as.string = function(x) {
  return(paste(x, collapse = ','))
}
dodge <- position_dodge(width=0.9)

# Only works in RStudio -- otherwise you have to set the path manually
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ---------------------------------------------------------------
one.att.levels =c('One', 'Multiple')
bin.wts.levels =c('Binary', 'Graded')
bin.atts.levels =c('Binary', 'Graded')

df.demo = read.csv('demo.csv', stringsAsFactors = F) %>% arrange(subject) %>%
  rowwise() %>%
  mutate(total_time_real = total_time / 60000,
        instructions_times_list = list(as.numeric.vector(instructions_times) / 1000)) %>%
  ungroup() %>%
  mutate(one.att = factor(full_or_lex, c(1,2), one.att.levels),
         bin.wts = factor(signed_weights, c(1,2), bin.wts.levels),
         bin.atts = factor(signed_attributes, c(1,2), bin.atts.levels),
         subject = factor(subject), subject.num = as.numeric(subject))
for (i in 1:nrow(df.demo)) {
  df.demo$instruction_times_median[i] = median(df.demo$instructions_times_list[i][[1]])
  df.demo$instruction_times_sd[i] = sd(df.demo$instructions_times_list[i][[1]])
}

df.demo.followup = read.csv('demo_followup.csv', stringsAsFactors = F) %>% arrange(subject) %>%
  rowwise() %>%
  mutate(total_time_real = total_time / 60000,
         instructions_times_list = list(as.numeric.vector(instructions_times) / 1000))
for (i in 1:nrow(df.demo.followup)) {
  demo.row = df.demo$subject == df.demo.followup$subject[i];
  df.demo$instruction_times_followup_median[demo.row] = median(df.demo.followup$instructions_times_list[i][[1]])
  df.demo$instruction_times_followup_sd[demo.row] = sd(df.demo.followup$instructions_times_list[i][[1]])
}

# filter out anyone who didn't finish
subjlist = unique(df.demo$subject)

df.s1 = read.csv('s1.csv', stringsAsFactors = F) %>% filter(subject %in% subjlist, practice == 0) %>%
  arrange(subject) %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))
df.s2 = read.csv('s2.csv', stringsAsFactors = F) %>% filter(subject %in% subjlist) %>%
  arrange(subject) %>%
  mutate(subject = factor(subject), subject.num = as.numeric(subject),
         least_preferred_temp = least_preferred, least_preferred = most_preferred,
         most_preferred = least_preferred_temp) %>% # temporary shit I gotta do because I reversed the columns.. lol
  dplyr::select(-least_preferred_temp)
df.attributes = read.csv('attributes.csv', stringsAsFactors = F) %>% filter(subject %in% subjlist) %>%
  arrange(subject) %>% mutate(subject = factor(subject), subject.num = as.numeric(subject))

# ANT ---------------------------------------------------------------------

df.ant.raw = read.csv('ant.csv', stringsAsFactors = F) %>% arrange(subject) %>%
  mutate(flanker_type = factor(flanker_type, c('neutral', 'congruent', 'incongruent')),
         cue = factor(cue, c('nocue', 'center', 'double', 'spatial')))
df.ant = df.ant.raw %>% filter(subject %in% subjlist, practice == 0)

mean(df.ant$correct)
mean(df.ant$timed_out)
hist(df.ant$rt)

df.ant.subj = df.ant %>% group_by(subject) %>%
  summarize(correct.m = mean(correct), correct.se = se.prop(correct),
            timed_out.m = mean(timed_out), timed_out.se = se.prop(timed_out),
            rt.m = mean(rt, na.rm = T), rt.se = se(rt))
hist(df.ant.subj$correct.m)
hist(df.ant.subj$rt.m)
hist(df.ant.subj$timed_out.m)

# filter out bad subj
exclude.subj.ant = (df.ant.subj %>% filter(correct.m < .7 | rt.m < 350 | timed_out.m > .1))$subject

df.ant.filt = df.ant %>% filter(!(subject %in% exclude.subj.ant), rt < 1200, rt > 200, !timed_out)

hist(df.ant.filt$rt)
hist(log(df.ant.filt$rt))

# graph
df.ant.filt.graph = df.ant.filt %>% group_by(flanker_type, cue, subject) %>%
  summarize(correct = mean(correct),
            rt = mean(rt)) %>%
  group_by(flanker_type, cue) %>%
  summarize(correct.m = mean(correct), correct.se = se(correct),
            rt.m = mean(rt), rt.se = se(rt))

df.ant.filt.graph.fake = df.ant.filt %>%
  group_by(flanker_type, cue) %>%
  summarize(correct.m = mean(correct), correct.se = se.prop(correct),
            rt.m = mean(rt), rt.se = se(rt))

ggplot(df.ant.filt.graph, aes(x = flanker_type, group = cue, color = cue, y = rt.m)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = rt.m - rt.se, ymax = rt.m + rt.se))

ggplot(df.ant.filt.graph, aes(x = flanker_type, group = cue, color = cue, y = correct.m)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = correct.m - correct.se, ymax = correct.m + correct.se))

# alerting effect
df.ant.filt.alert = df.ant.filt %>% filter(cue %in% c('nocue', 'double')) %>%
  group_by(cue, subject) %>%
  summarize(correct = mean(correct),
            rt = mean(rt)) %>%
  arrange(subject) %>%
  group_by(subject) %>%
  mutate(alerting = rt - lead(rt)) %>%
  filter(cue == 'nocue') %>%
  dplyr::select(subject, alerting)

# orienting effect
df.ant.filt.orient = df.ant.filt %>% filter(cue %in% c('center', 'spatial')) %>%
  group_by(cue, subject) %>%
  summarize(correct = mean(correct),
            rt = mean(rt)) %>%
  arrange(subject) %>%
  group_by(subject) %>%
  mutate(orienting = rt - lead(rt)) %>%
  filter(cue == 'center') %>%
  dplyr::select(subject, orienting)

# executive effect
df.ant.filt.exec = df.ant.filt %>% filter(flanker_type %in% c('congruent', 'incongruent')) %>%
  group_by(flanker_type, subject) %>%
  summarize(correct = mean(correct),
            rt = mean(rt)) %>%
  arrange(subject) %>%
  group_by(subject) %>%
  mutate(exec = rt - lag(rt)) %>%
  filter(flanker_type == 'incongruent') %>%
  dplyr::select(subject, exec)

# combine them
df.ant.networks = df.ant.filt.alert
df.ant.networks$orienting = df.ant.filt.orient$orienting
df.ant.networks$exec = df.ant.filt.exec$exec
rm(df.ant.filt.alert, df.ant.filt.orient, df.ant.filt.exec)

# Clean up demo -----------------------------------------

### MODELS
models = c('Full', 'BinAtts', 'BinWts', 'BinWtsAtts', '1-Att')
models.one.att = c('Multiple', 'Multiple', 'Multiple', 'Multiple', 'One')
models.bin.wts = c('Graded', 'Graded', 'Binary', 'Graded', NA)
models.bin.atts = c('Graded', 'Binary', 'Graded', 'Graded', NA)
models.order = c('Full', 'BinWts', 'BinAtts', 'BinWtsAtts', '1-Att')
df.demo = df.demo %>%
  mutate(chosen.model.num = ifelse(one.att == 'One', 5,
                                   ifelse(bin.wts == 'Binary',
                                          ifelse(bin.atts == 'Binary', 4, 3),
                                          ifelse(bin.atts == 'Binary', 2, 1))),
         chosen.model = models[chosen.model.num],
         chosen.model.fac = factor(chosen.model, models.order, models.order))

## ADD ANT
df.demo$alerting = NA
df.demo$orienting = NA
df.demo$exec = NA
for (i in 1:nrow(df.demo)) {
  ant.row = df.ant.networks$subject == df.demo$subject[i]
  
  if (any(ant.row)) {
    alerting = df.ant.networks$alerting[ant.row]
    if (!is.null(alerting)) df.demo$alerting[i] = alerting;
    
    orienting = df.ant.networks$orienting[ant.row]
    if (!is.null(orienting)) df.demo$orienting[i] = orienting;
    
    exec = df.ant.networks$exec[ant.row]
    if (!is.null(exec)) df.demo$exec[i] = exec;
  }
}

### ADD ICAR
for (i in 1:nrow(df.demo.followup)) {
  df.demo$icar_num_correct[df.demo$subject == df.demo.followup$subject[i]] = df.demo.followup$icar_num_correct[i]
}

### MEDITATION EXP
howoften_options = c('Less than once per week', 'About once per week', '2-4 times per week', 'Daily or almost daily')
amount_options = c('5-15 minutes per day', '15-30 minutes per day', '>30 minutes per day')
years_options = c('0-1 years', '1-3 years', '3-5 years', '5+ years')
df.demo = df.demo %>%
  mutate(meditation = factor(meditation_exp2, c('Yes', 'No', ''), c('Currently', 'Used to', 'Never')))

### EDUCATION
df.demo = df.demo %>%
  mutate(edu.num = as.numeric(factor(edu,
                                     levels=c('Some high school', 'High school', 'Some college',
                                              '2 year degree', '4 year degree', 'Postgraduate/Professional degree/other'))))

### SCALES
battery = c('decisionstyle', 'acs', 'mindfulness', 'sris', 'maia')

max.vals = c(5,4,4,6,6)

# which ones are reversed?
ds.reversed = 6:10
#acs.reversed = c(1,2,3,5,6,8)
acs.reversed = c(1, 2, 3, 6, 7, 8, 11, 12, 15, 16, 20)
mindfulness.reversed = c(2,6,7)
sris.reversed = c(1, 2, 4, 7, 14, 16, 17, 18, 19)
maia.reversed = c()
#ipip.reversed = c(6:10, 17:20, 27:30, 33:40, 48:50)
#bidr.reversed = c(1,3,5,8,9,11,12,13)

reversed = list(ds.reversed, acs.reversed, mindfulness.reversed, sris.reversed, maia.reversed)

# which factors?
ds.factors = c(rep('deliberative',5), rep('intuitive',5))
#acs.factors = c(rep('focusing', 5), rep('shifting', 5))
acs.factors = c(rep('focusing',9), rep('shifting',11))
mindfulness.factors = c('attention', 'present focus', 'acceptance', 'acceptance', 'awareness',
                        'attention', 'present focus', 'awareness', 'awareness', 'acceptance', 'present focus', 'attention')
sris.factors = c(rep('tendency',12), rep('insight',8))
maia.factors = c(rep('noticing',4),rep('attention regulation',7),rep('emotional awareness',5),rep('body listening',3),rep('trusting',3))
#ipip.factors = c(rep('extraversion',10), rep('agreeableness',10), rep('conscientiousness',10),
#                 rep('stability',10), rep('openness',10))

factors = list(ds.factors, acs.factors, mindfulness.factors, sris.factors, maia.factors)

for (battery.ind in 1:length(battery)) {
  name = battery[battery.ind]
  reversed.cur = reversed[[battery.ind]]
  factors.cur = factors[[battery.ind]]
  factors.unique = unique(factors.cur)
  max.val.cur = max.vals[battery.ind]
  df.demo.followup[,name] = NA
  df.demo[,name] = NA
  
  for (i in 1:nrow(df.demo.followup)) {
    resp.str = df.demo.followup[i, paste0(name, '_responses')]
    if (!is.null(resp.str)) {
      demo.row = df.demo$subject == df.demo.followup$subject[i]
      
      resp = as.numeric.vector(resp.str)
      resp[reversed.cur] = max.val.cur - resp[reversed.cur] + 1
      df.demo.followup[i,name] = mean(resp)
      df.demo[demo.row,name] = mean(resp)
      
      for (fac in factors.unique) {
        which.items = which(factors.cur == fac)
        df.demo.followup[i,paste0(name, '.', fac)] = mean(resp[which.items])
        df.demo[demo.row,paste0(name, '.', fac)] = mean(resp[which.items])
      }
    }
  }
}

# choice.domain.fac = factor(choice_domain > median(choice_domain), c(F,T), c('Low', 'High')),
# decisionstyle.fac = factor(decisionstyle > median(decisionstyle), c(F,T), c('Low', 'High')),
# mindfulness.fac = factor(mindfulness > median(mindfulness), c(F,T), c('Low', 'High')),
# sk.fac = factor(sk > median(sk), c(F,T), c('Low', 'High')),
# bidr.fac = factor(bidr > median(bidr), c(F,T), c('Low', 'High')),
# acs.fac = factor(acs > median(acs), c(F,T), c('Low', 'High')),

# Clean up S1 --------------------------------------------------------------

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
    #att.row = df.attributes$subject == df.s1$subject[i] & df.attributes$attribute == cur.atts[j]
    
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

df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(total.time = sum(rt) / 60000,
            pct_left = mean(choice == 0),
            median_rt = median(rt),
            sd_rt = sd(rt),
            num_trials = n())

# Clean up S2 / attributes -------------------------------------------------------------

for (i in 1:nrow(df.attributes)) {
  cur.att = df.attributes$attribute[i]
  subj = df.attributes$subject[i]
  cur.scale = df.attributes$scale[i]
  
  rows.demo = df.demo$subject == subj
  rows.s2.original = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$original_or_followup == 'original'
  rows.s2.followup = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$original_or_followup == 'followup'
  rows.s2.direction = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$original_or_followup == 'direction'
  
  lex_q = df.demo$one.att[rows.demo]
  weights_q = df.demo$bin.wts[rows.demo]
  attributes_q = df.demo$bin.atts[rows.demo]
  
  if (lex_q == 'One') {
    # if this is the one...
    if (any(rows.s2.original)) {
      df.attributes$rating[i] = 1 # should be 1
    } else {
      df.attributes$rating[i] = 0
    }
    df.attributes$rating.followup[i] = df.s2$rating[rows.s2.followup] / 100
  } else {
    if (weights_q == 'Binary') {
      df.attributes$rating[i] = df.s2$rating[rows.s2.original]
      df.attributes$rating.followup[i] = df.s2$rating[rows.s2.followup] / 100
    } else {
      df.attributes$rating[i] = df.s2$rating[rows.s2.original] / 100
      df.attributes$rating.followup[i] = df.s2$rating[rows.s2.original] / 100
    }
  }
  
  if (cur.scale == "") {
    least = as.numeric(sub('\\ .*', '', df.s2$least_preferred[rows.s2.direction]))
    most = as.numeric(sub('\\ .*', '', df.s2$most_preferred[rows.s2.direction]))
    bounds = c(df.attributes$lb[i], df.attributes$ub[i])
  } else {
    cur.scale = unlist(strsplit(cur.scale, ","))
    least = which(df.s2$least_preferred[rows.s2.direction] == cur.scale)
    most = which(df.s2$most_preferred[rows.s2.direction] == cur.scale)
    bounds = c(1, length(cur.scale))
  }
  
  df.attributes$most[i] = most
  df.attributes$least[i] = least
  df.attributes$direction[i] = sign(most - least)
  df.attributes$same[i] = most == least
  df.attributes$linear[i] = least %in% bounds & most %in% bounds
  df.attributes$almost.linear[i] = 
    (least %in% bounds | (least+1) %in% bounds | (least-1) %in% bounds) &
    (most %in% bounds | (most+1) %in% bounds | (most-1) %in% bounds)
  
  df.attributes$lex_q[i] = lex_q 
  df.attributes$weights_q[i] = weights_q 
  df.attributes$attributes_q[i] = attributes_q
  df.attributes$chosen.model[i] = df.demo$chosen.model[rows.demo]
  df.attributes$chosen.model.num[i] = df.demo$chosen.model.num[rows.demo]
  df.attributes$chosen.model.fac[i] = df.demo$chosen.model.fac[rows.demo]
}

df.attributes = df.attributes %>%
  mutate(rating.signed = rating * direction,
         rating.followup.signed = rating.followup * direction)

# Import modeling results -------------------------------------------------

version = '';

## get cross-validation results
# ll version
df.cv.norm = read.csv(paste0('cv_results_normalized',version,'.csv'), header = F)
colnames(df.cv.norm) = models
df.cv.norm$subject.num = df.demo$subject.num

for (i in 1:nrow(df.demo)) {
  df.demo$best.model.nums[i] = list(which(df.cv.norm[i,1:length(models)] == max(df.cv.norm[i,1:length(models)], na.rm = T)))
  df.demo$multiple.bests[i] = length(df.demo$best.model.nums[i][[1]]) > 1
  df.demo$best.model.num[i] = max(df.demo$best.model.nums[i][[1]])
  df.attributes$best.model.num[df.attributes$subject == df.demo$subject[i]] = df.demo$best.model.num[i]
  df.demo$chosen.model.ll[i] = df.cv.norm[i,df.demo$chosen.model.num[i]]
  df.demo$chosen.model.rank[i] = rank(-df.cv.norm[i,1:length(models)])[df.demo$chosen.model.num[i]]
  
  df.demo$chose.correct.model[i] = df.demo$chosen.model.num[i] %in% df.demo$best.model.nums[i][[1]]
}
df.demo = df.demo %>%
  mutate(best.model = models[best.model.num],
         best.model.fac = factor(best.model, models.order, models.order),
         one.att.real = factor(models.one.att[best.model.num], one.att.levels),
         bin.wts.real = factor(models.bin.wts[best.model.num], bin.wts.levels),
         bin.atts.real = factor(models.bin.atts[best.model.num], bin.atts.levels))

# get likelihoods of best model
df.cv.best = read.csv(paste0('cv_results_best',version,'.csv'), header = F)
df.demo$best.model.ll.magnitude = df.cv.best$V3
df.demo$below.chance = df.cv.best$V2

# get non-normalized version
df.cv = read.csv(paste0('cv_results',version,'.csv'), header = F)
colnames(df.cv) = models
df.cv$subject.num = df.demo$subject.num
df.cv$subject = df.demo$subject

df.demo$avg.model.ll = rowMeans(df.cv[,1:length(models)])
df.demo$var.model.ll = apply(df.cv[,1:length(models)], 1, var)
df.demo$sd.model.ll = sqrt(df.demo$var.model.ll)

# rounded version
df.cv.rnd = read.csv(paste0('cv_results_rounded',version,'.csv'), header = F)
colnames(df.cv.rnd) = models

for (i in 1:nrow(df.demo)) {
  df.demo$best.model.nums.rnd[i] = list(which(df.cv.rnd[i,1:length(models)] == max(df.cv.rnd[i,1:length(models)], na.rm = T)))
  df.demo$multiple.bests.rnd[i] = length(df.demo$best.model.nums.rnd[i][[1]]) > 1
  df.demo$best.model.num.rnd[i] = max(df.demo$best.model.nums.rnd[i][[1]])
  df.attributes$best.model.num.rnd[df.attributes$subject == df.demo$subject[i]] = df.demo$best.model.num.rnd[i]
  df.demo$chosen.model.rnd[i] = df.cv.rnd[i,df.demo$chosen.model.num[i]]
  df.demo$best.model.rnd.magnitude[i] = df.cv.rnd[i,df.demo$best.model.num.rnd[i]]
  df.demo$best.model.rnd.magnitude2[i] = df.cv.rnd[i,df.demo$best.model.num[i]]
  df.demo$chosen.model.rank.rnd[i] = rank(-df.cv.rnd[i,1:length(models)])[df.demo$chosen.model.num[i]]

  df.demo$chose.correct.model.rnd[i] = df.demo$chosen.model.num[i] %in% df.demo$best.model.nums.rnd[i][[1]]
  df.demo$model.fits.agree[i] = df.demo$best.model.num[i] %in% df.demo$best.model.nums.rnd[i][[1]]
}
df.demo = df.demo %>%
  mutate(best.model.rnd = models[best.model.num.rnd],
    best.model.fac.rnd = factor(best.model.rnd, models.order, models.order))

df.demo$avg.model.rnd = rowMeans(df.cv.rnd[,1:length(models)])
df.demo$var.model.rnd = apply(df.cv.rnd[,1:length(models)], 1, var)

## get weights
df.fitted.wad = read.csv('fitted_empirical_weights_WAD.csv', header = F)
df.fitted.wp = read.csv('fitted_empirical_weights_WP.csv', header = F)
df.fitted.ew = read.csv('fitted_empirical_weights_EW.csv', header = F)
df.fitted.tal = read.csv('fitted_empirical_weights_TAL.csv', header = F)
df.fitted.lex.raw = read.csv('fitted_empirical_weights_LEX.csv', header = F)

colnames(df.fitted.wad) = atts
colnames(df.fitted.wp) = atts
colnames(df.fitted.ew) = atts
colnames(df.fitted.tal) = atts

df.fitted.lex = df.fitted.wad
for (i in 1:nrow(df.fitted.lex)) {
  for (att in 1:length(atts)) {
    if (df.fitted.lex.raw$V1[i] == att) {
      df.fitted.lex[i,att] = ifelse(df.fitted.lex.raw$V2[i] == 1, 1, -1)
    } else {
      df.fitted.lex[i,att] = 0
    }
  }
}

fitted.weights = list(df.fitted.wad, df.fitted.wp, df.fitted.ew, df.fitted.tal, df.fitted.lex)

for (i in 1:nrow(df.attributes)) {
  df.attributes$fitted.weight[i] = fitted.weights[[df.attributes$chosen.model.num[i]]][df.attributes$subject.num[i],df.attributes$attribute[i]]
  df.attributes$fitted.weight.best[i] = fitted.weights[[df.attributes$best.model.num[i]]][df.attributes$subject.num[i],df.attributes$attribute[i]]
  
  df.attributes$fitted.weight.wad[i] = fitted.weights[[1]][df.attributes$subject.num[i],df.attributes$attribute[i]]
  df.attributes$fitted.weight.wp[i] = fitted.weights[[2]][df.attributes$subject.num[i],df.attributes$attribute[i]]
  df.attributes$fitted.weight.ew[i] = fitted.weights[[3]][df.attributes$subject.num[i],df.attributes$attribute[i]]
  df.attributes$fitted.weight.tal[i] = fitted.weights[[4]][df.attributes$subject.num[i],df.attributes$attribute[i]]
  df.attributes$fitted.weight.lex[i] = fitted.weights[[5]][df.attributes$subject.num[i],df.attributes$attribute[i]]
}

df.attributes = df.attributes %>%
  mutate(fitted.weight.abs = abs(fitted.weight),
         fitted.weight.best.abs = abs(fitted.weight.best),
         fitted.weight.wad.abs = abs(fitted.weight.wad),
         error = fitted.weight - rating.signed,
         error.sq = error ^ 2,
         error.abs = fitted.weight.abs - rating,
         error.abs.sq = error.abs ^ 2,
         error.followup = fitted.weight.wad - rating.followup.signed,
         error.followup.sq = error.followup ^ 2,
         error.followup.abs = fitted.weight.wad.abs - rating.followup,
         error.followup.abs.sq = error.followup.abs ^ 2,
         error.best = fitted.weight.best - rating.signed,
         error.best.sq = error.best ^ 2,
         error.best.abs = fitted.weight.best.abs - rating,
         error.best.abs.sq = error.best.abs ^ 2,
         error.followup.best = fitted.weight.best - rating.followup.signed,
         error.followup.best.sq = error.followup.best ^ 2,
         error.followup.best.abs = fitted.weight.best.abs - rating.followup,
         error.followup.best.abs.sq = error.followup.best.abs ^ 2)

# get normalized per-subject fitted weights and ratings
subjects = unique(df.attributes$subject.num)
for (i in subjects) {
  cur.rows = df.attributes$subject.num == i
  df.attributes$rating.signed.scaled[cur.rows] = scale(df.attributes$rating.signed[cur.rows])
  df.attributes$fitted.weight.scaled[cur.rows] = scale(df.attributes$fitted.weight[cur.rows])
  #df.s2$fitted.weight.lm.scaled[cur.rows] = scale(df.s2$fitted.weight.lm[cur.rows])
}

# get subject-level accuracies
df.attributes.subj = df.attributes %>% group_by(subject) %>%
  summarize(num_same = sum(same & (rating > 10)),
            accuracy = cor(fitted.weight, rating.signed, method = 'kendall'),
            accuracy.abs = cor(fitted.weight.abs, rating, method = 'kendall'),
            accuracy.followup = cor(fitted.weight.wad, rating.followup.signed, method = 'kendall'),
            accuracy.followup.abs = cor(fitted.weight.wad.abs, rating.followup, method = 'kendall'),
            accuracy.scaled = cor(fitted.weight.scaled, rating.signed.scaled, method = 'kendall'),
            accuracy.best = cor(fitted.weight.best, rating.signed, method = 'kendall'),
            accuracy.followup.best = cor(fitted.weight.best, rating.followup.signed, method = 'kendall'),
            mse = mean(error.sq),
            mse.abs = mean(error.abs.sq),
            mse.followup = mean(error.followup.sq),
            mse.followup.abs = mean(error.followup.abs.sq),
            mse.best = mean(error.best.sq),
            mse.best.abs = mean(error.best.abs.sq),
            mse.followup.best = mean(error.followup.best.sq),
            mse.followup.best.abs = mean(error.followup.best.abs.sq))

for (i in 1:nrow(df.demo)) {
  att.row = df.attributes.subj$subject == df.demo$subject[i]
  df.demo$accuracy[i] = df.attributes.subj$accuracy[att.row]
  df.demo$accuracy.abs[i] = df.attributes.subj$accuracy.abs[att.row]
  df.demo$accuracy.followup[i] = df.attributes.subj$accuracy.followup[att.row]
  df.demo$accuracy.followup.abs[i] = df.attributes.subj$accuracy.followup.abs[att.row]
  df.demo$accuracy.scaled[i] = df.attributes.subj$accuracy.scaled[att.row]
  df.demo$accuracy.best[i] = df.attributes.subj$accuracy.best[att.row]
  df.demo$accuracy.followup.best[i] = df.attributes.subj$accuracy.followup.best[att.row]
  df.demo$mse[i] = df.attributes.subj$mse[att.row]
  df.demo$mse.abs[i] = df.attributes.subj$mse.abs[att.row]
  df.demo$mse.followup[i] = df.attributes.subj$mse.followup[att.row]
  df.demo$mse.followup.abs[i] = df.attributes.subj$mse.followup.abs[att.row]
  df.demo$mse.best[i] = df.attributes.subj$mse.best[att.row]
  df.demo$mse.best.abs[i] = df.attributes.subj$mse.best.abs[att.row]
  df.demo$mse.followup.best[i] = df.attributes.subj$mse.followup.best[att.row]
  df.demo$mse.followup.best.abs[i] = df.attributes.subj$mse.followup.best.abs[att.row]
}

## get inv temps
df.temp.wad = read.csv('fitted_empirical_temps_WAD.csv', header = F)
df.temp.wp = read.csv('fitted_empirical_temps_WP.csv', header = F)
df.temp.ew = read.csv('fitted_empirical_temps_EW.csv', header = F)
df.temp.tal = read.csv('fitted_empirical_temps_TAL.csv', header = F)
df.temp.lex = read.csv('fitted_empirical_temps_LEX.csv', header = F)

df.temp = bind_cols(df.temp.wad, df.temp.wp, df.temp.ew, df.temp.tal, df.temp.lex)

for (i in 1:nrow(df.demo)) {
  df.demo$inv.temp[i] = df.temp[i,df.demo$best.model.num[i]]
}

## get avg option diffs
df.optiondiffs = read.csv('option_diffs.csv', header = F)
df.demo$option.diff.avg = df.optiondiffs$V1
df.demo$option.diff.choiceprob = df.optiondiffs$V2
df.demo$option.diff.sd = df.optiondiffs$V3

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

# Browser events ----------------------------------------------------------

df.browser = read.csv('browser_events.csv', stringsAsFactors = F)
df.browser.subj = df.browser %>%
  filter(browser_event == 'blur') %>%
  group_by(subject) %>%
  summarize(num.blur = n())


# Do filtering ---------------------------------------------------------

exclude.subj = c()

for (subj in subjlist) {
  demo.row = df.demo$subject == subj
  s1.subj = df.s1.subj$subject == subj
  browser.row = df.browser.subj$subject == subj
  
  if (subj %in% exclude.subj.ant |
      df.demo$instruction_times_median[demo.row] < 2 |
      (!is.na(df.demo$instruction_times_followup_median[demo.row]) && df.demo$instruction_times_followup_median[demo.row] < 2) |
      df.s1.subj$pct_left[s1.subj] > .8 |
      df.s1.subj$pct_left[s1.subj] < .2 |
      df.s1.subj$num_trials != 75 |
      df.attributes.subj$num_same[df.attributes.subj$subject == subj] > 0 |
      df.demo$attention[demo.row] < .5 |
      df.demo$below.chance[demo.row] |
      (any(browser.row) && df.browser.subj$num.blur[df.browser.subj$subject == subj] > 20)) {
    exclude.subj = c(exclude.subj, subj)
  }
}

df.demo.filt = df.demo %>% filter(!(subject %in% exclude.subj))
df.s1.filt = df.s1 %>% filter(!(subject %in% exclude.subj))
df.s2.filt = df.s2 %>% filter(!(subject %in% exclude.subj))
df.attributes.filt = df.attributes %>% filter(!(subject %in% exclude.subj))
df.attributes.subj.filt = df.attributes.subj %>% filter(!(subject %in% exclude.subj))
df.cv.filt = df.cv %>% filter(!(subject %in% exclude.subj))

# Check out data ----------------------------------------------------------

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
ggplot(df.demo.filt, aes(x = chosen.model.fac)) +
  geom_histogram(stat = 'count')

# linearity
mean(df.attributes.filt$linear[df.attributes.filt$rating > 10])
mean(df.attributes.filt$almost.linear[df.attributes.filt$rating > 10])
df.attributes.byatt = df.attributes.filt %>% filter(rating > 5) %>%
  group_by(attribute) %>%
  summarize(linear = mean(linear), almost.linear = mean(almost.linear))
df.attributes.byatt

# appropriateness
hist(df.demo.filt$appropriateness)

# test modeling results ---------------------------------------------------

## check out model fits
# which models were the best fits?
ggplot(df.demo.filt, aes(x = best.model.fac)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')
sum(df.demo.filt$multiple.bests)
ggplot(df.demo.filt, aes(x = best.model.fac.rnd)) +
  geom_bar(position = 'dodge')
sum(df.demo.filt$multiple.bests.rnd)

mean(df.demo.filt$model.fits.agree)

ggplot(df.demo.filt, aes(x = one.att.real)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')
ggplot(df.demo.filt, aes(x = bin.wts.real)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')
ggplot(df.demo.filt, aes(x = bin.atts.real)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')

# how good were the model fits?
ggplot(df.demo.filt, aes(x = best.model.ll.magnitude)) +
  geom_histogram(color = 'white', bins = 20) +
  labs(x = "\nAvg out-of-sample likelihood\nof best model",
    y = "# of subjects") +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = 1, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = .5, color = 'red', linetype = 'dashed') +
  theme_black()
ll.m = mean(df.demo.filt$best.model.ll.magnitude)
ll.se = se(df.demo.filt$best.model.ll.magnitude)
c(ll.m - 1.96*ll.se, ll.m, ll.m + 1.96*ll.se)
ggplot(df.demo.filt, aes(x = best.model.rnd.magnitude2)) +
  geom_histogram(color = 'white', bins = 25) +
  labs(x = '\n% correct out-of-sample choices\nof best model',   
    y = "# of subjects") +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = 1, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = .5, color = 'red', linetype = 'dashed') +
  theme_black()
ll.m = mean(df.demo.filt$best.model.rnd.magnitude2)
ll.se = se(df.demo.filt$best.model.rnd.magnitude2)
c(ll.m - 1.96*ll.se, ll.m, ll.m + 1.96*ll.se)

# how varied were the model fits?
ggplot(df.demo.filt, aes(x = sd.model.ll)) +
  geom_histogram(color = 'white', bins = 20) +
  labs(x = "\nAvg out-of-sample likelihood\nof best model",
       y = "# of subjects") +
  scale_y_continuous(breaks = NULL) +
  theme_black()
df.cv.filt$avg.model.ll = df.demo.filt$avg.model.ll
df.cv.filt$mse.best = df.demo.filt$mse.best
df.cv.filt$chosen.model.fac = df.demo.filt$chosen.model.fac
df.cv.long = df.cv.filt %>% 
  mutate(subject.num = factor(subject.num),
         subject.num = fct_reorder(subject.num, avg.model.ll)) %>%
  pivot_longer(!c(subject.num,avg.model.ll,mse.best,chosen.model.fac,subject), names_to = 'model', values_to = 'll') %>%
  arrange(avg.model.ll) %>%
  mutate(chosen.model = chosen.model.fac == model)
ggplot(df.cv.long %>% mutate(mse.best = 2 * (.5 - mse.best)),
       aes(x = ll, y = subject.num, group = subject.num, color = subject.num, shape = chosen.model)) +
  geom_point() +
  guides(group = 'none', color = 'none') +#,
  #alpha = guide_legend(title = 'Parameter accuracy\n(higher is better)')) +
  #scale_color_brewer(palette = 'Set3') +
  theme_black()+
  scale_y_discrete(expand = c(-.5,0)) +
  scale_colour_manual(values=rep(brewer.pal(9,"Set1"),times=100))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = '\nAvg out-of-sample model likelihood',
       y = 'Subject')

# how good did the subjects think the models were?
ggplot(df.demo.filt, aes(x = appropriateness)) +
  geom_histogram(color = 'white', bins = 10) +
  theme_black() +
  geom_vline(xintercept = 100, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = 50, color = 'red', linetype = 'dashed') +
  scale_x_continuous(breaks = c(50, 75, 100)) +
  labs(x = '\nRating', y = '# of subjects')
ggplot(df.demo.filt, aes(x = appropriateness, y = best.model.ll.magnitude)) +
  geom_point(color = 'gray') +
  geom_smooth(method='lm', color = 'white') +
  theme_black() +
  labs(x = '\nRating', y = 'Out-of-sample likelihood\nof best model')
summary(lm(scale(best.model.ll.magnitude) ~ scale(appropriateness), data = df.demo.filt))
ggplot(df.demo.filt, aes(x = appropriateness, y = best.model.rnd.magnitude)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(best.model.rnd.magnitude ~ appropriateness, data = df.demo.filt))

hist(df.demo.filt$consistency1)

ggplot(df.demo.filt, aes(x = consistency1, y = inv.temp)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = consistency1, y = best.model.ll.magnitude)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = inv.temp, y = best.model.ll.magnitude)) +
  geom_point() +
  geom_smooth(method='lm')

## process awareness
# what models did subjects report using?
ggplot(df.demo.filt, aes(x = chosen.model.fac)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nSelf-reported model', y = '# of subjects')

ggplot(df.demo.filt, aes(x = best.model.fac, fill = chosen.model.fac)) +
  geom_bar(position = 'dodge') +
  labs(x = 'Best fitted model', y = '') +
  guides(fill = guide_legend(title = 'Reported model', title.position = 'top', title.hjust = .5)) +
  theme(legend.position = 'top') +
  scale_y_continuous(breaks = NULL)
ggplot(df.demo.filt, aes(x = best.model.fac.rnd, fill = chosen.model.fac)) +
  geom_bar(position = 'dodge') +
  labs(x = 'Best fitted model', y = '') +
  guides(fill = guide_legend(title = 'Reported model', title.position = 'top', title.hjust = .5)) +
  theme(legend.position = 'top') +
  scale_y_continuous(breaks = NULL)

df.demo.heat = df.demo %>% group_by(chosen.model.fac, best.model.fac) %>%
  summarize(num.subj = n()) %>%
  ungroup() %>%
  mutate(num.subj.norm = num.subj / max(num.subj),
         num.subj.fac = as.factor(num.subj))
ggplot(df.demo.heat, aes(x = chosen.model.fac, y = best.model.fac,
                         fill = num.subj)) +
  geom_tile() +
  labs(x = '\nSelf-reported model', y = 'Best-fitting model') +
  #scale_fill_brewer(palette = 'YlOrRd') +
  scale_fill_continuous(low = 'black', high = 'white') +
  guides(fill = guide_colorbar(title = '# of subjects')) +
  theme_black()
df.demo.filt = df.demo.filt %>%
  mutate(overestimated =
           (chosen.model.fac == 'Full' & best.model.fac != 'Full') |
           (chosen.model.fac %in% c('BinWts', 'BinAtts') & best.model.fac %in% c('BinWtsAtts', '1-Att')) |
           (chosen.model.fac == 'BinWtsAtts' & best.model.fac %in% c('1-Att')))

other.names = c('Full\nweighting', 'In-or-out', 'Weighted\ntallying', 'Tallying', 'Single\nattribute')
ggplot(df.demo.heat, aes(x = chosen.model.fac, y = best.model.fac,
                         fill = num.subj)) +
  geom_tile() +
  labs(x = '\nSelf-reported model', y = 'Best-fitting model') +
  #scale_fill_brewer(palette = 'YlOrRd') +
  scale_fill_continuous(low = 'grey87', high = 'dodgerblue4') +
  guides(fill = guide_colorbar(title = '# of\nsubjects', size = 22)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_x_discrete(labels = other.names) +
  scale_y_discrete(labels = other.names)

pct.correct = mean(df.demo.filt$chose.correct.model)
pct.correct.se = se.prop(df.demo.filt$chose.correct.model)
c(pct.correct - 1.96 * pct.correct.se, pct.correct, pct.correct + 1.96 * pct.correct.se)

pct.correct = mean(df.demo.filt$chose.correct.model[df.demo.filt$sd.model.ll > median(df.demo.filt$sd.model.ll)])
pct.correct.se = se.prop(df.demo.filt$chose.correct.model)
c(pct.correct - 1.96 * pct.correct.se, pct.correct, pct.correct + 1.96 * pct.correct.se)

# by feature
ggplot(df.demo.filt, aes(x = one.att, fill = one.att.real, group = one.att.real)) +
  geom_bar(position = 'fill') +
  #theme_black() +
  labs(x = "Self-reported", y = 'Proportion of\nsubjects') +
  guides(fill = guide_legend(title = 'Fitted')) +
  title('Property 1: One vs. multiple attributes') +
  #scale_y_continuous(limits = c(0,1), breaks = c(0,1)) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_brewer(palette = 'Set1') +
  theme(axis.text=element_text(size=18, colour = "black"),
        axis.title=element_text(size=22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18))
df.demo.filt %>% count(one.att)
df.demo.filt %>% count(one.att.real)
df.demo.filt %>% count(one.att, one.att.real)
mean(df.demo.filt$one.att == df.demo.filt$one.att.real)
summary(glm(one.att.real ~ one.att, data = df.demo.filt, family = 'binomial'))
chisq.test(df.demo.filt$one.att, df.demo.filt$one.att.real)


df.demo.filt = df.demo.filt %>% mutate(
  one.att.correct = one.att == one.att.real,
  bin.wts.correct = bin.wts == bin.wts.real,
  bin.atts.correct = bin.atts == bin.atts.real
)

cor.test(df.demo.filt$one.att.correct, df.demo.filt$bin.wts.correct)
df.demo.filt %>% count(one.att.correct, bin.wts.correct)

ggplot(df.demo.filt, aes(x = bin.wts, fill = bin.wts.real, group = bin.wts.real)) +
  geom_bar(position = 'fill') +
  #theme_black() +
  labs(x = "Self-reported", y = 'Proportion of\nsubjects') +
  guides(fill = guide_legend(title = 'Fitted')) +
  title('Property 1: One vs. multiple attributes') +
  #scale_y_continuous(limits = c(0,1), breaks = c(0,1)) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_brewer(palette = 'Set2') +
  theme(axis.text=element_text(size=18, colour = "black"),
        axis.title=element_text(size=22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18))
df.demo.filt$bin.wts.nona = as.character(df.demo.filt$bin.wts)
df.demo.filt$bin.wts.nona[is.na(df.demo.filt$bin.wts.nona)] = 'Nope'
df.demo.filt$bin.wts.nona = factor(df.demo.filt$bin.wts.nona)
df.demo.filt$bin.wts.real.nona = as.character(df.demo.filt$bin.wts.real)
df.demo.filt$bin.wts.real.nona[is.na(df.demo.filt$bin.wts.real.nona)] = 'Nope'
df.demo.filt$bin.wts.real.nona = factor(df.demo.filt$bin.wts.real.nona)
summary(glm(bin.wts.real ~ bin.wts,
            data = df.demo.filt,
            family = 'binomial'))
chisq.test(df.demo.filt$bin.wts, df.demo.filt$bin.wts.real)

ggplot(df.demo.filt, aes(x = bin.atts, fill = bin.atts.real, group = bin.atts.real)) +
  geom_bar(position = 'fill') +
  #theme_black() +
  labs(x = "Self-reported", y = 'Proportion of\nsubjects') +
  guides(fill = guide_legend(title = 'Fitted')) +
  title('Property 1: One vs. multiple attributes') +
  #scale_y_continuous(limits = c(0,1), breaks = c(0,1)) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_brewer(palette = 'Set2') +
  theme(axis.text=element_text(size=18, colour = "black"),
        axis.title=element_text(size=22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18))
summary(glm(bin.atts.real ~ bin.atts, data = df.demo.filt, family = 'binomial'))
chisq.test(df.demo.filt$bin.atts, df.demo.filt$bin.atts.real)


# How good or bad were the models that subjects reported using?

rand.lls = numeric(1000)
for (i in 1:length(rand.lls)) {
  nrow.cv = nrow(df.cv.filt)
  rnd.choices = sample(5,nrow.cv,replace=T)
  test = numeric(nrow.cv)
  for (j in 1:nrow.cv) {
    test[j] = df.cv.filt[j,rnd.choices[j]]
  }
  rand.lls[i] = mean(test)
}

ggplot(df.demo.filt, aes(x = chosen.model.ll)) +
  geom_histogram(color = 'white', bins = 25) +
  labs(x = "\nOOS likelihood of reported model\n(normalized so worst model = 0, best = 1)",
    y = "# of subjects") +
  scale_y_continuous(breaks = NULL) +
  theme_black() +
  geom_vline(xintercept = mean(df.demo.filt$chosen.model.ll), linetype = 1, color = 'gray') +
  geom_vline(xintercept = mean(rand.lls), linetype = 'dashed', color = 'gray')
get.ci(df.demo.filt$chosen.model.ll)
get.ci(rand.lls)

ggplot(df.demo.filt, aes(x = chosen.model.ll)) +
  geom_histogram(color = 'black', bins = 25) +
  labs(x = "OOS likelihood of reported model\n(worst model = 0, best model = 1)",
       y = "# of subjects") +
  scale_x_continuous(breaks = c(0,.5,1)) +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = mean(df.demo.filt$chosen.model.ll), linetype = 1, color = 'black') +
  geom_vline(xintercept = mean(rand.lls), linetype = 'dashed', color = 'black')

ggplot(df.demo.filt, aes(chosen.model.rank)) +
  geom_histogram(color = 'black', bins = 50) +
  labs(x = "Avg likelihood of out-of-sample choice\nrank of reported model",
       y = "Number of\nsubjects\n") +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = mean(df.demo.filt$chosen.model.rank), size = 1.5, linetype = 'longdash')

ggplot(df.demo.filt, aes(x = best.model.rnd.magnitude)) +
  geom_histogram(color = 'black', bins = 50) +
  labs(x = '% of correct out-of-sample choices\nfrom reported model',   
    y = "Number of\nsubjects\n") +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = .5, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = 1, color = 'red', linetype = 'dashed')+ 
  geom_vline(xintercept = mean(df.demo.filt$best.model.rnd.magnitude), size = 1.5, linetype = 'longdash')
  #geom_vline(xintercept = mean(df.demo.filt$avg.model.rnd), size = 1.5, linetype = 'longdash', color = 'blue')
ggplot(df.demo.filt, aes(x = chosen.model.rank.rnd)) +
  geom_histogram(color = 'black', bins = 50) +
  labs(x = '% of correct out-of-sample choices\nrank of reported model',   
       y = "Number of\nsubjects\n") +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = mean(df.demo.filt$chosen.model.rank.rnd), size = 1.5, linetype = 'longdash')

## parameter awareness
# across all points
ggplot(df.attributes.filt, aes(x = rating.signed, y = fitted.weight)) +
  geom_point() +
  #theme_black() +
  geom_smooth(method='lm',color = 'black') +
  labs(x = 'Self-reported weight', y = 'Fitted weight') +
  scale_x_continuous(breaks = c(-1, 0, 1))
m = lmer(scale(fitted.weight) ~ scale(rating.signed) + (scale(rating.signed) | attribute) + (0 + scale(rating.signed) | subject), data = df.attributes.filt)
summary(m)
r2beta(m, method = 'kr')
#standardize_parameters(m)
#test = lm(log(fitted.weight) ~ 1/(1+exp(rating.signed)), data = df.attributes.filt)

ggplot(df.attributes.filt, aes(x = rating.signed, y = fitted.weight.best)) +
  geom_point(color='gray') +
  geom_smooth(method='lm', color = 'white') +
  theme_black() +
  labs(x = '\nSelf-reported weight', y = 'Fitted weight\n(of best model)')
m = lmer(scale(fitted.weight.best) ~ scale(rating.signed) + (1 | attribute), data = df.attributes.filt)
summary(m)

ggplot(df.attributes.filt, aes(x = fitted.weight.scaled, y = rating.signed.scaled)) +
  geom_point() +
  geom_smooth(method='lm')

# plot subject-level accuracies
rand.errs = numeric(1000)
rand.errs.best = numeric(1000)
for (i in 1:length(rand.errs)) {
  test = numeric(nrow(df.demo.filt))
  test.best = numeric(nrow(df.demo.filt))
  for (j in 1:nrow(df.demo.filt)) {
    subj.rows = df.attributes$subject == df.demo.filt$subject[j]
    # if (df.demo.filt$chosen.model.fac[j] == '1-Att') {
    #   rnd.lbls = 1:length(atts)
    # } else {
    #   rnd.lbls = sample(1:length(atts))
    # }
    reported = sample(df.attributes$rating.signed[subj.rows])
    #reported = reported[rnd.lbls]
    fitted = df.attributes$fitted.weight[subj.rows]
    fitted.best = df.attributes$fitted.weight.best[subj.rows]
    #test[j] = mean((reported - fitted) ^ 2)
    #test.best[j] = mean((reported - fitted.best) ^ 2)
    test[j] = cor(reported, fitted, method = 'kendall')
  }
  rand.errs[i] = mean(test, na.rm = T)
  #rand.errs.best[i] = mean(test.best)
}

ggplot(df.demo.filt, aes(x = accuracy)) +
  geom_histogram(alpha = .3, fill = 'gray', color = 'white') +
  geom_vline(xintercept = mean(rand.errs), linetype = 'dashed', color = 'gray') +
  geom_vline(xintercept = mean(df.demo.filt$accuracy), linetype = 1, color = 'gray') +
  labs(x = '\nMean squared error\n(compared to fitted params of reported model)', y = '# of subjects') +
  scale_y_continuous(limits = c(0,25), breaks = NULL) +
  scale_x_continuous(limits = c(-.05,.55), breaks = c(0, .25, .5, .75)) +
  theme_black()
ggplot(df.demo.filt, aes(x = mse.best)) +
  geom_histogram(alpha = .3, fill = 'gray', color = 'white') +
  geom_vline(xintercept = mean(rand.errs.best), linetype = 'dashed', color = 'gray') +
  geom_vline(xintercept = mean(df.demo.filt$mse.best), linetype = 1, color = 'gray') +
  labs(x = '\nMean squared error\n(compared to fitted params of best model)', y = '# of subjects') +
  scale_y_continuous(limits = c(0,25), breaks = NULL) +
  scale_x_continuous(limits = c(-.05,.55), breaks = c(0, .25, .5, .75)) +
  theme_black()

t.test(df.demo.filt$mse, df.demo.filt$mse.best)

ggplot(df.demo.filt, aes(x = accuracy.abs)) +
  geom_histogram(color = 'black', bins = 10) +
  #geom_vline(xintercept = mean(df.demo.filt$accuracy.abs, na.rm = T), linetype = 2) +
  labs(x = 'Correlation between reported\nand fitted parameters', y = '# of subjects') +
  scale_x_continuous(breaks = c(0, .5, 1)) +
  scale_y_continuous(breaks = NULL)

get.ci(df.demo.filt$accuracy)

ggplot(df.demo.filt, aes(x = mse, y = accuracy,
                         group = chosen.model.fac, color = chosen.model.fac)) +
  geom_point()
  #geom_smooth(method='lm') +
cor.test(df.demo.filt$mse.abs, df.demo.filt$accuracy.abs)

# compare original to followup
df.demo.hadfollowup = df.demo.filt %>% filter(!(chosen.model.fac %in% c('Full', 'BinAtts')))
ggplot(df.demo.hadfollowup, aes(x = mse, y = mse.followup,
                         group = chosen.model.fac, color = chosen.model.fac)) +
  geom_point() +
  #geom_smooth(method='lm') +s
  #scale_x_continuous(limits = c(0,.6))+
  #scale_y_continuous(limits = c(0,.6))+
  geom_abline(intercept = 0, slope = 1)
ggplot(df.demo.hadfollowup, aes(x = mse.best, y = mse.followup.best)) +
                                #group = chosen.model.fac, color = chosen.model.fac)) +
  geom_point(color = 'white') +
  #geom_smooth(method='lm') +s
  #scale_x_continuous(limits = c(0,.6))+
  #scale_y_continuous(limits = c(0,.6))+
  geom_abline(intercept = 0, slope = 1, color = 'white') +
  theme_black() +
  labs(x = '\nMean squared error\nof process-appropriate weight ratings', y = 'Mean squared error\nof follow-up weight ratings')

t.test(df.demo.hadfollowup$mse.best, df.demo.hadfollowup$mse.followup.best)

ggplot(df.attributes.filt %>% filter(subject == '5783027f913c71000197300d'),
       aes(x = rating.followup)) +
  geom_histogram(bins = 10, fill = 'gray', color = 'white') +
  theme_black()

df.demo.chosen = df.demo.filt %>% group_by(chosen.model.fac) %>%
  summarize(mse.m = mean(mse), mse.se = se(mse),
            mse.best.m = mean(mse.best), mse.best.se = se(mse.best))
ggplot(df.demo.filt, aes(x = chosen.model.fac, y = mse)) +
  geom_violin() +
  geom_point(data = df.demo.chosen, aes(y = mse.m)) +
  geom_errorbar(data = df.demo.chosen,
                aes(y = mse.m,
                    ymin = mse.m - mse.se,
                    ymax = mse.m + mse.se))
ggplot(df.demo.filt, aes(x = chosen.model.fac, y = mse.best)) +
  geom_violin() +
  geom_point(data = df.demo.chosen, aes(y = mse.best.m)) +
  geom_errorbar(data = df.demo.chosen,
                aes(y = mse.best.m,
                    ymin = mse.best.m - mse.best.se,
                    ymax = mse.best.m + mse.best.se))

## compare process w/ parameter awareness
df.demo.correct = df.demo.filt %>% group_by(chose.correct.model) %>%
  summarize(mse.m = mean(mse), mse.se = se(mse),
            mse.best.m = mean(mse.best), mse.best.se = se(mse.best),
            accuracy.m = mean(accuracy, na.rm = T), accuracy.se = se(accuracy))
ggplot(df.demo.filt, aes(x = chose.correct.model, y = mse)) +
  geom_violin() +
  geom_point(data = df.demo.correct, aes(y = mse.m)) +
  geom_errorbar(data = df.demo.correct,
                aes(y = mse.m, ymin = mse.m - mse.se, ymax = mse.m + mse.se),
                width = .1)
summary(lm(mse ~ chose.correct.model, data = df.demo.filt))
ggplot(df.demo.filt, aes(x = chose.correct.model, y = mse.best)) +
  geom_violin() +
  geom_point(data = df.demo.correct, aes(y = mse.best.m)) +
  geom_errorbar(data = df.demo.correct,
                aes(y = mse.best.m, ymin = mse.best.m - mse.best.se, ymax = mse.best.m + mse.best.se),
                width = .1)  
summary(lm(mse.best ~ chose.correct.model, data = df.demo.filt))
ggplot(df.demo.filt, aes(x = chose.correct.model, y = accuracy)) +
  geom_violin() +
  geom_point(data = df.demo.correct, aes(y = accuracy.m)) +
  geom_errorbar(data = df.demo.correct,
                aes(y = accuracy.m, ymin = accuracy.m - accuracy.se, ymax = accuracy.m + accuracy.se),
                width = .1)  
summary(lm(accuracy ~ chose.correct.model, data = df.demo.filt))

ggplot(df.demo.filt, aes(x = chosen.model.ll, y = mse)) +
  geom_point(color = 'gray') +
  geom_smooth(method='lm', color = 'gray') +
  theme_black() +
  labs(x = '\nAvg likelihood of chosen model\n(normalized so worst model = 0 and best model = 1)',
       y = 'Mean squared error\n(compared to params of reported model)')
summary(lm(scale(mse) ~ scale(chosen.model.ll), data = df.demo.filt))
ggplot(df.demo.filt, aes(x = chosen.model.ll, y = mse.best)) +
  geom_point(color = 'gray') +
  geom_smooth(method='lm', color = 'gray') +
  theme_black() +
  labs(x = '\nAvg likelihood of chosen model\n(normalized so worst model = 0 and best model = 1)',
       y = 'Mean squared error\n(compared to params of best model)')
summary(lm(scale(mse.best) ~ scale(chosen.model.ll), data = df.demo.filt))

ggplot(df.demo.filt, aes(x = chosen.model.ll, y = accuracy.abs)) +
  geom_point() +
  geom_smooth(method='lm', color = 'black') +
  labs(x = 'Normalized OOS likelihood\nof reported model',
       y = 'Corr. b/w reported\nand fitted weights') +
  scale_x_continuous(breaks = c(0,1)) +
  scale_y_continuous(breaks = c(0,1))
summary(lm(scale(accuracy.abs) ~ scale(chosen.model.ll), data = df.demo.filt))
summary(lm(scale(accuracy.abs) ~ scale(chosen.model.ll) +
             best.model.ll.magnitude + var.model.ll + inv.temp, data = df.demo.filt))
summary(lm(scale(accuracy) ~ scale(best.model.ll.magnitude), data = df.demo.filt))
summary(lm(scale(accuracy) ~ scale(inv.temp), data = df.demo.filt))
summary(lm(scale(accuracy) ~ scale(inv.temp) + scale(best.model.ll.magnitude), data = df.demo.filt))


# moderators --------------------------------------------------------------

# compare accuracy to overall model goodness-of-fit
ggplot(df.demo.filt, aes(x = best.model.ll.magnitude, y = mse.best)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = '\nAverage model magnitude', y = 'Mean squared error')
ggplot(df.demo.filt, aes(x = var.model.ll, y = mse.best)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = avg.model.ll, y = chosen.model.ll)) +
  geom_point(color='gray') +
  geom_smooth(method='lm', color='gray') +
  theme_black() +
  labs(x = '\nAverage model magnitude', y = 'Chosen model magnitude\n(normalized)')
summary(glm(chosen.model.ll ~ avg.model.ll, data = df.demo.filt))
ggplot(df.demo.filt, aes(x = var.model.ll, y = chosen.model.ll)) +
  geom_point() +
  geom_smooth(method='lm')
test = df.demo.filt %>% group_by(chose.correct.model) %>%
  summarize(avg.m = mean(avg.model.ll), avg.se = se(avg.model.ll),
            var.m = mean(var.model.ll), var.se = se(var.model.ll))
ggplot(test, aes(x = chose.correct.model, y = avg.m)) +
  geom_col() +
  geom_errorbar(aes(ymin = avg.m - avg.se, ymax = avg.m + avg.se), width = .2)
ggplot(test, aes(x = chose.correct.model, y = var.m)) +
  geom_col() +
  geom_errorbar(aes(ymin = var.m - var.se, ymax = var.m + var.se), width = .2)
summary(glm(chose.correct.model ~ sd.model.ll, data = df.demo.filt))

ggplot(df.demo.filt, aes(x = consistency1, y = chosen.model.ll)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = consistency2, y = mse.best)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = consistency1, y = inv.temp)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = inv.temp, y = mse.best)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = 'Inverse temperature', y = 'Mean squared error')
ggplot(df.demo.filt, aes(x = inv.temp, y = chosen.model.ll)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = 'Inverse temperature', y = 'Chosen model likelihood\n(normalized)')
ggplot(df.demo.filt, aes(x = best.model.ll.magnitude, y = mse.best, color = inv.temp)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(mse.best ~ best.model.ll.magnitude + inv.temp, data = df.demo.filt))
summary(lm(chosen.model.ll ~ best.model.ll.magnitude + inv.temp, data = df.demo.filt))

ggplot(df.demo.filt, aes(x = option.diff.avg, y = best.model.ll.magnitude)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = 'Inverse temperature', y = 'Mean squared error')
ggplot(df.demo.filt, aes(x = option.diff.choiceprob, y = chosen.model.ll)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = 'Inverse temperature', y = 'Chosen model likelihood\n(normalized)')
summary(lm(best.model.ll.magnitude ~  option.diff.sd, data = df.demo.filt))
summary(lm(chosen.model.ll ~ option.diff.choiceprob, data = df.demo.filt))

# others
ggplot(df.demo.filt, aes(x = choice_domain, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(scale(accuracy) ~ scale(choice_domain), data = df.demo.filt))

ggplot(df.demo.filt, aes(x = choice_exp, y = accuracy)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = confidence, y = mse.best.abs)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = decisionstyle, y = chosen.model.ll)) +
  geom_point() +
  geom_smooth(method='lm')
m.ds = lm(accuracy ~ decisionstyle, data = df.demo)
summary(m.ds)

ggplot(df.demo.filt, aes(x = mindfulness, y = mse.best)) +
  geom_point() +
  geom_smooth(method='lm')
m.mindfulness = lm(accuracy ~ mindfulness, data = df.demo.filt %>% filter(chosen.model.fac != 'LEX'))
m.mindfulness = glm(chose.correct.model ~ mindfulness, data = df.demo, family = 'binomial')
summary(m.mindfulness)

ggplot(df.demo.filt, aes(x = maia, y = accuracy.best.mse)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = sris.tendency, y = mse.best)) +
  geom_point() +
  geom_smooth(method='lm')
summary(lm(mse.bes))
ggplot(df.demo.filt, aes(x = sris.tendency, y = chosen.model.ll)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = acs.focusing, y = accuracy)) +
  geom_point(color = 'gray') +
  geom_smooth(method='lm', color = 'gray') +
  labs(x = "\nAttentional Control Scale\nscore", y = "Mean squared error") +
  theme_black()
  #scale_x_continuous(breaks = c(20,100), limits = c(20,100))
  #scale_y_continuous(breaks = c(0,1), limits = c(0,1))
m.acs1 = lm(mse.best ~ acs.focusing + acs.shifting +
              chosen.model.fac + best.model.ll.magnitude + var.model.ll + inv.temp, data = df.demo.filt)
summary(m.acs1)
standardize_parameters(m.acs1)
ggplot(df.demo.filt, aes(x = acs.focusing, y = chosen.model.ll)) +
  geom_point(color = 'gray') +
  geom_smooth(method='lm', color = 'gray') +
  labs(x = "\nAttentional Control Scale\nscore", y = "Likelihood of chosen model") +
  theme_black() #+
  #scale_x_continuous(breaks = c(20,100), limits = c(20,100)) +
  #scale_y_continuous(breaks = c(-2,-1,0,1), limits = c(-2,1.4))
m.acs2 = lm(chosen.model.ll ~ acs.focusing + acs.shifting +
              chosen.model.fac + avg.model.ll + inv.temp +
              age + gender + edu.num + icar_num_correct, data = df.demo)
summary(m.acs2)
standardize_parameters(m.acs2)

ggplot(df.demo.filt, aes(x = alerting, y = mse.best)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = icar_num_correct, y = mse)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(df.demo.filt, aes(x = meditation, y = chosen.model.rnd)) +
  geom_violin() +
  stat_summary(fun.data = "mean_se", geom = "pointrange",
               colour = "red")
m.meditation = lm(mse.best ~ meditation, data = df.demo.filt)
summary(m.meditation)

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

# stats
m.mods = lm(mse.best ~ decisionstyle + mindfulness + sris.tendency + sris.insight + maia + acs.shifting + acs.focusing +
              icar_num_correct + gender + age + edu.num + meditation_exp1 +
              alerting + orienting + exec +
              choice_domain + confidence + consistency1 +
              chosen.model.fac + avg.model.ll,
            data = df.demo.filt)
summary(m.mods)
m.mods = lm(accuracy ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
              maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + edu.num + meditation_exp1 +
              alerting + orienting + exec +
              choice_domain + confidence + consistency1 +
              chosen.model.fac + avg.model.ll,
            data = df.demo.filt)
summary(m.mods)
m.mods = lm(mse.followup.best ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
              maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + meditation_exp1 +
              alerting + orienting + exec +
              choice_domain + confidence + consistency1 +
              chosen.model.fac + avg.model.ll,
            data = df.demo.filt)
summary(m.mods)
m.mods = lm(accuracy.followup ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
              maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + meditation_exp1 +
              alerting + orienting + exec +
              choice_domain + confidence + consistency1 +
              chosen.model.fac + avg.model.ll,
            data = df.demo.filt)
summary(m.mods)

m.mods = glm(chose.correct.model ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
              maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + meditation_exp1 +
              alerting + orienting + exec +
              choice_domain + confidence + consistency2 +
              chosen.model.fac + avg.model.ll,
            data = df.demo.filt, family = 'binomial')
summary(m.mods)
m.mods = lm(chosen.model.ll ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
              maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + edu.num + meditation_exp1 +
              alerting + orienting + exec + chosen.model.fac +
              choice_domain + confidence + consistency2,
            data = df.demo.filt)
summary(m.mods)
m.mods = lm(chosen.model.rnd ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
               maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + edu.num + meditation_exp1 +
               alerting + orienting + exec + chosen.model.fac +
              choice_domain + confidence + consistency2,
             data = df.demo.filt)
summary(m.mods)


m.mods = lm(exec ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
              maia + acs.shifting + acs.focusing + icar_num_correct + gender + age + meditation_exp1 +
              chosen.model.fac +
              choice_domain + confidence + consistency2,
            data = df.demo.filt)
summary(m.mods)

# save data ---------------------------------------------------------------

save.image('analysis.rdata')

# get data for modeling ---------------------------------------------------

write.table(df.s1 %>% dplyr::select(subject.num, all_of(atts.opt1)), 'modeling_opts1.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% dplyr::select(subject.num, all_of(atts.opt2)), 'modeling_opts2.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% dplyr::select(subject.num, choice) %>% mutate(choice = choice + 1), 'modeling_choice.csv', row.names = F, col.names = F, sep = ",")
write.table(df.avail.atts, 'modeling_avail_atts.csv', row.names = F, col.names = F, sep = ",")

# get data for option set -------------------------------------------------

df.s1.cur = df.s1 %>% filter(subject == '60ff2fc7ff799a89b38e6adc')
write.table(df.s1.cur %>% dplyr::select(all_of(atts.opt1)), 'trialset_opts1.csv', row.names = F, col.names = atts, sep = ",")
write.table(df.s1.cur %>% dplyr::select(all_of(atts.opt2)), 'trialset_opts2.csv', row.names = F, col.names = atts, sep = ",")
