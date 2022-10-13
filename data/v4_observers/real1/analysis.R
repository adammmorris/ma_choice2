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
      axis.text.x = element_text(size = 14, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = 14, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = 18, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = 18, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
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
get.ci.prop = function(x) {return(c(mean(x,na.rm = T) - 1.96*se.prop(x), mean(x, na.rm = T), mean(x, na.rm = T) + 1.96*se.prop(x)))}
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
target.path = '../../v4/real1/'

one.att.levels =c('One', 'Multiple')
bin.wts.levels =c('Binary', 'Graded')
bin.atts.levels =c('Binary', 'Graded')

df.demo = read.csv('demo.csv', stringsAsFactors = F) %>% arrange(subject) %>%
  rowwise() %>%
  mutate(total_time_real = total_time / 60000,
        instructions_times_list = list(as.numeric.vector(instructions_times) / 1000)) %>%
  ungroup() %>%
  mutate(chosen.oneatt = factor(lex_real > 50, c(F,T), one.att.levels),
         chosen.binwts = factor(binwts_real > 50, c(F,T), bin.wts.levels),
         chosen.binatts = factor(binatts_real > 50, c(F,T), bin.atts.levels),
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

#df.prolific = read.csv('prolific_data.csv') %>% filter(subj %in% subjlist)
#subj.order = df.prolific$participant_id

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
models = c('Full', 'BinAtts', 'BinWts', 'BinWtsAtts', '1Att', '1AttBinAtts')
models.one.att = c('Multiple', 'Multiple', 'Multiple', 'Multiple', 'One', 'One')
models.bin.wts = c('Graded', 'Graded', 'Binary', 'Binary', NA, NA)
models.bin.atts = c('Graded', 'Binary', 'Graded', 'Binary', 'Graded', 'Binary')
models.order = c('Full', 'BinWts', 'BinAtts', 'BinWtsAtts', '1Att', '1AttBinAtts')
df.demo = df.demo %>%
  mutate(chosen.model.num = ifelse(chosen.oneatt == 'One',
                                   ifelse(chosen.binatts == 'Binary', 6, 5),
                                   ifelse(chosen.binwts == 'Binary',
                                          ifelse(chosen.binatts == 'Binary', 4, 3),
                                          ifelse(chosen.binatts == 'Binary', 2, 1))),
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

### COMPREHENSION CHECKS

demo.colnames = colnames(df.demo)
for (i in 1:nrow(df.demo)) {
  # b/c of bug in pilot2.. can be removed for later
  #df.demo$binatts_comp1_number[i] = ifelse(grepl(".*Xavier.*", df.demo$binatts_comp1_description[i]),
                                                #1, 2)
  #df.demo$binatts_comp2_number[i] = ifelse(grepl(".*Xavier.*", df.demo$binatts_comp2_description[i]),
                                                #1, 2)
  
  if (df.demo$lex_comp1_number[i] == 2) {
    demo.colnames.temp1 = demo.colnames[grepl("lex_comp1.*", demo.colnames)]
    demo.colnames.temp2 = demo.colnames[grepl("lex_comp2.*", demo.colnames)]
    values.temp = df.demo[i,demo.colnames.temp1]
    df.demo[i,demo.colnames.temp1] = df.demo[i,demo.colnames.temp2]
    df.demo[i,demo.colnames.temp2] = values.temp
  }
  if (df.demo$binwts_comp1_number[i] == 2) {
    demo.colnames.temp1 = demo.colnames[grepl("binwts_comp1.*", demo.colnames)]
    demo.colnames.temp2 = demo.colnames[grepl("binwts_comp2.*", demo.colnames)]
    values.temp = df.demo[i,demo.colnames.temp1]
    df.demo[i,demo.colnames.temp1] = df.demo[i,demo.colnames.temp2]
    df.demo[i,demo.colnames.temp2] = values.temp
  }
  if (df.demo$binatts_comp1_number[i] == 2) {
    demo.colnames.temp1 = demo.colnames[grepl("binatts_comp1.*", demo.colnames)]
    demo.colnames.temp2 = demo.colnames[grepl("binatts_comp2.*", demo.colnames)]
    values.temp = df.demo[i,demo.colnames.temp1]
    df.demo[i,demo.colnames.temp1] = df.demo[i,demo.colnames.temp2]
    df.demo[i,demo.colnames.temp2] = values.temp
  }
}

colnames.temp = c('lex_comp1', 'lex_comp2', 'binwts_comp1', 'binwts_comp2', 'binatts_comp1', 'binatts_comp2')
df.cc = df.demo %>% select(subject, strat_q_order, colnames.temp) %>% pivot_longer(colnames.temp)
df.cc = df.cc %>% mutate(answer = ifelse(grepl(".*1.*", name), 0, 100),
                         correct = (answer == 0 & value < 50) | (answer == 100 & value > 50))
ggplot(df.cc, aes(x = value)) +
  geom_histogram() +
  geom_vline(aes(xintercept = answer), color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = 50), color = 'black', linetype = 'dashed') +
  facet_wrap(~name)

df.cc.question = df.cc %>% group_by(name) %>%
  summarize(correct = mean(correct), correct.se = se.prop(correct))
df.cc.question

df.cc.order = df.cc %>% group_by(strat_q_order, name) %>%
  summarize(correct = mean(correct), correct.se = se.prop(correct))
df.cc.order %>% filter(name == 'binwts_comp1')

df.cc.subj = df.cc %>% group_by(subject) %>%
  summarize(correct.total = sum(correct),
            correct.without.binatts1 = sum(ifelse(name == 'binatts_comp1', 0, correct)),
            correct.lex = sum(ifelse(grepl('lex_comp.*', name), correct, 0)),
            correct.binwts = sum(ifelse(grepl('binwts_comp.*', name), correct, 0)),
            correct.binatts = sum(ifelse(grepl('binatts_comp.*', name), correct, 0)))

for (i in 1:nrow(df.demo)) {
  cc.row = df.cc.subj$subject == df.demo$subject[i]
  df.demo$cc.correct.total[i] = df.cc.subj$correct.total[cc.row]
  df.demo$cc.correct.lex[i] = df.cc.subj$correct.lex[cc.row]
  df.demo$cc.correct.binwts[i] = df.cc.subj$correct.binwts[cc.row]
  df.demo$cc.correct.binatts[i] = df.cc.subj$correct.binatts[cc.row]
}

# Clean up S1 --------------------------------------------------------------

## Stage 1 choices
# make wide df, put each attribute in its own column
#atts = unique(df.attributes$attribute)
atts = read.csv(paste0(target.path, 'att_order.csv'), header = F)
atts = atts$V1

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
  cur.atts.order = numeric(length(cur.atts))
  for (j in 1:length(cur.atts)) {
    #att.row = df.attributes$subject == df.s1$subject[i] & df.attributes$attribute == cur.atts[j]
    
    cur.att.nums[j] = which(cur.atts[j] == atts)
    df.s1[i,atts.opt1[cur.att.nums[j]]] = cur.opt1.vals[j]
    df.s1[i,atts.opt2[cur.att.nums[j]]] = cur.opt2.vals[j]
    df.avail.atts[i,atts.opt1[cur.att.nums[j]]] = 1
    
    cur.atts.order[j] = which(atts[j] == cur.atts)
  }
  
  df.s1$atts.order[i] = as.string(cur.atts.order)
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
  
# same thing, except get rid of nan's
df.s1.scaled.nonan = df.s1.scaled
for (i in 1:ncol(df.s1.scaled)) {
  df.s1.scaled.nonan[is.na(df.s1.scaled.nonan[,i]),i] = 0
}

df.s1.subj = df.s1 %>% 
  mutate(correct.choice = choice == orig_choice) %>%
  group_by(subject) %>%
  summarize(total.time = sum(rt) / 60000,
            pct_left = mean(choice == 0),
            mean_rt = mean(rt),
            median_rt = median(rt),
            sd_rt = sd(rt),
            num_trials = n(),
            pct_correct = mean(correct.choice))
hist(df.s1.subj$pct_correct)

# Clean up S2 / attributes -------------------------------------------------------------

for (i in 1:nrow(df.attributes)) {
  cur.att = df.attributes$attribute[i]
  subj = df.attributes$subject[i]
  cur.scale = df.attributes$scale[i]
  
  rows.demo = df.demo$subject == subj
  rows.s2.wad = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$type == 'wad_att_rating'
  rows.s2.ew = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$type == 'ew_att_rating'
  rows.s2.lex = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$type == 'lex_att_rating'
  rows.s2.direction = df.s2$subject == subj & df.s2$attribute == cur.att & df.s2$type == 'direction'
  
  # if this is the one...
  if (any(rows.s2.lex)) {
    df.attributes$rating.lex[i] = 1 # should be 1
  } else {
    df.attributes$rating.lex[i] = 0
  }
  
  if (any(rows.s2.ew)) {
    df.attributes$rating.ew[i] = 1 # should be 1
  } else {
    df.attributes$rating.ew[i] = 0
  }

  df.attributes$rating.wad[i] = df.s2$rating[rows.s2.wad] / 100

  if (any(rows.s2.direction)) {
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
  } else {
    most = NA
    least = NA
    bounds = c(NA, NA)
  }
  
  df.attributes$most[i] = most
  df.attributes$least[i] = least
  df.attributes$direction[i] = sign(most - least)
  df.attributes$same[i] = most == least
  df.attributes$linear[i] = least %in% bounds & most %in% bounds
  df.attributes$almost.linear[i] = 
    (least %in% bounds | (least+1) %in% bounds | (least-1) %in% bounds) &
    (most %in% bounds | (most+1) %in% bounds | (most-1) %in% bounds)
  
  df.attributes$lex_real[i] = df.demo$lex_real[rows.demo] 
  df.attributes$binwts_real[i] = df.demo$binwts_real[rows.demo]  
  df.attributes$binatts_real[i] = df.demo$binatts_real[rows.demo] 
  df.attributes$chosen.oneatt[i] = df.demo$chosen.oneatt[rows.demo] 
  df.attributes$chosen.binwts[i] = df.demo$chosen.binwts[rows.demo]  
  df.attributes$chosen.binatts[i] = df.demo$chosen.binatts[rows.demo] 
  df.attributes$chosen.model[i] = df.demo$chosen.model[rows.demo]
  df.attributes$chosen.model.num[i] = df.demo$chosen.model.num[rows.demo]
  df.attributes$chosen.model.fac[i] = df.demo$chosen.model.fac[rows.demo]
  df.attributes$rating.chosen[i] = ifelse(df.demo$chosen.oneatt[rows.demo] == 'One',
                                          df.attributes$rating.lex[i],
                                          ifelse(df.demo$chosen.binwts[rows.demo] == 'Binary',
                                                 df.attributes$rating.ew[i],
                                                 df.attributes$rating.wad[i]))
}

df.attributes = df.attributes %>%
  mutate(rating.wad.signed = rating.wad * direction,
         rating.ew.signed = rating.ew * direction,
         rating.lex.signed = rating.lex * direction,
         rating.chosen.signed = rating.chosen * direction)

# Import modeling results -------------------------------------------------

version = '';

## get mapping
df.map = read.csv(paste0(target.path, 'observer_mapping.csv'), header = F)
colnames(df.map) = c('subject', 'subject.num')

## get cross-validation results
# non-normalized version
df.cv = read.csv(paste0(target.path, 'cv_results',version,'.csv'), header = F)
colnames(df.cv) = models
df.cv$subject.num = df.map$subject.num
df.cv$subject = df.map$subject
df.cv$avg.model.ll = rowMeans(df.cv[,1:length(models)])
df.cv$var.model.ll = apply(df.cv[,1:length(models)], 1, var)
df.cv$sd.model.ll = sqrt(df.cv$var.model.ll)

# likelihoods of best model
df.cv.best = read.csv(paste0(target.path, 'cv_results_best',version,'.csv'), header = F)
df.cv.best$subject = df.map$subject
df.cv.best$subject.num = df.map$subject.num
colnames(df.cv.best)[2:3] = c('below.chance', 'best.model.ll.magnitude')

# ll version
df.cv.norm = read.csv(paste0(target.path, 'cv_results_normalized',version,'.csv'), header = F)
colnames(df.cv.norm) = models
df.cv.norm$subject.num = df.map$subject.num
df.cv.norm$subject = df.map$subject

# rounded version
df.cv.rnd = read.csv(paste0(target.path, 'cv_results_rounded',version,'.csv'), header = F)
colnames(df.cv.rnd) = models
df.cv.rnd$subject.num = df.map$subject.num
df.cv.rnd$subject = df.map$subject

# add all to demo
df.demo$target_num_mapped = NA
df.demo$avg.model.ll = NA
df.demo$var.model.ll = NA
df.demo$sd.model.ll = NA
df.demo$below.chance = NA
df.demo$best.model.ll.magnitude = NA
df.demo$best.model.nums = NA
df.demo$multiple.bests = NA
df.demo$best.model.num = NA
df.demo$chosen.model.ll = NA
df.demo$chosen.model.ll.unnormed = NA
df.demo$chosen.model.rank = NA
df.demo$chose.correct.model = NA
df.demo$best.model.rnd.magnitude2 = NA

for (i in 1:nrow(df.demo)) {
  #target.id = df.demo$target_id[i]
  #target.mapped.num = df.map$subject.num[df.map$subject == target.id]
  target.mapped.num = df.demo$target_id_num[i]
  
  if (length(target.mapped.num) > 0) {
    df.demo$target_num_mapped[i] = target.mapped.num
    
    df.demo$avg.model.ll[i] = df.cv$avg.model.ll[target.mapped.num]
    df.demo$var.model.ll[i] = df.cv$var.model.ll[target.mapped.num]
    df.demo$sd.model.ll[i] = df.cv$sd.model.ll[target.mapped.num]
    
    df.demo$below.chance[i] = df.cv.best$below.chance[target.mapped.num]
    df.demo$best.model.ll.magnitude[i] = df.cv.best$best.model.ll.magnitude[target.mapped.num]
    
    df.demo$best.model.nums[i] = list(which(df.cv.norm[target.mapped.num,1:length(models)] == max(df.cv.norm[target.mapped.num,1:length(models)], na.rm = T)))
    df.demo$multiple.bests[i] = length(df.demo$best.model.nums[i][[1]]) > 1
    df.demo$best.model.num[i] = max(df.demo$best.model.nums[i][[1]])
    df.demo$chosen.model.ll[i] = df.cv.norm[target.mapped.num, df.demo$chosen.model.num[i]]
    df.demo$chosen.model.ll.unnormed[i] = df.cv[target.mapped.num, df.demo$chosen.model.num[i]]
    df.demo$chosen.model.rank[i] = rank(-df.cv.norm[target.mapped.num,1:length(models)])[df.demo$chosen.model.num[i]]
    
    df.demo$chose.correct.model[i] = df.demo$chosen.model.num[i] %in% df.demo$best.model.nums[i][[1]]
    
    df.demo$best.model.rnd.magnitude2[i] = df.cv.rnd[target.mapped.num,df.demo$best.model.num[i]]
  }
}

df.demo = df.demo %>%
  mutate(best.model = models[best.model.num],
         best.model.fac = factor(best.model, models.order, models.order),
         best.model.oneatt = factor(models.one.att[best.model.num], one.att.levels),
         best.model.binwts = factor(models.bin.wts[best.model.num], bin.wts.levels),
         best.model.binatts = factor(models.bin.atts[best.model.num], bin.atts.levels),
         chosen.model.dist = best.model.ll.magnitude - chosen.model.ll.unnormed,
         oneatt.correct = chosen.oneatt == best.model.oneatt,
         binwts.correct = chosen.binwts == best.model.binwts,
         binatts.correct = chosen.binatts == best.model.binatts)

# add to df.attributes

for (i in 1:nrow(df.attributes)) {
  row.demo = df.demo$subject == df.attributes$subject[i]
  df.attributes$target_num_mapped[i] = df.demo$target_num_mapped[row.demo]
  df.attributes$best.model.num[i] = df.demo$best.model.num[row.demo]
  df.attributes$rating.best[i] = ifelse(df.demo$best.model.oneatt[row.demo] == 'One',
                                        df.attributes$rating.lex[i],
                                        ifelse(df.demo$best.model.binwts[row.demo] == 'Binary',
                                               df.attributes$rating.ew[i],
                                               df.attributes$rating.wad[i]))
}
df.attributes = df.attributes %>% mutate(rating.best.signed = rating.best * direction)


## get weights
df.fitted.wad = read.csv(paste0(target.path, 'fitted_empirical_weights_WAD.csv'), header = F)
df.fitted.wp = read.csv(paste0(target.path, 'fitted_empirical_weights_WP.csv'), header = F)
df.fitted.ew = read.csv(paste0(target.path, 'fitted_empirical_weights_EW.csv'), header = F)
df.fitted.tal = read.csv(paste0(target.path, 'fitted_empirical_weights_TAL.csv'), header = F)
df.fitted.lex.raw = read.csv(paste0(target.path, 'fitted_empirical_weights_LEXNB.csv'), header = F)
df.fitted.lexb.raw = read.csv(paste0(target.path, 'fitted_empirical_weights_LEXB.csv'), header = F)

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
df.fitted.lexb = df.fitted.wad
for (i in 1:nrow(df.fitted.lexb)) {
  for (att in 1:length(atts)) {
    if (df.fitted.lexb.raw$V1[i] == att) {
      df.fitted.lexb[i,att] = ifelse(df.fitted.lexb.raw$V2[i] == 1, 1, -1)
    } else {
      df.fitted.lexb[i,att] = 0
    }
  }
}

fitted.weights = list(df.fitted.wad, df.fitted.wp, df.fitted.ew, df.fitted.tal, df.fitted.lex, df.fitted.lexb)

df.attributes[c('fitted.weight.chosen', 'fitted.weight.best',
                'fitted.weight.wad', 'fitted.weight.wp',
                'fitted.weight.ew', 'fitted.weight.tal',
                'fitted.weight.lex', 'fitted.weight.lexb')] = NA
for (i in 1:nrow(df.attributes)) {
  if (!is.na(df.attributes$target_num_mapped[i])) {
    df.attributes$fitted.weight.chosen[i] = fitted.weights[[df.attributes$chosen.model.num[i]]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    df.attributes$fitted.weight.best[i] = fitted.weights[[df.attributes$best.model.num[i]]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    
    df.attributes$fitted.weight.wad[i] = fitted.weights[[1]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    df.attributes$fitted.weight.wp[i] = fitted.weights[[2]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    df.attributes$fitted.weight.ew[i] = fitted.weights[[3]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    df.attributes$fitted.weight.tal[i] = fitted.weights[[4]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    df.attributes$fitted.weight.lex[i] = fitted.weights[[5]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
    df.attributes$fitted.weight.lexb[i] = fitted.weights[[6]][df.attributes$target_num_mapped[i],df.attributes$attribute[i]]
  }
}

df.attributes = df.attributes %>%
  mutate(fitted.weight.chosen.abs = abs(fitted.weight.chosen),
         fitted.weight.best.abs = abs(fitted.weight.best),
         fitted.weight.wad.abs = abs(fitted.weight.wad))

# get normalized per-subject fitted weights and ratings
# subjects = unique(df.attributes$subject.num)
# for (i in subjects) {
#   cur.rows = df.attributes$subject.num == i
#   df.attributes$rating.wad.signed.scaled[cur.rows] = scale(df.attributes$rating.wad.signed[cur.rows])
#   df.attributes$fitted.weight.chosen.scaled[cur.rows] = scale(df.attributes$fitted.weight.chosen[cur.rows])
#   #df.s2$fitted.weight.lm.scaled[cur.rows] = scale(df.s2$fitted.weight.lm[cur.rows])
# }

# get subject-level accuracies
df.attributes.subj = df.attributes %>% group_by(subject) %>%
  summarize(num_same = sum(same & (rating.wad > .1)),
            accuracy.wad.chosen = cor(fitted.weight.chosen, rating.wad.signed, method = 'kendall'),
            accuracy.wad.best = cor(fitted.weight.best, rating.wad.signed, method = 'kendall'),
            accuracy.ew.chosen = cor(fitted.weight.chosen, rating.ew.signed, method = 'kendall'),
            accuracy.ew.best = cor(fitted.weight.best, rating.ew.signed, method = 'kendall'),
            accuracy.lex.chosen = cor(fitted.weight.chosen, rating.lex.signed, method = 'kendall'),
            accuracy.lex.best = cor(fitted.weight.best, rating.lex.signed, method = 'kendall'),
            accuracy.chosen.best = cor(fitted.weight.best, rating.chosen.signed, method = 'kendall'),
            accuracy.chosen.chosen = cor(fitted.weight.chosen, rating.chosen.signed, method = 'kendall'),
            accuracy.best.best = cor(fitted.weight.best, rating.best.signed))

cols.to.xfer = colnames(df.attributes.subj %>% select(!subject))
for (i in 1:nrow(df.demo)) {
  att.row = df.attributes.subj$subject == df.demo$subject[i]
  
  for (cur.col in cols.to.xfer) {
    df.demo[i,cur.col] = df.attributes.subj[att.row, cur.col]
  }
}

## get inv temps
df.temp.wad = read.csv(paste0(target.path, 'fitted_empirical_temps_WAD.csv'), header = F)
df.temp.wp = read.csv(paste0(target.path, 'fitted_empirical_temps_WP.csv'), header = F)
df.temp.ew = read.csv(paste0(target.path, 'fitted_empirical_temps_EW.csv'), header = F)
df.temp.tal = read.csv(paste0(target.path, 'fitted_empirical_temps_TAL.csv'), header = F)
df.temp.lex = read.csv(paste0(target.path, 'fitted_empirical_temps_LEXNB.csv'), header = F)
df.temp.lexb = read.csv(paste0(target.path, 'fitted_empirical_temps_LEXB.csv'), header = F)

df.temp = bind_cols(df.temp.wad, df.temp.wp, df.temp.ew, df.temp.tal, df.temp.lex, df.temp.lexb)

df.demo$inv.temp = NA
for (i in 1:nrow(df.demo)) {
  if (!is.na(df.demo$target_num_mapped[i])) {
    df.demo$inv.temp[i] = df.temp[df.demo$target_num_mapped[i],df.demo$best.model.num[i]]
  }
}

## get avg option diffs
# df.optiondiffs = read.csv('option_diffs.csv', header = F)
# df.demo$option.diff.avg = df.optiondiffs$V1
# df.demo$option.diff.choiceprob = df.optiondiffs$V2
# df.demo$option.diff.sd = df.optiondiffs$V3

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

# do bonuses!
df.demo = df.demo %>% mutate(
  param.accuracy.bonus = round(df.demo$accuracy.best.best / 20,2)*100,
  param.accuracy.bonus = ifelse(param.accuracy.bonus < 0 | is.na(param.accuracy.bonus), 0, param.accuracy.bonus),
  process.accuracy.bonus = ifelse(!is.na(binwts.correct), oneatt.correct + binwts.correct + binatts.correct, oneatt.correct + binatts.correct + 1),
  bonus = .05 + .15 * process.accuracy.bonus + .1 * param.accuracy.bonus
)

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
  
  if (df.demo$instruction_times_median[demo.row] < 2 |
      df.s1.subj$num_trials[s1.subj] != 100 |
      df.s1.subj$pct_correct[s1.subj] < 0.95 |
      df.demo$attention[demo.row] < 50 |
      #df.attributes.subj$num_same[df.attributes.subj$subject == subj] > 0 |
      df.demo$below.chance[demo.row] |
      is.na(df.demo$target_num_mapped[demo.row]) |
      #subj %in% exclude.subj.ant |
      #(!is.na(df.demo$instruction_times_followup_median[demo.row]) && df.demo$instruction_times_followup_median[demo.row] < 2) |
      (any(browser.row) && df.browser.subj$num.blur[df.browser.subj$subject == subj] > 20)) {
    exclude.subj = c(exclude.subj, subj)
  }
}

df.demo.filt = df.demo %>% filter(!(subject %in% exclude.subj))
df.s1.filt = df.s1 %>% filter(!(subject %in% exclude.subj))
df.s1.subj.filt = df.s1.subj %>% filter(!(subject %in% exclude.subj))
df.s2.filt = df.s2 %>% filter(!(subject %in% exclude.subj))
df.attributes.filt = df.attributes %>% filter(!(subject %in% exclude.subj))
df.attributes.subj.filt = df.attributes.subj %>% filter(!(subject %in% exclude.subj))
df.cv.filt = df.cv %>% filter(!(subject %in% exclude.subj))
df.cv.norm.filt = df.cv.norm %>% filter(!(subject %in% exclude.subj))
df.cv.best.filt = df.cv.best %>% filter(!(subject %in% exclude.subj))
df.cc.filt = df.cc %>% filter(!(subject %in% exclude.subj))
df.cc.subj.filt = df.cc.subj %>% filter(!(subject %in% exclude.subj))

# Check out data ----------------------------------------------------------

test = df.demo %>% group_by(target_id_num) %>%
  summarize(num = n())

## run linear model
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

## linearity
mean(df.attributes.filt$linear[df.attributes.filt$rating.wad > .1])
mean(df.attributes.filt$almost.linear[df.attributes.filt$rating.wad > .1])
df.attributes.byatt = df.attributes.filt %>% filter(rating.wad > .1) %>%
  group_by(attribute) %>%
  summarize(linear = mean(linear), almost.linear = mean(almost.linear))
df.attributes.byatt

## appropriateness
hist(df.demo.filt$appropriateness)

## comprehension checks
ggplot(df.cc.filt, aes(x = value)) +
  geom_histogram() +
  geom_vline(aes(xintercept = answer), color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = 50), color = 'black', linetype = 'dashed') +
  facet_wrap(~name) +
  xlab('Response (0-100)') + ylab('# of Subjects') +
  scale_x_continuous(limits = c(-10,110), breaks = c(0,50,100)) +
  scale_y_continuous()

hist(df.cc.subj.filt$correct.total)

df.cc.question.filt = df.cc.filt %>% group_by(name) %>%
  summarize(correct = mean(correct), correct.se = se.prop(correct))
df.cc.question.filt

## self strat answers
hist(df.demo.filt$lex_real)
hist(df.demo.filt$binwts_real)
hist(df.demo.filt$binatts_real)

ggplot(df.demo.filt, aes(x = lex_real, y = binwts_real)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = lex_real, y = binatts_real)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = binwts_real, y = binatts_real)) +
  geom_point() +
  geom_smooth(method='lm')

## norm strat answers
ggplot(df.demo.filt, aes(x = lex_real, y = lex_norm)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = binwts_real, y = binwts_norm)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(df.demo.filt, aes(x = binatts_real, y = binatts_norm)) +
  geom_point() +
  geom_smooth(method='lm')

# test modeling results ---------------------------------------------------

## check out model fits
# which models were the best fits?
ggplot(df.demo.filt, aes(x = best.model.fac)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')
sum(df.demo.filt$multiple.bests)

ggplot(df.demo.filt, aes(x = best.model.oneatt)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')
ggplot(df.demo.filt, aes(x = best.model.binwts)) +
  geom_bar(position = 'dodge', color = 'white') +
  theme_black() +
  labs(x = '\nBest-fitting model', y = '# of subjects')
ggplot(df.demo.filt, aes(x = best.model.binatts)) +
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
  pivot_longer(!c(subject.num,avg.model.ll,chosen.model.fac,subject), names_to = 'model', values_to = 'll') %>%
  arrange(avg.model.ll) %>%
  mutate(chosen.model = chosen.model.fac == model)
ggplot(df.cv.long,
       aes(x = ll, y = subject.num, group = subject.num, color = subject.num, shape = model)) +
  geom_point(size = 2) +
  guides(group = 'none', color = 'none') +#,
  #alpha = guide_legend(title = 'Parameter accuracy\n(higher is better)')) +
  #scale_color_brewer(palette = 'Set3') +
  #theme_black()+
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

# look at stuff by model
summary(lm(best.model.ll.magnitude ~ best.model.fac, data = df.demo.filt))
summary(lm(sd.model.ll ~ best.model.fac, data = df.demo.filt))

summary(lm(consistency1 ~ best.model.fac, df.demo.filt)) 
summary(lm(consistency2 ~ best.model.fac, df.demo.filt)) 

df.demo.filt.model = df.demo.filt %>% group_by(best.model.fac) %>%
  summarize(best.model.ll.magnitude.m = mean(best.model.ll.magnitude),
            best.model.ll.magnitude.se = se(best.model.ll.magnitude),
            sd.model.ll.m = mean(sd.model.ll),
            sd.model.ll.se = se(sd.model.ll),
            consistency1.m = mean(consistency1),
            consistency1.se = se(consistency1),
            consistency2.m = mean(consistency2),
            consistency2.se = se(consistency2),
            accuracy.chosen.best.m = mean(accuracy.chosen.best),
            accuracy.chosen.best.se = se(accuracy.chosen.best),
            accuracy.chosen.chosen.m = mean(accuracy.chosen.chosen),
            accuracy.chosen.chosen.se = se(accuracy.chosen.chosen),
            chosen.model.ll.m = mean(chosen.model.ll),
            chosen.model.ll.se = se(chosen.model.ll),
            chose.correct.model.m = mean(chose.correct.model),
            chose.correct.model.se = se.prop(chose.correct.model))
ggplot(df.demo.filt.model, aes(x = best.model.fac, y = best.model.ll.magnitude.m)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = best.model.ll.magnitude.m - best.model.ll.magnitude.se,
                    ymax = best.model.ll.magnitude.m + best.model.ll.magnitude.se))
ggplot(df.demo.filt.model, aes(x = best.model.fac, y = sd.model.ll.m)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = sd.model.ll.m - sd.model.ll.se,
                    ymax = sd.model.ll.m + sd.model.ll.se))
ggplot(df.demo.filt.model, aes(x = best.model.fac, y = consistency2.m)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = consistency2.m - consistency2.se,
                    ymax = consistency2.m + consistency2.se))


# process awareness -------------------------------------------------------

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

df.demo.heat = df.demo %>% group_by(chosen.model.fac, best.model.fac) %>%
  summarize(num.subj = n())
ggplot(df.demo.heat, aes(x = best.model.fac, y = chosen.model.fac,
                         fill = num.subj)) +
  geom_tile() +
  labs(y = '\nSelf-reported model', x = 'Best-fitting model') +
  #scale_fill_brewer(palette = 'YlOrRd') +
  guides(fill = guide_colorbar(title = '# of subjects')) +
  theme_black()
df.demo.heat.normed = df.demo %>% group_by(chosen.model.fac, best.model.fac) %>%
  summarize(num.subj = n()) %>%
  group_by(best.model.fac) %>%
  mutate(num.subj.norm = num.subj / sum(num.subj))
ggplot(df.demo.heat.normed, aes(x = best.model.fac, y = chosen.model.fac,
                         fill = num.subj.norm)) +
  geom_tile() +
  geom_text(aes(label = round(num.subj.norm, 2))) +
  labs(y = '\nSelf-reported model', x = 'Best-fitting model') +
  #scale_fill_brewer(palette = 'YlOrRd') +
  scale_fill_continuous(limits = c(0,1), low = 'black', high = 'white') +
  guides(fill = guide_colorbar(title = '% of subjects')) +
  theme_black()

df.demo.filt = df.demo.filt %>%
  mutate(overestimated =
           (chosen.model.fac == 'Full' & best.model.fac != 'Full') |
           (chosen.model.fac %in% c('BinWts', 'BinAtts') & best.model.fac %in% c('BinWtsAtts', '1Att', '1AttBinAtts')) |
           (chosen.model.fac == 'BinWtsAtts' & best.model.fac %in% c('1Att', '1AttBinAtts')))
mean(df.demo.filt$overestimated)

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

get.ci.prop(df.demo.filt$chose.correct.model)

# do the tile plot but for simulated prob(true model | fitted model)

test = read.csv('normed_150.csv', header = F)
rownames(test) = models.order
colnames(test) = models.order
test$true = rownames(test)
test.long = test %>% pivot_longer(!true, 'fitted') %>%
  mutate(true = factor(true, models, models.order),
         fitted = factor(fitted, models, models.order))

ggplot(test.long, aes(x = fitted, y = true,
                         fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  labs(y = '\nTrue model', x = 'Best-fitting model') +
  #scale_fill_brewer(palette = 'YlOrRd') +
  scale_fill_continuous(limits = c(0,1), low = 'black', high = 'white') +
  guides(fill = guide_colorbar(title = '% of subjects')) +
  theme_black()

## by feature
# lex
df.feature.oneatt = df.demo.filt %>% group_by(best.model.oneatt) %>%
  summarize(lex_real.m = mean(lex_real),
            lex_real.se = se(lex_real))
df.feature.oneatt.cc = df.demo.filt %>% 
  filter(cc.correct.lex == 2) %>%
  group_by(best.model.oneatt) %>%
  summarize(lex_real.m = mean(lex_real))

ggplot(df.demo.filt, aes(x = lex_real, group = best.model.oneatt, fill = best.model.oneatt)) +
  geom_histogram(alpha = .5, position = 'identity')
ggplot(df.demo.filt %>% filter(cc.correct.lex == 2), aes(x = lex_real, group = best.model.oneatt, fill = best.model.oneatt)) +
  geom_histogram(alpha = .5, position = 'identity')
ggplot(df.feature.oneatt, aes(x = best.model.oneatt, y = lex_real.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = lex_real.m - lex_real.se, ymax = lex_real.m + lex_real.se))
ggplot(df.feature.oneatt.cc, aes(x = best.model.oneatt, y = lex_real.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = lex_real.m - lex_real.se, ymax = lex_real.m + lex_real.se))

# binwts
df.feature.binwts = df.demo.filt %>% group_by(best.model.binwts) %>%
  summarize(binwts_real.m = mean(binwts_real),
            binwts_real.se = se(binwts_real))
df.feature.binwts.cc = df.demo.filt %>% 
  filter(cc.correct.binwts == 2) %>%
  group_by(best.model.binwts) %>%
  summarize(binwts_real.m = mean(binwts_real),
            binwts_real.se = se(binwts_real))

ggplot(df.demo.filt, aes(x = binwts_real, group = best.model.binwts, fill = best.model.binwts)) +
  geom_histogram(alpha = .5, position = 'identity')
ggplot(df.demo.filt %>% filter(cc.correct.lex == 2), aes(x = binwts_real, group = best.model.oneatt, fill = best.model.oneatt)) +
  geom_histogram(alpha = .5, position = 'identity')
ggplot(df.feature.binwts, aes(x = best.model.binwts, y = binwts_real.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = binwts_real.m - binwts_real.se, ymax = binwts_real.m + binwts_real.se))
ggplot(df.feature.binwts.cc, aes(x = best.model.binwts, y = binwts_real.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = binwts_real.m - binwts_real.se, ymax = binwts_real.m + binwts_real.se))

# binatts
df.feature.binatts = df.demo.filt %>% group_by(best.model.binatts) %>%
  summarize(binatts_real.m = mean(binatts_real),
            binatts_real.se = se(binatts_real))
df.feature.binatts.cc = df.demo.filt %>% 
  filter(cc.correct.binatts == 2) %>%
  group_by(best.model.binatts) %>%
  summarize(binatts_real.m = mean(binatts_real),
            binatts_real.se = se(binatts_real))

ggplot(df.demo.filt, aes(x = binatts_real, group = best.model.binatts, fill = best.model.binatts)) +
  geom_histogram(alpha = .5, position = 'identity')
ggplot(df.demo.filt %>% filter(cc.correct.lex == 2), aes(x = binatts_real, group = best.model.oneatt, fill = best.model.oneatt)) +
  geom_histogram(alpha = .5, position = 'identity')
ggplot(df.feature.binatts, aes(x = best.model.binatts, y = binatts_real.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = binatts_real.m - binatts_real.se, ymax = binatts_real.m + binatts_real.se))
ggplot(df.feature.binatts.cc, aes(x = best.model.binatts, y = binatts_real.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = binatts_real.m - binatts_real.se, ymax = binatts_real.m + binatts_real.se))


df.demo.filt = df.demo.filt %>% mutate(
  oneatt.correct = chosen.oneatt == best.model.oneatt,
  binwts.correct = chosen.binwts == best.model.binwts,
  binatts.correct = chosen.binatts == best.model.binatts,
  norm.oneatt = factor(lex_norm > 50, c(F,T), one.att.levels),
  norm.binwts = factor(binwts_norm > 50, c(F,T), bin.wts.levels),
  norm.binatts = factor(binatts_norm > 50, c(F,T), bin.wts.levels),
  oneatt.norm.actual = norm.oneatt == best.model.oneatt,
  binwts.norm.actual = norm.binwts == best.model.binwts,
  binatts.norm.actual = norm.binatts == best.model.binatts,
  oneatt.norm.reported = norm.oneatt == chosen.oneatt,
  binwts.norm.reported = norm.binwts == chosen.binwts,
  binatts.norm.reported = norm.binatts == chosen.binatts) %>%
  rowwise() %>%
  mutate(
    num.features.correct = sum(c(oneatt.correct, binwts.correct, binatts.correct), na.rm = T),
    pct.features.correct = mean(c(oneatt.correct, binwts.correct, binatts.correct), na.rm = T),
    pct.features.norm = mean(c(oneatt.norm.actual, binwts.norm.actual, binatts.norm.actual), na.rm = T),
    pct.features.norm.reported = mean(c(oneatt.norm.reported, binwts.norm.reported, binatts.norm.reported), na.rm = T)
  )
get.ci.prop(df.demo.filt$oneatt.correct)
get.ci.prop(df.demo.filt$binwts.correct)
get.ci.prop(df.demo.filt$binatts.correct)

features.graph = df.demo.filt %>% select(subject,oneatt.correct, binwts.correct, binatts.correct, chose.correct.model) %>%
  pivot_longer(!subject) %>%
  group_by(name) %>%
  summarize(val = mean(value,na.rm=T),
            val.se = se(value)) %>%
  mutate(name = factor(name, c('oneatt.correct', 'binwts.correct', 'binatts.correct', 'chose.correct.model')))
ggplot(features.graph, aes(x = name, y = val)) +
  geom_col(color='white') +
  geom_errorbar(aes(ymin = val - val.se, ymax = val + val.se), width = .2, color = 'white') +
  geom_hline(yintercept = .5, color = 'red', linetype = 'dashed') +
  geom_hline(yintercept = 1/6, color = 'white', linetype = 'dashed') +
  theme_black() +
  labs(x = '', y = '% correct') +
  scale_x_discrete(labels = c('Q1\n(one vs.\nmultiple attributes)', 'Q2\n(binary vs.\ngraded weights)', 'Q3\n(binary vs.\ngraded attributes)', 'Overall model'))

# How good or bad were the models that subjects reported using?

rand.lls = numeric(1000)
rand.dists = numeric(1000)
for (i in 1:length(rand.lls)) {
  nrow.cv = nrow(df.cv.norm.filt)
  rnd.choices = sample(length(models),nrow.cv,replace=T)
  test = numeric(nrow.cv)
  test2 = numeric(nrow.cv)
  for (j in 1:nrow.cv) {
    test[j] = df.cv.norm.filt[j,rnd.choices[j]]
    test2[j] = df.demo$best.model.ll.magnitude[j] - df.cv.filt[j,rnd.choices[j]]
  }
  rand.lls[i] = mean(test)
  rand.dists[i] = mean(test2)
}

ggplot(df.demo.filt, aes(x = chosen.model.ll)) +
  geom_histogram(color = 'white', bins = 25) +
  labs(x = "\nOOS likelihood of reported model\n(normalized so worst model = 0, best = 1)",
    y = "# of subjects") +
  scale_y_continuous(breaks = NULL) +
  theme_black() #+
  #geom_vline(xintercept = mean(df.demo.filt$chosen.model.ll), linetype = 1, color = 'gray') +
  #geom_vline(xintercept = mean(rand.lls), linetype = 'dashed', color = 'gray')
get.ci(df.demo.filt$chosen.model.ll)
get.ci(rand.lls)


ggplot(df.demo.filt, aes(x = chosen.model.dist)) +
  geom_histogram(color = 'white', bins = 25) +
  labs(x = "\nDistance of OOS likelihood of reported model\nfrom best model",
       y = "# of subjects") +
  scale_y_continuous(breaks = NULL) +
  theme_black() +
  geom_vline(xintercept = mean(df.demo.filt$chosen.model.dist), linetype = 1, color = 'gray') +
  geom_vline(xintercept = mean(rand.dists), linetype = 'dashed', color = 'gray')

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


# parameter awareness -----------------------------------------------------

# across all points
ggplot(df.attributes.filt, aes(x = rating.chosen.signed, y = fitted.weight.chosen)) +
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

ggplot(df.demo.filt, aes(x = accuracy.best.best)) +
  geom_histogram(alpha = .3, fill = 'gray', color = 'white') +
  #geom_vline(xintercept = mean(rand.errs), linetype = 'dashed', color = 'gray') +
  #geom_vline(xintercept = mean(df.demo.filt$accuracy), linetype = 1, color = 'gray') +
  #labs(x = '\nMean squared error\n(compared to fitted params of reported model)', y = '# of subjects') +
  scale_y_continuous(limits = c(0,25), breaks = NULL) +
  #scale_x_continuous(limits = c(-.05,.55), breaks = c(0, .25, .5, .75)) +
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

get.ci(df.demo.filt$accuracy.best.best)

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

# do 3x3 grid
ggplot(df.demo.filt, aes(x = accuracy.wad.)) +
  geom_histogram(alpha = .3, fill = 'gray', color = 'white') +
  #geom_vline(xintercept = mean(rand.errs), linetype = 'dashed', color = 'gray') +
  #geom_vline(xintercept = mean(df.demo.filt$accuracy), linetype = 1, color = 'gray') +
  #labs(x = '\nMean squared error\n(compared to fitted params of reported model)', y = '# of subjects') +
  scale_y_continuous(limits = c(0,25), breaks = NULL) +
  #scale_x_continuous(limits = c(-.05,.55), breaks = c(0, .25, .5, .75)) +
  theme_black()

## compare process w/ parameter awareness
df.demo.correct = df.demo.filt %>% group_by(chose.correct.model) %>%
  summarize(accuracy.m = mean(accuracy.best.best, na.rm = T), accuracy.se = se(accuracy.best.best))
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

ggplot(df.demo.filt, aes(x = chosen.model.ll, y = accuracy.chosen.chosen)) +
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

# comprehension checks
ggplot(df.demo.filt, aes(x = cc.correct.total, y = chosen.model.ll)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = '\nTotal correct CC questions', y = 'Scaled likelihood of chosen model')
summary(lm(chosen.model.ll ~ cc.correct.total, df.demo.filt))
ggplot(df.demo.filt, aes(x = cc.correct.total, y = chosen.model.dist)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = '\nTotal correct CC questions', y = 'Scaled likelihood of chosen model')
ggplot(df.demo.filt, aes(x = cc.correct.total, y = accuracy.chosen.chosen)) +
  geom_point(color='gray') +
  geom_smooth(method='lm',color='gray') +
  theme_black() +
  labs(x = '\nTotal correct CC questions', y = 'Parameter accuracy')

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

#norms
ggplot(df.demo.filt, aes(x = chosen.model.ll, y = pct.features.norm)) +
  geom_point() +
  geom_smooth(method='lm')
test = df.demo.filt %>% group_by(chose.correct.model) %>%
  summarize(pct.features.norm.m = mean(pct.features.norm),
            pct.features.norm.se = se(pct.features.norm),
            pct.features.norm.reported.m = mean(pct.features.norm.reported),
            pct.features.norm.reported.se = se(pct.features.norm.reported),)
ggplot(test, aes(x = chose.correct.model, y = pct.features.norm.m)) +
  geom_point(size = 5, color = 'white') +
  geom_errorbar(aes(ymin = pct.features.norm.m - pct.features.norm.se,
                    ymax = pct.features.norm.m + pct.features.norm.se),
                width = .2, color = 'white') +
  scale_x_discrete(labels = c('No', 'Yes')) +
  labs(x = 'Reported correct\nmodel', y = '% of strategy properties\nthat were\nnormatively aligned') +
  theme_black()

ggplot(df.demo.filt, aes(x = icar_num_correct, y = accuracy.best.best)) +
  geom_point(color = 'gray') +
  geom_smooth(method='lm', color = 'white') +
  theme_black() +
  labs(x = 'IQ score', y = 'Weight accuracy')

ggplot(df.demo.filt, aes(x = meditation, y = chosen.model.rnd)) +
  geom_violin() +
  stat_summary(fun.data = "mean_se", geom = "pointrange",
               colour = "red")
m.meditation = lm(mse.best ~ meditation, data = df.demo.filt)
summary(m.meditation)

test = df.demo.filt %>% group_by(meditation_exp1) %>%
  summarize(accuracy.best.best.m = mean(accuracy.best.best, na.rm = T),
            accuracy.best.best.se = se(accuracy.best.best),
            chosen.model.ll.m = mean(chosen.model.ll, na.rm = T),
            chosen.model.ll.se = se(chosen.model.ll),
            chose.correct.model.m = mean(chose.correct.model, na.rm = T),
            chose.correct.model.se = se.prop(chose.correct.model))
ggplot(test, aes(x = meditation_exp1, y = chosen.model.ll.m)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = chosen.model.ll.m - chosen.model.ll.se,
                    ymax = chosen.model.ll.m + chosen.model.ll.se,
                    width = .2))
ggplot(test, aes(x = meditation_exp1, y = chose.correct.model.m)) +
  geom_point(size = 5, color = 'white') +
  geom_errorbar(aes(ymin = chose.correct.model.m - chose.correct.model.se,
                    ymax = chose.correct.model.m + chose.correct.model.se),
                width = .2, color = 'white') +
  scale_y_continuous(limits = c(0.1, 0.4), breaks = c(0.1, 0.2, 0.3, 0.4), labels = c('10', '20', '30', '40')) +
  labs(x = '\nPrior meditation experience', y = '% reporting correct model') +
  theme_black()

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
m.mods = lm(accuracy.chosen.chosen ~ decisionstyle + mindfulness + sris.tendency + sris.insight +
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
df.demo.filt.obs = df.demo.filt
save(df.demo.filt.obs, file = 'observer_results.rdata')

# get data for modeling ---------------------------------------------------

write.table(df.s1 %>% dplyr::select(subject.num, all_of(atts.opt1)), 'modeling_opts1.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% dplyr::select(subject.num, all_of(atts.opt2)), 'modeling_opts2.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% dplyr::select(subject.num, orig_choice) %>% mutate(choice = orig_choice + 1, orig_choice = NULL), 'modeling_choice.csv', row.names = F, col.names = F, sep = ",")
write.table(df.avail.atts, 'modeling_avail_atts.csv', row.names = F, col.names = F, sep = ",")


df.s1$first.att = NULL
df.s1$first.maxdiff.att = NULL
for (i in 1:nrow(df.s1)) {
  cur.order = as.numeric.vector(df.s1$atts.order[i])
  df.s1$first.att[i] = which(cur.order == 1)
  for (j in 1:length(cur.order)) {
    cur.att = which(cur.order == j)
    cur.att.name = atts.opt.diff[cur.att]
    if (abs(df.s1[i,cur.att.name]) == max(abs(df.s1[,cur.att.name]))) {
      df.s1$first.maxdiff.att[i] = cur.att
      break
    }
  }
}

write.table(df.s1 %>% dplyr::select(subject.num, first.att), 'first_atts.csv', row.names = F, col.names = F, sep = ",")
write.table(df.s1 %>% dplyr::select(subject.num, first.maxdiff.att), 'first_maxdiff_atts.csv', row.names = F, col.names = F, sep = ",")

# get data for option set -------------------------------------------------

df.s1.cur = df.s1 %>% filter(subject == '60ff2fc7ff799a89b38e6adc')
write.table(df.s1.cur %>% dplyr::select(all_of(atts.opt1)), 'trialset_opts1.csv', row.names = F, col.names = atts, sep = ",")
write.table(df.s1.cur %>% dplyr::select(all_of(atts.opt2)), 'trialset_opts2.csv', row.names = F, col.names = atts, sep = ",")

df.s1.cur = df.s1 %>% filter(subject == '615fb219ac749020fdb60c3c')
write.table(df.s1.cur %>% dplyr::select(all_of(atts.opt1)), 'trialset_opts1_subj2.csv', row.names = F, col.names = atts, sep = ",")
write.table(df.s1.cur %>% dplyr::select(all_of(atts.opt2)), 'trialset_opts2_subj2.csv', row.names = F, col.names = atts, sep = ",")


# for follow-up -----------------------------------------------------------

write(paste(df.demo$assignmentId, collapse = ","), 'followup_list.txt')

to.followup = setdiff(df.demo$assignmentId, df.demo.followup$assignmentId)
write(paste(to.followup, collapse = ","), "to_followup.txt")


# bonuses -----------------------------------------------------------------

bonus_str = ''
for (i in 1:nrow(df.demo)) {
  bonus_str = paste0(bonus_str, df.demo$assignmentId[i], ",", format(df.demo$bonus[i], nsmall = 2), '\n')
}
cat(bonus_str)
