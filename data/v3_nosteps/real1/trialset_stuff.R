setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

require(dplyr)
require(ggcorrplot)

trialset1 = read.csv('trialset_opts1.csv')
trialset2 = read.csv('trialset_opts2.csv')

for (i in 1:85) {
  opts = sample(5,2)
  while (abs(opts[1] - opts[2]) < 2) {
    opts = sample(5,2)
  }
  
  trialset1$Washer.dryer.in.unit[i] = opts[1]
  trialset2$Washer.dryer.in.unit[i] = opts[2]
}

trialset1 = trialset1 %>% rename(Quality.of.kitchen = Washer.dryer.in.unit)
trialset2 = trialset2 %>% rename(Quality.of.kitchen = Washer.dryer.in.unit)

df.cors = round(cor(trialset1),1)
ggcorrplot(df.cors, type = 'lower', lab = T)

write.table(trialset1, 'trialsets_opt1.txt', row.names = F, sep = ",")
write.table(trialset2, 'trialsets_opt2.txt', row.names = F, sep = ",")
