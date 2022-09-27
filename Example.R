
#Data import/cleaning

raw_data<-read_csv('Sample Agree.csv')

#Name columns
names(raw_data)<-c('Guideline_ID','Domain','Scorer_1','Scorer_2')

#Rename domains

raw_data<- raw_data %>% 
  mutate(Domain = case_when(
    Domain == '1' ~ 'Scope and purpose',
    Domain == '2' ~ 'Stakeholder involvement',
    Domain == '3' ~ 'Rigour of development',
    Domain == '4' ~ 'Clarity of presentation',
    Domain == '5' ~ 'Applicability',
    Domain == '6' ~ 'Editorial independence'
  ))

#Convert to a weighted score as per AGREE instructions

raw_data<-raw_data %>% group_by(Guideline_ID,Domain) %>% summarise(R1 = sum(Scorer_1), R2=sum(Scorer_2))

raw_data$consensus<-(raw_data$R1 + raw_data$R2)

max_scores<-read_csv('max_scores.csv')

raw_data<-left_join(raw_data,max_scores,by=c("Domain"="Domain"))

raw_data$min<-(raw_data$one_reviewer/7)*2
raw_data$weighted<-(raw_data$consensus - raw_data$min)/(raw_data$two_reviewers - raw_data$min)

raw_data$weighted<-round(raw_data$weighted,2)

#Melina's violin plot}

raw_data$Domain<-factor(raw_data$Domain,levels = c('Scope and purpose','Stakeholder involvement','Rigour of development','Clarity of presentation','Applicability','Editorial independence'))

ggplot(raw_data) +
  aes(x=Domain,y=weighted,colour=Domain) +
  geom_jitter() +
  geom_violin(size=0.5, alpha=0.5) +
  ylab('Score')+xlab('Domain') +    theme_light()+
  theme(axis.text.x = element_text(angle = 60, size=15,hjust = 1)) +
  theme(legend.title = element_text(size =15), legend.text = element_text(size = 15))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15,face="bold"))

#Calculate Median (IQR)
agree_scores <- raw_data %>% group_by(Domain) %>% summarise(q1=quantile(weighted,probs=c(0.25)), q2=quantile(weighted,probs=c(0.5)), q3=quantile(weighted,probs=c(0.75)))
agree_scores


