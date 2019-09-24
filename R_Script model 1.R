library(readxl)
library(dplyr)
library(MLmetrics)
library(corrplot)
library(caret)
library(foreach)
library(ROCR)
library(InformationValue)
library(Information)


data <- read_xlsx('factors model 1.xlsx')

#Переводим дату в формат, который понимает R. 
#Нужно еще предварительно произвести перобразование в Excel

data <- filter(data, data[,'otrasl_sector']!='Госорганы', data[,'otrasl_sector']!='Финансовая')

sum(data[,'def']==1)

table(data[,'report_date'])

a <- group_by(data, report_date) %>% summarise(sum(def))
write_xlsx(a,'defaults.xlsx')


data <- data %>% filter(bal!='0')
data <- data %>% mutate(def_date=as.Date(data$def_date, format='%d/%m/%y'),report_date=as.Date(data$report_date, format='%d/%m/%y'),bal=as.factor(data$bal))
data <- data[order(data$bal),]
n_occur_a <- data.frame(table(data$bal))
n_occur_a <- n_occur_a[n_occur_a$Freq>1,]
n_occur_a <- data.frame(bal=n_occur_a$Var1, Freq=n_occur_a$Freq)

id_a <- data.frame(id=data$id, def_date=data$def_date , bal=as.factor(data$bal))

id_f_a <- left_join(id_a,n_occur_a)
id_f_a <- id_f_a[order(id_f_a$bal),]

rm(n_occur_a, id_a)

#Производится очистка от одинаковой консолидированной отчетности
#По группе выставляется минимальная дата дефолта (если такая есть)

#Присваивание каждому наблюдению номер группы, на основе его баланса
id_f_a[,'group'] <- as.numeric(id_f_a$Freq)
a <- 1
for (i in 2:nrow(id_f_a)) {
        if(id_f_a[i,'bal']==id_f_a[i+1,'bal'] && id_f_a[i,'bal']!=id_f_a[i-1,'bal']) {
          a <- a+1
          id_f_a[i,'group']=a
        } else if (id_f_a[i,'bal']==id_f_a[i+1,'bal']) {
          id_f_a[i,'group']=a
          } else if (id_f_a[i,'bal']!=id_f_a[i+1,'bal'] && id_f_a[i,'bal']==id_f_a[i-1,'bal']) {
            id_f_a[i,'group']=a
          } else {
            a <- a+1  
            id_f_a[i,'group']=a
    }
id_f_a[nrow(id_f_a),'group']=a+1
id_f_a[1,'group']=1
  }
rm(a,i)

#Проставление минимальной даты дефолта по группе
id_f_a[,'def_true'] <- as.Date(id_f_a$def_date, format='%d/%m/%y')
a <- 1
for (i in 1:nrow(id_f_a)) {
  if (id_f_a[i,'bal']==id_f_a[i+1,'bal']) {
    id_f_a[i,'def_true']=min(id_f_a[id_f_a$group==a,'def_date'], na.rm=T)
  } else if (is.na(id_f_a[i,'bal']!=id_f_a[i+1,'bal'] && id_f_a[i,'bal']==id_f_a[i-1,'bal'])) {
    id_f_a[i,'def_true']=min(id_f_a[id_f_a$group==a,'def_date'], na.rm=T)
    a <- a+1
  }  else if (id_f_a[i,'bal']!=id_f_a[i+1,'bal'] && id_f_a[i,'bal']==id_f_a[i-1,'bal']) {
    id_f_a[i,'def_true']=min(id_f_a[id_f_a$group==a,'def_date'], na.rm=T)
    a <- a+1
  } else {
    id_f_a[i,'def_true']=min(id_f_a[id_f_a$group==a,'def_date'], na.rm=T)
    a <- a+1  
  }
}
rm(a,i)

#Проставление правильного флага дефолта (def), согласно минимальным датам дефолта по кластеру
data_0 <- data.frame(group=id_f_a$group,def_true=id_f_a$def_true,data)
sum(data_0[,'def']==1)

rm(id_f_a, data)

for (i in 1:nrow(data_0)) {
  if (is.na(data_0[i,'def_true'])) {
    data_0[i,'def']=0
  } else if (as.numeric(data_0[i,'def_true']-data_0[i,'report_date'])<368 && as.numeric(data_0[i,'def_true']-data_0[i,'report_date'])>0) {
    data_0[i,'def']=1
  } else if (as.numeric(data_0[i,'report_date']-data_0[i,'def_true'])>=0) {
    data_0[i,'def']=2
  } else {
    data_0[i,'def']=0
  }
}


#Выбор уникальных отчетностей согласно номеру группы
a <- 1
for (i in 1:nrow(data_0)) {
    if (data_0[i,'group']==data_0[i+1,'group']) {
      data_0[a,]=data_0[i,]  
    } else if (is.na(data_0[i,'group']!=data_0[i+1,'group'] && data_0[i,'group']==data_0[i-1,'group'])) {
      data_0[a,]=data_0[i,]
      a <- a+1
    }  else if (data_0[i,'group']!=data_0[i+1,'group'] && data_0[i,'group']==data_0[i-1,'group']) {
      data_0[a,]=data_0[i,]
      a <- a+1
    } else {
      data_0[a,]=data_0[i,]
      a <- a+1  
    }
  }
data_0[a,]=data_0[i,]
data_0 <- data_0[1:a,]
data_0 <- data_0 %>% filter(def!=2)

#87 def
sum(data_0[,'def']==1)


#Проставление номера группы согласно id
data_0 <- data_0[order(data_0$id, data_0$report_date),]
a <- 1
for (i in 2:nrow(data_0)) {
  if(data_0[i,'id']==data_0[i+1,'id'] && data_0[i,'id']!=data_0[i-1,'id']) {
    a <- a+1
    data_0[i,'group']=a
  } else if (data_0[i,'id']==data_0[i+1,'id'] && data_0[i, 'def']==0) {
    data_0[i,'group']=a
  } else if (data_0[i,'id']!=data_0[i+1,'id'] && data_0[i,'id']==data_0[i-1,'id'] && data_0[i, 'def']==0) {
    data_0[i,'group']=a
  } else {
    a <- a+1
    data_0[i,'group']=a
  }
  data_0[nrow(data_0),'group']=a+1
  data_0[1,'group']=1
}
rm(a,i)

#Выбор одной отчетности для одного клиента
a <- 1
for (i in 1:nrow(data_0)) {
  if (data_0[i,'group']==data_0[i+1,'group']) {
    data_0[a,]=data_0[i,]
  } else if (is.na(data_0[i,'group']!=data_0[i+1,'group'] && data_0[i,'group']==data_0[i-1,'group'])) {
    data_0[a,]=data_0[i,]
    a <- a+1
  }  else if (data_0[i,'group']!=data_0[i+1,'group'] && data_0[i,'group']==data_0[i-1,'group']) {
    data_0[a,]=data_0[i,]
    a <- a+1
  } else {
    data_0[a,]=data_0[i,]
    a <- a+1
  }
}
data_0[a,]=data_0[i,]
data_0 <- data_0[1:a,]
rm(a,i)

#83 def
sum(data_0[,'def']==1)

#Выбор отчетности не позднее 2013 года по недефольной отчетности и 2014 года по дефолтной
data_y <- data_0 %>% filter(def==0,report_date>='2013-01-01')
data_yy <- data_0 %>% filter(def==1, report_date>='2014-01-01')
data_0 <- rbind(data_y,data_yy)
rm(data_y, data_yy) 

#Изучение default rate
sum(data_0[,'def']==1)


#Переименовываются переменные для удобства работы с ними
#СЛЕДИТЬ ЗА КОЛИЧЕСТВОМ ПЕРЕМЕННЫХ ПРИ ОТБОРЕ СТОЛБЦОВ ДЛЯ df
df <- data_0[,14:ncol(data_0)]

for (i in 1:ncol(df)) {
  names(df)[i]<-paste("X", i, sep="")
}

data_1 <- cbind.data.frame(data_0['def'], df)
rm(df)

#Анализ на пропущенные значения
na_0 <- data.frame(factors=names(data_1),number_of_0=round(apply(data_1, 2, function(x){
  sum(is.na(x)/nrow(data_1))
}
),3))

na_1 <- na_0 %>% filter(na_0$number_of_0<0.1)
data_x <- select(data_1,one_of(as.character(na_1[,1])))

# #Использовать данный блок кода, если будет принято решение работать с качественными переменными
# df1 <- data_0[,87:92]
# 
# for (i in 1:6) {
#   names(df1)[i]<-paste("X", i+74, sep="")
# }
# 
# data_q <- cbind.data.frame(data_0['def'], df1)
# rm(df1)
# 
# na_q <- data.frame(factors=names(data_q),number_of_0=round(apply(data_q, 2, function(x){
#   sum(is.na(x)/nrow(data_q))
# }
# ),3))
# 
# na_1q <- na_q %>% filter(na_q$number_of_0<0.3)
# data_xq <- select(data_q,one_of(as.character(na_1q[,1])))
# 
# rm(data_1, na_0, na_1)

data_2 <- data_x

#Удаление выбросов для каждого столбца через +/- 1.5IQR к 75% и 25% квантилям соответственно
for (i in 1:nrow(data_2)) {
  for (j in 2:ncol(data_2)) {
    if (is.na(data_2[i,j])) {
      data_2[i,j] <- NA
    } else if (data_2[i,j]>(quantile(data_2[[j]], 0.75, na.rm=T)+1.5*(quantile(data_2[[j]], 0.75,na.rm=T)-quantile(data_2[[j]], 0.25,na.rm=T)))) {
      data_2[i,j]=(quantile(data_2[[j]], 0.5, na.rm=T))
    } else if (data_2[i,j]<(quantile(data_2[[j]], 0.25, na.rm=T)-1.5*(quantile(data_2[[j]], 0.75, na.rm=T)-quantile(data_2[[j]], 0.25, na.rm=T)))) {
      data_2[i,j]=(quantile(data_2[[j]], 0.5, na.rm=T))
    } else {
      data_2[i,j]=data_2[i,j]
    }
  }
}

#Заполнение дефолтных пустых наблюдений средними по отрасли
data_sector<-cbind(data_0$otrasl_sector,data_0$id,data_2)
subsdef<-subset(data_sector,data_sector$def==1)
subsdef_f<-data.frame(id=subsdef$`data_0$id`)

for (i in 4:ncol(subsdef)) {
  df_fact<-cbind(subsdef[2],subsdef[1],subsdef[i])
  names(df_fact)[3]= "fact"
  names(df_fact)[2]= "otrasl"
  df_summ<- group_by (df_fact, otrasl) %>% summarize(mean(fact, na.rm=T))
  df_fact<-merge(df_fact,df_summ,by="otrasl",all.x=TRUE)
  
  for (j in 1:nrow(df_fact)) {
    if(is.na(df_fact[j,3])){
      df_fact[j,3]=df_fact[j,4]
    }
    else {
      df_fact[j,1]=df_fact[j,1]
    }
  }
  
  df_fact<-df_fact[2:3]
  names(df_fact)[1]= "id"
  subsdef_f <- subsdef_f[order(subsdef_f[,1]),]
  df_fact <- df_fact[order(df_fact[,1]),]
  subsdef_f<-cbind(subsdef_f,df_fact)
  subsdef_f <- subsdef_f[,-i+2]
  names(subsdef_f)[i-2]= paste("y",i-3, sep="")
}

subsdef_f[1]<-1

data_2<-subset(data_2,data_2$def==0)
names(subsdef_f)= names(data_2)
data_2<-rbind(data_2,subsdef_f)

rm(data_sector,subsdef,subsdef_f,df_fact,df_summ)

#Gini one factor
models <- apply(data_2, 2, function(x) {
  return(glm(data_2$def~x, family = binomial(link='logit')))
})

gini_0 <- sapply(models, function(x) {
  return(Gini(x$fitted.values,x$y))
})

gini_0 <- data.frame(factors=names(data_2),gini_0)
gini_1 <- gini_0 %>% filter(gini_0>0.05)
gini_1 <- gini_1[order(gini_1$gini_0,decreasing=TRUE),]
data_2 <- data_2 %>% select(one_of(as.character(gini_1[,1])))


#S-shape
library(caret)
data_n <- na.omit(data_2)

alpha <- 0.1
c.coef <- function(x, alpha) 2*log(alpha/(1-alpha))/(quantile(x,1-alpha)-quantile(x,alpha))
d.coef <- function(x, alpha) log(alpha/(1-alpha))-quantile(x,1-alpha)*(2*log(alpha/(1-alpha))/(quantile(x,1-alpha)-quantile(x,alpha)))

c.s <- apply(data_n[-1], 2, c.coef, alpha=alpha)
d.s <- apply(data_n[-1], 2, d.coef, alpha=alpha)

data_s <- data.frame(mapply(function(x,c,d) 1/(1+exp(c*x+d)), data_n[-1], c.s, d.s))

#Нормализация (опционально)
# library(BBmisc)
# data_s <- apply(data_s, 2, normalize)

data_s <- data.frame(def=data_n$def,data_s)
data_n<-data_s


#Correlation analysis
cor <- data.frame(cor(data_n[,-1]))
cor(data_n[,-1], use='complete.obs') %>% corrplot(method='number', type='lower', tl.pos='u', tl.cex=0.7, tl.col='black')


corr_filter <- function(x) {
  a <- 1
  df <- data.frame(matrix(0, ncol=2, nrow=ncol(x)*nrow(x)))
  names(df) <- c('factors', 'corr')
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      if (i<j & abs(x[i,j])>0.5) {
        df[a,] <- c(names(x[j]),x[i,j]) 
        a <- a+1
      }
    }
  }
  rm(i,j,a,x)
  df[df==0] <- NA
  df <- na.omit(df)
  return(df)
} 

#ИСКЛЮЧАТЬ ПЕРЕМЕННЫЕ МОЖНО ПО GINI ИЛИ IV, НЕ ОБЯЗАТЕЛЬНО ВЫКИДВАТЬ ПЕРВУЮ ПЕРЕМЕННУЮ С ВЫСОКОЙ КОРРЕЛЯЦИЕЙ
#ВОЗМОЖНО ПРИДЕТСЯ ПОСЧИТАТЬ VIF
c <- corr_filter(cor)
d <- data.frame(fact=unique(c$factors))

data_3 <-  select(data_n,-one_of(as.vector(d$fact)))

cor(data_3[,-1], use='complete.obs') %>% corrplot(method='number', type='lower', tl.pos='u', tl.cex=0.7, tl.col='black')



#Регрессионный анализ

#Разделение на обучающую и тренировочную выборки
 # set.seed(228)
 # in_train <- createDataPartition(data_3$def,p=0.7, list=F)
 # dt_train <- data_3[in_train,]
 # dt_test <- data_3[-in_train,]


#Кросс-валидация
train_control <- trainControl(method='cv', number=5)

registerDoSEQ()

fmla <- as.formula(paste("def ~", paste(names(data_3[-1]), collapse="+")))
model_cv <- train(fmla, data=data_3, trControl=train_control, method='glmStepAIC')

#Финальная модель
log_model_0 <- glm(data=data_3, def~X42 + X52 + X59 + X68, family=binomial(link='logit'))
summary(log_model_0)

#Анализ качества модели
pred_log <- predict(log_model_0, data_3, type='response')
plotROC(data_3$def, pred_log)
Gini(pred_log, data_3$def)