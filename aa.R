lm_sep1 <- lm(nn ~ ef1+ ef3+ef4, data = cp_tot)
summary(lm_sep1)

lm_sep2 <- lm(nn ~ ef5+ ef6+ef7+ef8, data = cp_tot)
summary(lm_sep2)

lm_sep3 <- lm(nn ~ ef9+ ef10+ef11+ef12+ef13, data = cp_tot)
summary(lm_sep3)

lm_sep4 <- lm(nn ~ ef14+ ef15+ef16, data = cp_tot)
summary(lm_sep4)

lm_sep5 <- lm(nn ~ ef17+ ef18+ef19, data = cp_tot)
summary(lm_sep5)

lm_sep6 <- lm(nn ~ ef20+ ef21+ef22+ef23+ef24, data = cp_tot)
summary(lm_sep6)

lm_sep7 <- lm(nn ~ ef25+ ef26+ef27, data = cp_tot)
summary(lm_sep7)

lm_sep8 <- lm(nn ~ ef28+ ef29+ef30+ef31+ef32+ef33+ef34+ef35+ef36+ef37+ef38, data = cp_tot)
summary(lm_sep8)

lm_sep9 <- lm(nn ~wf, data = cp_tot)
summary(lm_sep9)









### 랜덤포레스트로 아동학대 위험 분류 모델 만들기 ####
cp_tot$nn
summary(cp_18_r$nn)
summary(cp_17_r$nn)
qc <- quantile(cp_tot$nn)
qc
cp_tot <- cp_tot %>% mutate(nc = ifelse(nn >= qc[4], 3, ifelse(nn >= qc[2], 2,  1)))  # factor형 변수 3: 주의 지역 / 2: 일반지역 1 : 안전지역
cp_tot$nc
cp_tot1 <- cp_tot[, -38]                            
cp_tot1$nc <- as.factor(cp_tot1$nc)

idx <- sample(1:nrow(cp_tot), nrow(cp_tot) * .8)
rf.train <- cp_tot1[idx,]
rf.test <- cp_tot1[-idx,]
rf.test
cp_tot1
rf.train

# randomforest
rf_model <- randomForest(nc ~ ., data = rf.train, mtry = 2, ntree = 1000, importance = T)
rf_pred <- predict(rf_model, newdata = rf.test)
confusionMatrix(rf_pred, rf.test$nc)

varImpPlot(rf_model)

# radomForest with cross validataion
fit_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

rf_fit <- train(nc ~ ., data = rf.train, method = "rf", trControl = fit_control, verbose = T)
rf_fit
plot(rf_fit)

rf_pred <- predict(rf_fit, newdata = rf.test) 
summary(rf_pred)
confusionMatrix(rf_pred, rf.test$nc)

roc <- roc(as.numeric(rf_pred), as.numeric(rf.test$nc))
plot.roc(roc, col = "red", print.auc = T,
         max.auc.polygon = T, print.thres = T,
         print.thres.pch = 19, print.thres.col = "red",
         auc.polygon = T, auc.polygon.col="#D1F2EB")


### SVM -> 랜덤포레스트 모델링 비교를 위해 

# 분석 실행
svm <- ksvm(nc ~ ., rf.train, kernel = "rbfdot")

# 예측 및 결과
svm.pred <- predict(svm, rf.test, type = "response")
sc <- confusionMatrix(svm.pred, rf.test$nc)
sc

### 2019년 아동학대 주의 지역 예측(randomforest with CV)
cp_19 <- read.csv("cp_19.csv", header = T)

cp_19_r <- cp_19 %>% mutate(ef3 = ef3 / pop * 1000, ef4 = ef4 / pop * 1000, ef5 = ef5 / pop * 1000, ef6 = ef6 / pop * 1000, ef7 = ef7 / pop * 1000, ef8 = ef8 / pop * 1000, ef9 = ef9 / pop * 1000, ef10= ef10/ pop * 1000, ef11= ef11/ pop * 1000, ef12= ef12/ pop * 1000, ef13= ef13/ pop * 1000, ef14= ef14/ pop * 1000, ef15= ef15/ pop * 1000, ef16= ef16/ pop * 1000, ef17= ef17/ pop * 1000, ef18= ef18/ pop * 1000, ef19= ef19/ pop * 1000, ef20= ef20/ pop * 1000, ef21= ef21/ pop * 1000, ef22= ef22/ pop * 1000, ef23 = ef23 / pop * 1000, ef24= ef24/ pop * 1000, ef38= ef38/ pop * 1000, wf= wf / pop * 1000)
cp_19_r

name <- cp_19_r[1]
cp_19_r <- cp_19_r[, c(-1, -2, -3, -5)]
cp_19_r
name

final_pred <- predict(rf_fit, newdata = cp_19_r)
table(final_pred)
final <- name %>% mutate(level = final_pred)
final

write.csv(final, file = "predcit_19.csv")

