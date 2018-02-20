library("readr")
YoungPeopleData <- read_csv(file = "C:\\Users\\saich\\Desktop\\IDMProject\\responses.csv")
load("C:\\Users\\saich\\Desktop\\IDMProject\\Survey_Data_Missing_Value_Handled.RData")
df <- survey_data_missing_value_handled
### Gender wise distribution of interest in music
YoungPeopleDataMnMInterest <- YoungPeopleData %>% mutate(musicLikeFlag = ifelse(Music > 3,1,0),
                                                         movieFlag = ifelse(Movies > 3,1,0)) %>%
  filter(Gender %in% c("male","female"))


ggplot(subset(YoungPeopleDataMusicInterest, !is.na(Gender))) + 
  geom_bar(mapping = aes(x = musicLikeFlag,fill = Gender),position = "dodge") + 
  labs(x = "m")

### 1st plot showing the proportions of male and female who like music, we can notice that there is not much 
ggplot() +  
  geom_bar(data = subset(YoungPeopleDataMusicInterest, !is.na(musicLikeFlag)),
           mapping = aes(x = musicLikeFlag, y = ..prop..,fill = Gender),position = "dodge")


### 1st plot showing the proportions of male and female who like movies, we can notice that there is not much 
ggplot() +  
  geom_bar(data = subset(YoungPeopleDataMusicInterest, !is.na(movieFlag)),
           mapping = aes(x = movieFlag, y = ..prop..,fill = Gender),position = "dodge")

### Comparison of different music preferences based on gender
YoungPeopleDataMusic <- YoungPeopleData %>% select(c(Gender,Dance:Opera))


analyze_group_differences <- function(df, group, start, end) {
  avgs_by_group <-  df %>% 
    dplyr::group_by(Gender) %>% 
    dplyr::summarise_all(mean, na.rm = TRUE) %>% 
    na.omit
  
  vars_by_difference <- avgs_by_group %>% 
    select(-Gender) %>% 
    apply(2, function(x) x[1] - x[2]) %>% 
    sort 
  
  namesOfvariable <- names(vars_by_difference)
  
  avgs_by_group %>% 
    gather(Variable, `rating`, -group) %>% 
    ggplot(aes(x = Variable, y = `avg response`, group = group, colour = group)) + 
    geom_point(size = 5) + 
    scale_x_discrete(limits = vars_by_difference) +
    ylim(1, 5) +
    geom_hline(yintercept = 3) +
    coord_flip() + 
    theme(axis.text.y = element_text(face="bold", color="black", size=14),
          legend.position="top")
}


xyz <- survey_data_missing_value_handled %>% analyze_group_differences("Gender", "Horror", "Action")
xyz <- xyz %>% gather(Variable, `avg response`, -group)
colnames(xyz) <- c("Gender","Movie","Rating")

ggplot(data = xyz) + 
  geom_point(mapping = aes(x = Movie,y = Rating,group = Gender,shape = Gender, color = Gender),size = 5) +
  scale_color_manual(values=c("#FF9999",'#56B4E9')) +
  ylim(1, 5) +
  geom_hline(yintercept = 3) +
  labs(x="Movie Preference", y = "Mean Rating") + 
  theme(axis.text.y = element_text(face="bold", color="black", size=10),
        axis.text.x = element_text(face = "bold",color = "black",size = 10),
        legend.position="bottom") + 
  coord_flip()


phobias <- workingData %>% analyze_group_differences("Gender", "Pets", "Rats")
phobias <- phobias %>% gather(Variable, `avg response`, -group)
colnames(phobias) <- c("Gender","phobias","Rating")

ggplot(data = phobias) + 
  geom_point(mapping = aes(x = phobias,y = Rating,group = Gender,color = Gender,shape = Gender),size = 5) +
  scale_color_manual(values=c("#FF9999",'#56B4E9')) +
  ylim(1, 5) +
  geom_hline(yintercept = 3) +
  labs(x="Phobias", y = "Mean Rating") +
  theme(axis.text.y = element_text(face="bold", color="black", size=10),
        axis.text.x = element_text(face = "bold",color = "black",size = 10),
        legend.position="bottom") + 
  coord_flip()

save(workingData,file = "C:\\Users\\saich\\Desktop\\IDMProject\\final.RData")
load("C:\\Users\\saich\\Desktop\\IDMProject\\final.RData")

workingData$Gender <- ifelse(workingData$Gender == "male",1,0)
workingDataTrain <- workingData[1:707,]
workingDataTest <- workingData[708:1010,]
glmObject <- glm(formula = Gender ~ Romantic + `Fantasy/Fairy tales` + Animated  + `Sci-fi` +
                   Western + Action + War + Storm + Spiders + Snakes + Rats + Darkness, data = workingDataTrain,
                 family = "binomial")

predictProbs <- predict(object = glmObject, newdata = workingDataTest,type = "response")
predictions <- ifelse(predictProbs > 0.5,1,0)
accuracy <- sum(predictions == workingDataTest$Gender) / length(workingDataTest$Gender)


predictProbs <- predict(object = glmObject, newdata = workingDataTrain,type = "response")
predictions <- ifelse(predictProbs > 0.5,1,0)
accuracy <- sum(predictions == workingDataTrain$Gender) / length(workingDataTrain$Gender)
workingData <- survey_data_missing_value_handled

#workingData <- workingData %>% select(-c(Punk,Country,Alternative,`Reggae, Ska`,`Swing, Jazz`))
#workingData <- workingData[,-c(Punk,Country,Alternative,`Reggae, Ska`,`Swing, Jazz`)]
workingData %>% analyze_group_differences("Gender", "Dance", "Opera")

df %>% analyze_group_differences("Gender", "Horror", "Action")

YoungPeopleDataWithoutNA <-  na.omit(YoungPeopleData)
YoungPeopleDataWithoutNA$Gender <- ifelse(YoungPeopleDataWithoutNA$Gender == "male",1,0)

YoungPeopleData$Gender <- ifelse(YoungPeopleDataWithoutNA$Gender == "male",1,0)

glmObject <- glm(formula = Gender ~ Romantic + `Fantasy/Fairy tales` + Animated + Horror + Thriller + `Sci-fi` +
                   Western + Action + War + Latino + Musical + Pop + `Hiphop, Rap` 
                 + `Techno, Trance` + `Metal or Hardrock`,data = YoungPeopleDataWithoutNAtraining,family = "binomial")


predictProbs <- predict(object = glmObject, newdata = YoungPeopleDataWithoutNAtest,type = "response")
predictions <- ifelse(predictProbs > 0.5,1,0)
accuracy <- sum(predictions == YoungPeopleDataWithoutNAtest$Gender) / length(YoungPeopleDataWithoutNAtest$Gender)


YoungPeopleDataWithoutNAtraining <- YoungPeopleDataWithoutNA[1:500,]

YoungPeopleDataWithoutNAtest <- YoungPeopleDataWithoutNA[501:674,]


