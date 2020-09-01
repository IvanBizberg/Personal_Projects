# Forest Cover Type Prediction


# library ----


library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)
library(ggthemes)

library(randomForest)
library(caret)


# Import data ----

train = read.csv("C:/Users/ivan/Google Drive/Machine learning/Kaggle/Forest Cover Type Prediction/train.csv",
                 sep = ",", header = T)
test = read.csv("C:/Users/ivan/Google Drive/Machine learning/Kaggle/Forest Cover Type Prediction/test.csv",
                 sep = ",", header = T)


# Explore data ----

# Check na
colSums(is.na(train))
colSums(train == "")

# Check data
head(train)
tail(train)
names(train)
glimpse(train)

# Edit data for exploration ----
# Merge data training/testing for exploration
full = bind_rows(train,test) %>% mutate_at(c("Cover_Type"), as.factor)
train %<>%  mutate(Cover_Type = case_when(Cover_Type == 1 ~ "Spruce/Fir",
                                          Cover_Type == 2 ~ "Lodgepole Pine",
                                          Cover_Type == 3 ~ "Ponderosa Pine",
                                          Cover_Type == 4 ~ "Cottonwood/Willow",
                                          Cover_Type == 5 ~ "Aspen",
                                          Cover_Type == 6 ~ "Douglas-fir",
                                          Cover_Type == 7 ~ "Krummholz")) %>%
  mutate_at(c("Cover_Type"), as.factor)


# Prelim Plots ----
# Cover_Type
ggplot(train, aes(-Cover_Type, fill = Cover_Type)) + geom_bar()

# Elevation
gr.mean = train %>% group_by(Cover_Type) %>% dplyr::summarise(ElevationMean = mean(Elevation, na.rm = T))
ggplot(train, aes(x = Elevation , fill = Cover_Type)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = gr.mean, aes(xintercept = ElevationMean, color = Cover_Type),linetype="dashed")
ggplot(train, aes(x = Cover_Type, y = Elevation, fill = Cover_Type)) +
  geom_boxplot(alpha = 0.5)

# Distance_To_Hydrology
ggplot(train, aes(Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, color = Cover_Type)) +
  geom_point(alpha = 0.1, size = 7)

# Aspect
ggplot(train, aes(Aspect, fill = Cover_Type)) + geom_histogram(bins = 20) + coord_polar()

# Wilderness_Area

train %>% select(Cover_Type, Wilderness_Area1 , Wilderness_Area2 ,Wilderness_Area3 ,Wilderness_Area4) %>%
  pivot_longer(-Cover_Type, names_to = "Wilderness_Area", values_to = "count") %>%
  group_by(Cover_Type,Wilderness_Area ) %>%
  summarise(Count = sum(count)) %>%
  mutate(Wilderness_Area = case_when(
    Wilderness_Area == "Wilderness_Area1" ~  "Rawah",
    Wilderness_Area == "Wilderness_Area2" ~  "Neota",
    Wilderness_Area == "Wilderness_Area3" ~  "Comanche Peak",
    Wilderness_Area == "Wilderness_Area4" ~  "Cache la Poudre")) %>%
  ggplot(aes(Wilderness_Area, y = Count, fill=Cover_Type)) +
  geom_bar(stat="identity")

# Soil Types

train %>% select(Cover_Type, starts_with("Soil_Type")) %>% pivot_longer(-Cover_Type, names_to = "Soil_Types", values_to = "Prensence") %>% group_by(Cover_Type, Soil_Types) %>% summarise(Count = sum(Prensence)) %>%
  mutate(Soil_Types = str_sub(Soil_Types, start = 10, end = 11)) %>% arrange(as.numeric(Soil_Types)) %>%
  ggplot(aes(reorder(Soil_Types, as.numeric(Soil_Types)), Count, fill = Cover_Type)) + geom_bar(stat = "identity")

# remove data from memory
rm(gr.mean)


# Prepare data for randomForest ----


# oil_Type7 and Soil_Type15 have zero variance. So, lets not consider them for model building exercise.
names(train)
train %<>% select(-Soil_Type7, -Soil_Type15) %>% mutate_at(12:54, as.factor) %>% glimpse()
test %<>% mutate_at(12:55, as.factor) %>% glimpse()


# Create test data from training ----
set.seed(123)
#Create training set
datatrain = train %>% sample_frac(.70)
#Create test set
datatest  <- anti_join(train, datatrain, by = "Id")

# Checking balance of distribution between datatesting and datatraining set
table(datatrain$Cover_Type)/nrow(datatrain)
table(datatest$Cover_Type)/nrow(datatest)
# The above data shows balanced distribution of Cover Types in devlopment and validation sets.


# Random Forest ----

mod = randomForest(Cover_Type ~ .-Id, data = train, mtry=sqrt(ncol(coverDataDev)), ntree = 300, importance =T, do.trace=25)
print(mod)

Accuracy = 100 - 17.51
plot(mod)
# Feature importance

Importance = importance(mod, type = 2)

# Importance plot

varImpPlot(mod, type = 2, main="Feature Importance", col="steelblue", pch =20)


# Predict on test set ----

prediction = predict(mod, datatest)
# Creating Confusion Matrix
confusionMatrix(data=prediction,
                reference=datatest$Cover_Type,
                positive='yes')

t1<-table(prediction,datatest$Cover_Type)


# Now with the real test set ----
# Create solution file

Prediction = data.frame(Id = test$Id, Cover_Type = prediction)
write.csv(Prediction, file = "C:/Users/ivan/Google Drive/Machine learning/Kaggle/Forest Cover Type Prediction/output.csv",
          row.names = F)
