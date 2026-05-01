library(ggplot2)
library(patchwork)
library(corrplot)
library(dplyr)
library(glmnet)
library(caret)
#----------------------------------------------------------
# TIỀN XỬ LÝ DỮ LIỆU
#----------------------------------------------------------
# 3.1 Đọc dữ liệu, làm sạch dữ liệu và gán tên biến
#----------------------------------------------------------
original_data <- read.csv("add.csv", na.strings = "?", strip.white = TRUE)

# Xóa cột đầu vì cột đầu là cột thứ tự
original_data <- original_data[,-1]

# Đổi tên cột cho dễ làm việc (đặt tên theo thứ tự: Height, Width, Ratio, Local, url_, origurl_, ancurl_, alt_, caption_, Class)
colnames(original_data) <- c(
    "Height", "Width", "Ratio", "Local",
    paste0("url_", 1:457),
    paste0("origurl_", 1:495),
    paste0("ancurl_", 1:472),
    paste0("alt_", 1:111),
    paste0("caption_", 1:19),
    "Class"
)

# In ra các thông tin của dataset
get_data <- function(data) {
    # Kích thước dữ liệu
    cat("Kích thước dimension của dữ liệu sau khi đã đọc:\n")
    print(dim(data))

    # Nếu dataset là original thì in ra 5 cột đầu + Class, nếu là dataset đã chọn biến phân tích thì in ra toàn bộ
    cat("\nBảng dữ liệu các cột đầu tiên và cột Class:\n")
    if (ncol(data) > 10) {
        # Kiểm tra kiểu dữ liệu của các cột Height, Width, Ratio, Local và Class
        cat("Kiểu dữ liệu của các cột:\n")
        print(sapply(data[, c("Height", "Width", "Ratio", "Local", "Class")], class))
        cat("\n")
        data_display <- data.frame(
            data[, 1:5],
            "..." = "...",
            "Class" = data[, ncol(data)]
        )
    } else {
        data_display <- data
    }
    print(head(data_display))
    cat("\n")

    str(data, list.len = 10)
}

#----------------------------------------------------------
# 3.2 Chọn biến phân tích, xóa và xử lý định dạng
#----------------------------------------------------------
# Đưa tất cả các cột về kiểu phù hợp: 3 cột đầu là numeric, cột Local, url_, origurl_, ancurl_, alt_, caption_ là factor, và cột Class là sẽ convert ad = 1, nonad = 0
original_data[,1:3] <- lapply(original_data[,1:3], as.numeric)
original_data[,4:ncol(original_data)] <- lapply(original_data[,4:ncol(original_data)], as.factor)
original_data$Class <- ifelse(original_data$Class == "ad.", 1, 0)
original_data$Class <- as.factor(original_data$Class)

# Chọn biến phân tích
analysis_data <- original_data[, c("Height", "Width", "Local", "Class")]

#----------------------------------------------------------
# 3.3 Xử lý số liệu
#----------------------------------------------------------
# Kiểm tra xem có bao nhiêu giá trị thiếu (NA) trong 4 cột đầu và cột Class
check_na <- function(data) {
    cat("Các cột có giá trị thiếu (NA): ")
    cat(colnames(data)[which(colSums(is.na(data)) > 0)])
    # Loại bỏ cột ratio vì cột này dựa trên height và width
    data$ratio <- NULL
    cat("\nSố lượng giá trị thiếu (NA) trong các cột quan trọng:\n")
    # số NA theo cột
    na_count <- colSums(is.na(data))
    # chỉ giữ các cột có NA
    na_cols <- na_count[na_count > 0]
    # tính tỷ lệ %
    na_ratio <- (na_cols / nrow(data)) * 100
    # tạo bảng kết quả
    result <- data.frame(
        Column = names(na_cols),
        NA_Count = na_cols,
        NA_Ratio_Percent = round(na_ratio, 2)
    )
    print(result, row.names = FALSE)
    cat("\n")
}

# Xử lý giá trị thiếu: thay thế NA bằng median
# return: dataset đã được thay thế NA
na_replace <- function(data) {
    data$Height[is.na(data$Height)] <- median(data$Height, na.rm = TRUE)
    data$Width[is.na(data$Width)] <- median(data$Width, na.rm = TRUE)
    data$Local[is.na(data$Local)] <- as.factor(median(as.numeric(data$Local) - 1, na.rm = TRUE))
    return(data)
}

# Loại bỏ các bản ghi trùng lặp trong dataset
# return: dataset đã được loại bỏ trùng lặp
get_unique_dataset <- function(data) {
    sum(duplicated(data))
    print(paste("Số lượng bản ghi trùng lặp:", sum(duplicated(data))))
    cat("\n")
    data <- data[!duplicated(data), ]
    # Lưu data vào unique_data sau khi đã loại bỏ trùng lặp
    return(data)
}

# Vẽ đồ thị histogram và boxplot cho các cột Height, Width và Local
draw_histograms_and_boxplots <- function(data) {
    # Histogram height
    p1<-ggplot(data, aes(x = Height)) +
    geom_histogram(binwidth = 21.333, fill = "lightblue", color = "black",na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Histogram of Height", x = "Height", y = "Frequency")

    # Boxplot height
    p2<-ggplot(data, aes(x = Height)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Boxplot of Height", x = "Height")

    # Histogram width
    p3<-ggplot(data, aes(x = Width)) +
    geom_histogram(binwidth = 21.333, fill = "orange", color = "black",na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Histogram of Width", x = "Width", y = "Frequency")

    # Boxplot width
    p4<-ggplot(data, aes(x = Width)) +
    geom_boxplot(fill = "orange",na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Boxplot of Width", x = "Width")

    # chuyển thành factor nếu chưa
    data$Local <- as.factor(data$Local)
    p5<-ggplot(data, aes(x = Local)) +
    geom_bar(fill = "lightgreen", color = "black", na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Distribution of Local Variable",
        x = "Local (0 = external, 1 = internal)",
        y = "Count")

    (p1 + p2) / (p3 + p4 ) / (plot_spacer() + p5 + plot_spacer())
}

#----------------------------------------------------------
# 3.4 Kiểm tra và xử lý ngoại lai (outliers)
#----------------------------------------------------------
# Lấy các giá trị outliers cho các cột Height, Width và Ratio
get_all_outliers <- function(data) {
    cat("--- Thống kê ngoại lai (Outliers) ---\n")
    calc_outlier <- function(x) {
        if(!is.numeric(x)) return(c(0, 0))
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        outliers <- x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)]
        return(c(length(outliers), round((length(outliers) / length(x)) * 100, 2)))
    }
    
    outlier_df <- data.frame(
        Column = c("Height", "Width"),
        Count = c(calc_outlier(data$Height)[1], calc_outlier(data$Width)[1]),
        Percentage = c(calc_outlier(data$Height)[2], calc_outlier(data$Width)[2])
    )
    print(outlier_df, row.names = FALSE)
}

# Xử lý ngoại lai

#----------------------------------------------------------
# THỐNG KÊ MÔ TẢ
#----------------------------------------------------------
# 4.1 Thống kê mô tả tổng quát
#----------------------------------------------------------
# Lấy mean, median, mode, min, max cho các cột Height, Width, Local và Class
get_summary_stats <- function(data) {
    # Hàm tìm Mode
    get_mode <- function(v) {
        uniqv <- unique(na.omit(v))
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    # Xử lý nhóm Định lượng (Height, Width)
    cols_num <- c("Height", "Width")
    stats_num <- t(sapply(data[, cols_num], function(x) {
        c(
        "Mean"     = mean(x, na.rm = TRUE),
        "Median"   = median(x, na.rm = TRUE),
        "Std.Dev"  = sd(x, na.rm = TRUE),
        "Variance" = var(x, na.rm = TRUE),
        "Min"      = min(x, na.rm = TRUE),
        "Max"      = max(x, na.rm = TRUE),
        "Q1"       = quantile(x, 0.25, na.rm = TRUE),
        "Q3"       = quantile(x, 0.75, na.rm = TRUE)
        )
    }))

    # Xử lý nhóm Định tính (Local, Class)
    cols_fac <- c("Local", "Class")
    stats_fac <- t(sapply(data[, cols_fac], function(x) {
        c(
        "Mode"    = get_mode(x),
        "Count_0" = sum(x == 0, na.rm = TRUE),
        "Count_1" = sum(x == 1, na.rm = TRUE)
        )
    }))

    # Hiển thị kết quả
    cat("\n--- THỐNG KÊ BIẾN ĐỊNH LƯỢNG (HEIGHT, WIDTH) ---\n")
    print(round(as.data.frame(stats_num), 3))
    
    cat("\n--- THỐNG KÊ BIẾN ĐỊNH TÍNH (LOCAL, CLASS) ---\n")
    print(as.data.frame(stats_fac))
}

#----------------------------------------------------------
# 4.2 Trực quan hóa dữ liệu
#----------------------------------------------------------
# Vẽ đồ thị histogram cho các cột Height, Width
draw_histograms <- function(data) {
    # Histogram height
    p1<-ggplot(data, aes(x = Height)) +
    geom_histogram(binwidth = 21.333, fill = "lightblue", color = "black",na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Histogram of Height", x = "Height", y = "Frequency")

    # Histogram width
    p3<-ggplot(data, aes(x = Width)) +
    geom_histogram(binwidth = 21.333, fill = "orange", color = "black",na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Histogram of Width", x = "Width", y = "Frequency")

    p1 + p3
}

# Vẽ boxplot cho các cột Height, Width
draw_boxplots <- function(data) {
    # Boxplot height
    p1<-ggplot(data, aes(x = Height)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Boxplot of Height", x = "Height")

    # Boxplot width
    p2<-ggplot(data, aes(x = Width)) +
    geom_boxplot(fill = "orange",na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Boxplot of Width", x = "Width")

    p1 + p2
}

# Vẽ biểu đồ cột cho cột Local
draw_barplot_local <- function(data) {
    # chuyển thành factor nếu chưa
    data$Local <- as.factor(data$Local)
    p<-ggplot(data, aes(x = Local)) +
    geom_bar(fill = "lightgreen", color = "black", na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Distribution of Local Variable",
        x = "Local (0 = external, 1 = internal)",
        y = "Count")
    print(p)
}

# Vẽ biểu đồ phân tán giữa các cột Height, Width, Local và Class
draw_scatter <- function(data) {
    # Chuyển Class sang numeric để tính toán xác suất (phải là 0 và 1)
    data$Class <- as.numeric(as.character(data$Class)) 

    # Biểu đồ Height và P(ad)
    p1 <- ggplot(data, aes(x = Height, y = Class)) +
        # Điểm dữ liệu: Tùy chỉnh độ trong suốt (alpha) và kích cỡ
        geom_jitter(aes(color = Local), height = 0.05, width = 0, alpha = 0.3, na.rm = TRUE) +
        # Đường và Vùng tin cậy:
        stat_smooth(
            method = "glm", 
            method.args = list(family = "binomial"), 
            se = TRUE, 
            color = "darkblue",    # Đổi màu đường thành xanh đậm
            fill = "lightblue",    # Đổi vùng màu xám thành xanh nhạt
            alpha = 0.3,           # Độ trong suốt của vùng tin cậy
            linewidth = 1.2, 
            level = 0.95
        ) + 
        # Định nghĩa màu cụ thể cho từng nhóm Local
        scale_color_manual(values = c("0" = "red", "1" = "green")) +  
        scale_y_continuous(breaks = c(0, 1), labels = c("nonad", "ad")) +
        theme_minimal() +
        labs(title = "Height vs Probability of Ad", x = "Height", y = "P(ad)")

    # Biểu đồ Width và P(ad)
    p2 <- ggplot(data, aes(x = Width, y = Class)) +
        geom_jitter(aes(color = Local), height = 0.05, width = 0, alpha = 0.3, na.rm = TRUE) +
        stat_smooth(
            method = "glm", 
            method.args = list(family = "binomial"), 
            se = TRUE, 
            color = "orange",
            fill = "#ffce72",
            alpha = 0.3,
            linewidth = 1.2, 
            level = 0.95
        ) + 
        scale_color_manual(values = c("0" = "red", "1" = "green")) +  
        scale_y_continuous(breaks = c(0, 1), labels = c("nonad", "ad")) +
        theme_minimal() +
        labs(title = "Width vs Probability of Ad", x = "Width", y = "P(ad)")

    # Phân bố Height, Width, color theo Local và shape theo Class
    p3 <- ggplot(data, aes(x = Height, y = Width, color = Local, shape = as.factor(Class))) +
        geom_point(alpha = 0.6, na.rm = TRUE) +
        theme_minimal() +
        labs(title = "Height vs Width Distribution", x = "Height", y = "Width", shape = "Class")
    
    print(p1)
    print(p2)
    print(p3)
}

# Ma trận tương quan giữa các biến Height, Width, Local và Class
draw_correlation_matrix <- function(data) {
    data$Local <- as.numeric(as.character(data$Local))
    data$Class <- as.numeric(as.character(data$Class))

    # Chọn các cột số và chuyển thành số nếu cần
    numeric_data <- data[, sapply(data, is.numeric)]
    # Tính ma trận tương quan
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    # Vẽ ma trận tương quan
    corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45)
}

#----------------------------------------------------------
# THỐNG KÊ SUY DIỄN
#----------------------------------------------------------
# 5.1 Kiểm định trung bình 2 mẫu
#----------------------------------------------------------
analysis_data <- na_replace(analysis_data)
analysis_data <- get_unique_dataset(analysis_data)

# tính p-value cho từng nhóm
shapiro_df <- analysis_data %>%
  group_by(Class) %>%
  summarise(p_value = shapiro.test(sample(Width, min(500, n())))$p.value)

# QQ plot + annotate
ggplot(analysis_data, aes(sample = Width)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Class) +
  theme_minimal() +
  labs(title = "QQ Plot of Width by Class") +
  geom_text(data = shapiro_df,
            aes(x = Inf, y = -Inf,
                label = paste0("Shapiro p-value = ", (p_value))),
            hjust = 1.1, vjust = -0.5,
            inherit.aes = FALSE)

#---------------------------------------------------------
t.test(Width ~ Class, data = analysis_data)

#---------------------------------------------------------
wilcox_result <- wilcox.test(Width ~ Class, data = analysis_data)
wilcox_result

#---------------------------------------------------------
# 5.2 Kiểm định tỉ lệ 2 mẫu
#---------------------------------------------------------

#---------------------------------------------------------
# số thành công (local = 1)
success <- c(tab["1","1"], tab["0","1"])

# tổng mẫu
n <- c(sum(tab["1",]), sum(tab["0",]))

# tỷ lệ
p_hat <- success / n

# kiểm tra điều kiện
check_df <- data.frame(
  Group = c("ad", "nonad"),
  n = n,
  p_hat = p_hat,
  n_p = n * p_hat,
  n_1_p = n * (1 - p_hat)
)

check_df


# bảng tần số
tab <- table(analysis_data$Class, analysis_data$Local)

# prop test

prop.test(success, n, alternative = "greater")


#---------------------------------------------------------
# Hồi quy logistic kết hợp lasso
#---------------------------------------------------------
# tạo train/test (70-30)
set.seed(99)
train_index <- createDataPartition(analysis_data$Class, p = 0.7, list = FALSE)

train_data <- analysis_data[train_index, ]
test_data  <- analysis_data[-train_index, ]

# in ra số lượng quan sát của các tập
cat("Số lượng quan sát của tập Train:", nrow(train_data), "quan sát\n")
cat("Số lượng quan sát của tập Test:", nrow(test_data), "quan sát\n")

# đếm số lượng
table(train_data$Class)

# tạo weight
weights <- ifelse(train_data$Class == "1",
                  1 / sum(train_data$Class == "1"),
                  1 / sum(train_data$Class == "0"))

# bỏ biến mục tiêu
x_train <- model.matrix(Class ~ .-1, data = train_data)
y_train <- train_data$Class

x_test <- model.matrix(Class ~ .-1, data = test_data)
y_test <- test_data$Class

set.seed(99)

cv_model <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1,  # Lasso
  weights = weights
)

cv_model$lambda.min
cv_model$lambda.1se

best_lambda <- cv_model$lambda.min
best_lambda

coef_lasso <- coef(cv_model, s = "lambda.min")

# chuyển sang dạng dễ nhìn
coef_df <- as.matrix(coef_lasso)

# lọc biến có hệ số khác 0
selected_features <- coef_df[coef_df != 0, , drop = FALSE]

selected_features

length(selected_features) - 1  # trừ intercept

prob_pred <- predict(cv_model, s = "lambda.min", newx = x_test, type = "response")

# chuyển về class
class_pred <- ifelse(prob_pred > 0.5, "1", "0")
class_pred <- as.factor(class_pred)

confusionMatrix(class_pred, y_test)

library(pROC)

# chuyển y_test về numeric (0/1)
y_test_num <- as.numeric(as.character(y_test))

# tạo ROC object
roc_obj <- roc(y_test_num, as.vector(prob_pred))

# AUC
auc_value <- auc(roc_obj)
auc_value

# vẽ ROC
plot(roc_obj, 
     main = "ROC Curve - Lasso Logistic Regression",
     col = "blue", 
     lwd = 2)

# thêm đường random (baseline)
abline(a = 0, b = 1, lty = 2, col = "gray")

#---------------------------------------------------------
# 5.4 Random forest
#---------------------------------------------------------

library(randomForest)

set.seed(99)

rf_model <- randomForest(
  Class ~ ., 
  data = train_data,
  ntree = 500,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

# dự đoán xác suất
rf_prob <- predict(rf_model, test_data, type = "prob")[,2]

# chuyển sang class
rf_pred <- ifelse(rf_prob > 0.5, "1", "0")
rf_pred <- as.factor(rf_pred)

library(caret)
confusionMatrix(rf_pred, test_data$Class)

library(pROC)

roc_rf <- roc(as.numeric(as.character(test_data$Class)), rf_prob)

auc_rf <- auc(roc_rf)
auc_rf

plot(roc_rf, col = "red", lwd = 2)

varImpPlot(rf_model)

#---------------------------------------------------------
# Hướng mở rộng
#---------------------------------------------------------
# Hàm loại outlier theo IQR
remove_outlier <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR_val <- IQR(df[[col]], na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  df[df[[col]] >= lower & df[[col]] <= upper, ]
}

# copy dữ liệu
ad_data_clean <- analysis_data

# loại outlier
ad_data_clean <- remove_outlier(ad_data_clean, "Height")
ad_data_clean <- remove_outlier(ad_data_clean, "Width")

# kiểm tra số dòng còn lại
nrow(ad_data_clean)


library(caret)

set.seed(99)
train_index2 <- createDataPartition(ad_data_clean$Class, p = 0.7, list = FALSE)

train_data2 <- ad_data_clean[train_index2, ]
test_data2  <- ad_data_clean[-train_index2, ]


library(glmnet)

# tạo ma trận
x_train2 <- model.matrix(Class ~ . -1, data = train_data2)
y_train2 <- train_data2$Class

x_test2 <- model.matrix(Class ~ . -1, data = test_data2)
y_test2 <- test_data2$Class

# weight xử lý imbalance
weights2 <- ifelse(y_train2 == "1",
                   1 / sum(y_train2 == "1"),
                   1 / sum(y_train2 == "0"))

# train model
set.seed(99)
cv_model2 <- cv.glmnet(
  x_train2,
  y_train2,
  family = "binomial",
  alpha = 1,
  weights = weights2
)

# dự đoán
prob_pred2 <- predict(cv_model2, s = "lambda.min", newx = x_test2, type = "response")

class_pred2 <- ifelse(prob_pred2 > 0.5, "1", "0")
class_pred2 <- as.factor(class_pred2)

# confusion matrix
library(caret)
confusionMatrix(class_pred2, y_test2)

library(pROC)

y_test_num2 <- as.numeric(as.character(y_test2))

roc_lasso2 <- roc(y_test_num2, as.vector(prob_pred2))

auc_lasso2 <- auc(roc_lasso2)

auc_lasso2

plot(roc_lasso2, col = "blue", main = "ROC - Lasso (No Outliers)")
abline(a = 0, b = 1, lty = 2)


#---------------------------------------------------------
# Các hàm model
#---------------------------------------------------------


#---------------------------------------------------------
# Thực thi (Main)
#---------------------------------------------------------
# In ra các thông tin của dataset
# print("--- Thông tin dataset gốc ---")
#get_data(original_data)
#get_data(analysis_data)

#Xử lý số liệu
#cat("\n")
#print("--- Xử lý số liệu ---")
check_na(analysis_data)
# draw_histograms_and_boxplots(analysis_data)

get_data(analysis_data)

# Kiểm tra và xử lý ngoại lai
cat("\n")
#print("--- Kiểm tra và xử lý ngoại lai ---")
#get_all_outliers(analysis_data)

# cat("\n")
print("--- Thống kê mô tả tổng quát ---")
get_summary_stats(analysis_data)

# cat("\n")
# print("--- Trực quan hóa dữ liệu ---")
draw_histograms_and_boxplots(original_data)
draw_histograms_and_boxplots(analysis_data)

draw_histograms(analysis_data)
draw_boxplots(analysis_data)
draw_barplot_local(analysis_data)
draw_scatter(analysis_data)
draw_correlation_matrix(analysis_data)
