# README.md

## Giới thiệu dự án

Đây là đồ án môn **Xác suất Thống kê (HK252 - 2025–2026)** với mục tiêu xây dựng mô hình **phân loại hình ảnh quảng cáo trên website** dựa trên bộ dữ liệu **Internet Advertisements Data Set** từ UCI Machine Learning Repository.

Dự án tập trung áp dụng các kiến thức:

* Tiền xử lý dữ liệu
* Phân tích khám phá dữ liệu (EDA)
* Kiểm định giả thuyết thống kê
* Hồi quy Logistic
* Hồi quy tuyến tính
* ANOVA
* Đánh giá mô hình phân loại

---

## Bộ dữ liệu sử dụng

* **Tên dataset:** Internet Advertisements Data Set
* **Nguồn:** UCI Machine Learning Repository
* **Số quan sát ban đầu:** 3,279 dòng
* **Số thuộc tính ban đầu:** 1,559 biến

### Biến mục tiêu

* `Class = 1`: Quảng cáo (`ad.`)
* `Class = 0`: Nội dung thường (`nonad.`)

---

## Mục tiêu nghiên cứu

Nhóm lựa chọn các biến có tính đại diện cao:

* `Height`: Chiều cao ảnh
* `Width`: Chiều rộng ảnh
* `Local`: Ảnh nội bộ / ngoại vi
* `Class`: Nhãn phân loại

Mục tiêu là dự đoán xem một hình ảnh có phải là quảng cáo hay không.

---

## Quy trình thực hiện

## 1. Tiền xử lý dữ liệu

### Đọc dữ liệu

* Loại bỏ cột ID
* Chuyển dấu `?` thành `NA`

### Chuẩn hóa dữ liệu

* `Height`, `Width` → Numeric
* `Local`, `Class` → Factor

### Xử lý giá trị thiếu

* `Height`, `Width`: thay bằng Median
* `Local`: thay bằng Mode

### Xử lý trùng lặp

* Phát hiện **2,225 dòng trùng**
* Loại bỏ các quan sát trùng lặp

### Kiểm tra ngoại lai

* Height: ~3.61%
* Width: ~5.79%

Các ngoại lai được giữ lại vì phản ánh đúng dữ liệu ảnh web thực tế.

---

## 2. Phân tích dữ liệu

* Histogram
* Boxplot
* Barplot
* Ma trận tương quan
* So sánh phân phối giữa quảng cáo và không quảng cáo

---

## 3. Mô hình sử dụng

### Hồi quy Logistic

Dự đoán xác suất thuộc lớp quảng cáo cho bài toán phân loại nhị phân.

### Hồi quy tuyến tính

Khảo sát mối quan hệ tuyến tính giữa các biến kích thước.

### ANOVA

Kiểm định sự khác biệt trung bình giữa các nhóm.

---

## Công nghệ sử dụng

* R
* ggplot2
* dplyr
* patchwork
* caret
* stats

---

## Cấu trúc thư mục

```text
project/
│── add.csv
│── ads_detection.r
└── README.md
```

---

## Cách chạy chương trình

```r
source("ads_detection.r")
```

Hoặc mở file `.R` bằng RStudio và chạy tuần tự.

---

## Kết quả mong đợi

* Làm sạch dữ liệu
* Trực quan hóa dữ liệu
* Xây dựng mô hình phân loại quảng cáo
* Đánh giá Accuracy / Precision / Recall

---

## Ghi chú

Dự án phục vụ mục đích học thuật. Dataset thuộc quyền sở hữu UC
