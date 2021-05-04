data\_cleaning\_202105
================
Zongchao
5/4/2021

# Import data

注意：石墨下载下来的 `编号.xlsx` 不知道为什么在r里读取时会是另一个文件。需要先把 `编号.csv`再进行读取：

``` r
data = read_xlsx('./health_development/h_20210503.xlsx') %>%
         mutate(h68 = as.numeric(h68))
# 另存为 *.csv!
code = read_csv('./health_development/编号 (1).csv', col_names = F) %>% select(-X3)
```

# 需要解决的问题

  - 匹配班级编号和年级编号

  - 清理体重

  - 清理身高

## 匹配班级编号和年级编号

``` r
# 创建`school_class`
data = data %>% mutate(school_class = str_sub(ID, 3, 4)) %>% 
  select(school_class,everything())

# 创建`grade`
## 先整理班级花名册
code = code %>% 
  filter(nchar(X1) != 1)

## 匹配学号与班级，该函数返回一个匹配好班级的dataframe
grade_gen = function(df = code){
  class_list = df[which(is.na(code$X2)),1]$X1
  class_idx = c(which(is.na(df$X2)),1991)
  grade_vec = vector()
  for (i in 2:length(class_idx)){
    grade_vec = c(grade_vec, rep(class_list[i-1],class_idx[i] - class_idx[i-1]))
  }
  res = cbind(unlist(grade_vec), df) %>% .[,c(1,3)]
  colnames(res) = c("grade", "ID")
  return(res)
}

## 获取匹配好的数据（班级&学好）
grade_matched = grade_gen()

## 检查匹配结果，最下面那行代码返回True则匹配无误
check_grade = grade_matched %>% filter(is.na(grade))
sum(check_grade[,1] != check_grade[,2]) == 0
```

    ## [1] TRUE

``` r
## 在原始数据集创建 `grade`
data_class_grade = left_join(data, grade_matched) %>%
  select(grade, school_class, ID, everything()) %>%
  mutate(grade = str_sub(grade,1,1))
```

    ## Joining, by = "ID"

共有1859人中共有48人未能匹配上grade，他们的ID在`编号.xlsx`中不能被查询到：

``` r
data_class_grade %>% filter(is.na(grade)) %>%
  select(grade, school_class, ID) %>%
  knitr::kable()
```

| grade | school\_class |       ID |
| :---- | :------------ | -------: |
| NA    | 12            | 11125928 |
| NA    | 13            | 11130098 |
| NA    | 13            | 11130099 |
| NA    | 13            | 11131455 |
| NA    | 15            | 11155866 |
| NA    | 15            | 11159432 |
| NA    | 17            | 11170099 |
| NA    | 19            | 11190099 |
| NA    | 19            | 11190198 |
| NA    | 19            | 11190297 |
| NA    | 22            | 12220099 |
| NA    | 25            | 12256138 |
| NA    | 27            | 12270088 |
| NA    | 27            | 12270089 |
| NA    | 27            | 12270090 |
| NA    | 27            | 12270091 |
| NA    | 27            | 12270092 |
| NA    | 27            | 12270093 |
| NA    | 27            | 12270094 |
| NA    | 27            | 12270095 |
| NA    | 27            | 12270096 |
| NA    | 27            | 12270097 |
| NA    | 27            | 12270098 |
| NA    | 27            | 12270099 |
| NA    | 28            | 12280001 |
| NA    | 28            | 12280002 |
| NA    | 28            | 12280003 |
| NA    | 28            | 12280004 |
| NA    | 28            | 12280005 |
| NA    | 28            | 12280006 |
| NA    | 28            | 12280007 |
| NA    | 28            | 12280008 |
| NA    | 28            | 12280009 |
| NA    | 28            | 12280010 |
| NA    | 28            | 12280011 |
| NA    | 28            | 12280012 |
| NA    | 28            | 12280013 |
| NA    | 29            | 12290009 |
| NA    | 31            | 12315115 |
| NA    | 50            | 15500899 |
| NA    | 51            | 15512962 |
| NA    | 54            | 15540099 |
| NA    | 56            | 15561199 |
| NA    | 56            | 15561298 |
| NA    | 56            | 15561397 |
| NA    | 82            | 18827211 |
| NA    | 84            | 18848528 |
| NA    | 85            | 18859114 |

## 清理体重 & 身高

``` r
# 算众数
## a simpe function for getting mode
getmode = function(v) {
   uniqv = unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

## 算众数
mode = data_class_grade %>% 
  na.omit() %>%
  group_by(grade) %>%
  summarise(weight_mode = getmode(h66),
            height_mode = getmode(h68)) %>%
  rename("h66" = "weight_mode",
         "h68" = "height_mode")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
## imputation 填体重身高

weight_imp = function(grade, df_mode = mode){
  return(mode[mode[,1]== grade,]$h66)
}

height_imp = function(grade, df_mode = mode){
    return(mode[mode[,1]== grade,]$h68)
  }

wh_imp = function(df = data_class_grade, df_mode = mode){
  
  df = df %>%
    mutate(h66 = ifelse(is.na(h66)|h66 <= 45, NA, h66),
           h68 = ifelse(is.na(h68)|h68 <= 120, NA, h68))
  
  df = df %>%
    mutate(h66 = ifelse(is.na(h66)&!is.na(grade), unlist(map(.x = grade,  weight_imp)), h66),
           h68 = ifelse(is.na(h68)&!is.na(grade), unlist(map(.x = grade, height_imp)), h68))
  return(df)
}

### 获取填补后的数据集
data_imp = wh_imp()
```

## Simple check

没有填补的人是不在`编号.xlsx`中被记录的人

``` r
skimr::skim(data_imp$h66)
```

    ## 
    ## Skim summary statistics
    ## 
    ## ── Variable type:numeric ───────────────────────────────────────────────────────────────────────────────
    ##      variable missing complete    n  mean    sd    p0 p25 p50 p75 p100
    ##  data_imp$h66      20     1839 1859 74.02 18.77 45.65  60  70  83  299
    ##      hist
    ##  ▇▅▁▁▁▁▁▁

``` r
skimr::skim(data_imp$h68)
```

    ## 
    ## Skim summary statistics
    ## 
    ## ── Variable type:numeric ───────────────────────────────────────────────────────────────────────────────
    ##      variable missing complete    n   mean    sd  p0 p25 p50 p75 p100
    ##  data_imp$h68      30     1829 1859 148.85 10.76 122 140 148 155  250
    ##      hist
    ##  ▂▇▅▁▁▁▁▁

``` r
unique(data_imp$h66)
```

    ##   [1]  60.00  50.00  56.00  61.00  81.00  62.00  51.00  80.00  67.00  65.00
    ##  [11]  63.00  46.00  71.00  64.00  87.00  79.00  74.00  52.00  68.00  59.00
    ##  [21]  72.00  48.00  70.00  75.00  82.00  69.00  54.00 146.00  84.00  66.00
    ##  [31]  85.00  57.00  90.00 100.00  95.00  65.10 101.00     NA  58.00  73.00
    ##  [41]  55.00  77.00  78.00  92.00  49.00  53.00 110.00  72.42  94.00  83.00
    ##  [51] 104.00 130.00  76.00  89.00 102.00 105.00 123.00  45.65  65.45  98.00
    ##  [61]  86.00  93.00 150.00 106.00 145.00 299.00  96.00 149.00  88.00  99.00
    ##  [71]  93.28  91.00 170.00 115.00  97.00  59.10 113.00 140.00 120.00  75.37
    ##  [81] 103.00  50.26 160.00  61.82 118.00  60.45 108.00  47.00  60.28 116.00
    ##  [91] 107.00  73.20  63.30  60.43  72.45 135.00 114.00 109.00 112.00 108.80
    ## [101] 230.00 125.00 121.00 198.00  68.30  86.44 134.00  62.42  58.90 200.00
    ## [111]  61.83  68.59  74.34  76.24 124.00  64.61

``` r
unique(data_imp$h68)
```

    ##  [1] 140.00 126.00 150.00 143.00 125.00 139.00 128.00 130.00 153.00 135.00
    ## [11] 123.00 145.00 144.00 133.00 155.00 137.00 158.00 210.00 160.00 149.00
    ## [21] 134.00     NA 190.00 142.00 148.00 122.00 250.00 136.00 168.00 199.00
    ## [31] 141.00 132.00 163.00 159.00 170.00 154.00 124.00 138.00 146.00 165.00
    ## [41] 164.00 152.00 156.00 147.00 151.00 162.00 180.00 157.00 161.00 166.00
    ## [51] 175.00 169.00 167.00 172.00 174.00 127.00 211.00 200.00 131.00 129.00
    ## [61] 171.00 130.30 189.00 156.14
