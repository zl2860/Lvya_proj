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
