---
layout: post
title: "双因素方差分析"
date:   2024-11-5
tags: [web]
comments: true
author: 学不懂概率论
toc: true
toc_depth: 3  # 设置显示三级目录
---

本篇文章主要介绍有交互效应的双因素方差分析以及无交互效应的方差分析，并使用R语言实现。


<!-- more -->

## 目录

- [无交互效应的双因素方差分析](#无交互效应的双因素方差分析)
- [有交互效应的双因素方差分析](#无交互效应的双因素方差分析)
  

# 无交互效应的双因素方差分析

## 1.数据读取与处理
```r
dd = read.csv("D:/文档/R-practice/抽样调查/data.csv", header = TRUE, row.names = NULL)
library(tidyr)
colnames(dd)[1] <- 'B'
data1 <- gather(dd, A, Y, -B)
data1$A <- factor(data1$A, levels = c("A1", "A2", "A3", "A4", "A5"), labels = c(1:5))
data1$B <- factor(data1$B, levels = c("B1", "B2", "B3", "B4"), labels = c(1:4))
```
## 2.正态性和方差齐性检验
```r
shapiro.test(data1$Y)
bartlett.test(data1$Y ~ data1$A, data = data1)
bartlett.test(data1$Y ~ data1$B, data = data1)
```
## 3.双因素方差分析
```r
aov_data1 <- aov(Y ~ A + B, data = data1)
summary(aov_data1)
```
## 4.残差分析图
```r
residuals <- residuals(aov_data1)
fitted_values <- fitted(aov_data1)
```
### QQ图
```r
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210224909.png)

### 残差与拟合值的散点图
```r
plot(fitted_values, residuals, pch = 19, col = 'blue')
abline(h = 0, col = "red", lwd = 2)
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210224930.png)
###  绘制因子A的残差箱线图
```r
library(ggplot2)
p1 <- ggplot(data1, aes(x = A, y = residuals, fill = A)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 16) + 
  labs(title = "不同位置因子A的残差箱线图", x = "位置水平", y = "残差") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+  # 颜色调色板
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中
```
###  绘制因子B的残差箱线图
```r
library(ggplot2)
p2 <- ggplot(data1, aes(x = B, y = residuals, fill = B)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 16) + 
  labs(title = "不同品牌因子B的残差箱线图", x = "品牌水平", y = "残差") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+  # 颜色调色板
  theme(plot.title = element_text(hjust = 0.5))# 标题居中
```
### 将两个图合并在一张图中
```r
library(patchwork)  # 用于拼接多个图
p1 / p2  # 竖直排列p1和p2
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210224944.png)
# 有交互效应的双因素方差分析

## 1. 读取和整理数据
```r
data1<-data.frame(
  x=c(14,10,11,11,13,9,10,12,9,7,10,8,7,11,6,10,5,11,13,14,12,13,14,10),
  den=gl(3,8),
  tem=gl(4,2,24)
 )
head(data1)
table(data1$den,data1$tem)
```
## 2.正态性和方差齐性检验
```r
shapiro.test(data1$x)
bartlett.test(x~ den, data = data1)
bartlett.test(x ~ tem, data = data1)
```
## 3. 双因素方差分析，加入交互效应
```r
fit <- aov(x~den*tem,data = data1)
summary(fit)
```
## 4. 残差分析
```r
residuals <- residuals(fit)
fitted_values <- fitted(fit)
```
### QQ图
```r
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210225601.png)
### 残差与拟合值的散点图
```r
plot(fitted_values, residuals, pch = 19, col = 'blue',
     xlab = "拟合值", ylab = "残差", main = "拟合值与残差的散点图")
abline(h = 0, col = "red", lwd = 2)
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210225614.png)

### 绘制因子A的残差箱线图
```r
p1 <- ggplot(data1, aes(x = den, y = residuals, fill = den)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 16) + 
  labs(title = "不同位置因子A的残差箱线图", x = "因子A（位置水平）", y = "残差") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") + 
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中
```
### 绘制因子B的残差箱线图
```r
p2 <- ggplot(data1, aes(x = tem, y = residuals, fill = tem)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 16) + 
  labs(title = "不同品牌因子B的残差箱线图", x = "因子B（品牌水平）", y = "残差") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") + 
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中
```
### 将两个图合并
```r
library(patchwork)  # 用于拼接多个图
p1 / p2  # 竖直排列p1和p2
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210225637.png)

## 5.交互效应可视化
```r
interaction.plot(data1$den, data1$tem, data1$x, 
                 type = "b", col = rainbow(4), pch = 16, 
                 xlab = "浓度", ylab = "质量指标", 
                 main = "浓度和温度的交互效应图")
```
![](https://cdn.jsdelivr.net/gh/azure2870/zyf.github.io/zyf.github.io/images/20241210225647.png)
