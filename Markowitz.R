# 1. 导入需要的库

#检查是否全部都安装了否则自动安装
if(!require(tseries)){install.packages("tseries")}
if(!require(outliers)){install.packages("outliers")}
if(!require(zoo)){install.packages("zoo")}
if(!require(dygraphs)){install.packages("dygraphs")}
if(!require(fPortfolio)){install.packages("fPortfolio")}
if(!require(Tushare)){install.packages("Tushare")}
if(!require(RiskPortfolios)){install.packages("RiskPortfolios")}
if(!require(ggplot2)){install.packages("ggplot2")}
#如果报错could not find function "xts" 或者 "mutate"
if(!require(dplyr)){install.packages("dplyr")}
if(!require(xts)){install.packages("xts")}

#导入要使用的库
library(tseries) 
library(outliers)
library(zoo) 
library(dygraphs)
library(fPortfolio)
library(Tushare)
library(RiskPortfolios)  
library(ggplot2)
#如果报错could not find function "xts" 或者 "mutate"
library(dplyr)
library(xts)

# 2. 确定需要分析的股票列表
## 取决于你自己的需要

# 3. 使用Tushare获取这些股票的日线数据

#初始化接口
api <- Tushare::pro_api(token = "Your token")
#获取股票列表
stocknames <- data.frame(api(api_name = "stock_basic"))


# 我们对近3年的数据进行分析：

#定义日线数据开始，结束日期
startdate = "20181231"
enddate = "20211231"

#中国平安
ZGPA <- api(api_name = "daily", ts_code = "601318.SH", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
ZGPA<-xts(ZGPA$close,as.Date(ZGPA$trade_date)) %>% as.zoo()

#洋河股份
YHGF<- api(api_name = "daily", ts_code = "002304.SZ", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
YHGF<-xts(YHGF$close,as.Date(YHGF$trade_date)) %>% as.zoo()

#格力电器
GLDQ <- api(api_name = "daily", ts_code = "000651.SZ", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
GLDQ<-xts(GLDQ$close,as.Date(GLDQ$trade_date)) %>% as.zoo()

#伊利股份
YLGF<- api(api_name = "daily", ts_code = "600887.SH", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
YLGF<-xts(YLGF$close,as.Date(YLGF$trade_date)) %>% as.zoo()

#恒瑞医药
HRYY<- api(api_name = "daily", ts_code = "600276.SH", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
HRYY<-xts(HRYY$close,as.Date(HRYY$trade_date)) %>% as.zoo()

#海康威视
HKWY <-api(api_name = "daily", ts_code = "002415.SZ", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
HKWY<-xts(HKWY$close,as.Date(HKWY$trade_date)) %>% as.zoo()

#信立泰
XLT <-api(api_name = "daily", ts_code = "002294.SZ", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
XLT<-xts(XLT$close,as.Date(XLT$trade_date)) %>% as.zoo()

#先导智能
XDZN <-api(api_name = "daily", ts_code = "300450.SZ", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
XDZN<-xts(XDZN$close,as.Date(XDZN$trade_date)) %>% as.zoo()

#集友股份
JYGF<-api(api_name = "daily", ts_code = "603429.SH", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
JYGF<-xts(JYGF$close,as.Date(JYGF$trade_date)) %>% as.zoo()

#双汇发展
SHFZ <-api(api_name = "daily", ts_code = "000895.SZ", start_date = startdate, end_date = enddate) %>%
  mutate(trade_date = as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", trade_date))) %>%
  mutate_at(vars(3:dim(.)[2]), as.numeric)
##取收盘价转化为Time series
SHFZ<-xts(SHFZ$close,as.Date(SHFZ$trade_date)) %>% as.zoo()

# 无图无真相，我们将我们投资组合中的10只股票走势绘制出来：

# 将数据合成到一个Dataframe 中
MyPortfolio <- merge(ZGPA,YHGF,GLDQ,YLGF,HRYY,HKWY,XLT,XDZN,JYGF,SHFZ)
#可视化
names(MyPortfolio)<-c("中国平安","洋河股份","格力电器","伊利股份","恒瑞医药","海康威视","信立泰","先导智能","集友股份","双汇发展")
dygraph(MyPortfolio)


# 4. 计算日收益率序列和数据清洗工作

##我们使用指数收益率计算收益率,我已经将清洗和差分的运算写为函数:

#用于数据清洗的函数
DataClean <- function(MyPortfolio){
  #去除NA值
  table(is.na(MyPortfolio))
  MyPortfolio<-MyPortfolio[complete.cases(MyPortfolio), ] 
  table(is.na(MyPortfolio))
  
  #获取股票名字
  stocknames<-names(MyPortfolio)
  #生成收益率序列名字r_001-r_010
  number<- as.character(sprintf("%03d",1:ncol(MyPortfolio)))
  new_stockname <- paste("r_",number,sep="")
  
  #差分取对数收益率并且去除OUTLIERS
  for (m in 1:ncol(MyPortfolio)) {
    assign(new_stockname[m],
           diff(log(MyPortfolio[,m]))[abs(outliers::scores(diff(log(MyPortfolio[,m]))))<=5])
  }
  #将数据合并
  RSeries <- cbind(r_001,r_002)
  for (i in 3:ncol(MyPortfolio)){
    temp <-get(new_stockname[i])
    RSeries <- cbind(RSeries,temp)
  }
  #再次去除NA值
  table(is.na(RSeries))
  RSeries<-RSeries[complete.cases(RSeries), ] 
  table(is.na(RSeries))
  #重命名
  names(RSeries) <- stocknames
  return(RSeries)
}

## 现在我们调用DataClean函数:

#清洗差分数据
RPortfolio <- DataClean(MyPortfolio = MyPortfolio)
#可视化
dygraph(RPortfolio)

# 5. 使用蒙特卡洛方法进行有效边界的绘制

## 在构建有效边界前，我们先看看收益率序列的统计学数据：

#投资组合的统计学数据
sta_por <- data.frame()
for (i in 1:ncol(RPortfolio)) {
  sta_por <- rbind.data.frame(sta_por,data.frame(names(RPortfolio)[i],
                                                 mean(RPortfolio[,i]),
                                                 sd(RPortfolio[,i])))
}
names(sta_por)<- c("股票名称","收益率期望","标准差")
View(sta_por)


# 为了绘制马科维兹模型中的有效边界，我们需要生成许多组的仓位（投资比例）组合:

#制作收益率和协方差矩阵
returnPort<-colMeans(RPortfolio)
riskPort<-cov(RPortfolio)
#初始化仓位（投资比例）矩阵
w <- matrix(1,ncol=ncol(RPortfolio),nrow=1)
mean_disv <- as.matrix(data.frame(colMeans(RPortfolio),colSds(RPortfolio)))
#模拟20000种可能的比例组合
n <- 20000
for (i in 1:n){
  w<-c(runif(ncol(RPortfolio),0,1))
  #将比例化为百分比
  w<-w/sum(w)
  #计算结果
  mean_disv<-rbind(mean_disv,c(t(w)%*%returnPort,sqrt(t(w)%*%riskPort%*%w)))
}

# 将所有的这些可能的收益和风险绘制出来：

#可视化
plot(mean_disv[,2],mean_disv[,1],type = "p", 
     col="Royalblue",cex = 0.2,xlab = "风险（标准差σ)",
     ylab = "收益率 μ")

# 为了绘制此有效边界，我们使用Portfolio库[3]，取某理财产品的7日年化利率2.0950%为无风险利率:

#初始化
specPort<-portfolioSpec()
#设定无风险资产的利率为2.095%
setRiskFreeRate(specPort)<- 0.02095
#有效边界点数量
setNFrontierPoints(specPort) <- 500
Frontera <- portfolioFrontier(as.timeSeries(RPortfolio),spec=specPort )
#绘制有效边界和有无风险资产投资下的夏普比率最大的投资组合比例
plot(Frontera,risk="cov",c(1,2,3,7,8))




#6. 结论和最佳投资组合

##接着，为了得出合适的仓位安排，我们需要将上述两个最佳组合点所代表的仓位（投资比例组合）求出来：

##基于有效边界理论的最小风险组合：
#最小风险组合(红色圆点)
PortfolioMinRisk<-minriskPortfolio(as.timeSeries(RPortfolio),spec=specPort, constraints = "LongOnly")
#理想仓位比例
PortfolioMinRisk@portfolio@portfolio$weights
#可视化(饼图)
df <- data.frame(weights = as.numeric(PortfolioMinRisk@portfolio@portfolio$weights)*100, 
                 StockName = names(PortfolioMinRisk@portfolio@portfolio$weights))
df$StockName<-as.factor(df$StockName)
df$StockNameP <-as.factor(paste(df$StockName,round(df$weights,2),"%",sep=" "))
ggplot(df, aes(x = "", y = weights, fill = StockNameP)) + 
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") 
df <- df[,-3]
names(df) <-c("理想仓位（%）", "股票名称")
View(df)
PortfolioMinRisk

