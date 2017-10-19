# 爬取链家二手房信息

# 加载所需的包
library(xml2)
library(rvest)
library(dplyr)
library(stringr)
library(DT)
library(data.table)

# 对爬取页数进行设定并创建数据框
house_inf_0<-data.frame()
url <- "http://cd.lianjia.com/ershoufang/pg"
# 使用for循环进行批量数据爬取（发现url的规律，写for循环语句）
for (i in 1:10){
    web <- read_html(str_c(url, i), encoding="UTF-8")

  # 用SelectorGadget定位节点信息并爬取房名
  house_name <- web%>%html_nodes(".houseInfo a")%>%html_text()

  # 爬取二手房基本信息并消除空格
  house_basic_inf<-web%>%html_nodes(".houseInfo")%>%html_text()
  house_basic_inf<-str_replace_all(house_basic_inf," ","")

  # SelectorGadget定位节点信息爬取地址
  house_address<-web%>%html_nodes(".positionInfo a")%>%html_text()

  # SelectorGadget定位节点信息爬取总价
  house_totalprice<-web%>%html_nodes(".totalPrice")%>%html_text()

  # SelectorGadget定位节点信息爬取单价
  house_unitprice<-web%>%html_nodes(".unitPrice span")%>%html_text()

  # 创建数据框存储以上信息
  house<-data_frame(house_name,house_basic_inf,house_address,house_totalprice,house_unitprice)
  house_inf_0<-rbind(house_inf_0, house)
}
# 删除中间数据
rm(url, house, house_address, house_basic_inf, house_name, house_totalprice, house_unitprice,i, web)

# 整理数据，分割house_inf$house_basic_inf的有用信息
house_inf_1 <- str_split_fixed(house_inf_0$house_basic_inf, "\\|", 8) %>% data.table()

# 名字含有“别墅”的字符串多一个“|”分隔符，调整表格
house_inf_2 <- house_inf_1[V2 %like% "别墅", ':='(V8=V2, V2=V3,V3=V4,V4=V5,V5=V6,V6=V7,V7=V8)][,V7:=V8][,V8:=NULL]

# 合并表格
house_inf_3 <- cbind(house_inf_0,house_inf_2)

# 提取可分析数据(numeric)
house_inf<- house_inf_3[,':='('总价（万元）'=as.numeric(str_replace(house_totalprice, "万", "")), 
                          '面积（平米）'=as.numeric(str_replace(V3, "平米", "")),
                          '单价（元/平方）'=as.numeric(str_replace(str_replace(house_unitprice, "元/平米", ""), "单价", "")),
                          '室'=as.numeric(substr(V2, 1, 1)),
                          '厅'=as.numeric(substr(V2, 3, 3))
                          )][,c(1,3,13,14,15,16,17,9,10,11,12,2)] %>% 
               rename('小区名称'=house_name, '位置'=house_address, '朝向'=V4,
                      '装修'=V5, '电梯'=V6, '别墅'=V7, '备注'=house_basic_inf)

# 删除中间数据
rm(house_inf_0, house_inf_1, house_inf_2, house_inf_3)

# 生成表格
DT::datatable(house_inf)

