library(ggplot2)
library(ggthemes)
library(GGally)
data<-read.csv("Iseki.csv")
data$iseki[data$iseki==0]<-"遺跡なし"
data$iseki[data$iseki==1]<-"遺跡あり"


############データの整形
data<-data[4:8]


################散布図行列
p<-ggpairs(data,diag=list(continuous="barDiag"))+theme_minimal()+theme(axis.text= element_text(size=5),legend.title = element_text(size=7),legend.text = element_text(size=7),axis.title = element_text(size=7),plot.title= element_text(size=7),strip.text=element_text(size=7))
ggsave("graph01.png", p, family="Japan1GothicBBB",width=6,height=6) #ゴシックフォントを指定してpdfに保存

###傾斜ヒストグラム
p<-ggplot(data)+geom_histogram(aes(x=Slope))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph02.png", p, family="Japan1GothicBBB",width=5,height=4) #ゴシックフォントを指定してpdfに保存

###日射量ヒストグラム
p<-ggplot(data)+geom_histogram(aes(x=Irrad))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph03.png", p, family="Japan1GothicBBB",width=5,height=4) #ゴシックフォントを指定してpdfに保存

###方位棒グラフ
p<-ggplot(na.omit(data))+geom_bar(aes(Aspect))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph04.png", p, family="Japan1GothicBBB",width=5,height=4) #ゴシックフォントを指定してpdfに保存


####遺跡有りとナシを結合
merge<-read.csv("Merge.csv")
##データ整形
merge<-merge[,c(1,4:8)]

################散布図行列
p<-ggpairs(merge,diag=list(continuous="barDiag"))+theme_minimal()+theme(axis.text= element_text(size=5),legend.title = element_text(size=7),legend.text = element_text(size=7),axis.title = element_text(size=7),plot.title= element_text(size=7),strip.text=element_text(size=7))
ggsave("graph10.png", p, family="Japan1GothicBBB",width=6,height=6) #ゴシックフォントを指定してpdfに保存

###傾斜ヒストグラム
p<-ggplot(merge)+geom_histogram(aes(x=Slope))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph05.png", p, family="Japan1GothicBBB",width=4,height=4) #ゴシックフォントを指定してpdfに保存

###日射量ヒストグラム
p<-ggplot(merge)+geom_histogram(aes(x=Irrad))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
xlim(c(4000, NA))+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph06.png", p, family="Japan1GothicBBB",width=4,height=4) #ゴシックフォントを指定してpdfに保存

###標高ヒストグラム
p<-ggplot(merge)+geom_histogram(aes(x=Level))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
#xlim(c(4000, NA))+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph07.png", p, family="Japan1GothicBBB",width=4,height=4) #ゴシックフォントを指定してpdfに保存

###河川からの距離ヒストグラム
p<-ggplot(merge)+geom_histogram(aes(x=WL))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
#xlim(c(4000, NA))+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph08.png", p, family="Japan1GothicBBB",width=4,height=4) #ゴシックフォントを指定してpdfに保存

###斜面方位棒グラフ
p<-ggplot(merge)+geom_bar(aes(x=Aspect))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
coord_flip()
#xlim(c(4000, NA))+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("graph09.png", p, family="Japan1GothicBBB",width=6,height=4) #ゴシックフォントを指定してpdfに保存




################変数の相関関係を調べる
####相関係数行列
r<-cor(data[2:ncol(data)])
write.csv(r,file="相関係数.csv")

################散布図行列にヒストグラム
p<-ggpairs(data[c(1:10)],diag=list(continuous="barDiag"))+theme_minimal()+theme(axis.text= element_text(size=5),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("pairs_hist_ALL.pdf", p, family="Japan1GothicBBB",width=10,height=10) #ゴシックフォントを指定してpdfに保存


+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+scale_colour_ptol()
ggsave("pairs_ALL.pdf", p, family="Japan1GothicBBB",width=7,height=7) #ゴシックフォントを指定してpdfに保存

,upper = list(combo = "facethist"),lower = list(continuous = "smooth", combo = "denstrip"),params=list(corSize=6,size=2)


################箱ひげ図
###遺跡の有無をカテゴリ化
data$iseki[data$iseki==0]<-"遺跡なし"
data$iseki[data$iseki==1]<-"遺跡あり"

###Aspect_N2
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Aspect_N2))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Aspect_N2.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Aspect_E2
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Aspect_E2))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Aspect_E2.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###long_curve
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=longitudin_curve))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_long_curve.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Plan_Curve
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Plan_Curve))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Plan_Curve.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Profile.Curve
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Profile.Curve))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Profile_Curve.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Sky_View
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Sky_View))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Sky_View.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Slope
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Slope))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Slope.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Visible_Sky
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Visible_Sky))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Visible_Sky.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Wl_buffer
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Wl_buffer))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Wl_buffer.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Elevation
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Elevation))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Elevation.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Irradiance_172
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Irradiance_172))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Irradiance_172.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存
#######Aspect_N2とE2の計算
###Aspect_N2
data$Aspect_N2<-ifelse(data$Aspect<180,data$Aspect,360-data$Aspect) 
###Aspect_E2
a<-data$Aspect-90			#90度引く
b<-ifelse(a<0,abs(a)+270,a)			#0以下なら絶対値に270足す。
data$Aspect_E2<-b
###Irradiance_355
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Irradiance_355))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Irradiance_355.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###maximal_curve
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=maximal_curve))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_maximal_curve.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存

###Wetness_index
p<-ggplot(data)+geom_boxplot(aes(x=iseki,y=Wetness_index))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
ggsave("box_Wetness_index.pdf", p, family="Japan1GothicBBB",width=2,height=5) #ゴシックフォントを指定してpdfに保存


################正規生検定
#####準備
a<-subset(data,data$iseki=="遺跡あり")
b<-subset(data,data$iseki=="遺跡なし")
######
kstest2<-data.frame()				##空のデータフレーム作る
for(i in 2:ncol(data))			
{
kstest_a<-ks.test(a[,i],"pnorm",mean=mean(a[,i]),sd=sd(a[,i]))
kstest_b<-ks.test(b[,i],"pnorm",mean=mean(b[,i]),sd=sd(b[,i]))
kstest2[i-1,1]<-kstest_a[[2]]
kstest2[i-1,2]<-kstest_b[[2]]
}
kstest2[,3]<-colnames(data[,c(2:ncol(data))])			##属性名称を3列目に付値
colnames(kstest2)<-c("遺跡あり","遺跡なし","属性")				##列名をつける
write.csv(kstest2,file="正規性検定.csv")

################等分散性検定
var2<-data.frame()				##空のデータフレーム作る
for(i in 2:ncol(data))			
{
var<-var.test(data[,i]~data[,1])
var2[i-1,1]<-var[[1]][[1]]
var2[i-1,2]<-var[[3]]
}
var2[,3]<-colnames(data[,c(2:ncol(data))])			##属性名称を1列目に付値
colnames(var2)<-c("統計量","属性","p値")				##列名をつける
write.csv(var2,file="等分散検定.csv")

###############差の検定　ノンパラメトリック（マン・ホイットニーU検定）
nonpara2<-data.frame()
for(i in 2:ncol(data))	{
nonpara <- wilcox.test(data[,i]~data[,1], correct=FALSE)
nonpara2[i-1,1]<-nonpara[[1]][[1]]
nonpara2[i-1,2]<-nonpara[[3]]
}
nonpara2[,3]<-colnames(data[,c(2:ncol(data))])			##属性名称を2列目に付値
colnames(nonpara2)<-c("統計量","p値","属性")				##列名をつける
write.csv(nonpara2,file="マン・ホイットニーU検定.csv")

###############ヒストグラム
library(ggplot2)
library(ggthemes)
data<-read.csv("All_data.csv")
#######Aspect_N2とE2の計算
###Aspect_N2
data$Aspect_N2<-ifelse(data$Aspect<180,data$Aspect,360-data$Aspect) 
###Aspect_E2
a<-data$Aspect-90			#90度引く
b<-ifelse(a<0,abs(a)+270,a)			#0以下なら絶対値に270足す。
data$Aspect_E2<-b

##################コサインウェーブの実験
a<-cos(c(0:360)*pi/180)
b<-c(0:360)
test<-data.frame(a,b)
p<-ggplot(test)+geom_point(aes(x=b,y= a))

###Aspect
p<-ggplot(data)+geom_histogram(aes(x=Aspect,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Aspect.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Aspect_N2
p<-ggplot(data)+geom_histogram(aes(x=Aspect_N2,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Aspect_N2.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保存
###Aspect_E2
p<-ggplot(data)+geom_histogram(aes(x=Aspect_E2,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Aspect_E2.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###longitudin_curve
p<-ggplot(data)+geom_histogram(aes(x=longitudin_curve,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_longitudin_curve.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Plan_Curve
p<-ggplot(data)+geom_histogram(aes(x=Plan_Curve,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Plan_Curve.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Profile.Curve
p<-ggplot(data)+geom_histogram(aes(x=Profile.Curve,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Profile.Curve.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Sky_View
p<-ggplot(data)+geom_histogram(aes(x=Sky_View,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Sky_View.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Slope
p<-ggplot(data)+geom_histogram(aes(x=Slope,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Slope.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Visible_Sky
p<-ggplot(data)+geom_histogram(aes(x=Visible_Sky,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Visible_Sky.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Wl_buffer
p<-ggplot(data)+geom_histogram(aes(x=Wl_buffer,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Wl_buffer.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Elevation
p<-ggplot(data)+geom_histogram(aes(x=Elevation,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Elevation.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Irradiance_172
p<-ggplot(data)+geom_histogram(aes(x=Irradiance_172,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Irradiance_172.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Irradiance_355
p<-ggplot(data)+geom_histogram(aes(x=Irradiance_355,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Irradiance_355.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###maximal_curve
p<-ggplot(data)+geom_histogram(aes(x=maximal_curve,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_maximal_curve.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保
###Wetness_index
p<-ggplot(data)+geom_histogram(aes(x=Wetness_index,y= ..density..))+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("hist_Wetness_index.pdf", p, family="Japan1GothicBBB",width=4,height=2.5) #ゴシックフォントを指定してpdfに保

#############相関係数の高い変数の散布図
###ProfileとLong
p<-ggplot(data)+geom_point(aes(x=longitudin_curve,y=Profile.Curve))+geom_smooth(aes(x=longitudin_curve,y=Profile.Curve),method=lm,colour="grey20")+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("point_long_profile.pdf", p, family="Japan1GothicBBB",width=4,height=3) #ゴシックフォントを指定してpdfに保
###slopeとwettness
p<-ggplot(data)+geom_point(aes(x=Slope,y=Wetness_index))+geom_smooth(aes(x=Slope,y=Wetness_index),method=loess,colour="grey100",alpha=60)+scale_fill_ptol()+theme_minimal()+theme(axis.text= element_text(size=7),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))+facet_wrap(~iseki)
ggsave("point_slope_wetness.pdf", p, family="Japan1GothicBBB",width=4,height=3) #ゴシックフォントを指定してpdfに保


################t検定
t.test(data$longitudin_curve~data$iseki,var.equal=FALSE)


[1] "iseki"            "Aspect_N"         "Aspect_E"         "longitudin_curve"
 [5] "Plan_Curve"       "Profile.Curve"    "Sky_View"         "Slope"           
 [9] "Visible_Sky"      "Wl_buffer"        "Elevation"        "Irradiance_172"  
[13] "Irradiance_355"   "maximal_curve"    "Wetness_index" 
















