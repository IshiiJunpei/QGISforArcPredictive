# QGISで遺跡立地分析

## この資料について

本資料は2018年9月19日に行われた奈良文化財研究所「遺跡情報記録課程」の「GIS演習」で使用した解説資料です。

## マスターすること

プロセッシング機能を利用して高機能なGRASS GISの分析機能を使用します。傾斜や傾斜方位の他に日射量の算出を行います。QGISに用意されている豊富なプラグインのインストール方法を学びながら「Point sampling tool」プラグインを利用して地形データの取得を行います。

以上のようにして取得した地形データから遺跡の立地条件を検討します。

- 標高データから新たな地形指標を作成する
- プロセッシング機能を使った他のGISソフト機能の利用
- プラグインを使う
- PointSamplingtoolを使用した地形データの取得
- GISデータを利用した統計処理
## ドキュメント

- [配布資料](https://github.com/IshiiJunpei/QGISforArcPredictive/blob/master/99QGIS%E3%81%A7%E9%81%BA%E8%B7%A1%E7%AB%8B%E5%9C%B0%E5%88%86%E6%9E%90.pdf)

-  [スライド](https://IshiiJunpei.github.io/QGISforArcPredictive)

# QGISで遺跡立地分析

Created by Ishii Junpei ( [@ishiijunpei](https://twitter.com/ishiijunpei))

## 作業の内容

- 標高データから新たな地形指標を作成する
- プロセッシング機能を使った他のGISソフト機能の利用
- ラインベクタから距離ラスタの作成
- 連続量から離散量へのデータ変換
- プラグインのインストール
- PointSamplingtoolを使用した地形データの取得
- GISデータを利用した統計処理

---
標高データと遺跡データをロード

<img src="01.png" width=90%>

---
ベクタデータはデータベースとしての側面があります。この遺跡分布図は表計算ソフトで作成したものをQGISに読み込んでシェープファイルに変換しています。

<img src="02.png" width=90%>


---
## プロセッシング機能

- とても高機能なGRASS GISの機能を使う
- GRASS GISで傾斜方位と傾斜角度、日射量を計算する
- GRASS GISのほかにもSAGA GISなどマニアックな機能をQGISから利用できる

---
「プロセッシング」→「ツールボックス」

<img src="08.png" width=90%>


「オプション」をクリック

<img src="09.png" width=90%>


「プロバイダ」→「GRASS」

→「有効化」にチェック

<img src="10.png" width=90%>

---
「GRASS」→「Raster」

→「r.slope.aspect」

<img src="11.png" width=90%>

---
「Elevation」→「merge2_utm54」

<img src="12.png" width=90%>

- 「Slope」と「Aspect」のチェックボックスにチェック
- 保存ファイル名は「slope」と「aspect」

<img src="13.png" width=90%>

---
GRASS GISの傾斜方位のトラップ（注意点）

- 原点は東
- 角度は半時計回り
- 東向きの斜面が0度、北向きの斜面は90度、西は180度、南は270度

<img src="16.png" width=50%>

---
傾斜方位

<img src="14.png" width=90%>

傾斜角度

<img src="15.png" width=90%>

---
日射量=「r.sun」

<img src="17.png" width=90%>

---
- 「Elevation layer」→「merg2_utm54」
- 「Aspect layer」→「Aspect」
- 「A single value...」→「270」（傾斜方位の「南」の値を指定）
- 「name of the input raster map」→「Slope」

<img src="18.png" width=90%>

「No. of day of the year」（1月1日を基点にした日数）→「173」（夏至の頃を指定） 

<img src="19.png" width=90%>

- 「Global(total) irradiance」（合計放射輝度）にチェック
- ファイル名「irradiation」

<img src="20.png" width=90%>


---
- 計算時間がかかるので注意
- 研修では作成済みの日射量ラスタ「irradiation.tif」を用意してあります。

<img src="21.png" width=90%>

---
日射量ラスタ「irradiation.tif」

<img src="22.png" width=90%>

---
### 河川距離ラスタの作成

河川からの距離をラスタ化し、距離地図を作成します。

- 「kokudo_WL_utm54」を開く

<img src="23.png" width=90%>

---
「ラスタ」→「変換」→「ラスタ化（ラスタのベクタ化）」

<img src="24.png" width=90%>

---
- 「入力レイヤ」→「kokudo_WL_utm54」
- 「A fixed value to burn」（データのあるところに入力する値）→1.0
- 「出力ラスターサイズの単位」→「Georeferenced units」（投影系上の単位　ここではm）
- 「幅/水平方向の解像度」→10

<img src="27.png" width=90%>


- 「出力領域」（ラスタ化する領域の端点を入力）→417000.0 459000.0 4621000.0 4659000.0
- 「出力バンドに指定されたnodata値を割り当てる」（データのないところに入力する値）→0

<img src="28.png" width=90%>

- 「ラスタ化」→WL.tif

<img src="29.png" width=90%>

---
あるいは一番下までスクロールしてコンソールに直接打ち込むことも可能です。

<img src="25.png" width=90%>

```
gdal_rasterize  -burn 1.0 -tr 10.0 10.0 -a_nodata 0.0
 -te 417000.0 4623000.0 458000.0 4658000.0
  kokudo_WL_utm54.shp WL.tif
```

- glal_rasterize　コマンド
- -burnオプション　データのあるところに代入する値（1を代入）
- -trオプション　解像度を指定（x座標、y座標ともに10mを指定）
- -teオプション　ラスタ化する対象範囲を指定（xmini,xmax,ymini,ymax）
- ofオプション　作成するラスタファイル形式（GTiffを指定）
- 入力ファイル　出力ファイル

---
ラスタ化（画像化）された河川データ

<img src="30.png" width=90%>

---
 「ラスタ」→「解析」→「Proximity」

<img src="31.png" width=90%>

---
- 「入力レイヤ」→「WL」（ラスタ化した河川データ）
- 「距離単位」→「ジオリファレンス座標」（実際の距離）

<img src="32.png" width=90%>

- 「ファイル名」→「WL_buffer」

<img src="33.png" width=90%>

---
河川からの距離地図

<img src="34.png" width=90%>

<img src="35.png" width=90%>

---
### 傾斜方位をカテゴリ化する

- 現時点で傾斜方位（Aspect.tif）は南をゼロとした連続量です。
- このままでは統計的に扱いにくいので離散量に変換します。
- カテゴリは「北」、「東」、「南」、「西」の4区分です。

---
「ラスタ」→「ラスタ計算機」

<img src="36.png" width=90%>

---
- 「出力レイヤ」→Aspect_reclass.tif
- 「選択レイヤの領域」をクリック（Aspectレイヤを選択しておく）

<img src="37.png" width=50%>

---
### ラスタ演算式

- 東が0で半時計回りに増加するラスタ地図
- 以下の式では次のような値が新たに代入される
- 東10　北20　西30　南40

```
("Aspect@1">0)*("Aspect@1"<=45)*10+
("Aspect@1">45)*("Aspect@1"<=135)*20+
("Aspect@1">135)*("Aspect@1"<=225)*30+
("Aspect@1">225)*("Aspect@1"<=315)*40+
("Aspect@1">315)*10
```

---
Aspectレイヤのバンド1を意味します。

```
"Aspect@1"
```

ピクセルが0以上なら計算機は「1」を返し、0以下なら「0」を返します。

```
"Aspect@1">0
```

ピクセルが45以下なら計算機は「1」を返し、45を超えると「0」を返します。

```
"Aspect@1"<=45
```

以下の式では0より大きく45以下の値は「1」を、それ以外はすべて0が返されます。

```
("Aspect@1">0)*("Aspect@1"<=45)
```

したがって、0以上45以下という条件を満たすピクセルには「10」が代入されます。

```
("Aspect@1">0)*("Aspect@1"<=45)*10
```

---
同様に45〜135（北）では20が代入され、135〜225（西）では30が代入され、225〜315（南）では40が代入され、315〜（東）は10が代入されます。

```
("Aspect@1">45)*("Aspect@1"<=135)*20
("Aspect@1">135)*("Aspect@1"<=225)*30
("Aspect@1">225)*("Aspect@1"<=315)*40
("Aspect@1">315)*10
```

---
以下の計算式で真となる（1が代入される）項は一つしかないので、全部の項を足し合わせると真となる項の数字だけが該当するピクセルに代入されます。

```
("Aspect@1">0)*("Aspect@1"<=45)*10+
("Aspect@1">45)*("Aspect@1"<=135)*20+
("Aspect@1">135)*("Aspect@1"<=225)*30+
("Aspect@1">225)*("Aspect@1"<=315)*40+
("Aspect@1">315)*10
```

---
四方位に分類されたAspcect_reclass

<img src="38.png" width=90%>

---
### ラスタのポリゴン化
<br>
- 方位を離散量に変更する
- 離散量ラスタをベクタ化する
- 方位ラスタをポリゴン化する

---
「ラスタ」→「変換」→「ポリゴン化」

<img src="39.png" width=90%>

---
ポリゴン化された斜面方位

<img src="40.png" width=90%>

---
### プラグインのインストール

- QGISに豊富な追加機能を提供する
- リポジトリからえらんでダウンロード

---
「プラグイン」→「プラグインの管理とインストール」

<img src="03.png" width=90%>

「Point sampling tool」→「インストール」

<img src="04.png" width=90%>

---
「プラグイン」→「Analysis」→「Point sampling tool」

<img src="05.png" width=90%>

---
- 「General」タブを選択
- サンプリングポイントレイヤに「IsekiData_utm54」を選択
- 値を取得したいレイヤを選択
- 出力レイヤは「.gpkg」一択

<img src="06.png" width=35%>

- 「Field」タブを選択
- 「name」フィールドをわかりやすくリネーム

<img src="07.png" width=50%>

「Iseki.gpkg」の「属性テーブル」に追加のデータが書き込まれていれば成功です

<img src="45.png" width=90%>

---
### ベクタ計算

- フィールドに10とか20とかの数値が入っている。
- 「東」「西」「南」「北」の文字列に置き換えたい。
- 「フィールド計算機」を使ったベクタ計算

---
フィールド計算機を開く

- 「出力フィールド名」→「aspect」
- 「出力フィールドタイプ」→「string」

<img src="42.png" width=60%>

---
構文は次のとおりです。

```
CASE
WHEN 条件式 THEN 入力値
END
```

DNフィールド値が「10」なら「東」、「20」なら「北」・・・と指定しています。

```
CASE 
WHEN "DN"=10 THEN  '東'
WHEN "DN"=20 THEN  '北'
WHEN "DN"=30 THEN  '西'
WHEN "DN"=40 THEN  '南'
END
```

---
あらたに作成された「aspect」フィールドに文字列が代入されます。

<img src="43.png" width=90%>

方位ごとに塗り分け

<img src="44.png" width=90%>

---
### 遺跡立地地点の地形指標をcsvに書き出す

- GISデータをcsvに出力する
- 表計算ソフトで統計処理

---
「Iseki.gpkg」→右クリック→「エクスポート」→「地物の保存」

<img src="47.png" width=90%>

- 「形式」→「カンマで区切られた値[CSV]」
- 「ファイル名」→「Iseki.csv」

<img src="48.png" width=70%>

このような表が作成できれば成功

<img src="49.png" width=90%>


---
### 地形データの可視化
今回の研修では統計処理については扱いませんが、以下のように変数の性質を整理して可視化の方法や分析の方法を検討していくこととなります。

- 連続量
	- データの分布=ヒストグラム
	- 2変量の関係=散布図
- 離散量
	- 棒グラフ
- 連続量×離散量
	- データの分布=ヒストグラム、箱ひげ図

---
### データ全体の可視化
<img src="R_graph/graph01.png" width=60%>

### 傾斜角度のヒストグラム
<img src="R_graph/graph02.png" width=70%>

### 日射量のヒストグラム
<img src="R_graph/graph03.png" width=70%>

### 斜面方位の棒グラフ
<img src="R_graph/graph04.png" width=70%>

---
### 遺跡のない領域と比較したい
遺跡の存在しないと考えられる地形のデータを取得し、遺跡立地地点と比較します。

- ランダムポイントの生成
- ランダムポイントに地形情報を付与
- 遺跡のデータと結合

---
「Clip_coast.shp」開く

<img src="50.png" width=90%>

---
「ベクタ」→「調査ツール」→「ポリゴン内のランダムポイント」

<img src="51.png" width=90%>

---
- 「入力レイヤ」→「ClipAria」
- 「サンプリング手法」→「ポイント数」
- 「式」→「300」（遺跡数と同数）
- 「ランダム点群」→ファイル名を「Random.shp」

<img src="52.png" width=90%>

---
クリップレイヤ領域内にランダム点群が生成されます。

<img src="53.png" width=90%>

---
「Point sampling tool」で地形指標を取得

<img src="05.png" width=90%>

---
「irradiation」、「Aspect reclass」、「WL buffer」、「Slope」、「merge2 utm」を選択

<img src="54.png" width=50%>

---
フィールド名を変更

<img src="55.png" width=50%>

---
フィールド計算機で方位を文字列に変換

```
CASE 
WHEN "DN"=10 THEN  '東'
WHEN "DN"=20 THEN  '北'
WHEN "DN"=30 THEN  '西'
WHEN "DN"=40 THEN  '南'
END
```

---
「Random.gpkg」→右クリック→「エクスポート」→「地物の保存」

<img src="56.png" width=90%>

---
- 「形式」→「カンマで区切られた値[CSV]」
- 「ファイル名」→「Iseki.csv」

<img src="57.png" width=50%>

---
このような表が作成できれば成功です

<img src="58.png" width=90%>

---
- 先に作成した「Iseki.csv」と結合
- 新たに「Class」カラムを作成して「遺跡」と「自然地形」を入力
- 「Merge.csv」という名称で保存

<img src="59.png" width=90%>

---
### 再び統計処理へ

調べたいことは何か？

- 遺跡立地に影響を与える地形指標を知りたい
- 「遺跡の有無」という離散量に対して、それ以外の連続量や離散量がどのように影響するか。

---
Rのggplot2パッケージとGGallyパッケージを利用して地形指標を一括で可視化する。

<img src="R_graph/graph10.png" width=70%>

```{r}
##パッケージ読み込み
library(ggplot2)
library(GGally)

##データ読み込み
merge<-read.csv("Merge.csv")

##必要なデータのみ選択
merge<-merge[,c(1,4:8)]

##散布図行列
p<-ggpairs(merge,diag=list(continuous="barDiag"))+theme_minimal()+theme(axis.text= element_text(size=5),legend.title = element_text(size=7),legend.text = element_text(size=7),axis.title = element_text(size=7),plot.title= element_text(size=7),strip.text=element_text(size=7))
#ゴシックフォントを指定してpdfに保存
ggsave("graph10.png", p, family="Japan1GothicBBB",width=6,height=6) 
```

---
離散量×連続量=「遺跡の有無」×「傾斜」

<img src="R_graph/graph05.png" width=70%>

```{r}
##傾斜ヒストグラム
p<-ggplot(merge)+geom_histogram(aes(x=Slope))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
#ゴシックフォントを指定してpdfに保存
ggsave("graph05.png", p, family="Japan1GothicBBB",width=5,height=4)
```

---
離散量×連続量=「遺跡の有無」×「日射量」

<img src="R_graph/graph06.png" width=70%>

```{r}
##日射量ヒストグラム
p<-ggplot(merge)+geom_histogram(aes(x=Irrad))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
xlim(c(4000, NA))+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
#ゴシックフォントを指定してpdfに保存
ggsave("graph06.png", p, family="Japan1GothicBBB",width=5,height=4) 
```

---
離散量×連続量=「遺跡の有無」×「標高」

<img src="R_graph/graph07.png" width=70%>

```{r}
p<-ggplot(merge)+geom_histogram(aes(x=Level))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
#ゴシックフォントを指定してpdfに保存
ggsave("graph07.png", p, family="Japan1GothicBBB",width=4,height=4) 
```

---
離散量×連続量=「遺跡の有無」×「河川からの距離」

<img src="R_graph/graph08.png" width=70%>

```{r}
p<-ggplot(merge)+geom_histogram(aes(x=WL))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
#ゴシックフォントを指定してpdfに保存
ggsave("graph08.png", p, family="Japan1GothicBBB",width=4,height=4) 
```

---
離散量×離散量=「遺跡の有無」×「斜面方位」

<img src="R_graph/graph09.png" width=90%>

```{r}
p<-ggplot(merge)+geom_bar(aes(x=Aspect))+
facet_wrap(~Class,ncol=1,scales="free_y")+
scale_fill_ptol()+theme_minimal()+
coord_flip()+
theme(axis.text= element_text(size=9),legend.title = element_text(size=9),legend.text = element_text(size=9),axis.title = element_text(size=9),plot.title= element_text(size=9),strip.text=element_text(size=9))
#ゴシックフォントを指定してpdfに保存
ggsave("graph09.png", p, family="Japan1GothicBBB",width=6,height=4) 
```

---
### GISと統計処理

- GISは地理情報の統計処理を行うためには不可欠
- ただし、GISが統計を行ってくれるわけではない
- 表計算ソフトは意外にも統計処理に向いていない
	- ヒストグラムは小学校で習う基本的なグラフですが、エクセルで作るのは案外難しい。

