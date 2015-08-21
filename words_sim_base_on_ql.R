# 同一分支‘#’相似度：e=0.5
#  同一分支相似度：1
# 第一层分支，相似度：sim(A,B)=f=0.1
# 第二层分支，相似度：sim(a,b)= 1*a*cos(n*(pi/180))*((n-k+1)/n)  :  a=0.65
# 第三层分支，相似度：sim(a,b)= 1*b*cos(n*(pi/180))*((n-k+1)/n)  :  b=0.8
# 第四层分支，相似度：sim(a,b)= 1*c*cos(n*(pi/180))*((n-k+1)/n)  :  c=0.9
# 第五层分支，相似度：sim(a,b)= 1*d*cos(n*(pi/180))*((n-k+1)/n)  :  d=0.96

#基于哈工大同义词词林扩展版计算词之间相关性
#1 导入词林
#2 解析词林
# 2.1 编码，符号，词提取，保存为codes, symbols, hwords
# 2.2 拆分词串为单独各词，存入 words <- c()
# 2.3 数据存list(列表) dict
#3 词搜索，返回编码及符号
#4 计算词相似度
# 4.1 比较分叉层，确定各层系数c(0.1,0.65,0.8,0.9,0.96,1,0.5),
#     如果符号为’@’，相似度为0
#     其它如果在同一叶子节点符号为“=”，系数为1,符号为'#'，系数为0.5
#     其它相应层分叉对应相应系数：c(0.1,0.65,0.8,0.9,0.96,1,0.5)
# 4.2 计算相似度并，并返回两词相似度最大值
# Modified by stephen at 2015/7/31 增加未登陆词处理


wordSimilarity <- function(w1,w2){
  #搜索对应词对应编码
  world1 <- paseList(getCode(w1))
  world2 <- paseList(getCode(w2))
  if(any(is.null(world1),is.null(world2))){
    print(paste('ERROR:找不到词',w1,'或',w2))
    return(NULL)
  }
  #计算词相似度
  print
  wsim <- vector()
  for(i in world1){
    for(j in world2){
      wsim <- c(wsim,calSim(i,j))
    }
  }
  return(max(wsim))
# return((wsim))
}

#####################
#字典初始化
#####################
#导入词林
dictInit <- function(){
  hdict <<- read.table(file='E:\\stephen\\dict\\hdict.txt',
                      comment.char = "|",sep = '|',stringsAsFactors=FALSE)
  #解析词林
  dictList <<- paseWord(hdict)
}
#计算两个词相似度
#初始化
dictInit()
w1 <- '车胎'
w2 <- '方向盘'
wordSimilarity(w1,w2)

