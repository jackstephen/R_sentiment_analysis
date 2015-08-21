#######################################################
#2 解析词林
# 2.1 编码，符号，词提取，保存为codes, symbols, hwords
# 2.2 编码各层最大值计算 Ml1 ,Ml2, Ml3, Ml4, Ml5
# 2.3 拆分词串为单独各词，存入 words <- c()
# 2.4 数据存list(列表) dict
#Author:stephen
#Date:2015/7/17
#Update: 
######################################

paseWord <- function(words){
  #取编码，符号，词串
  codes <- apply(words,2,FUN=function(x) substr(x,1,7))
 # codes <- lapply(codes.m,clevel)
  symbols <- as.vector(apply(words,2,FUN=function(x) substr(x,8,8)))
  hwords <- apply(words,2,FUN=function(x) substr(x,10,nchar(x)))
  #把词串分拆为list,每个list元素就一个vector
  words <- strsplit(hwords," ")  
  return(list(codes=codes,symbols = symbols,words=words))
}


#################
#取对应词编码
######################
getCode <- function(w){
  wMatch <- lapply(dictList$words, FUN = function(x) return(x %in% w) )
  match.num <- which(unlist(lapply(wMatch, any)))
  code <- dictList$codes[match.num]
  symbol <- dictList$symbols[match.num]
  if(length(code)==0) return(NULL)
  return(list(code=code,symbol=symbol))
}

#################
#计算两词相似度
#4 计算词相似度
# 4.1 比较分叉层，确定各层系数c(0.1,0.65,0.8,0.9,0.96,1,0.5),
#     如果符号为’@’，相似度为0
#     其它如果在同一叶子节点符号为“=”，系数为1,符号为'#'，系数为0.5
#     其它相应层分叉对应相应系数：c(0.1,0.65,0.8,0.9,0.96,1,0.5)
# 4.2 计算相似度并，并返回两词相似度最大值
######################
#wd1 <- world1
#wd2 <- world2
calSim <- function(wd1,wd2){
  code1 <- wd1$code
  code2 <- wd2$code
  symbol1 <- wd1$symbol
  symbol2 <- wd2$symbol
  coe <- c(0.1,0.65,0.8,0.9,0.96)
  #编码分割格式化
  c1.fm <- clevel(code1)
  c2.fm <- clevel(code2)
  #取分叉层
  level.nums <- which(!(c1.fm==c2.fm))
  #如果在同一层，置层数为6,否取每日一个分叉点
  level.num <- ifelse(length(level.nums)==0,6,level.nums[1])
  #相似度计算
  #初始化符号系数
  
  if(all(symbol1=='=',symbol2=='=')){
    w_simbol <- 1
  }else {
    w_simbol <- ifelse(any(symbol1=='#',symbol2=='#'),0.5,0.0)
  }
  #如果存在一个符号是'@',直接返回0
  if(any(symbol1=='@',symbol2=='@')){
    #print('符号为@,直接返回0.0')
    return(0.0)
  }
  #如果在第一层就分叉，直接返回0.1
  if(level.num == 1){
    #print('第一层分叉，返回0.1')
    return(0.1)
  }
  #如果在同一层，根据根据符号不同返回不同'=':1,'#':0.5
  if(level.num==6){
    if(symbol1=='=') return(1.0)
    if(symbol1=='#') return(0.5)
  }
  #初始系数
  e = coe[level.num]
  #取当层总数
  n <-getTotal(code1,level.num) 
  #两编码距离（同一层）
  #如果3，5层，将编码转换成数据相减，如果是2、4层用ascii码来比较
  k=0
  if(level.num %% 2 == 1){
    k <- abs(as.numeric(c1.fm[level.num])-as.numeric(c2.fm[level.num]))
  }else{
    k <- abs(as.numeric(charToRaw(c1.fm[level.num]))-as.numeric(charToRaw(c2.fm[level.num])))
  }
  sim <- w_simbol*e*cos(n*(pi/180))*((n-k+1)/n) 
  return(sim)
}

################
#拆分编码并合并成向量
###############
clevel <- function(c){
  return(c(substr(c,1,1),substr(c,2,2),substr(c,3,4),substr(c,5,5),substr(c,6,7)))
}

################
#取当层总数
###############
#wd <- 'Aa01B01'
#lev <- 2
getTotal <- function(wd,lev){
  #如果lev是1，返回 0
  if(lev == 1) return(0)
  cur.level <- lev
  pre.level <- lev-1
  if(lev >= 3){
    cur.level <- ifelse(lev > 4,lev+2,lev+1)
    pre.level <- ifelse(lev >= 4,lev,pre.level)   #修改为大于等于4
  }
  #pre.level <- ifelse(cur.level %% 2 == 1,cur.level-2,cur.level-1)
  mat <- dictList$codes[substr(dictList$codes,1,pre.level)==substr(wd,1,pre.level)]
  mat.kw <- substr(mat,pre.level+1,cur.level)
  k <- 0
  if(lev %% 2 == 1){
    k <- abs(as.numeric(max(mat.kw))-as.numeric(min(mat.kw)))
  }else{
    k <- abs(as.numeric(charToRaw(max(mat.kw)))-as.numeric(charToRaw(min(mat.kw))))
  }
  return(k+1)
}

#############################
#将有多个编码返回list进行转换
#list(list(code='001',sybole='='),list(code='001',sybole='='))
#Modified by stephen at 2015/7/31 增加未发现语处理
#############################
paseList <- function(worlds){
  if(is.null(worlds)) return(worlds)
  len <- length(worlds$code)
  wl <- list()
  if(len==1) wl[[1]]<-worlds
  for(i in 1:len){
    wl[[i]] <- list(code=worlds$code[i],symbol=worlds$symbol[i])
  }
  return(wl)
}


