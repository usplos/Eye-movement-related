latinsquare <- function(len, reps=1, seed=NA, returnstrings=FALSE){ 
  
  # 保存旧的随机种子，若有新的随机种子则使用新的 
  if (!is.na(seed)) { 
    if (exists(".Random.seed")) { saved.seed <- .Random.seed } 
    else { saved.seed <- NA } 
    set.seed(seed) 
  } 
  
  # 构建一个大矩阵用于保存所有的单个方阵 
  allsq <- matrix(nrow=reps*len, ncol=len) 
  
  # 根据参数设置保存方阵的字符串id 
  if (returnstrings) { squareid <- vector(mode = "character", length = reps) } 
  
  # 构建函数用于随机获得向量中的一个元素（内置的sample函数在向量中只有一个元素时不太好用） 
  
  sample1 <- function(x) { 
    if (length(x)==1) { return(x) } 
    else { return(sample(x,1)) } 
  } 
  
  # 方阵生成（依次生成n个中的每一个） 
  for (n in 1:reps) { 
    
    # 构建一个空的方阵 
    sq <- matrix(nrow=len, ncol=len) 
    
    # 如果我们从方阵的左上角开始填充元素，将会导致拉丁方阵之间被生成的概率的不均衡， 
    # 为了避免这种情况的发生，我们需要加入一个随机机制，让元素填充的起点可以是方阵中的任意位置。 
    
    # 粗略来讲，这整个机制要包含以下流程： 
    # - 随机选择方阵中的一个空位（我们将其称为target） 
    # - 找到与target属于同行或同列的所有空位 
    # - 用数字填充target 
    # - 用其他数字填充与target属于同行或同列的所有空位 
    # - 如果所有可用的数字都无法满足填充的条件，就退出并构建一个空的方阵重新开始上述流程。 
    
    # 简而言之，我们的程序将会随机找到一个空位进行填充，之后填充其同行或同列的其他空位， 
    # 这是一个接近完全随机的过程，所以也面临着很高的失败几率。 
    while (any(is.na(sq))) { 
      
      # 随机选择方阵中的一个空位 
      k <- sample1(which(is.na(sq))) 
      
      i <- (k-1) %% len +1 # 获得行序号 
      j <- floor((k-1) / len) +1 # 获得列序号 
      
      # 找到在i行j列的其他空位 
      sqrow <- sq[i,] 
      sqcol <- sq[,j] 
      
      # 用矩阵记录满足条件的其他空位的坐标 
      openCell <-rbind( cbind(which(is.na(sqcol)), j), 
                        cbind(i, which(is.na(sqrow)))) 
      # 打乱数字填充顺序 
      openCell <- openCell[sample(nrow(openCell)),] 
      
      # 将中心空位至于列表首位，以保证先对它进行填充 
      openCell <- rbind(c(i,j), openCell) 
      # 现在openCell中记录了三次中心空位的坐标，所以我们要先进行一次去重。 
      # 在此基础上，我们需要确保openCell是一个矩阵（matrix）—— 
      # 因为在上面的过程中，如果它只有一行元素，就很可能被自动转换为一个向量，进而影响我们的后续操作。 
      openCell <- matrix(openCell[!duplicated(openCell),], ncol=2) 
      
      # 用数字填充中心空位，之后填充同行或同列的其他空位 
      for (c in 1:nrow(openCell)) { 
        # 选择要填充的空位 
        ci <- openCell[c,1] 
        cj <- openCell[c,2] 
        # 得到在第i行或j列中没有用到的数字 
        freeNum <- which(!(1:len %in% c(sq[ci,], sq[,cj]))) 
        
        # 用数字填充空位 
        if (length(freeNum)>0) { sq[ci,cj] <- sample1(freeNum) } 
        else { 
          # 失败的情况——找不到可用的填充数字 
          # 重新构建空方阵 
          sq <- matrix(nrow=len, ncol=len) 
          
          # 退出循环 
          break; 
        } 
      } 
    } 
    
    # 将单个方阵存入矩阵 
    allsqrows <- ((n-1)*len) + 1:len 
    allsq[allsqrows,] <- sq 
    
    # 根据参数设置存储方阵对应的字符串id，每个方阵都对应一个唯一的id 
    if (returnstrings) { squareid[n] <- paste(sq, collapse="") } 
    
  } 
  
  # 还原旧的随机种子 
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed } 
  
  if (returnstrings) { return(squareid) } 
  else { return(allsq) } 
}
