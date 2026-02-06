##============如何读入RTF,并转换为数据框================##
setwd("E:/learn/R_code")

#如何读入rtf?
library(striprtf)
library(tibble)
library(stringr)
rtf_text <- read_rtf("output/tbl_pop2.rtf")

# 首先，去除可能存在的空行
rtf_text <- rtf_text[rtf_text != ""]
#跳过标题行
rtf_text <- rtf_text[c(-1,-2)]
#去掉开头行的"*|"和结尾的”|“，然后按”|“分割
clean_lines <- gsub("^\\*\\|\\s*|\\s*\\|\\s*$", "", rtf_text)
#模式A：匹配*|：^\\*\\|\\s*   或者| 模式B：匹配 | ：\\s*\\|\\s*
# ^--匹配行首、\\*--匹配字符*、\\|匹配字符|、\\s*---匹配0个或多个空白字符（空格、制表符等）

# 现在用”|“分割
split_lines <- str_split(clean_lines, " \\| ")
# 重新分割：先按"|"分割，然后去除每个元素两端的空格
split_lines <- lapply(rtf_text, function(line) {
  # 去除开头的"*|"和结尾的"|"
  line <- sub("^\\*\\|", "", line)
  line <- sub("\\|\\s*$", "", line)
  # 按"|"分割
  parts <- str_split(line, "\\|")[[1]]
  # 去除每个部分两端的空格
  trimws(parts)
})

# lapply函数将函数应用于列表或向量，返回与输入长度相同的列表。函数的语法如下：lapply(X,FUN,...)
# lapply函数可用于避免for循环，众所周知，如果使用不当，for循环在R中会很慢

#每行的长度不都是5，有的行是4，因为第一列前面有两个空格，所以被分割成了两个空字符串
split_lines <- lapply(rtf_text,function(line){
  # 去掉开头的“*”
  line <- sub("^\\*","",line)
  # 按"|"分割
  parts <- str_split(line,"\\|")[[1]]
  # 去除每个部分两端的空格
  parts <- trimws(parts)
  # 移除空字符串
  parts <- parts[parts != ""]
  return(parts)
  })

#[1] "Placebo" "Xanomeline line Low Dose" "Xanomeline line High Dose" 
# 但实际上，第3行应该有4个元素，因为第一列是空，但我们去掉了空字符串，所以只剩3个。这不对。
split_lines <- lapply(rtf_text,function(line){
  # 去掉开头的“*|”和结尾的“|”
  line <- sub("^\\*\\|","",line)
  line <- sub("\\|\\s*$","",line)
  # 按"|"分割
  parts <- str_split(line,"\\|")[[1]] #取每一行的元素
  # 固定取5个元素，如果不够，用NA填充
  if (length(parts) < 5){
    parts <- c(parts,rep(NA,5 - length(parts)))
  }
  # 去除每个部分两端空格
  parts <- trimws(parts)
  return(parts)
})

# 提取列名：第一行（表头）的第2列到第4列
col_names <- c("Analysis Set",split_lines[[1]][2:4])
# 提取数据行
data_rows <- split_lines[3:6]
# 将数据行转换为数据框
df <- as.data.frame(do.call(rbind,data_rows),stringsAsFactors = FALSE)

# 需要第1列到第4列（第5列是空列，我们不要）
df <- df[,1:4]
# 设置列名
names(df) <- col_names
#如果第一列有空格，去掉第一列中多余的空格
df$`Analysis Set` <- trimws(df$`Analysis Set`)

#====== 将上述过程封装为函数 ==============#
parse_rtf_table <- function(rtf_text){
  # 提取表格行，假设从第三行开始
  table_lines <- rtf_text[3:length(rtf_text)]
  
  # 清理和分割每一行
  split_lines <- lapply(table_lines,function(line){
    # 去掉开头的“*|”和结尾的“|”
    line <- sub("^\\*\\|","",line)
    line <- sub("\\|\\s*$","",line)
    # 按“|”分割
    parts <- str_split(line,"\\|")[[1]]
    # 固定取5个元素，如果不够用NA填充
    if (length(parts) < 5) {
      parts <- c(parts,rep(NA, 5-length(parts)))
    }
    # 去除每个部分两端的空格
    parts <- trimws(parts)
    return(parts)
  })
  
  # 提取列名：第一行（表头）的第2到第4列
  col_names <- c("Analysis Set",split_lines[[1]][2:4])
  # 数据行：从第3行开始（即第3个元素）到最后一个元素
  data_rows <- split_lines[3:length(split_lines)]
  
  # 构建数据框
  df <- as.data.frame(do.call(rbind,data_rows),stringsAsFactors = FALSE)
  df <- df[,1:4] # 取前4列
  names(df) <- col_names
  # 清理第一列的空格
  df$`Analysis Set` <- trimws(df$`Analysis Set`)
  return(df)
}

#使用函数
df <- parse_rtf_table(rtf_text)
