### This script is to predict and make submission

data.pred <- read.csv(file = 'test.csv', stringsAsFactors = F)
data.pred$search_term = stemDocument(tolower(data.pred$search_term))

termLength = c()
for (i in 1:nrow(data.pred)) {
      term.current = data.pred$search_term[i]
      termLength[i] = length(unlist(str_extract_all(term.current, ' '))) + 1
      print(i)
}

termInTitleRatio = c()
for (i in 1:nrow(data.pred)) {
      termList = unlist(str_split(data.pred$search_term[i], ' '))
      term.total.current = length(termList)
      title.current = data.pred[i, 'product_title']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, title.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInTitleRatio[i] = numIn / term.total.current
      print(i)
}

termInTitleDummy = c()
for (i in 1:nrow(data.pred)) {
      termInTitleDummy[i] = ifelse(termInTitleRatio[i] != 0, 1, 0)
      print(i)
}

termInDescRatio = c()
for (i in 1:nrow(data.pred)) {
      termList = unlist(str_split(data.pred$search_term[i], ' '))
      term.total.current = length(termList)
      id.current = data.pred[i,'product_uid']
      desc.current = data.product[data.product$product_uid == id.current, 'product_description']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, desc.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInDescRatio[i] = numIn / term.total.current
      print(i)
}

termInDescDummy = c()
for (i in 1:nrow(data.pred)) {
      termInDescDummy[i] = ifelse(termInDescRatio[i] != 0, 1, 0)
      print(i)
}

termInTitleCount = c()
for (i in 1:nrow(data.pred)) {
      termList = unlist(str_split(data.pred$search_term[i], ' '))
      term.total.current = length(termList)
      title.current = data.pred[i, 'product_title']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, title.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInTitleCount[i] = numIn
      print(i)
}

termInDescCount = c()
for (i in 1:nrow(data.pred)) {
      termList = unlist(str_split(data.pred$search_term[i], ' '))
      term.total.current = length(termList)
      id.current = data.pred[i,'product_uid']
      desc.current = data.product[data.product$product_uid == id.current, 'product_description']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, desc.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInDescCount[i] = numIn
      print(i)
}

termFirstInTitle = c()
for (i in 1:nrow(data.pred)) {
      termList = unlist(str_split(data.pred$search_term[i], ' '))
      term.length = length(termList)
      title.current = data.pred[i, 'product_title']
      termIndex = 0
      termFirstInTitle[i] = 0
      for (termIndex in 1:term.length) {
            term.current = termList[termIndex]
            if (grepl(term.current, title.current, ignore.case = T) == T) {
                  termFirstInTitle[i] = termIndex
                  break()
            }
      }
      print(i)
}

termFirstInDesc = c()
for (i in 1:nrow(data.pred)) {
      termList = unlist(str_split(data.pred$search_term[i], ' '))
      term.length = length(termList)
      desc.current = data.product[i,'product_description']
      termIndex = 0
      termFirstInDesc[i] = 0
      for (termIndex in 1:term.length) {
            term.current = termList[termIndex]
            if (grepl(term.current, desc.current, ignore.case = T) == T) {
                  termFirstInDesc[i] = termIndex
                  break()
            }
      }
      print(i)
}

########################################################

data.do = data.frame(termLength = termLength, 
                     termInTitleRatio = termInTitleRatio, termInTitleDummy = termInTitleDummy, 
                     termInDescRatio = termInDescRatio, termInDescDummy = termInDescDummy,
                     termInTitleCount = termInTitleCount, termInDescCount = termInDescCount,
                     termFirstInTitle = termFirstInTitle, termFirstInDesc = termFirstInDesc)
save(data.do, file = 'data.do.Rda')

########################################################
pred = predict(fit.rf.num, data.do)
data.submit <- data.frame(id = data.pred$id, relevance = pred)
write.csv(data.submit, file = 'submission2.csv', quote=FALSE, row.names = FALSE)
