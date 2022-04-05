rm(list=ls())
library(shinyjs)

NAME = "895869134"
PASSWORD = "Smida1122"

# load CSV file
f_testAnswer  <-  read.csv("test_answer.csv")
f_testQuestion  <-  read.csv("test_question.csv")
f_test <-  read.csv("test.csv")

colnames(f_testAnswer)[1] <- 'Test'
colnames(f_testQuestion)[1] <- 'Test'
colnames(f_test)[1] <- 'Test'

testQA <- merge(f_testQuestion,f_testAnswer,by = c('Test','Question'))
testQA <- testQA[with(testQA,order(Test,Question)),]

testQA$right_answer <- as.numeric(testQA$right_answer)
# define some reusable functions

#----------------func 0-----------------
# a query to select between test
get_test<- function(num){
  testQA[testQA$Test == num, ]
}
#---------------------------------------

# ---------------func 1-----------------
# get questions for selected test
get_questions <- function(test_df,num){
  paste(test_df$Question[num],".",test_df$Problem[num])
}
# --------------------------------------

# ---------------func 2-----------------
# get answers for selected test
get_answers<- function(test_df, num){
  rowVal = as.list(test_df[num,])
  out = c()
  
  for (i in 4:length(rowVal)) {
    if(rowVal[i] != "" & i!=9){
      out <- c(out,as.character(rowVal[i]))
    }
  }
  out
}
# --------------------------------------


# ---------------func 3-----------------
# to random select answers and record the right one
shuffle_answers <- function(test_df){
  correct_answer  <-  c()
  for (i in 1:nrow(test_df)) {
    r <-  test_df[i, -c(1:3,9)]
    #print(r)
    answers  <-  r[r != ""]
    
    num_valid_answer  <-  length(answers)
    s <-  sample(num_valid_answer)
    correct_answer <- c(correct_answer,test_df[i,9])
    test_df[i, c(4:(3 + num_valid_answer))]  <-  answers[s]
  }
  foo <- vector("list", length = 2)
  names(foo) <- c("test_df", "correctAnswer")
  foo$test_df  <-  test_df
  foo$correctAnswer  <-  correct_answer
  foo
}
# --------------------------------------

# ---------------func 4-----------------
# get correct answer
get_correct_answer <- function(correctAnswer, num){
  correctAnswer[testQA$Test == num]
}

dG <- function(dList, answerList){
  c = rep(0,max(dList))
  d = rep(0,max(dList))
  for(i in 1:length(dList)){
    c[dList[i]] = c[dList[i]] + answerList[i]
    d[dList[i]] = d[dList[i]] + 1
  }
  c/d
}
assamble_table <- function(dG,t,grade,correctAnswer,selection){
  ct <- ''
  Q1_Q10 <- 0
  Q11_Q20 <- 0
  Q21_Q30 <- 0
  Q31_Q40 <- 0
  Q41_Q50 <- 0
  
  for(i in 1:length(selection)){
    
    if(selection[i]=='FALSE'){
      selection[i] <- -1
    }else{
      selection[i] <- as.numeric(selection[i])
    }
  }
  selection <- as.numeric(selection)
  for(i in 1:10){
    if(correctAnswer[i]==selection[i]){
      Q1_Q10 <- Q1_Q10+2
    }
  }
  for(i in 11:20){
    if(correctAnswer[i]==selection[i]){
      Q11_Q20 <- Q11_Q20+2
    }
  }
  for(i in 21:30){
    if(correctAnswer[i]==selection[i]){
      Q21_Q30 <- Q21_Q30+2
    }
  }
  for(i in 31:40){
    if(correctAnswer[i]==selection[i]){
      Q31_Q40 <- Q31_Q40+2
    }
  }
  for(i in 41:50){
    if(correctAnswer[i]==selection[i]){
      Q41_Q50 <- Q41_Q50+2
    }
  }
  total_score <- Q1_Q10+Q11_Q20+Q21_Q30+Q31_Q40+Q41_Q50
  if(t==1){
    ct <- paste(ct,'Supply chain manage',sep='')
  }
  if(t==2){
    ct <- paste(ct,'Security manage',sep='')
  }
  if(t==3){
    ct <- paste(ct,'data analyst',sep='')
  }
  if(t==4){
    ct <- paste(ct,'Project Manage',sep='')
  }
  if(total_score>0 & total_score<=30 &t!=3){
    ct<- paste('You are poor on ',ct,' knowledge',sep='')
  }
  if(total_score>30 & total_score<=60 &t!=3){
    ct<- paste(ct,' knowledge under the average',sep='')
  }
  if(total_score>60 & total_score<=80 &t!=3){
    ct<- paste(ct,' knowledge over average',sep='')
  }
  if(total_score>80 & total_score<=100 &t!=3){
    ct<- paste('You have competitive advantage on ',ct,' knowledge',sep='')
  }
  if(total_score>0 & total_score<=30 &t==3){
    ct<- paste('Your ',ct,' knowledge poor',sep='')
  }
  if(total_score>30 & total_score<=60 &t==3){
    ct<- paste('Your ',ct,' knowledge under the average',sep='')
  }
  if(total_score>60 & total_score<=80 &t==3){
    ct<- paste('Your ',ct,' knowledge over average',sep='')
  }
  if(total_score>80 & total_score<=100 &t==3){
    ct<- paste('You have competitive advantage on ',ct,' knowledge',sep='')
  }
  
  result <- data.frame(
    test = t,
    Q1_Q10=Q1_Q10,
    Q11_Q20=Q11_Q20,
    Q21_Q30=Q21_Q30,
    Q31_Q40=Q31_Q40,
    Q41_Q50=Q41_Q50,
    total_score=total_score,
    comment=ct
  )
  
  result
}

# ---------------initiating-----------------
foo  <-  shuffle_answers(testQA)
testQA  <-  foo$test_df 
correctAnswer  <-  foo$correctAnswer
# ------------------------------------------

server <- function(input, output, session) {
  
  # initiatives
  counters <- reactiveValues()
  counters$page <- 0
  counters$n <- 0
  counters$selection <- vector(length = 0)
  counters$test <- testQA
  counters$correctAnswer <- c()
  counters$grade <- 0
  
  #-----------------------------------------------------------------------------
  # switch between test, javascript include
  observeEvent(input$start,{
    if(!is.null(input$RBstart)){
      counters$test <- get_test(input$RBstart)
      counters$correctAnswer <- get_correct_answer(correctAnswer,input$RBstart)
      counters$n <- nrow(counters$test)
      counters$selection <- vector(length = counters$n)
      shinyjs::hide("RBstart")
      shinyjs::hide("start")
      shinyjs::hide("scoreButton")
      shinyjs::show("goButton")
      shinyjs::show("backButton")
      shinyjs::show("question")
      shinyjs::show("answers") 
      shinyjs::show("submitButton")
    }
  })
  
  
  observeEvent(input$submitButton,{
    shinyjs::hide("goButton")
    shinyjs::hide("backButton")
    shinyjs::hide("question")
    shinyjs::hide("submitButton")
    shinyjs::hide("answers")
    shinyjs::show("grade")
   
    dG = dG(
      counters$test$Question,
      as.integer(counters$correctAnswer == counters$selection)
    )
    result  <-  assamble_table(dG, input$RBstart, counters$grade,counters$correctAnswer,counters$selection)
    
    if(!file.exists("test_result.csv")){
      write.csv(result,"test_result.csv",row.names = F)
    }else{
      f  <-  read.csv("test_result.csv")
      f  <-  rbind(f, result)
      write.csv(f,"test_result.csv",row.names = F)
    }
  })
  
  viewscore <- observeEvent(input$scoreButton,{
    shinyjs::hide("RBstart")
    shinyjs::hide("start")
    shinyjs::show("toMain")
    shinyjs::show("nameInput")
    shinyjs::show("passwordInput")
    shinyjs::show("loginButton")
  })
  
  backscore<- observeEvent(input$toMain,{
    shinyjs::show("RBstart")
    shinyjs::show("start")
    shinyjs::show("scoreButton")
    shinyjs::hide("toMain")
    shinyjs::hide("nameInput")
    shinyjs::hide("passwordInput")
    shinyjs::hide("loginButton")
    shinyjs::hide("scoretable")
    shinyjs::hide("download")
  })
  #-----------------------------------------------------------------------
  
  page_forward <- observeEvent(input$goButton,{
    # ---------- test------------
    if(!is.null(input$answerButton)){
      counters$selection[counters$page+1] <- input$answerButton
    }
    # ---------------------------
    counters$grade <- sum(counters$correctAnswer == counters$selection)
    counters$page <- counters$page + 1
  })
  
  page_backward <- observeEvent(input$backButton,{
    counters$page <- counters$page - 1
  })
  
  output$question <- renderText({
    get_questions(counters$test,counters$page%%counters$n + 1)
  })
  
  output$answers <- renderUI({
    radioButtons("answerButton", "select from following", 
                 choiceNames = get_answers(counters$test,counters$page%%counters$n + 1), 
                 selected = counters$selection[counters$page%%counters$n+1],
                 choiceValues = 1:length(get_answers(counters$test,counters$page%%counters$n + 1))
    )
  })
  
  output$grade <- renderTable({
    assamble_table(
      dG(
        counters$test$Question,
        as.integer(counters$correctAnswer == counters$selection)
      ), 
      input$RBstart, 
      counters$grade,
      counters$correctAnswer,
      counters$selection
    )
  })
  
  output$scoretable <- renderTable({
    read.csv("test_result.csv")
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste("studentScore","csv",sep=".")
    },
    content = function(file){
      f = read.csv("test_result.csv")
      write.csv(f,file = file,row.names = F)
    }
  )
  
  # -------------this is for unit testing-----------------
  output$test <- renderPrint({
    counters$test$Domain
  })
  
  output$test2 <- renderPrint({
    as.integer(counters$correctAnswer == counters$selection)
  })
  # -------------------------------------------------
  
  # -------------this is for password-----------------
  observeEvent(input$loginButton,{
    if(input$nameInput == NAME && input$passwordInput == PASSWORD){
      shinyjs::hide("loginButton")
      shinyjs::show("scoretable")  
      shinyjs::show("download")
      shinyjs::hide("nameInput")
      shinyjs::hide("passwordInput")
      shinyjs::hide("scoreButton")
    }
  })
  # -------------------------------------------------
  
}



