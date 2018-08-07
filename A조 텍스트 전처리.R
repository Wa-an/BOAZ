# 텍스트마이닝이란?
# -자연어 처리 기법을 통해 반정형 혹은 비정형 데이터를 정형화한 후, 
# 언어적, 통계적 기법을 적용하여 의미 있는 정보를 추출하는 작업
# 이러한 텍스트마이닝을 통해 SNS에 올라오는 다양한 글이나 
# 각종 비정형화 된 문서들을 분석하여 새로운 의미를 도출해낼 수 있다. 
# 즉, 쉽게 말하자면 '문자로 된 데이터에서 가치 있는 정보를 얻어내는 분석기법'

# 한글 텍스트마이닝을 위해서는 java가 설치되어야 한다. 설치 후 아래의 패키지를 설치해주자.
install.packages("dplyr")
install.packages("rJava")
install.packages("KoNLP")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("stringr")
install.packages("wordcloud")
#install.packages(c("rJava","KoNLP","tm","RColorBrewer",“stringr”,"wordcloud"))

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-10.0.1') 
#java가 깔렸는데도 라이브러리가 실행이 안 될 때는 위와 같이 설정하면 된다.

library(dplyr) # 편집 함수
library(rJava) # 한글 텍스트마이닝 라이브러리인 KoNLP을 실행하기 위해 필요
library(KoNLP) # 한글 문자열  처리를 위한 라이브러리
library(tm)  # 영문 문자열  처리를 위한 라이브러리
library(RColorBrewer)  # 워드클라우드 색상 지정용 라이브러리
library(stringr)  #문자열 처리를 위한 라이브러리
library(wordcloud)  # 워드클라우드(텍스트 마이닝 시각화) 라이브러리

###################형태소 분석################
# 형태소 분석(Morephology Analysis): 텍스트 마이닝 할때 가장 먼저 하는 작업, 어절들의 품사 파악
# 형태소 분석으로 어절들의 품사를 파악후 명사,동사, 형용사 등 의미를 지닌 품사의단어를 추출해 
# 각 단어가 얼마나 많이 등장했는지 확인하다.
# 형태소 분석을 하기 위해서는 참조할 사전이 필요
# ->분석할 문장에 포함된 단어들이 사전에 알맞은 형태(품사)로 포함되어야 정확한 분석 가능
# NIADic사전(가장 최신버전)과 system사전과 Sejong사전 중 Sejong 사전을 사용해보자.

useSejongDic()

#'아버지가 방에 스르륵 들어가신다.' 분석
sentence<-'아버지가 방에 스르륵 들어가신다.'
extractNoun(sentence) #명사 추출함수

#스스륵은 명사가 아니라 부사, 세종 사전에 스르륵이란 단어가 포함되어 있지 않음
#따라서, mergeUserDic이라는 사전에 단어를 추가하는 함수 사용
#앞부분에는 내가 사전에 추가하고자 하는 단어, 뒤에는 해당 단어의 품사를 선언
#품사의 종류는 KAIST 품사 태그셋 참조 스르륵은 일반부사임으로 mag 태그를 달아주었음
mergeUserDic(data.frame(c('스르륵'),c('mag')))
extractNoun(sentence) #스르륵이 없어짐

#이제 'MorphAnalyzer'이라는 형태소 분석 함수 이용
MorphAnalyzer(sentence)
#어절 단위로 분석이 됨, 각 어절에 대해서는 가능한 모든 상황에 대한 분석이 행해짐
#각 단어에 대한 품사를 확정 짓는다기 보다는 형태소를 분석한 결과를 산출해줌

#이번에는 품사를 확정시킬 뿐 아니라 각 단어들에 태그를 붙여주는 함수인
#'SimplePos09'함수와 'SimplePos22'함수를 사용할 것임
#'https://github.com/haven-jeon/KoNLP/blob/master/etcs/figures/konlp_tags.png
#위의 주소에 나와있는 KAIST품사 태그셋을 보면 두 함수의 차이가
#분류 수준의 차이라는 것을 알 수 있다. 
#'SimplePos09'함수는 가장 상위 분류인 9개의 기준으로만 분류하고
#'SimplePos22'함수는 그 다음 분류인 22개의 기준으로 분류
#가장 상세한 수준의 분류는 'MorphAnalyzer'이다.
SimplePos09(sentence)
SimplePos22(sentence)

###########본격적인 텍스트마이닝(워드 클라우드, 단어 빈도표)#############

#1. 데이터 불러오기(예전 멜론 1위~50위 노래 가사집 참조)
txt <- readLines("hiphop.txt") 
  #readLines는 파일을 행단위로 읽어 벡터로 저장해주는 함ㅅ
head(txt)

#2. 불용어 처리-특수문자 제거하기(밑에서 더 자세서하게 다룰 예정)
  #-stringr패키지의 str_replace_all()을 이용해서 문장에 들어있는 특수문자를 빈값으로 수정
txt <- str_replace_all(txt, "\\W", " ")

#3. 가장 많이 사용되는 단어 추출
  #가사에서 명사 추출
nouns <- extractNoun(txt) #extractNoun함수는 결과를 리스트형태로 반환

  # 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))

  # 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

  # 변수명 수정
?rename
df_word<-rename(df_word,
                word = Var1,
                freq = Freq)

  #전처리-filter 함수
  #글자수가 한 개인 단어는 무의미한 경우가 많으므로 글자수가 2개 이상인 단어들만 추출
df_word <- filter(df_word, nchar(word) >= 2)
  #df_word <- filter(function(x){nchar(x)>=2}, df_word)도 같은 결과를 추출함
  #이것만으로도 데, 등, 도, 수 와 같은 수많은 의존명사들을 거를 수 있다. 
  #한국어문법상 의존명사도 실질형태소이자 자립형태소이기 때문에 이 과정은 필수

  #빈도 수가 가장 많은 글자 순으로(내림차순)정리
top_20 <- df_word %>%arrange(desc(freq)) %>%head(20)
top_20

#4. 단어 빈도 막대그래프 생성
  #- ggplot2를 이용하여 막대 그래프 만든다.
library(ggplot2)

  # 빈도 순서 변수 생성
order <- arrange(top_20, freq)$word               
ggplot(data = top_20, aes(x = word, y = freq)) +  
  ylim(0, 100) +    
  geom_col() + 
  coord_flip() +
  scale_x_discrete(limit = order) +              # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3)     # 빈도 표시

#5. 워드클라우드 생성
  #RColorBrewer패키지의 brewer.pal()을 이용해 색상 코드를 만든다.
pal<-brewer.pal(8,"Dark2")

  #워드클라우드는 함수를 실행할 때마다 난수를 이용해서 매번 다른 모양의 워드클라우드를 만든다.
  #따라서 동일한 워드클라우드가 생성되도록 난수를 고정시킨다.
set.seed(1234)

  #워드클라우드 생성: 함수 wordcloud() 사용
  #scale: 가장 빈도가 큰 단어와 가장 빈도가 작은단어 폰트사이의 크기차이 scale=c(10,1)
  #minfreq: 출력될 단어들의 최소빈도
  #maxwords: 출력될 단어들의 최대빈도(inf:제한없음)
  #random.order: true면 랜덤, false면 빈도수가 큰 단어를 중앙에 배치하고 작은순)
  #random.color: true면 색 랜덤, false면 빈도순
  #rot.per: 90도 회전된 각도로 출력되는 단어비율
  #colors: 가장 작은빈도부터 큰 빈도까지의 단어색
  #family: 글씨체

wordcloud(words = df_word$word,    # 단어
          freq = df_word$freq,     # 빈도
          min.freq = 10,           # 최소 단어 빈도
          max.words = 200,         # 표현 단어 수
          random.order = F,        # 고빈도 단어 중앙 배치
          rot.per = .1,            # 회전 단어 비율
          scale = c(6, 0.2),       # 단어 크기 범위
          colors = pal)            # 색상 목록
  #words 수만큼 freq (빈도)를 나타낸다.
  #등장하는 단어의 가장 작은 빈도 수는10로, 등장하는 단어의 가장 큰 빈도 수는 200이다.
  #빈도가 가장 큰 단어를 중앙에 두도록 하기 위해 random order는 False 값을 준다.
  #scale(폰트의 크기)은 최고 6픽셀에서, 제일 작은건 0.2 픽셀까지
  #rotation되는 단어의 빈도는 0.1정도로 하고, 
  #컬러는 위에서 정한 pal의 값으로 컬러 팔레트를 사용한다



########텍스트마이닝의 다른 예제(전처리가 매우 어려운 예제)#############

#1. 데이터 불러오기
  #영화 박열의 네이버 리뷰를 크롤링해 .txt 파일로 저장한 것을 사용
data <- readLines("review.txt")
head(data)수

#2. 명사 추출
  #extractNoun으로 명사를 추출   
  #sapply() 함수를 사용할 건데 function에 extractNoun을 넣으면 된다.
  #리뷰 텍스트파일인 만큼 컬럼은 필요없으니 USE.NAMES=F 라는 옵션으로 컬럼명을 뺀다.
data <- sapply(data, extractNoun, USE.NAMES = F)
  
  #sapply는 리스트형태로 리턴 
  #분석을 위해 unlist()를 이용해 다시 벡터로 변환
data_unlist <- unlist(data)

  #빈도수를 출력하기 위해 table 함수 사용
wordcount <- table(data_unlist)
head(wordcount)

  #head()를 이용해 최대빈도 100개 정도면 뽑아 다른 변수에 넣음
wordcount_top <-head(sort(wordcount, decreasing = T),100)
wordcount_top

#3. 전처리 제 1과정: 등록
  #이제 전처리가 필요 
  #영화리뷰는 신조어 등으로 제대로 된 데이터를 뽑기가 힘들다. 
  #사전에 없는 단어들이고, 그것마저 사람마다 여러 형태로 변형해서 쓰며 
  #띄어쓰기는 고사하고 오타마저 너무나 빈번
  #앞에 나온 mergeUserDic() 을 사용해 우리가 수동으로 단어들을 등록 ex) '꿀잼', '노답'
mergeUserDic(data.frame(c("노잼"), "ncn")) #ncn은 형태소 분석상 명사라는 뜻
  #한꺼번에 등록하기 위에 메모장으로 단어집을 만들고 등록하는 방법도 있음
mergeUserDic(data.frame(readLines("addNoun.txt"), "ncn"))

  #등록 후 위에 과정 다시 수행
data <- sapply(data, extractNoun, USE.NAMES = F)
data_unlist <- unlist(data)

#4. 전처리 제 2과정: filter(2글자 이상의 단어들만 보게 하기)
data_unlist <- Filter(function(x){nchar(x)>=2}, data_unlist)
wordcount <- table(data_unlist)
wordcount_top <-head(sort(wordcount, decreasing = T),100)
wordcount_top

#5. 전처리 제 3과정: 불용어(의미가 없는 단어)처리
  #위에 사용했던 str_replace_all() 함수 말고 
  #gsub(찾을 단어, 바꿀 단어, 찾을 곳)을 이용하여 바꿔줄 수 있다.
  #내가 워드클라우드를 만들고자 할 때 필요없거나 원하지 않는 단어를 제거함은 물론 
  #동사나 형용사의 활용형을 기본형으로 바꾸거나 ( ex)달리는 -> 달리다 )
  #같은 명사인데 다른 조사나 보조사가 붙은 것들을 모두 제거하여 하나의 단어로 통일시키는 등 
  #빠져서는 안되는 필수적 과정
  
 #영화 "박열" 리뷰임을 이미 알고 있는데 영화와 박열이라는 단어가 많으므로 제외시킨다.
data_unlist <- gsub("영화", "", data_unlist)
data_unlist <- gsub("박열", "", data_unlist)
  
  #그러나 이 방법은 사실상 비효율적이다. 
  #'영화'는 지워지겠지만 '영화를' 이라는 단어는 
  #'영화'가 ""로 대체되어 '를' 이라는 단어가 된다. 
  #'이영화를' '그영화는' 이렇게 띄어쓰기를 하지 않아 한 단어로 분류되어버린 단어가 있었다면
  #'영화'가 ""로 사라졌으므로 '이를', '그는' 이라는 전혀 딴판의 단어로 바뀌어 버린다.
  #여기서는 '정규표현식'이라는 것이 필요(지금 다루지는 않을 것임)
  #R 정규식 : https://statkclee.github.io/ds-authoring/regex-r.html 참조

  #특수문자 삭제
data_unlist<- gsub('[~!@#$%&*()_+=?<>]','',data_unlist)
  #아마 저 안에 등록해도 몇몇 특수문자는 삭제가 잘 되지 않을 것이다.
  #용도가 정해진 특수문자들이 있는데, R에서 그 특수문자를 걸러버리기 때문(위 링크 참조)
  #따라서 R에서는 \\를 앞에 사용하면 텍스트 그대로 인식하니 삭제하면 된다.

data_unlist <- gsub("\\[","",data_unlist)

  #자음 삭제(ㅋㅋ,ㅎㅎ,ㅇㅇ 등)
data_unlist <- gsub('[ㄱ-ㅎ]','',data_unlist)

  #ㅜㅜ나 ㅠㅠ 등 삭제
data_unlist<- gsub('(ㅜ|ㅠ)','',data_unlist)
  
  #숫자 삭제
data_unlist <- gsub("\\d+","",data_unlist)

  #이렇게 여러 불용어처리 후에는, 위의 필터를 이용해 한번 더 2글자 미만 단어를 걸러줘야 한다.
  #"" 눈속임으로 빈 칸이 되어버리고 붙어있던 특수문자나 숫자, 
  #자모가 없어지면서 2글자 미만 단어가 많이 생겼을 것이기 때문
data_unlist <- Filter(function(x){nchar(x)>=2}, data_unlist)
wordcount <- table(data_unlist)
wordcount_top <-head(sort(wordcount, decreasing = T),100)
wordcount_top
  #아직도 어이없는 데이터가....  불용어처리가 덜 되었기 때문이다.
  #마찬가지로 따로 텍스트파일을 저장해서 쓸모없는 단어들을 교체하거나 지우는 과정을 한다.
  
#############정규식의 예#################
  #단어들을 보다 보면 띄어쓰기가 되지 않아 단어로 인식하지 못한 것들을 찾을 수 있다.
  #안타깝게도(멍청하게도) 단어를 사전에 추가했지만 인식하지 못하는 경우들이 있다.
  #'박열이라는사람이', '박열이이렇게나' '박열에게꼭' ... 과 같은 경우이다.
  #보통 이런 경우 '박열'이라는 명사를 제외한 뒤의 
  #어미나 조사 등등의 형식형태소들은 필요없다.

  #이 gsup의 바꿀 단어 안에 정규식을 이용
data_unlist = gsub("박열\\S*", "박열", data_unlist)
  #\S 라는 말은 박열 뒤에 붙은 공백,탭,개행을 제외한 모든 문자라는 말이고, 
  #뒤의 *는 뒤에 몇글자가 오든 상관없다는 뜻이다.
  #즉 "박열\\S*"의 뜻은 
  #'박열이라는 단어 뒤에 뭐가 얼마나 붙었는지간에' 라는 의미이다.  

  #이렇게 하면 박열로 시작하고 뒤에 뭐가 붙었든 전부 선택해서 박열로 바꿔버리게 된다. 
  #언급한 바와 같이 어떤 단어에 접두사와 접미사(혹은 보조사나 조사) 등이 모두 붙어버렸으면
  #단순 gsup()으로는 전혀 다른 단어를 만들어버릴 수 있기 때문에 추천하지 않는다.
  #정규식을 활용해 각자 열심히 전처리를 해보는 것이 좋다.
  #그리고 gsup()도 mergeUserDic()과 마찬가지로 
  #메모장에 단어들을 등록해놓고 한꺼번에 불용어처리 할 수 있다.

  # <정규식참조>
  # *: 적어도 0 번 매칭한다.
  # +: 적어도 1 번 매칭한다.
  # ?: 많아아 1 번 매칭한다.
  # {n}: 정확히 n 번 매칭한다.
  # {n,}: 적어도 n 번 매칭한다.
  # {n,m}: n 번에서 m 번 매칭한다.


###########기타 R전처리의 문제점(?)들#################
  #조사와 어미 등이 어간에 붙어버리면 이걸 구별할 방법이 R에서는 사실상 없고 
  #띄어쓰기가 잘 안 된 문장도 형태소분석이 거의 불가하다.(반복 작업이 필요)
  #예를 들어
  #'아버지가방에들어가신다'를 KoNLP로 분석하면 
  #''아버지가방에들어가신'이 명사로, '다'가 어미로 분류된다.
  #띄어쓰기가 되어있지 않으면 거의 분석이 불가한 수준임을 알 수 있다.
  
  #'구경꾼'이라는 단어를 분석하면 '구경'과 '꾼'으로 나눠버립니다.
  #구경꾼은 구경이라는 명사에 명사파생접미사 -꾼이 붙어 구경꾼이라는 하나의 명사가 된 것
  #'꾼'이라는 단어가 있기는 하지만 이건 띄어쓰기가 되어있지 않음에도 나눠진다. 

  #마찬가지로 고급스럽다, 평화롭다 등등 파생접미사가 붙은 단어들은 거의 분류하지 못한다 
  #이외에도 문제점이 매우매우 많지만 생략하고,
  #자바의 형태소분석기 라이브러리들은 조금 더 똑똑하게 분류하니
  #텍스트마이닝만 따로 하고 싶다면 참고하면 좋다.
  
  #하지만 이게 귀찮다고 대충 하면 본래 분석 결과와는 정반대의 결과가 나올 가능성이 있다.
  #따라서 가장 좋은 방법은 head()를 뽑아보지 말고 
  #전체 wordcount 를 확인해서 가장 밑에서부터 차근차근 gsup()해주는 것이다.
  #(정규식을 이용하면 이 귀찮음을 조금은 덜 수 있으니 꼭 활용하자.)