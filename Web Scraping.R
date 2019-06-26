#Krista Johnson


#Start with the url
library(XML)
library(RCurl)
beginning_url = "http://stackoverflow.com"
url = getURLContent("http://stackoverflow.com/questions/tagged/r?sort=newest&pagesize=50", binary = TRUE)
middle = rawToChar(url)
html = htmlParse(middle, encoding = "UTF8", asText = TRUE)
saveXML(html, "stack_overflow.html")

get_title = function(page){
  xpath = "//a[@class = 'question-hyperlink']/text()"
  title = getNodeSet(page, xpath)
  sapply(title, saveXML)
}

get_votes = function(page){
  post_info = getNodeSet(page, "//div[@class = 'statscontainer']")
  #look at a specific section in each post
  vote = NULL
  for(i in 1:length(post_info)){
    vote[i] = xpathApply(post_info[[i]], ".//div[@class = 'votes']", xmlChildren)
  }
  #Use the second item of the list
  vote.one = sapply(vote, unlist)
  only_vote = vote.one[row.names(vote.one) == "span"]
  vote_num = as.numeric(sapply(only_vote, xmlValue))
}

get_views = function(page){
  post_info = getNodeSet(page, "//div[@class = 'statscontainer']")
  #look for views
  view_num = NULL
  for(i in 1:length(post_info)){
    view_num[i] = xpathSApply(post_info[[i]], ".//div[contains(@class, 'views') and contains(@title, 'views')]", xmlChildren)
  }
  view_num.one = sapply(view_num, saveXML)
  new_view_num = gsub("(&#13;\n)", "", view_num.one)
  only = as.numeric(gsub("(view|views)", "", new_view_num))
}

get_answers = function(page){
  post_info = getNodeSet(page, "//div[@class = 'statscontainer']")
  #look at a specific section in each post
  ans = NULL
  for(i in 1:length(post_info)){
    ans[i] = xpathApply(post_info[[i]], ".//div[@class = 'status answered' or @class = 'status unanswered' or
                   @class = 'status answered-accepted']")
  }
  as.numeric(sapply(ans, function(x) xmlValue((xmlChildren(x)$strong))))
}

get_user_info = function(page){
  user = xpathApply(html, "//div[starts-with(@class, 'user-info')]")
  
  user.name = sapply(user, function(node){
    poster = xpathApply(node, "./div[@class = 'user-details']/a")
    #Find anonymous
    if(length(poster) < 1) return(NA)
    else return(sapply(poster, xmlValue))
  })
  
  user.reputation.one = sapply(user, function(node){
    poster = xpathApply(node, "./div[@class = 'user-details']/a")
    #Find out if it is not an anonymous poster
    if(length(poster) < 1) return(NA)
    else{
      parents = rapply(poster, xmlParent)
      user.readable = sapply(parents, function(nodes) xpathApply(nodes, ".//span[contains(@class, 'reputation-score')]"))
      scores = rapply(user.readable, function(x) {
        #if the score is larger than 10000
        if(length(grep("*k", xmlValue(x))) == 0){
          xmlValue(unlist(x))
        }
        else{
          rep_name = xmlGetAttr(x, "title")
          as.numeric(gsub("reputation score ", "", rep_name))
        }
      }) 
    }
  })
  user.score = as.numeric(gsub(",", "", user.reputation))
  data.frame(user_name = user.name, reputation = user.score)
}

get_time = function(page){
  when = xpathApply(page, "//div[@class = 'user-action-time']/span")
  #The time is the attribute, so you need to get each specific attribute
  when.one = sapply(when, function(x) xmlGetAttr(x, "title"))
  data.frame(time = when.one)
}

get_tags = function(page){
  tag_code = xpathApply(page, "//div[@class = 'summary']//div[contains(@class, 'tags')]")
  #the full list of tags are attributes
  tags = sapply(tag_code, function(x) xmlGetAttr(x, "class"))
  #clean the data to get only the words
  minus_tags = gsub("(tags )", "", tags)
  only_tags = gsub("(t-)", "", minus_tags)
  all_tags = sapply(only_tags, function(x) gsub(" ", ";", x))
  #change the strange letter in the tags (wasn't easily converted using encoding = "latin1")
  all_tags_fixed = gsub("û", ".", all_tags)
  data.frame(tags = all_tags_fixed)
}

get_url = function(page, main_url){
  little_url = unique(unlist(xpathApply(page, "//div[@class = 'summary']/h3/a")))
  almost_all_url = sapply(little_url, function(x) xmlGetAttr(x, "href"))
  #The getRelativeURL won't work if there is a "/" in the front of the url you want appended so remove it
  full_url = as.character(sapply(substring(almost_all_url, 2), function(u) getRelativeURL(u, main_url)))
  #full_urls = sapply(full_url, saveXML)
  data.frame(urls = full_url, row.names = NULL)
}

get_ids = function(page){
  id_line = unique(unlist(xpathApply(page, "//div[contains(@class, 'question-summary')]")))
  ids = sapply(id_line, function(x) xmlGetAttr(x, "id"))
  only_id = as.numeric(gsub("(question-summary-)", "", ids))
  data.frame(id = only_id)
}

get_next_page = function(page, main_url){
  next_url = unique(unlist(xpathApply(page, "//a[@rel = 'next']/@href")))
  #sapply(substring(next_url, 2), function(u) getRelativeURL(u, main_url))
  bottom_page = getRelativeURL(substring(next_url, 2), main_url)
  url = getURLContent(bottom_page, binary = TRUE)
  middle = rawToChar(url)
  html = htmlParse(middle, encoding = "UTF8", asText = TRUE)
}

ll_together = function(page, main_url){
  id =  get_ids(page)
  date = get_time(page)
  tags = get_tags(page)
  title = get_title(page)
  url = get_url(page, main_url)
  views = get_views(page)
  votes = get_votes(page)
  answers = get_answers(page)
  user = get_user_info(page)
  data = cbind(id, date)
  data = cbind(data, tags)
  data = cbind(data, title)
  data = cbind(data, url)
  data = cbind(data, views)
  data = cbind(data, votes)
  data = cbind(data, answers)
  data = cbind(data, user)
}

stack_overflow_data = function(page, main_url){
  ans = NULL
  while(TRUE) {
    data = all_together(page, main_url)
    ans = rbind(ans, data)
    page = get_next_page(page, main_url)
    data = NULL
    Sys.sleep(5)
    if(nrow(ans) >= 11000)
      break
  }
  return(ans)
}
scrape_data = stack_overflow_data(html, beginning_url)
