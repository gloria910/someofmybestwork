df<-as.matrix(pddata)

extra_col <- matrix(0, ncol=2, nrow = 2000)
#colnames(extra_col)<-c("AA","AB")
df_new<-cbind(df, extra_col)

ll_rl<-function(h){
  id1<-rbind(df_new[1:100,])  
  id2<-rbind(df_new[101:200,])
  id3<-rbind(df_new[201:300,])
  id4<-rbind(df_new[301:400,])
  id5<-rbind(df_new[401:500,])
  id6<-rbind(df_new[501:600,])
  id7<-rbind(df_new[601:700,])
  id8<-rbind(df_new[701:800,])
  id9<-rbind(df_new[801:900,])
  id10<-rbind(df_new[901:1000,])
  id11<-rbind(df_new[1001:1100,])
  id12<-rbind(df_new[1101:1200,])
  id13<-rbind(df_new[1201:1300,])
  id14<-rbind(df_new[1301:1400,])
  id15<-rbind(df_new[1401:1500,])
  id16<-rbind(df_new[1501:1600,])
  id17<-rbind(df_new[1601:1700,])
  id18<-rbind(df_new[1701:1800,])
  id19<-rbind(df_new[1801:1900,])
  id20<-rbind(df_new[1901:2000,])
  
  id<-list(id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11,id12,id13,id14,id15,id16,id17,id18,id19,id20)
  
  for (i in 1:20){
    temp<-id[[i]]
    for (k in 2:100){
      last_row = k - 1
      if (temp[last_row,1] == 0) {
        temp[k,9] = temp[last_row,9] + temp[last_row,4]
        temp[k,10] = temp[last_row,10]
      }
      else {
        temp[k,9] = temp[last_row,9]
        temp[k,10] = temp[last_row,10] + temp[last_row,4]
      }
    }
    id[[i]]<-temp
  }
  
  ll = 0
  
  for (i in 1:20){
    temp<-id[[i]]
    for (k in 2:100){
      if (temp[k,1] == 0) {
        ll=h*temp[k,9]-log(exp(h * temp[k,9] ) +exp(h * temp[k,10])) + ll
      }
      else {
        ll=h*temp[k,10]-log(exp(h * temp[k,9] ) +exp(h * temp[k,10])) + ll
      }
    }
  }
  return(-ll)
}

llresult<-nlminb(start = 0, objective = ll_rl, lower = 0, upper = Inf, control = list(step.min = 0.0001, step.max = 0.0001))

# par = (d, h)
ll_rl2<-function(par){
  id1<-rbind(df_new[1:100,])  
  id2<-rbind(df_new[101:200,])
  id3<-rbind(df_new[201:300,])
  id4<-rbind(df_new[301:400,])
  id5<-rbind(df_new[401:500,])
  id6<-rbind(df_new[501:600,])
  id7<-rbind(df_new[601:700,])
  id8<-rbind(df_new[701:800,])
  id9<-rbind(df_new[801:900,])
  id10<-rbind(df_new[901:1000,])
  id11<-rbind(df_new[1001:1100,])
  id12<-rbind(df_new[1101:1200,])
  id13<-rbind(df_new[1201:1300,])
  id14<-rbind(df_new[1301:1400,])
  id15<-rbind(df_new[1401:1500,])
  id16<-rbind(df_new[1501:1600,])
  id17<-rbind(df_new[1601:1700,])
  id18<-rbind(df_new[1701:1800,])
  id19<-rbind(df_new[1801:1900,])
  id20<-rbind(df_new[1901:2000,])
  
  id<-list(id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11,id12,id13,id14,id15,id16,id17,id18,id19,id20)
  
  for (i in 1:20){
    temp<-id[[i]]
    for (k in 2:100){
      last_row = k - 1
      if (temp[last_row,1] == 0) {
        temp[k,9] = par[1]* temp[last_row,9] + temp[last_row,4]
        temp[k,10] = par[1]* temp[last_row,10]
      }
      else {
        temp[k,9] = par[1] * temp[last_row,9]
        temp[k,10] = par[1] * temp[last_row,10] + temp[last_row,4]
      }
    }
    id[[i]]<-temp
  }
  ll=0
  for (i in 1:20){
    temp<-id[[i]]
    for (k in 2:100){
      if (temp[k,1] == 0) {
        ll=par[2]*temp[k,9]-log(exp(par[2] * temp[k,9] ) +exp(par[2] * temp[k,10])) + ll
      }
      else {
        ll=par[2]*temp[k,10]-log(exp(par[2] * temp[k,9] ) +exp(par[2] * temp[k,10])) + ll
      }
    }
  }
  return(-ll)
}

llresult2<-nlminb(c(0,0), ll_rl2, lower = c(0,0), upper = c(1,Inf), control = list(step.min = 0.0001, step.max = 0.0001))