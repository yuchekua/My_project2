## Q1

a = c(15,3,20,3,9,5,14,3,82)
b = c(1,5,6)
c = c(12,18,5,2)

mx_a = matrix(c(15,3,20,3,9,5,14,3,82), nrow= 3,ncol= 3,byrow= TRUE)
mx_b = matrix(c(1,5,6), nrow= 3,ncol= 1,byrow= TRUE)
mx_c = matrix(c(12,18,5,2), nrow= 1,ncol= 4,byrow= TRUE)

mx_d = mx_a[,1,drop= FALSE]
mx_e = cbind(mx_b,d)
mx_f = rbind(mx_b,d)
mx_g = rbind(mx_a, mx_c[1,1:3])
mx_h = rbind(c(mx_a[1,3],mx_c[1,2],mx_b[2,1]))
mx_h

area_tri = function(a,b,c) {
  if ((a+b) <= c | (b+c) <= a | (c+a) <= b){
    stop("input a wrong triangle")
  }
  s = (a+b+c)/2
  return(sqrt(s*(s-a)*(s-b)*(s-c)))
}
area_tri(3,4,5)
area_tri(10,13,16)



series = function(n){
  sum_x = 0
  fatorial = 1
  for (i in 0:n) {
    if(i == 0){
      sum_x = sum_x + 1/fatorial
      next
    } else{
      fatorial = fatorial*i
    }
    sum_x = sum_x + 1/fatorial
  }
  return(sum_x)
}
series(10)


find_armstrong_btw_a_b = function(a,b){
  if (a>b) {
    c = a
    a = b
    b = c
  }
  ls_armstrong = c()
  for (i in a:b){
    sum = 0 
    x = i
    while(x>0){
      digit = x %% 10
      sum = sum + (digit^3)
      x = as.integer(x/10)
    }
    if (sum == i){
    ls_armstrong = append(ls_armstrong,i)
    }
  }
  return(ls_armstrong)
}

find_armstrong_btw_a_b(1,1000)

mx_sum_upper_tri = function(m){
  return(sum(m[1,2]+m[1,3]+m[2,3]))
}
mx_sum_upper_tri(mx_a)

dayofweek = function(m,d){
  month = matrix(c(31,29,31,30,31,30,31,31,30,31,30,31), nrow= 1,ncol= 12,byrow= TRUE)
  day = matrix(c('Wed','Thu','Fri','Sat','Sun','Mon','Tue' ), nrow= 1,ncol= 7,byrow= TRUE)
  tot_days = d
  for (i in 1:m){
    tot_days = tot_days + month[1,i]
  }
  tot_days = tot_days - month[1,m]
  if (tot_days%%7==0) {
    return(day[1,7])
  }else{
    return(day[1,tot_days%%7])
  }
}

dayofweek(1,14)


pell = function(n){
  if (n<=2){
    return(n)
  }else{
    return(2*pell(n-1)+pell(n-2))
  }
}

is_pell = function(n){
  x = 1
  i = (-1)
  while (i < 0) {
    i = pell(x)-n
    x = x+1
    if (i == 0){
      break
    }
  }
  if (i == 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

is_pell(60)
is_pell(70)
is_pell(80)


z = c(3,8,5,4,3,4,3,5)


count_occur = function(m,x){
 c = 0
  for (i in m){
   if(i == x){
     c=c+1
   }
  }
 return(c)
}

odd_occur = function(m){
  ls = c()
  for (i in m){
    if(count_occur(m,i)%%2 ==1 & !(i %in% ls)){
      ls  = append(ls,i)
    }
  } 
  return(ls)
}

odd_occur(z)
