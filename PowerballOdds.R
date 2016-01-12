library('ggplot2');
library('data.table');
#Rules http://www.lottoreport.com/PB2015Rule.htm
odds = 1/292201338
jackpotPctRevenue = .5*.681 #50% goes into prize pool, 68.1% of which goes to grand prize
tax = .395+.0882 #Federal and new york state tax
yield30Yr=.0298 #treasury yield as of 1/12/16
powerplayFrac = .06 # Pct of revenue from powerplay option.estimate based on previous powerplay revenue.
lastJackpotSeq = rev(seq(400,2000, by=100)) #last jackpots to calculate
jackpotAddedSeq = c(seq(0,1000, by=50)) #added jackpots to calculate


getSplitProb = function(ticketSales) return(data.table(winners=0:10)[, splitProb:=dbinom(winners, size=ticketSales, prob = odds)])
getWinShare = function(ticketSales) {
  splitProb = getSplitProb(ticketSales)
  return(sum(splitProb[winners>0,splitProb/winners]/(1-splitProb[winners==0,splitProb])))
}

getEVDollar = function(lastJackpotSeq, jackpotAddedSeq, powerplayFrac, yield30Yr){
  annuity = 30*yield30Yr/((1+yield30Yr)^30-1) #present value multiplier for future val of 30 yr annuity
  evDollar = CJ(lastJackpot=lastJackpotSeq, jackpotAdded=jackpotAddedSeq)
  evDollar[,jackpot:=lastJackpot+jackpotAdded]
  evDollar[,newRevenue := jackpotAdded*annuity/(jackpotPctRevenue*(1-powerplayFrac))] # we back into the new revenue from this round by assuming the difference between the last jackpot and current is 32.5% of revenue
  evDollar[,ticketSales := newRevenue/2] # Ticket sale estimate. This is not completely accurate because players may pay more than $2 for the power play or less than $2 if they dont pay for hte powerball
  evDollar[,winnerProb:= 1-dbinom(0, size=1e6*ticketSales, prob = odds)] #probability of winner
  evDollar[,winShare:=sapply(1+1e6*ticketSales,getWinShare)]
  evDollar[,ev:= 1e6*(1-tax)*annuity*jackpot*odds*winShare/2]
  return(evDollar)
}

plotSplits = function(evDollar,lastJackpotSeq, jackpotAddedSeq){
  calcWinnerSplits = function(x) getSplitProb(1e6*x['ticketSales'])[,jackpotAdded:=x['jackpotAdded']][,prob:=splitProb/(x['winnerProb'])]
  winnerCount = data.table(do.call(rbind, apply(evDollar[lastJackpot==lastJackpotSeq[1]], 1, calcWinnerSplits))) #probabilities by number of winners given there is a winner

  print(ggplot(winnerCount)+
        ggtitle("Split Probability by Additional Jackpot (mm)")+
        geom_bar(aes(x=winners, y=splitProb, fill=winners), stat="identity")+
        facet_wrap(~jackpotAdded)+xlab("Number of Winners")+ylab("Probability")+
        scale_x_continuous(breaks=0:10))
#  print(ggplot(winnerCount[jackpotAdded>0&winners>0])+
#        ggtitle("Splits Probability Given a Winner by Additional Jackpot (mm)")+
#        geom_bar(aes(x=winners, y=prob, fill=winners), stat="identity")+
#        facet_wrap(~jackpotAdded)+xlab("Number of Winners")+
#        ylab("Probability")+
#        scale_x_continuous(breaks=1:10))
}
# faceted by last jackpot
#print(ggplot(evDollar[(jackpotAdded>lastJackpot*.2 | jackpotAdded>300) & jackpotAdded<lastJackpot & jackpotAdded<500])+geom_line(aes(x=jackpot, y=ev, color=lastJackpot))+facet_grid(~lastJackpot, scales="free_x")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)))

getEVPlot=function(evDollar){
  PreviousJackpot = as.factor(evDollar[,lastJackpot])
  return(ggplot(evDollar)+
        ggtitle("Powerball Expected Value Estimate")+
        geom_line(aes(x=jackpot, y=ev, color=as.factor(lastJackpot), group=lastJackpot))+
        #geom_point(data=evDollar[(jackpotAdded>jackpot*.1 | jackpotAdded>300) & jackpotAdded<lastJackpot & jackpotAdded<700], aes(x=jackpot,y=ev, group=1), color='red')+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Current Jackpot (mm)")+
        ylab("Expected Value")+labs(color = "Previous Jackpot") +
        scale_x_continuous(breaks=evDollar[,jackpot]))
}

