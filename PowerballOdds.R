library('ggplot2');
library('data.table');
#Rules http://www.lottoreport.com/PB2015Rule.htm
odds = 1/292201338.0
jackpotPctRevenue = .34 #now 34% # only 32.5 pct of proceeds are contributed to the advertized jackpot 
tax = .395+.0882
annuity = 3/5
powerplayFrac = .06 # Pct of revenue from powerplay option.estimate based on previous powerplay revenue.
lastJackpotSeq = rev(seq(400,2000, by=100))
jackpotAddedSeq = c(seq(0,1000, by=50))
newJackpot = CJ(lastJackpot=lastJackpotSeq, jackpotAdded=jackpotAddedSeq)
newJackpot[,jackpot:=lastJackpot+jackpotAdded]
newJackpot[,newRevenue := jackpotAdded*annuity/(jackpotPctRevenue*(1-powerplayFrac))] # we back into the new revenue from this round by assuming the difference between the last jackpot and current is 32.5% of revenue
newJackpot[,ticketSales := newRevenue/2] # Ticket sale estimate. This is not completely accurate because players may pay more than $2 for the power play or less than $2 if they dont pay for hte powerball
newJackpot[,noWinnerProb:= dbinom(0, size=1+1e6*ticketSales, prob = odds)] #probability of no winner

calcWinnerSplits = function(x) data.table(winners=0:10)[,jackpot:=x['jackpot']][,jackpotAdded:=x['jackpotAdded']][,lastJackpot:=x['lastJackpot']][, totalProb:=dbinom(winners, size=1+1e6*x['ticketSales'], prob = odds)][,prob:=totalProb/(1-x['noWinnerProb'])]
winnerCount = data.table(do.call(rbind, apply(newJackpot, 1, calcWinnerSplits))) #probabilities by number of winners given there is a winner
winnerCount[, ev:= 1e6*(1-tax)*annuity*jackpot*odds*prob/winners] #expected value per ticket by number of winners

print(ggplot(winnerCount[lastJackpot==lastJackpotSeq[1]])+
        ggtitle("Expected Splits by Additional Jackpot (mm)")+
        geom_bar(aes(x=winners, y=totalProb, fill=winners), stat="identity")+
        facet_wrap(~jackpotAdded)+xlab("Number of Winners")+ylab("Probability")+
        scale_x_continuous(breaks=0:10))
print(ggplot(winnerCount[winners>0 & lastJackpot==lastJackpotSeq[1]])+
        ggtitle("Expected Splits Given a Winner by Additional Jackpot (mm)")+
        geom_bar(aes(x=winners, y=prob, fill=winners), stat="identity")+
        facet_wrap(~jackpotAdded)+xlab("Number of Winners")+
        ylab("Probability")+
        scale_x_continuous(breaks=1:10))

evDollar = winnerCount[winners>0,.(ev=sum(ev)/2), keyby=.(lastJackpot, jackpotAdded, jackpot)] #total expected value per dollar
evDollar [,pred:=exp(jackpot*.005)]
print(ggplot(evDollar[(jackpotAdded>lastJackpot*.2 | jackpotAdded>300) & jackpotAdded<lastJackpot & jackpotAdded<500])+geom_line(aes(x=jackpot, y=ev, color=lastJackpot))+facet_grid(~lastJackpot, scales="free_x")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)))

PreviousJackpot = factor(evDollar$lastJackpot, levels = lastJackpotSeq)
print(ggplot(evDollar)+
        ggtitle("Powerball Expected Value Estimate")+
        geom_line(aes(x=jackpot, y=ev, color=PreviousJackpot, group=lastJackpot))+
        geom_point(data=evDollar[(jackpotAdded>lastJackpot*.2 | jackpotAdded>300) & jackpotAdded<lastJackpot & jackpotAdded<700], aes(x=jackpot,y=ev, group=1), color='red')+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Current Jackpot (mm)")+
        ylab("Expected Value")+
        scale_x_continuous(breaks=evDollar[,jackpot]))

