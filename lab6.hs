import Euterpea

start = g 5 qn :=: g 4 qn

int1 = line [d 4 en :=: g 4 en :=: bf 4 en]
int1l = line [d 4 qn :=: g 4 qn :=: bf 4 qn]

int2 = line [c 4 en :=: f 4 en :=: a 4 en]

int2sta = Modify (Phrase [Art (Staccato (5/10))]) $ keysig Bf Major $ int2

int1sta = Modify (Phrase [Art (Staccato (5/10))])  $ keysig Bf Major $ int1

line1treb = start :+: times 6 int1sta :+: int2sta :+: int2 :+: times 6 int2sta :+: int1 :+: times 7 int1sta :+: int2sta :+: int2 :+: times 6 int2sta :+: int1l

line1bass = line [a 1 en :=: g 2 en, a 1 wn :=: g 2 wn, a 1 hn :=: g 2 hn, a 1 en :=: g 2 en, d 2 en, f 2 en, a 1 en :=: g 2 en, a 1 wn :=: g 2 wn, a 1 hn :=: g 2 hn, a 1 en :=: g 2 en, d 2 en, f 2 en, a 1 en :=: g 2 en] 

l2triQN = d 4 qn :=: bf 3 qn :=: g 3 qn

l2triDQN = d 4 dqn :=: bf 3 dqn :=: g 3 dqn

l2triQNd = c 4 qn :=: a 3 qn :=: f 3 qn

l2triDQNd = c 4 dqn :=: a 3 dqn :=: f 3 dqn

line2treb = line[times 3 $ d 4 en, l2triDQN, d 4 en, l2triQNd, a 3 en, f 3 hn, f 3 en, l2triQN, l2triQN, c 4 en, bf 3 en, c 4 qn, l2triDQNd, l2triQN, d 4 dqn :=: bf 3 dqn :=: f 3 dqn]  

bassRiff1 = times 8 $ g 2 en
bassHRiff1 = times 4 $ d 3 en

bassHRiff2 = line[times 3 $ ef 3 en, bf 2 en :=: bf 1 en]  

bassHRiff3 = line[d 3 en, d 2 en, f 2 en :=: f 1 en, g 2 en :=: g 1 en]

bassRiff2 = line[f 2 en :=: g 1 en, times 7 $ f 2 en]

bassRiff3 = line[f 2 en :=: g 1 en, times 2 $ f 2 en, f 2 en :=: g 1 en, f 2 en, bf 2 en :=: bf 1 en, times 2 $ bf 2 en]

bassRiff4 = line[bf 2 en :=: bf 1 en, times 2 $ bf 2 en, c 3 en :=: c 2 en, times 2 $ c 3 en, d 3 en :=: d 2 en, d 3 en]

line2bass = line [bassRiff1, bassRiff2, transpose 2 bassRiff2, bassRiff3]

line3bass = line[times 4 $ ef 3 en, bassHRiff2, transpose 3 bassRiff1, bassRiff4, bassHRiff1, bassHRiff3]

line3treb = line[ef 4 en :=: bf 3 en :=: g 3 en, ef 4 en, ef 4 qn, ef 4 qn :=: bf 3 qn :=: g 3 qn, ef 4 en, d 4 qn :=: bf 3 qn :=: f 3 qn, c 4 qn, bf 3 hn, bf 3 en, d 4 qn :=: bf 3 qn :=: f 3 qn, d 4 en, c 4 qn :+: g 3 qn, bf 3 en, d 4 qn :=: a 3 qn :=: g 3 qn, d 4 dqn :=: a 3 dqn :=: g 3 dqn, d 4 dqn :=: a 3 dqn :=: fs 3 dqn, qnr]    

line4bass = line[bassRiff1, bassRiff2, transpose 2 bassRiff2, bassRiff3]

line5bass = line[transpose 10 bassRiff2, transpose 5 bassRiff2, bassRiff4, transpose 7 bassRiff1]

treb = line1treb :+: line2treb :+: line3treb

bass = line1bass :+: line2bass :+: line3bass :+: line4bass :+: line5bass

finished = play $ tempo (160/120) $ bass :=: treb 
