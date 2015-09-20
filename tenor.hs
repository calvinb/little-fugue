module Tenor where

import Euterpea

tenor = foldl1 (:+:) [tenor1, tenor12, tenor14, tenor18, tenor21, tenor24, tenor29, tenor32, tenor37,
                        tenor41, tenor44, tenor51, tenor55, tenor62, tenor67]

fugue :: Music Pitch
fugue = tempo (4/7) (instrument ChurchOrgan (tenor1 :+: tenor12 :+: tenor14 :+: tenor18 :+: tenor21 :+: tenor24 :+:
                                                tenor29 :+: tenor32 :+: tenor37 :+: tenor41 :+: tenor44))
fuguetest :: Music Pitch
fuguetest = tempo (4/7) (instrument ChurchOrgan (tenor12))

tenor1 :: Music Pitch
tenor1 = rest (24/2)

tenor12 :: Music Pitch
tenor12 = line [g 4 qn,d 5 qn,bf 4 dqn] :+: (toEN [A,G,Bf,A,G,Fs,A] 4) :+: line [d 4 qn]

tenor14 :: Music Pitch
tenor14 = (toEN [G,D,A,D,Bf] 4) :+: (toSN [A,G] 4) :+: (toEN [A,D,G] 4) :+: (toSN [D,G] 4) :+: line [a 4 en] :+: (toSN [D,A] 4) :+: 
            line [bf 4 en] :+: (toSN [A,G,A,D] 4) :+: (toSN [D,C] 5) :+: (toSN [Bf,A,G,Bf,A,G,Fs,A] 4) :+:
            (toSN [G,D,G,A,Bf] 4) :+: (toSN [C,D,E,F,E,D,F,E,D,Cs,E] 5) :+: line [d 5 en,a 4 en,d 5 en,e 5 en]

tenor18 :: Music Pitch
tenor18 = (toSN [F,G,F,G] 5) :+: line [trill 2 (1/32) (g 5 den), f 5 sfn, g 5 tn] :+: (toSN [A,G,A,Bf,A,G,F,E,F,A,G,A] 4) :+: line [cs 4 sn, a 4 sn, g 4 sn, a 4 sn, d 4 sn, a 4 sn, g 4 sn, a 4 sn, cs 4 sn, a 4 sn, g 4 sn, a 4 sn] :+: (toSN [F,D,Cs,D,G,D,Cs,D,A,D,Cs,D,G,D,Cs,D] 4)

tenor21 :: Music Pitch
tenor21 = line [f 4 en, e 4 sn, d 4 sn, trill 1 (1/32) (cs 4 den), d 4 sn] :+: (toSN [D,C,D,E,D,C] 4) :+: (toSN [Bf,A] 3) :+: (toSN [G,F,G,A,G,F,Ef,D] 4) :+: line [c 5 sn, bf 4 sn, c 5 sn, d 5 sn, c 5 sn] :+: (toSN [Bf,A,G,F,Ef,F,G,F,Ef,D,C] 4) :+: line [bf 3 sn, bf 4 sn, c 5 sn, d 5 sn, ef 5 qn]

tenor24 :: Music Pitch
tenor24 = rest (1/16) :+: line[a 5 sn, bf 5 sn, c 6 sn, d 6 qn] :+:
            line [g 5 qn, d 6 qn, bf 5 dqn, a 5 en] :+:
            (toSN [G,Bf,A,G,Fs,G,E,Fs,G,D,E,Fs,G,D,G,A] 5) :+:
            line [bf 5 sn, c 6 sn, bf 5 sn, c 6 sn, trill 2 (1/32) (c 6 den), bf 5 tn, c 6 tn] :+: (toSN [D,C,D,Ef,D,C] 6) :+: line [bf 5 sn, a 5 sn] :+:
            line [bf 5 sn, d 6 sn, c 6 sn, d 6 sn, fs 5 sn, d 6 sn, c 6 sn, d 6 sn, g 5 sn, d 6 sn, c 6 sn, d 6 sn, fs 5 sn, d 6 sn, c 6 sn, d 6 sn]

tenor29 :: Music Pitch
tenor29 = (toSN [Bf,G,Fs,G] 5) :+: line [c 6 sn, g 5 sn, fs 5 sn, g 5 sn, d 6 sn, g 5 sn, fs 5 sn, g 5 sn, c 6 sn, g 5 sn, fs 5 sn, g 5 sn] :+:
            line [g 5 en, bf 5 en, c 6 sn, bf 5 sn, a 5 sn, c 6 sn] :+: line [bf 5 (qn+sn), d 6 sn, c 6 sn, bf 5 sn] :+:
            line [a 5 (qn+sn), c 6 sn, bf 5 sn, a 5 sn] :+: line [g 5 (qn+sn), bf 5 sn, a 5 sn, g 5 sn]

tenor32 :: Music Pitch
tenor32 = line [f 5 dqn, e 5 en, f 5 en] :+: (toEN [Ef,D,C] 5) :+: line [bf 4 qn, rest (dqn), c 6 en, bf 5 en, a 5 en] :+:
            line [g 5 hn, f 5 qn, f 5 qn] :+:
            line [bf 5 en, f 5 en, c 6 en, f 5 en, d 6 en, c 6 sn, bf 5 sn, c 6 en, f 5 en] :+:
            line [bf 5 en, f 5 sn, bf 5 sn, c 6 en, f 5 sn, c 6 sn, d 6 en, c 6 sn, bf 5 sn, c 6 sn, f 5 sn, f 6 sn, ef 6 sn]

tenor37 :: Music Pitch
tenor37 = line [d 6 sn, c 6 sn, bf 5 sn, d 6 sn, c 6 sn, bf 5 sn, a 5 sn, c 6 sn, bf 5 en, g 6 en, enr, g 5 en] :+:
            line [a 5 en, f 6 en, enr, f 5 en, g 5 en, f 6 en, c 6 en, e 6 en] :+:
            (toSN [F,Ef,F,G,  F,Ef,D,F,   Ef,D,Ef,F,  Ef,D,C,Ef] 6) :+:
            (toSN [D,C,D,Ef,  D,C] 6) :+: line [bf 5 sn, d 6 sn,  c 6 sn, bf 5 sn, c 6 sn, d 6 sn,    c 6 sn, bf 5 sn, a 5 sn, c 6 sn]

tenor41 :: Music Pitch
tenor41 = line [bf 5 sn, c 6 sn, d 6 sn, bf 5 sn] :+: (toSN [C,D,Ef,C,  D,C] 6) :+: line [bf 5 sn] :+: (toSN [C,D,Ef,D,Ef] 6) :+:
            (toSN [F,G,F,G] 6) :+: line [trill 2 (1/32) (g 6 den), f 6 tn, g 6 tn] :+: (toSN [A,G,A,Bf,A,G,F,Ef] 6) :+:
            (toSN [D,F,Ef,F] 6) :+: line [a 5 sn] :+: (toSN [F,Ef,F] 6) :+: line [bf 5 sn] :+: (toSN [F,Ef,F] 6) :+: line [a 5 sn] :+: (toSN [F,Ef,F] 6)

tenor44 :: Music Pitch
tenor44 = (toSN [D,Bf,A,Bf,Ef,Bf,A,Bf,F,Bf,A,Bf,Ef,Bf,A,Bf] 6) :+:
            line [bf 6 sn, d 7 sn, c 7 sn, bf 6 sn, a 6 sn, g 6 sn, f 6 sn, ef 6 sn, d 6 (hn+en), g 5 en, c 6 en, bf 5 en, a 5 (hn+en)] :+:
            line [f 5 en, bf 5 en, a 5 en, g 5 (hn+en)] :+:
            line [ef 5 en, af 5 en, g 5 en, f 5 qn, g 5 (qn+sn)] :+:
            (toSN [G,A,B] 5) :+: (toSN [C,D,Ef,C] 6) :+: line [af 5 hn] :+:
            line [g 5 en, a 5 en, b 5 qn, c 6 en, dqnr]

tenor51 :: Music Pitch
tenor51 = line [rest 4]

tenor55 :: Music Pitch
tenor55 = line [rest en, g 5 en, ef 5 en, c 5 en, f 5 qn, qnr, enr, f 5 en, d 5 en, bf 4 en, ef 5 qn, qnr, enr] :+:
            line [ef 5 en, c 5 en, a 4 en, d 5 qn, qnr] :+:
            line [g 4 sn, bf 4 sn, a 4 sn, g 4 sn, d 5 sn, a 4 sn, d 4 sn, c 5 sn, bf 4 en, d 5 en, fs 4 en, d 5 en] :+:
            line [g 5 qn, fs 5 qn, g 5 qn, a 5 qn] :+:
            line [bf 5 qn, b 5 qn, c 6 qn, cs 6 qn] :+: line [d 6 qn, e 6 en, fs 6 en, g 6 qn, a 6 en, bf 6 en]

tenor62 :: Music Pitch
tenor62 = line [c 7 sn, g 6 sn, f 6 sn, ef 6 sn, c 7 sn] :+: (toSN [A,F,A] 6) :+: line [bf 5 sn] :+: (toSN [F,Ef,D,Bf,G,Ef,G] 6) :+:
            line [a 5 sn, ef 6 sn, d 6 sn, c 6 sn,    a 6 sn, fs 6 sn, d 6 sn, fs 6 sn,   g 5 sn, bf 5 sn, d 6 sn, g 6 sn,    fs 6 sn, g 6 sn, e 6 sn, fs 6 sn] :+:
            line [g 6 dqn, fs 6 en, g 6 qn, a 6 en, d 6 en, d 6 qn, enr, fs 6 en, g 6 en, enr, c 7 en, enr] :+:
            line [bf 6 en, enr, c 7 en, enr, bf 6 en, enr, a 6 en, enr]

tenor67 :: Music Pitch
tenor67 = (phrase [Tmp (Ritardando $ 1/2)] slowing) :+: b 6 wn where
  slowing = line [g 6 en, bf 6 en, c 7 en, a 6 en]

toQN :: [PitchClass] -> Octave -> Music Pitch
toQN ps o = let f p = Prim (Note qn (p,o))
            in  line (map f ps)

toEN :: [PitchClass] -> Octave -> Music Pitch
toEN ps o = let f p = Prim (Note en (p,o))
            in  line (map f ps)

toSN :: [PitchClass] -> Octave -> Music Pitch
toSN ps o = let f p = Prim (Note sn (p,o))
            in  line (map f ps)