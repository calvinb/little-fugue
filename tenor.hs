module Tenor where

import Euterpea

tenor = foldl1 (:+:) [tenor1, tenor12, tenor14, tenor18, tenor21, tenor24, tenor29, tenor32, tenor37,
                        tenor41, tenor44, tenor51, tenor55, tenor62, tenor67]

fugue :: Music Pitch
fugue = tempo (4/7) (instrument ChurchOrgan (tenor1 :+: tenor12 :+: tenor14 :+: tenor18 :+: tenor21 :+: tenor24 :+:
                                                tenor29 :+: tenor32 :+: tenor37 :+: tenor41 :+: tenor44 :+: tenor51 :+:
                                                tenor55 :+: tenor62 :+: tenor67))
fuguetest :: Music Pitch
fuguetest = tempo (1) (tenor55 :+: tenor62 :+: tenor67)

tenor1 :: Music Pitch
tenor1 = timesM 6 wnr :+: hnr

tenor12 :: Music Pitch
            -- m12 beat 3
tenor12 = line [g 4 qn,d 5 qn,bf 4 dqn] :+: (toEN [A,G,Bf,A,G,Fs,A] 4) :+: line [d 4 qn]

tenor14 :: Music Pitch
            -- m14 beat 3
tenor14 = (toEN [G,D,A,D] 4) :+:
            -- m15
            line [bf 4 en] :+: (toSN [A,G] 4) :+: (toEN [A,D,G] 4) :+: (toSN [D,G] 4) :+: line [a 4 en] :+: (toSN [D,A] 4) :+:
            -- m16
            line [bf 4 en] :+: (toSN [A,G,A,D] 4) :+: (toSN [D,C] 5) :+: (toSN [Bf,A,G,Bf,A,G,Fs,A] 4) :+:
            -- m17
            (toSN [G,D,G,A,   Bf] 4) :+: (toSN [C,D,E,      F,E,D,F,    E,D,Cs,E] 5) :+:
            -- m18
            line [d 5 en,a 4 en,d 5 en,e 5 en]

tenor18 :: Music Pitch
            -- m18 beat 3 aka clef change
tenor18 = (toSN [F,G,F,G] 5) :+: line [trill 2 trillDur (g 5 den), f 5 sfn, g 5 tn] :+: 
            -- m19
            (toSN [A,G,A,Bf,  A,G,F,E,    F,A,G,A] 5) :+: line [cs 5 sn, a 5 sn, g 5 sn, a 5 sn] :+:
            -- m20
            line [d 5 sn, a 5 sn, g 5 sn, a 5 sn,     cs 5 sn, a 5 sn, g 5 sn, a 5 sn] :+: (toSN [F,D,Cs,D, G,D,Cs,D] 5) :+:
            -- m21
            (toSN [A,D,Cs,D,  G,D,Cs,D] 5)

tenor21 :: Music Pitch
            -- m21 beat 3
tenor21 = line [f 5 en, e 5 sn, d 5 sn, trill 1 trillDur (cs 5 den), d 5 sn] :+:
            -- m22
            (toSN [D,C,D,E,   D,C] 5) :+: (toSN [Bf,A] 4) :+:
            -- m22 beat 3, aka the clef change
            (toSN [G,F,G,A, G,F,Ef,D] 4) :+:
            -- m23
            line [c 5 sn, bf 4 sn, c 5 sn, d 5 sn,    c 5 sn] :+: (toSN [Bf,A,G,    F,Ef,F,G,   F,Ef,D,C] 4) :+:
            -- m24
            line [bf 3 sn, bf 4 sn, c 5 sn, d 5 sn, ef 5 qn]

tenor24 :: Music Pitch
            -- m24 beat 3
tenor24 = rest (1/16) :+: line[a 4 sn, bf 4 sn, c 5 sn, d 5 qn] :+:
            -- m25
            line [g 4 qn, d 5 qn, bf 4 dqn, a 4 en] :+:
            -- m26
            (toSN [G,Bf,A,G,  Fs,G,E,Fs,  G,D,E,Fs,   G,D,G,A] 4) :+:
            -- m27
            line [bf 4 sn, c 5 sn, bf 4 sn, c 5 sn, trill 2 trillDur (c 5 den), bf 4 tn, c 5 tn] :+:
            -- m27 beat 3
            (toSN [D,C,D,Ef,  D,C] 5) :+: line [bf 4 sn, a 4 sn] :+:
            -- m28
            (bf 4 sn) :+: (toSN [D,C,D] 5) :+: (fs 4 sn) :+: (toSN [D,C,D] 5) :+:
            -- m28 beat 3
            (g 4 sn) :+: (toSN [D,C,D] 5) :+: (fs 4 sn) :+: (toSN [D,C,D] 5)

tenor29 :: Music Pitch
            -- m29
tenor29 = (toSN [Bf,G,Fs,G] 4) :+: (c 5 sn) :+: (toSN [G,Fs,G] 4) :+:
            -- m29 beat 3
            (d 5 sn) :+: (toSN [G,Fs,G] 4) :+: (c 5 sn) :+: (toSN [G,Fs,G] 4) :+:
            -- m30
            line [g 4 en, bf 4 en, c 5 sn, bf 4 sn, a 4 sn, c 5 sn] :+: line [bf 4 (qn+sn), d 5 sn, c 5 sn, bf 4 sn] :+:
            -- m31
            line [a 4 (qn+sn), c 5 sn, bf 4 sn, a 4 sn] :+: line [g 4 (qn+sn), bf 4 sn, a 4 sn, g 4 sn]

tenor32 :: Music Pitch
            -- m32
tenor32 = line [f 4 dqn, e 4 en, f 4 en] :+: (toEN [Ef,D,C] 4) :+:
            -- m32
            line [bf 3 qn, rest (dqn), c 5 en, bf 4 en, a 4 en] :+:
            -- m34
            line [g 4 hn, f 4 qn, f 4 qn] :+:
            -- m35
            line [bf 4 en, f 4 en, c 5 en, f 4 en, d 5 en, c 5 sn, bf 4 sn, c 5 en, f 4 en] :+:
            -- m36
            line [bf 4 en, f 4 sn, bf 4 sn, c 5 en, f 4 sn, c 5 sn, d 5 en, c 5 sn, bf 4 sn, c 5 sn, f 4 sn, f 5 sn, ef 5 sn]

tenor37 :: Music Pitch
            -- m37
tenor37 = line [d 5 sn, c 5 sn, bf 4 sn, d 5 sn,      c 5 sn, bf 4 sn, a 4 sn, c 5 sn,    bf 4 en, g 5 en, enr, g 4 en] :+:
            -- m38
            line [a 4 en, f 5 en, enr, f 4 en, g 4 en, f 5 en, c 5 en, e 5 en] :+:
            -- m39
            (toSN [F,Ef,F,G,  F,Ef,D,F,   Ef,D,Ef,F,  Ef,D,C,Ef] 5) :+:
            -- m40
            (toSN [D,C,D,Ef,  D,C] 5) :+: line [bf 4 sn, d 5 sn] :+:
            -- m40 beat 3
            line [c 5 sn, bf 4 sn, c 5 sn, d 5 sn,    c 5 sn, bf 4 sn, a 4 sn, c 5 sn]

tenor41 :: Music Pitch
            -- m41
tenor41 = line [bf 4 sn, c 5 sn, d 5 sn, bf 4 sn] :+: (toSN [C,D,Ef,C,  D,C] 5) :+: line [bf 4 sn] :+: (toSN [C,  D,Ef,D,Ef] 5) :+:
            -- m42
            (toSN [F,G,F,G] 5) :+: line [trill 2 trillDur (g 5 den), f 5 tn, g 5 tn] :+:
            -- m42 beat 3
            (toSN [A,G,A,Bf,  A,G,F,Ef] 5) :+:
            -- m43
            (toSN [D,F,Ef,F] 5) :+: line [a 4 sn] :+: (toSN [F,Ef,F] 5) :+:
            -- m43 beat 3
            line [bf 4 sn] :+: (toSN [F,Ef,F] 5) :+: line [a 4 sn] :+: (toSN [F,Ef,F] 5)

tenor44 :: Music Pitch
            -- m44
tenor44 = (toSN [D,Bf,A,Bf,   Ef,Bf,A,Bf, F,Bf,A,Bf,  Ef,Bf,A,Bf] 5) :+:
            -- m45
            line [bf 5 sn, d 6 sn, c 6 sn, bf 5 sn, a 5 sn, g 5 sn, f 5 sn, ef 5 sn, d 5 (hn+en)] :+:
            -- m46 second half beat
            line [g 4 en, c 5 en, bf 4 en, a 4 (hn+en)] :+:
            -- m47 second half beat
            line [f 4 en, bf 4 en, a 4 en, g 4 (hn+en)] :+:
            -- m48 second half beat
            line [ef 4 en, af 4 en, g 4 en, f 4 qn, g 4 (qn+sn)] :+:
            -- m49 second quarter beat
            (toSN [G,A,B] 4) :+: (toSN [C,D,Ef,C] 5) :+: line [af 4 hn] :+:
            -- m50
            line [g 4 en, a 4 en, b 4 qn, c 5 en, dqnr]

tenor51 :: Music Pitch
            -- m51
tenor51 = line [rest 4]

tenor55 :: Music Pitch
            -- m55
tenor55 = line [rest en, g 4 en, ef 4 en, c 4 en, f 4 qn, qnr] :+:
            -- m56
            line [enr, f 4 en, d 4 en, bf 3 en, ef 4 qn, qnr] :+:
            -- m57
            line [enr, ef 4 en, c 4 en, a 3 en, d 4 qn, qnr] :+:
            -- m58
            (toSN [G,Bf,A,G] 3) :+: line [d 4 sn, a 3 sn, d 3 sn, c 4 sn, bf 3 en, d 4 en, fs 3 en, d 4 en] :+:
            -- m59
            line [g 4 qn, fs 4 qn, g 4 qn, a 4 qn] :+:
            -- m60
            line [bf 4 qn, b 4 qn, c 5 qn, cs 5 qn] :+:
            -- m61
            line [d 5 qn, e 5 en, fs 5 en, g 5 qn, a 5 en, bf 5 en]

tenor62 :: Music Pitch
            -- m62
tenor62 = (c 6 sn) :+: (toSN [G,F,Ef] 5) :+: (c 6 sn) :+: (toSN [A,F,A] 5) :+:
            -- m62 beat 3
            line [bf 4 sn] :+: (toSN [F,Ef,D,      Bf,G,Ef,G] 5) :+:
            -- m63
            (a 4 sn) :+: (toSN [Ef,D,C,   A,Fs,D,Fs] 5) :+: (g 4 sn) :+: (bf 4 sn) :+: (toSN [D,G,Fs,G,E,Fs] 5) :+:
            -- m64
            line [g 5 dqn, fs 5 en, g 5 qn, a 5 en, d 5 en, d 5 qn] :+:
            -- m65
            line [enr, fs 4 en, g 4 en, enr, c 5 en, enr] :+:
            -- m66
            line [bf 4 en, enr, c 5 en, enr, bf 4 en, enr, a 4 en, enr, g 4 en, enr, a 4 en, enr]

tenor67 :: Music Pitch
            -- m67
tenor67 = (phrase [Tmp (Ritardando $ 1/2)] slowing) :+: b 4 wn where
  slowing = line [g 4 en, bf 4 en, c 5 en, a 4 en]

toQN :: [PitchClass] -> Octave -> Music Pitch
toQN ps o = let f p = Prim (Note qn (p,o))
            in  line (map f ps)

toEN :: [PitchClass] -> Octave -> Music Pitch
toEN ps o = let f p = Prim (Note en (p,o))
            in  line (map f ps)

toSN :: [PitchClass] -> Octave -> Music Pitch
toSN ps o = let f p = Prim (Note sn (p,o))
            in  line (map f ps)

trillDur :: Dur
trillDur = tn * 2/3
