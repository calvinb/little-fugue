module Alto where

import Euterpea

fugue :: Music Pitch
fugue = tempo (4/7) $ instrument ChurchOrgan alto

alto = foldl1 (:+:) [alto6, alto11, alto22, alto33, alto43, alto49,
                    alto55, alto61, alto67]

alto6 = line [d 5 qn, a 5 qn, f 5 dqn, e 5 en] :+:
        ens 5 [d, f, e, d, cs, e] :+: a 4 qn :+:
        line [d 5 en, a 4 en, e 5 en, a 4 en] :+:
        line [f 5 en, e 5 sn, d 5 sn, e 5 en, a 4 en] :+:
        line [d 5 en, a 4 sn, d 5 sn, e 5 en, a 4 sn, e 5 sn] :+:
        f 5 en :+: sns 5 [e, d, e] :+: a 4 sn :+:
        sns 5 [a, g, f, e, d, f, e, d, cs, e, d] :+: a 4 sn :+:
        sns 5 [d, e, f, g, a, b]

alto11 = duration sn [c 6, bf 5, c 6, d 6, c 6, bf 5, a 5, c 6,
          bf 5, a 5, bf 5, c 6] :+: sns 5 [bf, a, g, bf] :+:
          ens 5 [a, g, fs, d] :+: line [g 5 qn, qnr, enr] :+:
          ens 5 [d, g, a] :+: duration sn [bf 5, c 6, bf 5, c 6] :+:
          line [trill 2 trillDur (c 6 den), bf 5 tn, c 6 tn] :+:
          sns 6 [d, c, d, ef, d, c] :+: sns 5 [bf, a] :+:
          timesM 2 (duration en [d 5, rest, c 5, rest]) :+:
          timesM 4 (duration sn [rest, g 5, fs 5, g 5]) :+:
          duration qn [d 5, c 5, bf 4, rest] :+: hnr :+: timesM 4 wnr

alto22 = line [enr, a 5 en, d 6 en, c 6 en, bf 5 (hn + en),
               g 5 en, c 6 en, bf 5 en, a 5 (hn + qn),
               g 5 hn, fs 5 qn, g 5 en, enr, qnr, hnr] :+:
         timesM 7 wnr

alto33 = line [bf 4 qn, f 5 qn, d 5 dqn, c 5 en] :+:
         duration en [bf 4, d 5, c 5, bf 4, a 4, c 5, rest, f 5] :+:
         line [f 5 bn, bnr] :+: timesM 4 wnr

alto43 = line [bnr, hnr, enr, f 5 en, bf 5 en, a 5 en, g 5 (hn + en),
              c 5 en, a 5 en, g 5 en, f 5 (hn + en),
              bf 4 en, g 5 en, f 5 en, ef 5 (hn + sn),
              ef 5 sn, d 5 sn, c 5 sn, b 4 qn]

alto49 = line [c 5 qn, qnr, enr, c 5 en, af 5 en, f 5 (en + qn + sn)] :+:
         duration sn (octave 5 [f, ef, d, ef, d, c, ef, d, c] ++
                      [b 4, d 5, c 5, g 4] ++
                      octave 5 [c, d, ef, f, ef, f, g, ef, c, g,
                                af, f, g, af, d, ef, d, c]) :+:
          line [b 4 en, d 5 en, g 4 (hn + wn + hn + en), g 5 en] :+:
          duration sn (octave 5 [f, ef, d, f])

alto55 = line [ef 5 qn, qnr, enr, c 5 en, a 4 en, f 4 en,
              bf 4 (hn + en), bf 4 en, g 4 en, ef 4 en,
              a 4 (hn + en), a 4 en, fs 4 en, d 4 en,
              g 4 qn, qnr] :+:
         duration sn [g 4, bf 4, a 4, g 4, d 5, a 4, d 4, c 5] :+:
         duration en ([bf 4, d 5, a 4]) :+: d 5 qn :+:
         line [g 5 en, c 5 en, f 5 qn, f 5 en, d 5 en, g 5 qn, g 5 en, e 5 en, a 5 den]

alto61 = duration sn ([ef 6, d 6, c 6, bf 5, d 6, a 5, d 6, g 5] ++
                      octave 6 [af, g, f, g, f, ef, d]) :+:
         line [ef 6 hn, d 6 hn, c 6 hn] :+:
         duration sn [bf 5, d 6, c 6, bf 5, a 5, bf 5, g 5, a 5,
                      bf 5, c 6, bf 5, c 6, d 6, ef 6, d 6, c 6] :+:
         duration en [bf 5, d 6, c 6, bf 5] :+:
         line [a 5 qn, enr, d 5 en, trill 1 trillDur (d 5 (hn + wn + hn))]

alto67 = (phrase [Tmp (Ritardando $ 1/2)] slowing) :+: d 5 wn where
  slowing = line [d 5 en, ef 5 qn, d 5 en]

ens :: Octave -> [Octave -> Dur -> Music Pitch] -> Music Pitch
ens o fs = line $ map (\f -> f o en) fs

sns :: Octave -> [Octave -> Dur -> Music Pitch] -> Music Pitch
sns o fs = line $ map (\f -> f o sn) fs

duration :: Dur -> [Dur -> Music Pitch] -> Music Pitch
duration d fs = line $ map (\f -> f d) fs

octave :: Octave -> [Octave -> Dur -> Music Pitch] -> [Dur -> Music Pitch]
octave o fs = map (\f -> f o) fs

followEach :: [Music Pitch] -> Music Pitch -> Music Pitch
followEach ms m = foldl1 (:+:) (map (:+: m) ms)

trillDur :: Dur
trillDur = tn * 2/3
