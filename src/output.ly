\version "2.22.0"
\score {
 << 
\new Staff { 
 \relative c' {
\time 4/4 \key c \major \clef bass 
b4 d4 <f a >2 | 
b4 d4 <f a >2 | 
\bar "||" \clef treble 
c1 c1 g1 g1 | 
} 
 } 
\new Staff { 
 \relative c' {
\time 4/4 \key c \major \clef bass 
a,4 gis4 <c e >2 | 
b4 d4 <f a >2 | 
} 
 } 
>> 
 \layout {} 
}
