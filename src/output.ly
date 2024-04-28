\version "2.22.0"
\score {
 << 
\new Staff { 
 \relative c' {
\time 4/4 \key c \major \clef treble 
c4 d4 e4 f4 | 
g4 a4 <c e g >2 | 
} 
 } 
\new Staff { 
 \relative c' {
\time 4/4 \key c \major \clef bass 
a,4 gis4 <c e g >2 | 
b4 d4 <f a c >2 | 
} 
 } 
>> 
 \layout {} 
}
