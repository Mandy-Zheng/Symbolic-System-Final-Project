\version "2.22.0"
\score {
 << 
\new Staff { 
 \relative c' {
\time 4/4 \key c \major \clef treble 
a8 b8 c8 d8 e8 f8 g8 a'8 | 
g4 a4 <c e g >2 | 
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
