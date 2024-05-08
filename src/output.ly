\version "2.22.0"
\header {title="name"}
\score {
 << 
\new Staff { 
  {
\time 4/4 \key c \major \clef treble 
r4 c'4 r2 | 
r4 <c' e' g' >2. | 
} 
 } 
\new Staff { 
  {
\time 4/4 \key c \major \clef bass 
a4 gis'4 <c' e' g' >2 | 
b'4 d'4 <f' a' c' >2 | 
} 
 } 
>> 
 \layout {indent=0} 
 \midi {} 
}
