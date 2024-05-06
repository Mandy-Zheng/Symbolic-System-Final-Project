\version "2.22.0"
\score {
 << 
\new Staff { 
  {
\time 4/4 \key c \major \clef treble 
c'1 c'1 g'1 g'1 | 
a'1 a'1 g'2 | 
f'1 f'1 e'1 e'1 | 
d'1 d'1 c'2 | 
g'1 g'1 f'1 f'1 | 
e'1 e'1 d'2 | 
g'1 g'1 f'1 f'1 | 
e'1 e'1 c'2 | 
c'1 c'1 g'1 g'1 | 
a'1 a'1 g'2 | 
f'1 f'1 e'1 e'1 | 
d'1 d'1 c'2 | 
} 
 } 
\new Staff { 
  {
\time 4/4 \key c \major \clef bass 
<c, e, g, >4 | 
<f, a, >2 <c, e, g, >4 | 
<f, a, >2 <c, g, >2 | 
<f, g, >2 <c, g, >2 | 
g,4 | 
g,4 | 
g,4 | 
g,4 | 
\bar "||" \key d \major 
<d, fis, a' >4 | 
<g, b, >2 <d, fis, a' >4 | 
<g, b, >2 <d, a' >2 | 
<g, a' >2 <d, a' >2 | 
} 
 } 
>> 
 \layout {} 
 \midi {} 
}
