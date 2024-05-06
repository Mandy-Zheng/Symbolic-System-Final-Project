\version "2.22.0"
\header {title="twinkle1"}
\score {
 << 
\new Staff { 
  {
\time 4/4 \key c \major \clef treble 
c'4 c'4 g'4 g'4 | 
a'4 a'4 g'2 | 
f'4 f'4 e'4 e'4 | 
d'4 d'4 c'2 | 
g'4 g'4 f'4 f'4 | 
e'4 e'4 d'2 | 
g'4 g'4 f'4 f'4 | 
e'4 e'4 d'2 | 
\bar "||" \key d \major 
d'4 d'4 a'4 a'4 | 
b'4 b'4 a'2 | 
g'4 g'4 fis'4 fis'4 | 
e'4 e'4 d'2 | 
} 
 } 
\new Staff { 
  {
\time 4/4 \key c \major \clef bass 
<c e g >1 | 
<f a >2 <c e g >2 | 
<f a >2 <c g >2 | 
<f g >2 <c e g >2 | 
g1 | 
g1 | 
g1 | 
g1 | 
\bar "||" \key d \major 
<d fis a >1 | 
<g b >2 <d fis a >2 | 
<g b >2 <d a >2 | 
<g a >2 <d fis a >2 | 
} 
 } 
>> 
 \layout {indent=0} 
 \midi {} 
}
