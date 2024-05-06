\version "2.22.0"
\header {
	title="Gibberish"
	}
\score {
 << 
\new Staff { 
  {
\time 4/4 \key c \major \clef treble 
c'4 c'4 e'4 g'4 | 
f'4 e'8 d'8 g'2 | 
\bar "||" \key d \major 
d'4 g'4 a'4 g'4 | 
cis'4 d'4 e'2 |
e'8 a'8 g'4 a'2 |
} 
 } 
\new Staff { 
  {
\time 4/4 \key c \major \clef bass 
<c e g >1 | 
<f a >2 <c e g >2 | 
g1 | 
\bar "||" \key d \major 
<d fis a >1 | 
<g b >2 <d fis a >2 | 
} 
 } 
>> 
 \layout {indent=0} 
 \midi {} 
}
