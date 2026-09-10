sy case match

sy keyword Keyword val fun using native return continue break if else while struct primitive throw
sy keyword Boolean true false

sy match Identifier /\w\+/
sy match Function /\w\+\ze\s*(\_.*)/
sy match Operator /[=+\-*/%<>!]\|||\|&&/
sy match Delimiter /[;:.,()\[\]]\|->/
sy match Type /\(:\s*\)\@<=\w\+/
sy match Type /\(->\s*\)\@<=\w\+/
sy match Type /\(struct\s*\)\@<=\w\+/
sy match Type /\(primitive\s*\)\@<=\w\+/
sy match Number /\d\+\(\.\d\+\)\?/
sy match EscapeCharacter /\\[\\nrt"]/ contained
sy match Special /@\w*/
sy match Comment /\/\/.*/

sy region Block matchgroup=Delimiter start="{" end="}" contains=TOP fold
sy region String start=/"/ end=/"/ skip=/\\"/ contains=EscapeCharacter
sy region String start=/$"/ end=/"/ skip=/\\"/ contains=EscapeCharacter,Block

hi link EscapeCharacter Special
hi link StringTemplate Special

