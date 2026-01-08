sy case match

sy keyword Keyword val fun using native return break if else while struct
sy keyword Boolean true false

sy match Identifier /\w\+/
sy match Function /\w\+\ze\s*(.*)/
sy match Operator /[=+\-*/%<>]\|==\|||\|&&/
sy match Delimiter /[;:.,(){}\[\]]\|->/
sy match Type /\(\(:\s*\)\@<=\w\+\)/
sy match Type /\(\(->\s*\)\@<=\w\+\)/
sy match Number /\d\+\(\.\d\+\)\?/
sy match EscapeCharacter /\\[\\nrt"]/ contained
sy match Special /@\w*/
sy match Comment /\/\/.*/

sy region String start=/"/ end=/"/ skip=/\\"/ contains=EscapeCharacter

hi link EscapeCharacter Special

