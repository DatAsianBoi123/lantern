macro_rules! impl_token {
    ($(#[$meta: meta])* $vis: vis struct $ident: ident = $name: literal) => {
        $(#[$meta])*
        $vis struct $ident;

        impl $ident {
            pub fn is(token: &parse::lex::TokenTree) -> bool {
                match token {
                    parse::lex::TokenTree::Ident(parse::lex::Ident { name }) if name == $name => true,
                    _ => false,
                }
            }
        }

        impl parse::Parse<parse::lex::TokenTree> for $ident {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self, parse::error::Diagnostics>
            where I: Iterator<Item = parse::lex::TokenTree>
            {
                let Some(parse::lex::TokenTree::Ident(parse::lex::Ident { name })) = iter.next() else {
                    return Err($crate::diagnostic!("expected token `{}`", $name).into());
                };
                if name == $name { Ok(Self) }
                else { Err($crate::diagnostic!("expected token `{}`", $name).into()) }
            }
        }
    };
}

impl_token!(
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Val = "val"
);

impl_token!(
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Fun = "fun"
);

