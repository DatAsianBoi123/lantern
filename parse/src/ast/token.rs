macro_rules! impl_token {
    ($(#[$meta: meta])* $vis: vis struct $ident: ident = $name: literal) => {
        $(#[$meta])*
        $vis struct $ident;

        impl $ident {
            pub fn is(token: &$crate::lex::TokenTree) -> bool {
                match token {
                    $crate::lex::TokenTree::Ident($crate::lex::Ident { name }) if name == $name => true,
                    _ => false,
                }
            }
        }

        impl $crate::Parse<$crate::lex::TokenTree> for $ident {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self, $crate::error::Diagnostics>
            where I: Iterator<Item = $crate::lex::TokenTree>
            {
                let Some($crate::lex::TokenTree::Ident($crate::lex::Ident { name })) = iter.next() else {
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

impl_token!(
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Using = "using"
);

