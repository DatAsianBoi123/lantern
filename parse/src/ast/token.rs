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
            where I: Iterator<Item = $crate::lex::TokenTree> + Clone
            {
                Option::<$ident>::parse(iter)?.ok_or_else(|| $crate::diagnostic!("expected token `{}`", $name).into())
            }
        }

        impl $crate::Parse<$crate::lex::TokenTree> for Option<$ident> {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self, $crate::error::Diagnostics>
            where I: Iterator<Item = $crate::lex::TokenTree> + Clone
            {
                let Some($crate::lex::TokenTree::Ident($crate::lex::Ident { name })) = iter.peek() else {
                    return Ok(None);
                };
                if name == $name {
                    iter.next();
                    Ok(Some($ident))
                } else {
                    Ok(None)
                }
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

impl_token!(
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Native = "native"
);

