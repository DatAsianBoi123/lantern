macro_rules! punct {
    ($ident: ident = $lit: literal) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $ident(pub $crate::error::Span);

        impl std::fmt::Display for $ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Write::write_char(f, $lit)
            }
        }

        impl $crate::ParseTokens for $ident {
            fn parse(stream: &mut $crate::stream::StreamBranch) -> $crate::Result<Self> {
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);
                let punct = $crate::stream::Char::<$lit>::parse(stream)?;
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);

                Ok(Self(punct.1))
            }
        }
    };

    ($ident: ident ($left: literal, $right: literal)) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $ident(pub $crate::error::Span);

        impl std::fmt::Display for $ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::write!(f, "{}{}", $left, $right)
            }
        }

        impl $crate::ParseTokens for $ident {
            fn parse(stream: &mut $crate::stream::StreamBranch) -> $crate::Result<Self> {
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);
                let first = $crate::stream::Char::<$left>::parse(stream)?;
                let _ = $crate::stream::Char::<$right>::parse(stream)?;
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);
                
                Ok(Self(first.1))
            }
        }
    };
}

punct!(DoubleSlash('/', '/'));

punct!(Comma = ',');
punct!(Semi = ';');
punct!(Colon = ':');
punct!(Period = '.');
punct!(Bang = '!');

punct!(Plus = '+');
punct!(Hyphen = '-');
punct!(Asterisk = '*');
punct!(Slash = '/');
punct!(Percent = '%');
punct!(Equals = '=');

punct!(Less = '<');
punct!(Greater = '>');
punct!(LessEq('<', '='));
punct!(GreaterEq('>', '='));
punct!(EqualsEquals('=', '='));

punct!(And('&', '&'));
punct!(Or('|', '|'));

punct!(ArrowRight('-', '>'));

punct!(OpenParen = '(');
punct!(ClosedParen = ')');
punct!(OpenBracket = '[');
punct!(ClosedBracket = ']');
punct!(OpenBrace = '{');
punct!(ClosedBrace = '}');

