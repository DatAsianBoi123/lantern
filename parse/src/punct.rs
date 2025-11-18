macro_rules! puncts {
    ($($vis: vis struct $ident: ident = $lit: literal ;)+) => {
        $(

        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $ident<T: PunctSpacing>(pub $crate::error::Span, std::marker::PhantomData<T>);

        impl<T: PunctSpacing> std::fmt::Display for $ident<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Write::write_char(f, $lit)
            }
        }

        impl $crate::ParseTokens for $ident<Spaced> {
            fn parse(stream: &mut $crate::stream::StreamBranch) -> $crate::Result<Self> {
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);
                let res = stream.try_read_one(|char, span| {
                    if char == $lit {
                        Ok(Self(span, std::marker::PhantomData))
                    } else {
                        Err($crate::diagnostic!(span, "expected `{}`", $lit).into())
                    }
                });
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);

                res
            }
        }

        impl $crate::ParseTokens for $ident<Touching> {
            fn parse(stream: &mut $crate::stream::StreamBranch) -> $crate::Result<Self> {
                stream.try_read_one(|char, span| {
                    if char == $lit {
                        Ok(Self(span, std::marker::PhantomData))
                    } else {
                        Err($crate::diagnostic!(span, "expected `{}`", $lit).into())
                    }
                })
            }
        }

        )+
    };
}

macro_rules! punct_spacing {
    ($vis: vis struct $ident: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $ident;
        impl private::Sealed for $ident {}
        impl PunctSpacing for $ident {}
    };
}

mod private {
    pub trait Sealed {}
}

pub trait PunctSpacing: private::Sealed {}

punct_spacing!(pub struct Spaced);
punct_spacing!(pub struct Touching);

puncts! {
    pub struct Comma = ',';
    pub struct Semi = ';';
    pub struct Colon = ':';
    pub struct Period = '.';
    pub struct Question = '?';
    pub struct Bang = '!';

    pub struct Plus = '+';
    pub struct Hyphen = '-';
    pub struct Asterisk = '*';
    pub struct Slash = '/';
    pub struct Percent = '%';
    pub struct Equals = '=';

    pub struct At = '@';

    pub struct OpenParen = '(';
    pub struct ClosedParen = ')';
    pub struct OpenBracket = '[';
    pub struct ClosedBracket = ']';
    pub struct OpenBrace = '{';
    pub struct ClosedBrace = '}';
}

