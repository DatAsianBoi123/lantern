macro_rules! keywords {
    ($( $vis: vis $ident: ident = $name: literal ; )+) => {
        pub fn is_keyword(str: &str) -> bool {
            match str {
            $(
                $name => true,
            )+
                _ => false,
            }
        }

        $(

        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $ident(pub $crate::error::Span);

        impl $crate::ParseTokens for $ident {
            fn parse(stream: &mut $crate::stream::StreamBranch) -> $crate::Result<Self> {
                let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);
                let span = stream.span();
                match $crate::IdentWord::parse(stream) {
                    Ok($crate::IdentWord(string, span)) if string == $name => {
                        let _ = $crate::stream::Repetition::<0, $crate::stream::Whitespace>::parse(stream);
                        Ok(Self(span))
                    },
                    Ok(_) | Err(_) => Err($crate::diagnostic!(span, "expected keyword `{}`", $name).into()),
                }
            }
        }

        )+
    };
}

keywords! {
    pub True = "true";
    pub False = "false";
    pub Val = "val";
    pub If = "if";
    pub Else = "else";
    pub While = "while";
    pub Fun = "fun";
    pub Using = "using";
    pub Native = "native";
    pub Struct = "struct";
    pub Return = "return";
    pub Break = "break";
}

