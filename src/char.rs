pub trait DOMScriptChar {
    fn is_ident_start(self) -> bool;
    fn is_ident_part(self) -> bool;
}

impl DOMScriptChar for char {
    fn is_ident_start(self) -> bool {
        match self {
            'a'...'z' | 'A'...'Z' => true,
            _ => false,
        }
    }
    fn is_ident_part(self) -> bool {
        match self {
            ch if ch.is_ident_start() => true,
            '0'...'9' | '_' => true,
            _ => false,
        }
    }
}
