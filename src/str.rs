#[inline]
pub fn char_at(str: &String, index: usize) -> Option<char> {
    str.chars().nth(index)
}
