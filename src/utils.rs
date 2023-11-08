use lang_c::span::Span;

pub trait SpanUtils {
    fn contains(&self, other: &Self) -> bool;
}

impl SpanUtils for Span {
    fn contains(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}
