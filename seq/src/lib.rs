use std::ops::Range;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use syn::{
    buffer::TokenBuffer,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitInt, Token,
};
struct SeqToken {
    ident: Ident,
    start: LitInt,
    end: LitInt,
    eq_token: Option<Token![=]>,
    body: proc_macro2::TokenStream,
}

impl Parse for SeqToken {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let ident = input.parse()?;
        input.parse::<Token!(in)>()?;
        let start = input.parse()?;
        input.parse::<Token!(..)>()?;
        let eq_token = input.parse().ok();
        let end = input.parse()?;
        syn::braced!(content in input);
        let body = content.parse()?;
        Ok(Self {
            ident,
            start,
            end,
            eq_token,
            body,
        })
    }
}

impl SeqToken {
    fn expand(self) -> TokenStream {
        let start = self.start.base10_parse::<usize>();
        let end = self.end.base10_parse::<usize>();
        match (start, end) {
            (Ok(s), Ok(e)) => {
                let range = self.eq_token.map_or(s..e, |_| s..e + 1);
                let mut has_repeat = false;
                let expanded = replace_repeat(
                    self.body.clone(),
                    range.clone(),
                    &self.ident,
                    &mut has_repeat,
                );
                if has_repeat {
                    TokenStream::from(expanded)
                } else {
                    let mut expanded = proc_macro2::TokenStream::new();
                    for v in range {
                        expanded.extend(replace(self.body.clone(), &self.ident, v));
                    }
                    TokenStream::from(expanded)
                }
            }
            (Err(e), _) => TokenStream::from(e.to_compile_error()),
            (_, Err(e)) => TokenStream::from(e.to_compile_error()),
        }
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as SeqToken);
    TokenStream::from(seq.expand())
}

fn replace(body: proc_macro2::TokenStream, var: &Ident, value: usize) -> proc_macro2::TokenStream {
    let mut output = proc_macro2::TokenStream::new();
    let buffer = TokenBuffer::new2(body);
    let mut cursor = buffer.begin();
    while let Some((t, next)) = cursor.token_tree() {
        cursor = next;
        match t {
            TokenTree::Group(g) => {
                let mut group = Group::new(g.delimiter(), replace(g.stream(), var, value));
                group.set_span(g.span());
                output.extend(proc_macro2::TokenStream::from(TokenTree::from(group)));
            }
            TokenTree::Ident(ref i) => {
                if i.eq(var) {
                    let mut literal = Literal::usize_unsuffixed(value);
                    literal.set_span(t.span());
                    output.extend(proc_macro2::TokenStream::from(TokenTree::from(literal)));
                } else if let Some((p, next)) = next.punct() {
                    if p.as_char().eq(&'~') {
                        if let Some((id, next)) = next.ident() {
                            if id.eq(var) {
                                cursor = next;
                                let mut name = quote::format_ident!("{}{}", i, value);
                                name.set_span(i.span());
                                output
                                    .extend(proc_macro2::TokenStream::from(TokenTree::from(name)));
                            }
                        }
                    } else {
                        output.extend(proc_macro2::TokenStream::from(t));
                    }
                } else {
                    output.extend(proc_macro2::TokenStream::from(t));
                }
            }
            _ => {
                output.extend(proc_macro2::TokenStream::from(t));
            }
        }
    }
    output
}

fn replace_repeat(
    body: proc_macro2::TokenStream,
    range: Range<usize>,
    var: &Ident,
    has_repeat: &mut bool,
) -> proc_macro2::TokenStream {
    let mut output = Vec::from_iter(body);
    let mut i = 0;
    while i < output.len() {
        if let TokenTree::Group(ref mut g) = output[i] {
            let content = replace_repeat(g.stream(), range.clone(), var, has_repeat);
            let original_span = g.span();
            *g = Group::new(g.delimiter(), content);
            g.set_span(original_span);
            i += 1;
            continue;
        }
        if i + 3 > output.len() {
            i += 1;
            continue;
        }
        if let (TokenTree::Punct(ref p), TokenTree::Group(ref g), TokenTree::Punct(ref p2)) =
            (&output[i], &output[i + 1], &output[i + 2])
        {
            if p.as_char() == '#' && p2.as_char() == '*' && g.delimiter() == Delimiter::Parenthesis
            {
                *has_repeat = true;
                let mut replaced = Vec::new();
                for v in range.clone() {
                    replaced.extend(replace(g.stream(), var, v));
                }
                let len = replaced.len();
                output.splice(i..=i + 2, replaced);
                i += len;
            }
        }
        i += 1;
    }
    proc_macro2::TokenStream::from_iter(output)
}
