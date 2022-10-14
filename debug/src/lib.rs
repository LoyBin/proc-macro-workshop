use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, spanned::Spanned, token::Comma,
    AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Error, Field, Fields,
    FieldsNamed, GenericArgument, GenericParam, Generics, Lit, LitStr, Meta, MetaList,
    MetaNameValue, NestedMeta, Path, PathArguments, Result, Type, TypePath, WherePredicate,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let struct_name = name.to_string();
    let where_preds = check_bound(&input.attrs);
    let tokens = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            let recurse = named.iter().map(|f| {
                let name = &f.ident;
                let field_name = name.as_ref().unwrap().to_string();

                let mut field_fn = quote! {
                    .field(#field_name, &self.#name)
                };
                if let Some(attr) = &f.attrs.first() {
                    if let Ok(s) = get_meta_lit(attr) {
                        field_fn = quote! {
                            .field(#field_name, &::std::format_args!(#s, &self.#name))
                        }
                    }
                };

                field_fn
            });
            let mut generics = input.generics.clone();
            if where_preds.is_empty() {
                generics = add_trait_bounds(input.generics, named);
            } else {
                let where_clause = generics.make_where_clause();
                where_clause.predicates.extend(where_preds);
            }
            let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
            quote! {
                impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
                    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        fmt.debug_struct(#struct_name)#(#recurse)*.finish()
                    }
                }
            }
        }
        _ => {
            panic!("Struct Only")
        }
    };
    tokens.into()
}

fn get_meta_lit(attr: &Attribute) -> Result<LitStr> {
    let meta = attr.parse_meta()?;
    if let Meta::NameValue(MetaNameValue {
        path,
        lit: Lit::Str(lit),
        ..
    }) = &meta
    {
        if path.is_ident("debug") {
            Ok(lit.clone())
        } else {
            Err(Error::new(meta.span(), "expected `debug = \"...\"`"))
        }
    } else {
        Err(Error::new(meta.span(), "expected `debug = \"...\"`"))
    }
}

fn add_trait_bounds(mut generics: Generics, fields: &Punctuated<Field, Comma>) -> Generics {
    let mut associated = ::std::collections::HashSet::with_capacity(8);
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = param {
            let ident = &type_param.ident;
            let phantom_data: Type = parse_quote!(PhantomData<#ident>);
            if fields.iter().fold(false, |acc, f| {
                let has_generic_ty = contain_generic_type(&f.ty, ident, &mut associated);
                acc || (has_generic_ty && f.ty.ne(&phantom_data))
            }) {
                type_param.bounds.push(parse_quote!(::std::fmt::Debug));
            };
        }
    }
    let where_clause = generics.make_where_clause();
    for t in associated {
        where_clause
            .predicates
            .push(parse_quote!(#t: ::std::fmt::Debug));
    }
    generics
}

fn contain_generic_type(
    ty: &Type,
    ident: &Ident,
    associated: &mut ::std::collections::HashSet<Type>,
) -> bool {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if segments.first().unwrap().ident.eq(ident) {
            if segments.len() > 1 {
                associated.insert(ty.clone());
                return false;
            }
            return true;
        }
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
            &segments.first().unwrap().arguments
        {
            return args.iter().fold(false, |acc, arg| {
                if let GenericArgument::Type(ref t) = arg {
                    return acc || contain_generic_type(t, ident, associated);
                }
                acc || false
            });
        }
    }
    false
}

fn check_bound(attrs: &Vec<Attribute>) -> Punctuated<WherePredicate, Comma> {
    let mut where_preds: Punctuated<WherePredicate, Comma> = Punctuated::new();
    for attr in attrs {
        let meta = attr.parse_meta().expect("parse meta err");
        if let Meta::List(MetaList { path, nested, .. }) = meta {
            if path.is_ident("debug") {
                for m in nested {
                    if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                        path,
                        lit: Lit::Str(s),
                        ..
                    })) = m
                    {
                        if path.is_ident("bound") {
                            let pw: Punctuated<WherePredicate, Comma> =
                                s.parse_with(Punctuated::parse_terminated).unwrap();
                            where_preds.extend(pw);
                        }
                    }
                }
            }
        }
    }
    where_preds
}
