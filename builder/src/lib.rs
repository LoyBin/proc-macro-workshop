use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Attribute, Data,
    DeriveInput, Error, Fields, FieldsNamed, GenericArgument, Ident, Lit, Meta, MetaList,
    MetaNameValue, NestedMeta, PathArguments, Result, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;

    let expand = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named_fields) => {
                let builder_struct = builder_struct(&ident, named_fields);
                let builder_fn = builder_fn(&ident, named_fields);
                let impl_builder = impl_builder(&ident, named_fields);
                quote! {
                    use ::std::error::Error;
                    #builder_struct
                    #builder_fn
                    #impl_builder
                }
            }
            _ => panic!("only support FieldsNamed"),
        },
        _ => panic!("only support Struct"),
    };

    expand.into()
}

fn builder_struct(ident: &Ident, fields: &FieldsNamed) -> TokenStream {
    let name = format_ident!("{}Builder", ident);
    let recurse = fields.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if field_type_cmp(ty, "Option") {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });
    quote! {
        struct #name {
            #(#recurse),*
        }
    }
}

fn builder_fn(ident: &Ident, fields: &FieldsNamed) -> TokenStream {
    let name = format_ident!("{}Builder", ident);
    let recurse = fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: None
        }
    });
    quote! {
        impl #ident {
            fn builder() -> #name {
                #name {
                    #(#recurse),*
                }
            }
        }
    }
}

fn impl_builder(ident: &Ident, fields: &FieldsNamed) -> TokenStream {
    let name = format_ident!("{}Builder", ident);
    let setter = fields.named.iter().map(|f| {
        let name = &f.ident;
        let each_fn = if let Some(attr) = &f.attrs.first() {
            let each = get_each_fn(attr);
            match each {
                Ok(e) => {
                    if name.eq(&e) {
                        return quote! {};
                    }
                    if let Some(i) = e {
                        let ty = get_sub_type(&f.ty).unwrap();
                        quote! {
                            fn #i(&mut self, value: #ty) -> &mut Self {
                                if self.#name.is_none() {
                                    self.#name = Some(vec![value])
                                } else {
                                    self.#name.as_mut().map(|v| v.push(value));
                                }
                                self
                            }
                        }
                    } else {
                        unimplemented!()
                    }
                }
                Err(err) => TokenStream::from(err.to_compile_error()),
            }
        } else {
            quote! {}
        };
        let mut ty = f.ty.clone();
        if field_type_cmp(&ty, "Option") {
            ty = get_sub_type(&ty).unwrap();
        }
        quote! {
            fn #name(&mut self, value: #ty) -> &mut Self {
                self.#name = Some(value);
                self
            }
            #each_fn
        }
    });
    let build_fields = fields.named.iter().map(|f| {
        let name = &f.ident;
        let err = format!("{} is none", name.as_ref().unwrap().to_string());
        if field_type_cmp(&f.ty, "Option") {
            quote! {
                #name: match self.#name {
                    Some(ref value) => Some(Clone::clone(value)),
                    None => None
                }
            }
        } else if field_type_cmp(&f.ty, "Vec") {
            quote! {
                #name: match self.#name {
                    Some(ref value) => Clone::clone(value),
                    None => vec![]
                }
            }
        } else {
            quote! {
                #name: match self.#name {
                    Some(ref value) => Clone::clone(value),
                    None => {
                        return Err(String::from(#err).into());
                    }
                }
            }
        }
    });
    quote! {
        impl #name {
            #(#setter)*
            pub fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn Error>> {
                Ok(
                    #ident {
                        #(#build_fields),*
                    }
                )
            }
        }
    }
}

// get Option or Vec sub Type
fn get_sub_type(ty: &Type) -> ::std::option::Option<Type> {
    if let Type::Path(TypePath { path, .. }) = &ty {
        let arguments = &path.segments.first().unwrap().arguments;
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
            arguments
        {
            let first = args.first().unwrap();
            if let GenericArgument::Type(t) = first {
                Some(t.clone())
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn field_type_cmp(ty: &Type, other: &str) -> bool {
    if let Type::Path(TypePath { path, .. }) = &ty {
        path.segments.first().unwrap().ident.eq(other)
    } else {
        false
    }
}

fn get_each_fn(attr: &Attribute) -> Result<::std::option::Option<Ident>> {
    let meta = attr.parse_meta()?;
    match &meta {
        Meta::List(MetaList { path, nested, .. }) => {
            if path.is_ident("builder") {
                if let NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) =
                    nested.first().unwrap()
                {
                    if path.is_ident("each") {
                        if let Lit::Str(s) = lit {
                            Ok(Some(format_ident!("{}", s.value())))
                        } else {
                            Err(Error::new(
                                meta.span(),
                                "expected `builder(each = \"...\")`",
                            ))
                        }
                    } else {
                        Err(Error::new(
                            meta.span(),
                            "expected `builder(each = \"...\")`",
                        ))
                    }
                } else {
                    Err(Error::new(
                        meta.span(),
                        "expected `builder(each = \"...\")`",
                    ))
                }
            } else {
                Err(Error::new(
                    meta.span(),
                    "expected `builder(each = \"...\")`",
                ))
            }
        }
        _ => Ok(None),
    }
}
