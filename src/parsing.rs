use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;
use crate::types::*;

pub fn parse_document(doc: &str) -> Result<ViewDecl, String> {
    let pdoc = roxmltree::Document::parse(doc).map_err(|e| e.to_string())?;
    let view_elem = pdoc.root_element();
    if view_elem.tag_name().name() != "view" {
        return Err("Root must be <view>".to_owned());
    }
    let name = view_elem.attribute("name").ok_or_else(|| "Root must have a name attribute".to_owned())?;
    let mut args = HashMap::new();
    let mut body = Rc::new(vec![]);

    let mut has_named_children = false;
    let mut has_unnamed_child = false;

    for child in view_elem.children() {
        if child.is_comment() || (child.is_text() && child.text().unwrap().chars().all(|x| x.is_whitespace())) {
            continue;
        }
        match child.tag_name().name() {
            "arg" => {
                let arg = parse_arg_decl(&child)?;
                match arg.loc {
                    ArgLoc::NamedChild => {
                        if has_unnamed_child {
                            return Err(format!("Cannot have both named and unnamed children args"));
                        }
                        has_named_children = true;
                    }
                    ArgLoc::UnnamedChild => {
                        if has_unnamed_child {
                            return Err(format!("Cannot have two unnamed children args"));
                        }
                        if has_named_children {
                            return Err(format!("Cannot have both named and unnamed children args"));
                        }
                        has_unnamed_child = true;
                    }
                    ArgLoc::Attr => {}
                }

                if args.contains_key(&arg.name) {
                    return Err(format!("Duplicate arg definition for {}", arg.name));
                }
                args.insert(arg.name.clone(), arg);
            }
            "body" => {
                if !body.is_empty() {
                    return Err(format!("Multiple body declarations for a single view"));
                }
                match parse_body(&child, &args)? {
                    Child::Children(ch) => body = ch,
                    Child::Leaf(_) => return Err(format!("View body must not be text")),
                }
            }
            _ => {
                return Err(format!("unexpected view child: {:?} {}", child.node_type(), child.tag_name().name()))
            }
        }
    }

    Ok(ViewDecl {
        name: name.to_owned(),
        args,
        varargs: false,
        body: ViewBody::Xml(body),
    })
}

fn parse_body(node: &roxmltree::Node, args: &HashMap<String, ArgDecl>) -> Result<Child, String> {
    let mut text_result = String::new();
    let mut children_result = vec![];

    for child in node.children() {
        if child.is_text() {
            let text = child.text().unwrap();
            let ttext = text.trim();
            // TODO allow multiple variables in a row
            if ttext.starts_with("{{") && ttext.ends_with("}}") {
                let var = ttext[2..ttext.len()-2].trim();
                if let Some(decl) = args.get(var) {
                    match decl.ty {
                        ArgType::String | ArgType::Int | ArgType::Float => return Ok(Child::Leaf(Leaf::Arg(var.to_owned()))),
                        ArgType::View => children_result.push(Node::Var(var.to_owned())),
                        ArgType::Callback => return Err(format!("Arg {} is a callback and cannot be a node body", var)),
                    }
                } else {
                    return Err(format!("Arg {} not found", var));
                }
            } else {
                text_result += text;
            }
        } else if child.is_element() {
            children_result.push(Node::Const(ConstNode {
                name: child.tag_name().name().to_owned(),
                attrs: child.attributes().iter().map(|a| Ok((a.name().to_owned(), a.value().parse()?))).collect::<Result<HashMap<String, Leaf>, String>>()?,
                children: parse_body(&child, args)?,
            }));
        } else if child.is_comment() {
            continue;
        } else {
            return Err(format!("Unexpected xml node: {:?}", child.node_type()));
        }
    }

    Ok(if children_result.is_empty() {
        Child::Leaf(text_result.parse()?)
    } else {
        if text_result.trim().is_empty() {
            Child::Children(Rc::new(children_result))
        } else {
            return Err(format!("Cannot interpolate text and tags"));
        }
    })
}

fn parse_arg_decl(node: &roxmltree::Node) -> Result<ArgDecl, String> {
    assert_eq!(node.tag_name().name(), "arg");
    let ty: ArgType = node.attribute("type").ok_or("Arg decl must have type")?.parse()?;
    Ok(ArgDecl {
        name: node.attribute("name").ok_or("Arg decl must have name")?.to_owned(),
        loc: match node.attribute("loc") {
            None => ArgLoc::Attr,
            Some(loc) => loc.parse()?,
        },
        default: match node.attribute("default") {
            None => None,
            Some(attr) => Some(ty.parse_const(attr)?),
        },
        ty,
    })
}

impl FromStr for ArgType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str().trim() {
            "string" => ArgType::String,
            "int" => ArgType::Int,
            "float" => ArgType::Float,
            "view" => ArgType::View,
            "callback" => ArgType::Callback,
            _ => return Err("Bad arg type: expected string, int, float, callback, or view".to_owned()),
        })
    }
}

impl FromStr for ArgLoc {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str().trim_matches(' ') {
            "attr" => ArgLoc::Attr,
            "unnamed-child" => ArgLoc::UnnamedChild,
            "named-child" => ArgLoc::NamedChild,
            _ => return Err("Bad arg loc: expected attr, unnamed-child, named-child".to_owned()),
        })
    }
}

impl FromStr for Leaf {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let st = s.trim();
        Ok(if st.starts_with("{{") && st.ends_with("}}") {
            Leaf::Arg(st[2..st.len()-2].trim().to_owned())
        } else {
            Leaf::Const(s.to_owned())
        })
    }
}

impl FromStr for DynableCallback {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.trim().to_lowercase() == "null" {
            Ok(DynableCallback::Null)
        } else {
            Err(format!("Constant callback may only be specified as null"))
        }
    }
}

impl FromStr for DynableView {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.trim().to_lowercase() == "null" {
            Ok(DynableView::Dyn(Rc::new(Box::new(|_| {}))))
        } else {
            Err(format!("Constant view may only be specified as null"))
        }
    }
}
