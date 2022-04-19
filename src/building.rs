use std::collections::HashMap;
use std::rc::Rc;
use vizia::*;
use crate::types::*;

const DEFAULT_ARGS: [&'static str; 2] = ["id", "class"];

impl Builder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            args: HashMap::new(),
        }
    }

    pub fn arg_const_str(mut self, name: &str, val: String) -> Self {
        self.args.insert(name.to_owned(), ArgVal::String(DynableValue::Const(val)));
        self
    }
    pub fn arg_const_int(mut self, name: &str, val: i32) -> Self {
        self.args.insert(name.to_owned(), ArgVal::Int(DynableValue::Const(val)));
        self
    }
    pub fn arg_const_float(mut self, name: &str, val: f32) -> Self {
        self.args.insert(name.to_owned(), ArgVal::Float(DynableValue::Const(val)));
        self
    }
    pub fn arg_dyn_str(mut self, name: &str, val: impl Lens<Target = String>) -> Self {
        self.args.insert(name.to_owned(), ArgVal::String(DynableValue::Dyn(Box::new(LensWrapSmaller { lens: val }))));
        self
    }
    pub fn arg_dyn_int(mut self, name: &str, val: impl Lens<Target = i32>) -> Self {
        self.args.insert(name.to_owned(), ArgVal::Int(DynableValue::Dyn(Box::new(LensWrapSmaller { lens: val }))));
        self
    }
    pub fn arg_dyn_float(mut self, name: &str, val: impl Lens<Target = f32>) -> Self {
        self.args.insert(name.to_owned(), ArgVal::Float(DynableValue::Dyn(Box::new(LensWrapSmaller { lens: val }))));
        self
    }
    pub fn arg_dyn_callback<F: 'static + Fn(&mut Context)>(mut self, name: &str, f: F) -> Self {
        self.args.insert(name.to_owned(), ArgVal::Callback(DynableCallback::Dyn(Rc::new(Box::new(f)))));
        self
    }
    pub fn arg_dyn_view<F: 'static + Fn(&mut Context)>(mut self, name: &str, f: F) -> Self {
        self.args.insert(name.to_owned(), ArgVal::View(DynableView::Dyn(Rc::new(Box::new(f)))));
        self
    }
    pub fn arg(mut self, name: &str, val: ArgVal) -> Self {
        self.args.insert(name.to_owned(), val);
        self
    }

    pub fn build(mut self, cx: &mut Context) -> Handle<'_, ()> {
        // look up what view we're referencing
        let store = cx.data::<Store>().unwrap().views.clone();
        let store_b = store.borrow();
        let decl = store_b.get(&self.name).unwrap_or_else(|| panic!(" view not registered: {}", &self.name));

        // typecheck args
        for (arg_name, arg_decl) in &decl.args {
            if self.args.contains_key(arg_name) {
                if self.args.get(arg_name).unwrap().ty() != arg_decl.ty {
                    return downgrade_handle(Label::new(cx, "Error: mismatched types"));
                }
            } else if let Some(arg_default) = &arg_decl.default {
                self.args.insert(arg_name.clone(), arg_default.clone());
            }
        }
        if !decl.varargs {
            for (arg_name, _) in &self.args {
                if !DEFAULT_ARGS.contains(&arg_name.as_str()) && !decl.args.contains_key(arg_name) {
                    return downgrade_handle(Label::new(cx, "Error: unexpected arg"));
                }
            }
        }

        match &decl.body {
            // if it's defined in xml, we need to evaluate its body
            ViewBody::Xml(_) => {
                apply_default_args(downgrade_handle(QuickflexView { name: self.name.clone() }.build(cx, |cx| {
                    let body = {
                        let store = cx.data::<Store>().unwrap().views.clone();
                        let store_b = store.borrow();
                        let decl = store_b.get(&self.name).unwrap_or_else(|| panic!("view not registered: {}", &self.name));
                        if let ViewBody::Xml(body) = &decl.body { body.clone() } else { unreachable!() }
                    };
                    if let Err(e) = build_body(cx, &body, &self.args) {
                        Label::new(cx, &format!("Error: {}", e));
                    }
                })), &self.args)
            }
            // if it's defined in rust, we need to just run the function. bye!
            ViewBody::Manual(func) => {
                apply_default_args(func(cx, &self.args), &self.args)
            }
        }
    }
}

fn build_body(cx: &mut Context, body: &Vec<Node>, args: &HashMap<String, ArgVal>) -> Result<(), String> {
    for child in body {
        match child {
            Node::Const(const_node) => {
                let store = cx.data::<Store>().unwrap().views.clone();
                let store_b = store.borrow();
                let child_decl = store_b.get(&const_node.name).unwrap_or_else(|| panic!("view not registered: {}", &const_node.name));
                let mut new_builder = Builder::new(&child_decl.name);
                for (arg_name, arg_decl) in child_decl.args.iter() {
                    // if the arg is not found in the tree, leave it unpopulated. there may be a default.
                    match arg_decl.loc {
                        ArgLoc::Attr => {
                            if let Some(attr_val) = const_node.attrs.get(arg_name) {
                                match attr_val {
                                    Leaf::Const(c) => {
                                        match arg_decl.ty.parse_const(c) {
                                            Ok(value) => {
                                                new_builder = new_builder.arg(arg_name, value);
                                            }
                                            Err(e) => {
                                                return Err(format!("could not parse constant: {}", e));
                                            }
                                        }
                                    }
                                    Leaf::Arg(a) => {
                                        if let Some(value) = args.get(a) {
                                            if value.ty() == arg_decl.ty {
                                                new_builder = new_builder.arg(arg_name, value.clone());
                                            } else {
                                                return Err(format!("bad type of quickflex variable"));
                                            }
                                        } else {
                                            return Err(format!("name not found: {}", a));
                                        }
                                    }
                                }
                            } else {
                                return Err(format!("Missing attribute {}", arg_name))
                            }
                        }
                        ArgLoc::NamedChild => {
                            if let Child::Children(children) = &const_node.children {
                                for named_child in children.iter() {
                                    if let Node::Const(const_named_child) = named_child {
                                        if &const_named_child.name == arg_name {
                                            // we found the node which contains our shit. now what?
                                            // depends on if our arg is int/str/float or a view
                                            let a = extract_arg_from_const_node(const_named_child, arg_decl, args)?;
                                            new_builder = new_builder.arg(arg_name, a);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                        ArgLoc::UnnamedChild => {
                            let a = extract_arg_from_const_node(const_node, arg_decl, args)?;
                            new_builder = new_builder.arg(arg_name, a);
                        }
                    }
                }
                // collect default args
                if let Some(attr_val) = const_node.attrs.get("id") {
                    match attr_val {
                        Leaf::Const(s) => {
                            new_builder = new_builder.arg("id", ArgVal::String(DynableValue::Const(s.clone())));
                        }
                        Leaf::Arg(_) => {
                            return Err(format!("id must be a constant"));
                        }
                    }
                }
                if let Some(attr_val) = const_node.attrs.get("class") {
                    match attr_val {
                        Leaf::Const(s) => {
                            new_builder = new_builder.arg("class", ArgVal::String(DynableValue::Const(s.clone())));
                        }
                        Leaf::Arg(_) => {
                            return Err(format!("class must be a constant"));
                        }
                    }
                }
                // collect varargs
                if child_decl.varargs {
                    for (key, val) in &const_node.attrs {
                        if DEFAULT_ARGS.contains(&key.as_str()) || child_decl.args.contains_key(key) {
                            continue;
                        }
                        match val {
                            Leaf::Const(s) => {
                                new_builder = new_builder.arg(key, ArgVal::String(DynableValue::Const(s.clone())));
                            }
                            Leaf::Arg(a) => {
                                match args.get(a) {
                                    None => return Err(format!("undefined variable: {}", a)),
                                    Some(v) => new_builder = new_builder.arg(key, v.clone())
                                }
                            }
                        }
                    }
                }
                // build the builder
                new_builder.build(cx);
            }
            Node::Var(name) => {
                match args.get(name) {
                    Some(ArgVal::View(DynableView::Const(DynableViewConst(body2, closure)))) => {
                        build_body(cx, body2, closure)?;
                    }
                    Some(ArgVal::View(DynableView::Dyn(func))) => {
                        func(cx);
                    }
                    _ => {
                        return Err(format!("bad type of quickflex variable, expected view"));
                    }
                }
            }
        }
    }

    Ok(())
}

fn extract_arg_from_const_node(node: &ConstNode, arg_decl: &ArgDecl, args: &HashMap<String, ArgVal>) -> Result<ArgVal, String> {
    match &node.children {
        Child::Children(child) => {
            if matches!(arg_decl.ty, ArgType::View) {
                Ok(ArgVal::View(DynableView::Const(DynableViewConst(child.clone(), args.clone()))))
            } else {
                Err(format!("Expected view, found leaf"))
            }
        }
        Child::Leaf(leaf) => {
            match leaf {
                Leaf::Const(c) => arg_decl.ty.parse_const(&c),
                Leaf::Arg(a) => args.get(a).cloned().ok_or_else(|| format!("Undefined variable: {}", a)),
            }
        }
    }
}

fn downgrade_handle<T>(h: Handle<T>) -> Handle<()> {
    Handle {
        entity: h.entity,
        p: Default::default(),
        cx: h.cx,
    }
}

impl<T: 'static + Clone> Res<T> for &DynableValue<T> {
    fn get_val(&self, cx: &Context) -> T {
        match self {
            DynableValue::Dyn(l) => l.get_val(cx),
            DynableValue::Const(t) => t.clone(),
        }
    }

    fn get_val_fallible(&self, cx: &Context) -> Option<T> {
        match self {
            DynableValue::Dyn(l) => l.get_val_fallible(cx),
            DynableValue::Const(t) => Some(t.clone()),
        }
    }

    fn set_or_bind<F>(&self, cx: &mut Context, entity: Entity, closure: F) where F: 'static + Clone + Fn(&mut Context, Entity, T) {
        match self {
            DynableValue::Dyn(l) => {
                let l = l.make_clone();
                let l2 = l.make_clone();
                l.bind(cx, Box::new(move |cx| closure(cx, entity, l2.get_val(cx))))
            },
            DynableValue::Const(t) => closure(cx, entity, t.clone()),
        }
    }
}

impl<T: Clone> Clone for DynableValue<T> {
    fn clone(&self) -> Self {
        match self {
            DynableValue::Const(t) => DynableValue::Const(t.clone()),
            DynableValue::Dyn(l) => DynableValue::Dyn(l.make_clone()),
        }
    }
}

impl DynableView {
    pub fn build(&self, cx: &mut Context) {
        match self {
            DynableView::Const(DynableViewConst(body, closure)) => match build_body(cx, body.as_ref(), closure) {
                Ok(_) => {}
                Err(e) => {
                    Label::new(cx, &format!("Error: {}", e));
                },
            },
            DynableView::Dyn(c) => {
                c(cx);
            }
        }
    }
}

pub fn vizia_views(store: &mut HashMap<String, ViewDecl>) {
    store.insert("button".to_owned(), ViewDecl {
        name: "button".to_owned(),
        args: HashMap::from([
            ("action".to_owned(), ArgDecl {
                name: "action".to_owned(),
                ty: ArgType::Callback,
                loc: ArgLoc::Attr,
                default: Some(ArgVal::Callback(DynableCallback::Null)),
            }),
            ("label".to_owned(), ArgDecl {
                name: "label".to_owned(),
                ty: ArgType::View,
                loc: ArgLoc::UnnamedChild,
                default: None,
            })
        ]),
        varargs: false,
        body: ViewBody::Manual(Box::new(|cx, args| {
            let action = args["action"].get_callback().clone();
            apply_default_args(downgrade_handle(Button::new(cx, move |cx| action.call(cx), move |cx| {
                HStack::new(cx, |cx| args["label"].get_view().build(cx))
            })), args)
        })),
    });
    store.insert("label".to_owned(), ViewDecl {
        name: "label".to_owned(),
        args: HashMap::from([
            ("text".to_owned(), ArgDecl {
                name: "text".to_owned(),
                ty: ArgType::String,
                loc: ArgLoc::UnnamedChild,
                default: Some(ArgVal::String(DynableValue::Const("".to_owned()))),
            })
        ]),
        varargs: false,
        body: ViewBody::Manual(Box::new(|cx, args| {
            apply_default_args(downgrade_handle(Label::new(cx, args["text"].get_string())), args)
        })),
    });
    store.insert("llabel".to_owned(), ViewDecl {
        name: "llabel".to_owned(),
        args: HashMap::from([
            ("key".to_owned(), ArgDecl {
                name: "key".to_owned(),
                ty: ArgType::String,
                loc: ArgLoc::Attr,
                default: None,
            })
        ]),
        varargs: true,
        body: ViewBody::Manual(Box::new(|cx, args| {
            let s = args.get("key").unwrap().get_string();
            let key = match s {
                DynableValue::Const(key) => key,
                DynableValue::Dyn(_) => return downgrade_handle(Label::new(cx, "Error: cannot specify llabel key as variable")),
            };
            let mut res = Localized::new(key);
            for (arg, val) in args {
                match val {
                    ArgVal::String(sv) => match sv {
                        DynableValue::Const(sc) => res = res.arg_const(arg, sc.clone()),
                        DynableValue::Dyn(sd) => res = sd.loc_arg(arg, res),
                    }
                    ArgVal::Int(sv) => match sv {
                        DynableValue::Const(sc) => res = res.arg_const(arg, sc.clone()),
                        DynableValue::Dyn(sd) => res = sd.loc_arg(arg, res),
                    }
                    ArgVal::Float(sv) => match sv {
                        DynableValue::Const(sc) => res = res.arg_const(arg, sc.clone()),
                        DynableValue::Dyn(sd) => res = sd.loc_arg(arg, res),
                    }
                    ArgVal::View(_) | ArgVal::Callback(_) => return downgrade_handle(Label::new(cx, "Error: cannot pass view or callback to llabel")),
                }
            }

            apply_default_args(downgrade_handle(Label::new(cx, res)), args)
        })),
    });
    store.insert("hstack".to_owned(), ViewDecl {
        name: "hstack".to_owned(),
        args: HashMap::from([
            ("children".to_owned(), ArgDecl {
                name: "children".to_owned(),
                ty: ArgType::View,
                loc: ArgLoc::UnnamedChild,
                default: None,
            })
        ]),
        varargs: false,
        body: ViewBody::Manual(Box::new(|cx, args| {
            apply_default_args(downgrade_handle(HStack::new(cx, move |cx| args["children"].get_view().build(cx))), args)
        })),
    });
    store.insert("vstack".to_owned(), ViewDecl {
        name: "vstack".to_owned(),
        args: HashMap::from([
            ("children".to_owned(), ArgDecl {
                name: "children".to_owned(),
                ty: ArgType::View,
                loc: ArgLoc::UnnamedChild,
                default: None,
            })
        ]),
        varargs: false,
        body: ViewBody::Manual(Box::new(|cx, args| {
            apply_default_args(downgrade_handle(VStack::new(cx, move |cx| args["children"].get_view().build(cx))), args)
        })),
    });
}

fn apply_default_args<'a, T>(mut handle: Handle<'a, T>, args: &HashMap<String, ArgVal>) -> Handle<'a, T> {
    if let Some(arg) = args.get("id") {
        let id = arg.get_string().get_val(handle.cx);
        handle = handle.id(&id);
    }
    if let Some(arg) = args.get("class") {
        let val = arg.get_string().get_val(handle.cx);
        for cls in val.split_whitespace() {
            handle = handle.class(cls);
        }
    }
    handle
}
