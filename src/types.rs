use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use vizia::*;

pub type Callback = Rc<Box<dyn 'static + Fn(&mut Context)>>;
pub type BuildCallback = Rc<Box<dyn 'static + Fn(&mut Context)>>;

pub struct Store {
    pub(crate) views: Rc<RefCell<HashMap<String, ViewDecl>>>
}

impl Store {
    pub fn new(views: HashMap<String, ViewDecl>) -> Self {
        Self { views: Rc::new(RefCell::new(views)) }
    }
}

impl Model for Store { }

pub struct Builder {
    pub(crate) name: String,
    pub(crate) args: HashMap<String, ArgVal>,
}

pub struct QuickflexView {
    pub(crate) name: String,
}

impl View for QuickflexView {
    fn element(&self) -> Option<String> {
        Some(self.name.clone())
    }
}

pub(crate) enum Leaf {
    Const(String),
    Arg(String),
}

// a quickflex xml file, parsed
pub struct ViewDecl {
    pub(crate) name: String,
    pub(crate) args: HashMap<String, ArgDecl>,
    pub(crate) varargs: bool,
    pub(crate) body: ViewBody,
}

pub(crate) enum ViewBody {
    Xml(Rc<Vec<Node>>),
    Manual(Box<dyn for<'a> Fn(&'a mut Context, &HashMap<String, ArgVal>) -> Handle<'a, ()>>),
}


// The children of a node in a body declaration. Can either be a list of nodes or a leaf (a string)
pub(crate) enum Child {
    Children(Rc<Vec<Node>>),
    Leaf(Leaf),
}

// An internal node in a body declaration. can specify either a constant tree or a variable to interpolate
pub(crate) enum Node {
    Const(ConstNode),
    Var(String),
}

pub(crate) struct ConstNode {
    pub(crate) name: String,
    pub(crate) attrs: HashMap<String, Leaf>,
    pub(crate) children: Child,
}

// An arg declaration, i.e. an <arg> element directly under <view>
pub(crate) struct ArgDecl {
    pub(crate) name: String,
    pub(crate) ty: ArgType,
    pub(crate) loc: ArgLoc,
    pub(crate) default: Option<ArgVal>
}

// The types an arg can be declared as
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ArgType {
    String,
    Int,
    Float,
    View,
    Callback,
}

impl ArgType {
    pub fn parse_const(&self, text: &str) -> Result<ArgVal, String> {
        Ok(match self {
            ArgType::String => ArgVal::String(DynableValue::Const(text.to_owned())),
            ArgType::Int => ArgVal::Int(DynableValue::Const(text.parse().map_err(|e: std::num::ParseIntError| e.to_string())?)),
            ArgType::Float => ArgVal::Float(DynableValue::Const(text.parse().map_err(|e: std::num::ParseFloatError| e.to_string())?)),
            ArgType::View => ArgVal::View(text.parse()?),
            ArgType::Callback => ArgVal::Callback(text.parse()?),
        })
    }
}

// An instantiation of an arg value
#[derive(Clone)]
pub enum ArgVal {
    String(DynableValue<String>),
    Int(DynableValue<i32>),
    Float(DynableValue<f32>),
    View(DynableView),
    Callback(DynableCallback)
}

impl ArgVal {
    pub fn ty(&self) -> ArgType {
        match self {
            ArgVal::String(_) => ArgType::String,
            ArgVal::Int(_) => ArgType::Int,
            ArgVal::Float(_) => ArgType::Float,
            ArgVal::View(_) => ArgType::View,
            ArgVal::Callback(_) => ArgType::Callback,
        }
    }

    pub fn get_string(&self) -> &DynableValue<String> {
        match self {
            ArgVal::String(d) => d,
            _ => panic!("get_string called on non-string"),
        }
    }

    pub fn get_int(&self) -> &DynableValue<i32> {
        match self {
            ArgVal::Int(d) => d,
            _ => panic!("get_int called on non-int"),
        }
    }

    pub fn get_float(&self) -> &DynableValue<f32> {
        match self {
            ArgVal::Float(d) => d,
            _ => panic!("get_float called on non-float"),
        }
    }

    pub fn get_view(&self) -> &DynableView {
        match self {
            ArgVal::View(d) => d,
            _ => panic!("get_view called on non-view"),
        }
    }

    pub fn get_callback(&self) -> &DynableCallback {
        match self {
            ArgVal::Callback(d) => d,
            _ => panic!("get_callback called on non-callback"),
        }
    }
}

pub enum DynableValue<T> {
    Const(T),
    Dyn(Box<dyn LensWrapSmallerTrait<T>>),
}

#[derive(Clone)]
pub enum DynableView {
    Const(DynableViewConst),
    Dyn(BuildCallback),
}

#[derive(Clone)]
pub struct DynableViewConst(pub(crate) Rc<Vec<Node>>, pub(crate) HashMap<String, ArgVal>);

#[derive(Clone)]
pub enum DynableCallback {
    Null,
    Dyn(Callback),
}

impl DynableCallback {
    pub fn call(&self, cx: &mut Context) {
        if let DynableCallback::Dyn(c) = self {
            c(cx);
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct LensWrapSmaller<L> {
    pub(crate) lens: L,
}

pub trait LensWrapSmallerTrait<T> {
    fn get_val(&self, cx: &Context) -> T;
    fn get_val_fallible(&self, cx: &Context) -> Option<T>;
    fn make_clone(&self) -> Box<dyn LensWrapSmallerTrait<T>>;
    fn bind(&self, cx: &mut Context, closure: Box<dyn Fn(&mut Context)>);
    fn loc_arg(&self, key: &str, loc: Localized) -> Localized where T: Into<FluentValue<'static>>;
}

impl<T: Data, L: Lens<Target = T>> LensWrapSmallerTrait<T> for LensWrapSmaller<L> {
    fn get_val(&self, cx: &Context) -> T {
        self.lens.get(cx)
    }

    fn get_val_fallible(&self, cx: &Context) -> Option<T> {
        self.lens.get_fallible(cx)
    }

    fn make_clone(&self) -> Box<dyn LensWrapSmallerTrait<T>> {
        Box::new(self.clone())
    }

    fn bind(&self, cx: &mut Context, closure: Box<dyn Fn(&mut Context)>) {
        Binding::new(cx, self.lens.clone(), move |cx, _| closure(cx));
    }

    fn loc_arg(&self, key: &str, loc: Localized) -> Localized where T: Into<FluentValue<'static>> {
        loc.arg(key, self.lens.clone())
    }
}

// How args should be picked out of the tree
pub enum ArgLoc {
    Attr,
    NamedChild,
    UnnamedChild,
}
