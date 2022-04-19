use vizia::*;
use vizia_quickflex::*;

#[derive(Lens)]
struct AppState {
    ok_count: i32,
    cancel_count: i32,
}

impl Model for AppState {
    fn event(&mut self, _cx: &mut Context, event: &mut Event) {
        if let Some(msg) = event.message.downcast() {
            match msg {
                AppEvent::Ok => self.ok_count += 1,
                AppEvent::Cancel => self.cancel_count += 1,
            }
        }
    }
}

#[derive(Debug)]
enum AppEvent {
    Ok,
    Cancel,
}

fn main() {
    Application::new(WindowDescription::new(), |cx| {
        cx.add_theme(include_str!("style.css"));
        cx.add_translation("en-US".parse().unwrap(), include_str!("main.ftl").to_owned());
        let mut views = std::collections::HashMap::from([
            ("main".to_owned(), parse_document(include_str!("main.xml")).unwrap()),
            ("prompt".to_owned(), parse_document(include_str!("prompt.xml")).unwrap()),
        ]);
        vizia_views(&mut views);
        Store::new(views).build(cx);

        AppState {
            ok_count: 0,
            cancel_count: 0,
        }.build(cx);

        Builder::new("main")
            .arg_dyn_int("count-ok", AppState::ok_count)
            .arg_dyn_int("count-cancel", AppState::cancel_count)
            .arg_dyn_callback("on-ok", |cx| cx.emit(AppEvent::Ok))
            .arg_dyn_callback("on-cancel", |cx| cx.emit(AppEvent::Cancel))
            .arg_dyn_view("back-to-rust", |cx| {
                Label::new(cx, "I made it back to rust!");
            })
            .build(cx)
            .id("main");
    }).run();
}
