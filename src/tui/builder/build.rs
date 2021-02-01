//! Builder views

use crate::tui::builder::bind::Binder;
use crate::tui::builder::merge::BoxedMergeable;
use cursive::direction::Direction;
use cursive::event::{AnyCb, Event, EventResult};
use cursive::view::{Selector, ViewNotFound};
use cursive::views::LastSizeView;
use cursive::{Printer, Rect, View, XY};
use std::ops::{Deref, DerefMut};

pub trait BuilderFn<S>: Fn(&S, &Binder<S>) -> BoxedMergeable + 'static {}

impl<S, FN> BuilderFn<S> for FN where FN: Fn(&S, &Binder<S>) -> BoxedMergeable + 'static {}

/// Rebuildable view with associated state.
///
/// A `Builder` is a view that holds state and can be rebuilt by calling a
/// builder function, which generates a new view hierarchy to display.
pub struct Builder<S> {
    /// The Cursive-accessible name for this builder.
    name: String,

    /// The view state.
    state: S,

    /// The builder function that compiles the view state into a view.
    ///
    /// This is boxed, predominantly so that code that wants to interact with
    /// builder state can do so via `Cursive.call_on_name` and the like.
    builder: Box<dyn BuilderFn<S>>,

    /// The current cached view, generated from state.
    content: LastSizeView<BoxedMergeable>,

    /// The last generated copy of the builder state, free from any
    /// modifications that may have been made by the widget toolkit.
    old: BoxedMergeable,
}

impl<S> Builder<S>
where
    S: PartialEq + Clone,
{
    /// Construct a new builder view around a given function.
    ///
    /// `name` should be a string unique to this view builder for the duration
    /// of it's execution.
    ///
    /// This is only viable if your choice of state structure provides an
    /// initial default state. Otherwise, to provide one, use
    /// `from_state_and_builder`.
    #[allow(dead_code)]
    pub fn new<B>(name: &str, builder: B) -> Self
    where
        S: Default,
        B: BuilderFn<S> + 'static,
    {
        let state = Default::default();

        Self::from_state_and_builder(name, state, builder)
    }

    /// Construct a new builder view around a given function and initial state.
    ///
    /// `name` should be a string unique to this view builder for the duration
    /// of it's execution.
    pub fn from_state_and_builder<B>(name: &str, state: S, builder: B) -> Self
    where
        B: BuilderFn<S> + 'static,
    {
        let binder = Binder::new(name);
        let content = LastSizeView::new(builder(&state, &binder));
        let old = builder(&state, &binder);

        Self {
            name: name.to_string(),
            state,
            builder: Box::new(builder),
            content,
            old,
        }
    }

    /// Rebuild the view.
    fn rebuild(&mut self) {
        let binder = Binder::new(&self.name);
        let new = (self.builder)(&self.state, &binder);
        let to = (self.builder)(&self.state, &binder);

        if let Some(v) = self
            .content
            .view
            .deref_mut()
            .merge(self.old.deref(), to.unwrap())
        {
            self.content.view = BoxedMergeable::new(v);
        }

        self.old = new;
    }

    /// Get the state data for mutation.
    ///
    /// Once the state data has been mutated, the view will be rebuilt, and the
    /// `Cursive` session flagged for a redraw.
    pub fn with_state_mut<M>(&mut self, mutator: M)
    where
        M: FnOnce(&mut S),
    {
        let old_state = self.state.clone();

        mutator(&mut self.state);

        if old_state != self.state {
            self.rebuild();
        }
    }
}

impl<S> View for Builder<S>
where
    Self: 'static,
{
    fn draw(&self, printer: &Printer) {
        self.content.draw(printer)
    }

    fn layout(&mut self, size: XY<usize>) {
        self.content.layout(size)
    }

    fn needs_relayout(&self) -> bool {
        self.content.needs_relayout()
    }

    fn required_size(&mut self, constraint: XY<usize>) -> XY<usize> {
        self.content.required_size(constraint)
    }

    fn on_event(&mut self, event: Event) -> EventResult {
        self.content.on_event(event)
    }

    fn call_on_any<'a>(&mut self, selector: &Selector, then: AnyCb<'a>) {
        self.content.call_on_any(selector, then)
    }

    fn focus_view(&mut self, selector: &Selector) -> Result<(), ViewNotFound> {
        self.content.focus_view(selector)
    }

    fn take_focus(&mut self, source: Direction) -> bool {
        self.content.take_focus(source)
    }

    fn important_area(&self, view_size: XY<usize>) -> Rect {
        self.content.important_area(view_size)
    }

    fn type_name(&self) -> &'static str {
        self.content.type_name()
    }
}
