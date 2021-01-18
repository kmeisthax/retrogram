//! Builder views

use cursive::direction::Direction;
use cursive::event::{AnyCb, Event, EventResult};
use cursive::view::Selector;
use cursive::views::{BoxedView, LastSizeView};
use cursive::{Printer, Rect, View, XY};

pub trait BuilderFn<S>: Fn(&S) -> BoxedView + 'static {}

impl<S, FN> BuilderFn<S> for FN where FN: Fn(&S) -> BoxedView + 'static {}

/// Rebuildable view with associated state.
///
/// A `Builder` is a view that holds state and can be rebuilt by calling a
/// builder function, which generates a new view hierarchy to display.
pub struct Builder<S> {
    /// The view state.
    state: S,

    /// The builder function that compiles the view state into a view.
    ///
    /// This is boxed, predominantly so that code that wants to interact with
    /// builder state can do so via `Cursive.call_on_name` and the like.
    builder: Box<dyn BuilderFn<S>>,

    /// The current cached view, generated from state.
    content: LastSizeView<BoxedView>,
}

impl<S> Builder<S>
where
    S: PartialEq + Clone,
{
    /// Construct a new builder view around a given function.
    ///
    /// This is only viable if your choice of state structure provides an
    /// initial default state. Otherwise, to provide one, use
    /// `from_state_and_builder`.
    #[allow(dead_code)]
    pub fn new<B>(builder: B) -> Self
    where
        S: Default,
        B: BuilderFn<S> + 'static,
    {
        let state = Default::default();

        Self::from_state_and_builder(state, builder)
    }

    /// Construct a new builder view around a given function and initial state.
    pub fn from_state_and_builder<B>(state: S, builder: B) -> Self
    where
        B: BuilderFn<S> + 'static,
    {
        let content = LastSizeView::new(builder(&state));

        Self {
            state,
            builder: Box::new(builder),
            content,
        }
    }

    /// Rebuild the view.
    fn rebuild(&mut self) {
        self.content = LastSizeView::new((&self.builder)(&self.state));
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

    fn focus_view(&mut self, selector: &Selector) -> Result<(), ()> {
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
