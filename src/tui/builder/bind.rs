//! Auto-binding functionality

use crate::tui::builder::build::Builder;
use cursive::event::Event;
use cursive::Cursive;
use std::marker::PhantomData;

/// A struct which can construct event handlers that update a given view's
/// state.
///
/// `S` is the state type used on the upstream view builder. We do not store
/// state here, but it is part of the struct type regardless.
#[derive(Clone)]
pub struct Binder<S> {
    /// The name of the view that stores the state.
    name: String,

    /// Marker type for `S`.
    state: PhantomData<S>,
}

impl<S> Binder<S> {
    /// Construct a new binder.
    ///
    /// The `name` is the name of the view that holds the bound state.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            state: PhantomData,
        }
    }
}

impl<S> Binder<S>
where
    S: PartialEq + Clone,
{
    /// Construct an event handler that updates state whenever the event
    /// triggers.
    ///
    /// This variant of `bind` is intended to work with event handlers that
    /// accept one borrowed non-Cursive parameter.
    pub fn bind_ref<UPDATE, U: ?Sized>(
        &self,
        update: UPDATE,
    ) -> impl Fn(&mut Cursive, &U) + Clone + 'static
    where
        UPDATE: Fn(&mut S, &U) + Clone + 'static,
        S: 'static,
    {
        let name = self.name.to_string();

        move |siv, q| {
            let update = update.clone();

            siv.call_on_name(&name, move |v: &mut Builder<S>| {
                v.with_state_mut(|v| update(v, q));
            });

            siv.on_event(Event::Refresh);
        }
    }

    /// Construct an event handler that updates state whenever the event
    /// triggers.
    ///
    /// This variant of `bind` is intended to work with event handlers that
    /// accept one owned non-Cursive parameter.
    pub fn bind_owned<UPDATE, U>(
        &self,
        update: UPDATE,
    ) -> impl Fn(&mut Cursive, U) + Clone + 'static
    where
        UPDATE: Fn(&mut S, U) + Clone + 'static,
        S: 'static,
    {
        let name = self.name.to_string();

        move |siv, u| {
            let update = update.clone();

            siv.call_on_name(&name, move |b: &mut Builder<S>| {
                b.with_state_mut(|s| update(s, u));
            });

            siv.on_event(Event::Refresh);
        }
    }

    /// Construct an event handler that updates state whenever the event
    /// triggers.
    ///
    /// This variant of `bind` is intended to work with event handlers that
    /// accept one borrowed non-Cursive parameter followed by one owned.
    pub fn bind_ref_owned<UPDATE, U: ?Sized, V>(
        &self,
        update: UPDATE,
    ) -> impl Fn(&mut Cursive, &U, V) + Clone + 'static
    where
        UPDATE: Fn(&mut S, &U, V) + Clone + 'static,
        S: 'static,
    {
        let name = self.name.to_string();

        move |siv, u, v| {
            let update = update.clone();

            siv.call_on_name(&name, move |b: &mut Builder<S>| {
                b.with_state_mut(|s| update(s, u, v));
            });

            siv.on_event(Event::Refresh);
        }
    }
}
