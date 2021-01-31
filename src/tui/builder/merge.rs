//! Mergeable view trait

use cursive::view::{View, ViewWrapper};
use cursive::views::{BoxedView, Dialog, EditView, LinearLayout, TextView};
use std::any::Any;
use std::cmp::{max, min};
use std::ops::{Deref, DerefMut};

pub trait Mergeable: View {
    /// Apply the changes embodied in the `from` and `to` views to this view.
    ///
    /// This is a three-way merge, it's implied that this particular view is
    /// also a descendent state of the `from` view. Properties that differ in
    /// both `self` and `to` should be resolved in favor of `to`.
    ///
    /// The implementation of `Mergeable` is free to either copy state updates
    /// from `to` into it`self`, copy it's own state into `to` and return it
    /// back, or construct a new view and return that. Callers of `merge` must
    /// replace the called element with the return value if this function
    /// returns `Some`.
    ///
    /// The most trivial implementation of `merge` would be to just return
    /// `to`. This is suitable for elements that hold no view state; and is
    /// also the default behavior for child views which are not `Mergeable`.
    /// The trivial behavior should also occur if `self`, `from` and `to` do
    /// not have the same type.
    fn merge(&mut self, from: &dyn Mergeable, to: Box<dyn Mergeable>)
        -> Option<Box<dyn Mergeable>>;

    /// Erase the mergeable type information off a box.
    fn upcast_to_view(self: Box<Self>) -> Box<dyn View>;
}

impl dyn Mergeable {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }

    #[allow(dead_code)]
    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        self.as_any_mut().downcast_mut()
    }

    pub fn downcast<T: Any>(self: Box<Self>) -> Result<Box<T>, Box<Self>> {
        if self.as_any().is::<T>() {
            Ok(self.as_boxed_any().downcast().unwrap())
        } else {
            Err(self)
        }
    }
}

impl Mergeable for EditView {
    fn merge(
        &mut self,
        from: &dyn Mergeable,
        to: Box<dyn Mergeable>,
    ) -> Option<Box<dyn Mergeable>> {
        if let (Some(from), Some(to)) = (from.downcast_ref::<Self>(), to.downcast_ref::<Self>()) {
            if from.get_content() != to.get_content() {
                self.set_content(to.get_content().as_ref().to_string());
            }

            None
        } else {
            Some(to)
        }
    }

    fn upcast_to_view(self: Box<Self>) -> Box<dyn View> {
        self
    }
}

impl Mergeable for LinearLayout {
    fn merge(
        &mut self,
        from: &dyn Mergeable,
        to: Box<dyn Mergeable>,
    ) -> Option<Box<dyn Mergeable>> {
        match (from.downcast_ref::<Self>(), to.downcast::<Self>()) {
            (_, Err(to)) => Some(to),
            (None, Ok(to)) => Some(to),
            (Some(from), Ok(mut to)) => {
                let old_focus_index = self.get_focus_index();
                let self_children = self.len();
                let from_children = from.len();
                let to_children = to.len();

                //First, try to merge children that exist in all three layouts.
                let mergeable_children = min(self_children, min(from_children, to_children));
                for i in 0..mergeable_children {
                    let self_child = self.get_child_mut(i).unwrap();
                    let from_child = from.get_child(i).unwrap();
                    let to_child = to.remove_child(0).unwrap();

                    match (
                        as_mergeable_view_mut(self_child),
                        as_mergeable_view(from_child),
                        into_mergeable_view(to_child),
                    ) {
                        (_, _, Err(unmergeable_to)) => {
                            self.remove_child(i);
                            self.insert_child(i, unmergeable_to);
                        }
                        (None, _, Ok(mergeable_to)) | (_, None, Ok(mergeable_to)) => {
                            self.remove_child(i);
                            self.insert_child(i, mergeable_to.upcast_to_view());
                        }
                        (Some(mergeable_self), Some(mergeable_from), Ok(mergeable_to)) => {
                            if let Some(to) = mergeable_self.merge(mergeable_from, mergeable_to) {
                                self.remove_child(i);
                                self.insert_child(i, to.upcast_to_view());
                            }
                        }
                    }
                }

                for i in mergeable_children..max(self_children, to_children) {
                    let to_child = to.remove_child(0);

                    self.remove_child(i);
                    if let Some(to_child) = to_child {
                        self.insert_child(i, to_child);
                    }
                }

                if self.set_focus_index(old_focus_index).is_err() {
                    //do nothing
                }

                None
            }
        }
    }

    fn upcast_to_view(self: Box<Self>) -> Box<dyn View> {
        self
    }
}

impl Mergeable for Dialog {
    fn merge(
        &mut self,
        from: &dyn Mergeable,
        to: Box<dyn Mergeable>,
    ) -> Option<Box<dyn Mergeable>> {
        if let (Some(from), Some(_)) = (from.downcast_ref::<Self>(), to.downcast_ref::<Self>()) {
            //Ok, so this merge is particularly tricky because most of the
            //things we'd want to copy from `to` are write-only properties,
            //which means we need to keep `to` and merge our focus state onto
            //it. However, we also need to merge the underlying content views
            //in standard order. This poses some problems:
            //
            // 1. We can't take `self`'s content to move it into `to`, because
            //    `merge` takes `&mut self`.
            // 2. Taking `to`'s content will delete all of the padding and
            //    title configuration that we wanted
            //
            //So instead we swap the contents of `self` and `to`'s boxes so
            //that they hold nominal views we don't care about. We then merge
            //the now-owned content and move it into `to`, and then return that
            //as our new merged view.

            let mut self_content = self.replace_content(TextView::new("test"));

            let mut boxed_to = if let Ok(to) = to.downcast::<Self>() {
                to
            } else {
                unreachable!();
            };
            let boxed_to_content = boxed_to.replace_content(TextView::new("test_to")).unwrap();

            if let (Some(inner_self_content), Some(from_content), Some(_)) = (
                as_mergeable_view_mut(self_content.deref_mut()),
                as_mergeable_view(from.get_content()),
                as_mergeable_view(boxed_to_content.deref()),
            ) {
                let inner_to_content = if let Ok(to_content) = into_mergeable_view(boxed_to_content)
                {
                    to_content
                } else {
                    unreachable!();
                };

                if let Some(to) = inner_self_content.merge(from_content, inner_to_content) {
                    self_content = BoxedView::new(to.upcast_to_view());
                }
            }

            boxed_to.set_focus(self.focus());
            boxed_to.set_content(self_content);

            Some(boxed_to)
        } else {
            Some(to)
        }
    }

    fn upcast_to_view(self: Box<Self>) -> Box<dyn View> {
        self
    }
}

/// Given a `View`, check if it's one of the view types we've explicitly marked
/// as mergeable.
///
/// Use of this function is discouraged; it cannot be extended by another View
/// implementing Mergeable due to limitations of `dyn Any`. Instead, containers
/// that are aware of the mergeable status of incoming child views should be
/// used.
fn as_mergeable_view(this: &dyn View) -> Option<&dyn Mergeable> {
    if this.is::<EditView>() {
        if let Some(ev) = this.downcast_ref::<EditView>() {
            return Some(ev);
        }
    } else if this.is::<Dialog>() {
        if let Some(ev) = this.downcast_ref::<Dialog>() {
            return Some(ev);
        }
    } else if this.is::<LinearLayout>() {
        if let Some(ev) = this.downcast_ref::<LinearLayout>() {
            return Some(ev);
        }
    } else if this.is::<BoxedView>() {
        if let Some(bv) = this.downcast_ref::<BoxedView>() {
            return as_mergeable_view(bv.deref());
        }
    } else if this.is::<BoxedMergeable>() {
        if let Some(mv) = this.downcast_ref::<BoxedMergeable>() {
            return Some(mv);
        }
    }

    None
}

/// Given a `View`, check if it's one of the view types we've explicitly marked
/// as mergeable.
///
/// Use of this function is discouraged; it cannot be extended by another View
/// implementing Mergeable due to limitations of `dyn Any`. Instead, containers
/// that are aware of the mergeable status of incoming child views should be
/// used.
fn as_mergeable_view_mut(this: &mut dyn View) -> Option<&mut dyn Mergeable> {
    if this.is::<EditView>() {
        if let Some(ev) = this.downcast_mut::<EditView>() {
            return Some(ev);
        }
    } else if this.is::<Dialog>() {
        if let Some(ev) = this.downcast_mut::<Dialog>() {
            return Some(ev);
        }
    } else if this.is::<LinearLayout>() {
        if let Some(ev) = this.downcast_mut::<LinearLayout>() {
            return Some(ev);
        }
    } else if this.is::<BoxedView>() {
        if let Some(bv) = this.downcast_mut::<BoxedView>() {
            return as_mergeable_view_mut(bv.deref_mut());
        }
    } else if this.is::<BoxedMergeable>() {
        if let Some(mv) = this.downcast_mut::<BoxedMergeable>() {
            return Some(mv);
        }
    }

    None
}

/// Given a boxed `View`, check if it's one of the view types we've explicitly
/// marked as mergeable.
///
/// Use of this function is discouraged; it cannot be extended by another View
/// implementing Mergeable due to limitations of `dyn Any`. Instead, containers
/// that are aware of the mergeable status of incoming child views should be
/// used.
fn into_mergeable_view(this: Box<dyn View>) -> Result<Box<dyn Mergeable>, Box<dyn View>> {
    if this.is::<EditView>() {
        if let Ok(ev) = this.downcast::<EditView>() {
            Ok(ev)
        } else {
            unreachable!();
        }
    } else if this.is::<Dialog>() {
        if let Ok(ev) = this.downcast::<Dialog>() {
            Ok(ev)
        } else {
            unreachable!();
        }
    } else if this.is::<LinearLayout>() {
        if let Ok(ev) = this.downcast::<LinearLayout>() {
            Ok(ev)
        } else {
            unreachable!();
        }
    } else if this.is::<BoxedView>() {
        if let Ok(ev) = this.downcast::<BoxedView>() {
            into_mergeable_view(ev.unwrap())
        } else {
            unreachable!();
        }
    } else if this.is::<BoxedMergeable>() {
        if let Ok(ev) = this.downcast::<BoxedMergeable>() {
            Ok(ev.view)
        } else {
            unreachable!();
        }
    } else {
        Err(this)
    }
}

pub struct BoxedMergeable {
    view: Box<dyn Mergeable>,
}

impl BoxedMergeable {
    pub fn new(view: Box<dyn Mergeable>) -> Self {
        Self { view }
    }

    pub fn boxed<T>(view: T) -> Self
    where
        T: IntoBoxedMergeable,
    {
        Self::new(view.into_boxed_mergeable())
    }

    pub fn unwrap(self) -> Box<dyn Mergeable> {
        self.view
    }
}

impl Deref for BoxedMergeable {
    type Target = dyn Mergeable;

    fn deref(&self) -> &dyn Mergeable {
        &*self.view
    }
}

impl DerefMut for BoxedMergeable {
    fn deref_mut(&mut self) -> &mut dyn Mergeable {
        &mut *self.view
    }
}

impl ViewWrapper for BoxedMergeable {
    type V = dyn Mergeable;

    fn with_view<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&Self::V) -> R,
    {
        Some(f(&*self.view))
    }

    fn with_view_mut<F, R>(&mut self, f: F) -> Option<R>
    where
        F: FnOnce(&mut Self::V) -> R,
    {
        Some(f(&mut *self.view))
    }
}

impl Mergeable for BoxedMergeable {
    fn merge(
        &mut self,
        from: &dyn Mergeable,
        to: Box<dyn Mergeable>,
    ) -> Option<Box<dyn Mergeable>> {
        //Strictly speaking we should just proxy to `self.view`, but the `to`
        //parameter is tricky as we don't want to force people to box up their
        //already-boxed mergable views. So we accept both cases.
        if to.downcast_ref::<Self>().is_some() {
            let boxed_to = if let Ok(to) = to.downcast::<Self>() {
                to
            } else {
                unreachable!();
            };

            self.view.merge(from, boxed_to.view)
        } else {
            self.view.merge(from, to)
        }
    }

    fn upcast_to_view(self: Box<Self>) -> Box<dyn View> {
        self
    }
}

pub trait IntoBoxedMergeable {
    fn into_boxed_mergeable(self) -> Box<dyn Mergeable>;
}

impl<T> IntoBoxedMergeable for T
where
    T: Mergeable,
{
    fn into_boxed_mergeable(self) -> Box<dyn Mergeable> {
        Box::new(self)
    }
}

impl IntoBoxedMergeable for Box<dyn Mergeable> {
    fn into_boxed_mergeable(self) -> Box<dyn Mergeable> {
        self
    }
}
