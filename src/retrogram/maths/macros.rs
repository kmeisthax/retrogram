use crate::retrogram::maths::BoundWidth;

/// Allows conversions into a given masked wrapper type when the internal
/// primitive of that wrapper can accept the given type. The primitive being
/// wrapped will be masked, of course.
macro_rules! masked_conv_impl {
    ($type:ident, $wrap_type:ident, $mask:expr) => {
        impl<T> From<T> for $type where $wrap_type: From<T> {
            fn from(t: T) -> Self {
                $type {
                    v: $wrap_type::from(t) & $mask
                }
            }
        }
    }
}

/// Allows implementing binary operations on a wrapped type.
/// 
/// All implementations are forwarded to the implementation provided by the
/// target primitive, which is assumed to be `self.v`. If an RHS type is
/// provided then the RHS is assumed to be primitive. Otherwise, we attempt to
/// access `rhs.v` to unwrap a second primitive.
macro_rules! binary_op_masked_impl {
    ($type:ident, $trait_name:ident, $method_name:ident, $mask:expr) => {
        impl $trait_name for $type {
            type Output = $type;

            fn $method_name(self, rhs: Self) -> Self {
                $type {
                    v: (self.v.$method_name(rhs.v)) & $mask
                }
            }
        }
    };
    ($type:ident, $rhs_type:ident, $trait_name:ident, $method_name:ident, $mask:expr) => {
        impl $trait_name<$rhs_type> for $type {
            type Output = $type;

            fn $method_name(self, rhs: $rhs_type) -> Self {
                $type {
                    v: (self.v.$method_name(rhs)) & $mask
                }
            }
        }
    };
}

/// Declares the bit width of a particular type.
/// 
/// The RHS parameter, as explained for `BoundWidth`, corresponds to the RHS of
/// a given `Shl` implementation. Normally, the bound width is the same for all
/// types.
macro_rules! boundwidth_impl {
    ($t:ty, $rhs:ty, $shifts:expr) => {
        impl crate::retrogram::maths::BoundWidth<$rhs> for $t {
            #[inline]
            fn bound_width() -> $rhs {
                $shifts
            }
        }
    }
}

/// Wraps a given non-trait method in a trait for a type.
macro_rules! checked_impl {
    ($trait_name:ident, $method:ident, $t:ty, $rhs:ty, $out:ty) => {
        impl $trait_name for $t {
            #[inline]
            fn $method(&self, v: &$rhs) -> Option<$out> {
                <$t>::$method(*self, *v)
            }
        }
    }
}