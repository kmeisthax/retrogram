/// Allows conversions into a given masked wrapper type from some other
/// primitive type. The primitive being wrapped will be masked, of course; and
/// we rely on the From implementation of the underlying wrapped type to do the
/// conversion.
macro_rules! masked_conv_impl {
    ($type:ident, $wrap_type:ident, $from_type: ident, $mask:expr) => {
        impl From<$from_type> for $type {
            fn from(t: $from_type) -> Self {
                $type {
                    v: $wrap_type::from(t) & $mask
                }
            }
        }
    }
}

/// Allows falliable conversions into a given masked wrapper type from some
/// other primitive type. The primitive must already be masked and conversion
/// will fail if the value exceeds the mask. We rely on the TryFrom
/// implementation of the underlying wrapped type to do the conversion.
macro_rules! masked_tryconv_impl {
    ($type:ident, $wrap_type:ident, $from_type: ident, $mask:expr) => {
        impl TryFrom<$from_type> for $type {
            type Error = Option<<$wrap_type as TryFrom<$from_type>>::Error>;

            fn try_from(t: $from_type) -> Result<Self, Self::Error> {
                if t > $mask {
                    return Err(None)
                }

                match $wrap_type::try_from(t) {
                    Ok(v) => Ok($type {
                        v: v & $mask
                    }),
                    Err(e) => Err(Some(e))
                }
            }
        }
    }
}

macro_rules! unwrap_impl {
    ($type:ident, $wrapped_type:ident, $into_type:ident) => {
        impl Into<$into_type> for $type {
            fn into(self) -> $into_type {
                self.v.into()
            }
        }
    };
}

macro_rules! try_unwrap_impl {
    ($type:ident, $wrapped_type:ident, $into_type:ident) => {
        impl TryInto<$into_type> for $type {
            type Error = <$wrapped_type as TryInto<$into_type>>::Error;

            fn try_into(self) -> Result<$into_type, Self::Error> {
                self.v.try_into()
            }
        }
    };
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

/// Allows implementing binary operations on a wrapped type.
/// 
/// All implementations are forwarded to the implementation provided by the
/// target primitive, which is assumed to be `self.v`. If an RHS type is
/// provided then the RHS is assumed to be primitive. Otherwise, we attempt to
/// access `rhs.v` to unwrap a second primitive.
/// 
/// This variant of the macro does not declare an Output type.
macro_rules! binary_op_masked_impl_notype {
    ($type:ident, $trait_name:ident, $method_name:ident, $mask:expr) => {
        impl $trait_name for $type {
            fn $method_name(self, rhs: Self) -> Self {
                $type {
                    v: (self.v.$method_name(rhs.v)) & $mask
                }
            }
        }
    };
    ($type:ident, $rhs_type:ident, $trait_name:ident, $method_name:ident, $mask:expr) => {
        impl $trait_name<$rhs_type> for $type {
            fn $method_name(self, rhs: $rhs_type) -> Self {
                $type {
                    v: (self.v.$method_name(rhs)) & $mask
                }
            }
        }
    };
}

/// Allows implementing binary operations on a wrapped type.
/// 
/// All implementations are forwarded to the implementation provided by the
/// target primitive, which is assumed to be `self.v`. If an RHS type is
/// provided then the RHS is assumed to be primitive. Otherwise, we attempt to
/// access `rhs.v` to unwrap a second primitive.
macro_rules! assign_binary_op_masked_impl {
    ($type:ident, $trait_name:ident, $method_name:ident, $mask:expr) => {
        impl $trait_name for $type {
            fn $method_name(&mut self, rhs: Self) {
                self.v.$method_name(rhs.v & $mask);
                self.v &= $mask;
            }
        }
    };
    ($type:ident, $rhs_type:ident, $trait_name:ident, $method_name:ident, $mask:expr) => {
        impl $trait_name<$rhs_type> for $type {
            fn $method_name(&mut self, rhs: $rhs_type) {
                self.v.$method_name(rhs & mask);
                self.v &= $mask;
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
macro_rules! wrap_existing_impl {
    ($trait_name:ident, $method:ident, $t:ty, $rhs:ty, $out:ty) => {
        impl $trait_name for $t {
            #[inline]
            fn $method(self, v: $rhs) -> $out {
                <$t>::$method(self, v)
            }
        }
    }
}