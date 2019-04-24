//! Tools for declaring guard traits

#[macro_export]
macro_rules! declare_guard_trait {
    ($guard:ty, $basis:expr) => {
        pub trait $guard : $basis {

        }

        impl <T> $guard for T where T: $basis {

        }
    };
    ($guard:ty, $param:expr, $basis: expr) => {
        pub trait $guard<$param> : $basis {

        }

        impl <T, $param> $guard for T where T: $basis {

        }
    }
}