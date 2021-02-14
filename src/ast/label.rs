//! Label AST type

use std::{fmt, str};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Label {
    /// Name of the label.
    name: String,

    /// Name of the parent label (if any).
    /// If None, then the label is global.
    parent_name: Option<String>,
}

impl Label {
    pub fn new(name: &str, parent_name: Option<&str>) -> Label {
        Label {
            name: name.to_string(),
            parent_name: parent_name.map(|s| s.to_string()),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn parent_name(&self) -> Option<&str> {
        if let Some(ref parent_label) = self.parent_name {
            Some(&parent_label)
        } else {
            None
        }
    }
}

impl str::FromStr for Label {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split('.');
        let maybe_parent = split.next();
        let maybe_child = split.next();

        match (maybe_parent, maybe_child) {
            (Some(parent), Some(child)) => Ok(Label {
                name: child.to_string(),
                parent_name: Some(parent.to_string()),
            }),
            (Some(parent), None) => Ok(Label {
                name: parent.to_string(),
                parent_name: None,
            }),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref parent_name) = self.parent_name {
            write!(f, "{}.", parent_name)?;
        }

        write!(f, "{}", self.name)
    }
}

derive_deserialize_from_str!(Label, "valid label");
derive_serialize_from_display!(Label);
