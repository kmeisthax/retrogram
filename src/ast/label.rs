//! Label AST type

use std::{fmt, str};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Label {
    /// Name of the label.
    name: String,

    /// Name of the parent label (if any).
    /// If None, then the label is global.
    parent_name: Option<String>,

    /// TRUE if the label is auto-generated, FALSE if the label came from user
    /// input
    is_autogen: bool,
}

impl Label {
    pub fn new(name: &str, parent_name: Option<&str>) -> Label {
        Label {
            name: name.to_string(),
            parent_name: parent_name.map(|s| s.to_string()),
            is_autogen: false,
        }
    }

    pub fn new_placeholder(name: &str, parent_name: Option<&str>) -> Label {
        Label {
            name: name.to_string(),
            parent_name: parent_name.map(|s| s.to_string()),
            is_autogen: true,
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

    pub fn is_placeholder(&self) -> bool {
        self.is_autogen
    }
}

impl str::FromStr for Label {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(".");
        let maybe_parent = split.next();
        let maybe_child = split.next();
        let maybe_autogen_flag = s.get(0..2);

        match (maybe_autogen_flag, maybe_parent, maybe_child) {
            (Some("!!"), Some(parent), Some(child)) => Ok(Label {
                name: child.to_string(),
                parent_name: Some(parent.get(2..).expect("I expect that there's at least 2 characters in a string that matched two of them").to_string()),
                is_autogen: true
            }),
            (Some("!!"), Some(parent), None) => Ok(Label {
                name: parent.get(2..).expect("I expect that there's at least 2 characters in a string that matched two of them").to_string(),
                parent_name: None,
                is_autogen: true
            }),
            (_, Some(parent), Some(child)) => Ok(Label {
                name: child.to_string(),
                parent_name: Some(parent.to_string()),
                is_autogen: false
            }),
            (_, Some(parent), None) => Ok(Label {
                name: parent.to_string(),
                parent_name: None,
                is_autogen: false
            }),
            _ => Err(())
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_autogen {
            write!(f, "!!")?;
        }

        if let Some(ref parent_name) = self.parent_name {
            write!(f, "{}.", parent_name)?;
        }

        write!(f, "{}", self.name)
    }
}

derive_deserialize_from_str!(Label, "valid label");
derive_serialize_from_display!(Label);
