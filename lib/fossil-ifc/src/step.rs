use std::fs;
use std::path::Path;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Token {
    String(String),
    Float(f64),
    Int(i64),
    Enum(String),
    EntityRef(u32),
    List(Vec<Token>),
    Null,
    Derived,
    TypedValue(String, Box<Token>),
}

impl Token {
    pub fn as_string(&self) -> Option<String> {
        match self {
            Token::String(s) => Some(s.clone()),
            Token::TypedValue(_, inner) => inner.as_string(),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Token::Float(f) => Some(*f),
            Token::Int(i) => Some(*i as f64),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn as_enum(&self) -> Option<&str> {
        match self {
            Token::Enum(s) => Some(s),
            Token::TypedValue(_, inner) => inner.as_enum(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct StepEntity {
    pub id: u32,
    pub type_name: String,
    pub attributes: Vec<Token>,
}

impl StepEntity {
    /// Get the attribute at the given 0-based position.
    pub fn get(&self, index: usize) -> Option<&Token> {
        self.attributes.get(index)
    }
}

pub struct StepFile {
    content: String,
}

impl StepFile {
    pub fn open(path: &Path) -> Result<Self, String> {
        let content =
            fs::read_to_string(path).map_err(|e| format!("failed to read IFC file: {}", e))?;
        Ok(Self { content })
    }

    pub fn find_all(&self, type_name: &str) -> Vec<StepEntity> {
        let target = type_name.to_ascii_uppercase();
        let mut results = Vec::new();

        let bytes = self.content.as_bytes();
        let len = bytes.len();
        let mut pos = 0;

        while pos < len {
            match memchr(b'#', bytes, pos) {
                Some(hash_pos) => pos = hash_pos,
                None => break,
            }

            pos += 1; // skip '#'
            let id_start = pos;
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            if pos == id_start {
                continue;
            }
            let id_str = &self.content[id_start..pos];
            let id: u32 = match id_str.parse() {
                Ok(v) => v,
                Err(_) => continue,
            };

            while pos < len && bytes[pos].is_ascii_whitespace() {
                pos += 1;
            }

            if pos >= len || bytes[pos] != b'=' {
                continue;
            }
            pos += 1;

            while pos < len && bytes[pos].is_ascii_whitespace() {
                pos += 1;
            }

            let name_start = pos;
            while pos < len && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_') {
                pos += 1;
            }
            if pos == name_start {
                continue;
            }
            let entity_type = &self.content[name_start..pos];

            while pos < len && bytes[pos].is_ascii_whitespace() {
                pos += 1;
            }

            if pos >= len || bytes[pos] != b'(' {
                pos = skip_to_semicolon(bytes, pos);
                continue;
            }

            if !entity_type.eq_ignore_ascii_case(&target) {
                pos = skip_to_semicolon(bytes, pos);
                continue;
            }

            let (attrs, new_pos) = parse_attribute_list(bytes, &self.content, pos);
            pos = new_pos;

            results.push(StepEntity {
                id,
                type_name: entity_type.to_string(),
                attributes: attrs,
            });
        }

        results
    }
}

fn memchr(needle: u8, bytes: &[u8], start: usize) -> Option<usize> {
    bytes[start..]
        .iter()
        .position(|&b| b == needle)
        .map(|i| i + start)
}

fn skip_to_semicolon(bytes: &[u8], mut pos: usize) -> usize {
    let len = bytes.len();
    while pos < len {
        match bytes[pos] {
            b'\'' => {
                pos += 1;
                while pos < len {
                    if bytes[pos] == b'\'' {
                        pos += 1;
                        // Escaped quote ''
                        if pos < len && bytes[pos] == b'\'' {
                            pos += 1;
                            continue;
                        }
                        break;
                    }
                    pos += 1;
                }
            }
            b';' => {
                pos += 1;
                return pos;
            }
            _ => pos += 1,
        }
    }
    pos
}

fn parse_attribute_list(bytes: &[u8], content: &str, mut pos: usize) -> (Vec<Token>, usize) {
    let len = bytes.len();

    if pos < len && bytes[pos] == b'(' {
        pos += 1;
    }

    let mut attrs = Vec::new();

    loop {
        while pos < len && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }

        if pos >= len {
            break;
        }

        if bytes[pos] == b')' {
            pos += 1;
            break;
        }

        if bytes[pos] == b',' {
            pos += 1;
            continue;
        }

        let (token, new_pos) = parse_token(bytes, content, pos);
        pos = new_pos;
        attrs.push(token);
    }

    (attrs, pos)
}

fn parse_token(bytes: &[u8], content: &str, mut pos: usize) -> (Token, usize) {
    let len = bytes.len();

    while pos < len && bytes[pos].is_ascii_whitespace() {
        pos += 1;
    }

    if pos >= len {
        return (Token::Null, pos);
    }

    match bytes[pos] {
        // String: 'text with ''escaped'' quotes'
        b'\'' => {
            pos += 1;
            let mut s = String::new();
            while pos < len {
                if bytes[pos] == b'\'' {
                    pos += 1;
                    // Escaped quote ''
                    if pos < len && bytes[pos] == b'\'' {
                        s.push('\'');
                        pos += 1;
                        continue;
                    }
                    break;
                }
                s.push(bytes[pos] as char);
                pos += 1;
            }
            (Token::String(s), pos)
        }

        // Null: $
        b'$' => {
            pos += 1;
            (Token::Null, pos)
        }

        // Derived: *
        b'*' => {
            pos += 1;
            (Token::Derived, pos)
        }

        // Enum: .SOMETHING.
        b'.' => {
            pos += 1;
            let start = pos;
            while pos < len && bytes[pos] != b'.' {
                pos += 1;
            }
            let name = content[start..pos].to_string();
            if pos < len {
                pos += 1; // skip closing '.'
            }
            (Token::Enum(name), pos)
        }

        // Entity reference: #digits
        b'#' => {
            pos += 1;
            let start = pos;
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            let id: u32 = content[start..pos].parse().unwrap_or(0);
            (Token::EntityRef(id), pos)
        }

        // Nested list: (...)
        b'(' => {
            let (items, new_pos) = parse_attribute_list(bytes, content, pos);
            (Token::List(items), new_pos)
        }

        // Number (int or float) or negative
        b'-' | b'0'..=b'9' => {
            let start = pos;
            if bytes[pos] == b'-' {
                pos += 1;
            }
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            let mut is_float = false;
            if pos < len && bytes[pos] == b'.' {
                is_float = true;
                pos += 1;
                while pos < len && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
            // Scientific notation
            if pos < len && (bytes[pos] == b'E' || bytes[pos] == b'e') {
                is_float = true;
                pos += 1;
                if pos < len && (bytes[pos] == b'+' || bytes[pos] == b'-') {
                    pos += 1;
                }
                while pos < len && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
            let num_str = &content[start..pos];
            if is_float {
                let f: f64 = num_str.parse().unwrap_or(0.0);
                (Token::Float(f), pos)
            } else {
                let i: i64 = num_str.parse().unwrap_or(0);
                (Token::Int(i), pos)
            }
        }

        // Typed value: IFCIDENTIFIER('something') or other uppercase identifiers
        _ if bytes[pos].is_ascii_alphabetic() || bytes[pos] == b'_' => {
            let start = pos;
            while pos < len && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_') {
                pos += 1;
            }
            let name = content[start..pos].to_string();

            // Skip whitespace
            while pos < len && bytes[pos].is_ascii_whitespace() {
                pos += 1;
            }

            if pos < len && bytes[pos] == b'(' {
                // It's a typed value like IFCIDENTIFIER('text')
                let (inner_attrs, new_pos) = parse_attribute_list(bytes, content, pos);
                pos = new_pos;
                let inner = if inner_attrs.len() == 1 {
                    inner_attrs.into_iter().next().unwrap()
                } else {
                    Token::List(inner_attrs)
                };
                (Token::TypedValue(name, Box::new(inner)), pos)
            } else {
                // Bare identifier — treat as enum-like
                (Token::Enum(name), pos)
            }
        }

        // Unknown — skip
        _ => {
            pos += 1;
            (Token::Null, pos)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_entity() {
        let content = "#1=IFCWALL('0abc',#2,'Wall Name','Description',$,$,#5,$,.STANDARD.);";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("IFCWALL");
        assert_eq!(entities.len(), 1);
        let e = &entities[0];
        assert_eq!(e.id, 1);
        assert_eq!(e.type_name, "IFCWALL");
        assert_eq!(e.get(0).unwrap().as_string().unwrap(), "0abc");
        assert!(matches!(e.get(1).unwrap(), Token::EntityRef(2)));
        assert_eq!(e.get(2).unwrap().as_string().unwrap(), "Wall Name");
        assert_eq!(e.get(3).unwrap().as_string().unwrap(), "Description");
        assert!(matches!(e.get(4).unwrap(), Token::Null));
        assert!(matches!(e.get(5).unwrap(), Token::Null));
        assert!(matches!(e.get(6).unwrap(), Token::EntityRef(5)));
        assert!(matches!(e.get(7).unwrap(), Token::Null));
        assert_eq!(e.get(8).unwrap().as_enum().unwrap(), "STANDARD");
    }

    #[test]
    fn test_escaped_quotes() {
        let content = "#1=IFCWALL('it''s a test');";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("IFCWALL");
        assert_eq!(entities.len(), 1);
        assert_eq!(
            entities[0].get(0).unwrap().as_string().unwrap(),
            "it's a test"
        );
    }

    #[test]
    fn test_typed_value() {
        let content = "#1=IFCWALL(IFCIDENTIFIER('hello'));";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("IFCWALL");
        assert_eq!(entities.len(), 1);
        match entities[0].get(0).unwrap() {
            Token::TypedValue(name, inner) => {
                assert_eq!(name, "IFCIDENTIFIER");
                assert_eq!(inner.as_string().unwrap(), "hello");
            }
            _ => panic!("expected TypedValue"),
        }
    }

    #[test]
    fn test_multiline_entity() {
        let content = "#1=IFCWALL(\n  '0abc',\n  #2,\n  'Wall'\n);";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("IFCWALL");
        assert_eq!(entities.len(), 1);
        assert_eq!(entities[0].get(0).unwrap().as_string().unwrap(), "0abc");
        assert_eq!(entities[0].get(2).unwrap().as_string().unwrap(), "Wall");
    }

    #[test]
    fn test_case_insensitive_lookup() {
        let content = "#1=IFCWALL('a');#2=IfcWall('b');";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("ifcwall");
        assert_eq!(entities.len(), 2);
    }

    #[test]
    fn test_float_and_int() {
        let content = "#1=IFCWALL(42,-3,1.5,-2.7,3E10,1.5E-3);";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("IFCWALL");
        assert_eq!(entities.len(), 1);
        let e = &entities[0];
        assert_eq!(e.get(0).unwrap().as_float().unwrap(), 42.0);
        assert_eq!(e.get(1).unwrap().as_float().unwrap(), -3.0);
        assert_eq!(e.get(2).unwrap().as_float().unwrap(), 1.5);
        assert_eq!(e.get(3).unwrap().as_float().unwrap(), -2.7);
        assert_eq!(e.get(4).unwrap().as_float().unwrap(), 3e10);
        assert_eq!(e.get(5).unwrap().as_float().unwrap(), 1.5e-3);
    }

    #[test]
    fn test_nested_list() {
        let content = "#1=IFCWALL((1,2,3));";
        let file = StepFile {
            content: content.to_string(),
        };

        let entities = file.find_all("IFCWALL");
        assert_eq!(entities.len(), 1);
        match entities[0].get(0).unwrap() {
            Token::List(items) => assert_eq!(items.len(), 3),
            _ => panic!("expected List"),
        }
    }

    #[test]
    fn test_skips_non_matching() {
        let content = "#1=IFCWALL('a');#2=IFCDOOR('b');#3=IFCWALL('c');";
        let file = StepFile {
            content: content.to_string(),
        };

        let walls = file.find_all("IFCWALL");
        assert_eq!(walls.len(), 2);
        let doors = file.find_all("IFCDOOR");
        assert_eq!(doors.len(), 1);
    }
}
