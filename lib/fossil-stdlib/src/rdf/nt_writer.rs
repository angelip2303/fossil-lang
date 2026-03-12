use std::io::Write;

use super::triple_writer::TripleWriter;

const BNODE_PREFIX: &str = "_:";

pub struct NTriplesWriter {
    writer: Box<dyn Write + Send>,
}

impl NTriplesWriter {
    pub fn new(writer: Box<dyn Write + Send>) -> Self {
        Self { writer }
    }
}

impl TripleWriter for NTriplesWriter {
    fn write_triple(
        &mut self,
        subject: &str,
        predicate: &str,
        object: &str,
        datatype: Option<&str>,
        lang: Option<&str>,
        _graph: Option<&str>,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // Subject
        if subject.starts_with(BNODE_PREFIX) {
            write!(self.writer, "{subject} ")?;
        } else {
            write!(self.writer, "<{subject}> ")?;
        }

        // Predicate (always an IRI)
        let pred = predicate
            .strip_prefix('<')
            .and_then(|s| s.strip_suffix('>'))
            .unwrap_or(predicate);
        write!(self.writer, "<{pred}> ")?;

        // Object
        if object.starts_with(BNODE_PREFIX) {
            write!(self.writer, "{object}")?;
        } else if object.starts_with("http://")
            || object.starts_with("https://")
            || object.starts_with("urn:")
        {
            write!(self.writer, "<{object}>")?;
        } else if let Some(stripped) = object
            .strip_prefix('<')
            .and_then(|s| s.strip_suffix('>'))
        {
            write!(self.writer, "<{stripped}>")?;
        } else {
            let escaped = escape_ntriples(object);
            if let Some(lang) = lang {
                write!(self.writer, "\"{escaped}\"@{lang}")?;
            } else if let Some(dt) = datatype {
                write!(self.writer, "\"{escaped}\"^^<{dt}>")?;
            } else {
                write!(self.writer, "\"{escaped}\"")?;
            }
        }

        writeln!(self.writer, " .")?;
        Ok(())
    }

    fn finish(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        self.writer.flush()?;
        Ok(())
    }
}

fn escape_ntriples(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}
