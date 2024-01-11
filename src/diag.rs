use std::io::Write;
use std::rc::Rc;

use ariadne::Config;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;

use crate::lexer::Span;

#[derive(Default, Clone)]
pub struct Sources(Vec<(String, Rc<Source>)>);

impl Sources {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn next_file(&self) -> u16 {
        self.0.len() as u16
    }

    pub fn add(&mut self, name: impl ToString, data: impl ToString) -> u16 {
        let id = self.0.len() as u16;
        self.0
            .push((name.to_string(), Rc::new(Source::from(data.to_string()))));
        id
    }
}

impl std::ops::Index<u16> for Sources {
    type Output = Source;

    fn index(&self, index: u16) -> &Self::Output {
        &self.0[index as usize].1
    }
}

impl std::fmt::Debug for Sources {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let names = self.0.iter().map(|(x, _)| x).collect::<Vec<_>>();
        f.debug_struct("Sources").field("sources", &names).finish()
    }
}

impl ariadne::Cache<u16> for Sources {
    type Storage = String;

    fn fetch(&mut self, id: &u16) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        self.0
            .get(*id as usize)
            .map(|(_, s)| s.as_ref())
            .ok_or_else(|| {
                Box::new(format!("Source with id {} not found in cache", id))
                    as Box<dyn std::fmt::Debug + '_>
            })
    }

    fn display<'a>(&self, id: &'a u16) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0
            .get(*id as usize)
            .map(|x| Box::new(x.0.clone()) as Box<dyn std::fmt::Display + 'a>)
    }
}

impl ariadne::Span for Span {
    fn start(&self) -> usize {
        *self.start() as usize
    }

    fn end(&self) -> usize {
        *self.end() as usize
    }

    type SourceId = u16;

    fn source(&self) -> &Self::SourceId {
        self.file()
    }
}

#[derive(Debug)]
pub struct Diags(pub Vec<ariadne::Report<'static, Span>>);

impl Diags {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn err(&mut self, span: Span, label: impl AsRef<str>, msg: impl AsRef<str>) {
        let kind = ReportKind::Error;
        let report = Report::build(kind, *span.file(), *span.start() as usize)
            .with_label(Label::new(span).with_message(msg.as_ref()))
            .with_message(label.as_ref())
            .with_config(Config::default().with_color(false))
            .finish();
        self.0.push(report);
    }

    pub fn err2(
        &mut self,
        s0: Span,
        s1: Span,
        label: impl AsRef<str>,
        msg0: impl AsRef<str>,
        msg1: impl AsRef<str>,
    ) {
        let kind = ReportKind::Error;
        let report = Report::build(kind, *s0.file(), *s0.start() as usize)
            .with_label(Label::new(s0).with_message(msg0.as_ref()))
            .with_label(Label::new(s1).with_message(msg1.as_ref()))
            .with_message(&label.as_ref())
            .with_config(Config::default().with_color(false))
            .finish();
        self.0.push(report);
    }

    pub fn print(&mut self, mut sources: &mut Sources) -> std::io::Result<()> {
        for diag in &self.0 {
            diag.eprint(&mut sources)?;
        }
        Ok(())
    }

    pub fn string(
        &mut self,
        mut sources: &mut Sources,
    ) -> Result<String, std::string::FromUtf8Error> {
        let mut buf = Vec::new();
        for diag in &self.0 {
            diag.write(&mut sources, &mut buf).unwrap();
            writeln!(&mut buf, "").unwrap();
        }
        String::from_utf8(buf)
    }
}
