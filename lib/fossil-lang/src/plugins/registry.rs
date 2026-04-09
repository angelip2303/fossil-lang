//! PluginRegistry — central registry for all plugin types.

use std::sync::Arc;

use super::source::SourcePlugin;
use super::transform::TransformPlugin;
use super::pipeline::PipelinePlugin;
use super::output::OutputPlugin;

/// Central registry holding all registered plugins.
pub struct PluginRegistry {
    pub sources: Vec<Arc<dyn SourcePlugin>>,
    pub transforms: Vec<Arc<dyn TransformPlugin>>,
    pub pipelines: Vec<Arc<dyn PipelinePlugin>>,
    pub outputs: Vec<Arc<dyn OutputPlugin>>,
}

impl PluginRegistry {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
            transforms: Vec::new(),
            pipelines: Vec::new(),
            outputs: Vec::new(),
        }
    }

    pub fn source(mut self, plugin: impl SourcePlugin + 'static) -> Self {
        self.sources.push(Arc::new(plugin));
        self
    }

    pub fn transform(mut self, plugin: impl TransformPlugin + 'static) -> Self {
        self.transforms.push(Arc::new(plugin));
        self
    }

    pub fn pipeline(mut self, plugin: impl PipelinePlugin + 'static) -> Self {
        self.pipelines.push(Arc::new(plugin));
        self
    }

    pub fn output(mut self, plugin: impl OutputPlugin + 'static) -> Self {
        self.outputs.push(Arc::new(plugin));
        self
    }

    /// Find a source plugin by name.
    pub fn find_source(&self, name: &str) -> Option<&dyn SourcePlugin> {
        self.sources.iter().find(|p| p.name() == name).map(|p| p.as_ref())
    }

    /// Find a source plugin by file extension.
    pub fn find_source_by_ext(&self, ext: &str) -> Option<&dyn SourcePlugin> {
        self.sources.iter().find(|p| p.extensions().contains(&ext)).map(|p| p.as_ref())
    }

    /// Find a transform plugin by namespace.
    pub fn find_transform(&self, namespace: &str) -> Option<&dyn TransformPlugin> {
        self.transforms.iter().find(|p| p.namespace() == namespace).map(|p| p.as_ref())
    }

    /// Find a pipeline plugin by name.
    pub fn find_pipeline(&self, name: &str) -> Option<&dyn PipelinePlugin> {
        self.pipelines.iter().find(|p| p.name() == name).map(|p| p.as_ref())
    }

    /// Find an output plugin by name.
    pub fn find_output(&self, name: &str) -> Option<&dyn OutputPlugin> {
        self.outputs.iter().find(|p| p.name() == name).map(|p| p.as_ref())
    }
}
