use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IR {
    pub program_name: String,
    pub source_file: String,
    pub identification_division: IdentificationDivision,
    pub environment_division: EnvironmentDivision,
    pub data_division: DataDivision,
    pub paragraphs: Vec<Paragraph>,
    pub procedure_division: ProcedureDivision,
    pub call_graph: Vec<CallGraphEntry>,
    pub control_flow_graph: Vec<ControlFlowEdge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdentificationDivision {
    pub author: String,
    pub date_written: String,
    pub comments: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentDivision {
    pub input_output_section: InputOutputSection,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputOutputSection {
    pub files: Vec<IOFile>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IOFile {
    pub name: String,
    #[serde(rename = "type")]
    pub r#type: String,
    pub description: String,
    pub record_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataItem {
    pub name: String,
    pub level: i32,
    pub picture: Option<String>,
    #[serde(rename = "type")]
    pub r#type: Option<String>,
    pub value: Option<String>,
    pub occurs: Option<usize>,
    pub redefines: Option<String>,
    pub comp3: bool,
    pub section: Option<String>,
    pub children: Vec<DataItem>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataDivision {
    pub working_storage: Vec<DataItem>,
    pub file_section: Vec<DataItem>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcedureSection {
    pub name: String,
    pub paragraphs: Vec<Paragraph>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Paragraph {
    pub name: String,
    pub section: Option<String>,
    pub kind: String,
    pub line: Option<usize>,
    pub source_location: Option<String>,
    pub statements: Vec<Statement>,
    pub variable_usage: Vec<VariableUsage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Statement {
    #[serde(rename = "type")]
    pub r#type: String,
    pub operands: Vec<String>,
    pub raw: String,
    pub line: Option<usize>,
    pub source_location: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallGraphEntry {
    pub from: String,
    pub to: String,
    #[serde(rename = "type")]
    pub r#type: String,
    pub kind: String,
    pub line: Option<usize>,
    pub section: Option<String>,
    pub source_location: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcedureDivision {
    pub sections: Vec<ProcedureSection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableUsage {
    pub name: String,
    pub read: bool,
    pub written: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlFlowEdge {
    pub from: String,
    pub to: String,
    #[serde(rename = "type")]
    pub r#type: String,
}
