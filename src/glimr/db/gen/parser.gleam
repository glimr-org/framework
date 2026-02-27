//// SQL Parser
////
//// Parses SQL query files to extract table names, columns, and
//// parameter references.
////
//// This module provides the main entry point for SQL parsing.
//// The parsing logic is split across sub-modules:
////
//// - parser/normalize: SQL normalization (comments, whitespace)
//// - parser/tables: Table extraction (FROM, JOIN, etc.)
//// - parser/columns: Column extraction (SELECT, RETURNING)
//// - parser/params: Parameter extraction and mapping
//// - parser/util: Shared utilities

import glimr/db/gen/parser/columns
import glimr/db/gen/parser/normalize
import glimr/db/gen/parser/params
import glimr/db/gen/parser/tables

// ------------------------------------------------------------- Public Types

/// The result of parsing a SQL query, containing all extracted
/// metadata needed for code generation including tables,
/// columns, parameters, and parameter-column mappings.
///
pub type ParsedQuery {
  ParsedQuery(
    tables: List(String),
    columns: List(columns.SelectedColumn),
    params: List(Int),
    param_columns: List(#(Int, String)),
  )
}

// ------------------------------------------------------------- Public Functions

/// Parse a SQL query to extract tables, columns, parameters,
/// and parameter-to-column mappings. This is the main entry
/// point for the SQL parser.
///
pub fn parse_sql(sql: String) -> Result(ParsedQuery, String) {
  let normalized = normalize.normalize(sql)

  // Extract tables from FROM and JOIN clauses
  let extracted_tables = tables.extract(normalized)

  // Extract selected columns
  let extracted_columns = columns.extract(normalized)

  // Extract parameter positions ($1, $2, etc.)
  let extracted_params = params.extract(normalized)

  // Extract parameter-to-column mappings from WHERE clause
  let extracted_param_columns = params.extract_columns(normalized)

  Ok(ParsedQuery(
    tables: extracted_tables,
    columns: extracted_columns,
    params: extracted_params,
    param_columns: extracted_param_columns,
  ))
}
