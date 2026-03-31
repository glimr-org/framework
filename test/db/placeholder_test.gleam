import gleeunit/should
import glimr/db/db

// ------------------------------------------------------------- Basic Conversion

pub fn converts_single_placeholder_test() {
  db.convert_placeholders("SELECT * FROM users WHERE id = $1", db.Sqlite)
  |> should.equal("SELECT * FROM users WHERE id = ?")
}

pub fn converts_multiple_placeholders_test() {
  db.convert_placeholders(
    "INSERT INTO users (name, email) VALUES ($1, $2)",
    db.Sqlite,
  )
  |> should.equal("INSERT INTO users (name, email) VALUES (?, ?)")
}

pub fn converts_double_digit_placeholders_test() {
  db.convert_placeholders("SELECT $1, $2, $10, $12", db.Sqlite)
  |> should.equal("SELECT ?, ?, ?, ?")
}

pub fn postgres_driver_returns_sql_unchanged_test() {
  let sql = "SELECT * FROM users WHERE id = $1"
  db.convert_placeholders(sql, db.Postgres)
  |> should.equal(sql)
}

// ------------------------------------------------------------- String Literals with $

pub fn preserves_dollar_in_single_quoted_string_test() {
  db.convert_placeholders(
    "UPDATE users SET password = '$argon2id$v=13$m=19456,t=2,p=1$abc$xyz' WHERE id = $1",
    db.Sqlite,
  )
  |> should.equal(
    "UPDATE users SET password = '$argon2id$v=13$m=19456,t=2,p=1$abc$xyz' WHERE id = ?",
  )
}

pub fn preserves_dollar_in_string_with_no_placeholders_test() {
  db.convert_placeholders("SELECT '$100 bill' AS label", db.Sqlite)
  |> should.equal("SELECT '$100 bill' AS label")
}

pub fn handles_escaped_single_quotes_in_string_test() {
  db.convert_placeholders(
    "SELECT 'it''s $100' AS label WHERE id = $1",
    db.Sqlite,
  )
  |> should.equal("SELECT 'it''s $100' AS label WHERE id = ?")
}

pub fn handles_multiple_string_literals_and_placeholders_test() {
  db.convert_placeholders(
    "UPDATE t SET a = '$foo' WHERE b = $1 AND c = '$bar' AND d = $2",
    db.Sqlite,
  )
  |> should.equal(
    "UPDATE t SET a = '$foo' WHERE b = ? AND c = '$bar' AND d = ?",
  )
}

pub fn handles_string_with_no_dollar_signs_test() {
  db.convert_placeholders(
    "UPDATE users SET name = 'hello' WHERE id = $1",
    db.Sqlite,
  )
  |> should.equal("UPDATE users SET name = 'hello' WHERE id = ?")
}

pub fn handles_empty_string_literal_test() {
  db.convert_placeholders("UPDATE users SET name = '' WHERE id = $1", db.Sqlite)
  |> should.equal("UPDATE users SET name = '' WHERE id = ?")
}

// ------------------------------------------------------------- Edge Cases

pub fn preserves_placeholder_like_value_inside_string_test() {
  db.convert_placeholders(
    "SELECT * FROM t WHERE a = '$1' AND b = $1",
    db.Sqlite,
  )
  |> should.equal("SELECT * FROM t WHERE a = '$1' AND b = ?")
}

pub fn preserves_string_at_end_of_sql_test() {
  db.convert_placeholders("SELECT '$1'", db.Sqlite)
  |> should.equal("SELECT '$1'")
}

pub fn converts_placeholder_right_after_closing_quote_test() {
  db.convert_placeholders("SELECT 'text',$1 FROM t", db.Sqlite)
  |> should.equal("SELECT 'text',? FROM t")
}

pub fn preserves_lone_dollar_inside_string_test() {
  db.convert_placeholders("SELECT '$' AS sym WHERE id = $1", db.Sqlite)
  |> should.equal("SELECT '$' AS sym WHERE id = ?")
}

pub fn preserves_dollars_in_adjacent_string_literals_test() {
  db.convert_placeholders("SELECT '$1' || '$2' WHERE id = $1", db.Sqlite)
  |> should.equal("SELECT '$1' || '$2' WHERE id = ?")
}
