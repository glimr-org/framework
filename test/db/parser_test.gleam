import gleam/option.{None, Some}
import gleeunit/should
import glimr/db/gen/parser
import glimr/db/gen/parser/columns.{SelectedColumn}

// ------------------------------------------------------------- SELECT Queries

pub fn parse_simple_select_test() {
  let sql = "SELECT id, name FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.columns
  |> should.equal([
    SelectedColumn(table: None, name: "id", alias: None),
    SelectedColumn(table: None, name: "name", alias: None),
  ])

  parsed.params
  |> should.equal([])
}

pub fn parse_select_with_alias_test() {
  let sql = "SELECT id, name AS username FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.columns
  |> should.equal([
    SelectedColumn(table: None, name: "id", alias: None),
    SelectedColumn(table: None, name: "name", alias: Some("username")),
  ])
}

pub fn parse_select_star_test() {
  let sql = "SELECT * FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.columns
  |> should.equal([SelectedColumn(table: None, name: "*", alias: None)])
}

pub fn parse_select_with_where_param_test() {
  let sql = "SELECT id, name FROM users WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1])

  parsed.param_columns
  |> should.equal([#(1, "id")])
}

pub fn parse_select_with_multiple_where_params_test() {
  let sql = "SELECT * FROM users WHERE status = $1 AND role = $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "status"), #(2, "role")])
}

pub fn parse_select_with_or_where_test() {
  let sql = "SELECT * FROM users WHERE status = $1 OR role = $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "status"), #(2, "role")])
}

pub fn parse_select_with_mixed_and_or_test() {
  let sql =
    "SELECT * FROM users WHERE (status = $1 AND role = $2) OR admin = $3"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2, 3])

  parsed.param_columns
  |> should.equal([#(1, "status"), #(2, "role"), #(3, "admin")])
}

pub fn parse_select_with_like_test() {
  let sql = "SELECT * FROM users WHERE name LIKE $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])

  parsed.param_columns
  |> should.equal([#(1, "name")])
}

pub fn parse_select_with_in_test() {
  let sql = "SELECT * FROM users WHERE status IN ($1)"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])

  // IN clause param extraction is not yet supported
  parsed.param_columns
  |> should.equal([])
}

pub fn parse_select_with_between_test() {
  let sql = "SELECT * FROM orders WHERE created_at BETWEEN $1 AND $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "start_created_at"), #(2, "end_created_at")])
}

// ------------------------------------------------------------- JOIN Queries

pub fn parse_select_with_join_test() {
  let sql = "SELECT u.id, p.title FROM users u JOIN posts p ON u.id = p.user_id"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Tables are extracted in reverse order (JOINed tables first)
  parsed.tables
  |> should.equal(["posts", "users"])
}

pub fn parse_select_with_left_join_test() {
  let sql =
    "SELECT u.id, p.title FROM users u LEFT JOIN posts p ON u.id = p.user_id"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["posts", "users"])
}

pub fn parse_select_with_multiple_joins_test() {
  let sql =
    "SELECT * FROM users u JOIN posts p ON u.id = p.user_id JOIN comments c ON p.id = c.post_id"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["comments", "posts", "users"])
}

// ------------------------------------------------------------- INSERT Queries

pub fn parse_insert_test() {
  let sql = "INSERT INTO users (name, email) VALUES ($1, $2)"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "name"), #(2, "email")])
}

pub fn parse_insert_with_returning_test() {
  let sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.columns
  |> should.equal([
    SelectedColumn(table: None, name: "id", alias: None),
    SelectedColumn(table: None, name: "name", alias: None),
  ])

  parsed.params
  |> should.equal([1, 2])
}

pub fn parse_insert_with_more_columns_test() {
  let sql =
    "INSERT INTO users (name, email, role, status) VALUES ($1, $2, $3, $4)"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2, 3, 4])

  parsed.param_columns
  |> should.equal([#(1, "name"), #(2, "email"), #(3, "role"), #(4, "status")])
}

// ------------------------------------------------------------- UPDATE Queries

pub fn parse_update_test() {
  let sql = "UPDATE users SET name = $1 WHERE id = $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "name"), #(2, "id")])
}

pub fn parse_update_multiple_columns_test() {
  let sql = "UPDATE users SET name = $1, email = $2, status = $3 WHERE id = $4"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2, 3, 4])

  parsed.param_columns
  |> should.equal([#(1, "name"), #(2, "email"), #(3, "status"), #(4, "id")])
}

pub fn parse_update_with_returning_test() {
  let sql = "UPDATE users SET name = $1 WHERE id = $2 RETURNING id, name"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.columns
  |> should.equal([
    SelectedColumn(table: None, name: "id", alias: None),
    SelectedColumn(table: None, name: "name", alias: None),
  ])
}

// ------------------------------------------------------------- DELETE Queries

pub fn parse_delete_test() {
  let sql = "DELETE FROM users WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1])

  parsed.param_columns
  |> should.equal([#(1, "id")])
}

pub fn parse_delete_with_multiple_conditions_test() {
  let sql = "DELETE FROM users WHERE status = $1 AND created_at < $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "status"), #(2, "created_at")])
}

// ------------------------------------------------------------- Subqueries

pub fn parse_subquery_in_where_test() {
  let sql =
    "SELECT * FROM users WHERE id IN (SELECT user_id FROM orders WHERE status = $1)"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])

  // Tables are returned in alphabetical order
  parsed.tables
  |> should.equal(["orders", "users"])
}

pub fn parse_subquery_in_from_test() {
  let sql =
    "SELECT s.id, s.total FROM (SELECT id, COUNT(*) AS total FROM orders GROUP BY id) s"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // The derived table should be recognized
  parsed.tables
  |> should.equal(["orders"])
}

pub fn parse_exists_subquery_test() {
  let sql =
    "SELECT * FROM users u WHERE EXISTS (SELECT 1 FROM orders o WHERE o.user_id = u.id AND o.status = $1)"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

// ------------------------------------------------------------- UNION Queries

pub fn parse_union_test() {
  let sql =
    "SELECT id, name FROM users WHERE status = $1 UNION SELECT id, name FROM admins WHERE active = $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])

  // Tables are returned in alphabetical order
  parsed.tables
  |> should.equal(["admins", "users"])
}

pub fn parse_union_all_test() {
  let sql =
    "SELECT id FROM active_users UNION ALL SELECT id FROM inactive_users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Tables in SQL order
  parsed.tables
  |> should.equal(["active_users", "inactive_users"])
}

// ------------------------------------------------------------- CTEs (WITH clause)

pub fn parse_cte_test() {
  let sql =
    "WITH active AS (SELECT * FROM users WHERE status = $1) SELECT * FROM active WHERE role = $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])
}

pub fn parse_multiple_ctes_test() {
  let sql =
    "WITH admins AS (SELECT * FROM users WHERE role = 'admin'), active AS (SELECT * FROM admins WHERE status = $1) SELECT * FROM active"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

// ------------------------------------------------------------- Window Functions

pub fn parse_window_function_row_number_test() {
  let sql =
    "SELECT id, name, ROW_NUMBER() OVER (ORDER BY created_at) AS row_num FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_window_function_partition_test() {
  let sql =
    "SELECT id, department, salary, RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS rank FROM employees"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["employees"])
}

pub fn parse_window_function_with_alias_test() {
  let sql =
    "SELECT id, SUM(amount) OVER (PARTITION BY user_id ORDER BY created_at) AS running_total FROM orders"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["orders"])
}

// ------------------------------------------------------------- GROUP BY / HAVING

pub fn parse_group_by_test() {
  let sql = "SELECT status, COUNT(*) FROM users GROUP BY status"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_group_by_with_having_test() {
  let sql =
    "SELECT status, COUNT(*) AS cnt FROM users GROUP BY status HAVING COUNT(*) > $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

pub fn parse_group_by_multiple_columns_test() {
  let sql =
    "SELECT department, role, COUNT(*) FROM employees GROUP BY department, role"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["employees"])
}

// ------------------------------------------------------------- ORDER BY / LIMIT / OFFSET

pub fn parse_order_by_test() {
  let sql = "SELECT * FROM users ORDER BY created_at DESC"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_limit_offset_test() {
  let sql = "SELECT * FROM users ORDER BY id LIMIT $1 OFFSET $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])
}

pub fn parse_order_by_multiple_columns_test() {
  let sql = "SELECT * FROM users ORDER BY last_name ASC, first_name ASC"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

// ------------------------------------------------------------- DISTINCT

pub fn parse_distinct_test() {
  let sql = "SELECT DISTINCT status FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_distinct_on_test() {
  let sql =
    "SELECT DISTINCT ON (user_id) * FROM orders ORDER BY user_id, created_at DESC"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["orders"])
}

// ------------------------------------------------------------- Aggregate Functions

pub fn parse_aggregate_sum_test() {
  let sql = "SELECT SUM(amount) AS total FROM orders WHERE user_id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])

  parsed.param_columns
  |> should.equal([#(1, "user_id")])
}

pub fn parse_aggregate_avg_test() {
  let sql = "SELECT AVG(price) AS avg_price FROM products"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["products"])
}

pub fn parse_aggregate_with_filter_test() {
  let sql =
    "SELECT COUNT(*) FILTER (WHERE status = 'active') AS active_count FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

// ------------------------------------------------------------- CASE Expressions

pub fn parse_case_expression_test() {
  let sql =
    "SELECT id, CASE WHEN status = 'active' THEN 1 ELSE 0 END AS is_active FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_case_with_param_test() {
  let sql =
    "SELECT id, CASE WHEN role = $1 THEN 'admin' ELSE 'user' END AS role_label FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

// ------------------------------------------------------------- Complex Queries

pub fn parse_complex_join_with_subquery_test() {
  let sql =
    "SELECT u.id, u.name, o.total FROM users u LEFT JOIN (SELECT user_id, SUM(amount) AS total FROM orders GROUP BY user_id) o ON u.id = o.user_id WHERE u.status = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

pub fn parse_multiple_nested_subqueries_test() {
  let sql =
    "SELECT * FROM users WHERE id IN (SELECT user_id FROM orders WHERE product_id IN (SELECT id FROM products WHERE category = $1))"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

pub fn parse_cte_with_join_test() {
  let sql =
    "WITH recent_orders AS (SELECT * FROM orders WHERE created_at > $1) SELECT u.name, ro.amount FROM users u JOIN recent_orders ro ON u.id = ro.user_id"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])
}

// ------------------------------------------------------------- Schema-Qualified Tables

pub fn parse_schema_qualified_table_test() {
  let sql = "SELECT * FROM public.users WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["public.users"])

  parsed.params
  |> should.equal([1])
}

pub fn parse_schema_qualified_with_join_test() {
  let sql =
    "SELECT * FROM public.users u JOIN auth.sessions s ON u.id = s.user_id"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["auth.sessions", "public.users"])
}

pub fn parse_schema_qualified_insert_test() {
  let sql = "INSERT INTO myschema.users (name) VALUES ($1)"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["myschema.users"])
}

pub fn parse_schema_qualified_update_test() {
  let sql = "UPDATE app.settings SET value = $1 WHERE key = $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["app.settings"])
}

pub fn parse_schema_qualified_delete_test() {
  let sql = "DELETE FROM archive.logs WHERE created_at < $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["archive.logs"])
}

// ------------------------------------------------------------- Quoted Identifiers

pub fn parse_quoted_table_name_test() {
  let sql = "SELECT * FROM \"user-data\" WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["user-data"])

  parsed.params
  |> should.equal([1])
}

pub fn parse_quoted_reserved_word_table_test() {
  let sql = "SELECT * FROM \"select\" WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["select"])
}

pub fn parse_quoted_table_with_spaces_test() {
  let sql = "SELECT * FROM \"user data\" WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["user data"])
}

pub fn parse_quoted_schema_and_table_test() {
  let sql = "SELECT * FROM \"my-schema\".\"my-table\" WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["my-schema.my-table"])
}

// ------------------------------------------------------------- String Literals with SQL Keywords

pub fn parse_string_literal_with_select_test() {
  let sql = "SELECT * FROM users WHERE bio = 'I SELECT things FROM stores'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Should only find 'users', not 'stores' from string literal
  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_string_literal_with_join_test() {
  let sql =
    "SELECT * FROM users WHERE description = 'JOIN us for fun JOIN activities'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_param_near_string_literal_test() {
  let sql = "SELECT * FROM users WHERE name = $1 AND bio = 'FROM somewhere'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1])
}

pub fn parse_escaped_quotes_in_string_test() {
  let sql = "SELECT * FROM users WHERE name = 'O''Brien'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_escaped_quote_with_keyword_test() {
  // Escaped quote followed by SQL keyword inside string
  let sql = "SELECT * FROM users WHERE bio = 'I''m FROM the city'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Should only find 'users', not be confused by FROM inside string
  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_multiple_escaped_quotes_test() {
  let sql = "SELECT * FROM users WHERE bio = 'It''s a ''JOIN'' party FROM here'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_string_with_subquery_pattern_test() {
  let sql =
    "SELECT * FROM users WHERE note = 'Check (SELECT * FROM orders) for info'"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Should only find 'users', not 'orders' from string literal
  parsed.tables
  |> should.equal(["users"])
}

// ------------------------------------------------------------- Comments

pub fn parse_with_comments_test() {
  let sql =
    "
    -- Get user by id
    SELECT id, name FROM users WHERE id = $1
  "
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1])
}

pub fn parse_block_comment_test() {
  let sql =
    "
    /* This is a block comment */
    SELECT id, name FROM users WHERE id = $1
  "
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1])
}

pub fn parse_inline_block_comment_test() {
  let sql = "SELECT id /* user id */, name FROM users WHERE id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_comment_with_sql_keywords_test() {
  let sql =
    "
    -- SELECT * FROM secret_table
    /* DELETE FROM users WHERE 1=1 */
    SELECT id FROM users WHERE id = $1
  "
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Should only find 'users', not 'secret_table' from comment
  parsed.tables
  |> should.equal(["users"])
}

pub fn parse_multiline_block_comment_test() {
  let sql =
    "
    /*
     * This query fetches users
     * FROM the database
     */
    SELECT * FROM users
  "
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])
}

// ------------------------------------------------------------- Edge Cases

pub fn parse_with_extra_whitespace_test() {
  let sql = "SELECT    id,    name    FROM    users    WHERE   id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.columns
  |> should.equal([
    SelectedColumn(table: None, name: "id", alias: None),
    SelectedColumn(table: None, name: "name", alias: None),
  ])
}

pub fn parse_case_insensitive_keywords_test() {
  let sql = "select id, name from users where id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.params
  |> should.equal([1])
}

pub fn parse_select_with_function_test() {
  let sql = "SELECT COUNT(*) AS total FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.tables
  |> should.equal(["users"])

  parsed.columns
  |> should.equal([
    SelectedColumn(table: None, name: "COUNT(*)", alias: Some("total")),
  ])
}

pub fn parse_select_with_coalesce_test() {
  let sql = "SELECT COALESCE(nickname, name) AS display_name FROM users"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.columns
  |> should.equal([
    SelectedColumn(
      table: None,
      name: "COALESCE(nickname, name)",
      alias: Some("display_name"),
    ),
  ])
}

pub fn parse_params_out_of_order_test() {
  let sql = "SELECT * FROM users WHERE role = $2 AND status = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Params should be sorted
  parsed.params
  |> should.equal([1, 2])
}

pub fn parse_duplicate_params_test() {
  let sql = "SELECT * FROM users WHERE id = $1 OR parent_id = $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  // Params should be deduplicated
  parsed.params
  |> should.equal([1])
}

pub fn parse_comparison_operators_test() {
  let sql = "SELECT * FROM users WHERE age >= $1 AND age <= $2"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1, 2])

  parsed.param_columns
  |> should.equal([#(1, "age"), #(2, "age")])
}

pub fn parse_not_equal_operator_test() {
  let sql = "SELECT * FROM users WHERE status != $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.params
  |> should.equal([1])

  parsed.param_columns
  |> should.equal([#(1, "status")])
}

pub fn parse_ilike_operator_test() {
  let sql = "SELECT * FROM users WHERE name ILIKE $1"
  let assert Ok(parsed) = parser.parse_sql(sql)

  parsed.param_columns
  |> should.equal([#(1, "name")])
}
