use r#final::Compiler;

#[cfg(test)]
mod tests {
    use super::*;

    //  Should fail: Missing semicolon after global variable declaration
    #[test]
    fn missing_semicolon() {
        let mut compiler = Compiler::new();
        compiler.set_source(r#"int x = 5"#);
        let result = compiler.program();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected ';' after global"));
    }

    //  Should fail: Missing closing brace } in main function
    #[test]
    fn unmatched_brace() {
        let mut compiler = Compiler::new();
        compiler.set_source(r#"int main() { int x = 5; "#);
        let result = compiler.program();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected '}' at end of function body"));
    }

    //  Should fail: Malformed function declaration (no identifier after 'int')
    #[test]
    fn malformed_main() {
        let mut compiler = Compiler::new();
        compiler.set_source(r#"int () {}"#);
        let result = compiler.program();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("identifier expected"));
    }

    //  Should pass: Valid global variable declaration
    #[test]
    fn global_int_declaration() {
        let mut compiler = Compiler::new();
        compiler.set_source("int x;");
        let result = compiler.program();
        assert!(result.is_ok());
    }

    //  Should pass: Valid empty main function
    #[test]
    fn empty_main_function() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { }");
        let result = compiler.program();
        assert!(result.is_ok());
    }

    //  Should pass: Return statement inside main
    #[test]
    fn return_statement() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { return 42; }");
        let result = compiler.program();
        assert!(result.is_ok());
    }

    //  Should pass: Expression statement using addition
    #[test]
    fn expression_with_addition() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { 2 + 3; }");
        let result = compiler.program();
        assert!(result.is_ok());
    }

    //  Should pass: While loop with a condition and body
    #[test]
    fn test_while_loop() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { int x; x = 0; while (x < 3) { x = x + 1; } }");
        assert!(compiler.program().is_ok(), "Expected valid while loop to compile");
    }

    //  Should pass: If-else control flow
    #[test]
    fn test_if_else_statement() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { int x; x = 1; if (x == 1) { x = 2; } else { x = 3; } }");
        assert!(compiler.program().is_ok(), "Expected valid if-else statement to compile");
    }

    //  Should fail: Using sizeof without parentheses
    #[test]
    fn test_sizeof_missing_parenthesis() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { int a; a = sizeof int; }");
        let result = compiler.program();
        assert!(result.is_err(), "Expected error for missing parentheses in sizeof");
    }

    //  Should pass: Valid use of sizeof with parentheses
    #[test]
    fn test_sizeof_usage() {
        let mut compiler = Compiler::new();
        compiler.set_source("int main() { int a; a = sizeof(int); a = sizeof(char*); }");
        let result = compiler.program();
        assert!(result.is_ok(), "Expected valid sizeof usage to compile");
    }
}
