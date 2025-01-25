use std::collections::{HashMap, HashSet};
use crate::lexer::Constant;

pub struct SemanticAnalyzer {
    var_table: HashMap<String, Constant>,
    function_set: HashSet<String>,
}



impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            var_table: HashMap::new(),
            function_set: HashSet::new(),
        }
    }

    pub fn declare_variable(&mut self, name: &String) -> Result<(), String> {
        if self.var_table.contains_key(name) {
            // Return an error if the key already exists
            Err(format!("Key '{}' already exists in the map", name))
        } else {
            // Add the key-value pair to the map if the key doesn't exist
            self.var_table.insert(name.to_string(), Constant::Undefined);
            Ok(())
        }
    }

    pub fn assign_value_to_variable(&mut self, name: String, value: Constant) -> Result<(), String> {
        if self.var_table.contains_key(&name) {
            let old_val = self.var_table.insert(name, value.clone());
            assert!(old_val.is_some());
            assert_eq!(value.get_type(), old_val.unwrap().get_type());

            Ok(())
        } else {
            Err(format!("Key '{}' does not exist in the map", name.to_string()))
        }
    }

}
