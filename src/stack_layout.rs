use crate::ast::{FuncDef, Name, Type, LetAssignment, Line, Block, Param};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
enum VariableType {
    Variable,
    Parameter,
}

pub struct StackLayout<'s> {
    variables: HashMap<Name<'s>, (u64, Type, VariableType)>,
    last_variable: u64,
    last_param: u64,
}

impl<'s> StackLayout<'s> {
    pub fn new() -> Self {
        Self {
            variables: Default::default(),
            last_variable: 0,
            last_param: 0,
        }
    }

    pub fn add_parameter(&mut self, name: Name<'s>, tp: Type) -> u64 {
        assert_eq!(self.last_variable, 0);

        if let Some(i) = self.variables.get(&name) {
            i.0
        } else {
            let index = self.last_param;
            self.last_param += 1;
            self.variables.insert(name, (index, tp, VariableType::Parameter));
            index
        }
    }

    pub fn add_variable(&mut self, name: Name<'s>, tp: Type) -> u64 {
        if self.last_variable == 0 {
            // +2 for return pointer
            self.last_variable = self.last_param + 2;
        }

        if let Some(i) = self.variables.get(&name) {
            i.0
        } else {
            let index = self.last_variable;
            self.last_variable += 1;
            self.variables.insert(name, (index, tp, VariableType::Variable));
            index
        }
    }

    pub fn size(&self) -> u64 {
        let mut res = 0;
        for (_, tp, vtp) in self.variables.values() {
            if vtp == &VariableType::Variable {
                res += tp.size();
            }
        }

        res
    }
}

impl<'s> FuncDef<'s> {
    fn find_let_in_line(line: &Line<'s>, layout: &mut StackLayout<'s>) {
        match line {
            Line::LetAssignment(LetAssignment{ tp, to, .. }) => {
                layout.add_variable(to.clone(), tp.clone());
            },
            _ => {},
        }
    }

    fn find_let_in_block(block: &Block<'s>, layout: &mut StackLayout<'s>) {
        for i in &block.lines {
            Self::find_let_in_line(&i, layout)
        }
    }

    pub fn get_declared_bytes(&self, layout: &mut StackLayout<'s>) -> u64 {
        for Param { param_type, name } in &self.params {
            layout.add_parameter(name.clone(), param_type.clone());
        }

        Self::find_let_in_block(&self.block, layout);

        layout.size()
    }
}