use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Instruction, Module};
use miden_debug_types::{DefaultSourceManager, Span};
use tower_lsp::lsp_types::{CodeLens, Command};

use crate::analysis::{ContractStore, StackEffect};
use crate::diagnostics::span_to_range;
use crate::stack_effect::format_stack_change_from_counts;
use crate::stack_effect::ToStackEffect;
use crate::symbol_resolution::create_resolver;

/// Collect stack-effect code lenses for all instructions in a module.
pub fn collect_code_lenses(
    module: &Module,
    sources: &DefaultSourceManager,
    contracts: Option<&ContractStore>,
) -> Vec<CodeLens> {
    let mut collector = InstructionCollector::default();
    let _ = visit::visit_module(&mut collector, module);
    let resolver = create_resolver(module);

    collector
        .instructions
        .into_iter()
        .filter_map(|inst| {
            let Some(range) = span_to_range(sources, inst.span()) else {
                return None;
            };
            let label = contract_stack_effect(inst.inner(), contracts, &resolver)
                .or_else(|| inst.inner().to_stack_effect())?;
            Some(CodeLens {
                range,
                command: Some(Command {
                    title: label,
                    command: String::new(),
                    arguments: None,
                }),
                data: None,
            })
        })
        .collect()
}

#[derive(Default)]
struct InstructionCollector {
    instructions: Vec<Span<Instruction>>,
}

impl Visit for InstructionCollector {
    fn visit_inst(&mut self, inst: &Span<Instruction>) -> core::ops::ControlFlow<()> {
        self.instructions.push(inst.clone());
        visit::visit_inst(self, inst)
    }
}

fn contract_stack_effect(
    inst: &Instruction,
    contracts: Option<&ContractStore>,
    resolver: &crate::symbol_resolution::SymbolResolver<'_>,
) -> Option<String> {
    let contracts = contracts?;
    let path = match inst {
        Instruction::Exec(target) | Instruction::Call(target) => resolver.resolve_target(target)?,
        _ => return None,
    };

    let contract = contracts
        .get(&path)
        .or_else(|| contracts.get_by_suffix(path.as_str()))?;

    match contract.stack_effect {
        StackEffect::Known { inputs, outputs } => {
            Some(format_stack_change_from_counts(inputs, outputs))
        }
        _ => Some("[...] → [...] (net effect: unknown)".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::span_to_range;
    use crate::util::to_miden_uri;
    use crate::{analysis::contracts::types::StackEffect, analysis::ContractStore};
    use miden_assembly_syntax::{Parse, ParseOptions};
    use miden_debug_types::{SourceLanguage, SourceManager};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn one_code_lens_per_instruction() {
        let source = "begin\n  push.1\n  push.2\n  add\nend\n";
        let uri = Url::parse("file:///tmp/code-lens-test.masm").unwrap();
        let miden_uri = to_miden_uri(&uri);

        let sources = DefaultSourceManager::default();
        sources.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());
        let file = sources.get_by_uri(&miden_uri).expect("source file");
        let module = file
            .clone()
            .parse_with_options(&sources, ParseOptions::default())
            .expect("parsed module");

        // Count instructions
        let mut counter = InstructionCollector::default();
        let _ = visit::visit_module(&mut counter, &module);
        let inst_count = counter.instructions.len();

        let lenses = collect_code_lenses(&module, &sources, None);
        assert_eq!(
            lenses.len(),
            inst_count,
            "expected one code lens per instruction"
        );
    }

    #[test]
    fn single_line_multiple_instructions_not_duplicated() {
        let source = "begin\n  push.1 push.2 add\nend\n";
        let uri = Url::parse("file:///tmp/code-lens-test2.masm").unwrap();
        let miden_uri = to_miden_uri(&uri);

        let sources = DefaultSourceManager::default();
        sources.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());
        let file = sources.get_by_uri(&miden_uri).expect("source file");
        let module = file
            .clone()
            .parse_with_options(&sources, ParseOptions::default())
            .expect("parsed module");

        let mut counter = InstructionCollector::default();
        let _ = visit::visit_module(&mut counter, &module);
        let inst_count = counter.instructions.len();
        assert_eq!(inst_count, 3, "expected 3 instructions in line");

        let lenses = collect_code_lenses(&module, &sources, None);
        assert_eq!(lenses.len(), inst_count);
    }

    #[test]
    fn call_uses_known_contract_stack_effect() {
        let source = "proc foo\n  add\nend\n\nbegin\n  exec.foo\nend\n";
        let uri = Url::parse("file:///tmp/code-lens-contract.masm").unwrap();
        let miden_uri = to_miden_uri(&uri);

        let sources = DefaultSourceManager::default();
        sources.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());
        let file = sources.get_by_uri(&miden_uri).expect("source file");
        let module = file
            .clone()
            .parse_with_options(&sources, ParseOptions::default())
            .expect("parsed module");

        let mut contracts = ContractStore::new();
        let path = crate::symbol_path::SymbolPath::from_module_and_name(&module, "foo");
        contracts.update_document(vec![crate::analysis::ProcContract {
            path: path.clone(),
            validates: crate::analysis::ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known {
                inputs: 2,
                outputs: 1,
            },
            signature: None,
            definition_range: None,
        }]);

        let lenses = collect_code_lenses(&module, &sources, Some(&contracts));
        assert_eq!(lenses.len(), 2, "expected one lens per instruction");

        // Find the exec instruction range to pick the right lens
        let mut collector = InstructionCollector::default();
        let _ = visit::visit_module(&mut collector, &module);
        let exec_inst = collector
            .instructions
            .iter()
            .find(|span| matches!(span.inner(), Instruction::Exec(_)))
            .expect("exec instruction");
        let exec_range = span_to_range(&sources, exec_inst.span()).expect("exec range");

        let exec_lens = lenses
            .iter()
            .find(|lens| lens.range == exec_range)
            .expect("exec lens present");
        let label = exec_lens.command.as_ref().unwrap().title.clone();
        assert_eq!(label, "[a, b, ...] → [c, ...] (net effect: -1)");
    }

    #[test]
    fn immediate_instruction_uses_constant_in_label() {
        let source = "begin\n  mul.5\nend\n";
        let uri = Url::parse("file:///tmp/code-lens-imm.masm").unwrap();
        let miden_uri = to_miden_uri(&uri);

        let sources = DefaultSourceManager::default();
        sources.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());
        let file = sources.get_by_uri(&miden_uri).expect("source file");
        let module = file
            .clone()
            .parse_with_options(&sources, ParseOptions::default())
            .expect("parsed module");

        let lenses = collect_code_lenses(&module, &sources, None);
        assert_eq!(lenses.len(), 1);
        let label = lenses[0].command.as_ref().unwrap().title.clone();
        assert_eq!(label, "[a, ...] → [5 * a, ...] (net effect: 0)");
    }
}
