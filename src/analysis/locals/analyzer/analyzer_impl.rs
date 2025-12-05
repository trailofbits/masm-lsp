//! Analyzer for detecting uninitialized local variable access.

use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Span};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

use crate::analysis::call_effect::{resolve_call_effect, CallEffect};
use crate::analysis::contracts::ContractStore;
use crate::analysis::static_effect::{apply_stack_manipulation, StackLike, StackLikeResult};
use crate::analysis::utils::u16_imm_to_u16;
use crate::diagnostics::{span_to_range, SOURCE_ANALYSIS};

use crate::analysis::locals::state::{LocalState, LocalsState};

#[derive(Clone, Debug, Default)]
enum StackElement {
    LocalAddress(u16),
    #[default]
    Other,
}

#[derive(Clone, Debug, Default)]
struct AddressStack {
    elements: Vec<StackElement>,
}

impl AddressStack {
    fn new() -> Self {
        Self::default()
    }

    fn get_local_at(&self, n: usize) -> Option<u16> {
        match self.peek(n) {
            Some(StackElement::LocalAddress(idx)) => Some(*idx),
            _ => None,
        }
    }

    fn clear(&mut self) {
        self.elements.clear();
    }
}

impl StackLike for AddressStack {
    type Element = StackElement;

    fn depth(&self) -> usize {
        self.elements.len()
    }

    fn push(&mut self, elem: StackElement) {
        self.elements.push(elem);
    }

    fn pop(&mut self) -> StackElement {
        self.elements.pop().unwrap_or_default()
    }

    fn peek(&self, n: usize) -> Option<&StackElement> {
        let depth = self.elements.len();
        if n < depth {
            Some(&self.elements[depth - 1 - n])
        } else {
            None
        }
    }

    fn ensure_depth(&mut self, needed: usize) {
        while self.elements.len() < needed {
            self.elements.insert(0, StackElement::Other);
        }
    }

    fn swap(&mut self, a: usize, b: usize) {
        let max_idx = a.max(b);
        self.ensure_depth(max_idx + 1);
        let len = self.elements.len();
        let idx_a = len - 1 - a;
        let idx_b = len - 1 - b;
        self.elements.swap(idx_a, idx_b);
    }

    fn movup(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let len = self.elements.len();
        let idx = len - 1 - n;
        let elem = self.elements.remove(idx);
        self.elements.push(elem);
    }

    fn movdn(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let elem = self.elements.pop().unwrap();
        let len = self.elements.len();
        let idx = len.saturating_sub(n);
        self.elements.insert(idx, elem);
    }
}

#[derive(Clone, Debug)]
struct LocalsFinding {
    message: String,
    span: SourceSpan,
    severity: DiagnosticSeverity,
}

pub struct LocalsAnalyzer<'a> {
    source_manager: &'a DefaultSourceManager,
    #[allow(dead_code)]
    uri: Url,
    contracts: Option<&'a ContractStore>,
    findings: Vec<LocalsFinding>,
}

impl<'a> LocalsAnalyzer<'a> {
    pub fn new(source_manager: &'a DefaultSourceManager, uri: Url) -> Self {
        Self {
            source_manager,
            uri,
            contracts: None,
            findings: Vec::new(),
        }
    }

    pub fn with_contracts(
        source_manager: &'a DefaultSourceManager,
        uri: Url,
        contracts: &'a ContractStore,
    ) -> Self {
        Self {
            source_manager,
            uri,
            contracts: Some(contracts),
            findings: Vec::new(),
        }
    }

    pub fn analyze(mut self, module: &Module) -> Vec<Diagnostic> {
        let _ = visit::visit_module(&mut self, module);
        self.build_diagnostics()
    }

    fn analyze_procedure_body(&mut self, proc: &Procedure) {
        let mut state = LocalsState::new();
        let mut addr_stack = AddressStack::new();
        self.analyze_block(proc.body(), &mut state, &mut addr_stack);
    }

    fn analyze_block(
        &mut self,
        block: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        for op in block.iter() {
            self.analyze_op(op, state, addr_stack);
        }
    }

    fn analyze_op(&mut self, op: &Op, state: &mut LocalsState, addr_stack: &mut AddressStack) {
        match op {
            Op::Inst(inst) => self.analyze_instruction(inst, state, addr_stack),
            Op::If {
                then_blk, else_blk, ..
            } => self.analyze_if_else(then_blk, else_blk, state, addr_stack),
            Op::While { body, .. } => self.analyze_while(body, state, addr_stack),
            Op::Repeat { count, body, .. } => self.analyze_repeat(*count, body, state, addr_stack),
        }
    }

    fn analyze_if_else(
        &mut self,
        then_blk: &Block,
        else_blk: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        addr_stack.pop();

        let mut then_state = state.clone();
        let mut else_state = state.clone();
        let mut then_addr_stack = addr_stack.clone();
        let mut else_addr_stack = addr_stack.clone();

        self.analyze_block(then_blk, &mut then_state, &mut then_addr_stack);
        self.analyze_block(else_blk, &mut else_state, &mut else_addr_stack);

        *state = then_state.meet(&else_state);
    }

    fn analyze_while(
        &mut self,
        body: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        let entry_state = state.clone();

        let mut body_addr_stack = addr_stack.clone();
        self.analyze_block(body, state, &mut body_addr_stack);

        *state = entry_state.meet(state);
    }

    fn analyze_repeat(
        &mut self,
        count: u32,
        body: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        if count == 0 {
            return;
        }

        let mut body_addr_stack = addr_stack.clone();
        self.analyze_block(body, state, &mut body_addr_stack);
    }

    fn analyze_instruction(
        &mut self,
        inst: &Span<Instruction>,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        let span = inst.span();

        if apply_stack_manipulation(addr_stack, inst.inner()) == StackLikeResult::Applied {
            return;
        }

        match inst.inner() {
            Instruction::LocStore(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    state.init_single(idx);
                }
                addr_stack.pop();
            }
            Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    state.init_word(idx);
                }
                addr_stack.pop_n(4);
            }
            Instruction::LocLoad(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_single_read(idx, span, state);
                }
                addr_stack.push(StackElement::Other);
            }
            Instruction::LocLoadWBe(idx) | Instruction::LocLoadWLe(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_word_read(idx, span, state);
                }
                addr_stack.pop_n(4);
                addr_stack.push_defaults(4);
            }
            Instruction::Locaddr(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    if self.contracts.is_none() {
                        self.check_address_escape(idx, span, state);
                    }
                    addr_stack.push(StackElement::LocalAddress(idx));
                } else {
                    addr_stack.push(StackElement::Other);
                }
            }
            Instruction::Exec(target)
            | Instruction::Call(target)
            | Instruction::SysCall(target) => {
                self.handle_procedure_call(target, state, addr_stack);
            }
            Instruction::DynExec | Instruction::DynCall => {
                addr_stack.pop_n(4);
            }
            Instruction::MemLoad => {
                addr_stack.pop();
                addr_stack.push(StackElement::Other);
            }
            Instruction::MemLoadImm(_) => {
                addr_stack.push(StackElement::Other);
            }
            Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
                addr_stack.pop();
                addr_stack.pop_n(4);
                addr_stack.push_defaults(4);
            }
            Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
                addr_stack.pop_n(4);
                addr_stack.push_defaults(4);
            }
            Instruction::MemStore => {
                addr_stack.pop();
                addr_stack.pop();
            }
            Instruction::MemStoreImm(_) => {
                addr_stack.pop();
            }
            Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
                addr_stack.pop();
                addr_stack.pop_n(4);
            }
            Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
                addr_stack.pop_n(4);
            }
            _ => {
                let actions = crate::analysis::dispatcher::actions_for(inst.inner());
                for action in actions {
                    match action {
                        crate::analysis::dispatcher::Action::Pop(n) => addr_stack.pop_n(n),
                        crate::analysis::dispatcher::Action::Push { count, .. } => {
                            addr_stack.push_defaults(count)
                        }
                        crate::analysis::dispatcher::Action::Stack(_) => {}
                        crate::analysis::dispatcher::Action::Unknown => {}
                    }
                }
            }
        }
    }

    fn handle_procedure_call(
        &mut self,
        target: &InvocationTarget,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        let target_name = match target {
            InvocationTarget::Symbol(ident) => ident.as_str(),
            InvocationTarget::Path(path) => path.inner().as_str(),
            InvocationTarget::MastRoot(_) => {
                addr_stack.clear();
                return;
            }
        };

        let contract = self.contracts.and_then(|store| {
            store
                .get_by_suffix(target_name)
                .or_else(|| store.get_by_name(target_name))
        });

        match contract {
            Some(c) => match resolve_call_effect(None, self.contracts, target) {
                CallEffect::Known { inputs, outputs } => {
                    if let Some(sig) = c.signature.as_ref() {
                        for (i, kind) in sig.inputs.iter().enumerate() {
                            if kind.is_output() {
                                if let Some(local_idx) = addr_stack.get_local_at(i) {
                                    state.init_single(local_idx);
                                }
                            }
                        }
                    }
                    addr_stack.pop_n(inputs);
                    addr_stack.push_defaults(outputs);
                }
                CallEffect::KnownInputs { inputs } => {
                    addr_stack.pop_n(inputs);
                    addr_stack.clear();
                }
                CallEffect::Unknown => {
                    addr_stack.clear();
                }
            },
            None => {
                addr_stack.clear();
            }
        }
    }

    fn check_single_read(&mut self, idx: u16, span: SourceSpan, state: &LocalsState) {
        match state.get(idx) {
            LocalState::Uninitialized => {
                self.findings.push(LocalsFinding {
                    message: format!("Read from uninitialized local {}.", idx),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::MaybeInitialized => {
                self.findings.push(LocalsFinding {
                    message: format!("Local {} may not be initialized on all code paths.", idx),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::Initialized => {}
        }
    }

    fn check_word_read(&mut self, base_idx: u16, span: SourceSpan, state: &LocalsState) {
        if let Some((idx, local_state)) = state.first_uninitialized_in_word(base_idx) {
            match local_state {
                LocalState::Uninitialized => {
                    self.findings.push(LocalsFinding {
                        message: format!(
                            "Read from uninitialized local {} (in word starting at {}).",
                            idx, base_idx
                        ),
                        span,
                        severity: DiagnosticSeverity::WARNING,
                    });
                }
                LocalState::MaybeInitialized => {
                    self.findings.push(LocalsFinding {
                        message: format!(
                            "Local {} may not be initialized on all code paths (in word starting at {}).",
                            idx, base_idx
                        ),
                        span,
                        severity: DiagnosticSeverity::WARNING,
                    });
                }
                LocalState::Initialized => {}
            }
        }
    }

    fn check_address_escape(&mut self, idx: u16, span: SourceSpan, state: &LocalsState) {
        match state.get(idx) {
            LocalState::Uninitialized => {
                self.findings.push(LocalsFinding {
                    message: format!(
                        "Taking address of uninitialized local {}. If passed to another procedure, it may read garbage.",
                        idx
                    ),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::MaybeInitialized => {
                self.findings.push(LocalsFinding {
                    message: format!(
                        "Taking address of local {} which may not be initialized on all code paths.",
                        idx
                    ),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::Initialized => {}
        }
    }

    fn build_diagnostics(&self) -> Vec<Diagnostic> {
        self.findings
            .iter()
            .map(|finding| self.finding_to_diagnostic(finding))
            .collect()
    }

    fn finding_to_diagnostic(&self, finding: &LocalsFinding) -> Diagnostic {
        let range = span_to_range(self.source_manager, finding.span)
            .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));

        Diagnostic {
            range,
            severity: Some(finding.severity),
            code: None,
            code_description: None,
            source: Some(SOURCE_ANALYSIS.to_string()),
            message: finding.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        }
    }
}

impl<'a> Visit for LocalsAnalyzer<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> ControlFlow<()> {
        self.analyze_procedure_body(proc);
        ControlFlow::Continue(())
    }
}

pub fn analyze_locals(
    module: &Module,
    source_manager: &DefaultSourceManager,
    uri: &Url,
) -> Vec<Diagnostic> {
    let analyzer = LocalsAnalyzer::new(source_manager, uri.clone());
    analyzer.analyze(module)
}

pub fn analyze_locals_with_contracts(
    module: &Module,
    source_manager: &DefaultSourceManager,
    uri: &Url,
    contracts: &ContractStore,
) -> Vec<Diagnostic> {
    let analyzer = LocalsAnalyzer::with_contracts(source_manager, uri.clone(), contracts);
    analyzer.analyze(module)
}
