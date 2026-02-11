#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional


ANSI_RESET = "\x1b[0m"
ANSI_GRAY = "\x1b[90m"
ANSI_BLUE = "\x1b[94m"
ANSI_YELLOW = "\x1b[93m"
ANSI_RED = "\x1b[91m"


class Logger:
    def __init__(self) -> None:
        self.use_color = sys.stderr.isatty()

    def _emit(self, label: str, color: str, message: str) -> None:
        prefix = f"{label}:"
        if self.use_color:
            sys.stderr.write(f"{color}{prefix}{ANSI_RESET} {message}\n")
        else:
            sys.stderr.write(f"{prefix} {message}\n")

    def trace(self, message: str) -> None:
        self._emit("trace", ANSI_GRAY, message)

    def info(self, message: str) -> None:
        self._emit("info", ANSI_BLUE, message)

    def warn(self, message: str) -> None:
        self._emit("warning", ANSI_YELLOW, message)

    def error(self, message: str) -> None:
        self._emit("error", ANSI_RED, message)


@dataclass
class Instruction:
    name: str
    description: str
    stack_effect: str


def replace_html_breaks(text: str, replacement: str) -> str:
    return re.sub(r"<br\s*/?>", replacement, text, flags=re.IGNORECASE)


def normalize_whitespace(text: str) -> str:
    return re.sub(r"\s+", " ", text).strip()


def unescape_pipes(text: str) -> str:
    return text.replace("\\|", "|")


def clean_cell_text(text: str, break_replacement: str = " ") -> str:
    text = replace_html_breaks(text, break_replacement)
    text = unescape_pipes(text)
    return normalize_whitespace(text)


def strip_markdown(text: str) -> str:
    return normalize_whitespace(text.replace("`", ""))


def split_markdown_row(line: str) -> List[str]:
    stripped = line.strip()
    if stripped.startswith("|"):
        stripped = stripped[1:]
    if stripped.endswith("|"):
        stripped = stripped[:-1]

    cells: List[str] = []
    current: List[str] = []
    in_code = False
    escaped = False

    for ch in stripped:
        if escaped:
            current.append(ch)
            escaped = False
            continue
        if ch == "\\":
            escaped = True
            current.append(ch)
            continue
        if ch == "`":
            in_code = not in_code
            current.append(ch)
            continue
        if ch == "|" and not in_code:
            cells.append("".join(current).strip())
            current = []
        else:
            current.append(ch)

    cells.append("".join(current).strip())
    return cells


def is_separator_row(line: str) -> bool:
    stripped = line.strip()
    if stripped.startswith(("+|", "-|")):
        stripped = stripped[1:]
    if not stripped:
        return False
    if "-" not in stripped:
        return False
    cleaned = stripped.replace("|", "").replace(":", "").replace("-", "").replace(" ", "")
    return cleaned == ""


def header_indices(cells: List[str]) -> Optional[Dict[str, int]]:
    mapping: Dict[str, int] = {}
    for idx, cell in enumerate(cells):
        normalized = strip_markdown(cell).lower()
        if normalized == "instruction":
            mapping["instruction"] = idx
        elif normalized == "stack input":
            mapping["stack_input"] = idx
        elif normalized == "stack output":
            mapping["stack_output"] = idx
        elif normalized == "notes":
            mapping["notes"] = idx
    if "instruction" in mapping and "stack_input" in mapping and "stack_output" in mapping:
        return mapping
    return None


def split_instruction_cell(cell: str) -> List[str]:
    cell = replace_html_breaks(cell, "\n")
    parts = [part.strip() for part in cell.splitlines() if part.strip()]
    names: List[str] = []
    for part in parts:
        name = strip_markdown(part)
        if name:
            names.append(name)
    return names


@dataclass
class TemplateResult:
    name: str
    token: Optional[str] = None


def template_placeholders(_token: str) -> tuple[str, str]:
    return "{n}", "{nth}"


def templatize_instruction_name(name: str) -> TemplateResult:
    patterns = [
        (r"^(?P<base>.+)\.a\.\.\.$", "a", "{base}.{placeholder}"),
        (r"^(?P<base>.+)\.uxx$", "xx", "{base}.u{placeholder}"),
        (r"^(?P<base>.+)\.<(?P<token>[^>]+)>$", None, "{base}.{placeholder}"),
        (r"^(?P<base>.+)\.name$", "name", "{base}.{placeholder}"),
        (r"^(?P<base>.+)\.n$", "n", "{base}.{placeholder}"),
        (r"^(?P<base>.+)\.i$", "i", "{base}.{placeholder}"),
        (r"^(?P<base>.+)\.a$", "a", "{base}.{placeholder}"),
        (r"^(?P<base>.+)\.b$", "b", "{base}.{placeholder}"),
    ]

    for pattern, token, template in patterns:
        match = re.match(pattern, name)
        if not match:
            continue
        base = match.group("base")
        if token is None:
            token = match.group("token")
        placeholder, _ = template_placeholders(token)
        return TemplateResult(
            name=template.format(base=base, placeholder=placeholder),
            token=token,
        )

    return TemplateResult(name=name, token=None)


def apply_template_token(text: str, token: Optional[str]) -> str:
    if not token:
        return text
    placeholder, ordinal_placeholder = template_placeholders(token)
    escaped = re.escape(token)
    text = re.sub(rf"`{escaped}`th", ordinal_placeholder, text)
    text = re.sub(rf"`{escaped}`-th", ordinal_placeholder, text)
    text = text.replace(f"`{token}`", placeholder)
    text = text.replace(f"<{token}>", placeholder)
    return text


def normalize_description(text: str) -> str:
    text = strip_markdown(text)
    text = text.replace("$$", "").replace("$", "")
    text = re.sub(r"\\begin\{cases\}", "", text)
    text = re.sub(r"\\end\{cases\}", "", text)
    text = re.sub(r"\\text\{([^}]*)\}", r"\1", text)

    replacements = {
        r"geq": "≥",
        r"leq": "≤",
        r"neq": "≠",
        r"cdot": "*",
        r"bmod": "mod",
        r"log_2": "log2",
        r"min": "min",
        r"max": "max",
        r"forall": "for all",
        r"lfloor": "floor(",
        r"rfloor": ")",
    }
    for key, value in replacements.items():
        text = re.sub(rf"\\+{key}", value, text)

    text = text.replace("\\\\", " ")
    text = text.replace("\\", "")
    text = re.sub(r"\^\{([^}]+)\}", r"^\1", text)
    text = text.replace("&", "")
    return normalize_whitespace(text)


def normalize_stack_part(text: str) -> str:
    text = strip_markdown(text)
    text = text.replace("[", "(").replace("]", ")")
    text = re.sub(r"\(\s+", "(", text)
    text = re.sub(r"\s+\)", ")", text)
    return normalize_whitespace(text)


def build_stack_effect(stack_input: str, stack_output: str) -> str:
    if stack_input and stack_output:
        return f"{stack_input} → {stack_output}"
    if stack_input:
        return stack_input
    if stack_output:
        return f"→ {stack_output}"
    return ""


def parse_instruction_reference(text: str, logger: Logger) -> List[Instruction]:
    instructions: List[Instruction] = []
    seen: Dict[str, int] = {}
    lines = text.splitlines()
    idx = 0

    while idx < len(lines):
        line = lines[idx]
        if "|" not in line:
            idx += 1
            continue
        header = header_indices(split_markdown_row(line))
        if header is None:
            idx += 1
            continue
        if idx + 1 >= len(lines) or not is_separator_row(lines[idx + 1]):
            idx += 1
            continue

        idx += 2
        while idx < len(lines):
            row = lines[idx]
            if not row.strip():
                break
            if "|" not in row:
                break
            if is_separator_row(row):
                idx += 1
                continue

            cells = split_markdown_row(row)
            if len(cells) <= max(header.values()):
                logger.warn(f"Skipping malformed row at line {idx + 1}: {row.strip()}")
                idx += 1
                continue

            instr_cell = cells[header["instruction"]]
            stack_in_cell = cells[header["stack_input"]]
            stack_out_cell = cells[header["stack_output"]]
            notes_cell = cells[header["notes"]] if "notes" in header else ""

            names = split_instruction_cell(instr_cell)
            if not names:
                logger.warn(f"No instruction names found at line {idx + 1}: {row.strip()}")
                idx += 1
                continue

            stack_input_raw = clean_cell_text(stack_in_cell)
            stack_output_raw = clean_cell_text(stack_out_cell)
            description_raw = clean_cell_text(notes_cell, break_replacement="; ")

            for name in names:
                template = templatize_instruction_name(name)
                templated_description = apply_template_token(
                    description_raw, template.token
                )
                templated_stack_effect = apply_template_token(
                    build_stack_effect(
                        normalize_stack_part(stack_input_raw),
                        normalize_stack_part(stack_output_raw),
                    ),
                    template.token,
                )
                templated_description = normalize_description(templated_description)
                templated_name = template.name
                if templated_name in seen:
                    logger.warn(
                        "Duplicate instruction '{0}' at line {1} (already seen at line {2})".format(
                            templated_name,
                            idx + 1,
                            seen[templated_name],
                        )
                    )
                    continue
                seen[templated_name] = idx + 1
                instructions.append(
                    Instruction(
                        name=templated_name,
                        description=templated_description,
                        stack_effect=templated_stack_effect,
                    )
                )
            idx += 1
        idx += 1

    return instructions


def toml_escape(value: str) -> str:
    escaped = value.replace("\\", "\\\\").replace('"', "\\\"")
    return f'"{escaped}"'


def render_toml(instructions: Iterable[Instruction]) -> str:
    lines: List[str] = [
        "# Generated from instruction_reference.md",
        "",
    ]
    for instr in instructions:
        lines.append("[[instructions]]")
        lines.append(f"name = {toml_escape(instr.name)}")
        lines.append(f"description = {toml_escape(instr.description)}")
        lines.append(f"stack_effect = {toml_escape(instr.stack_effect)}")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Convert MASM instruction_reference.md to a TOML list of instructions."
    )
    parser.add_argument(
        "-i",
        "--input-file",
        dest="input_path",
        required=True,
        type=Path,
        help="Path to docs/instruction_reference.md",
    )
    parser.add_argument(
        "-o",
        "--output-file",
        dest="output_path",
        type=Path,
        default=None,
        help="Optional output path for TOML (defaults to stdout)",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    logger = Logger()

    if not args.input_path.exists():
        logger.error(f"Input file not found: {args.input_path}")
        return 1

    logger.trace(f"Reading {args.input_path}")
    text = args.input_path.read_text(encoding="utf-8")
    instructions = parse_instruction_reference(text, logger)
    logger.info(f"Parsed {len(instructions)} instructions")

    output = render_toml(instructions)
    if args.output_path is None:
        sys.stdout.write(output)
        return 0

    args.output_path.parent.mkdir(parents=True, exist_ok=True)
    logger.trace(f"Writing TOML to {args.output_path}")
    args.output_path.write_text(output, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
