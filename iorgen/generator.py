# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Sacha Delanoue
"""Helpers to generate, compile and run parsers for all supported languages"""

import subprocess
import os
from typing import Callable, Optional, List

from iorgen.types import Input
from iorgen.parser_c import gen_c
from iorgen.parser_cpp import gen_cpp
from iorgen.parser_haskell import gen_haskell
from iorgen.parser_ocaml import gen_ocaml
from iorgen.parser_php import gen_php
from iorgen.parser_python import gen_python
from iorgen.parser_rust import gen_rust


class Language:
    """Describe how to generate, compile and run, the parser for a language"""

    def __init__(self,
                 extension: str,
                 generator: Callable[[Input, bool], str],
                 compile_command: List[str],
                 exec_command: Optional[List[str]] = None) -> None:
        self.extension = extension
        self.generator = generator
        self.compile_command = compile_command
        self.exec_command = [] if exec_command is None else exec_command

    def compile(self, filename: str) -> str:
        """Compile the file at location 'filename'"""
        if self.compile_command:
            cwd = os.getcwd()
            os.chdir(os.path.dirname(filename))
            name = filename[:-len(self.extension) - 1]
            command = [i.format(name=name) for i in self.compile_command]
            subprocess.run(command + [filename], stdout=subprocess.DEVNULL)
            os.chdir(cwd)
            return name
        return filename

    def compile_and_run(self, filename: str, input_file: str) -> str:
        """Compile filename, and run the executable with input_file as stdin"""
        exe = self.compile(filename)
        out = ""
        with open(input_file) as sample_input:
            res = subprocess.run(
                self.exec_command + [exe],
                stdin=sample_input,
                stdout=subprocess.PIPE)
            out = res.stdout.decode()
        return out

    def generate(self, input_data: Input) -> str:
        """Generate an input parser with a function to complete"""
        return self.generator(input_data, False)


ALL_LANGUAGES = [
    Language(
        "c", gen_c,
        ["gcc", "-std=c11", "-Wall", "-Wextra", "-O2", "-lm", "-o", "{name}"]),
    Language("cpp", gen_cpp,
             ["g++", "-std=c++17", "-Wall", "-Wextra", "-O2", "-o", "{name}"]),
    Language("hs", gen_haskell,
             ["ghc", "-Wall", "-Wno-name-shadowing", "-dynamic", "-O2"]),
    Language("ml", gen_ocaml, ["ocamlopt", "-w", "A", "-o", "{name}"]),
    Language("php", gen_php, [], ["php"]),
    Language("py", gen_python, [], ["python3", "-S"]),
    Language("rs", gen_rust, ["rustc", "-W", "warnings", "-O"])
]