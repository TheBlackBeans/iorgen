# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2019-2020 Sacha Delanoue
"""Generate an ADA parser"""

import textwrap
from typing import List, Optional, Set
from iorgen.types import Input, Type, TypeEnum, Variable
from iorgen.utils import IteratorName

KEYWORDS = [
    "abort", "abs", "abstract", "accept", "access", "aliased", "all", "and",
    "array", "at", "begin", "body", "case", "constant", "declare", "delay",
    "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
    "exit", "for", "function", "generic", "goto", "if", "in", "interface"
    "is", "limited", "loop", "mod", "new", "not", "null", "of", "or", "others",
    "out", "overriding", "package", "pragma", "private", "procedure",
    "protected", "raise", "range", "record", "rem", "renames", "requeue",
    "return", "reverse", "select", "separate", "some", "subtype",
    "synchronized", "tagged", "task", "terminate", "then", "type", "until",
    "use", "when", "while", "with", "xor"
] + ["integer", "character", "string", "ada"]

INDENTATION = "   "


def var_name(name: str) -> str:
    """Transform a variable name into a valid one for ADA"""
    candidate = "".join(i.lower().capitalize() for i in name.split())
    if candidate.lower() in KEYWORDS:
        # Note: Ada does not allow a variable name to end with an undescore
        # So there is no easy way for Iorgen to be sure variable names will be
        # uniques. That is why I chose to reserve the use of undescore for
        # Iorgen internals need. '_0' is used for conflict with keywords.
        return candidate + "_0"
    return candidate


def type_str(var: Variable, input_: Input, decl: bool = False) -> str:
    """Return the ADA name for a type"""
    # pylint: disable=too-many-return-statements
    if var.type.main == TypeEnum.INT:
        return "Integer"
    if var.type.main == TypeEnum.CHAR:
        return "Character"
    if var.type.main == TypeEnum.STR:
        return "String"
    if var.type.main == TypeEnum.STRUCT:
        return var_name(var.type.struct_name)

    assert var.type.main == TypeEnum.LIST
    assert var.type.encapsulated
    if var.type.encapsulated.main == TypeEnum.CHAR:
        return "String"
    if not decl:
        # TODO multidimentional vectors
        return ("V_{}.Vector" if is_vector(var.type, input_) else
                "T_{}").format(var_name(var.name))
    inner = var.type
    size = []
    unconstrained = False
    while inner.encapsulated:
        try:
            size.append("1 .. {}".format(int(inner.size)))
        except ValueError:
            size.append("Positive range <>")
            unconstrained = True
        inner = inner.encapsulated
    if inner.main == TypeEnum.STR:
        try:
            size.append("1 .. {}".format(int(inner.size)))
        except ValueError:
            size.append("Positive range <>")
            unconstrained = True
        inner = Type(TypeEnum.CHAR)
    if unconstrained:
        size = ["Positive range <>"] * len(size)
    return "array ({}) of {}".format(", ".join(size),
                                     type_str(Variable("", "", inner), input_))


def array_range(name: str, type_: Type, input_: Input) -> str:
    """Return the code to get a array's range"""
    if is_vector(type_, input_):
        return "1 .. {}".format(var_name(type_.size))
    structs = name.split(".")
    dimensions = structs[-1].count("(") + structs[-1].count(",") + 1
    if structs[-1].count("("):
        prefix = structs[-1][:structs[-1].find("(")]
        if len(structs) > 1:
            prefix = ".".join(structs[:-1]) + "." + prefix
        return "{}'Range({})".format(prefix, dimensions)
    return name + "'Range"


def sub_name(name: str, index: str) -> str:
    """Return the name of the variable accessed by an index"""
    # TODO won't work with multidimentional vector
    return "{}({})".format(name,
                           index) if name[-1] != ")" else "{}, {})".format(
                               name[:-1], index)


def is_integer(string: str) -> bool:
    """Does the string hold an integer?"""
    try:
        int(string)
        return True
    except ValueError:
        return False


def number_of_unconstrained(type_: Type, input_: Input) -> int:
    """Compute how unconstrained a type is: how many bounds will have to be
    specified later"""
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR):
        return 0
    if type_.main == TypeEnum.STR:
        return 0 if is_integer(type_.size) else 1
    if type_.main == TypeEnum.STRUCT:
        struct = input_.get_struct(type_.struct_name)
        sum_ = 0
        for field in struct.fields:
            sum_ += number_of_unconstrained(field.type, input_)
        return sum_
    assert type_.main == TypeEnum.LIST
    sum_ = 0
    inner = type_
    while inner.encapsulated:
        if not is_integer(inner.size):
            sum_ += 1
        inner = inner.encapsulated
    if inner.main == TypeEnum.STR:
        if not is_integer(inner.size):
            sum_ += 1
    return sum_


def is_unconstrained(type_: Type, input_: Input) -> bool:
    """Is the type unconstrained, meaning that we'll need to specify one or
    several sized for instanciation"""
    return number_of_unconstrained(type_, input_) != 0


def get_constrains(type_: Type,
                   input_: Input,
                   with_variables: bool = False,
                   placeholders: bool = False) -> str:
    if type_.main in (TypeEnum.INT, TypeEnum.CHAR):
        return ""
    if type_.main == TypeEnum.STR:
        try:
            return "1 .. {}".format(
                var_name(type_.size) if with_variables else int(type_.size))
        except ValueError:
            return "" if not placeholders else "1 .. {}"
    if type_.main == TypeEnum.STRUCT:
        struct = input_.get_struct(type_.struct_name)
        constraints = []
        for field in struct.fields:
            field_constraints = get_constrains(field.type, input_,
                                               with_variables, placeholders)
            if not field_constraints:
                if not with_variables:
                    return ""
            else:
                sizes = ", ".join(
                    i
                    for i in field_constraints.replace("1 .. ", "").split(", ")
                    if not with_variables or not is_integer(i))
                if sizes:
                    constraints.append(sizes)
        return ", ".join(constraints)
    assert type_.main == TypeEnum.LIST
    constraints = []
    inner = type_
    while inner.encapsulated:
        try:
            constraints.append("1 .. {}".format(
                var_name(inner.size) if with_variables else int(inner.size)))
        except ValueError:
            if placeholders:
                constraints.append("1 .. {}")
            else:
                return ""
        inner = inner.encapsulated
    if inner.main == TypeEnum.STR:
        try:
            constraints.append("1 .. {}".format(
                var_name(inner.size) if with_variables else int(inner.size)))
        except ValueError:
            if placeholders:
                constraints.append("1 .. {}")
            else:
                return ""
    if placeholders or with_variables or (
            type_.encapsulated and type_.encapsulated.main == TypeEnum.CHAR):
        return ", ".join(constraints)
    return ""


def is_vector(type_: Type, input_: Input) -> bool:
    """Is the type an Ada.Containers.Indefinite_Vectors?"""
    if type_.main != TypeEnum.LIST:
        return False
    inner = type_
    while inner.encapsulated:
        inner = inner.encapsulated
    return inner.main == TypeEnum.STRUCT and is_unconstrained(inner, input_)


def declare_type(var: Variable, packages: Set[str],
                 input_: Input) -> List[str]:
    """Declare a type for a variable that need it (unconstrained list and list
    of structs basically)"""
    if var.type.main != TypeEnum.LIST:
        return []
    assert var.type.encapsulated
    if var.type.encapsulated.main == TypeEnum.CHAR:
        return []
    if not is_vector(var.type, input_):
        return [
            INDENTATION + "type T_{} is {};".format(
                var_name(var.name), type_str(var, input_, True))
        ]
    packages.add("Ada.Containers.Indefinite_Vectors")
    inner = var.type
    while inner.encapsulated:
        inner = inner.encapsulated
    out = [
        INDENTATION +
        "package V_{} is new Ada.Containers.Indefinite_Vectors (Positive, {});"
        .format(var_name(var.name), var_name(inner.struct_name))
    ]
    return out


def declare_records(packages: Set[str], input_: Input) -> List[str]:
    """Declare the records (structs) present in the input"""
    ret = []
    for struct in input_.structs:
        for field in struct.fields:
            if field.type.main == TypeEnum.LIST:
                ret.extend(declare_type(field, packages, input_))
        n_constraints = number_of_unconstrained(
            Type(TypeEnum.STRUCT, struct_name=struct.name), input_)
        constraints = ""
        if n_constraints != 0:
            constraints = " (" + ", ".join(
                ("Size_{} : Natural".format(i + 1)
                 for i in range(n_constraints))) + ")"
        ret.append(INDENTATION +
                   "type {}{} is".format(var_name(struct.name), constraints))
        ret.append(INDENTATION * 2 + "-- " + struct.comment)
        ret.append(INDENTATION * 2 + "record")
        lines = []
        index = 1
        for field in struct.fields:
            type_ = type_str(field, input_)
            unconstrained = number_of_unconstrained(field.type, input_)
            if unconstrained != 0 or field.type.main == TypeEnum.STR:
                type_ += " ({})".format(
                    get_constrains(field.type, input_,
                                   placeholders=True).format(*[
                                       "Size_{}".format(i + index)
                                       for i in range(unconstrained)
                                   ]))
            index += unconstrained
            lines.append((var_name(field.name), type_, field.comment))
        max1 = max(len(i) for i, _, _ in lines)
        max2 = max(len(i) for _, i, _ in lines)
        for name, type_, comment in lines:
            ret.append(3 * INDENTATION + "{: <{}} : {: <{}} -- {}".format(
                name, max1, type_ + ";", max2 + 1, comment))
        ret.extend([INDENTATION * 2 + "end record;", ""])
    return ret


class ParserAda():
    """Create the Ada code to parse an input"""
    def __init__(self, input_data: Input) -> None:
        self.input = input_data

        self.packages = set()  # type: Set[str]
        self.additional_vars = set()  # type: Set[str]
        self.iterator = IteratorName([var.name for var in input_data.input])
        self.opened_scoped = 0

    def read_line(self, name: str, type_: Type, size: str,
                  indent_lvl: int) -> List[str]:
        """Read an entire line and store it into the right place(s)"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * (indent_lvl + self.opened_scoped)
        if type_.main == TypeEnum.INT:
            self.packages.add("Ada.Text_Io")
            self.packages.add("Ada.Integer_Text_Io")
            return [
                indent + "Ada.Integer_Text_Io.Get({});".format(name),
                indent + "Ada.Text_Io.Skip_Line;"
            ]
        if type_.main == TypeEnum.CHAR:
            self.packages.add("Ada.Text_Io")
            return [
                indent + "Ada.Text_Io.Get({});".format(name),
                indent + "Ada.Text_Io.Skip_Line;"
            ]
        if type_.main == TypeEnum.STR:
            self.packages.add("Ada.Text_Io")
            if name[-1] != ")":
                self.additional_vars.add("String_End")
                out = [
                    indent +
                    "Ada.Text_Io.Get_Line({}, String_End);".format(name)
                ]
                index = var_name(self.iterator.new_it())
                out.append(indent +
                           "for {} in String_End + 1 .. {}'Last loop".format(
                               index, name))
                out.append(indent + INDENTATION +
                           " {}({}) := Character'Val(0);".format(name, index))
                out.append(indent + "end loop;")
                self.iterator.pop_it()
                out.extend([
                    indent + "if String_End = 0 then",
                    indent + INDENTATION + "Ada.Text_Io.Skip_Line;",
                    indent + "end if;"
                ])
                return out
            out = []
            index = var_name(self.iterator.new_it())
            out.append(indent + "for {} in {} loop".format(
                index, array_range(name, type_, self.input)))
            out.append(INDENTATION + indent +
                       "if Ada.Text_IO.End_Of_Line then")
            out.append(2 * INDENTATION + indent +
                       "{} := Character'Val(0);".format(sub_name(name, index)))
            out.append(INDENTATION + indent + "else")
            out.append(2 * INDENTATION + indent +
                       'Ada.Text_Io.Get({});'.format(sub_name(name, index)))
            out.append(INDENTATION + indent + "end if;")
            out.append(indent + "end loop;")
            out.append(indent + "Ada.Text_Io.Skip_Line;")
            self.iterator.pop_it()
            return out
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            self.packages.add("Ada.Text_Io")
            if type_.encapsulated.main == TypeEnum.CHAR and "(" not in name:
                return [
                    indent + "Ada.Text_Io.Get({});".format(name),
                    indent + "Ada.Text_Io.Skip_Line;"
                ]
            out = []
            index = var_name(self.iterator.new_it())
            out.append(indent + "for {} in {} loop".format(
                index, array_range(name, type_, self.input)))
            out.append(INDENTATION + indent + 'Ada.{}Text_Io.Get({});'.format(
                "Integer_" if type_.encapsulated.main == TypeEnum.INT else "",
                sub_name(name, index)))
            out.append(indent + "end loop;")
            out.append(indent + "Ada.Text_Io.Skip_Line;")
            self.iterator.pop_it()
            return out
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        out = []
        self.packages.add("Ada.Text_Io")
        for i, field in enumerate(struct.fields):
            if field.type.main == TypeEnum.CHAR:
                self.packages.add("Ada.Text_Io")
                if i != 0:
                    out.append(indent + "Ada.Text_Io.Get({}.{});".format(
                        name, var_name(field.name)))
                out.append(indent + "Ada.Text_Io.Get({}.{});".format(
                    name, var_name(field.name)))
            else:
                self.packages.add("Ada.Integer_Text_Io")
                out.append(indent + "Ada.Integer_Text_Io.Get({}.{});".format(
                    name, var_name(field.name)))
        out.append(indent + "Ada.Text_Io.Skip_Line;")
        return out

    def read_unconstrained_struct(self, name: str, type_: Type) -> List[str]:
        """Read an unconstrained record"""
        assert type_.main == TypeEnum.STRUCT
        assert is_unconstrained(type_, self.input)
        out = []
        struct = self.input.get_struct(type_.struct_name)
        constraints = get_constrains(type_, self.input, True).split(", ")
        if struct.is_sized_struct():
            out.extend(self.read_lines("Size_1", Type(TypeEnum.INT), ""))
            constraints[0] = "Size_1"
        out.append(INDENTATION * self.opened_scoped + "declare")
        out.append(INDENTATION * (self.opened_scoped + 1) +
                   "{} : {} ({});".format(
                       name, type_str(Variable("", "", type_), self.input),
                       ", ".join(constraints)))
        out.append(INDENTATION * self.opened_scoped + "begin")
        if struct.is_sized_struct():
            out.append(INDENTATION * (self.opened_scoped + 1) +
                       "{}.{} := Size_1;".format(
                           name, var_name(struct.fields[0].name)))
        return out

    def read_lines(self,
                   name: str,
                   type_: Type,
                   size: str,
                   indent_lvl: int = 0) -> List[str]:
        """Read one or several lines and store them into the right place(s)"""
        if type_.fits_in_one_line(self.input.structs):
            return self.read_line(name, type_, size, indent_lvl)
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            out = []
            for f_name, f_type, f_size in struct.fields_name_type_size(
                    "{}.{{}}".format(name), var_name):
                if struct.is_sized_struct() and f_type.main == TypeEnum.INT:
                    continue
                out.extend(self.read_lines(f_name, f_type, f_size, indent_lvl))
            return out
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        out = []
        indent = INDENTATION * (indent_lvl + self.opened_scoped)
        index = var_name(self.iterator.new_it())
        sub = sub_name(name, index)
        if is_vector(type_, self.input):
            sub = name + "_E"
            out.append("{}for {} in {} loop".format(
                indent, index, array_range(name, type_, self.input)))
            self.opened_scoped += 1
            out.extend(self.read_unconstrained_struct(sub, type_.encapsulated))
            indent += INDENTATION
        else:
            out.append("{}for {} in {} loop".format(
                indent, index, array_range(name, type_, self.input)))
        out.extend(
            self.read_lines(sub, type_.encapsulated,
                            var_name(type_.encapsulated.size), indent_lvl + 1))
        if is_vector(type_, self.input):
            out.append(indent + INDENTATION +
                       "{}.Append({});".format(name, sub))
            out.append(indent + "end;")
            out.append(indent[:-len(INDENTATION)] + "end loop;")
            self.opened_scoped -= 1
        else:
            out.append(indent + "end loop;")
        self.iterator.pop_it()
        return out

    def call(self, reprint: bool) -> List[str]:
        """Declare and call the function take all inputs in arguments"""
        output = []
        arguments = []
        for i, arg in enumerate(self.input.input):
            arguments.append(
                (var_name(arg.name), type_str(arg, self.input) +
                 (";" if i != len(self.input.input) - 1 else ""), arg.comment))
        max1 = max(len(i) for i, _, _ in arguments)
        max2 = max(len(i) for _, i, _ in arguments)
        spaces = INDENTATION + " " * len("precedure {} (".format(
            var_name(self.input.name)))
        output.append(INDENTATION + "procedure {} ({}\n{}) is".format(
            var_name(self.input.name), ("\n" + spaces).join(
                "{: <{}} : {: <{}} -- {}".format(i, max1, j, max2, k)
                for i, j, k in arguments), spaces[:-1]))
        output.append(INDENTATION + "begin")
        if reprint:
            for var in self.input.input:
                output.extend(
                    self.print_lines(var_name(var.name), var.type,
                                     var_name(var.type.size), 2))
        else:
            output.extend(
                textwrap.wrap(self.input.output,
                              79,
                              initial_indent=INDENTATION * 2 + "-- TODO ",
                              subsequent_indent=INDENTATION * 2 + "-- "))
            output.append(INDENTATION * 2 + "null;")
        output.append(INDENTATION +
                      "end {};\n".format(var_name(self.input.name)))
        return output

    def print_line(self, name: str, type_: Type, size: str,
                   indent_lvl: int) -> List[str]:
        """Print the content of a var that holds in one line"""
        assert type_.fits_in_one_line(self.input.structs)
        indent = INDENTATION * indent_lvl
        if type_.main == TypeEnum.INT:
            self.packages.add("Ada.Text_Io")
            return [
                indent + "Ada.Integer_Text_Io.Put({}, 0);".format(name),
                indent + "Ada.Text_Io.New_Line;"
            ]
        if type_.main == TypeEnum.CHAR:
            return [
                indent + "Ada.Text_Io.Put({});".format(name),
                indent + "Ada.Text_Io.New_Line;"
            ]
        if type_.main == TypeEnum.STR:
            out = []
            index = var_name(self.iterator.new_it())
            out.append(indent + "for {} in {} loop".format(
                index, array_range(name, type_, self.input)))
            indexed_name = sub_name(name, index)
            out.append(indent + INDENTATION +
                       "if {} /= Character'Val(0) then".format(indexed_name))
            out.append(indent + 2 * INDENTATION +
                       "Ada.Text_Io.Put({});".format(indexed_name))
            out.append(indent + INDENTATION + "end if;")
            out.append(indent + "end loop;")
            out.append(indent + "Ada.Text_Io.New_Line;")
            self.iterator.pop_it()
            return out
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            if type_.encapsulated.main == TypeEnum.CHAR and "(" not in name:
                return [indent + 'Ada.Text_Io.Put_Line({});'.format(name)]
            index = var_name(self.iterator.new_it())
            out = []
            out.append(indent + "for {} in {} loop".format(
                index, array_range(name, type_, self.input)))
            if type_.encapsulated.main == TypeEnum.INT:
                out.append(indent + INDENTATION +
                           "Ada.Integer_Text_Io.Put({}, 0);".format(
                               sub_name(name, index)))
                out.append(indent + INDENTATION +
                           "if {} /= {} then".format(index, size))
                out.append(indent + INDENTATION * 2 + "Ada.Text_Io.Put(' ');")
                out.append(indent + INDENTATION + "end if;")
            elif name[-1] == ")":
                out.append(
                    indent + INDENTATION +
                    "Ada.Text_Io.Put({}, {}));".format(name[:-1], index))
            else:
                out.append(indent + INDENTATION +
                           "Ada.Text_Io.Put({}({}));".format(name, index))
            out.append(indent + "end loop;")
            out.append(indent + "Ada.Text_Io.New_Line;")
            self.iterator.pop_it()
            return out
        assert type_.main == TypeEnum.STRUCT
        struct = self.input.get_struct(type_.struct_name)
        out = []
        for i, field in enumerate(struct.fields):
            if field.type.main == TypeEnum.INT:
                out.append(indent +
                           "Ada.Integer_Text_Io.Put({}.{}, 0);".format(
                               name, var_name(field.name)))
            else:
                out.append(indent + "Ada.Text_Io.Put({}.{});".format(
                    name, var_name(field.name)))
            if i != len(struct.fields) - 1:
                out.append(indent + "Ada.Text_Io.Put(' ');")
        out.append(indent + "Ada.Text_Io.New_Line;")
        return out

    def print_lines(self,
                    name: str,
                    type_: Type,
                    size: str,
                    indent_lvl: int = 0) -> List[str]:
        """Print the content of a var that holds in one or more lines"""
        if type_.fits_in_one_line(self.input.structs):
            return self.print_line(name, type_, size, indent_lvl)
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            struct = self.input.get_struct(type_.struct_name)
            out = []
            for f_name, f_type, f_size in struct.fields_name_type_size(
                    "{}.{{}}".format(name), var_name):
                out.extend(self.print_lines(f_name, f_type, f_size,
                                            indent_lvl))
            return out
        if type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            index = var_name(self.iterator.new_it())
            out = [
                "{}for {} in {} loop".format(
                    INDENTATION * indent_lvl, index,
                    array_range(name, type_, self.input)),
            ]
            out.extend(
                self.print_lines(sub_name(name, index), type_.encapsulated,
                                 var_name(type_.encapsulated.size),
                                 indent_lvl + 1))
            out.append(INDENTATION * indent_lvl + "end loop;")
            self.iterator.pop_it()
            return out
        assert False
        return []

    def read_input(self) -> List[str]:
        """The code to actually read the input variables, without the variable
        declaration, the function call, and everything"""
        out = []
        for var in self.input.input:
            if is_unconstrained(var.type, self.input):
                if var.type.main == TypeEnum.STRUCT:
                    out.extend(
                        self.read_unconstrained_struct(var_name(var.name),
                                                       var.type))
                    self.opened_scoped += 1
                elif is_vector(var.type, self.input):
                    out.append(
                        INDENTATION * self.opened_scoped +
                        "{}.Reserve_capacity(Ada.Containers.Count_Type({}));".
                        format(var_name(var.name), var_name(var.type.size)))
                else:
                    assert var.type.main in (TypeEnum.LIST, TypeEnum.STR)
                    out.append(INDENTATION * self.opened_scoped + "declare")
                    sizes = []
                    inner = var.type  # type: Optional[Type]
                    while inner and inner.size != "":
                        sizes.append("1 .. {}".format(var_name(inner.size)))
                        inner = inner.encapsulated
                    out.append(INDENTATION * (self.opened_scoped + 1) +
                               "{} : {} ({});".format(
                                   var_name(var.name), type_str(
                                       var, self.input), ", ".join(sizes)))
                    out.append(INDENTATION * self.opened_scoped + "begin")
                    self.opened_scoped += 1
            out.extend(
                self.read_lines(var_name(var.name), var.type,
                                var_name(var.type.size)))
        return out

    def content(self, reprint: bool) -> str:
        """Return the parser content"""
        code_to_read_input = self.read_input()
        output = ""
        output += "procedure Main is\n"
        records = declare_records(self.packages, self.input)
        if records:
            output += "\n".join(records) + "\n"
        types = []
        for var in self.input.input:
            types.extend(declare_type(var, self.packages, self.input))
        if types:
            output += "\n".join(types) + "\n\n"
        output += "\n".join(self.call(reprint)) + "\n"
        declared_vars = [
            var for var in self.input.input
            if not is_unconstrained(var.type, self.input)
            or is_vector(var.type, self.input)
        ]
        extras_vars = []
        if any(i.is_sized_struct() for i in self.input.structs):
            extras_vars.append("Size_1")
        for extra_var in sorted(self.additional_vars):
            extras_vars.append(extra_var)
        max_var_name = max(len(var_name(i.name)) for i in declared_vars)
        if extras_vars:
            max_var_name = max(max_var_name, max(len(i) for i in extras_vars))
        for var in declared_vars:
            constraints_str = get_constrains(var.type, self.input)
            if constraints_str:
                constraints_str = " ({})".format(constraints_str)
            output += INDENTATION + "{: <{}} : {}{};\n".format(
                var_name(var.name), max_var_name, type_str(var, self.input),
                constraints_str)
        for extra_var in extras_vars:
            output += INDENTATION + "{: <{}} : Natural;\n".format(
                extra_var, max_var_name)

        output += "begin\n"
        for line in code_to_read_input:
            output += INDENTATION + line + "\n"
        output += INDENTATION * (self.opened_scoped + 1) + "{}({});\n".format(
            var_name(self.input.name), ", ".join(
                [var_name(i.name) for i in self.input.input]))
        for i in range(self.opened_scoped, 0, -1):
            output += INDENTATION * i + "end;\n"
        output += "end Main;\n"
        if self.packages:
            output = "\n".join(
                "with {};".format(i)
                for i in sorted(self.packages)) + "\n\n" + output
        return output


def gen_ada(input_data: Input, reprint: bool = False) -> str:
    """Generate an ADA code to parse input"""
    return ParserAda(input_data).content(reprint)
