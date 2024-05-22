def generate_ast(output_directory: str, base_type: str, types: dict[str, list[tuple[str, str]]]) -> None:

    # Enum Definition
    enum_definition = f"#[derive(Debug, Clone)]\nenum {base_type} {{\n"
    for t in types.keys():
        enum_definition += f"    {t}({t}),\n"
    enum_definition += "}\n"
    
    # Struct Definitions
    struct_definitions = ""
    for t, fields in types.items():
        struct_definitions += "#[derive(Debug, Clone)]\n"
        struct_definitions += f"struct {t} {{\n"
        for field_name, field_type in fields:
            struct_definitions += f"    {field_name}: {field_type},\n"
        struct_definitions += "}\n\n"
    
    with open(f"{output_directory}/{base_type}.rs", "w") as f:
        f.write(enum_definition + "\n" + struct_definitions)

types = {
    "Binary": [("lhs", "Box<Expression>"), ("rhs", "Box<Expression>"), ("kind", "Token")],
    "Unary": [("value", "Box<Expression>"), ("operator", "Token")],
    "Assign": [("variable", "Token"), ("value", "Box<Expression>")],
    "Call": [("callee", "Box<Expression>"), ("parentheses", "Token"), ("arguments", "Vec<Box<Expression>>")],
    "Get": [("object", "Box<Expression>"), ("name", "Token")],
    "Grouping": [("expression", "Box<Expression>")],
    "Literal": [("value", "Object")],
    "Logical": [("lhs", "Box<Expression>"), ("operator", "Token"), ("rhs", "Box<Expression>")],
    "Set": [("object", "Box<Expression>"), ("name", "Token"), ("value", "Box<Expression>")],
    "Super": [("keyword", "Token"), ("method", "Token")],
    "This": [("keyword", "Token")],
    "Variable": [("name", "Token")]
}

generate_ast(".", "Expression", types)
