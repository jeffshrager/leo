import re

source = ["def fib n",
          "if n < 2",
          "return n",
          "end",
          "a = fib n - 1",
          "b = fib n - 2",
          "return a + b",
          "end",
          "",
          "main = fib 9",
          "print main",
          ]

import re

def parse(lines):
    lines = [l.strip() for l in lines if l.strip() and not l.strip().startswith("#")]
    ast = []
    stack = [ast]

    for line in lines:
        if m := re.match(r"^def (\w+) (\w+)$", line):
            func = {"type": "def", "name": m[1], "arg": m[2], "body": []}
            stack[-1].append(func)
            stack.append(func["body"])
        elif m := re.match(r"^if (.+)$", line):
            node = {"type": "if", "cond": m[1], "body": []}
            stack[-1].append(node)
            stack.append(node["body"])
        elif m := re.match(r"^return (.+)$", line):
            stack[-1].append({"type": "return", "expr": m[1]})
        elif m := re.match(r"^print (.+)$", line):
            stack[-1].append({"type": "print", "expr": m[1]})
        elif m := re.match(r"^(\w+)\s*=\s*(.+)$", line):
            stack[-1].append({"type": "assign", "var": m[1], "expr": m[2]})
        elif line == "end":
            stack.pop()
        else:
            raise SyntaxError(f"Unknown line: {line}")
    return ast

counter = 0
def temp():  # for generating temp variable names
    global counter
    counter += 1
    return f"tmp{counter}"

def compile_expr(expr):
    expr = expr.strip()

    # handle function calls like "fib 5" or "fib n - 1"
    if expr.startswith("fib "):
        parts = expr.split()
        fn_name = parts[0]
        arg_expr = " ".join(parts[1:])
        arg_tmp, arg_code = compile_expr(arg_expr)
        tmp = temp()
        return tmp, arg_code + [
            f"PUSH {arg_tmp}",
            f"CALL {fn_name}",
            f"MOV _retval {tmp}"
        ]

    # handle binary operations
    if "+" in expr:
        a, b = expr.split("+")
        a, b = a.strip(), b.strip()
        ta, ca = compile_expr(a)
        tb, cb = compile_expr(b)
        t = temp()
        return t, ca + cb + [f"ADD {ta} {tb} {t}"]
    elif "-" in expr:
        a, b = expr.split("-")
        a, b = a.strip(), b.strip()
        ta, ca = compile_expr(a)
        tb, cb = compile_expr(b)
        t = temp()
        return t, ca + cb + [f"SUB {ta} {tb} {t}"]

    # number literal or variable
    return expr, []

def compile_stmt(stmt, output):
    if stmt["type"] == "assign":
        t, code = compile_expr(stmt["expr"])
        output.extend(code)
        output.append(f"MOV {t} {stmt['var']}")
    elif stmt["type"] == "return":
        t, code = compile_expr(stmt["expr"])
        output.extend(code)
        output.append(f"RET {t}")
    elif stmt["type"] == "if":
        cond, _ = stmt["cond"].split("<")
        cond = cond.strip()
        t, code = compile_expr(cond)
        label = temp()
        output.extend(code)
        output.append(f"JGE {t} 2 {label}_end")
        for s in stmt["body"]:
            compile_stmt(s, output)
        output.append(f"LABEL {label}_end")
    elif stmt["type"] == "print":
        t, code = compile_expr(stmt["expr"])
        output.extend(code)
        output.append(f"PRINT {t}")

def compile_fn(fn):
    output = [f"LABEL {fn['name']}"]
    output.append(f"PARAM {fn['arg']}")
    for stmt in fn["body"]:
        compile_stmt(stmt, output)
    return output

def compile_all(ast):
    code = []
    main_code = []

    # Emit functions first
    for item in ast:
        if item["type"] == "def":
            code.extend(compile_fn(item))
        else:
            if item["type"] == "assign":
                compile_stmt(item, main_code)
            elif item["type"] == "print":
                compile_stmt(item, main_code)

    # Start program with jump to main code
    code.insert(0, "JMP main")
    code.append("LABEL main")
    code.extend(main_code)
    code.append("HALT")  # Add this!
    return code


def run(code):
    labels = {}
    pc = 0
    stack = []
    env_stack = [{}]  # Stack of environments
    output = []

    # === Label Map ===
    for i, line in enumerate(code):
        if line.startswith("LABEL "):
            labels[line.split()[1]] = i

    print("=== Label Map ===")
    for name, addr in labels.items():
        print(f"  {name} => PC {addr}")
    print("=================")

    def current_env():
        return env_stack[-1]

    def eval_arg(arg):
        return int(arg) if arg.isdigit() else current_env().get(arg, 0)

    while pc < len(code):
        instr = code[pc]
        parts = instr.split()
        op = parts[0]

        print(f"[PC={pc:02}] {instr}")

        if op == "LABEL":
            pass  # nothing to do

        elif op == "JMP":
            print(f"  → JMP to {parts[1]}")
            pc = labels[parts[1]]
            continue

        elif op == "PARAM":
            val = stack.pop()
            current_env()[parts[1]] = val
            print(f"  → PARAM {parts[1]} = {val}")

        elif op == "MOV":
            val = eval_arg(parts[1])
            current_env()[parts[2]] = val
            print(f"  → MOV {parts[2]} = {val}")

        elif op == "ADD":
            a = eval_arg(parts[1])
            b = eval_arg(parts[2])
            current_env()[parts[3]] = a + b
            print(f"  → ADD {a} + {b} = {current_env()[parts[3]]} → {parts[3]}")

        elif op == "SUB":
            a = eval_arg(parts[1])
            b = eval_arg(parts[2])
            current_env()[parts[3]] = a - b
            print(f"  → SUB {a} - {b} = {current_env()[parts[3]]} → {parts[3]}")

        elif op == "RET":
            val = eval_arg(parts[1])
            ret_to = stack.pop()
            env_stack.pop()  # exit current call's scope
            current_env()["_retval"] = val
            print(f"  → RET {val} → return to PC={ret_to}")
            pc = ret_to
            continue

        elif op == "JGE":
            val = eval_arg(parts[1])
            threshold = int(parts[2])
            label = parts[3]
            if val >= threshold:
                print(f"  → JGE {val} >= {threshold}, jumping to {label}")
                pc = labels[label]
                continue
            else:
                print(f"  → JGE {val} < {threshold}, not jumping")

        elif op == "PUSH":
            val = eval_arg(parts[1])
            stack.append(val)
            print(f"  → push argument => {val}")

        elif op == "CALL":
            arg = stack.pop()
            ret_addr = pc + 1
            stack.append(ret_addr)
            stack.append(arg)
            env_stack.append({})  # push new local scope
            print(f"  → CALL {parts[1]}")
            print(f"    pop arg => {arg}")
            print(f"    push return address => {ret_addr}")
            pc = labels[parts[1]]
            continue

        elif op == "PRINT":
            val = eval_arg(parts[1])
            output.append(str(val))
            print(f"  → PRINT {val}")

        elif op == "HALT":
            print("  → HALT")
            break

        else:
            print(f"!! Unknown instruction: {instr}")
            break

        pc += 1

    print("=== Program Output ===")
    print("\n".join(output))

def display_ast_tree(ast, indent=0):
    pad = '  ' * indent
    if isinstance(ast, list):
        for item in ast:
            display_ast_tree(item, indent)
    elif isinstance(ast, dict):
        print(f"{pad}{{")
        for key, value in ast.items():
            print(f"{pad}  {repr(key)}:", end=" ")
            if isinstance(value, (dict, list)):
                print()
                display_ast_tree(value, indent + 2)
            else:
                print(repr(value))
        print(f"{pad}}}")
    else:
        print(f"{pad}{repr(ast)}")


def print_ast(ast, indent=0):
    pad = '  ' * indent
    for node in ast:
        if node["type"] == "def":
            print(f"{pad}def {node['name']} {node['arg']}")
            print_ast(node["body"], indent + 1)
            print(f"{pad}end")
        elif node["type"] == "if":
            print(f"{pad}if {node['cond']}")
            print_ast(node["body"], indent + 1)
            print(f"{pad}end")
        elif node["type"] == "assign":
            print(f"{pad}{node['var']} = {node['expr']}")
        elif node["type"] == "return":
            print(f"{pad}return {node['expr']}")
        elif node["type"] == "print":
            print(f"{pad}print {node['expr']}")
        else:
            print(f"{pad}??? {node}")


def test():
  ast = parse(source)
  display_ast_tree(ast)
  code = compile_all(ast)
  print(code)
  run(code)

test()

