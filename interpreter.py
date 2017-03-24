
class Unit(object):

    def __init__(self, name, val):
        self.name = name
        self.value = val

    def __str__(self):
        return ":unit:"

def interpreter(input_name, output_name):

    BOOLS = [":true:", ":false:"]
    stack = []
    bindings = {}

    with open(input_name, "r") as file:

        for line in file.readlines():
            line = "".join(line.split())

            if "push" in line:
                if len(line.split('"')) == 3:
                    stack.append('"' + line.split('"')[1] + '"')
                else:
                    try:
                        stack.append(int(line.replace("push", "")))
                    except:
                        if '"' in line:
                            stack.append(line.replace("push", ""))
                        elif line.replace("push", "") in bindings:
                            stack.append(bindings[line.replace("push", "")].value)
                        else:
                            stack.append(line.replace("push", ""))

            elif ":error:" in line:
                stack.append(":error:")
            elif line in [":true:", ":false:"]:
                stack.append(line)

            elif "add" in line:
                if len(stack) < 2:
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        stack.append(stack.pop() + stack.pop())
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int) and isinstance(stack[-1].value, int):
                        stack.append(stack.pop().value + stack.pop())
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        stack.append(stack.pop() + stack.pop().value)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-1].value, int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        stack.append(stack.pop().value + stack.pop().value)
                    else:
                        stack.append(":error:")

            elif "sub" in line:
                if len(stack) < 2:
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        y = stack.pop()
                        x = stack.pop()
                        stack.append(x - y)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int) and isinstance(stack[-1].value, int):
                        y = stack.pop().value
                        x = stack.pop()
                        stack.append(x - y)
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        y = stack.pop()
                        x = stack.pop().value
                        stack.append(x - y)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-1].value, int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        y = stack.pop().value
                        x = stack.pop().value
                        stack.append(x - y)
                    else:
                        stack.append(":error:")

            elif "mul" in line:
                if len(stack) < 2:
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        stack.append(stack.pop() * stack.pop())
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int) and isinstance(stack[-1].value, int):
                        stack.append(stack.pop().value * stack.pop())
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        stack.append(stack.pop() * stack.pop().value)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-1].value, int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        stack.append(stack.pop().value * stack.pop().value)
                    else:
                        stack.append(":error:")

            elif "div" in line:
                if len(stack) < 2:
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        y = stack.pop()
                        x = stack.pop()
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x // y)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int) and isinstance(stack[-1].value, int):
                        y = stack.pop().value
                        x = stack.pop()
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x // y)
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        y = stack.pop()
                        x = stack.pop().value
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x // y)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-1].value, int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        y = stack.pop().value
                        x = stack.pop().value
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x // y)
                    else:
                        stack.append(":error:")

            elif "rem" in line:
                if len(stack) < 2:
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        y = stack.pop()
                        x = stack.pop()
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x % y)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int) and isinstance(stack[-1].value, int):
                        y = stack.pop().value
                        x = stack.pop()
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x % y)
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        y = stack.pop()
                        x = stack.pop().value
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x % y)
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-1].value, int) and isinstance(stack[-2], Unit) and isinstance(stack[-2].value, int):
                        y = stack.pop().value
                        x = stack.pop().value
                        if y == 0:
                            stack.append(x)
                            stack.append(y)
                            stack.append(":error:")
                        else:
                            stack.append(x % y)
                    else:
                        stack.append(":error:")

            elif "neg" in line:
                if len(stack) < 1:
                    stack.append(":error:")
                else:
                    to_neg = stack.pop() if isinstance(stack[-1], int) else stack.pop().value if isinstance(stack[-1], Unit) else ":error:"
                    if to_neg == ":error:":
                        stack.append(to_neg)
                    else:
                        stack.append(to_neg * -1)

            elif "swap" in line:
                if len(stack) < 2:
                    stack.append(":error:")
                else:
                    y = stack.pop()
                    x = stack.pop()
                    stack.append(y)
                    stack.append(x)

            elif "quit" in line:
                with open(output_name, "w") as output:
                    while len(stack) != 0:
                        value = stack.pop()
                        if isinstance(value, Unit):
                            print("%s => %s" % (value.name, value.value))
                        output.write("%s\n" % value)

            elif "pop" in line:
                if len(stack) == 0:
                    stack.append(":error:")
                else:
                    stack.pop()

            elif "and" in line:
                if len(stack) < 2:
                    print("Error and")
                    stack.append(":error:")
                if (isinstance(stack[-1], str) and stack[-1] in BOOLS) and (isinstance(stack[-2], str) and stack[-2] in BOOLS):
                    x = True if stack.pop() == ":true:" else False
                    y = True if stack.pop() == ":true:" else False

                    stack.append(":true:" if x and y else ":false:")
                elif isinstance(stack[-1], Unit) and stack[-1].value in BOOLS and (isinstance(stack[-2], str) and stack[-2] in BOOLS):
                    x = True if stack.pop().value == ":true:" else False
                    y = True if stack.pop() == ":true:" else False

                    stack.append(":true:" if x and y else ":false:")
                elif isinstance(stack[-2], Unit) and stack[-2].value in BOOLS and (isinstance(stack[-1], str) and stack[-1] in BOOLS):
                    x = True if stack.pop() == ":true:" else False
                    y = True if stack.pop().value == ":true:" else False

                    stack.append(":true:" if x and y else ":false:")
                elif isinstance(stack[-1], Unit) and stack[-1].value in BOOLS and (isinstance(stack[-2], Unit) and stack[-2].value in BOOLS):
                    x = True if stack.pop().value == ":true:" else False
                    y = True if stack.pop().value == ":true:" else False

                    stack.append(":true:" if x and y else ":false:")
                else:
                    print("Error and")
                    stack.append(":error:")

            elif "or" in line:
                if len(stack) < 2:
                    print("Error or")
                    stack.append(":error:")
                if (isinstance(stack[-1], str) and stack[-1] in BOOLS) and (isinstance(stack[-2], str) and stack[-2] in BOOLS):
                    x = True if stack.pop() == ":true:" else False
                    y = True if stack.pop() == ":true:" else False

                    stack.append(":true:" if x or y else ":false:")
                elif isinstance(stack[-1], Unit) and stack[-1].value in BOOLS and (isinstance(stack[-2], str) and stack[-2] in BOOLS):
                    x = True if stack.pop().value == ":true:" else False
                    y = True if stack.pop() == ":true:" else False

                    stack.append(":true:" if x or y else ":false:")
                elif isinstance(stack[-2], Unit) and stack[-2].value in BOOLS and (isinstance(stack[-1], str) and stack[-1] in BOOLS):
                    x = True if stack.pop() == ":true:" else False
                    y = True if stack.pop().value == ":true:" else False

                    stack.append(":true:" if x or y else ":false:")
                elif isinstance(stack[-1], Unit) and stack[-1].value in BOOLS and (isinstance(stack[-2], Unit) and stack[-2].value in BOOLS):
                    x = True if stack.pop().value == ":true:" else False
                    y = True if stack.pop().value == ":true:" else False

                    stack.append(":true:" if x or y else ":false:")
                else:
                    print("Error or")
                    stack.append(":error:")

            elif "not" in line:
                if len(stack) < 1:
                    print("Error not")
                    stack.append(":error:")
                if isinstance(stack[-1], str) and stack[-1] in BOOLS:
                    stack.append(":false:" if stack.pop() == ":true:" else ":true:")
                elif isinstance(stack[-1], Unit) and stack[-1].value in BOOLS:
                    stack.append(":false:" if stack.pop().value == ":true:" else ":true:")
                else:
                    print("Error not")
                    stack.append(":error:")

            elif "equal" in line:
                if len(stack) < 2:
                    print("Error Equal")
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        y = stack.pop()
                        x = stack.pop()
                        stack.append(":true:" if x == y else ":false:")
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int):
                        y = stack.pop().value
                        x = stack.pop()
                        stack.append(":true:" if x == y else ":false:")
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit):
                        y = stack.pop()
                        x = stack.pop().value
                        stack.append(":true:" if x == y else ":false:")
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], Unit):
                        y = stack.pop().value
                        x = stack.pop().value
                        stack.append(":true:" if x == y else ":false:")
                    else:
                        print("Error Equal")
                        stack.append(":error:")

            elif "lessThan" in line:
                if len(stack) < 2:
                    print("Error lessThan")
                    stack.append(":error:")
                else:
                    if isinstance(stack[-1], int) and isinstance(stack[-2], int):
                        y = stack.pop()
                        x = stack.pop()
                        stack.append(":true:" if x < y else ":false:")
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], int):
                        y = stack.pop().value
                        x = stack.pop()
                        stack.append(":true:" if x < y else ":false:")
                    elif isinstance(stack[-1], int) and isinstance(stack[-2], Unit):
                        y = stack.pop()
                        x = stack.pop().value
                        stack.append(":true:" if x < y else ":false:")
                    elif isinstance(stack[-1], Unit) and isinstance(stack[-2], Unit):
                        y = stack.pop().value
                        x = stack.pop().value
                        stack.append(":true:" if x < y else ":false:")
                    else:
                        print("Error lessThan")
                        stack.append(":error:")

            elif "bind" in line:
                if len(stack) < 2:
                    print("Error binding")
                    stack.append(":error:")

                if isinstance(stack[-2], str) and not '"' in stack[-2] and stack[-1] != ":error:" and stack[-2][0].isalpha():
                    value = stack.pop()
                    if isinstance(value, Unit):  # Handle if binding to a unit already
                        value = value.value
                    name = stack.pop()

                    unit = Unit(name, value)
                    bindings["name"] = unit

                    stack.append(unit)
                else:
                    print("Error binding")
                    stack.append(":error:")

            print("------------")
            for value in reversed(stack):
                print(value)
            print("")

    with open(output_name, "a") as output:
        while len(stack) != 0:
            output.write("%s\n" % stack.pop())


if __name__ == "__main__":
    interpreter("input.txt", "output.txt")
