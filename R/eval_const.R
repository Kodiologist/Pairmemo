const.env = new.env(parent = emptyenv())
for (s in c(
        "T", "F",
        "{", "(", "[", "[[",
        getGroupMembers("Math"),
        getGroupMembers("Arith"),
        getGroupMembers("Compare"),
        "c", "list",
        "paste", "paste0", "sprintf"))
    const.env[[s]] = get(s, "package:base")

eval.const = \(e)
    eval(e, envir = const.env)
