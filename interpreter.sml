(* Main interpreter function *)
fun interpreter(inFile : string, outFile : string) =
let

  (* Output file *)
  val outStream = TextIO.openOut outFile

  fun print(str: string) = TextIO.output(outStream, str)

  fun push (item: string, list) = item::list
  fun pop [] = [":error:"]
    | pop (first::list) = list

  fun isInteger (str: string) =
    case Int.fromString(str) of
        NONE => false
      | SOME(i) => true
  fun toInteger (str: string) = valOf (Int.fromString str)

  (* BINDING STUFF *)
  fun isName (str: string) = if String.isPrefix "\"" str orelse String.isSuffix "\"" str orelse isInteger(str) then false else true
  fun isBindable (str: string) =
    case str of
        ":error:" => false
      | _ => true
  (* This works! Yay *)
  fun getBinding (s: string, bindings: string list list) =
    case bindings of
        [] => ":error:"
      | ([name, value]::rest) => if name = s then value else getBinding(s, rest)
      | _ => ":error:"

  (* THESE WORK *)
  fun updateBindingHelper(bef: string list list, name: string, value: string, bindings: string list list) =
    case bindings of
      (* Dunno if this should be appended at the beginning or the end *)
        [] => [name, value]::bef
      | ([n, v]::rest) => if name = n then [name, value]::bef @ rest else updateBindingHelper(bef @ [[n, v]], name, value, rest)
      | _ => bef @ bindings
  fun updateOrAddBinding(name: string, value: string, bindings) = updateBindingHelper([], name, value, bindings)
  (* This works *)
  fun validBinding(value: string, name: string) = isName(name) andalso isBindable(value)

  fun evalBind(stack: string list, commands: string list, bindings: string list list) =
    case stack of
      (* Binding to a binding *)
        ("$"::prev_bind::new_bind::rest) => if isName(new_bind) andalso getBinding(prev_bind, bindings) <> ":error:" then (push("$", push(new_bind, push("$", push(prev_bind, pop(pop(pop(stack))))))), commands, updateOrAddBinding(getBinding(prev_bind, bindings), new_bind, bindings)) else (push(":error:", stack), commands, bindings)
      (* Binding to a bad value *)
      | (value::name::rest) => if isName(name) andalso isBindable(value) then (push("$", pop(stack)), commands, updateOrAddBinding(name, value, bindings)) else (push(":error:", stack), commands, bindings)
      (* Binding to a non-name *)
      | _ => (push(":error:", stack), commands, bindings)

  (* -------- *)
  (* Swapping *)
  (* -------- *)
  fun evalSwap(stack: string list, commands, bindings) =
    case stack of
        ("$"::n1::"$"::n2::rest) => ("$"::n2::"$"::n1::rest, commands, bindings)
      | (x::"$"::n::rest) => ("$"::n::x::rest, commands, bindings)
      | ("$"::n::x::rest) => (x::"$"::n::rest, commands, bindings)
      | (x::y::rest) => (y::x::rest, commands, bindings)
      | _ => (push(":error:", stack), commands, bindings)

  (* --------- *)
  (* Math stuff*)
  (* --------- *)
  (* Neg works with bindings and values *)
  fun evalNeg(stack: string list, commands: string list, bindings: string list list) =
    case stack of
      (* With a binding *)
        ("$"::binding::rest) => if isInteger(getBinding(binding, bindings)) then (push(Int.toString(~1 * toInteger(getBinding(binding, bindings))), pop(pop(stack))), commands, bindings) else (push(":error:", stack), commands, bindings)
      | (x::rest) => if isInteger(x) then (push(Int.toString(~1 * toInteger(x)), pop(stack)), commands, bindings) else (push(":error:", stack), commands, bindings)
      | _ => (push(":error:", stack), commands, bindings)

  fun squiggleToNeg (str: string) = String.map(fn c => if c = #"~" then #"-" else c) str
  fun negToSquiggle (str: string) = String.map(fn c => if c = #"-" then #"~" else c) str

  fun evalAdd(stack: string list, commands: string list, bindings: string list list) =
    let
      fun evalAddHelp(x:int, y:int) = Int.toString(x + y)
    in
      case stack of
        (* Two bindings *)
          ("$"::n1::"$"::n2::rest) => if isInteger(getBinding(n1, bindings)) andalso isInteger(getBinding(n2, bindings)) then
                                        (push(evalAddHelp(toInteger(getBinding(n1, bindings)), toInteger(getBinding(n2, bindings))), pop(pop(pop(pop(stack))))), commands, bindings)
                                        else
                                        (push(":error:", stack), commands, bindings)
        | (x::"$"::n::rest) =>  if isInteger(getBinding(n, bindings)) andalso isInteger(x) then
                                  (push(evalAddHelp(toInteger(getBinding(n, bindings)), toInteger(x)), pop(pop(pop(stack)))), commands, bindings)
                                else
                                  (push(":error:", stack), commands, bindings)
        | ("$"::n::x::rest) =>  if isInteger(getBinding(n, bindings)) andalso isInteger(x) then
                                  (push(evalAddHelp(toInteger(getBinding(n, bindings)), toInteger(x)), pop(pop(pop(stack)))), commands, bindings)
                                else
                                  (push(":error:", stack), commands, bindings)
        | (x::y::rest) => if isInteger(x) andalso isInteger(y) then
                            (push(evalAddHelp(toInteger(x), toInteger(y)), pop(pop(stack))), commands, bindings)
                          else
                            (push(":error:", stack), commands, bindings)
        | _ => (push(":error:", stack), commands, bindings)
    end

  fun evalMul(stack: string list, commands: string list, bindings: string list list) =
    let
      fun evalMulHelp(x:int, y:int) = Int.toString(x * y)
    in
      case stack of
        (* Two bindings *)
          ("$"::n1::"$"::n2::rest) => if isInteger(getBinding(n1, bindings)) andalso isInteger(getBinding(n2, bindings)) then
                                        (push(evalMulHelp(toInteger(getBinding(n1, bindings)), toInteger(getBinding(n2, bindings))), pop(pop(pop(pop(stack))))), commands, bindings)
                                        else
                                        (push(":error:", stack), commands, bindings)
        | (x::"$"::n::rest) =>  if isInteger(getBinding(n, bindings)) andalso isInteger(x) then
                                  (push(evalMulHelp(toInteger(getBinding(n, bindings)), toInteger(x)), pop(pop(pop(stack)))), commands, bindings)
                                else
                                  (push(":error:", stack), commands, bindings)
        | ("$"::n::x::rest) =>  if isInteger(getBinding(n, bindings)) andalso isInteger(x) then
                                  (push(evalMulHelp(toInteger(getBinding(n, bindings)), toInteger(x)), pop(pop(pop(stack)))), commands, bindings)
                                else
                                  (push(":error:", stack), commands, bindings)
        | (x::y::rest) => if isInteger(x) andalso isInteger(y) then
                            (push(evalMulHelp(toInteger(x), toInteger(y)), pop(pop(stack))), commands, bindings)
                          else
                            (push(":error:", stack), commands, bindings)
        | _ => (push(":error:", stack), commands, bindings)
    end


  (* Writes to the file at the end *)
  fun writeToFile(stack: string list) =
    let
      (* TODO: Have this remove quotes *)
      fun removeQuotes(str: string) = implode(List.rev(List.drop(List.rev(List.drop(explode(str), 1)), 1)))
    in
      case stack of
          [] => TextIO.closeOut outStream
        (* If first contains '$', then print ':unit:' *)
        | ("$"::binding::rest) => (TextIO.output(outStream, ":unit:" ^ "\n"); writeToFile(rest))
        | (first::rest) =>
                        if String.isPrefix "\"" first andalso String.isSuffix "\"" first then
                          (TextIO.output(outStream, removeQuotes(first) ^ "\n"); writeToFile(rest))
                        else
                          (TextIO.output(outStream, squiggleToNeg(first) ^ "\n"); writeToFile(rest))
    end

  fun evaluate (stack: string list, commands: string list, bindings: string list list) =
    case (stack, commands) of
      (* End of instructions *)
        (_, []) => writeToFile(stack)
      (* Close *)
      | (stack, ("quit"::rest)) => writeToFile(stack)

      | ([], ("pop"::rest)) => evaluate(push(":error:", stack), rest, bindings)
      | (_, ("pop"::rest)) => evaluate(pop(stack), rest, bindings)

      (* Handle Booleans *)
      | (stack, (":true:"::rest)) => evaluate(push(":true:", stack), rest, bindings)
      | (stack, (":false:"::rest)) => evaluate(push(":false:", stack), rest, bindings)
      (* Handle any other values *)
      (* If pushing something that already has a binding, then push "$", "binding_name" to the stack, else just push the value *)
      | (stack, ("push"::value::rest)) => if isName(value) andalso getBinding(value, bindings) <> ":error:" then evaluate(push( "$", push(negToSquiggle(value), stack)), rest, bindings) else evaluate(push(negToSquiggle(value), stack), rest, bindings)
      | (stack, (":error:"::rest)) => evaluate(push(":error:", stack), rest, bindings)

      (* Math *)
      | (stack, ("neg"::rest)) => evaluate(evalNeg(stack, rest, bindings))
      | (stack, ("add"::rest)) => evaluate(evalAdd(stack, rest, bindings))
      | (stack, ("mul"::rest)) => evaluate(evalMul(stack, rest, bindings))

      (* Binding and extras *)
      | (stack, ("bind"::rest)) => evaluate(evalBind(stack, rest, bindings))
      | (stack, ("swap"::rest)) => evaluate(evalSwap(stack, rest, bindings))
      | (_, x::rest) => evaluate(push(":error:", stack), rest, bindings)

  (* ---------------- *)
  (* HELPER FUNCTIONS *)
  (* ---------------- *)
  fun commandList (input: string) =
    let
      (* the input file *)
      val inStream = TextIO.openIn input
      (* Current line *)
      val currentLine = TextIO.inputLine inStream
      fun read_next(currentLine : string option) =
        case currentLine of
          (* If nothing left *)
          NONE => (TextIO.closeIn inStream; "")
          (* Else Add the line the line *)
          | SOME(c) => (c ^ read_next(TextIO.inputLine inStream))
      (*Iterate over string *)
      fun explodeAndReplace(prog: string) = String.tokens (fn c => c = #" " orelse c = #"\n") prog
    in
      explodeAndReplace(read_next(currentLine))
    end


in
  (* Evaluate by passing empty stack and then command list *)
  evaluate([], commandList(inFile), [])
end;
