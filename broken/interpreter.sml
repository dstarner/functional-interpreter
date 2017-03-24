
(* Main interpreter function *)
fun interpreter(inFile : string, outFile : string) =
let

  (* Output file *)
  val outStream = TextIO.openOut outFile

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
      | (value::name::rest) => if isName(name) andalso isBindable(value) then (push("$", pop(stack)), commands, updateOrAddBinding(value, name, bindings)) else (push(":error:", stack), commands, bindings)
      (* Binding to a non-name *)
      | _ => (push(":error:", stack), commands, bindings)


  fun squiggleToNeg (str: string) = String.map(fn c => if c = #"~" then #"-" else c) str
  fun negToSquiggle (str: string) = String.map(fn c => if c = #"-" then #"~" else c) str

  (* Writes to the file at the end *)
  fun writeToFile(stack: string list) =
    case stack of
        [] => TextIO.closeOut outStream
      (* If first contains '$', then print ':unit:' *)
      | ("$"::binding::rest) => (TextIO.output(outStream, ":unit:" ^ "\n"); writeToFile(rest))
      | (first::rest) =>
                        if String.isPrefix "$" first then
                          (TextIO.output(outStream, ":unit:" ^ "\n"); writeToFile(rest))
                        else
                          (TextIO.output(outStream, squiggleToNeg(first) ^ "\n"); writeToFile(rest))

  fun evalNeg(stack: string list) =
    case stack of
        (x::rest) => if isInteger(x) then push(Int.toString(~1 * toInteger(x)), pop(stack)) else push(":error:", stack)
    | _ => push(":error:", stack)

  fun evalAddHelp(x:int, y:int) = Int.toString(x + y)
  fun evalAdd(stack: string list) =
    case stack of
      (* Two bindings  *)
        ("$"::name1::"$"::name2::rest) => if isInteger(getBinding(name1)) andalso isInteger(getBinding(name2)) then push(evalAddHelp(toInteger(getBinding(name1)), toInteger(getBinding(name2))), pop(pop(stack)) ) else push(":error:", stack)
      (* Left side binding *)
      | (x::"$"::name::rest) => if isInteger(getBinding(name)) andalso isInteger(x) then push(evalAddHelp(toInteger(getBinding(name)), toInteger(x)), pop(pop(stack)) ) else push(":error:", stack)
      | ("$"::name::y::rest) => if isInteger(getBinding(name)) andalso isInteger(y) then push(evalAddHelp(toInteger(getBinding(name)), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | (x::y::rest) => if isInteger(x) andalso isInteger(y) then push(evalAddHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  fun evalSubHelp(x:int, y:int) = Int.toString(y - x)
  fun evalSub(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) then push(evalSubHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  fun evalMulHelp(x:int, y:int) = Int.toString(x * y)
  fun evalMul(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) then push(evalMulHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  fun evalRemHelp(x:int, y:int) = Int.toString((y mod x))
  fun evalRem(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) andalso toInteger(x) <> 0 then push(evalRemHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  fun evalDivHelp(x:int, y:int) = Int.toString((y div x))
  fun evalDiv(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) andalso toInteger(x) <> 0 then push(evalDivHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  (* Evaluates 'and'*)
  fun evalAnd(stack: string list, bindings: string list list) =
    case stack of
        (":false:"::":false:"::rest) => push(":false:", pop(pop(stack)))
      | (":true:"::":false:"::rest) => push(":false:", pop(pop(stack)))
      | (":false:"::":true:"::rest) => push(":false:", pop(pop(stack)))
      | (":true:"::":true:"::rest) => push(":true:", pop(pop(stack)))

      (*)| ("$"::name1::"$"::name2::rest) => if getBoolBinding(name1, bindings) = "" orelse getBoolBinding(name2, bindings) = "" then push(":error:", stack)
                                            else if getBoolBinding(name1, bindings) = ":true:" andalso getBoolBinding(name2, bindings) = ":true:" then push(":true:", pop(pop(pop(pop(stack)))))
                                              else push(":false:", pop(pop(pop(pop(stack)))))

      | ("$"::name::":true:"::rest) => if getBoolBinding(name, bindings) = "" then push(":error:", stack) else
                                          if getBoolBinding(name, bindings) = ":true:" then push(":true:", pop(pop(pop(stack)))) else push(":false:", pop(pop(pop(stack))))
      | ("$"::name::":false:"::rest) => if getBoolBinding(name, bindings) = "" then push(":error:", stack) else push(":false:", pop(pop(pop(stack))))
      | (":true:"::"$"::name::rest) => if getBoolBinding(name, bindings) = "" then push(":error:", stack) else
                                          if getBoolBinding(name, bindings) = ":true:" then push(":true:", pop(pop(pop(stack)))) else push(":false:", pop(pop(pop(stack))))
      | (":false:"::"$"::name::rest) => if getBoolBinding(name, bindings) = "" then push(":error:", stack) else push(":false:", pop(pop(pop(stack))))
      *)
      | _ => push(":error:", stack)

  (* Evaluates 'or'*)
  fun evalOr(stack: string list) =
    case stack of
        (":false:"::":false:"::rest) => push(":false:", pop(pop(stack)))
      | (":true:"::":false:"::rest) => push(":true:", pop(pop(stack)))
      | (":false:"::":true:"::rest) => push(":true:", pop(pop(stack)))
      | (":true:"::":true:"::rest) => push(":true:", pop(pop(stack)))
      | _ => push(":error:", stack)

  fun evalNot(stack: string list) =
    case stack of
        (":false:"::rest) => push(":true:", pop(stack))
      | (":true:"::rest) => push(":false:", pop(stack))
      | _ => push(":error:", stack)

  fun evalEqualHelp(x: int, y:int) = if x = y then ":true:" else ":false:"
  fun evalEqual(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) then push(evalEqualHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  fun evalLTHelp(x: int, y: int) = if x > y then ":true:" else ":false:"
  fun evalLT(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) then push(evalLTHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
      | _ => push(":error:", stack)

  fun evalSwap(stack: string list) =
    case stack of
        (x::y::rest) => y::x::rest
      | _ => push(":error:", stack)


  fun evaluate (stack: string list, commands: string list, bindings: string list list) =
    case (stack, commands) of
      (* End of instructions *)
        (_, []) => writeToFile(stack)
      (* Close *)
      | (stack, ("quit"::rest)) => writeToFile(stack)

      (* Handle pop TODO: Handle pop from empty *)
      | ([], ("pop"::rest)) => evaluate(push(":error:", stack), rest, bindings)
      | (_, ("pop"::rest)) => evaluate(pop(stack), rest, bindings)

      (* Handle Booleans *)
      | (stack, (":true:"::rest)) => evaluate(push(":true:", stack), rest, bindings)
      | (stack, (":false:"::rest)) => evaluate(push(":false:", stack), rest, bindings)
      (* Handle any other values *)
      | (stack, ("push"::value::rest)) => evaluate(push(negToSquiggle(value), stack), rest, bindings)
      | (stack, (":error:"::rest)) => evaluate(push(":error:", stack), rest, bindings)

      (* Boolean Expression *)
      | (stack, ("and"::rest)) => evaluate(evalAnd(stack, bindings), rest, bindings)
      | (stack, ("or"::rest)) => evaluate(evalOr(stack), rest, bindings)
      | (stack, ("equal"::rest)) => evaluate(evalEqual(stack), rest, bindings)
      | (stack, ("lessThan"::rest)) => evaluate(evalLT(stack), rest, bindings)

      (* Math *)
      | (stack, ("neg"::rest)) => evaluate(evalNeg(stack), rest, bindings)
      | (stack, ("add"::rest)) => evaluate(evalAdd(stack), rest, bindings)
      | (stack, ("sub"::rest)) => evaluate(evalSub(stack), rest, bindings)
      | (stack, ("mul"::rest)) => evaluate(evalMul(stack), rest, bindings)
      | (stack, ("rem"::rest)) => evaluate(evalRem(stack), rest, bindings)
      | (stack, ("div"::rest)) => evaluate(evalDiv(stack), rest, bindings)

      (* Operations *)
      | (stack, ("swap"::rest)) => evaluate(evalSwap(stack), rest, bindings)

      (* Check if the bindings is valid *)
      | (stack, ("bind"::rest)) => evaluate(evalBind(stack, rest, bindings))
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
