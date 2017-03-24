
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

  fun squiggleToNeg (str: string) = String.map(fn c => if c = #"~" then #"-" else c) str
  fun negToSquiggle (str: string) = String.map(fn c => if c = #"-" then #"~" else c) str

  (* Writes to the file at the end *)
  fun writeToFile(stack: string list) =
    case stack of
        [] => TextIO.closeOut outStream
      | (first::rest) => (TextIO.output(outStream, squiggleToNeg(first) ^ "\n"); writeToFile(rest))

  fun evalNeg(stack: string list) =
    case stack of
        (x::rest) => if isInteger(x) then push(Int.toString(~1 * toInteger(x)), pop(stack)) else push(":error:", stack)
    | _ => push(":error:", stack)

  fun evalAddHelp(x:int, y:int) = Int.toString(x + y)
  fun evalAdd(stack: string list) =
    case stack of
        (x::y::rest) => if isInteger(x) andalso isInteger(y) then push(evalAddHelp(toInteger(x), toInteger(y)), pop(pop(stack)) ) else push(":error:", stack)
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
  fun evalAnd(stack: string list) =
    case stack of
        (":false:"::":false:"::rest) => push(":false:", pop(pop(stack)))
      | (":true:"::":false:"::rest) => push(":false:", pop(pop(stack)))
      | (":false:"::":true:"::rest) => push(":false:", pop(pop(stack)))
      | (":true:"::":true:"::rest) => push(":true:", pop(pop(stack)))
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


  fun evaluate (stack: string list, commands: string list) =
    case (stack, commands) of
      (* End of instructions *)
        (_, []) => writeToFile(stack)
      (* Close *)
      | (stack, ("quit"::rest)) => writeToFile(stack)

      (* Handle pop TODO: Handle pop from empty *)
      | ([], ("pop"::rest)) => evaluate(push(":error:", stack), rest)
      | (_, ("pop"::rest)) => evaluate(pop(stack), rest)

      (* Handle Booleans *)
      | (stack, (":true:"::rest)) => evaluate(push(":true:", stack), rest)
      | (stack, (":false:"::rest)) => evaluate(push(":false:", stack), rest)
      (* Handle any other values *)
      | (stack, ("push"::value::rest)) => evaluate(push(negToSquiggle(value), stack), rest)
      | (stack, (":error:"::rest)) => evaluate(evalAnd(stack), rest)

      (* Boolean Expression *)
      | (stack, ("and"::rest)) => evaluate(evalAnd(stack), rest)
      | (stack, ("or"::rest)) => evaluate(evalOr(stack), rest)
      | (stack, ("equal"::rest)) => evaluate(evalEqual(stack), rest)
      | (stack, ("lessThan"::rest)) => evaluate(evalLT(stack), rest)

      (* Math *)
      | (stack, ("neg"::rest)) => evaluate(evalNeg(stack), rest)
      | (stack, ("add"::rest)) => evaluate(evalAdd(stack), rest)
      | (stack, ("sub"::rest)) => evaluate(evalSub(stack), rest)
      | (stack, ("mul"::rest)) => evaluate(evalMul(stack), rest)
      | (stack, ("rem"::rest)) => evaluate(evalRem(stack), rest)
      | (stack, ("div"::rest)) => evaluate(evalDiv(stack), rest)

      (* Operations *)
      | (stack, ("swap"::rest)) => evaluate(evalSwap(stack), rest)
      | (_, x::rest) => evaluate(push(":error:", stack), rest)




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
  evaluate([], commandList(inFile))
end;
