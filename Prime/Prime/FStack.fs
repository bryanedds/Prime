namespace Prime
open System
open System.Collections
open System.Collections.Generic

// TODO: document!
[<RequireQualifiedAccess>]
module FStack =

    /// A functional stack with fast iteration and amortized sqrt n conj.
    /// NOTE: currently not supported by SymbolicConverter!
    type [<CustomEquality; NoComparison>] 'a FStack =
        private
            { Front : 'a array
              Back : 'a array }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () = 
                (Seq.append this.Front this.Back).GetEnumerator ()

            member this.GetEnumerator () =
                (Seq.append this.Front this.Back).GetEnumerator () :> IEnumerator

        override this.Equals (thatObj : obj) =
            match thatObj with
            | :? ('a FStack) as that -> Seq.forall2 Unchecked.equals this that
            | _ -> false

        override this.GetHashCode () =
            let mutable hash = 1
            for a in this do hash <- 31 * hash + Unchecked.hash a
            hash

        member this.Item with get index =
            if index >= 0 then
                if index >= this.Front.Length then 
                    let index' = index - this.Front.Length
                    if index' >= this.Back.Length
                    then raise (IndexOutOfRangeException "Cannot index outside of FastStack's range.")
                    else this.Back.[index']
                else this.Front.[index]
            else raise (IndexOutOfRangeException "Cannot index outside of FastStack's range.")

    let rec private balance stack =
        if length stack > 0 then
            let buffer = stack |> length |> double |> Math.Sqrt |> int
            if stack.Back.Length > buffer * 2 then
                let front = Array.zeroCreate (stack.Front.Length + buffer)
                let back = Array.zeroCreate (stack.Back.Length - buffer)
                Array.blit stack.Front 0 front 0 stack.Front.Length
                Array.blit stack.Back 0 front stack.Front.Length buffer
                Array.blit stack.Back buffer back 0 back.Length
                { Front = front; Back = back }
            elif stack.Back.Length = 0 then
                let front = Array.zeroCreate (stack.Front.Length - buffer)
                let back = Array.zeroCreate buffer
                Array.blit stack.Front front.Length back 0 buffer
                Array.blit stack.Front 0 front 0 front.Length
                { Front = front; Back = back }
            else stack
        else stack

    and length stack =
        stack.Front.Length +
        stack.Back.Length
        
    let isEmpty stack =
        length stack = 0

    let notEmpty stack =
        length stack <> 0

    let ofSeq seq =
        let stack = { Front = Seq.toArray seq; Back = [||] }
        balance stack

    let ofList lst =
        let stack = { Front = List.toArray lst; Back = [||] }
        balance stack

    let ofArray arr =
        let stack = { Front = [||]; Back = arr }
        balance stack

    let toSeq (stack : 'a FStack) =
        stack :> 'a seq

    let toList (stack : 'a FStack) =
        stack :> 'a seq |> Seq.toList

    let toArray (stack : 'a FStack) =
        Array.append stack.Front stack.Back

    let fold (f : 'a -> 'b -> 'a) s (stack : 'b FStack) =
        stack |> toSeq |> Seq.fold f s

    let map f (stack : 'a FStack) =
        stack |> toSeq |> Seq.map f |> ofSeq

    let filter f (stack : 'a FStack) =
        stack |> toSeq |> Seq.filter f |> ofSeq

    let head stack =
        stack.Front.[0]

    let tryHead stack =
        if stack.Front.Length <> 0
        then Some stack.Front.[0]
        else None

    let remove pred stack =
        let stack = { Front = Array.remove pred stack.Front; Back = Array.remove pred stack.Back }
        balance stack

    let removeAt index stack =
        let arr = toArray stack
        let arr = Array.removeAt index arr
        let stack = { Front = arr; Back = [||] }
        balance stack
        
    let replace pred replacement stack =
        { Front = Array.replace pred replacement stack.Front
          Back = Array.replace pred replacement stack.Back; }

    let replaceAt index replacement stack =
        if index < stack.Front.Length then
            let front = Array.copy stack.Front
            front.[index] <- replacement
            { stack with Front = front }
        else
            let index' = index - stack.Front.Length
            if index' < stack.Back.Length then
                let back = Array.copy stack.Back
                back.[index'] <- replacement
                { stack with Back = back }
            else raise (IndexOutOfRangeException "Cannot index outside of FastStack's range.")

    let tryFind pred stack =
        match Array.tryFind pred stack.Front with
        | None -> Array.tryFind pred stack.Back
        | Some item -> Some item

    let find pred stack =
        Option.get (tryFind pred stack)

    let tryIndex index stack =
        if index >= 0 then
            if index >= stack.Front.Length then 
                let index' = index - stack.Front.Length
                if index' >= stack.Back.Length
                then None
                else Some stack.Back.[index']
            else Some stack.Front.[index]
        else None

    let index i (stack : 'a FStack) =
        stack.[i]

    let conj a stack =
        let stack = { Front = stack.Front; Back = Array.add a stack.Back }
        balance stack

    let tryUnconj stack =
            match stack.Back with
            | [||] ->
                match stack.Front with
                | [||] -> None
                | _ ->
                    let front = Array.zeroCreate (stack.Front.Length - 1)
                    Array.blit stack.Front 0 front 0 front.Length
                    let stack = { Front = front; Back = stack.Back }
                    Some (balance stack)
            | _ ->
                let back = Array.zeroCreate (stack.Back.Length - 1)
                Array.blit stack.Back 0 back 0 back.Length
                let stack = { Front = stack.Front; Back = back }
                Some (balance stack)

    let unconj stack =
        match tryUnconj stack with
        | Some stack -> stack
        | None -> raise (InvalidOperationException "Cannot unconj an empty FStack.")

    let singleton a =
        { Front = [|a|]; Back = [||] }

    let empty =
        { Front = [||]; Back = [||] }

/// A functional stack with fast iteration and amortized sqrt n conj.
/// NOTE: currently not supported by SymbolicConverter!
type 'a FStack = 'a FStack.FStack