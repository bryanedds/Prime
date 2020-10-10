namespace Prime
open System
open System.Collections
open System.Collections.Generic

// TODO: document!
[<RequireQualifiedAccess>]
module FastStack =

    type [<CustomEquality; NoComparison>] 'a FastStack =
        private 
            { Front : 'a array
              Back : 'a array }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () = 
                let enr =
                    seq {
                      yield! this.Front
                      yield! this.Back }
                enr.GetEnumerator ()
    
            member this.GetEnumerator() =
                (this :> _ seq).GetEnumerator () :> IEnumerator

        override this.GetHashCode () =
            let mutable hash = 1
            for a in this do hash <- 31 * hash + Unchecked.hash a
            hash

        override this.Equals (that : obj) =
            match that with
            | :? ('a FastStack) as thatStack -> Seq.forall2 Unchecked.equals this thatStack
            | _ -> false

    let rec private balance stack =
        let buffer = int (Math.Sqrt (double (length stack)))
        if stack.Back.Length > buffer * 2 then
            let front = Array.zeroCreate (stack.Front.Length + buffer)
            let back = Array.zeroCreate (stack.Back.Length - buffer)
            Array.blit stack.Back 0 front stack.Front.Length buffer
            Array.blit stack.Back buffer back 0 back.Length
            { Front = front; Back = back }
        else stack

    and length stack =
        stack.Front.Length +
        stack.Back.Length
        
    let isEmpty stack =
        length stack = 0

    let notEmpty stack =
        length stack <> 0

    let fromSeq seq =
        let stack = { Front = [||]; Back = Seq.toArray seq }
        balance stack

    let toSeq (stack : 'a FastStack) =
        stack :> 'a seq

    let fold (f : 'a -> 'b -> 'a) s (stack : 'b FastStack) =
        stack |> toSeq |> Seq.fold f s

    let map f (stack : 'a FastStack) =
        stack |> toSeq |> Seq.map f |> fromSeq

    let filter f (stack : 'a FastStack) =
        stack |> toSeq |> Seq.filter f |> fromSeq

    let head stack =
        stack.Front.[0]

    let tryHead stack =
        if stack.Front.Length <> 0
        then Some stack.Front.[0]
        else None

    let remove a stack =
        { Front = Array.remove a stack.Front; Back = Array.remove a stack.Back }

    let tryIndex i stack =
        if i >= 0 then
            if i >= stack.Front.Length then 
                let j = i - stack.Front.Length
                if j >= stack.Back.Length
                then None
                else Some stack.Back.[i]
            else Some stack.Front.[i]
        else None

    let index i stack =
        match tryIndex i stack with
        | Some a -> a
        | None -> raise (IndexOutOfRangeException "Cannot index outside of FastStack's range.")

    let conj a stack =
        let stack = balance stack
        { Front = stack.Front; Back = Array.add a stack.Back }

    let tryUnconj stack =
        let stack = balance stack
        match stack.Back with
        | [||] ->
            match stack.Front with
            | [||] -> None
            | _ ->
                let front = Array.zeroCreate (stack.Front.Length - 1)
                Array.blit stack.Front 0 front 0 front.Length
                Some { Front = front; Back = stack.Back }
        | _ ->
            let back = Array.zeroCreate (stack.Back.Length - 1)
            Array.blit stack.Back 0 back 0 back.Length
            Some { Front = stack.Front; Back = back }

    let unconj stack =
        match tryUnconj stack with
        | Some stack -> stack
        | None -> raise (InvalidOperationException "Cannot unconj an empty FastStack.")

    let empty =
        { Front = [||]; Back = [||] }

type 'a FastStack = 'a FastStack.FastStack