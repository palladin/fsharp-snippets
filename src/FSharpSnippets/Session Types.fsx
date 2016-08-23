// An implementation of session types in F#. Inspired from the paper "Haskell Session Types with (Almost) No Class"

// http://www.eecs.harvard.edu/~tov/pubs/haskell-session-types/session08.pdf


// Basic Operations
[<AbstractClass>]
type Ops = class end
type Eps = class inherit Ops end 
type Send<'T, 'Rest when 'Rest :> Ops> = class inherit Ops end 
type Recv<'T, 'Rest when 'Rest :> Ops> = class inherit Ops end 
type Choose<'Left, 'Right when 'Left :> Ops and 'Right :> Ops> = class inherit Ops end 
type Offer<'Left, 'Right when 'Left :> Ops and 'Right :> Ops> = class inherit Ops end 

// Client/Server Duality 
type Dual<'Client, 'Server when 'Client :> Ops and 'Server :> Ops> private() = 
    static member Eps = new Dual<Eps, Eps>()
    member self.Send<'T>() = new Dual<Send<'T, 'Client>, Recv<'T, 'Server>>()
    member self.Recv<'T>() = new Dual<Recv<'T, 'Client>, Send<'T, 'Server>>()
    member self.Choose<'Client', 'Server' when 'Client' :> Ops and 'Server' :> Ops>(dual : Dual<'Client', 'Server'>) =
        new Dual<Choose<'Client, 'Client'>, Offer<'Server, 'Server'>>()
    member self.Offer<'Client', 'Server' when 'Client' :> Ops and 'Server' :> Ops>(dual : Dual<'Client', 'Server'>) =
        new Dual<Offer<'Client, 'Client'>, Choose<'Server, 'Server'>>()

let eps = Dual<_, _>.Eps

// Session Parameterized Monad
type Msg = Msg of (obj * AsyncReplyChannel<unit>)
type Session<'S1, 'S2, 'T> = Session of (MailboxProcessor<Msg> -> Async<'T>) 
type SessionBuilder() = 
    member self.Return (value : 'T) : Session<'S, 'S, 'T> = 
        Session (fun _ -> async { return value })
    member self.Bind (session : Session<'S1, 'S2, 'T>, f : 'T -> Session<'S2, 'S3, 'R>) 
        : Session<'S1, 'S3, 'R> =
        Session (fun mailBox ->
            async {
                let (Session r) = session
                let! x = r mailBox
                let (Session r') = f x
                return! r' mailBox
            }) 

let session = new SessionBuilder()

// Basic Operations
let send (value : 'T) : Session<Send<'T, 'Rest>, 'Rest, unit> = 
    Session (fun mailBox -> 
        async { return! mailBox.PostAndAsyncReply (fun reply -> Msg (value :> obj, reply)) })

let recv () : Session<Recv<'T, 'Rest>, 'Rest, 'T> = 
    Session (fun mailBox -> 
        async { 
            let! (Msg (value, reply)) = mailBox.Receive() 
            reply.Reply ()
            return value :?> 'T 
        })

let sel1 () : Session<Choose<'First, 'Second>, 'First, unit> =
    Session (fun mailBox -> 
        async { return! mailBox.PostAndAsyncReply (fun reply -> Msg (true :> obj, reply)) })

let sel2 () : Session<Choose<'First, 'Second>, 'Second, unit> =
    Session (fun mailBox -> 
        async { return! mailBox.PostAndAsyncReply (fun reply -> Msg (false :> obj, reply)) })

let cases (left : Session<'First, 'Rest, 'T>) (right : Session<'Second, 'Rest, 'T>) 
    : Session<Offer<'First, 'Second>, 'Rest, 'T> = 
    Session (fun mailBox -> 
        async {
            let! (Msg (value, reply)) = mailBox.Receive()
            reply.Reply ()
            match value :?> bool with
            | true ->
                let (Session r) = left
                return! r mailBox 
            | false ->
                let (Session r) = right
                return! r mailBox
        })

let run (dual : Dual<'Client, 'Server>) 
        (client : Session<'Client, Eps, 'T>)
        (server : Session<'Server, Eps, unit>) = 
    let mailBox = MailboxProcessor.Start(fun _ -> async { () })
    let (Session r) = server
    let (Session r') = client
    let result  = 
        [|async { let! _ = r mailBox in return Unchecked.defaultof<'T> }; r' mailBox|] 
        |> Async.Parallel |> Async.RunSynchronously 
    result.[1]
    
       
// Example
let addDual = eps.Recv<int>().Send<int>().Send<int>()
let stringToIntDual = eps.Recv<int>().Send<string>()
let chooseDual = addDual.Choose(stringToIntDual)

let clientAdd() = 
    session {
        do! send 1
        do! send 2
        let! result = recv ()
        return result
    }


let serverAdd() = 
    session {
        let! first = recv ()
        let! second = recv ()
        do! send (first + second)
    }

let clientStringToInt() = 
    session {
        do! send "42"
        let! result = recv ()
        return result
    }

let serverStringToInt() = 
    session {
        let! value = recv ()
        do! send (System.Int32.Parse value)
    }
    
let client (input : int) = 
    session {
        if input = 1 then
            do! sel1 ()
            let! value = clientAdd()
            return value
        else
            do! sel2 ()
            let! value = clientStringToInt()
            return value
    }

let server () =
    cases (serverAdd()) (serverStringToInt())

run addDual (clientAdd()) (serverAdd()) // 3
run stringToIntDual (clientStringToInt()) (serverStringToInt()) // 42
run chooseDual (client 1) (server()) // 3
run chooseDual (client 2) (server()) // 42