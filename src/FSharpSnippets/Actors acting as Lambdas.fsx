
// The Untyped Lambda Calculus encoded as actors (F#'s MailboxProcessors)

// Useful type aliases
type Actor = MailboxProcessor<obj>
type Ident = string
type Cond = Actor
type Env = Actor
 
 
let (<!>) (actor : Actor) (msg : 'T) = actor.Post msg
 
// run forever - template
let start<'T> (work : 'T -> unit) : Actor =
    MailboxProcessor<obj>.Start(fun mb ->
        let rec loop () =
            async {
                let! msg = mb.Receive()
                match msg with
                | :? 'T as msg' -> work msg'
                | _ -> () // oops... undefined behaviour
                return! loop ()
            }
        loop () )
 
// Helper print expression
let printExp = start<obj>(fun value -> printfn "Print: %A" value)
 
// Enviroment encoding functions
let emptyEnv =
    start<Cond * Ident>(fun _ -> ()(*oops... undefined behaviour*))
let buildEnv ident value env =
    start<Cond * Ident>(fun (cond, ident') -> if ident = ident' then cond <!> value else env <!> (cond, ident'))
 
// Closures are also actors
let closure ident body env =
    start<Cond * obj>(fun (cond, arg) -> let env' = buildEnv ident arg env in body <!> (cond, env'))
 
// Lambda Calculus expressions
let constExp value = start<Cond * Env>(fun (cond, _) -> cond <!> value)
let identExp ident = start<Cond * Env>(fun (cond, env) -> env <!> (cond, ident))
 
let lambdaExp ident body =
    start<Cond * Env>(fun (cond, env) -> cond <!> closure ident body env)
 
let appExp exp argExp =
    start<Cond * Env>(fun (cond, env) ->
        let closureCond =
            start<Cond>(fun closure ->
                let argCond = start<obj>(fun value -> closure <!> (cond, value))
                argExp <!> (argCond, env))
        exp <!> (closureCond, env))
 
// Examples
 
let idExp = lambdaExp "x" (identExp "x")
let example = appExp idExp (constExp 42)
 
example <!> (printExp, emptyEnv) // Print: 42
 
let selfAppExp = lambdaExp "x" (appExp (identExp "x") (identExp "x"))
let omega = appExp selfAppExp selfAppExp
omega <!> (printExp, emptyEnv) // run forever