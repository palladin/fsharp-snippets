// A Monad for composing computations with retry logic. (Useful when we work with Cloud Services)

open System
open System.Threading

type ShouldRetry = ShouldRetry of (RetryCount * LastException -> bool * RetryDelay)
and RetryCount = int
and LastException = exn
and RetryDelay = TimeSpan
type RetryPolicy = RetryPolicy of ShouldRetry
    
type RetryPolicies() =
    static member NoRetry () : RetryPolicy =
        RetryPolicy( ShouldRetry (fun (retryCount, _) -> (retryCount < 1, TimeSpan.Zero)) )
    static member Retry (retryCount : int , intervalBewteenRetries : RetryDelay) : RetryPolicy =
        RetryPolicy( ShouldRetry (fun (currentRetryCount, _) -> (currentRetryCount < retryCount, intervalBewteenRetries)))
    static member Retry (currentRetryCount : int) : RetryPolicy =
        RetryPolicies.Retry(currentRetryCount, TimeSpan.Zero)

type RetryResult<'T> = 
    | RetrySuccess of 'T
    | RetryFailure of exn
    
type Retry<'T> = Retry of (RetryPolicy -> RetryResult<'T>)

type RetryBuilder() =
    member self.Return (value : 'T) : Retry<'T> = Retry (fun retryPolicy -> RetrySuccess value)

    member self.Bind (retry : Retry<'T>, bindFunc : 'T -> Retry<'U>) : Retry<'U> = 
        Retry (fun retryPolicy ->
            let (Retry retryFunc) = retry 
            match retryFunc retryPolicy with
            | RetrySuccess value ->
                let (Retry retryFunc') = bindFunc value
                retryFunc' retryPolicy
            | RetryFailure exn -> RetryFailure exn )

    member self.Delay (f : unit -> Retry<'T>) : Retry<'T> = 
        Retry (fun retryPolicy ->
            let resultCell : option<RetryResult<'T>> ref = ref None 
            let lastExceptionCell : exn ref = ref null
            let (RetryPolicy(ShouldRetry shouldRetry)) = retryPolicy
            let canRetryCell : bool ref = ref true
            let currentRetryCountCell : int ref = ref 0
            while !canRetryCell do
                try
                    let (Retry retryFunc) = f ()
                    let result = retryFunc retryPolicy
                    resultCell := Some result
                    canRetryCell := false
                with e -> 
                    lastExceptionCell := e
                    currentRetryCountCell := 1 + !currentRetryCountCell
                    match shouldRetry(!currentRetryCountCell, !lastExceptionCell) with
                    | (true, retryDelay) ->
                        Thread.Sleep(retryDelay)
                    | (false, _) -> 
                        canRetryCell := false
            
            match !resultCell with
            | Some result -> result
            | None -> RetryFailure !lastExceptionCell )

[<AutoOpen>]
module Retry = 
    let retry = new RetryBuilder()
    let retryWithPolicy (retryPolicy : RetryPolicy) (retry : Retry<'T>) = 
        Retry (fun _ -> let (Retry retryFunc) = retry in retryFunc retryPolicy)
    let run (retry : Retry<'T>) (retryPolicy : RetryPolicy) : RetryResult<'T> =
        let (Retry retryFunc) = retry
        retryFunc retryPolicy


// Example
let test = 
    let random = new Random()
    retry {
        return 1 / random.Next(0, 2)
    }

(test, RetryPolicies.NoRetry()) ||> run
(test, RetryPolicies.Retry 10) ||> run