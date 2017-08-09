module Static 
    open System.Threading.Tasks
    open System.Collections.Concurrent

    let get<'T> (queue : ConcurrentQueue<'T>) = let (_, v) = queue.TryDequeue() in v
    let run<'T> (f : unit -> 'T) : Task<'T> = Task.Run(f)

