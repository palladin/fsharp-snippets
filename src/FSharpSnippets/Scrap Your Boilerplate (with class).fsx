// A typecast free experiment in Generic Programming. Inspired by "Scrap Your Boilerplate (with class)".

// http://homepages.cwi.nl/~ralf/syb3/

let inline gmap f g  (x : ^R) : ^R = (f ? (g) <- x) 

type Data = Data with
    static member inline ($)(f : ^F, x : ^A) = gmap Data f x

    static member inline (?<-)(Data, f, x : int) = x
    static member inline (?<-)(Data, f, x : string) = x
    static member inline (?<-)(Data, f, x : bool) = x
    static member inline (?<-)(Data, f : _, xs : _ option) = 
        xs |> Option.map (fun x -> f $ x)
    static member inline (?<-)(Data, f : _, (x, y) : (_ * _)) = 
        (f $ x, f $ y)
    static member inline (?<-)(Data, f : _, xs : _ list) = 
        xs |> List.map (fun x -> f $ x)

type Inc = Inc with
    static member inline ($)(f : _, x : _) = gmap Data f x
    static member inline (?<-)(Data, _ : Inc, x : int) = x + 1


type Upper = Upper with
    static member inline ($)(f : _, x : _) = gmap Data f x
    static member inline (?<-)(Data, _ : Upper, x : string) = x.ToUpper()

// Example
gmap Data Data [("nick", 1)] // [("nick", 1)]
gmap Data Inc [("nick", 1)] // [("nick", 2)]
gmap Data Upper [("nick", 1)] // [("NICK", 1)]