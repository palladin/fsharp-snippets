// A combinator based DSL for composing type-safe parameterized sql queries. Inspired by Olivier Danvy's "Functional Unparsing" paper.

// Functional Unparsing http://www.brics.dk/RS/98/12/BRICS-RS-98-12.pdf

open System
open System.Data
open System.Data.SqlClient


// Type Decls
type SqlText = string
type Counter = int
type Value = obj
type GetParamName = Counter -> SqlText
type GetParameter = SqlText -> Value -> IDataParameter 
type QueryContext = QueryContext of (SqlText * Counter * IDataParameter list * GetParamName * GetParameter)

// Basic Combinators
let sql (value : String) cont (queryContext : QueryContext) = 
    let (QueryContext (sqlText, counter, parameters, getParamName, getParam)) = queryContext
    cont (QueryContext (sqlText + value, counter, parameters, getParamName, getParam))

let ``%o`` cont (queryContext : QueryContext) (value : obj) = 
    let (QueryContext (sqlText, counter, parameters, getParamName, getParam)) = queryContext
    let paramName = getParamName counter
    cont (QueryContext (sqlText + paramName, counter + 1, parameters @ [getParam paramName value], getParamName, getParam))

let ``%d`` cont (queryContext : QueryContext) (value : int) = ``%o`` cont queryContext value
let ``%s`` cont (queryContext : QueryContext) (value : string) = ``%o`` cont queryContext value
let ``%b`` cont (queryContext : QueryContext) (value : bool) = ``%o`` cont queryContext value
let ``%dt``cont (queryContext : QueryContext) (value : DateTime) = ``%o`` cont queryContext value

let ``%L``<'T, 'R> cont (queryContext : QueryContext) (values : 'T list) : 'R = 
    let (QueryContext (sqlText, counter, parameters, getParamName, getParam)) = queryContext
    match values with
    | [] -> cont (QueryContext (sqlText + "(null)", counter, parameters, getParamName, getParam))
    | _ ->
        let (parameters', paramNames) = values 
                                        |> List.mapi (fun index value -> (value :> obj, getParamName (index + counter)))
                                        |> List.map (fun (value, paramName) -> (getParam paramName value, paramName))
                                        |> (fun list -> (List.map fst list, List.map snd list))
        let result = sprintf "(%s)" <| String.Join(", ", paramNames) 
        cont (QueryContext (sqlText + result, counter + List.length paramNames, parameters @ parameters', getParamName, getParam))

// concatenation as composition
let (++) = (<<)

// Prepare-Map-Exec functions
let query (q : (QueryContext -> QueryContext) -> QueryContext -> 'a) : 'a =
    q id (QueryContext ("", 0, [], (fun counter -> sprintf "@p%d" counter), 
                              (fun paramName value -> new SqlParameter(paramName, value) :> _)))

let asTuple2 (reader : IDataReader) : ('a * 'b) = 
    (reader.GetValue 0 :?> 'a, reader.GetValue 1 :?> 'b)
let asTuple3 (reader : IDataReader) : ('a * 'b * 'c) = 
    (reader.GetValue 0 :?> 'a, reader.GetValue 1 :?> 'b, reader.GetValue 2 :?> 'c)

let exec (conn : string) (map : IDataReader -> 'a) (queryContext : QueryContext) : 'a list = 
    let (QueryContext (sqlText, _, parameters, _, _)) = queryContext
    // open conntection
    use sqlConnection = new SqlConnection(conn)
    sqlConnection.Open()
    // execute command
    use command = new SqlCommand(sqlText, sqlConnection) :> IDbCommand
    parameters |> List.iter (fun parameter -> command.Parameters.Add(parameter) |> ignore) 
    use reader = command.ExecuteReader()
    let rec loop (reader : IDataReader) acc = 
        if reader.Read() then
            loop reader (map reader :: acc)
        else
            acc |> List.rev
    loop reader []


// Example

let testQuery age name ids = 
    sql "SELECT name, age"
    ++ sql " FROM Customers" 
    ++ sql " WHERE age = " ++ ``%d`` 
    ++ sql " AND name = " ++ ``%s`` 
    ++ sql " AND id IN " ++ ``%L``<int, _>
    ++ sql " ORDER by id" |> query <| age <| name <| ids

let conn = "ConnectionString here"
for (name, age) in exec conn asTuple2 (testQuery 26 "George" [1..3]) do
    printfn "Name: %s, Age: %d" name age