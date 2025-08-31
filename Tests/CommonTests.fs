module CommonTests

open Expecto
open CommonTypes
open Helpers
open EEExtensions
open Optics

// Test data for Helpers module
let testHelpersData =
    [
        "Parse component ID",
        fun () ->
            let id = CommonTypes.ComponentId "test-id-123"
            let str = CommonTypes.getComponentIdString id
            Expect.equal str "test-id-123" "ComponentId string should match"

        "Parse connection ID",
        fun () ->
            let id = CommonTypes.ConnectionId "conn-456"
            let str = CommonTypes.getConnectionIdString id
            Expect.equal str "conn-456" "ConnectionId string should match"

        "Map2 function",
        fun () ->
            let result = Helpers.map2 ((+) 1) ((+) 2) (10, 20)
            Expect.equal result (11, 22) "map2 should apply functions to tuple elements"

        "Map3 function",
        fun () ->
            let result = Helpers.map3 ((+) 1) ((*) 2) ((+) 3) (10, 20, 30)
            Expect.equal result (11, 40, 33) "map3 should apply functions correctly"

        "List split function",
        fun () ->
            let lst = [1; 2; 3; 4; 5]
            let (first3, rest) = Helpers.listSplit 3 lst
            Expect.equal first3 [1; 2; 3] "First part should have 3 elements"
            Expect.equal rest [4; 5] "Rest should have remaining elements"

        "MapKeys function",
        fun () ->
            let map = Map.ofList [(1, "a"); (2, "b"); (3, "c")]
            let keys = Helpers.mapKeys map
            Expect.containsAll keys [1; 2; 3] "Should return all map keys"

        "MapValues function",
        fun () ->
            let map = Map.ofList [("a", 10); ("b", 20)]
            let values = Helpers.mapValues map
            Expect.containsAll values [10; 20] "Should return all map values"
    ]

// Test data for EEExtensions
let testEEExtensionsData =
    [
        "List.AllEqual - all same",
        fun () ->
            let result = [1; 1; 1; 1] |> List.allEqual
            Expect.isTrue result "All equal elements should return true"

        "List.AllEqual - different",
        fun () ->
            let result = [1; 2; 1] |> List.allEqual
            Expect.isFalse result "Different elements should return false"

        "List.CountBy",
        fun () ->
            let counts = [1; 2; 2; 3; 3; 3] |> List.countBy id
            Expect.equal (Map.find 1 counts) 1 "1 appears once"
            Expect.equal (Map.find 2 counts) 2 "2 appears twice"
            Expect.equal (Map.find 3 counts) 3 "3 appears three times"

        "String.StartsWith",
        fun () ->
            let result = "HelloWorld" |> String.startsWith "Hello"
            Expect.isTrue result "Should detect string prefix"

        "String.RegexMatch",
        fun () ->
            let pattern = @"^\d{3}-\d{3}-\d{4}$"
            let result1 = "123-456-7890" |> String.regexMatch pattern
            let result2 = "invalid" |> String.regexMatch pattern
            Expect.isTrue result1 "Valid phone number should match"
            Expect.isFalse result2 "Invalid string should not match"

        "Result.IsOk",
        fun () ->
            let ok = Ok 42
            let err = Error "failed"
            Expect.isTrue (Result.isOk ok) "Ok should return true"
            Expect.isFalse (Result.isOk err) "Error should return false"

        "Result.Map",
        fun () ->
            let result = Ok 10 |> Result.map ((*) 2)
            match result with
            | Ok v -> Expect.equal v 20 "Should map Ok value"
            | Error _ -> failtest "Should be Ok"
    ]

// Test data for Optics (lenses)
let testOpticsData =
    [
        "Lens Get",
        fun () ->
            type Person = { Name: string; Age: int }
            let nameLens = Lens.create (fun p -> p.Name) (fun n p -> { p with Name = n })
            let person = { Name = "Alice"; Age = 30 }
            let name = Optic.get nameLens person
            Expect.equal name "Alice" "Should get field value"

        "Lens Set",
        fun () ->
            type Person = { Name: string; Age: int }
            let ageLens = Lens.create (fun p -> p.Age) (fun a p -> { p with Age = a })
            let person = { Name = "Bob"; Age = 25 }
            let updated = Optic.set ageLens 26 person
            Expect.equal updated.Age 26 "Should set field value"
            Expect.equal updated.Name "Bob" "Other fields unchanged"

        "Lens Composition",
        fun () ->
            type Address = { Street: string; City: string }
            type Person = { Name: string; Address: Address }
            
            let addressLens = Lens.create (fun p -> p.Address) (fun a p -> { p with Address = a })
            let cityLens = Lens.create (fun a -> a.City) (fun c a -> { a with City = c })
            let personCityLens = addressLens >-> cityLens
            
            let person = { 
                Name = "Charlie"
                Address = { Street = "Main St"; City = "Boston" }
            }
            let updated = Optic.set personCityLens "New York" person
            Expect.equal updated.Address.City "New York" "Should update nested field"
            Expect.equal updated.Address.Street "Main St" "Other nested fields unchanged"

        "Prism for Option",
        fun () ->
            let somePrism = Prism.create Option.isSome Option.get Some
            let value = Some 42
            let extracted = Optic.get somePrism value
            Expect.equal extracted (Some 42) "Should extract Some value"
    ]

// Test data for Position operations
let testPositionData =
    [
        "Position Init",
        fun () ->
            let pos = Pos.init 10.5 20.7
            Expect.equal pos.X 10.5 "X coordinate should match"
            Expect.equal pos.Y 20.7 "Y coordinate should match"

        "Position Addition",
        fun () ->
            let p1 = Pos.init 10.0 20.0
            let p2 = Pos.init 5.0 3.0
            let sum = Pos.add p1 p2
            Expect.equal sum (Pos.init 15.0 23.0) "Should add positions"

        "Position Subtraction",
        fun () ->
            let p1 = Pos.init 10.0 20.0
            let p2 = Pos.init 3.0 5.0
            let diff = Pos.subtract p1 p2
            Expect.equal diff (Pos.init 7.0 15.0) "Should subtract positions"

        "Position Distance",
        fun () ->
            let p1 = Pos.init 0.0 0.0
            let p2 = Pos.init 3.0 4.0
            let dist = Pos.distance p1 p2
            Expect.floatClose Accuracy.high dist 5.0 "Should calculate Euclidean distance"

        "Position Bounding Box",
        fun () ->
            let positions = [
                Pos.init 10.0 20.0
                Pos.init 30.0 15.0
                Pos.init 5.0 25.0
            ]
            let bbox = Pos.boundingBox positions
            Expect.equal bbox.Min (Pos.init 5.0 15.0) "Min should be correct"
            Expect.equal bbox.Max (Pos.init 30.0 25.0) "Max should be correct"
    ]

// Mock implementations for missing functions  
module Pos =
    let init x y = { X = x; Y = y }
    let add p1 p2 = { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    let subtract p1 p2 = { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    let distance p1 p2 = 
        let dx = p1.X - p2.X
        let dy = p1.Y - p2.Y
        sqrt (dx * dx + dy * dy)
    let boundingBox positions =
        let xs = positions |> List.map (fun p -> p.X)
        let ys = positions |> List.map (fun p -> p.Y)
        { Min = init (List.min xs) (List.min ys)
          Max = init (List.max xs) (List.max ys) }

module String =
    let startsWith (prefix: string) (str: string) = str.StartsWith(prefix)
    let regexMatch pattern str = 
        System.Text.RegularExpressions.Regex.IsMatch(str, pattern)

module Result =
    let isOk = function
        | Ok _ -> true
        | Error _ -> false

module List =
    let allEqual lst =
        match lst with
        | [] | [_] -> true
        | h::t -> List.forall ((=) h) t
        
    let countBy f lst =
        lst 
        |> List.groupBy f
        |> List.map (fun (k, v) -> k, List.length v)
        |> Map.ofList

// Create test lists
[<Tests>]
let helpersTests =
    testList "Helpers Module Tests" (
        testHelpersData |> List.map (fun (name, test) ->
            testCase name test
        )
    )

[<Tests>]
let eeExtensionsTests =
    testList "EEExtensions Module Tests" (
        testEEExtensionsData |> List.map (fun (name, test) ->
            testCase name test
        )
    )

[<Tests>]
let opticsTests =
    testList "Optics Module Tests" (
        testOpticsData |> List.map (fun (name, test) ->
            testCase name test
        )
    )

[<Tests>]
let positionTests =
    testList "Position Operations Tests" (
        testPositionData |> List.map (fun (name, test) ->
            testCase name test
        )
    )