module Library.Tests

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

[<TestFixture>]
type GenerateSubWords () =

    [<Test>]
    member this.``Read dictionary`` () =
        let dict = Dictionary.read

        let length = dict.Length;

        Assert.That(dict, Is.Not.Empty)

    [<Test>]
    member this.``Hello`` () =
        let dict = Dictionary.read
        let expected = [| "he"; "e"; "hell"; "ho"; "lo"; "ell"; "eo"; "h"; "hello"; "heo"; "hoe"; "holl"; "l"; "ll"; "o" |]

        let subwords = SubWords.generateSubWords "Hello" dict

        Assert.That(Set.toArray subwords, Is.EquivalentTo(expected))

    [<Test>]
    member this.``Recipe Change Tracker`` () =
        let dict = Dictionary.read
        let expected = [| "he"; "e"; "hell"; "ho"; "lo"; "ell"; "eo"; "h"; "hello"; "heo"; "hoe"; "holl"; "l"; "ll"; "o" |]

        let subwords =
            SubWords.generateSubWords "RecipeChangeTracker" dict
            |> Seq.toArray
            |> Array.sortBy (fun word -> String.length word)
            |> Array.where (fun word -> word.StartsWith('r'))
            |> Array.where (fun word -> word.Length = 4)

        Assert.That(subwords, Is.EquivalentTo(expected))

[<TestFixture>]
type JoinLetters () =
    
    [<Property>]
    let ``Join split sequence`` (NonNull text) =
        let letters = Seq.toList text

        let joined = Letters.join letters

        joined = text

[<TestFixture>]
type GetPossibleWords () =
    
    [<Test>]
    member this.``Test`` () =
        let dict = [ "h"; "he"; "hello"; "allo"; "mud"; "blah" ]
        let possibleWords = Dictionary.getPossibleWords 'h' dict
        let expected = [ ['h']; ['h'; 'e']; ['h'; 'e'; 'l'; 'l'; 'o'; ]]

        Assert.That(possibleWords, Is.EquivalentTo(expected))

