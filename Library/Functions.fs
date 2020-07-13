namespace Library

open System
open System.IO

module Dictionary =
    
    let read =
        File.ReadAllLines("words_alpha.txt")
        |> Seq.toList

    let letterIsWord letter dictionary =
        List.contains (string letter) dictionary

    let getPossibleWords (firstLetter:char) dictionary =
        List.where (fun (word:string) -> word.StartsWith(firstLetter)) dictionary

module Letters =
    
    let folder state c =
        state + string c

    let join (letters:char list) =
        Seq.fold folder "" letters

    let getOccurances letters =
        List.groupBy (fun letter -> letter) letters
        |> List.map (fun (key, values) -> (key, values.Length))

module Text =

    let clean text =
        if text = null then "" else text

    let splitWords (text:string) =
        text.Split ' '

module SubWords =

    let canBuildWordFromLetters (word:string) (letterCombination:char list) =
        let allowedLetters = Letters.getOccurances letterCombination 

        let wordLetterOccurances = Seq.toList word |> Letters.getOccurances

        let hasEnoughOfLetter requiredLetter requiredCount =
            List.exists (fun (letter, count) -> requiredLetter = letter && requiredCount <= count) allowedLetters

        List.forall (fun (requiredLetter, requiredCount) -> hasEnoughOfLetter requiredLetter requiredCount) wordLetterOccurances 

    let searchPossibleWords (letters:char list) (dictionary:string list) =
        let firstLetter = List.head letters
        let possibleWords =
            Dictionary.getPossibleWords firstLetter dictionary

        [ for possibleWord in possibleWords do yield (if canBuildWordFromLetters possibleWord letters then Some possibleWord else None) ]
        |> List.choose id

    let rec buildLetterCombinations letters =
        if letters = [] then [] else [ letters ] @ buildLetterCombinations (List.tail letters)

    let generateWords letterCombination dictionary =
        let subwords = searchPossibleWords letterCombination dictionary

        let firstLetter = List.head letterCombination
        if Dictionary.letterIsWord firstLetter dictionary then string firstLetter :: subwords else subwords

    let constructWordsFromLetters (letters:char list) (dictionary:string list) =
        let letterCombinations = buildLetterCombinations letters
        [ for letterCombination in letterCombinations do yield (generateWords letterCombination dictionary) ]
        |> List.concat

    let generateSubWordsFromDictionary (text:string) dictionary =
        let letters = Seq.toList (text.ToLower())
        let subWords = constructWordsFromLetters letters dictionary 
        Set.ofSeq subWords

    let generateSubWords (text:string) dictionary =
        match Text.clean text with
        | "" -> Set []
        | cleaned -> generateSubWordsFromDictionary cleaned dictionary
