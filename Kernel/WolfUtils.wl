(* ::Package:: *)

(* ::Section:: *)

(*Package Header*)

BeginPackage["LloydKirk`WolfUtils`"];

(* ::Text:: *)

(* Data Summarization Utilities *)

DisplayTable

(* Benchmark Utilities *)

MeanBench

RunBench

(* Functional Utilities *)

One

(* IO *)

LoadOrRun

NBFile

Begin["`Private`"];

(* ::Section:: *)

(*Data Summarization*)

DisplayTable[data_, headers_] :=
    With[{colWidths = ArrayReduce[Max, {ArrayReduce[Max[StringLength[
        ToString[#]]& /@ #]&, data, 1], StringLength /@ headers}, 1], dims = 
        Reverse[Dimensions[data]]},
        TableView[data, ItemSize -> {colWidths}, Headers -> {headers},
             AllowedDimensions -> dims]
    ]

DisplayTable[data_] :=
    With[{colWidths = ArrayReduce[Max[StringLength[ToString[#]]& /@ #
        ]&, data, 1], dims = Reverse[Dimensions[data]]},
        TableView[data, ItemSize -> {colWidths}, AllowedDimensions ->
             dims]
    ]

(* ::Section:: *)

(*Benchmarking*)

SetAttributes[{MeanBench, RunBench}, HoldFirst]

RunBench[expr_] :=
    (
(* TODO: clearing the cache incurs significant overhead,
         making microbenchmarks unpalatable. Should
         disable caching for the benchmarking session. *)ClearSystemCache[
    ];
        AbsoluteTiming[MaxMemoryUsed[expr]]
    )

RunBench[expr_, k_] :=
    Table[RunBench[expr], {n, 1, k}]

MeanBench[expr_, k_] :=
    ArrayReduce[Mean, RunBench[expr, k], 1]

(* ::Section:: *)

(*Functional*)

One::err = "Iterable is expected to contain exactly one item, got ``";

One[{x_}] :=
    x

One[x_] :=
    Message[One::err, HoldForm[x]]

(* TODO: Figure out errors. Should be able to inspect context *)

(* ::Section:: *)

(* IO *)

SetAttributes[LoadOrRun, HoldRest]

LoadOrRun[fname_String, fn_] :=
    Module[{result},
        If[FileExistsQ[fname],
            Print["Found ", fname, " loading..."];
            Import[fname]
            ,
            result = ReleaseHold[fn];
            Export[fname, result];
            Print["Wrote ", fname];
            result
        ]
    ]

NBFile[fmt_, args__] :=
    FileNameJoin[NotebookDirectory[], ToString @ StringForm[fmt, args
        ]]

NBFile[fname_] :=
    FileNameJoin[NotebookDirectory[], fname]

(* ::Section::Closed:: *)

(*Package Footer*)

End[];

EndPackage[];
