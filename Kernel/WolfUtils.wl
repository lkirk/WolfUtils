(* ::Package:: *)

(* ::Section:: *)

(*Package Header*)

BeginPackage["LKirk`WolfUtils`"];

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

tableColWidth[data_] :=
    ArrayReduce[Max[StringLength[ToString[#]]& /@ #]&, data, 1]

tableColWidth[data_, headers_] :=
    ArrayReduce[Max, {tableColWidth[data], StringLength /@ headers[[1]]}, 1]

DisplayTable[data_, headers_] /; {Depth[data], Depth[headers]} == {3, 3} :=
    With[{cw = tableColWidth[data, headers], dims = Reverse[Dimensions[data]]},
    TableView[data, ItemSize -> {cw}, Headers -> {headers}, AllowedDimensions -> dims]]

DisplayTable[data_] /; Depth[data] == 3 :=
    With[{cw = tableColWidth[data], dims = Reverse[Dimensions[data]]},
        TableView[data, ItemSize -> {cw}, AllowedDimensions -> dims]]

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
