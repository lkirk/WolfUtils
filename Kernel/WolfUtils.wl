(* ::Package:: *)

(* ::Section:: *)

(*Package Header*)

BeginPackage["LloydKirk`WolfUtils`"];

(* ::Text:: *)

(*Declare your public symbols here:*)

DisplayTable;

MeanBench;

RunBench;

Begin["`Private`"];

(* ::Section:: *)

(*Definitions*)

(* ::Text:: *)

(*Define your public and private symbols here:*)

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

SetAttributes[{MeanBench, RunBench}, HoldFirst]

RunBench[expr_] :=
    (
        ClearSystemCache[];
        AbsoluteTiming[MaxMemoryUsed[expr]]
    )

RunBench[expr_, k_] :=
    Table[RunBench[expr], {n, 1, k}]

MeanBench[expr_, k_] :=
    ArrayReduce[Mean, RunBench[expr, k], 1] /. {t_, m_} :> {t, m / 1000000
        } // N

(* ::Section::Closed:: *)

(*Package Footer*)

End[];

EndPackage[];
