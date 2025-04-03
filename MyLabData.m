(* ::Package:: *)

MLDExcelCell[cell_] := With[
  {letters = Select[StringSplit[cell], LetterQ],
   row = StringSplit[cell][[-1]]},
  With[{column = 
     Table[LetterNumber[letters[[i]]]*10^(Length@letters - i), {i, 
       Length[letters]}]},
   {ToExpression@row, Total@column}
   ]
  ];

MLDReadExcel[path_, topleft_, bottomright_] := Import[path][[1,
   MLDExcelCell[topleft][[1]] ;; MLDExcelCell[bottomright][[1]],
   MLDExcelCell[topleft][[2]] ;; MLDExcelCell[bottomright][[2]]
   ]];
MLDReadExcel::usage = 
  "MLDReadExcel[path_, topleft_, bottomright_] - reads Excel file \
from path path_ from cell topleft_ to cell bottomright_. Cells are \
entered in format \"A B 12\".";

MLDMakeLabDataWOUnits[rawdata_] := 
 Transpose@
  Dataset[<|
    Table[rawdata[[1, i]] -> rawdata[[2 ;;, i]], {i, 1, 
      Length[rawdata[[1]]]}]|>];
MLDMakeLabDataWOUnits::usage = 
  "MLDMakeLabDataWOUnits[rawdata_] - makes a dataset from list of \
lists rawdata_ of data without units of measurements. First raw is \
considered as names of the columns, carefully!";
MLDMakeLabData[rawdata_] := Transpose@Dataset[<|
    Table[
     rawdata[[1, i]] -> If[
       rawdata[[2, i]] == "",
       rawdata[[3 ;;, i]],
       Quantity[rawdata[[3 ;;, i]], rawdata[[2, i]]]
       ]
     , {i, 1, Length[rawdata[[1]]]}
     ]
    |>];
MLDMakeLabData::usage = 
  "MLDMakeLabData[rawdata_] - makes a dataset from list of lists \
rawdata_ of data with units of measurements. First row is considered \
as names of the columns, and the second as coresponding units (if \
unitless, cell is left empty), carefully!";

MLDMakeColumnWErrors[data_, name_, values_, errs_] := 
 data[All, Append[#, name -> Around[#[values], #[errs]]] &];
MLDMakeColumnWErrors[data_, name_, nvalues_Integer, nerrs_Integer] := 
 With[{keys = Normal@Keys@Transpose@data},
  MLDMakeColumnWErrors[data, name, keys[[nvalues]], keys[[nerrs]]]
  ];

MLDMakeColumnWErrors::usage = 
  "MLDMakeColumnWErrors[data_,name_,values_,errs_] - makes a column \
name_, which is values of values_ with errors errs_ in dataset data_. \
values_ end errs_ can be names (String) or indeces (Integer).";

MLDSetUnit[data_, column_, unit_] := 
 data[All, {column -> (Quantity[#, unit] &)}];
MLDSetUnit[data_, n_, unit_] := With[
  {column = (Normal@Keys@Transpose@data)[[n]]},
  MLDSetUnit[data, column, unit]
  ];
MLDSetUnit::usage = 
  "MLDSetUnit[data_, column_, unit_] - sets unit unit_ to column \
column_ of dataset data_. column_ can be name (String) or index \
(Integer).";

MLDConvertUnit[data_, column_, unit_] := 
 data[All, {column -> (UnitConvert[#, unit] &)}];
MLDConvertUnit[data_, n_, unit_] := 
 With[{column = (Normal@Keys@Transpose@data)[[n]]},
  MLDConvertUnit[data, column, unit]
  ];
MLDConvertUnit::usage = 
  "MLDConvertUnit[data_, column_, unit_] - converts column column_ of \
dataset data_ into unit unit_. column_ can be name (String) or index \
(Integer).";

MLDFindColumnWithName[data_, name_] := 
 Block[{i, keys = Normal@Keys@Transpose@data},
  If[
   ContainsAll[keys, {name}],
   For[i = 0, i <= Length@keys, i++, If[keys[[i]] == name, Return[i]]],
   Return[-1]
   ]
  ];
MLDFindColumnWithName::usage = 
  "MLDFindColumnWithName[data_,name_] - returns index of column of \
dataset data_ named name_. If no such, returns -1.";

MLDRenameColumn[data_, n_Integer, name_] := 
 Block[{keys = Normal@Keys@Transpose@data}, 
  data[All, Append[#, name -> #[keys[[n]]]] &][
   All, (Range@Length@keys /. {n -> Length@keys + 1})]
  ];
MLDRenameColumn[data_, old_, name_] := 
 MLDRenameColumn[data, MLDFindColumnWithName[data, old], name];

MLDRenameColumn::usage = 
  "MLDRenameColumn[data_, old_, name_] - renames column old_ name \
name_. old_ can be name (String) or index (Integer).";

MLDNonLinearFit[data_, xcol_, ycol_, form_, params_, x_, options___] :=
  Block[{x}, Block[params,
   NonlinearModelFit[
    data[All, {xcol, ycol}][Values] // Normal // QuantityMagnitude, 
    form, params, x, options
    ]
   ]];
SetAttributes[MLDNonLinearFit, HoldAll];
MLDNonLinearFit::usage = 
  "MLDNonLinearFit[data_,xcol_,ycol_,form_,params_,x_,options_] - \
gives approximation of ycol_(xcol_) of dataset data_ of the form \
form_(x_) varying parameters params_={\[Alpha],\[Beta],\[Gamma]}, \
using options options_. options_ are optional (ironical) and are \
identical of those of NonLinearModelFit. Function will perform well \
if x_ is declared globally previously, and almost well if one or all \
of params_ is declared globally previously.";

MLDLinearFit[data_, xcol_, ycol_, funcs_, x_, options___] := Block[
  {x},
  LinearModelFit[
   data[All, {xcol, ycol}][Values] // Normal // QuantityMagnitude, 
   funcs, x, options
   ]
  ];
SetAttributes[MLDLinearFit, HoldAll];
MLDNonLinearFit::usage = 
  "MLDLinearFit[data_,xcol_,ycol_,funcs_,x_,options_] - gives \
approximation of ycol_(xcol_) of dataset data_ of with the linear \
combination of functions funcs_(x_), using options options_. options_ \
are optional (ironical) and are identical of those of \
NonLinearModelFit. Function will perform well if x_ is declared \
globally previously, and almost well if one or all of params_ is \
declared globally previously.";

MLDFitForm[fit_FittedModel /; fit[[1, 1]] == "Linear"] := With[
  {\[Alpha] = fit["ParameterTable"][[1, 1, 3, 2]], 
   d\[Alpha] = fit["ParameterTable"][[1, 1, 3, 3]], \[Beta] = 
    fit["ParameterTable"][[1, 1, 2, 2]], 
   d\[Beta] = fit["ParameterTable"][[1, 1, 2, 3]]},
  With[
   {\[CapitalDelta]\[Alpha] = 
     If[Length@d\[Alpha] == 2, Plus @@ d\[Alpha], 
      d\[Alpha]], \[CapitalDelta]\[Beta] = 
     If[Length@d\[Beta] == 2, Plus @@ d\[Beta], d\[Beta]]},
   (x |-> 
     Around[\[Alpha], \[CapitalDelta]\[Alpha]] x + 
      Around[\[Beta], \[CapitalDelta]\[Beta]])
   ]
  ];
MLDFitForm[fit_FittedModel /; fit[[1, 1]] == "Nonlinear"] := x |->
  (fit[[1, 3, 2]] /.
     Table[
      fit["ParameterTable"][[1, 1, i, 1]] -> 
       Around[fit["ParameterTable"][[1, 1, i, 2]], 
        fit["ParameterTable"][[1, 1, i, 3]]],
      {i, 2, Length@fit["ParameterTable"][[1, 1]]}
      ]
    /. {fit[[1, 3, 1, 1]] -> x}
   );
MLDFitForm::usage = 
  "MLDFitForm[fit_FittedModel] - returns a function, associated with \
fit_ (obtained for example from MLDNonLinearFit) where coefficients \
have their errors. Rather useful. Will NOT perform well, if the \
parameters used in fit_ are declared globally previously!";

MLDTexTable[data_List] := 
 Text["\\begin{table}[]\n  \\centering \n  \\begin{tabular}{|" <>
   StringJoin @@ Table["c|", {i, 1, Length[data[[1]]]}] <> 
   "} \n     \\hline \n     " <>
   "%insert columns' names manually into the line below!!! \n     \
\\\\ \\hline \n     " <>
   StringJoin @@ Table[
     StringJoin @@ 
       Table["$" <> 
         ToString@TeXForm[data[[j, i]] // QuantityMagnitude] <> 
         "$ & ", {i, 1, Length[data[[1]]] - 1}] <>
      "$" <> 
      ToString@
       TeXForm[data[[j, Length[data[[1]]]]] // QuantityMagnitude] <> 
      "$ \\\\ \n     "
     , {j, 1, Length[data]}] <>
   "\\hline \n   \\end{tabular} \n \\end{table}"];
MLDTexTable::usage = 
  "MLDTexTable[data_List] - return generated TeX code of the table \
data_, which can be then inserted directly into TeX. Also deletes \
units if present.";

MLD := "Hello! It's a small library, consisting of several functions \
for working with data using Wolfram during laboratory works. 
Original (raw) data may be stored in Excel, and then, using \
MLDReadExcel[], be stored in Wolfram list of lists. Then, \
MLDMakeLabData[] or MLDMakeLabDataWOUnits[] transforms it into \
Wolfram dataset. All other functions are made to work with datasets \
and only them.";

MLDHelp := 
  "MLDReadExcel[path_, topleft_, bottomright_] (*cells of format \"AB \
12\" *)
MLDMakeLabDataWOUnits[rawdata_]
MLDMakeLabData[rawdata_] (*from a list of lists, where 1st row is \
names of columns*)

MLDMakeColumnWErrors[data_,name_,values_,errs_] (*values_ end errs_ \
are names or indeces*)
MLDSetUnit[data_, column_, unit_]
MLDConvertUnit[data_, column_, unit_](*column_ is name or index*)

MLDFindColumnWithName[data_,name_]
MLDRenameColumn[data_, old_, name_] (*old_ is name or index*)

MLDNonLinearFit[data_,xcol_,ycol_,form_,params_,x_,options_]
MLDLinearFit[data_,xcol_,ycol_,funcs_,x_,options_] (*options_ are of \
format { , , }!!!*)
";

MLDDatasetHelp := 
  "Columns: \
{<|x\[Rule]\[EmptySmallSquare],y\[Rule]\[EmptySmallSquare],\[Ellipsis]\
|>,    Rows: <|a\[Rule]{\[EmptySmallSquare],\[EmptySmallSquare]},
                              <|x\[Rule]\[EmptySmallSquare],y\[Rule]\
\[EmptySmallSquare],\[Ellipsis]|>,             b\[Rule]{\
\[EmptySmallSquare],\[EmptySmallSquare]},
                              <|x\[Rule]\[EmptySmallSquare],y\[Rule]\
\[EmptySmallSquare],\[Ellipsis]|> }            c\[Rule]{\
\[EmptySmallSquare],\[EmptySmallSquare]}

dataset[[\"row\"]] - extract named row; dataset[[n]] - extract n-th \
row;
dataset[[All, \"col\"]] - extract named column; dataset[[All, n]] - \
extract n-th column;
dataset[[Values]], dataset[[All, Values]] - remove labels from \
rows/columns;

dataset[f, column], dataset[n, f] - apply f to column/row;
dataset[Min, string];
dataset[Count[value],string];
Total, Mean analog.;

dataset[Select[h]] - extract rows that satisfy h, example: \
dataset[Select[#a < 2 &]];
dataset[TakeLargestBy[row, n]] - give the n rows for which the named \
column is largest;
dataset[TakeLargest[n],column] - give the n largest values in the \
named column;
dataset[SortBy[criterion]];
dataset[DeleteDuplicatesBy[string]];

dataset[All, <|new -> old>] - rename columns (idk why fails when old \
is string);
dataset[All, Append[#, name -> func[ #[\"a\"], #[\"b\"] ]]&] - make \
new column;

titanic[Counts, column] - count number of appearences for each value \
of named column;
titanic[Histogram, column] - histogram for column;
titanic[GroupBy[column1], Histogram[#, {0, 80, 4}] &, column2] - make \
a histogram of column2 for each value of column1;
";

MLDPlotHelp := "AspectRatio -> m/n - ratio of height to width;
Axes -> True;
AxesLable -> {x, y};
AxesOrigin -> {x0, y0};
AxisStyle -> {Directive[Dashed, Red], Blue};
Background -> LightBlue;
ColourFunction -> \"DarkRainbow\" - color of the curve;
Epilog -> {} - what to do after you plot, 
    ex: ListLinePlot[sdata,Epilog->{PointSize[Medium],Point[sdata]}, \
InterpolationOrder->2];
Exclusions -> None / {Pi, -Pi} - what points to exclude;
Filling -> Axis / Top / Bottom / 0.3 - coloring region under/above \
the curve;
FillingStyle -> Red; 
Frame -> {{True, False},{False, True}};
FrameLabel -> {{,},{,}};
FrameStyle ->;
FrameTics -> Automatic;
GridLines -> {{xlines}, {ylines}};
IntervalMarkers -> \"Bars\" / \"Bands\";
IntervalMarkersStyle -> Red;
Joined -> True - whether to join points of ListPlot;
LabelingSize ->;
MaxRecursion -> n - recursion subdivisions idk;
Mesh -> {xofmesh} / n - points on the curve;
MeshStyle -> {Red, PointSize[Medium]};
MeshShading -> {Red, None} - how to draw the curve between \
meshpoints;
PerformanceGoal -> \"Speed\" / \"Quality\";
PlotHighlighting -> \"Ball\" / \"Dropline\" / \"XSlice\";
PlotLabel -> \"label\";
PlotLabels -> {};
PlotLayout -> \"Row\" / {\"Column\", 4} - how to display several plots;
PlotLegends -> \"Expressions\" / \"Placeholder\" / {,};
PlotMarkers -> \"OpenMarkers\" / {\[Alpha],\[Beta],\[Gamma]};
PlotPoints -> n;
PlotRange -> All;
PlotStyle -> Directive[Red, Dashed];
!PlotTheme -> \"Business\" / !\"Detailed\" / \"Minimal\" / \
\"Monochrome\" / \"Scientific\" / \"Classic\" /
              \"FullAxesGrid\" / \"BoldColor\" / \"CoolColor\" / \
\"DarkColor\" / \"GrayColor\" / \"PastelColor\" /
              \"BoldLabels\" / \"ItalicLabels\" / \"LargeLabels\" / \
\"SmallLabels\" / \"ThinLines\";
PlotRangeClipping -> True;
Prolog -> {} - what to do before you plot;
TargetUnits -> unit - to change the units;
Ticks -> {{xticks},{yticks}};
TicksStyle -> Directive[Red, Bald];
WorkingPrecision -> n / \"MachinePrecision\"
";
