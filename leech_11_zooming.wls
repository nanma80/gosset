#!/usr/bin/env wolframscript

dataFolder = "data";
shape = "leech_vertices";
dataFileName = FileNameJoin[{dataFolder, shape <> ".wl"}];

exportToPov = True;
lighting = {{"Point", White, {10, -10, 10}}};
rangeFactor = 0.78;
thicknessRatio = 0.003;
imageFolder = "output";
imageSize = {4, 3} * 240;
roundPrecision = 0.000001;
maxZoom = 1.0;
minZoom = 0.02;
frameCount = 10;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

vertices = Get[dataFileName];
viewPoint = {0, 0, 1};

getProjectionMatrix[t_] := Module[
    {projectionMatrix, i, j, renormer, elevenCycle},
    projectionMatrix = Table[0, {i, 3}, {j, 24}];
    renormer = Sqrt[11];
    (* from http://www.madore.org/~david/math/higman-sims-graph.html *)
    elevenCycle = {
        {0}, {1, 0}, {1, 1}, {1, 8},
        {0}, {2, 0}, {2, 3}, {2, 10},
        {2, 8}, {1, 2}, {1, 9}, {1, 6}, 
        {1, 3}, {2, 9}, {2, 5}, {2, 2}, 
        {2, 7}, {1, 10}, {1, 5}, {1, 7}, 
        {1, 4}, {2, 1}, {2, 4}, {2, 6}
    };

    For[i = 0, i < 24, i++,
        If[elevenCycle[[i + 1]][[1]] == 0,
            projectionMatrix[[1]][[i+1]] = 0;
            projectionMatrix[[2]][[i+1]] = 0
        ];

        If[elevenCycle[[i + 1]][[1]] == 1,
            projectionMatrix[[1]][[i+1]] = Cos[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) + t]/renormer;
            projectionMatrix[[2]][[i+1]] = -Sin[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) + t]/renormer
        ];

        If[elevenCycle[[i + 1]][[1]] == 2,
            projectionMatrix[[1]][[i+1]] = Cos[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) - t]/renormer;
            projectionMatrix[[2]][[i+1]] = -Sin[2 * Pi * (elevenCycle[[i + 1]][[2]] / 11) - t]/renormer
        ];
    ];
    projectionMatrix
];

exportImage[zoom_, serialNumber_, frameFolder_] := Module[
    {verticesOnEdges, cropCommand,
        graph, outputFolder, outputFileName},

    range = zoom * rangeFactor;
    thickness = thicknessRatio * range / zoom^(1/3);

    graph = Graphics3D[
        Map[{
            Opacity[1], 
            FaceForm[Hue[Mod[ #[[2]] - 1 , 13]/13, 1, 1 ]], 
            Sphere[projectTo3d[#[[1]]], thickness * (#[[2]])^(1/3) ]
            }&, verticesTally],
        ViewPoint -> viewPoint * 3000,
        PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
        Lighting -> lighting
    ];

    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

    outputFileName = "animation_frame_" <> IntegerString[serialNumber, 10, 4] <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];

    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    If[exportToPov,
        If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
        POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
        Run[cropCommand];
        Print[cropCommand]
        ,
        Export[outputFileName, graph];
    ]
    Print["Saved the image to " <> outputFileName];
];

exportAnimation[] := Module[
    {
        shape, frameFolder, gifFileName, gifFolder, outputFolder, zoomFactor, zoomFactors, frameIndex, gifCommand
    },

    shape = "leech";
    frameFolder = shape <> "_11_zooming" <> "_frames";
    gifFileName = shape <> "_11_zooming";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    zoomFactors = Table[(maxZoom - minZoom)/2 Cos[t] + (maxZoom + minZoom)/2, {t, 0, Pi, Pi/(frameCount - 1)}];

    For[frameIndex = 1, frameIndex <= Length[zoomFactors], frameIndex ++,
        zoomFactor = zoomFactors[[frameIndex]];
        Print["Frame #" <> IntegerString[frameIndex] <> "  zoomFactor: " <> ToString[zoomFactor]];
        If[Mod[frameIndex, 1] == 0,
            exportImage[zoomFactor, frameIndex, frameFolder];
        ];
    ];

    gifCommand = "magick convert -duplicate 1,-2-1 -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[30] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];

    mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
        <> " -movflags faststart -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" "
        <> gifFolder <> "\\" <> gifFileName <> ".mp4";

    mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
    If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
    Print[mp4Command];
    Run[mp4Command];
    Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"];
];

projectionMatrix = getProjectionMatrix[0] * 1.0;

vertices3d = vertices.Transpose[projectionMatrix];
matrixNorm = Norm[projectionMatrix[[1]]];

maxNorm = Max[Map[Norm, vertices3d]];
Print[maxNorm//N];
(* Print[matrixNorm//N]; *)
(* matrix norm is always 1 *)
maxNorm = 4.3;

projectTo3d[vertex_] := vertex/maxNorm;

roundedVertices3d = Round[vertices3d, roundPrecision];
verticesTally = Tally[Expand[roundedVertices3d]];
(* verticesTally = Tally[Expand[vertices3d]]; *)
Print["Finished tallying " <> IntegerString[verticesTally//Length]];
tallyTally = Tally[Map[#[[2]]&, verticesTally]];
Print["Distribution of tally counts:"];
Print[Sort[tallyTally]];


exportAnimation[];

(* exportImage[0.5, 11, "test"];
exportImage[0.1, 12, "test"];
exportImage[0.03, 13, "test"];
 *)

end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
