#!/usr/bin/env wolframscript

dataFolder = "data";
shape = "leech_vertices";
dataFileName = FileNameJoin[{dataFolder, shape <> ".wl"}];

exportToPov = True;
rangeFactor = 0.78;
thicknessRatio = 0.003;
imageFolder = "output";
imageSize = {4, 3} * 240;
roundPrecision = 0.000001;

start = AbsoluteTime[];
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

vertices = Get[dataFileName];
(* viewPoint = {0, 0, 1}; *)
viewPoint = {1, -1, 1}/Sqrt[3];
rotation = RotationMatrix[{{0, 0, 1}, viewPoint}];
lighting = {{"Point", White, viewPoint * 10}};

getProjectionMatrix[] := 
    (* 42a *)
    {
        {
        0.42106755308607178026741442348366006,   -0.027032832537504892079117708662154807, 0.42848580882876254007761230218098422,   -  0.37921361887720244291782911319528545,  -0.00034912708073628519848191550718945172,   -0.28358996717979976478145998689107116,  -0.40306329220539667806687084010863428,  0.049698776148647632368111846650370306,  -0.05530448851224257066489058898236828,  0.07591468606638557888837720152760013,   0.14343442165000684923646277959133588,   -0.10197506381824443542609491916779396,  -0.094011996361551764585288596046741809, 0.10886031472049612993600423841219367,  -0.017322164959455581937308528538070413, 0.080614166112221096157539388910737667,  0.0061400989498499690616269675844420372, 0.086167307505664955101487622754005174,  0.18276660907932375914227198906428703,   0.32682287096949818601607539435237241,   -0.084916391171505739535297685613447645, -0.089555213108349330102458657492623523, -0.13031294226086872852565123556110797,  0.045185052127277172558135522886647575
        },
        {
        0.,  -0.05774816608889871304040019259887453,  -0.016288214020786447236572380249934274, -0.21888466954877713887904819348573874,  -0.21129783845990104556494120385444282,  0.018180538153442765159223559662253006,  0.33081611824855120106453065352029564,   -0.12132464232266780233168937772129927,  0.079625933799196614500866575676755405,  0.073685289588337971195584944142184824,  -0.15245341316570725996332198928774373,  -0.076892548202822463919386636613967035, -0.2310428834286608410958481147791904,   0.40262128389288714159057056195593461,   0.27327243046620684193242773025654395,   -0.34703327920228196200056890723384898,  -0.32614168481232659179190011448764191,  0.097857868060908785247501561230889459,  0.16030619898317510948141075283901558,   -0.070045301839338040665846789341169987, -0.076620147324063400321292404212177451, 0.20240118215464403555267146670249379,  -0.2609524738388319731316723938074607,   0.25758892228755384209685450310778632
        },
        Table[0, {i, 24}]
    };

exportImage[serialNumber_, frameFolder_] := Module[
    {verticesOnEdges, verticesTally, cropCommand,
        graph, outputFolder, outputFileName},

    projectionMatrix = getProjectionMatrix[] * 1.0;

    vertices3d = vertices.Transpose[projectionMatrix].Transpose[rotation];
    matrixNorm = Norm[projectionMatrix[[1]]];

    maxNorm = Max[Map[Norm, vertices3d]];
    Print[maxNorm//N];
    (* Print[matrixNorm//N]; *)
    (* matrix norm is always 1 *)
    maxNorm = 4.3;

    projectTo3d[vertex_] := vertex/maxNorm;
    range = rangeFactor;

    thickness = thicknessRatio * range;
    
    roundedVertices3d = Round[vertices3d, roundPrecision];
    verticesTally = Tally[Expand[roundedVertices3d]];
    (* verticesTally = Tally[Expand[vertices3d]]; *)
    Print["Finished tallying " <> IntegerString[verticesTally//Length]];
    tallyTally = Tally[Map[#[[2]]&, verticesTally]];
    Print["Distribution of tally counts:"];
    Print[Sort[tallyTally]];

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
        shape, frameFolder, gifFileName, gifFolder, outputFolder, maxTheta,
        thetaStep, epsilon, thetas, theta, gifCommand
    },

    shape = "leech";
    frameFolder = shape <> "_42a" <> "_frames";
    gifFileName = shape <> "_42a" ;
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    frameIndex = 1;
    For[angle = 0, angle <= 360, angle += 1,
        Print["Frame #" <> IntegerString[frameIndex] <> "  angle: " <> IntegerString[angle]];
        If[Mod[angle, 1] == 0,
            exportImage[frameIndex, frameFolder];
            frameIndex ++;
        ];
    ];

    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[3] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

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

(* exportAnimation[True]; *)
(* exportAnimation[False]; *)

exportImage[17, "test"];


end = AbsoluteTime[];
Print["Elapsed time in seconds: " <> ToString[end - start]];
