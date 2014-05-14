(* ::Package:: *)

(* ::Subsection:: *)
(*Needed Functions*)


makeBaseFromPoly[poly_]:=Module[{xVector,yVector,zVector},
xVector=Normalize[Last[Last[ Sort[ {Norm[#[[2]]-#[[1]] ],#[[2]]-#[[1]]}& /@ Partition[Append[poly,First[poly]],2,1] ]]]];
zVector=Normalize[-Cross[poly[[2]]-poly[[1]],poly[[3]]-poly[[1]]]];
yVector=Normalize[Cross[zVector,xVector]];
Chop[{xVector,yVector,zVector}]
]


spherePoints[NN_, R_, L_, x_, y_, z_]:=Module[{n, points},points={};
For[n=1, n<= NN, n++,points=Append[points, {N[2R Cos[n Pi / NN] Sin [n Pi / NN]] + x, N[2R Sin[n Pi / NN] Sin [n Pi / NN]]-R+y,L+z}]];
points]


rodNormals[NN_, R_, L_]:=Module[{n, points},points={};
For[n=1, n<= NN+1, n++,points=Append[points, {N[2R Cos[(n -0.5)Pi / NN] Sin [(n -0.5) Pi / NN]], N[2R Sin[(n -0.5) Pi / NN] Sin [(n -0.5) Pi / NN]]-R ,0}]];
points]


(* ::Subsection:: *)
(*Primitives*)


cyl[NN_,L_, R_, x_, y_, z_]:=Module[{rodNormal, topRodPoints, bottomRodPoints,cylinder},
rodNormal=Append[rodNormals[NN,L], First[rodNormals[NN,L]]];
topRodPoints=Append[spherePoints[NN,R,L, x, y, z], First[spherePoints[NN,R,L, x,y,z]]];
bottomRodPoints=Append[spherePoints[NN,R,-L, x,y,z], First[spherePoints[NN,R,-L, x, y,z]]];
H={rodNormal, topRodPoints, bottomRodPoints};
cylinder={};
For[i=1, i<=NN, 
cylinder=Append[cylinder,{{topRodPoints[[i]], topRodPoints[[i+1]],bottomRodPoints[[i]]},rodNormal[[i+1]],{{1,2},{2,3},{3,1}},{}}];
cylinder=Append[cylinder,{{bottomRodPoints[[i]], bottomRodPoints[[i+1]],topRodPoints[[i+1]]},rodNormal[[i+1]],{{1,2},{2,3},{3,1}},{}}];

cylinder=Append[cylinder,{{bottomRodPoints[[i]], bottomRodPoints[[i+1]],{x,y,z-L}},{0,0,-1},{{1,2},{2,3},{3,1}},{}}];

cylinder=Append[cylinder,{{topRodPoints[[i]], topRodPoints[[i+1]], {x,y,z+L}},{0,0,1},{{1,2},{2,3},{3,1}},{}}];

i++];
cylinder]


rod[{x_, y_, z_},{a_,b_,c_}, theta_]:=Module[{p1,p2,p3, p4, p5,p6,p7, p8, rotMat, Edges, pointsA, pointsB, pointsC, pointsD, pointsE, pointsF,
pointsZ, pointsY, pointsX,pointsW, pointsV, pointsU},
rotMat={{Cos[theta],-Sin[theta],0},{Sin[theta],Cos[theta],0},{0,0,1}};
p1=rotMat.{x-a/2,y-b/2,z-c/2};
p2=rotMat.{x-a/2,y-b/2,z+c/2};
p3=rotMat.{x-a/2,y+b/2,z-c/2};
p4=rotMat.{x-a/2,y+b/2,z+c/2};
p5=rotMat.{x+a/2,y-b/2,z-c/2};
p6=rotMat.{x+a/2,y-b/2,z+c/2};
p7=rotMat.{x+a/2,y+b/2,z-c/2};
p8=rotMat.{x+a/2,y+b/2,z+c/2};
Edges={{1,2},{2,3},{3,1}};
(*1,2,3,4*)
pointsA={p2,p3, p4};
pointsZ={p2, p3,p1};
(*5,6,7,8*)
pointsB={p6,p5, p7};
pointsY={p6, p8,p7};
(*1,2,5,6*)
pointsC={p1,p2, p5};
pointsX={p2, p5,p6};
(*1,3,5,7*)
pointsD={p1,p3, p7};
pointsW={ p1,p7,p5};
(*3,4,7,8*)
pointsE={p3,p4, p7};
pointsV={p4,p7, p8};
(*2,4,6,8*)
pointsF={p8,p4, p6};
pointsU={ p2,p6,p4};
(*pointsA={p1,p2,p3, p4};
pointsB={p5,p6,p7, p8};
pointsC={p3,p4,p7, p8};
pointsD={p1,p2,p5, p6};
pointsE={p2,p4,p6, p8};
pointsF={p1,p3,p5, p7};*)
{{ pointsZ,-N[makeBaseFromPoly[pointsZ]][[3]], Edges, {}},
{ pointsY,N[makeBaseFromPoly[pointsY]][[3]], Edges, {}},
{ pointsX,-N[makeBaseFromPoly[pointsX]][[3]], Edges, {}},
{ pointsW,-N[makeBaseFromPoly[pointsW]][[3]], Edges, {}},
{ pointsV,N[makeBaseFromPoly[pointsV]][[3]], Edges, {}},
{ pointsU,-N[makeBaseFromPoly[pointsU]][[3]], Edges, {}},
{ pointsA,N[makeBaseFromPoly[pointsA]][[3]], Edges, {}},
{pointsB,-N[makeBaseFromPoly[pointsB]][[3]], Edges,{}},
{pointsC,N[makeBaseFromPoly[pointsC]][[3]], Edges,{}},
{pointsD,-N[makeBaseFromPoly[pointsD]][[3]], Edges,{}},
{pointsE,-N[makeBaseFromPoly[pointsE]][[3]], Edges, {}},
{pointsF,-N[makeBaseFromPoly[pointsF]][[3]], Edges, {}}}]


tetrahedron[{x_,y_,z_},L_,theta_]:=Module[{rotMat,p1,p2,p3,p4,Edges,pointsA,pointsB,pointsC,pointsD},
rotMat={{Cos[theta],-Sin[theta],0},{Sin[theta],Cos[theta],0},{0,0,1}};
p1=rotMat.{x,y,z};
p2=rotMat.{x+L,y,z};
p3=rotMat.{x+L/2,y+Sqrt[3]*L/2,z};
p4=rotMat.{x+L/2,y+L/(2*Sqrt[3]),z+Sqrt[5/6]*L};
Edges={{1,2},{2,3},{3,1}};
pointsA={p1,p2,p3};
pointsB={p2,p1,p4};
pointsC={p3,p2,p4};
pointsD={p1,p3,p4};
{{ pointsA,N[makeBaseFromPoly[pointsA]][[3]], Edges, {}},
{pointsB,N[makeBaseFromPoly[pointsB]][[3]], Edges,{}},
{pointsC,N[makeBaseFromPoly[pointsC]][[3]], Edges,{}},
{pointsD,N[makeBaseFromPoly[pointsD]][[3]], Edges,{}}}]


cube[{x_, y_, z_},L_, theta_]:=Module[{p1,p2,p3, p4, p5,p6,p7, p8, rotMat, Edges, pointsA, pointsB, pointsC, pointsD, pointsE, pointsF,
pointsZ, pointsY, pointsX,pointsW, pointsV, pointsU},
rotMat={{Cos[theta],-Sin[theta],0},{Sin[theta],Cos[theta],0},{0,0,1}};
p1=rotMat.{x-L/2,y-L/2,z-L/2};
p2=rotMat.{x-L/2,y-L/2,z+L/2};
p3=rotMat.{x-L/2,y+L/2,z-L/2};
p4=rotMat.{x-L/2,y+L/2,z+L/2};
p5=rotMat.{x+L/2,y-L/2,z-L/2};
p6=rotMat.{x+L/2,y-L/2,z+L/2};
p7=rotMat.{x+L/2,y+L/2,z-L/2};
p8=rotMat.{x+L/2,y+L/2,z+L/2};
Edges={{1,2},{2,3},{3,1}};
(*1,2,3,4*)
pointsA={p2,p3, p4};
pointsZ={p2, p3,p1};
(*5,6,7,8*)
pointsB={p6,p5, p7};
pointsY={p6, p8,p7};
(*1,2,5,6*)
pointsC={p1,p2, p5};
pointsX={p2, p5,p6};
(*1,3,5,7*)
pointsD={p1,p3, p7};
pointsW={ p1,p7,p5};
(*3,4,7,8*)
pointsE={p3,p4, p7};
pointsV={p4,p7, p8};
(*2,4,6,8*)
pointsF={p8,p4, p6};
pointsU={ p2,p6,p4};
(*pointsA={p1,p2,p3, p4};
pointsB={p5,p6,p7, p8};
pointsC={p3,p4,p7, p8};
pointsD={p1,p2,p5, p6};
pointsE={p2,p4,p6, p8};
pointsF={p1,p3,p5, p7};*)
{{ pointsZ,-N[makeBaseFromPoly[pointsZ]][[3]], Edges, {}},
{ pointsY,N[makeBaseFromPoly[pointsY]][[3]], Edges, {}},
{ pointsX,-N[makeBaseFromPoly[pointsX]][[3]], Edges, {}},
{ pointsW,-N[makeBaseFromPoly[pointsW]][[3]], Edges, {}},
{ pointsV,N[makeBaseFromPoly[pointsV]][[3]], Edges, {}},
{ pointsU,-N[makeBaseFromPoly[pointsU]][[3]], Edges, {}},
{ pointsA,N[makeBaseFromPoly[pointsA]][[3]], Edges, {}},
{pointsB,-N[makeBaseFromPoly[pointsB]][[3]], Edges,{}},
{pointsC,N[makeBaseFromPoly[pointsC]][[3]], Edges,{}},
{pointsD,-N[makeBaseFromPoly[pointsD]][[3]], Edges,{}},
{pointsE,-N[makeBaseFromPoly[pointsE]][[3]], Edges, {}},
{pointsF,-N[makeBaseFromPoly[pointsF]][[3]], Edges, {}}}]


stretchHedra[hedra_,a_,b_,c_]:=Module[
{polys, points, pts, average, newPolys, stretchedPolys, polysReturned, A={}},
polys=First[#]&/@hedra;
points=Flatten[polys,1];
pts=Complement[points];
average=Mean[pts];
newPolys=((#-average)&/@#)&/@polys;
stretchedPolys=({a*#[[1]], b*#[[2]], c*#[[3]]}&/@#)&/@newPolys;
polysReturned=((#+average)&/@#)&/@stretchedPolys;
For[i=1, i<=Length[hedra],
A=Append[A, {polysReturned[[i]], hedra[[i]][[2]], hedra[[i]][[3]], hedra[[i]][[4]]}];
i++];

(*THE NORMALS CHANGE HERE AS WELL SO WE NEED TO FIX THIS!
Partition[Riffle[polysReturned,#[[2;;4]]&/@hedra],2]*)
A
]


rotatePoly[poly_, thetaZ_, thetaY_,thetaX_]:=Module[{rotMatZ, meanPoint, rotatedShiftedPolyZ, rotatedShiftedPolyZY,unShiftedPoly, rotMatY, rotMatX, rotatedShiftedPolyZYX},
meanPoint=Mean[Mean[poly]];
rotMatZ={{Cos[thetaZ],-Sin[thetaZ],0},{Sin[thetaZ],Cos[thetaZ],0},{0,0,1}};
rotMatY={{Cos[thetaY],0,-Sin[thetaY]},{0,1,0},{Sin[thetaY],0,Cos[thetaY]}};
rotMatX={{1,0,0},{0,Cos[thetaX],-Sin[thetaX]},{0,Sin[thetaX],Cos[thetaX]}};
rotatedShiftedPolyZ=(rotMatZ.(#-meanPoint)&/@#)&/@poly;
rotatedShiftedPolyZY=(rotMatY.(#-{0,0,0})&/@#)&/@rotatedShiftedPolyZ;
rotatedShiftedPolyZYX=(rotMatX.(#-{0,0,0})&/@#)&/@rotatedShiftedPolyZY;
unShiftedPoly=((#+meanPoint)&/@#)&/@rotatedShiftedPolyZYX]


rotateNormal[poly_, thetaZ_, thetaY_,thetaX_]:=Module[{rotMatZ, meanPoint, rotatedShiftedPolyZ, rotatedShiftedPolyZY,unShiftedPoly, rotMatY, rotMatX, rotatedShiftedPolyZYX},
meanPoint={0,0,0};
rotMatZ={{Cos[thetaZ],-Sin[thetaZ],0},{Sin[thetaZ],Cos[thetaZ],0},{0,0,1}};
rotMatY={{Cos[thetaY],0,-Sin[thetaY]},{0,1,0},{Sin[thetaY],0,Cos[thetaY]}};
rotMatX={{1,0,0},{0,Cos[thetaX],-Sin[thetaX]},{0,Sin[thetaX],Cos[thetaX]}};
rotatedShiftedPolyZ=rotMatZ.poly;
rotatedShiftedPolyZY=rotMatY.rotatedShiftedPolyZ;
rotatedShiftedPolyZYX=rotMatX.rotatedShiftedPolyZY]


translateHedra[rightSideShape_, xxx_, yyy_, zzz_, tx_, ty_, tz_]:=Module[{A={}, stuf3,stuf4, polyHandl, polyHandlN, polyHandlPoints, polyHandlNormal},
polyHandl=(First[#]+{{xxx,yyy,zzz},{xxx,yyy,zzz},{xxx,yyy,zzz}})&/@rightSideShape;
stuf3=#[[3]]&/@rightSideShape;
stuf4=#[[4]]&/@rightSideShape;
polyHandlN=(#[[2]])&/@rightSideShape;
polyHandlPoints=rotatePoly[polyHandl, tx, ty, tz];
polyHandlNormal=rotateNormal[#, tx, ty, tz]&/@polyHandlN;
For[i=1,i<=Length[stuf4],
A=Append[A,{polyHandlPoints[[i]], polyHandlNormal[[i]], stuf3[[i]], stuf4[[i]]}];
i++];
A]


sphereFaces2=(PolyhedronData["PentakisDodecahedron", "Faces"][[1]][[#]])&/@(#&/@(PolyhedronData["PentakisDodecahedron", "Faces"][[2]][[1]]));
