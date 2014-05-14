(* ::Package:: *)

FindZPoint[{{x1_, y1_, z1_}, {x2_,y2_,z2_}}]:={N[x1-((x1-x2)/(z1-z2))*z1],N[y1-((y1-y2)/(z1-z2))*z1], 0};


inorout[x_] := If[x > 0, "Outside", "Inside"]


reduceList[{aa_, bb_,cc_}]:=Module[{l={}, x, y, z},
x=Chop[aa-bb];
y=Chop[aa-cc];
z=Chop[cc-bb];
If[x=={0,0,0},l={aa,cc}];
If[y=={0,0,0},l={aa,bb}];
If[z=={0,0,0},l={aa,bb}];
l]


makeBaseFromPoly[poly_]:=Module[{xVector,yVector,zVector},
xVector=Normalize[Last[Last[ Sort[ {Norm[#[[2]]-#[[1]] ],#[[2]]-#[[1]]}& /@ Partition[Append[poly,First[poly]],2,1] ]]]];
zVector=Normalize[-Cross[poly[[2]]-poly[[1]],poly[[3]]-poly[[1]]]];
yVector=Normalize[Cross[zVector,xVector]];
Chop[{xVector,yVector,zVector}]
];


DetermineCut3[pts_,edges_]:=Module[{position, values, newpts},
position=If[#[[3]]>0,"above","below"]&/@#&/@(pts[[#]]&/@edges);
values=Partition[Riffle[position,pts[[#]]&/@edges],2];
newpts=If[(#[[1,2]]=="above"&&#[[1,1]]=="below")||(#[[1,1]]=="above"&&#[[1,2]]=="below"), FindZPoint[#[[2]]],"no cut"]&/@values;
Select[newpts,#=!="no cut"&]]


DetermineCut[pts_,edges_]:=Module[{position, values, newpts, AA, BB, CC,result},
AA=If[(pts[[1]][[3]] > 0 && pts[[2]][[3]] <= 0) ||(pts[[1]][[3]] >= 0 && pts[[2]][[3]] < 0) 
|| (pts[[1]][[3]] < 0 && pts[[2]][[3]] >= 0) ||(pts[[1]][[3]] <= 0 && pts[[2]][[3]] > 0), FindZPoint[{pts[[1]],pts[[2]]}], "A"];
BB=If[(pts[[2]][[3]] > 0 && pts[[3]][[3]] <= 0) ||(pts[[2]][[3]] >= 0 && pts[[3]][[3]] < 0) 
||(pts[[2]][[3]] < 0 && pts[[3]][[3]] >= 0) ||(pts[[2]][[3]] <= 0 && pts[[3]][[3]] > 0), FindZPoint[{pts[[2]],pts[[3]]}], "B"];
CC=If[(pts[[3]][[3]] > 0 && pts[[1]][[3]] <= 0) ||(pts[[3]][[3]] >= 0 && pts[[1]][[3]] < 0)  
||(pts[[3]][[3]] < 0 && pts[[1]][[3]] >= 0) ||(pts[[3]][[3]] <= 0 && pts[[1]][[3]] > 0), FindZPoint[{pts[[3]],pts[[1]]}], "C"];
result=Complement[Select[{AA, BB, CC}, Not[StringQ[#]] &]];
If[Length[result]>2,reduceList[result], result]
]


(*Provided with two points, return the equation of the line (the slope and the y intercept)*)
EquationOfLine2[{{x1A_,y1A_, z1A_},{x2A_,y2A_,z2A_}}]:=
Module[{mA, bA},
(*Need to account for the possibility that the slope is infinity!*)
{mA,bA}=If[x1A!=x2A,
{((y1A-y2A)/(x1A-x2A)),y1A-mA*x1A},{Infinity,x1A}]]


DetermineIfPointInPoly[pt_,polyPts_,polyEdges_]:=
Module[{ m1,m2,m3, b1,b2,b3, Y,XX1, XX2, XX3, linePoints, 
x1A,y1A,z1A, x1B,y1B,z1B, x2A,y2A,z2A, x2B,y2B,z2B, x3A,y3A,z3A, x3B,y3B,z3B, TF, yMax, yMin}, 
(*Given the polygon with the edges indexed by points, we determine the line segments*)
{{{x1A,y1A,z1A},{x1B,y1B,z1B}},{{x2A,y2A,z2A},{x2B,y2B,z2B}},{{x3A,y3A,z3A},{x3B,y3B,z3B}}}=polyPts[[#]]&/@polyEdges;
yMax=Max[y1A,y2A,y3A];
yMin=Min[y1A,y2A,y3A];
(*Each line segment can be represented by a line equation in that range*)
{m1,b1}=EquationOfLine2[{{x1A,y1A,z1A},{x1B,y1B,z1B}}];
{m2,b2}=EquationOfLine2[{{x2A,y2A,z2A},{x2B,y2B,z2B}}];
{m3,b3}=EquationOfLine2[{{x3A,y3A,z3A},{x3B,y3B,z3B}}];
(*Write an equation of a line that has constant y value and goes through the pt we want to determine if it is in the poly or not*)
Y=pt[[2]];
(*Plug the equation of this line into the equations describing the line segments and determine where the lines intersect, at what x value?*)
XX1=If[m1==Infinity, b1,If[m1!=0,(Y-b1)/m1,Infinity]];
XX2=If[m2==Infinity, b2,If[m2!=0,(Y-b2)/m2,Infinity]];
XX3=If[m3==Infinity, b3,If[m3!=0,(Y-b3)/m3,Infinity]];
(*XX2=If[m2!=0,(Y-b2)/m2,Infinity];
XX3=If[m3!=0,(Y-b3)/m3,Infinity];*)
(*This is the crummiest part of the code.  The condition for a point to be in a poly is that an extending ray will cross the poly's edges
an odd number of times (we use 1 here because we are dealing only with triangles).  The problem arises when the extending ray intersects a 
vertex of the polygon so that it possibly crosses both lines (or neither lines).  There is a way to figure out with less conditions, for now we'll stick with this...*)
TF={
If[m1>0,
If[(Y<=y1A &&XX1<=x1A && Y>y1B && XX1>=x1B && XX1>pt[[1]]&& Y!= yMax && Y!= yMin)||
(Y>= y1A && XX1>=x1A &&  Y<y1B && XX1<=x1B && XX1>pt[[1]]&& Y!= yMax && Y!= yMin),True,False],
If[(Y>=y1A &&XX1<=x1A && Y<y1B && XX1>=x1B && XX1>pt[[1]]&& Y!= yMax && Y!= yMin)||
(Y<= y1A && XX1>=x1A &&  Y>y1B && XX1<=x1B && XX1>pt[[1]]&& Y!= yMax && Y!= yMin),True,False]],
If[m2>0,
If[(Y<=y2A &&XX2<=x2A && Y>y2B && XX2>=x2B && XX2>pt[[1]]&& Y!= yMax && Y!= yMin)||
(Y>= y2A && XX2>=x2A &&  Y<y2B && XX2<=x2B && XX2>pt[[1]]&& Y!= yMax && Y!= yMin),True,False],
If[(Y>=y2A&& XX2<=x2A && Y<y2B && XX2>=x2B && XX2>pt[[1]]&& Y!= yMax && Y!= yMin)||
(Y<= y2A && XX2>=x2A &&  Y>y2B && XX2<=x2B && XX2>pt[[1]]&& Y!= yMax && Y!= yMin),True,False]],
If[m3>0,
If[(Y<=y3A &&XX3<=x3A && Y>y3B && XX3>=x3B && XX3>pt[[1]]&& Y!= yMax && Y!= yMin)||
(Y>= y3A && XX3>=x3A &&  Y<y3B && XX3<=x3B && XX3>pt[[1]]&& Y!= yMax && Y!= yMin),True,False],
If[(Y>=y3A &&XX3<=x3A && Y<y3B && XX3>=x3B && XX3>pt[[1]]&& Y!= yMax && Y!= yMin)||
(Y<= y3A && XX3>=x3A &&  Y>y3B && XX3<=x3B && XX3>pt[[1]]&& Y!= yMax && Y!= yMin),True,False]]
};
If[Count[TF,True]==1,"Point in Poly", "Not Inside"]]



(*Given two line segments (edges of polygons), determine where (if) the lines intersect within this range*)
DetermineIntersect2[{{{x1A_,y1A_,z1A_},{x2A_,y2A_,z2A_}},{{x1B_,y1B_,z1B_},{x2B_,y2B_,z2B_}}}]:=
Module[{mA,mB,bA,bB, X, coor},

If[x1A!=x2A && x1B!=x2B,
mA=((y1A-y2A)/(x1A-x2A));
mB=((y1B-y2B)/(x1B-x2B));
bA=y1A-mA*x1A;
bB=y1B-mB*x1B;
coor=If[mA!=mB,{X=(bB-bA)/(mA-mB), mA*X+bA, 0},{X=Infinity, Infinity, 0}],
If[x1A==x2A && x1B!=x2B,
mB=((y1B-y2B)/(x1B-x2B));
bB=y1B-mB*x1B;
coor={X=x1A, mB*X+bB,0},
If[x1A!=x2A && x1B==x2B,
mA=((y1A-y2A)/(x1A-x2A));
bA=y1A-mA*x1A;
coor={X=x1B, mA*X+bA,0},
coor={X=Infinity, Infinity, 0}]]];

If[(X>x1A &&X<=x2A)||(X<x1A && X>=x2A), coor, False]
]


DetermineIntersect4[{{{x1A_,y1A_,z1A_},{x2A_,y2A_,z2A_}},{{x1B_,y1B_,z1B_},{x2B_,y2B_,z2B_}}}]:=
Module[{mA,mB,bA,bB, X, coor},
If[x1A!=x2A && x1B!=x2B,
mA=((y1A-y2A)/(x1A-x2A));
mB=((y1B-y2B)/(x1B-x2B));
bA=y1A-mA*x1A;
bB=y1B-mB*x1B;
coor=If[mA!=mB,{X=(bB-bA)/(mA-mB), mA*X+bA, 0},{X=Infinity, Infinity, 0}],
If[x1A==x2A && x1B!=x2B,
mB=((y1B-y2B)/(x1B-x2B));
bB=y1B-mB*x1B;
coor={X=x1A, mB*X+bB,0},
If[x1A!=x2A && x1B==x2B,
mA=((y1A-y2A)/(x1A-x2A));
bA=y1A-mA*x1A;
coor={X=x1B, mA*X+bA,0},
coor={X=Infinity, Infinity, 0}]]];
If[(X>x1A &&X<=x2A && X>=x1B &&X<=x2B)||(X>x1A &&X<=x2A && X<=x1B &&X>=x2B)||
(X<x1A &&X>=x2A && X<=x1B &&X>=x2B)||(X<x1A &&X>=x2A && X>=x1B &&X<=x2B), coor, False]]


getNewPoly[cuttingPts_,cuttingEdges_, polyPts_, polyEdges_]:=
Module[{zPlanePoints, pointsInPolyQ, newPts,twoDZPlanePts, ptsAre},
zPlanePoints=DetermineCut[cuttingPts,cuttingEdges];
If[zPlanePoints=={} || Length[zPlanePoints]==1,{polyPts,{}},
pointsInPolyQ=DetermineIfPointInPoly[#,polyPts,polyEdges]&/@zPlanePoints;
ptsAre=getNewPts2[cuttingPts,cuttingEdges, polyPts, polyEdges];
If[Length[ptsAre]>2, ptsAre=ptsAre[[1;;2]]];
If[Length[ptsAre]<2,{polyPts,{}},
createNewPolys2[polyPts,polyEdges,ptsAre]]]
]


createNewPolys2[polyPts_,polyEdges_,ptsAre_]:=
Module[{newPts,cutEdges, poly1, poly2,whichPoint,currentEdge,i, whichPoly},
newPts=#[[1]]&/@ptsAre;
cutEdges=#[[2]]&/@ptsAre;
poly1={};
poly2={};
whichPoint=1;
whichPoly=1;
poly1=Append[poly1,polyPts[[whichPoint]]];
For[i=1,i<4,i++;
(*There is no i used in the for loop but we simply increment whichPoint
by taking the second point of the current edge and then taking then next time
taking the edge that begins with that point!*)
currentEdge=Flatten[Select[polyEdges,#[[1]]==whichPoint&]];
whichPoint=currentEdge[[2]];
If[MemberQ[cutEdges,currentEdge],
(*If the current edge that we are considering is a cut edge*)
If[whichPoly==1,
(*If we are currently focused on poly1*)
{whichPoly=2,
poly1=Append[poly1,newPts[[First[Flatten[Position[cutEdges,currentEdge]]]]]],
poly2=Append[poly2,newPts[[First[Flatten[Position[cutEdges,currentEdge]]]]]],
whichPoint=currentEdge[[2]],
poly2=If[polyPts[[whichPoint]]!= newPts[[First[Flatten[Position[cutEdges,currentEdge]]]]],
Append[poly2,polyPts[[whichPoint]]],poly2]},
{whichPoly=1,
poly2=Append[poly2,newPts[[First[Flatten[Position[cutEdges,currentEdge]]]]]],
poly1=Append[poly1,newPts[[First[Flatten[Position[cutEdges,currentEdge]]]]]],
whichPoint=currentEdge[[2]],
poly1=If[polyPts[[whichPoint]]!= newPts[[First[Flatten[Position[cutEdges,currentEdge]]]]],
Append[poly1,polyPts[[whichPoint]]],poly1]}],
(*If the current edge is not a cut edge*)
If[whichPoly==1,
poly1=Append[poly1,polyPts[[whichPoint]]],
poly2=Append[poly2,polyPts[[whichPoint]]]]]];
{poly1,poly2}
]


getNewPts2[cuttingPts_,cuttingEdges_, polyPts_, polyEdges_]:=
Module[{zPlanePoints, pointsInPolyQ, newPts,twoDZPlanePts, ptsAre, pts, newPtLine,check},
zPlanePoints=DetermineCut[cuttingPts,cuttingEdges];
pointsInPolyQ=DetermineIfPointInPoly[#,polyPts,polyEdges]&/@zPlanePoints;
ptsAre=If[MemberQ[pointsInPolyQ,"Point in Poly"],
(*Why is there a difference between if one of the points
are in a poly and neither of the points are in a poly??*)
Select[{DetermineIntersect2[{polyPts[[#]], zPlanePoints}],#}&/@polyEdges,#[[1]]=!=False&],
Select[{DetermineIntersect4[{polyPts[[#]], zPlanePoints}],#}&/@polyEdges,#[[1]]=!=False&]];
pts=#[[1]]&/@ptsAre;
newPtLine=Chop[EquationOfLine2[pts]];
(* What does this catch? 
I think this is catching when a new cut line is actually and edge!*)
If[MemberQ[Chop[EquationOfLine2[polyPts[[#]]]-newPtLine]&/@polyEdges,{0,0}]==False,ptsAre,{}]
]


(* ::Subsection:: *)
(*Main Cut Polygon Function*)


dubugPolys[{polyPoints_, polyNormal_, polyEdges_,inout_},
{cuttingPoints_,cuttingNormal_, cuttingEdges_}]:=
Module[{Q, polyBase, zSubtract,cuttingBase,newPolyPoints,newPolyNormal,polyMean, cuttingMean,
newPolyMean,newCuttingPoints, newCuttingNormal, newCuttingMean,rotateBack,newPoly1, newPoly2,
finalPoly1, finalPoly2, finalEdges, p1, p2, L1, L2,W},

polyBase=N[makeBaseFromPoly[polyPoints]];
cuttingBase=N[makeBaseFromPoly[cuttingPoints]];
polyMean=Mean[polyPoints];
cuttingMean=Mean[cuttingPoints];

zSubtract={0,0,Mean[#[[3]]&/@((polyBase.#)&/@polyPoints)]};

newPolyPoints=(#-zSubtract)&/@((polyBase.#)&/@polyPoints);
newPolyNormal=Chop[polyBase.polyNormal];
newPolyMean=Chop[polyBase.polyMean-zSubtract];

newCuttingPoints=(#-zSubtract)&/@((polyBase.#)&/@cuttingPoints);
newCuttingNormal=Chop[polyBase.cuttingNormal];
newCuttingMean=Chop[polyBase.cuttingMean-zSubtract];

rotateBack=Inverse[polyBase];

finalEdges={{1,2},{2,3},{3,1}};

(*There are a few instances to worry about, 1) if the points dont even straddle the plane and two if they straddle but don't cut!*)

{DetermineCut3[newCuttingPoints,cuttingEdges], "*****************************************",
DetermineCut[newCuttingPoints,cuttingEdges]}
]


cutPolysTest[{polyPoints_, polyNormal_, polyEdges_,inout_},
{cuttingPoints_,cuttingNormal_, cuttingEdges_}]:=
Module[{Q, polyBase, zSubtract,cuttingBase,newPolyPoints,newPolyNormal,polyMean, cuttingMean,
newPolyMean,newCuttingPoints, newCuttingNormal, newCuttingMean,rotateBack,newPoly1, newPoly2,
finalPoly1, finalPoly2, finalEdges, p1, p2, L1, L2,W},

polyBase=N[makeBaseFromPoly[polyPoints]];
cuttingBase=N[makeBaseFromPoly[cuttingPoints]];
polyMean=Mean[polyPoints];
cuttingMean=Mean[cuttingPoints];

zSubtract={0,0,Mean[#[[3]]&/@((polyBase.#)&/@polyPoints)]};

newPolyPoints=Chop[(#-zSubtract)&/@((polyBase.#)&/@polyPoints)];
newPolyNormal=Chop[polyBase.polyNormal];
newPolyMean=Chop[polyBase.polyMean-zSubtract];

newCuttingPoints=Chop[(#-zSubtract)&/@((polyBase.#)&/@cuttingPoints)];
newCuttingNormal=Chop[polyBase.cuttingNormal];
newCuttingMean=Chop[polyBase.cuttingMean-zSubtract];

rotateBack=Inverse[polyBase];
finalEdges={{1,2},{2,3},{3,1}};

{newPoly1, newPoly2}=Chop[getNewPoly[newCuttingPoints,cuttingEdges, newPolyPoints, polyEdges]];

]


cutPolys[{polyPoints_, polyNormal_, polyEdges_,inout_},
{cuttingPoints_,cuttingNormal_, cuttingEdges_}]:=
Module[{Q, polyBase, zSubtract,cuttingBase,newPolyPoints,newPolyNormal,polyMean, cuttingMean,
newPolyMean,newCuttingPoints, newCuttingNormal, newCuttingMean,rotateBack,newPoly1, newPoly2,
finalPoly1, finalPoly2, finalEdges, p1, p2, L1, L2,W},

polyBase=N[makeBaseFromPoly[polyPoints]];
cuttingBase=N[makeBaseFromPoly[cuttingPoints]];
polyMean=Mean[polyPoints];
cuttingMean=Mean[cuttingPoints];

zSubtract={0,0,Mean[#[[3]]&/@((polyBase.#)&/@polyPoints)]};

newPolyPoints=Chop[(#-zSubtract)&/@((polyBase.#)&/@polyPoints)];
newPolyNormal=Chop[polyBase.polyNormal];
newPolyMean=Chop[polyBase.polyMean-zSubtract];

newCuttingPoints=Chop[(#-zSubtract)&/@((polyBase.#)&/@cuttingPoints)];
newCuttingNormal=Chop[polyBase.cuttingNormal];
newCuttingMean=Chop[polyBase.cuttingMean-zSubtract];

rotateBack=Inverse[polyBase];

finalEdges={{1,2},{2,3},{3,1}};

(*There are a few instances to worry about, 1) if the points dont even straddle the plane and two if they straddle but don't cut!*)

{newPoly1, newPoly2}=Chop[getNewPoly[newCuttingPoints,cuttingEdges, newPolyPoints, polyEdges]];

If[Length[newPoly2]==2, newPoly2 = {}];

newPoly2=If[newPoly2=={} ,{},If[Chop[newPoly2[[1]]-newPoly2[[2]]] =={0,0,0}
|| Chop[newPoly2[[1]]-newPoly2[[3]]] =={0,0,0} 
|| Chop[newPoly2[[2]]-newPoly2[[3]]] =={0,0,0},{}, newPoly2]];

newPoly1=If[Chop[newPoly1[[1]]-newPoly1[[2]]] =={0,0,0}
|| Chop[newPoly1[[1]]-newPoly1[[3]]] =={0,0,0} 
|| Chop[newPoly1[[2]]-newPoly1[[3]]] =={0,0,0},newPoly2, newPoly1];

If[newPoly1==newPoly2, newPoly2={}];

W=If[newPoly2=={},

{{polyPoints,polyNormal, polyEdges,inout}},

finalPoly1=removeDuplicatePoint[((rotateBack.#)&/@((#+zSubtract)&/@newPoly1))];
finalPoly2=removeDuplicatePoint[((rotateBack.#)&/@((#+zSubtract)&/@newPoly2))];

p1=Mean[finalPoly1]-cuttingMean;
p2=Mean[finalPoly2]-cuttingMean;

L1=Length[finalPoly1];
L2=Length[finalPoly2];

{
If[L1==3,
{finalPoly1, polyNormal, finalEdges, inout},
{{triangulateQuad[finalPoly1][[1]], polyNormal, finalEdges, inout},
{triangulateQuad[finalPoly1][[2]], polyNormal, finalEdges, inout}}

],

If[L2==3,
{finalPoly2, polyNormal, finalEdges,  inout},
{{triangulateQuad[finalPoly2][[1]], polyNormal, finalEdges, inout},
{triangulateQuad[finalPoly2][[2]], polyNormal, finalEdges,  inout}}]

}];

Q=If[Length[W]==2,If[Length[W[[1]]]==2 || Length[W[[2]]]==2, FlattenAt[W,First[Position[Length[#]&/@W,2]]],W],W];
Select[shardCheck[#]&/@Q,#!={}&]
]


(* ::Subsection:: *)
(*Clean up*)


shardCheck[newPoly2_]:=If[Chop[newPoly2[[1]][[1]]-newPoly2[[1]][[2]]] =={0,0,0}
|| Chop[newPoly2[[1]][[1]]-newPoly2[[1]][[3]]] =={0,0,0} 
|| Chop[newPoly2[[1]][[2]]-newPoly2[[1]][[3]]] =={0,0,0},{}, newPoly2]


removeDuplicatePoint[pts_]:=If[Last[pts]==First[pts],Most[pts],pts];


triangulateQuad[pts_]:=Module[{d1,d2},
d1=N[Sqrt[Total[(#^2)&/@(pts[[1]]-pts[[3]])]]];
d2=N[Sqrt[Total[(#^2)&/@(pts[[2]]-pts[[4]])]]];
If[d1>d2,{{pts[[1]],pts[[2]],pts[[4]]},{pts[[2]],pts[[3]],pts[[4]]}},{{pts[[1]],pts[[2]],pts[[3]]},{pts[[3]],pts[[1]],pts[[4]]}}]];


(* ::Subsection:: *)
(*Perform Operation*)


performOperationNot[a_,b_]:=Module[{i,aL, bL,aI, bI, newA, aNew, inners, realInner, realOuters,verif},
aL=Length[a];
bL=Length[b];
aNew={};
For[aI=1,aI<=aL,
newA={a[[aI]]};
For[bI=1, bI<=bL, 
(*Print[newA];*)
newA=Flatten[cutPolys[#,Most[ b[[bI]]]]&/@newA,1];
bI++];
aNew=Append[aNew,newA];
aI++];
aNew=Flatten[aNew,1];

inners=aNew;
realOuters={};
realInner={};
For[i=0,i<Length[inners],i++;
(* I want this to work!!*)

verif=determinePolyInOutNot[inners[[i]][[1]],b];

If[verif=="Outside",realOuters=Append[realOuters,inners[[i]]],realInner=Append[realInner,inners[[i]]]]];

{realInner,
realOuters}]


(* ::Subsection:: *)
(*Boolean Operations*)


booNot[a_,b_]:=Module[{aIn, aOut, bIn, bOut, notOne, notTwo},
{aIn, aOut}=performOperationNot[a, b];
{bIn, bOut}=performOperationNot[b, a];
notOne=Join[aIn,bOut];
notTwo=Join[aOut,bIn];
{notOne,notTwo}
(*{aIn, aOut, bIn, bOut}*)]


booOr[a_,b_]:=Module[{aIn, aOut, bIn, bOut, outOne},
{aIn, aOut}=performOperationNot[a, b];
{bIn, bOut}=performOperationNot[b, a];
outOne=Join[aOut,bOut]
]


booAnd[a_,b_]:=Module[{aIn, aOut, bIn, bOut, andOne},
{aIn, aOut}=performOperationNot[a, b];
{bIn, bOut}=performOperationNot[b, a];
andOne=Join[aIn,bIn];
]


(* ::Subsection:: *)
(*Polygon in Polyhedra*)


determineStraddle[point_, poly_]:=Module[{x,y,xmax,xmin,ymax,ymin},
xmax=Max[#[[1]]&/@poly];
xmin=Min[#[[1]]&/@poly];
ymax=Max[#[[2]]&/@poly];
ymin=Min[#[[2]]&/@poly];
x=point[[1]];
y=point[[2]];
If[x >= xmin && x <= xmax && y >= ymin && y <=ymax, "True", "False"]]


EquationOfPlane[{pt1_, pt2_, pt3_}]:=Module[{z,one,two,n, C},
one=pt1-pt2;
two=pt3-pt2;
n=Cross[one,two];
C=n.pt1;
z=pt1[[3]];
{n,C,z}]


FindZ[x_,y_,n_,C_,z_]:=If[n[[3]]==0,z,(C-n[[1]]*x-n[[2]]*y)/n[[3]]]


determinePolyInOutNot[poly_,hedra_]:=Module[{actualZ, eopS, up, down, point, zPointsOfHedra, zPoint,aboveBelow, belowAbove, newHedra ,belowFrac, direction, considerPolysA, considerPolysB, considerPolysC, whichPolys, determineWhichPolys, numberInteract, a},
If[coplanarQNew[poly, First[#]&/@hedra],"Inside",
point=Mean[poly];
zPointsOfHedra=(Last[#]&/@#)&/@(#[[1]]&/@hedra);
zPoint=Last[point];
aboveBelow=MemberQ[Positive[zPoint-#]&/@#,False]&/@zPointsOfHedra;
belowAbove=MemberQ[Negative[zPoint-#]&/@#,False]&/@zPointsOfHedra;
up=Length[Select[aboveBelow,#==True &]]/Length[aboveBelow];
down=Length[Select[belowAbove,#==True &]]/Length[belowAbove];
direction=If[up>down, "Down", "Up"];
considerPolysA=If[direction=="Down", 

Select[Flatten[#,1]&/@Partition[Riffle[hedra,belowAbove],2],  Last[#]==True &],
Select[Flatten[#,1]&/@Partition[Riffle[hedra,aboveBelow],2],  Last[#]==True &]];

eopS=EquationOfPlane[First[#]]&/@considerPolysA;

actualZ=FindZ[point[[1]],point[[2]],#[[1]],#[[2]], #[[3]]]&/@eopS;

considerPolysB=If[direction=="Up",
Select[Flatten[#,1]&/@Partition[Riffle[considerPolysA,actualZ],2], Last[#]>zPoint &],
Select[Flatten[#,1]&/@Partition[Riffle[considerPolysA,actualZ],2], Last[#]<zPoint &]];
(*From those polygons, select those that are cearly Inside! *)
 (* From those polys, select those that straddle the ray *)
considerPolysC=determineStraddle[point, First[#]]&/@considerPolysB;
whichPolys=Position[considerPolysC,"True"];
determineWhichPolys=If[Length[considerPolysC]>0,
a=(#&/@#)&/@(First[First[#]]&/@(considerPolysB[[#]]&/@whichPolys ));
DetermineIfPointInPoly[point ,#,{{1,2},{2,3},{3,1}}]&/@a,{}];
numberInteract=Count[determineWhichPolys,"Point in Poly"];
If[OddQ[numberInteract], "Inside", "Outside"]]]


coplanarQNew[poly_, hedra_]:=Module[{eP, i,polyE, l,hedriE, pval, hval,cP, A={}},
polyE=EquationOfPlane[poly];

eP=If[polyE[[2]] == 0, {0,0,0} , N[polyE[[1]]/polyE[[2]]]];
pval={eP,True};
l=Length[hedra];
For[i=1, i<= l, 
hedriE=EquationOfPlane[hedra[[i]]];
(*This is subtle, you are checking if either one has their mean inside the other*)
cP=Or[meanInPolygon[poly,hedra[[i]]], meanInPolygon[hedra[[i]],poly]];
hval = If [N[hedriE[[2]]] == 0, {0,0,0}, N[hedriE[[1]]/hedriE[[2]]]];
A=Append[A,{Chop[hval-eP], cP}];
i++;
];
 MemberQ[A,{{0,0,0},True}]]


meanInPolygon[polyPoints_,cuttingPoints_]:=
Module[{Q, polyBase, zSubtract,cuttingBase,newPolyPoints,newPolyNormal,polyMean, cuttingMean,
newPolyMean,newCuttingPoints, newCuttingNormal, newCuttingMean,rotateBack,newPoly1, newPoly2,
finalPoly1, finalPoly2, finalEdges, p1, p2, L1, L2,W, pointInPolyQ},

polyBase=N[makeBaseFromPoly[polyPoints]];
polyMean=Mean[polyPoints];
cuttingMean=Mean[cuttingPoints];

zSubtract={0,0,Mean[#[[3]]&/@((polyBase.#)&/@polyPoints)]};

newPolyPoints=(#-zSubtract)&/@((polyBase.#)&/@polyPoints);
newCuttingMean=Chop[polyBase.cuttingMean-zSubtract];
finalEdges={{1,2},{2,3},{3,1}};
pointInPolyQ=DetermineIfPointInPoly[newCuttingMean,newPolyPoints,finalEdges];
If[pointInPolyQ=="Point in Poly", True, False]
]


(* ::Subsection::Closed:: *)
(*Combine Polys*)


sharePlaneQ[polyA_, hedraB_]:=Module[{i,polyE, l,hedriE, pval, hval,cP, A={}},
polyE=EquationOfPlane[polyA];
pval=N[polyE[[1]]*polyE[[2]]];
l=Length[hedraB];
For[i=1, i<= l, 
hedriE=EquationOfPlane[hedraB];
hval=N[hedriE[[1]]*hedriE[[2]]];
A=Append[A,hval];
i++;
];
 MemberQ[A,pval]]


combinePolys[polyA_, polyB_]:=Module[{edge,edgesA, edgesB, position, sharedEdge, otherPointA, otherPointB, 
lineA1, lineA2, lineB1,lineB2, MA1, MA2, MB1, MB2},
edge={{1,2},{2,3},{3,1}};
edgesA={polyA[[First[#]]],polyA[[Last[#]]]}&/@edge;
(* We need to do "both ways" because we don't know the "handedness" (the direction) that the edges traverse the triangle*)
edgesB=Flatten[{{polyB[[First[#]]],polyB[[Last[#]]]},{polyB[[Last[#]]],polyB[[First[#]]]}}&/@edge,1];
position=Position[MemberQ[edgesB,#]&/@edgesA,True];
If[position!={},
sharedEdge=edgesA[[First[First[position]]]];
otherPointA=Flatten[Complement[polyA,sharedEdge]];
otherPointB=Flatten[Complement[polyB,sharedEdge]];
lineA1={otherPointA, sharedEdge[[1]]};
lineA2={otherPointA, sharedEdge[[2]]};
lineB1={otherPointB, sharedEdge[[1]]};
lineB2={otherPointB, sharedEdge[[2]]};
MA1=lineA1[[1]]-lineA1[[2]];
MA2=lineA2[[1]]-lineA2[[2]];
MB1=lineB1[[1]]-lineB1[[2]];
MB2=lineB2[[1]]-lineB2[[2]];
If[TrueQ[MA1==MB1 || MA1 == - MB1],{otherPointA, otherPointB, sharedEdge[[2]]}, 
If[TrueQ[MA2==MB2 || MA2 == - MB2], {otherPointA, otherPointB, sharedEdge[[1]]}, "none"]], "none"]]
