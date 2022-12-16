(* ::Package:: *)

(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, epimutation, backepimut,avgfitness, results,jacobian, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
dp1:=Simplify[Divide[(q1*(q1*w11+q3*w13+q2*w12+w14*(1-q1-q2-q3))*(1-t1-t2)+t3*q3*(q3*w33+q1*w13+q2*w23+w34*(1-q1-q2-q3))),
((q1^2)*w11+(q2^2)*w22+(q3^2)*w33+((1-q1-q2-q3)^2)*w44+
2*q1*q3*w13+2*q1*q2*w12+2*q1*(1-q1-q2-q3)*w14+2*q3*q2*w23+2*q3*(1-q1-q2-q3)*w34+
2*q2*(1-q1-q2-q3)*w24)]-q1]


(* ::Input::Initialization:: *)
dp2:=
Simplify[
Divide[(q2*(q1*w12+q3*w23+q2*w22+w24*(1-q1-q2-q3))*(1-t2)+q1*t1*(q1*w11+q3*w13+q2*w12+w14*(1-q1-q2-q3))+t3*(1-q1-q2-q3)*(w44*(1-q1-q2-q3)+q1*w14+q2*w24+q3*w34)),((q1^2)*w11+(q2^2)*w22+(q3^2)*w33+((1-q1-q2-q3)^2)*w44+
2*q1*q3*w13+2*q1*q2*w12+2*q1*(1-q1-q2-q3)*w14+2*q3*q2*w23+2*q3*(1-q1-q2-q3)*w34+
2*q2*(1-q1-q2-q3)*w24)]-q2]


(* ::Input::Initialization:: *)
dp3:=
Simplify[
Divide[q3*(q3*w33+q1*w13+q2*w23+w34*(1-q1-q2-q3))*(1-t1-t3)+t2*(q1*(q1*w11+q3*w13+q2*w12+w14*(1-q1-q2-q3))),((q1^2)*w11+(q2^2)*w22+(q3^2)*w33+((1-q1-q2-q3)^2)*w44+
2*q1*q3*w13+2*q1*q2*w12+2*q1*(1-q1-q2-q3)*w14+2*q3*q2*w23+2*q3*(1-q1-q2-q3)*w34+
2*q2*(1-q1-q2-q3)*w24)]-q3]



(* ::Input::Initialization:: *)
jacobian={{D[dp1,q1],D[dp1,q2],D[dp1,q3]},{D[dp2,q1],D[dp2,q2],D[dp2,q3]},{D[dp3,q1],D[dp3,q2],D[dp3,q3]}}


(* ::Input::Initialization:: *)
avgfitness:=
((p1^2)*w11+(p2^2)*w22+(p3^2)*w33+(p4^2)*w44+
2*p1*p3*w13+2*p1*p2*w12+2*p1*p4*w14+2*p3*p2*w23+2*p3*p4*w34+
2*p2*p4*w24)


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
backepimut={0.01,0.1,0.33}


(* ::Input::Initialization:: *)
epimutation=E^RandomVariate[UniformDistribution[{Log[0.00001],Log[0.5]}],300]



(* ::Input::Initialization:: *)
finaltable={};


(* ::Subsubsection::Closed:: *)
(*Scenario 1a - symmetric*)


(* ::Input::Initialization:: *)
w11:=0.99
w22:=0.99
w33:=1
w44:=0.99
w12:=0.99
w13:=0.99
w14:=0.99
w23:=0.99
w24:=0.99
w34:=0.99
scenario:="1a-symmetric"


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 1b - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.98
w22:=0.98
w33:=1
w44:=0.99
w12:=0.98
w13:=0.98
w14:=0.98
w23:=0.98
w24:=0.98
w34:=0.99
scenario:="1b-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 0*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=1
w22:=1
w33:=1
w44:=1
w12:=1
w13:=1
w14:=1
w23:=1
w24:=1
w34:=1
scenario:="0"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 1c - deleterious, symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=1
w22:=1
w33:=0.99
w44:=1
w12:=1
w13:=1
w14:=1
w23:=1
w24:=1
w34:=1
scenario:="1c-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 2a - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.99
w22:=0.99
w33:=1
w44:=0.99
w12:=0.99
w13:=1
w14:=0.99
w23:=1
w24:=0.99
w34:=1
scenario:="2a-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 2b - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.99
w22:=0.98
w33:=1
w44:=0.99
w12:=0.99
w13:=1
w14:=0.99
w23:=1
w24:=0.99
w34:=1
scenario:="2b-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 3a - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.99
w22:=1
w33:=1
w44:=1
w12:=1
w13:=1
w14:=1
w23:=1
w24:=1
w34:=1
scenario:="3a-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 3b - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.98
w22:=0.99
w33:=1
w44:=1
w12:=0.99
w13:=1
w14:=1
w23:=1
w24:=1
w34:=1
scenario:="3b-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 3c - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.98
w22:=0.99
w33:=1
w44:=0.99
w12:=0.99
w13:=1
w14:=0.99
w23:=1
w24:=0.99
w34:=1
scenario:="3c-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 4a - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.99
w22:=1
w33:=1
w44:=1
w12:=0.99
w13:=0.99
w14:=0.99
w23:=1
w24:=1
w34:=1
scenario:="4a-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 4b - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.98
w22:=0.99
w33:=1
w44:=1
w12:=0.98
w13:=0.98
w14:=0.98
w23:=0.99
w24:=0.99
w34:=1
scenario:="4b-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 4c - symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.98
w22:=0.99
w33:=1
w44:=0.99
w12:=0.98
w13:=0.98
w14:=0.98
w23:=0.99
w24:=0.99
w34:=0.99
scenario:="4c-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 2c - deleterious*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=1
w22:=1
w33:=1
w44:=0.99
w12:=1
w13:=1
w14:=0.99
w23:=1
w24:=0.99
w34:=0.99
scenario:="2c-deleterious"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 2c - deleterious, symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=1
w22:=1
w33:=0.99
w44:=1
w12:=1
w13:=0.99
w14:=1
w23:=0.99
w24:=1
w34:=0.99
scenario:="2c-deleterious-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 3d - deleterious*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.99
w22:=1
w33:=0.99
w44:=0.98
w12:=0.99
w13:=0.99
w14:=0.98
w23:=0.99
w24:=0.98
w34:=0.98
scenario:="3d-deleterious"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 3d - deleterious, symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=1
w22:=0.99
w33:=0.98
w44:=0.99
w12:=0.99
w13:=0.98
w14:=0.99
w23:=0.98
w24:=0.99
w34:=0.98
scenario:="3d-deleterious-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 4d- deleterious*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=0.99
w22:=1
w33:=0.99
w44:=0.98
w12:=1
w13:=0.99
w14:=0.99
w23:=1
w24:=1
w34:=0.99
scenario:="4d-deleterious"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Subsubsection::Closed:: *)
(*Scenario 4d- deleterious, symmetric*)


(* ::Input::Initialization:: *)
Clear[p1, p2, p3, p4, q1, q2, q3, w11, w22, w33, w44, w12, w13, w14, w23, w24, w34, t,t1, t2, t3, sumofp, results, jacobian1, jacobian2, jacobian3, detjacobian1, detjacobian2, detjacobian3,scenario]


(* ::Input::Initialization:: *)
w11:=1
w22:=0.99
w33:=0.98
w44:=0.99
w12:=1
w13:=1
w14:=1
w23:=0.99
w24:=0.99
w34:=0.99
scenario:="4d-deleterious-symmetric"


(* ::Input::Initialization:: *)
p4:=(1-(p1+p2+p3))


(* ::Input::Initialization:: *)
For[q=1,q<4,q++,
For[i=1,i<301,i++,
t1:=0.0000001;
t2:=Part[epimutation,i];
t3:=Part[backepimut,q];
results:=NSolve[{p1==Chop[Divide[(p1*(p1*w11+p3*w13+p2*w12+w14*p4)*(1-t1-t2)+t3*p3*(p3*w33+p1*w13+p2*w23+w34*p4)),
avgfitness]],p2==Chop[Divide[(p2*(p1*w12+p3*w23+p2*w22+w24*p4)*(1-t2)+p1*t1*(p1*w11+p3*w13+p2*w12+w14*p4)+t3*p4*(w44*p4+p1*w14+p2*w24+p3*w34)),avgfitness]],
p3==Chop[Divide[p3*(p3*w33+p1*w13+p2*w23+w34*p4)*(1-t1-t3)+t2*(p1*(p1*w11+p3*w13+p2*w12+w14*p4)),avgfitness]],0<= p1<= 1,0<= p2 <= 1,0<= p3<= 1,p1+p2+p3 <=1},{p1,p2,p3},WorkingPrecision->10];
listp1=p1/.results;listp2=p2/.results; listp3 = p3/.results;
equil1={Part[listp1,1],Part[listp2,1],Part[listp3,1]};
q1:=Part[equil1,1]; q2:=Part[equil1,2]; q3:=Part[equil1,3];jacobian1=Eigenvalues[jacobian];detjacobian1=Det[jacobian];
If[FromDigits[Dimensions[results,1]]>1,
	equil2={Part[listp1,2],Part[listp2,2],Part[listp3,2]};
	q1:=Part[equil2,1]; q2:=Part[equil2,2]; q3:=Part[equil2,3];
	 jacobian2=Eigenvalues[jacobian];detjacobian2=Det[jacobian];
	If[FromDigits[Dimensions[results,1]]>2,
equil3={Part[listp1,3],Part[listp2,3],Part[listp3,3]};
	q1:=Part[equil3,1]; q2:=Part[equil3,2]; q3:=Part[equil3,3];
	 jacobian3=Eigenvalues[jacobian];detjacobian3=Det[jacobian];AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2,equil3,jacobian3,detjacobian3}],AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1,equil2,jacobian2,detjacobian2}]]
	,
AppendTo[finaltable,{scenario,t1,t2,t3,w11,w22,w33,w44,w12,w13,w14,w23,w24,w34,FromDigits[Dimensions[results,1]],equil1,jacobian1,detjacobian1}]]]]



(* ::Input::Initialization:: *)
finaltable


(* ::Input::Initialization:: *)
Export["model_FocalBackEpi_AllScenarios_Symmetric.txt",finaltable,"Table"]
