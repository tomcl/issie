module jripplecarryadder(Y,carryout,A,B,carryin);
  output bit [3:0] Y;
  output bit carryout;
  input bit [3:0]A,B;
  input bit carryin;
  
  bit c1,c2,c3;
  
  jfulladder jfa0(.y(Y[0]),.carryout(c1),.a(A[0]),.b(B[0]),.carryin(carryin));
  jfulladder jfa1(.y(Y[1]),.carryout(c2),.a(A[1]),.b(B[1]),.carryin(c1));
  jfulladder jfa2(.y(Y[2]),.carryout(c3),.a(A[2]),.b(B[2]),.carryin(c2));
  jfulladder jfa3(.y(Y[3]),.carryout(carryout),.a(A[3]),.b(B[3]),.carryin(c3));

endmodule