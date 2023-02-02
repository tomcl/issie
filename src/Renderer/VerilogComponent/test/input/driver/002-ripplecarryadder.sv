module top_module;
  bit [3:0] A;
  bit [3:0] A_array [9:0];
  bit [3:0] B;
  bit [3:0] B_array [9:0];
  bit [0:0] carryin;
  bit [0:0] carryin_array [9:0];
  bit [3:0] Y;
  bit [3:0] Y_array [9:0];
  bit [0:0] carryout;
  bit [0:0] carryout_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"Y\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", Y_array[i_]); end
    $display("%d]}, ", Y_array[9]);
    $write("{\"Label\": \"carryout\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", carryout_array[i_]); end
    $display("%d]}", carryout_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      A_array[0] = 4'd0;
      A_array[1] = 4'd12;
      A_array[2] = 4'd14;
      A_array[3] = 4'd9;
      A_array[4] = 4'd7;
      A_array[5] = 4'd5;
      A_array[6] = 4'd10;
      A_array[7] = 4'd11;
      A_array[8] = 4'd3;
      A_array[9] = 4'd4;
      B_array[0] = 4'd1;
      B_array[1] = 4'd0;
      B_array[2] = 4'd10;
      B_array[3] = 4'd3;
      B_array[4] = 4'd4;
      B_array[5] = 4'd2;
      B_array[6] = 4'd10;
      B_array[7] = 4'd15;
      B_array[8] = 4'd2;
      B_array[9] = 4'd4;
      carryin_array[0] = 1'd0;
      carryin_array[1] = 1'd0;
      carryin_array[2] = 1'd0;
      carryin_array[3] = 1'd1;
      carryin_array[4] = 1'd1;
      carryin_array[5] = 1'd1;
      carryin_array[6] = 1'd0;
      carryin_array[7] = 1'd1;
      carryin_array[8] = 1'd0;
      carryin_array[9] = 1'd1;
    for(j_=0; j_<10; j_=j_+1) begin
        A=A_array[j_];
        B=B_array[j_];
        carryin=carryin_array[j_];
      #0.5;
        Y_array[j_]=Y;
        carryout_array[j_]=carryout;
      @(negedge clk);
end
  end
  jripplecarryadder dut (.A(A), .B(B), .carryin(carryin), .Y(Y), .carryout(carryout));
endmodule