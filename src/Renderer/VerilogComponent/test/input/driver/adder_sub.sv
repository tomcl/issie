module top_module;
  bit [0:0] oper;
  bit [0:0] oper_array [15:0];
  bit [7:0] a;
  bit [7:0] a_array [15:0];
  bit [7:0] b;
  bit [7:0] b_array [15:0];
  bit [7:0] res;
  bit [7:0] res_array [15:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(36) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"res\", \"Values\": [");
    for(i_=0;i_<15; i_=i_+1) begin $write("%d, ", res_array[i_]); end
    $display("%d]}", res_array[15]);
    $write("]");
    $finish(0);
  end
  initial begin
      oper_array[0] = 1'd0;
      oper_array[1] = 1'd1;
      oper_array[2] = 1'd1;
      oper_array[3] = 1'd0;
      oper_array[4] = 1'd0;
      oper_array[5] = 1'd1;
      oper_array[6] = 1'd0;
      oper_array[7] = 1'd1;
      oper_array[8] = 1'd0;
      oper_array[9] = 1'd1;
      oper_array[10] = 1'd0;
      oper_array[11] = 1'd1;
      oper_array[12] = 1'd0;
      oper_array[13] = 1'd1;
      oper_array[14] = 1'd0;
      oper_array[15] = 1'd1;
      a_array[0] = 8'd1;
      a_array[1] = 8'd2;
      a_array[2] = 8'd4;
      a_array[3] = 8'd5;
      a_array[4] = 8'd3;
      a_array[5] = 8'd12;
      a_array[6] = 8'd15;
      a_array[7] = 8'd50;
      a_array[8] = 8'd45;
      a_array[9] = 8'd120;
      a_array[10] = 8'd80;
      a_array[11] = 8'd85;
      a_array[12] = 8'd91;
      a_array[13] = 8'd95;
      a_array[14] = 8'd102;
      a_array[15] = 8'd107;
      b_array[0] = 8'd0;
      b_array[1] = 8'd1;
      b_array[2] = 8'd123;
      b_array[3] = 8'd124;
      b_array[4] = 8'd127;
      b_array[5] = 8'd100;
      b_array[6] = 8'd40;
      b_array[7] = 8'd40;
      b_array[8] = 8'd10;
      b_array[9] = 8'd13;
      b_array[10] = 8'd25;
      b_array[11] = 8'd37;
      b_array[12] = 8'd72;
      b_array[13] = 8'd10;
      b_array[14] = 8'd13;
      b_array[15] = 8'd1;
    for(j_=0; j_<16; j_=j_+1) begin
        oper=oper_array[j_];
        a=a_array[j_];
        b=b_array[j_];
      #0.5;
        res_array[j_]=res;
      @(negedge clk);
end
  end
  addsub dut (.oper(oper), .a(a), .b(b), .res(res));
endmodule