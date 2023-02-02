module top_module;
  bit [3:0] A;
  bit [3:0] A_array [5:0];
  bit [3:0] B;
  bit [3:0] B_array [5:0];
  bit [0:0] AEQB;
  bit [0:0] AEQB_array [5:0];
  bit [0:0] AGTB;
  bit [0:0] AGTB_array [5:0];
  bit [0:0] ALTB;
  bit [0:0] ALTB_array [5:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(16) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"AEQB\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", AEQB_array[i_]); end
    $display("%d]}, ", AEQB_array[5]);
    $write("{\"Label\": \"AGTB\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", AGTB_array[i_]); end
    $display("%d]}, ", AGTB_array[5]);
    $write("{\"Label\": \"ALTB\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", ALTB_array[i_]); end
    $display("%d]}", ALTB_array[5]);
    $write("]");
    $finish(0);
  end
  initial begin
      A_array[0] = 4'd7;
      A_array[1] = 4'd2;
      A_array[2] = 4'd3;
      A_array[3] = 4'd1;
      A_array[4] = 4'd0;
      A_array[5] = 4'd1;
      B_array[0] = 4'd0;
      B_array[1] = 4'd1;
      B_array[2] = 4'd3;
      B_array[3] = 4'd5;
      B_array[4] = 4'd4;
      B_array[5] = 4'd1;
    for(j_=0; j_<6; j_=j_+1) begin
        A=A_array[j_];
        B=B_array[j_];
      #0.5;
        AEQB_array[j_]=AEQB;
        AGTB_array[j_]=AGTB;
        ALTB_array[j_]=ALTB;
      @(negedge clk);
end
  end
  jmagnitudeComparator dut (.A(A), .B(B), .AEQB(AEQB), .AGTB(AGTB), .ALTB(ALTB));
endmodule