module top_module;
  bit [7:0] in_a;
  bit [7:0] in_a_array [7:0];
  bit [7:0] in_b;
  bit [7:0] in_b_array [7:0];
  bit [0:0] load;
  bit [0:0] load_array [7:0];
  bit [0:0] reset;
  bit [0:0] reset_array [7:0];
  bit [0:0] out_valid;
  bit [0:0] out_valid_array [7:0];
  bit [15:0] out_prod;
  bit [15:0] out_prod_array [7:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(20) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"out_valid\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", out_valid_array[i_]); end
    $display("%d]}, ", out_valid_array[7]);
    $write("{\"Label\": \"out_prod\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", out_prod_array[i_]); end
    $display("%d]}", out_prod_array[7]);
    $write("]");
    $finish(0);
  end
  initial begin
      in_a_array[0] = 8'd3;
      in_a_array[1] = 8'd6;
      in_a_array[2] = 8'd12;
      in_a_array[3] = 8'd40;
      in_a_array[4] = 8'd23;
      in_a_array[5] = 8'd54;
      in_a_array[6] = 8'd29;
      in_a_array[7] = 8'd58;
      in_b_array[0] = 8'd1;
      in_b_array[1] = 8'd0;
      in_b_array[2] = 8'd128;
      in_b_array[3] = 8'd98;
      in_b_array[4] = 8'd103;
      in_b_array[5] = 8'd256;
      in_b_array[6] = 8'd31;
      in_b_array[7] = 8'd5;
      load_array[0] = 1'd0;
      load_array[1] = 1'd1;
      load_array[2] = 1'd1;
      load_array[3] = 1'd1;
      load_array[4] = 1'd1;
      load_array[5] = 1'd1;
      load_array[6] = 1'd1;
      load_array[7] = 1'd1;
      reset_array[0] = 1'd1;
      reset_array[1] = 1'd0;
      reset_array[2] = 1'd0;
      reset_array[3] = 1'd0;
      reset_array[4] = 1'd0;
      reset_array[5] = 1'd0;
      reset_array[6] = 1'd0;
      reset_array[7] = 1'd0;
    for(j_=0; j_<8; j_=j_+1) begin
        in_a=in_a_array[j_];
        in_b=in_b_array[j_];
        load=load_array[j_];
        reset=reset_array[j_];
      #0.5;
        out_valid_array[j_]=out_valid;
        out_prod_array[j_]=out_prod;
      @(negedge clk);
end
  end
  seqmult dut (.in_a(in_a), .in_b(in_b), .load(load), .reset(reset), .out_valid(out_valid), .out_prod(out_prod), .clk(clk));
endmodule