module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [14:0];
  bit [0:0] load;
  bit [0:0] load_array [14:0];
  bit [0:0] enable;
  bit [0:0] enable_array [14:0];
  bit [3:0] count_out;
  bit [3:0] count_out_array [14:0];
  bit [3:0] count;
  bit [3:0] count_array [14:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(34) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"count_out\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", count_out_array[i_]); end
    $display("%d]}, ", count_out_array[14]);
    $write("{\"Label\": \"count\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", count_array[i_]); end
    $display("%d]}", count_array[14]);
    $write("]");
    $finish(0);
  end
  initial begin
      reset_array[0] = 1'd0;
      reset_array[1] = 1'd1;
      reset_array[2] = 1'd0;
      reset_array[3] = 1'd0;
      reset_array[4] = 1'd0;
      reset_array[5] = 1'd0;
      reset_array[6] = 1'd0;
      reset_array[7] = 1'd0;
      reset_array[8] = 1'd0;
      reset_array[9] = 1'd0;
      reset_array[10] = 1'd0;
      reset_array[11] = 1'd0;
      reset_array[12] = 1'd0;
      reset_array[13] = 1'd0;
      reset_array[14] = 1'd0;
      load_array[0] = 1'd1;
      load_array[1] = 1'd0;
      load_array[2] = 1'd0;
      load_array[3] = 1'd0;
      load_array[4] = 1'd0;
      load_array[5] = 1'd0;
      load_array[6] = 1'd0;
      load_array[7] = 1'd0;
      load_array[8] = 1'd0;
      load_array[9] = 1'd0;
      load_array[10] = 1'd0;
      load_array[11] = 1'd0;
      load_array[12] = 1'd0;
      load_array[13] = 1'd0;
      load_array[14] = 1'd0;
      enable_array[0] = 1'd1;
      enable_array[1] = 1'd1;
      enable_array[2] = 1'd1;
      enable_array[3] = 1'd1;
      enable_array[4] = 1'd1;
      enable_array[5] = 1'd1;
      enable_array[6] = 1'd1;
      enable_array[7] = 1'd1;
      enable_array[8] = 1'd1;
      enable_array[9] = 1'd1;
      enable_array[10] = 1'd1;
      enable_array[11] = 1'd1;
      enable_array[12] = 1'd1;
      enable_array[13] = 1'd1;
      enable_array[14] = 1'd1;
    for(j_=0; j_<15; j_=j_+1) begin
        reset=reset_array[j_];
        load=load_array[j_];
        enable=enable_array[j_];
      #0.5;
        count_out_array[j_]=count_out;
        count_array[j_]=count;
      @(negedge clk);
end
  end
  counter4bit dut (.reset(reset), .load(load), .enable(enable), .count_out(count_out), .count(count), .clk(clk));
endmodule