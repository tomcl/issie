// Verilog 1995 style port declaration
module combined (
    addr,
    wdata,
    rdata
);

parameter BUS_WIDTH  = 2,
          DATA_WIDTH = 5,
          FIFO_DEPTH = 10;

input  bit [BUS_WIDTH-1:0]  addr;
input  bit [DATA_WIDTH-1:0] wdata;
input  bit                  clk;
output bit [DATA_WIDTH-1:0] rdata;

bit [DATA_WIDTH-1:0] fifo [FIFO_DEPTH-1:0];

bit i;

always_ff @(posedge clk) begin
  for (i = 5'd0; i < 5'd(FIFO_DEPTH); i = i + 1'd1) begin
      if (addr == i) 
        fifo[i] <= wdata;
  end
end

endmodule