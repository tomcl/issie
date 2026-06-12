 //-----------------------------------------------------
  2 // Design Name : ram_sp_sr_sw
  3 // File Name   : ram_sp_sr_sw.v
  4 // Function    : Synchronous read write RAM 
  5 // Coder       : Deepak Kumar Tala
  6 //-----------------------------------------------------
  7 module ram_sp_sr_sw (
  8 clk         , // Clock Input
  9 address     , // Address Input
 10 data        , // Data bi-directional
 11 cs          , // Chip Select
 12 we          , // Write Enable/Read Enable
 13 oe            // Output Enable
 14 ); 
 15 
 16 parameter DATA_WIDTH = 8 ;
 17 parameter ADDR_WIDTH = 8 ;
 18 parameter RAM_DEPTH = 1 << ADDR_WIDTH;
 19 
 20 //--------------Input Ports----------------------- 
 21 input                  clk         ;
 22 input [ADDR_WIDTH-1:0] address     ;
 23 input                  cs          ;
 24 input                  we          ;
 25 input                  oe          ; 
 26 
 27 //--------------Inout Ports----------------------- 
 28 inout [DATA_WIDTH-1:0]  data       ;
 29 
 30 //--------------Internal variables---------------- 
 31 bit [DATA_WIDTH-1:0] data_out ;
 32 bit [DATA_WIDTH-1:0] mem [0:RAM_DEPTH-1];
 33 bit                  oe_r;
 34 
 35 //--------------Code Starts Here------------------ 
 36 
 37 // Tri-State Buffer control 
 38 // output : When we = 0, oe = 1, cs = 1
 39 assign data = (cs && oe &&  ! we) ? data_out : 8'bz; 
 40 
 41 // Memory Write Block 
 42 // Write Operation : When we = 1, cs = 1
 43 always_ff @ (posedge clk)
 44 begin : MEM_WRITE
 45    if ( cs && we ) begin
 46        mem[address] = data;
 47    end
 48 end
 49 
 50 // Memory Read Block 
 51 // Read Operation : When we = 0, oe = 1, cs = 1
 52 always_ff @ (posedge clk)
 53 begin : MEM_READ
 54   if (cs &&  ! we && oe) begin
 55     data_out = mem[address];
 56     oe_r = 1;
 57   end else begin
 58     oe_r = 0;
 59   end
 60 end
 61 
 62 endmodule // End of Module ram_sp_sr_sw