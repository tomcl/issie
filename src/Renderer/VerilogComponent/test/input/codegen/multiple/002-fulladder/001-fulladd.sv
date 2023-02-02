module fulladd(s, c_out, ain, bin, c_in);
   output bit s, c_out;
   input bit ain, bin, c_in;
   assign s = (ain^bin)^c_in; // sum bit
   assign c_out = (ain & bin) | (bin & c_in) | (c_in & ain); //carry bit
endmodule