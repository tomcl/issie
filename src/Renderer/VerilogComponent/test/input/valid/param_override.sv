module param_override(
  // Write your IO Port Declarations here
  input bit address,
  output bit [1:0] outputs
);  
  // Write your Assignments here
  param_new p1 (
    .addr(address),
    .out(outputs)
  );

endmodule