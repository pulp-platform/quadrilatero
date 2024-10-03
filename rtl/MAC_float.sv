// Copyright 2024 EPFL
// Solderpad Hardware License, Version 2.1, see LICENSE.md for details.
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
//
// Author: Danilo Cammarata

module MAC_float (
    input  logic        clk_i         ,
    input  logic        rst_ni        ,
    input  logic [31:0] data_i        ,
    input  logic [31:0] weight_i      ,
    input  logic [31:0] acc_i         ,
    input  logic        valid_i       ,

    output logic        mac_finished_o,
    output logic [31:0] acc_o
);

  logic               in_ready_out;
  logic               busy_out    ;
  logic               tag_out     ;
  logic               ready       ;
  fpnew_pkg::status_t status_out  ;

  localparam fpnew_pkg::fpu_implementation_t FPUImplementation [1] = '{
    '{
        PipeRegs: // FMA Block
                  '{
                    '{  1, // FP32
                        2, // FP64
                        0, // FP16
                        0, // FP8
                        0  // FP16alt
                      },
                    '{1, 1, 1, 1, 1},   // DIVSQRT
                    '{1,
                      1,
                      1,
                      1,
                      1},   // NONCOMP
                    '{2,
                      2,
                      2,
                      2,
                      2}    // CONV
                    },
        UnitTypes: '{'{fpnew_pkg::PARALLEL,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED},  // FMA
                    '{fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED}, // DIVSQRT
                    '{fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED}, // NONCOMP
                    '{fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED}   // CONV
                    },  
        PipeConfig: fpnew_pkg::BEFORE
    }
  };

  fpnew_top #(
    // FPU configuration
    .Features       (fpnew_pkg::RV32F),
    .Implementation (FPUImplementation[0])
  ) fpu_inst (
    .clk_i          ,
    .rst_ni         ,
    // Input signals
    .operands_i     ({acc_i,weight_i,data_i}),
    .rnd_mode_i     (fpnew_pkg::RNE),
    .op_i           (fpnew_pkg::FMADD),
    .op_mod_i       (1'b0),
    .src_fmt_i      (fpnew_pkg::FP32),
    .dst_fmt_i      (fpnew_pkg::FP32),
    .int_fmt_i      (fpnew_pkg::INT32),
    .vectorial_op_i (1'b0),
    .tag_i          (1'b0),
    .simd_mask_i    ('1),
    // Input Handshake
    .in_valid_i     (valid_i  ),
    .in_ready_o     (in_ready_out),
    .flush_i        (1'b0),
    // Output signals
    .result_o       (acc_o),
    .status_o       (status_out),
    .tag_o          (tag_out),
    // Output handshake
    .out_valid_o    (mac_finished_o),
    .out_ready_i    (ready),
    // Indication of valid data in flight
    .busy_o         (busy_out)
  );

  always_ff @(posedge clk_i or negedge rst_ni) begin : seq_block
    if (!rst_ni) begin
      ready        <= '0;
    end else begin
      ready        <= valid_i;
    end
  end
endmodule
