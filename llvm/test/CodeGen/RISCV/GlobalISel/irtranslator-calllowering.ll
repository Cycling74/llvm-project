; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=riscv32 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV32I %s
; RUN: llc -mtriple=riscv64 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV64I %s

define void @foo() {
  ; RV32I-LABEL: name: foo
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: foo
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoRET
entry:
  ret void
}
