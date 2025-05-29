(; import_nested_wat.wat – Minimal nested-import module written directly in WAT
   Mirrors the interface of import_nested.aot built from C, but authored in WAT
   so we can exercise mixed-tool-chain scenarios. :) ;)

(module
  ;; ----- Imports -----
  (import "env" "give_host_control" (func $give (param i32)))

  ;; ----- Memory -----
  (memory (export "memory") 1)   ;; 64KiB
  ;; Zero-initialised data segment occupying first 64 bytes (16 * 4-byte words)
  (data (i32.const 0) "\00\00\00\00" "\00\00\00\00" "\00\00\00\00" "\00\00\00\00"
                  "\00\00\00\00" "\00\00\00\00" "\00\00\00\00" "\00\00\00\00"
                  "\00\00\00\00" "\00\00\00\00" "\00\00\00\00" "\00\00\00\00"
                  "\00\00\00\00" "\00\00\00\00" "\00\00\00\00" "\00\00\00\00")

  (global $g_entry (mut i32) (i32.const 0))
  (global $g_exit  (mut i32) (i32.const 0))

  ;; ----- Helpers -----
  (func $bounds_ok (param $idx i32) (result i32)
    (i32.lt_u (local.get $idx) (i32.const 16)))

  ;; ----- Exports -----
  ;; Return ptr (i32) to the start of global buffer
  (func (export "get_data_ptr") (result i32)
    (i32.const 0))

  (func $get_mem_base (export "get_mem_base") (result i32)
        (i32.const 0))

  ;; helper: addr = base + depth*8
  (func $slot_addr (param $d i32) (result i32)
        (i32.add (i32.const 0x0010)        ;; first slot offset
                 (i32.mul (local.get $d) (i32.const 8))))

  ;; xor_memory(index: i32, xor_val: i32)
  (func (export "xor_memory") (param $idx i32) (param $xor i32)
    (local $addr i32)
    (if (call $bounds_ok (local.get $idx))
        (then
          (local.set $addr (i32.mul (local.get $idx) (i32.const 4)))
          (i32.store
            (local.get $addr)
            (i32.xor (i32.load (local.get $addr)) (local.get $xor)))))
  )

  ;; call_host_and_read(index: i32, init_val: i32) -> i32
  (func (export "call_host_and_read")
        (param $d i32) (param $seed i32) (result i32)
    (local $addr i32)
    ;; bounds check if desired …

    ;; ---- Step A ----
    (local.set $addr (call $slot_addr (local.get $d)))
    (i32.store (local.get $addr) (local.get $seed))          ;; write seed
    ;; set ENTERED flag
    (i32.store8 offset=4 (local.get $addr) (i32.const 1))
    ;; ++entry
    (global.set $g_entry
       (i32.add (global.get $g_entry) (i32.const 1)))

    ;; ---- Host round-trip ----
    (call $give (local.get $d))

    ;; ---- Step C ----
    (if (i32.ne (i32.load (local.get $addr)) (local.get $seed))
        (then unreachable))                                  ;; host tampered
    ;; set HOST_OK flag
    (i32.store8 offset=4 (local.get $addr) (i32.const 3))    ;; bits0|1

    ;; ---- Step D ----
    (i32.store (local.get $addr)
      (i32.xor (i32.load (local.get $addr))
               (i32.xor (i32.const 0xA5A5A5A5) (local.get $d))))
    ;; ++exit
    (global.set $g_exit
      (i32.add (global.get $g_exit) (i32.const 1)))
    ;; set EXITED flag
    (i32.store8 offset=4 (local.get $addr) (i32.const 7))

    (return (i32.load (local.get $addr)))
  )
) 