(module
  (type $i32_to_i32 (func (param i32) (result i32)))
  (type $i32_i32_to_i32 (func (param i32 i32) (result i32)))
  (type $void_to_void (func))
  (type $f32_f32_to_f32 (func (param f32 f32) (result f32)))

  (func $add_10 (type $i32_to_i32) (param $p i32) (result i32)
    local.get $p
    i32.const 10
    i32.add)

  (func $mul_by_5 (type $i32_to_i32) (param $p i32) (result i32)
    local.get $p
    i32.const 5
    i32.mul)
  
  (func $nop_func (type $void_to_void)
    nop)

  (func $sum_two_f32 (type $f32_f32_to_f32) (param $a f32) (param $b f32) (result f32)
    local.get $a
    local.get $b
    f32.add)

  (table (export "__indirect_function_table") 4 4 funcref) 
  ;; Table exported as "__indirect_function_table", initial size 4, max size 4, elements are funcref

  ;; Initialize table elements (WAMR might need explicit initialization or linkage)
  ;; Element segments are one way, or functions can be placed via host or Wasm.
  ;; For simplicity with WAMR's wasm_runtime_call_indirect, the table must be populated.
  ;; If emcc compiles C to wasm, it often creates and populates __indirect_function_table.
  ;; We can rely on that or use element segments.
  (elem (i32.const 0) $add_10 $mul_by_5 $nop_func $sum_two_f32) ;; Put $add_10 at index 0, $mul_by_5 at index 1, $nop_func at index 2, $sum_two_f32 at index 3

  (func (export "call_idx_0_with_val") (param $val i32) (result i32)
    local.get $val
    i32.const 0 
    call_indirect (type $i32_to_i32)
  )
  (func (export "call_idx_1_with_val") (param $val i32) (result i32)
    local.get $val
    i32.const 1
    call_indirect (type $i32_to_i32)
  )
  (func (export "call_idx_2") 
    i32.const 2
    call_indirect (type $void_to_void)
  )
)