(module
  (memory (export "memory") 1 1) ;; export memory "memory" of 1 page (64KB), max 1 page
  (func (export "write_byte_wat") (param $offset i32) (param $value i32)
    local.get $offset
    local.get $value
    i32.store8
  )
  (func (export "read_byte_wat") (param $offset i32) (result i32)
    local.get $offset
    i32.load8_u
  )
)