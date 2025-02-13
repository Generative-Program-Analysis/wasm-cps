(module $state
  (type $gf (;0;) (func (param i32) (result i32)))
  (type $sf (;1;) (func (result i32)))
  (type $gk (;2;) (cont $gf))
  (type $sk (;3;) (cont $sf))
  (type (;4;) (func (param i32)))
  (type (;5;) (func (param (ref $gk) i32) (result i32)))
  (type (;6;) (func (result i32 (ref $sk))))
  (type (;7;) (func (param i32 (ref $sk)) (result i32)))
  (tag $get (;0;) (type $sf) (result i32))
  (tag $set (;1;) (type 4) (param i32))
  ;; (export "run" (func $run))
  (export "main" (func $run))
  (elem (;0;) declare func $f)
  (func $getting (;0;) (type 5) (param $k (ref $gk)) (param $s i32) (result i32)
    block $on_get (result (ref $gk))
      block $on_set (type 6) (result i32 (ref $sk))
        local.get $s
        local.get $k
        resume $gk (on $get $on_get) (on $set $on_set)
        return
      end
      return_call $setting
    end
    local.get $s
    return_call $getting
  )
  (func $setting (;1;) (type 7) (param $s i32) (param $k (ref $sk)) (result i32)
    block $on_get (result (ref $gk))
      block $on_set (type 6) (result i32 (ref $sk))
        local.get $k
        resume $sk (on $get $on_get) (on $set $on_set)
        return
      end
      return_call $setting
    end
    local.get $s
    return_call $getting
  )
  (func $f (;2;) (type $sf) (result i32)
    i32.const 7
    suspend $set
    suspend $get
    i32.const 2
    i32.const 3
    suspend $set
    i32.const 3
    suspend $get
    i32.add
    i32.mul
    i32.add
  )
  (func $run (;3;) (type $sf) (result i32)
    i32.const 0
    ref.func $f
    cont.new $sk
    call $setting
  )
)
