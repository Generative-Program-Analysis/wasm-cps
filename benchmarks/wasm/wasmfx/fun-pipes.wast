
;; Simple pipes example (functional version)
;; modified for just a single module
(module $pipes
  (type $pfun (func (result i32)))
  (type $cfun (func (param i32) (result i32)))
  (type $producer (cont $pfun))
  (type $consumer (cont $cfun))

  (start $main)

  (func $log (import "spectest" "print_i32") (param i32))
  (tag $send  (param i32))
  (tag $receive (result i32))


  (func $piper (param $n i32) (param $p (ref $producer)) (param $c (ref $consumer))
     (block $on-receive (result (ref $consumer))
        (resume $consumer (on $receive $on-receive) (local.get $n) (local.get $c))
        (return)
     ) ;; receive
     (local.set $c)
     (return_call $copiper (local.get $c) (local.get $p))
  )

  (func $copiper (param $c (ref $consumer)) (param $p (ref $producer))
     (local $n i32)
     (block $on-send (result i32 (ref $producer))
        (resume $producer (on $send $on-send) (local.get $p))
        (return)
     ) ;; send
     (local.set $p)
     (local.set $n)
     (return_call $piper (local.get $n) (local.get $p) (local.get $c))
  )

  (func $pipe (export "pipe") (param $p (ref $producer)) (param $c (ref $consumer))
     (call $piper (i32.const -1) (local.get $p) (local.get $c))
  )

  (elem declare func $nats $sum)

  ;; send n, n+1, ...
  (func $nats (param $n i32) (result i32)
     (loop $l
       (call $log (i32.const -1))
       (call $log (local.get $n))
       (suspend $send (local.get $n))
       (local.set $n (i32.add (local.get $n) (i32.const 1)))
       (br $l)
     )
     (unreachable)
  )

  ;; receive 10 nats and return their sum
  (func $sum (param $dummy i32) (result i32)
     (local $i i32)
     (local $a i32)
     (local.set $i (i32.const 10))
     (local.set $a (i32.const 0))
     (loop $l
       (local.set $a (i32.add (local.get $a) (suspend $receive)))
       (call $log (i32.const -2))
       (call $log (local.get $a))
       (local.set $i (i32.sub (local.get $i) (i32.const 1)))
       (br_if $l (i32.ne (local.get $i) (i32.const 0)))
     )
     (return (local.get $a))
  )

  (func $run (export "run") (param $n i32)
     (call $pipe (cont.bind $consumer $producer (local.get $n) (cont.new $consumer (ref.func $nats)))
                 (cont.new $consumer (ref.func $sum))
     )
  )
  (func $main
    (call $run (i32.const 0))  
  )
 
)

(invoke "run" (i32.const 0))
