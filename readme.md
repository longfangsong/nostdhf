# nostdhf

I really want to do some floating point arithmetic on my embedded controller, which do has an FPU, 
but it seems rust [still doesn't provide f32 and f64 methods in `no_std`](https://github.com/rust-lang/rust/issues/50145) now.

Some alternative is using libraries like [micromath](https://docs.rs/micromath/1.1.0/micromath/), 
but micromath is just a software floating point library. 
Even if we do have an FPU which supports `copysign` with commands like `fsgnj` on riscv,
and we can use `intrinsics::copysignf64`, micromath does not use it.

So I wrote this, to make our life easier when doing floating point arithmetic on embedded systems,
and use hardware floating point if possible.

Basically, this will use unsafe functions in `intrinsics` to implement the floating point operations, which just as the std library does,
and export `F32Ext` and `F64Ext` traits.
