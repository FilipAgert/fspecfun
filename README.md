# fspec — Special Functions Library in Modern Fortran

`fspec` is a lightweight, high-precision library for computing special mathematical functions in Fortran.

---
## Compiling
To compile, simply run:

```bash
make
```

This builds the executable at:

```
app/fspec
```

---
## Status

**Implemented**:
- `gamma(x)` — Gamma function via [Lanczos approximation](https://en.wikipedia.org/wiki/Lanczos_approximation) (currently accurate to >10 digits)

Accuracy of `gamma(0.1)`:
```text
Expected:   9.5135076986687318362924871772654021925505786260883773430500007704342654...
Current:  ≈ gamma(  0.10) =      9.51350769866873627377868141404
Number of correct digits: 14
```

**Work in Progress**:


**TODO**:
- `Ai(x)` — Airy function
- `Pnl(x)` — Orthogonal polynomials (Hermite, Legendre, Laguerre...)
- `jn(x), yn(x)` — Bessel functions
