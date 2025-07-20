# fspec — Special Functions Library in Modern Fortran

`fspec` is a lightweight, high-precision library for computing special mathematical functions in Fortran.

---

## 🚧 Status

✅ **Implemented**:
- `gammaf(x)` — Gamma function via [Lanczos approximation](https://en.wikipedia.org/wiki/Lanczos_approximation) (currently accurate to ~4 digits)

⚠️ Accuracy of `gammaf(0.1)`:
```text
Expected:   9.5135076986687318362924871772654021925505786260883773430500007704342654...
Current:  ≈ 9.51341 (3-decimal agreement)

