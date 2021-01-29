use core::intrinsics;

/// Polyfill the methods for f32
pub trait F32Ext {
    /// Returns the largest integer less than or equal to a number.
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 3.7_f32;
    /// let g = 3.0_f32;
    /// let h = -3.7_f32;
    ///
    /// assert_eq!(f.floor(), 3.0);
    /// assert_eq!(g.floor(), 3.0);
    /// assert_eq!(h.floor(), -4.0);
    /// ```
    fn floor(self) -> f32;

    /// Returns the smallest integer greater than or equal to a number.
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 3.01_f32;
    /// let g = 4.0_f32;
    ///
    /// assert_eq!(f.ceil(), 4.0);
    /// assert_eq!(g.ceil(), 4.0);
    /// ```
    fn ceil(self) -> f32;

    /// Returns the nearest integer to a number. Round half-way cases away from
    /// `0.0`.
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 3.3_f32;
    /// let g = -3.3_f32;
    ///
    /// assert_eq!(f.round(), 3.0);
    /// assert_eq!(g.round(), -3.0);
    /// ```
    fn round(self) -> f32;

    /// Returns the integer part of a number.
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 3.7_f32;
    /// let g = 3.0_f32;
    /// let h = -3.7_f32;
    ///
    /// assert_eq!(f.trunc(), 3.0);
    /// assert_eq!(g.trunc(), 3.0);
    /// assert_eq!(h.trunc(), -3.0);
    /// ```
    fn trunc(self) -> f32;

    /// Returns the fractional part of a number.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 3.6_f32;
    /// let y = -3.6_f32;
    /// let abs_difference_x = (x.fract() - 0.6).abs();
    /// let abs_difference_y = (y.fract() - (-0.6)).abs();
    ///
    /// assert!(abs_difference_x <= f32::EPSILON);
    /// assert!(abs_difference_y <= f32::EPSILON);
    /// ```
    fn fract(self) -> f32;

    /// Computes the absolute value of `self`. Returns `NAN` if the
    /// number is `NAN`.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 3.5_f32;
    /// let y = -3.5_f32;
    ///
    /// let abs_difference_x = (x.abs() - x).abs();
    /// let abs_difference_y = (y.abs() - (-y)).abs();
    ///
    /// assert!(abs_difference_x <= f32::EPSILON);
    /// assert!(abs_difference_y <= f32::EPSILON);
    ///
    /// assert!(f32::NAN.abs().is_nan());
    /// ```
    fn abs(self) -> f32;

    /// Returns a number that represents the sign of `self`.
    ///
    /// - `1.0` if the number is positive, `+0.0` or `INFINITY`
    /// - `-1.0` if the number is negative, `-0.0` or `NEG_INFINITY`
    /// - `NAN` if the number is `NAN`
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 3.5_f32;
    ///
    /// assert_eq!(f.signum(), 1.0);
    /// assert_eq!(f32::NEG_INFINITY.signum(), -1.0);
    ///
    /// assert!(f32::NAN.signum().is_nan());
    /// ```
    fn signum(self) -> f32;

    /// Returns a number composed of the magnitude of `self` and the sign of
    /// `sign`.
    ///
    /// Equal to `self` if the sign of `self` and `sign` are the same, otherwise
    /// equal to `-self`. If `self` is a `NAN`, then a `NAN` with the sign of
    /// `sign` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 3.5_f32;
    ///
    /// assert_eq!(f.copysign(0.42), 3.5_f32);
    /// assert_eq!(f.copysign(-0.42), -3.5_f32);
    /// assert_eq!((-f).copysign(0.42), 3.5_f32);
    /// assert_eq!((-f).copysign(-0.42), -3.5_f32);
    ///
    /// assert!(f32::NAN.copysign(1.0).is_nan());
    /// ```
    fn copysign(self, sign: f32) -> f32;

    /// Fused multiply-add. Computes `(self * a) + b` with only one rounding
    /// error, yielding a more accurate result than an unfused multiply-add.
    ///
    /// Using `mul_add` can be more performant than an unfused multiply-add if
    /// the target architecture has a dedicated `fma` CPU instruction.
    ///
    /// # Examples
    ///
    /// ```
    /// let m = 10.0_f32;
    /// let x = 4.0_f32;
    /// let b = 60.0_f32;
    ///
    /// // 100.0
    /// let abs_difference = (m.mul_add(x, b) - ((m * x) + b)).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn mul_add(self, a: f32, b: f32) -> f32;

    /// Calculates Euclidean division, the matching method for `rem_euclid`.
    ///
    /// This computes the integer `n` such that
    /// `self = n * rhs + self.rem_euclid(rhs)`.
    /// In other words, the result is `self / rhs` rounded to the integer `n`
    /// such that `self >= n * rhs`.
    ///
    /// # Examples
    ///
    /// ```
    /// let a: f32 = 7.0;
    /// let b = 4.0;
    /// assert_eq!(a.div_euclid(b), 1.0); // 7.0 > 4.0 * 1.0
    /// assert_eq!((-a).div_euclid(b), -2.0); // -7.0 >= 4.0 * -2.0
    /// assert_eq!(a.div_euclid(-b), -1.0); // 7.0 >= -4.0 * -1.0
    /// assert_eq!((-a).div_euclid(-b), 2.0); // -7.0 >= -4.0 * 2.0
    /// ```
    fn div_euclid(self, rhs: f32) -> f32;

    /// Calculates the least nonnegative remainder of `self (mod rhs)`.
    ///
    /// In particular, the return value `r` satisfies `0.0 <= r < rhs.abs()` in
    /// most cases. However, due to a floating point round-off error it can
    /// result in `r == rhs.abs()`, violating the mathematical definition, if
    /// `self` is much smaller than `rhs.abs()` in magnitude and `self < 0.0`.
    /// This result is not an element of the function's codomain, but it is the
    /// closest floating point number in the real numbers and thus fulfills the
    /// property `self == self.div_euclid(rhs) * rhs + self.rem_euclid(rhs)`
    /// approximatively.
    ///
    /// # Examples
    ///
    /// ```
    /// let a: f32 = 7.0;
    /// let b = 4.0;
    /// assert_eq!(a.rem_euclid(b), 3.0);
    /// assert_eq!((-a).rem_euclid(b), 1.0);
    /// assert_eq!(a.rem_euclid(-b), 3.0);
    /// assert_eq!((-a).rem_euclid(-b), 1.0);
    /// // limitation due to round-off error
    /// assert!((-f32::EPSILON).rem_euclid(3.0) != 0.0);
    /// ```
    fn rem_euclid(self, rhs: f32) -> f32;

    /// Raises a number to an integer power.
    ///
    /// Using this function is generally faster than using `powf`
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 2.0_f32;
    /// let abs_difference = (x.powi(2) - (x * x)).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn powi(self, n: i32) -> f32;

    /// Raises a number to a floating point power.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 2.0_f32;
    /// let abs_difference = (x.powf(2.0) - (x * x)).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn powf(self, n: f32) -> f32;

    /// Returns the square root of a number.
    ///
    /// Returns NaN if `self` is a negative number.
    ///
    /// # Examples
    ///
    /// ```
    /// let positive = 4.0_f32;
    /// let negative = -4.0_f32;
    ///
    /// let abs_difference = (positive.sqrt() - 2.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// assert!(negative.sqrt().is_nan());
    /// ```
    fn sqrt(self) -> f32;

    /// Returns `e^(self)`, (the exponential function).
    ///
    /// # Examples
    ///
    /// ```
    /// let one = 1.0f32;
    /// // e^1
    /// let e = one.exp();
    ///
    /// // ln(e) - 1 == 0
    /// let abs_difference = (e.ln() - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn exp(self) -> f32;

    /// Returns `2^(self)`.
    ///
    /// # Examples
    ///
    /// ```
    /// let f = 2.0f32;
    ///
    /// // 2^2 - 4 == 0
    /// let abs_difference = (f.exp2() - 4.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn exp2(self) -> f32;

    /// Returns the natural logarithm of the number.
    ///
    /// # Examples
    ///
    /// ```
    /// let one = 1.0f32;
    /// // e^1
    /// let e = one.exp();
    ///
    /// // ln(e) - 1 == 0
    /// let abs_difference = (e.ln() - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn ln(self) -> f32;

    /// Returns the logarithm of the number with respect to an arbitrary base.
    ///
    /// The result may not be correctly rounded owing to implementation details;
    /// `self.log2()` can produce more accurate results for base 2, and
    /// `self.log10()` can produce more accurate results for base 10.
    ///
    /// # Examples
    ///
    /// ```
    /// let five = 5.0f32;
    ///
    /// // log5(5) - 1 == 0
    /// let abs_difference = (five.log(5.0) - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn log(self, base: f32) -> f32;

    /// Returns the base 2 logarithm of the number.
    ///
    /// # Examples
    ///
    /// ```
    /// let two = 2.0f32;
    ///
    /// // log2(2) - 1 == 0
    /// let abs_difference = (two.log2() - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn log2(self) -> f32;

    /// Returns the base 10 logarithm of the number.
    ///
    /// # Examples
    ///
    /// ```
    /// let ten = 10.0f32;
    ///
    /// // log10(10) - 1 == 0
    /// let abs_difference = (ten.log10() - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn log10(self) -> f32;

    /// Computes the sine of a number (in radians).
    ///
    /// # Examples
    ///
    /// ```
    /// let x = std::f32::consts::FRAC_PI_2;
    ///
    /// let abs_difference = (x.sin() - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn sin(self) -> f32;

    /// Computes the cosine of a number (in radians).
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 2.0 * std::f32::consts::PI;
    ///
    /// let abs_difference = (x.cos() - 1.0).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn cos(self) -> f32;

    /// Simultaneously computes the sine and cosine of the number, `x`. Returns
    /// `(sin(x), cos(x))`.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = std::f32::consts::FRAC_PI_4;
    /// let f = x.sin_cos();
    ///
    /// let abs_difference_0 = (f.0 - x.sin()).abs();
    /// let abs_difference_1 = (f.1 - x.cos()).abs();
    ///
    /// assert!(abs_difference_0 <= f32::EPSILON);
    /// assert!(abs_difference_1 <= f32::EPSILON);
    /// ```
    fn sin_cos(self) -> (f32, f32);

    /// Inverse hyperbolic sine function.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 1.0f32;
    /// let f = x.sinh().asinh();
    ///
    /// let abs_difference = (f - x).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn asinh(self) -> f32;

    /// Inverse hyperbolic cosine function.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = 1.0f32;
    /// let f = x.cosh().acosh();
    ///
    /// let abs_difference = (f - x).abs();
    ///
    /// assert!(abs_difference <= f32::EPSILON);
    /// ```
    fn acosh(self) -> f32;

    /// Restrict a value to a certain interval unless it is NaN.
    ///
    /// Returns `max` if `self` is greater than `max`, and `min` if `self` is
    /// less than `min`. Otherwise this returns `self`.
    ///
    /// Note that this function returns NaN if the initial value was NaN as
    /// well.
    ///
    /// # Panics
    ///
    /// Panics if `min > max`, `min` is NaN, or `max` is NaN.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!((-3.0f32).clamp(-2.0, 1.0) == -2.0);
    /// assert!((0.0f32).clamp(-2.0, 1.0) == 0.0);
    /// assert!((2.0f32).clamp(-2.0, 1.0) == 1.0);
    /// assert!((f32::NAN).clamp(-2.0, 1.0).is_nan());
    /// ```
    fn clamp(self, min: f32, max: f32) -> f32;
}

impl F32Ext for f32 {
    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn floor(self) -> f32 {
        unsafe { intrinsics::floorf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn ceil(self) -> f32 {
        unsafe { intrinsics::ceilf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn round(self) -> f32 {
        unsafe { intrinsics::roundf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn trunc(self) -> f32 {
        unsafe { intrinsics::truncf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn fract(self) -> f32 {
        self - self.trunc()
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn abs(self) -> f32 {
        unsafe { intrinsics::fabsf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn signum(self) -> f32 {
        if self.is_nan() {
            Self::NAN
        } else {
            1.0_f32.copysign(self)
        }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    fn copysign(self, sign: f32) -> f32 {
        unsafe { intrinsics::copysignf32(self, sign) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn mul_add(self, a: f32, b: f32) -> f32 {
        unsafe { intrinsics::fmaf32(self, a, b) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn div_euclid(self, rhs: f32) -> f32 {
        let q = (self / rhs).trunc();
        if self % rhs < 0.0 {
            return if rhs > 0.0 { q - 1.0 } else { q + 1.0 };
        }
        q
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn rem_euclid(self, rhs: f32) -> f32 {
        let r = self % rhs;
        if r < 0.0 {
            r + rhs.abs()
        } else {
            r
        }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn powi(self, n: i32) -> f32 {
        unsafe { intrinsics::powif32(self, n) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn powf(self, n: f32) -> f32 {
        unsafe { intrinsics::powf32(self, n) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn sqrt(self) -> f32 {
        unsafe { intrinsics::sqrtf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn exp(self) -> f32 {
        unsafe { intrinsics::expf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn exp2(self) -> f32 {
        unsafe { intrinsics::exp2f32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn ln(self) -> f32 {
        unsafe { intrinsics::logf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn log(self, base: f32) -> f32 {
        self.ln() / base.ln()
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn log2(self) -> f32 {
        unsafe { intrinsics::log2f32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn log10(self) -> f32 {
        unsafe { intrinsics::log10f32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn sin(self) -> f32 {
        unsafe { intrinsics::sinf32(self) }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn cos(self) -> f32 {
        unsafe { intrinsics::cosf32(self) }
    }

    #[inline]
    fn sin_cos(self) -> (f32, f32) {
        (self.sin(), self.cos())
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn asinh(self) -> f32 {
        (self.abs() + ((self * self) + 1.0).sqrt())
            .ln()
            .copysign(self)
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn acosh(self) -> f32 {
        if self < 1.0 {
            Self::NAN
        } else {
            (self + ((self * self) - 1.0).sqrt()).ln()
        }
    }

    #[must_use = "method returns a new number and does not mutate the original value"]
    #[inline]
    fn clamp(self, min: f32, max: f32) -> f32 {
        assert!(min <= max);
        let mut x = self;
        if x < min {
            x = min;
        }
        if x > max {
            x = max;
        }
        x
    }
}
