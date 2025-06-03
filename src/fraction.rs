use std::num::NonZeroU32;

use num_integer::Integer;

#[derive(Clone, Copy)]
pub struct Fraction {
    pub n: u32,
    pub d: NonZeroU32,
}

struct InnerDisplayFraction<'a>(&'a Fraction);

impl std::fmt::Debug for InnerDisplayFraction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d = self.0.d.get();
        if d == 1 {
            write!(f, "{}", self.0.n)
        } else {
            write!(f, "{}/{}", self.0.n, d)
        }
    }
}

impl std::fmt::Display for Fraction {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&InnerDisplayFraction(self), f)
    }
}

impl std::fmt::Debug for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Fraction")
            .field(&InnerDisplayFraction(self))
            .finish()
    }
}

impl From<u32> for Fraction {
    fn from(n: u32) -> Self {
        Fraction {
            n,
            d: unsafe { NonZeroU32::new_unchecked(1) },
        }
    }
}

impl Fraction {
    pub fn simplify_assign(mut self) {
        let gcd = self.n.gcd(&self.d.get());
        self.n /= gcd;
        self.d = NonZeroU32::new(self.d.get() / gcd).expect("non zero");
    }

    pub fn simplify(&self) -> Self {
        let me = *self;
        me.simplify_assign();
        me
    }
}

impl std::ops::Div<Fraction> for Fraction {
    type Output = Fraction;

    #[inline]
    fn div(mut self, rhs: Fraction) -> Self::Output {
        std::ops::DivAssign::div_assign(&mut self, rhs);
        self
    }
}

impl std::ops::DivAssign<Fraction> for Fraction {
    #[allow(clippy::suspicious_op_assign_impl)]
    fn div_assign(&mut self, rhs: Fraction) {
        if let Some(d) = NonZeroU32::new(self.d.get() * rhs.n) {
            self.d = d;
        } else {
            panic!("attempt to divide by zero");
        }
        self.n *= rhs.d.get();
        self.simplify_assign();
    }
}

impl std::ops::Div<u32> for Fraction {
    type Output = Fraction;

    fn div(mut self, rhs: u32) -> Self::Output {
        std::ops::DivAssign::div_assign(&mut self, rhs);
        self
    }
}

impl std::ops::DivAssign<u32> for Fraction {
    fn div_assign(&mut self, rhs: u32) {
        let rhs: Fraction = rhs.into();
        *self /= rhs;
    }
}

impl std::ops::Mul for Fraction {
    type Output = Fraction;

    fn mul(mut self, rhs: Self) -> Self::Output {
        std::ops::MulAssign::mul_assign(&mut self, rhs);
        self
    }
}

impl std::ops::Mul<u32> for Fraction {
    type Output = Fraction;

    fn mul(self, rhs: u32) -> Self::Output {
        let rhs: Fraction = rhs.into();
        self * rhs
    }
}

impl std::ops::MulAssign for Fraction {
    fn mul_assign(&mut self, rhs: Self) {
        self.n *= rhs.n;
        self.d = NonZeroU32::new(self.d.get() * rhs.d.get()).expect("non zero");
        self.simplify_assign();
    }
}

impl std::ops::MulAssign<u32> for Fraction {
    fn mul_assign(&mut self, rhs: u32) {
        let rhs: Fraction = rhs.into();
        std::ops::MulAssign::mul_assign(self, rhs);
    }
}

impl std::cmp::PartialEq for Fraction {
    fn eq(&self, other: &Self) -> bool {
        let a = self.simplify();
        let b = other.simplify();
        a.n == b.n && a.d == b.d
    }
}

impl std::cmp::PartialEq<u32> for Fraction {
    fn eq(&self, other: &u32) -> bool {
        let other: Fraction = (*other).into();
        *self == other
    }
}

impl std::cmp::Eq for Fraction {}

impl std::cmp::PartialOrd for Fraction {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl std::cmp::PartialOrd<u32> for Fraction {
    fn partial_cmp(&self, other: &u32) -> Option<std::cmp::Ordering> {
        let other: Fraction = (*other).into();
        std::cmp::PartialOrd::partial_cmp(self, &other)
    }
}

impl std::cmp::Ord for Fraction {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let left = self.n * other.d.get();
        let right = self.d.get() * other.n;
        left.cmp(&right)
    }
}

impl std::hash::Hash for Fraction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let me = self.simplify();
        me.n.hash(state);
        me.d.hash(state);
    }
}
