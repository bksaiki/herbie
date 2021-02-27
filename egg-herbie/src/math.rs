use egg::*;
use std::sync::atomic::{AtomicBool, Ordering};

use num_bigint::BigInt;
use num_rational::Ratio;
use num_traits::{Pow, Signed, Zero};

pub type Constant = num_rational::BigRational;
pub type RecExpr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;
pub type Runner = egg::Runner<Math, ConstantFold, IterData>;
pub type Iteration = egg::Iteration<IterData>;

pub struct IterData {
    pub extracted: Vec<(Id, Extracted)>,
}

pub struct Extracted {
    pub best: RecExpr,
    pub cost: usize,
}

impl IterationData<Math, ConstantFold> for IterData {
    fn make(runner: &Runner) -> Self {
        let mut extractor = Extractor::new(&runner.egraph, AstSize);
        let extracted = runner
            .roots
            .iter()
            .map(|&root| {
                let (cost, best) = extractor.find_best(root);
                let ext = Extracted { cost, best };
                (root, ext)
            })
            .collect();
        Self { extracted }
    }
}

// operators from FPCore
define_language! {
    pub enum Math {

        // constant-folding operators

        "+" = Add([Id; 3]),
        "-" = Sub([Id; 3]),
        "*" = Mul([Id; 3]),
        "/" = Div([Id; 3]),
        "pow" = Pow([Id; 3]),
        "neg" = Neg([Id; 2]),
        "sqrt" = Sqrt([Id; 2]),
        "fabs" = Fabs([Id; 2]),
        "ceil" = Ceil([Id; 2]),
        "floor" = Floor([Id; 2]),
        "round" = Round([Id; 2]),

        Constant(Constant),
        Symbol(egg::Symbol),
        Other(egg::Symbol, Vec<Id>),
    }
}

pub struct ConstantFold {
    pub unsound: AtomicBool,
    pub constant_fold: bool,
    pub prune: bool,
}

impl Default for ConstantFold {
    fn default() -> Self {
        Self {
            constant_fold: true,
            prune: true,
            unsound: AtomicBool::from(false),
        }
    }
}

impl Analysis<Math> for ConstantFold {
    type Data = Option<(Constant, PatternAst<Math>)>;
    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        if !egraph.analysis.constant_fold {
            return None;
        }

        let x = |id: &Id| egraph[*id].data.clone().map(|x| x.0);
        let is_zero = |id: &Id| {
            let data = egraph[*id].data.as_ref();
            match data {
                Some(data) => data.0.is_zero(),
                None => false,
            }
        };

        let mut missing_child = false;
        enode.for_each(|n| if egraph[n].data == None {
            missing_child = true;
        });
        if missing_child {
            return None;
        }

        Some((
            match enode {
                Math::Constant(c) => c.clone(),

                // real
                Math::Add([_p, a, b]) => x(a)? + x(b)?,
                Math::Sub([_p, a, b]) => x(a)? - x(b)?,
                Math::Mul([_p, a, b]) => x(a)? * x(b)?,
                Math::Div([_p, a, b]) => {
                    if x(b)?.is_zero() {
                        return None
                    } else {
                        x(a)? / x(b)?
                    }
                }
                Math::Neg([_p, a]) => -x(a)?.clone(),
                Math::Pow([_p, a, b]) => {
                    if is_zero(b) && !is_zero(a) {
                        Ratio::new(BigInt::from(1), BigInt::from(1))
                    } else if is_zero(a) && !is_zero(b) {
                        Ratio::new(BigInt::from(0), BigInt::from(1))
                    } else if x(b)?.is_integer()
                        && !(x(a)?.is_zero() && (x(b)?.is_zero() || x(b)?.is_negative()))
                    {
                        Pow::pow(x(a)?, x(b)?.to_integer())
                    } else {
                        return None
                    }
                }
                Math::Sqrt([_p, a]) => {
                    let a = x(a)?;
                    if *a.numer() > BigInt::from(0) && *a.denom() > BigInt::from(0) {
                        let s1 = a.numer().sqrt();
                        let s2 = a.denom().sqrt();
                        let is_perfect = &(&s1 * &s1) == a.numer() && &(&s2 * &s2) == a.denom();
                        if is_perfect {
                            Ratio::new(s1, s2)
                        } else {
                            return None
                        }
                    } else {
                        return None
                    }
                }
                Math::Fabs([_p, a]) => x(a)?.clone().abs(),
                Math::Floor([_p, a]) => x(a)?.floor(),
                Math::Ceil([_p, a]) => x(a)?.ceil(),
                Math::Round([_p, a]) => x(a)?.round(),

                _ => return None
            },
            {
                let mut pattern: PatternAst<Math> = Default::default();
                enode.for_each(|child| {
                    if let Some(constant) = x(&child) {
                        pattern.add(ENodeOrVar::ENode(Math::Constant(constant)));
                    } else {
                        panic!("Child didn't have constant");
                    }
                });
                let mut counter = 0;
                let mut head = enode.clone();
                head.update_children(|_child| {
                    let res = Id::from(counter);
                    counter += 1;
                    res
                });
                pattern.add(ENodeOrVar::ENode(head));
                pattern
            }))
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        match (&to, from) {
            (None, None) => false,
            (Some(_), None) => false, // no update needed
            (None, Some(c)) => {
                *to = Some(c);
                true
            }
            (Some(a), Some(ref b)) => {
                if a.0 != b.0 {
                    if !self.unsound.swap(true, Ordering::SeqCst) {
                        log::warn!("Bad merge detected: {} != {}", a.0, b.0);
                    }
                }
                false
            }
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        let class = &mut egraph[id];
        if let Some((c, node)) = class.data.clone() {
            let added = egraph.add(Math::Constant(c.clone()));
            let (id, did_something) = egraph.union(id, added);
            if did_something {
                let mut const_pattern: PatternAst<Math> = Default::default();
                const_pattern.add(ENodeOrVar::ENode(Math::Constant(c)));
                egraph.add_union_proof(
                    node,
                    const_pattern,
                    Default::default(),
                    "metadata-eval".to_string(),
                );
            }
            if egraph.analysis.prune {
                egraph[id].nodes.retain(|n| n.is_leaf())
            }
        }
    }
}
