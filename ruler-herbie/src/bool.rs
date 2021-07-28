use egg::*;
use ruler::*;

use std::ops::*;

define_language! {
    pub enum Boolean {
        "~" = Not(Id),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "^" = Xor([Id; 2]),
        Lit(bool),
        Var(egg::Symbol),
    }
}
impl SynthLanguage for Boolean {
    type Constant = bool;

    fn convert_parse(s: &str) -> RecExpr<Self> {
        let s = s
            .replace("and", "&")
            .replace("xor", "^")
            .replace("or", "|")
            .replace("not", "~");
        s.parse().unwrap()
    }

    fn eval<'a, F>(&'a self, cvec_len: usize, mut v: F) -> CVec<Self>
    where
        F: FnMut(&'a Id) -> &'a CVec<Self>,
    {
        match self {
            Boolean::Not(a) => map!(v, a => Some(a.not())),

            Boolean::And([a, b]) => map!(v, a, b => Some(*a & *b)),
            Boolean::Or([a, b]) => map!(v, a, b => Some(*a | *b)),
            Boolean::Xor([a, b]) => map!(v, a, b => Some(*a ^ *b)),

            Boolean::Lit(n) => vec![Some(n.clone()); cvec_len],
            Boolean::Var(_) => vec![],
        }
    }

    fn to_var(&self) -> Option<Symbol> {
        if let Boolean::Var(sym) = self {
            Some(*sym)
        } else {
            None
        }
    }

    fn mk_var(sym: Symbol) -> Self {
        Boolean::Var(sym)
    }

    fn to_constant(&self) -> Option<&Self::Constant> {
        if let Boolean::Lit(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn mk_constant(c: Self::Constant) -> Self {
        Boolean::Lit(c)
    }

    fn init_synth(synth: &mut Synthesizer<Self>) {
        // let consts: Vec<Option<bool>> = vec![];
        let consts: Vec<Option<bool>> = vec![Some(false), Some(true)];

        let consts = self_product(&consts, synth.params.variables);
        println!("cvec len: {}", consts[0].len());

        let mut egraph = EGraph::new(SynthAnalysis {
            cvec_len: consts[0].len(),
        });

        egraph.add(Boolean::Lit(false));
        egraph.add(Boolean::Lit(true));

        for i in 0..synth.params.variables {
            let var = Symbol::from(letter(i));
            let id = egraph.add(Boolean::Var(var));
            egraph[id].data.cvec = consts[i].clone();
        }

        synth.egraph = egraph;
    }

    fn make_layer(synth: &Synthesizer<Self>, iter: usize) -> Vec<Self> {
        let mut extract = Extractor::new(&synth.egraph, NumberOfOps);

        // maps ids to n_ops
        let ids: HashMap<Id, usize> = synth
            .ids()
            .map(|id| (id, extract.find_best_cost(id)))
            .collect();

        let mut to_add = vec![];
        for i in synth.ids() {
            for j in synth.ids() {
                if ids[&i] + ids[&j] + 1 != iter {
                    continue;
                }
                to_add.push(Boolean::And([i, j]));
                to_add.push(Boolean::Or([i, j]));
                if !synth.params.no_xor {
                    to_add.push(Boolean::Xor([i, j]));
                }
            }
            if ids[&i] + 1 != iter {
                continue;
            }
            to_add.push(Boolean::Not(i));
        }

        log::info!("Made a layer of {} enodes", to_add.len());
        to_add
    }

    fn is_valid(
        _synth: &mut Synthesizer<Self>,
        _lhs: &Pattern<Self>,
        _rhs: &Pattern<Self>,
    ) -> bool {
        true
    }
}
