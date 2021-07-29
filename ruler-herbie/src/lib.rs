use ruler::*;

use std::ffi::{CString};
use std::os::raw::c_char;
use gag::Gag;

mod rational;
mod bool;

use crate::rational::Rational;
use crate::bool::Boolean;

trait HerbieLanguage {
    fn run_with_params(iterc: usize, argc: usize, fuzzc: usize, do_final: bool) -> String;
}

impl HerbieLanguage for Rational {
    fn run_with_params(iterc: usize, argc: usize, fuzzc: usize, do_final: bool) -> String {
        // Transcription of <ruler>/src/lib.rs
        let params = SynthParams {
            seed:0,
            n_samples:0,
            variables:argc,
            iters:iterc,
            rules_to_take:0,
            chunk_size:100000,
            minimize:false,
            no_constants_above_iter:999999,
            no_conditionals:false,
            no_run_rewrites:false,
            linear_cvec_matching:false,
            outfile:String::from("out.json"),
            eqsat_node_limit:300000,
            eqsat_iter_limit:2,
            eqsat_time_limit:60,
            important_cvec_offsets:2,
            str_int_variables:1,
            complete_cvec:false,
            no_xor:false,
            no_shift:false,
            num_fuzz:fuzzc,
            use_smt:false,
            do_final_run:do_final,
        };

        let syn = Synthesizer::<Self>::new(params);
        let report = syn.run();
        let mut string = String::from("");

        for eq in report.eqs {
            string.push_str(&eq.lhs.to_string());
            if eq.rewrites.len() > 1 {
                string.push_str(" <=> ");
            } else {
                string.push_str(" => ");
            }
            string.push_str(&eq.rhs.to_string());
            string.push('\n');
        }

        string
    }
}

impl HerbieLanguage for Boolean {
    fn run_with_params(iterc: usize, argc: usize, fuzzc: usize, do_final: bool) -> String {
        // Transcription of <ruler>/src/lib.rs
        let params = SynthParams {
            seed:0,
            n_samples:0,
            variables:argc,
            iters:iterc,
            rules_to_take:0,
            chunk_size:100000,
            minimize:false,
            no_constants_above_iter:999999,
            no_conditionals:true,
            no_run_rewrites:false,
            linear_cvec_matching:false,
            outfile:String::from("out.json"),
            eqsat_node_limit:300000,
            eqsat_iter_limit:2,
            eqsat_time_limit:60,
            important_cvec_offsets:5,
            str_int_variables:1,
            complete_cvec:false,
            no_xor:true,    // Herbie doesn't use xor
            no_shift:false,
            num_fuzz:fuzzc,
            use_smt:false,
            do_final_run:do_final,
        };

        let syn = Synthesizer::<Self>::new(params);
        let report = syn.run();
        let mut string = String::from("");

        for eq in report.eqs {
            string.push_str(&eq.lhs.to_string());
            if eq.rewrites.len() > 1 {
                string.push_str(" <=> ");
            } else {
                string.push_str(" => ");
            }
            string.push_str(&eq.rhs.to_string());
            string.push('\n');
        }
        
        string
    }
}

////////////////////// Exports //////////////////////

#[no_mangle]
pub unsafe extern "C" fn generate_rational_rules(
    iters: u32,
    argc: u32,
    fuzzc: u32,
    do_final: bool
) -> *const c_char { 
    println!("Generating rational rules w/ Ruler...");
    println!(" Iterations: {}", iters);
    println!(" Variables: {}", argc);
    println!(" Num Fuzz: {}", fuzzc);
    println!(" Final Run?: {}", do_final);

    let gag = Gag::stdout().unwrap();
    let rules = Rational::run_with_params(iters as usize, argc as usize, fuzzc as usize, do_final);
    let string = CString::new(rules.to_string()).unwrap();
    let ptr = string.as_ptr();
    std::mem::forget(string);
    drop(gag);

    ptr
}

#[no_mangle]
pub unsafe extern "C" fn generate_boolean_rules(
    iters: u32,
    argc: u32,
    fuzzc: u32,
    do_final: bool
) -> *const c_char { 
    println!("Generating boolean rules w/ Ruler...");
    println!(" Iterations: {}", iters);
    println!(" Variables: {}", argc);
    println!(" Num Fuzz: {}", fuzzc);
    println!(" Final Run?: {}", do_final);

    let gag = Gag::stdout().unwrap();
    let rules = Boolean::run_with_params(iters as usize, argc as usize, fuzzc as usize, do_final);
    let string = CString::new(rules.to_string()).unwrap();
    let ptr = string.as_ptr();
    std::mem::forget(string);
    drop(gag);

    ptr
}

#[no_mangle]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    let _str = CString::from_raw(ptr);
}
