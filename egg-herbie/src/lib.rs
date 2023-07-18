pub mod math;
pub mod rules;

use egg::{Extractor, Id, Iteration, Language, StopReason, Symbol};
use indexmap::IndexMap;
use math::*;

use std::cmp::min;
use std::ffi::{CStr, CString};
use std::mem::{self, ManuallyDrop};
use std::os::raw::c_char;
use std::time::Duration;
use std::{slice, sync::atomic::Ordering};

pub struct Context {
    iteration: usize,
    runner: Runner,
    rules: Vec<Rewrite>,
}

// I had to add $(rustc --print sysroot)/lib to LD_LIBRARY_PATH to get linking to work after installing rust with rustup
#[no_mangle]
pub unsafe extern "C" fn egraph_create() -> *mut Context {
    Box::into_raw(Box::new(Context {
        iteration: 0,
        runner: Runner::new(Default::default()).with_explanations_enabled(),
        rules: vec![],
    }))
}

#[no_mangle]
pub unsafe extern "C" fn egraph_destroy(ptr: *mut Context) {
    drop(Box::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn destroy_egraphiters(_size: u32, ptr: *mut EGraphIter) {
    // let array: &[EGraphIter] = slice::from_raw_parts(ptr, size as usize);
    // TODO: Maybe glue vector back together and call drop on it?
    libc::free(ptr as *mut libc::c_void);
}

#[no_mangle]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    drop(CString::from_raw(ptr))
}

#[repr(C)]
pub struct EGraphIter {
    numnodes: u32,
    numclasses: u32,
    time: f64,
}

// a struct for loading rules from external source
#[repr(C)]
pub struct FFIRule {
    name: *const c_char,
    left: *const c_char,
    right: *const c_char,
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_expr(ptr: *mut Context, expr: *const c_char) -> u32 {
        let _ = env_logger::try_init();
        // Safety: `ptr` was box allocated by `egraph_create`
        let mut context = Box::from_raw(ptr);

        assert_eq!(context.iteration, 0);

	let result = unsafe {
	    // TODO
	    match CStr::from_ptr(expr).to_str().unwrap().parse() {
		Err(_) => 0 as u32,
		Ok(rec_expr) => {
                    context.runner = context.runner.with_expr(&rec_expr);
                    let id = *context.runner.roots.last().unwrap();
                    let id = usize::from(id) as u32;
                    assert!(id < u32::MAX);
                    id + 1 as u32
		}
	    }
	};

        mem::forget(context);

        result
}

unsafe fn ptr_to_string(ptr: *const c_char) -> String {
    let bytes = CStr::from_ptr(ptr).to_bytes();
    String::from_utf8(bytes.to_vec()).unwrap()
}

// todo don't just unwrap, also make sure the rules are validly parsed
unsafe fn ffirule_to_tuple(rule_ptr: *mut FFIRule) -> (String, String, String) {
    let rule = &mut *rule_ptr;
    (
        ptr_to_string(rule.name),
        ptr_to_string(rule.left),
        ptr_to_string(rule.right),
    )
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run_with_iter_limit(
    ptr: *mut Context,
    output_size: *mut u32,
    iter_limit: u32,
    node_limit: u32,
    rules_array_ptr: *const *mut FFIRule,
    is_constant_folding_enabled: bool,
    rules_array_length: u32,
) -> *const EGraphIter {
        // Safety: `ptr` was box allocated by `egraph_create`
        let mut context = Box::from_raw(ptr);

        if context.runner.stop_reason.is_none() {
            let length: usize = rules_array_length as usize;
            let ffi_rules: &[*mut FFIRule] = slice::from_raw_parts(rules_array_ptr, length);
            let mut ffi_tuples: Vec<(&str, &str, &str)> = vec![];
            let mut ffi_strings: Vec<(String, String, String)> = vec![];
            for ffi_rule in ffi_rules.iter() {
                let str_tuple = ffirule_to_tuple(*ffi_rule);
                ffi_strings.push(str_tuple);
            }

            for ffi_string in ffi_strings.iter() {
                ffi_tuples.push((&ffi_string.0, &ffi_string.1, &ffi_string.2));
            }

            let rules: Vec<Rewrite> = rules::mk_rules(&ffi_tuples);
            context.rules = rules;

            context.runner.egraph.analysis.constant_fold = is_constant_folding_enabled;
            context.runner = context
                .runner
                .with_node_limit(node_limit as usize)
                .with_iter_limit(iter_limit as usize) // should never hit
                .with_time_limit(Duration::from_secs(u64::MAX))
                .with_hook(|r| {
                    if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                        Err("Unsoundness detected".into())
                    } else {
                        Ok(())
                    }
                })
                .run(&context.rules);
        }

        std::ptr::write(output_size, context.runner.iterations.len() as u32);
        let iterations = context
            .runner
            .iterations
            .iter()
            .map(|iteration| EGraphIter {
                numnodes: iteration.egraph_nodes as u32,
                numclasses: iteration.egraph_classes as u32,
                time: iteration.total_time,
            })
            .collect::<Vec<_>>();
        let iterations = ManuallyDrop::new(iterations);

        mem::forget(context);

        iterations.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run(
    ptr: *mut Context,
    output_size: *mut u32,
    node_limit: u32,
    rules_array_ptr: *const *mut FFIRule,
    is_constant_folding_enabled: bool,
    rules_array_length: u32,
) -> *const EGraphIter {
    egraph_run_with_iter_limit(
        ptr,
        output_size,
        u32::MAX,
        node_limit,
        rules_array_ptr,
        is_constant_folding_enabled,
        rules_array_length,
    )
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_stop_reason(ptr: *mut Context) -> u32 {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));

        match context.runner.stop_reason {
            Some(StopReason::Saturated) => 0,
            Some(StopReason::IterationLimit(_)) => 1,
            Some(StopReason::NodeLimit(_)) => 2,
            Some(StopReason::Other(_)) => 3,
            _ => 4,
        }
}

fn find_extracted(runner: &Runner, id: u32, iter: u32) -> &Extracted {
    let id = runner.egraph.find(Id::from(id as usize));

    // go back one more iter, egg can duplicate the final iter in the case of an error
    let is_unsound = runner.egraph.analysis.unsound.load(Ordering::SeqCst);
    let sound_iter = min(
	runner.iterations.len().saturating_sub(if is_unsound { 3 } else { 1 }),
	iter as usize
    );

    runner.iterations[sound_iter]
        .data
        .extracted
        .iter()
        .find(|(i, _)| runner.egraph.find(*i) == id)
        .map(|(_, ext)| ext)
        .expect("Couldn't find matching extraction!")
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_simplest(
    ptr: *mut Context,
    node_id: u32,
    iter: u32,
) -> *const c_char {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));

        let ext = find_extracted(&context.runner, node_id, iter);

        let best_str = CString::new(ext.best.to_string()).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);

        best_str_pointer
}

unsafe fn make_empty_string() -> *const c_char {
    let best_str = CString::new("".to_string()).unwrap();
    let best_str_pointer = best_str.as_ptr();
    std::mem::forget(best_str);
    best_str_pointer
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_proof(
    ptr: *mut Context,
    expr: *const c_char,
    goal: *const c_char,
) -> *const c_char {
        // Safety: `ptr` was box allocated by `egraph_create`
        let mut context = ManuallyDrop::new(Box::from_raw(ptr));

        assert_eq!(context.iteration, 0);

        let expr_rec = match CStr::from_ptr(expr).to_str().map(str::parse) {
	    Ok(Ok(expr)) => expr,
	    // TODO
	    _ => return make_empty_string()
        };
        let goal_rec = match CStr::from_ptr(goal).to_str().map(str::parse) {
	    Ok(Ok(goal)) => goal,
	    _ => return make_empty_string()
        };

        let proof = context.runner.explain_equivalence(&expr_rec, &goal_rec);
        let string = CString::new(proof.get_string_with_let().replace('\n', "")).unwrap();
        let string_pointer = string.as_ptr();
        std::mem::forget(string);
        string_pointer
}

#[no_mangle]
pub unsafe extern "C" fn egraph_is_equal(
    ptr: *mut Context,
    expr: *const c_char,
    goal: *const c_char,
) -> bool {
        // Safety: `ptr` was box allocated by `egraph_create`
        let mut context = ManuallyDrop::new(Box::from_raw(ptr));

        assert_eq!(context.iteration, 0);

        let expr_rec = match CStr::from_ptr(expr).to_str().map(str::parse) {
            Ok(Ok(rec_expr)) => rec_expr,
            _ => return false
        };

        let goal_rec = match CStr::from_ptr(goal).to_str().map(str::parse) {
            Ok(Ok(rec_expr)) => rec_expr,
	    _ => return false
        };
        let egraph = &mut context.runner.egraph;

        egraph.add_expr(&expr_rec) == egraph.add_expr(&goal_rec)
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_variants(
    ptr: *mut Context,
    node_id: u32,
    orig_expr: *const c_char,
) -> *const c_char {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));

        // root (id, expr)
        let id = Id::from(node_id as usize);
        let orig_recexpr: RecExpr = CStr::from_ptr(orig_expr).to_str().unwrap().parse().unwrap();
        let head_node = &orig_recexpr.as_ref()[orig_recexpr.as_ref().len() - 1];

        // extractor
        let extractor = Extractor::new(&context.runner.egraph, AltCost::new(&context.runner.egraph));
        let mut cache: IndexMap<Id, RecExpr> = Default::default();

        // extract variants
        let mut exprs = vec![];
        for n in &context.runner.egraph[id].nodes {
            // assuming same ops in an eclass cannot
            // have different precisions
            if !n.matches(head_node) {
                // extract if not in cache
                n.for_each(|id| {
                    if cache.get(&id).is_none() {
                        let (_, best) = extractor.find_best(id);
                        cache.insert(id, best);
                    }
                });

                exprs.push(n.join_recexprs(|id| cache.get(&id).unwrap().as_ref()));
            }
        }

        // format
        let expr_strs: Vec<String> = exprs.iter().map(|r| r.to_string()).collect();
        let best_str = CString::new(expr_strs.join(" ")).unwrap();
        let best_str_pointer = best_str.as_ptr();
        std::mem::forget(best_str);
        best_str_pointer
}

#[no_mangle]
pub unsafe extern "C" fn egraph_is_unsound_detected(ptr: *mut Context) -> bool {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));

        context.runner.egraph.analysis.unsound.load(Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_times_applied(ptr: *mut Context, name: *const c_char) -> u32 {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));
        let sym = Symbol::from(ptr_to_string(name));

        context
	    .runner
            .iterations
            .iter()
            .map(|iter| *iter.applied.get(&sym).unwrap_or(&0) as u32)
            .sum()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_cost(ptr: *mut Context, node_id: u32, iter: u32) -> u32 {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));
        let ext = find_extracted(&context.runner, node_id, iter);

        ext.cost as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_size(ptr: *mut Context) -> u32 {
        // Safety: `ptr` was box allocated by `egraph_create`
        let context = ManuallyDrop::new(Box::from_raw(ptr));

        context
	    .runner
	    .iterations
	    .last()
	    .map(|iteration| iteration.egraph_nodes as u32)
	    .unwrap_or_default()
}
