///Tests a cutoff implementation of a sub tree.
///Basically given a tree like
///```
///Union(){
///    Translate(){ SomePrim() }
///}{
///    SomeOtherPrim()
///}
///```
///
/// If we evaluate Color, and Translate.Color() does not have a evaluate, but always returns the same color.
///
/// Then we want that translated into our export. (Instead of bugs ^^).
#[test]
fn tree_cutoff() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/cutoff_tree.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn std_compiles() {
    //pretty simple. This should _always_ work.
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/std.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn multi_concept_impl() {
    //Tests the case that one entity implements multiple concepts,
    // and the _change_ from one concept into another on the tree
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/concept_change.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn unused_arg_define() {
    //Tests the case that one entity implements multiple concepts,
    // and the _change_ from one concept into another on the tree
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/unused_define_arg.vola")
        .unwrap();
    target.try_verify().unwrap();
}
