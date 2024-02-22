use rvsdg::region::{Input, Output};
use rvsdg_derive_lang::LangNode;

use rvsdg::nodes::LangNode;

#[test]
fn derive_lang_node_vec() {
    #[derive(LangNode)]
    pub struct VecNode {
        #[inputs]
        inp: Vec<Input>,
        #[outputs]
        outp: Vec<Output>,
    }

    let mut node = VecNode {
        inp: Vec::new(),
        outp: Vec::new(),
    };

    node.inp.push(Input { edge: None });
    node.inp.push(Input { edge: None });

    node.outp.push(Output::default());
    node.outp.push(Output::default());
    node.outp.push(Output::default());

    assert!(node.inputs().len() == 2);
    assert!(node.outputs().len() == 3);
}

#[test]
fn derive_lang_node_single() {
    #[derive(LangNode)]
    pub struct VecNode {
        #[input]
        inp: Input,
        #[output]
        outp: Output,
    }

    let node = VecNode {
        inp: Input::default(),
        outp: Output::default(),
    };

    assert!(node.inputs().len() == 1);
    assert!(node.outputs().len() == 1);
}

#[test]
fn derive_lang_node_mixed() {
    #[derive(LangNode)]
    pub struct VecNode {
        #[input]
        inp: Input,
        #[outputs]
        outp: Vec<Output>,
    }

    let node = VecNode {
        inp: Input::default(),
        outp: vec![Output::default(); 2],
    };

    assert!(node.inputs().len() == 1);
    assert!(node.outputs().len() == 2);

    #[derive(LangNode)]
    pub struct VecNode2 {
        #[inputs]
        inp: Vec<Input>,
        #[output]
        outp: Output,
    }

    let node = VecNode2 {
        inp: vec![Input::default(); 3],
        outp: Output::default(),
    };

    assert!(node.inputs().len() == 3);
    assert!(node.outputs().len() == 1);
}

#[test]
fn derive_lang_node_expose() {
    #[derive(LangNode)]
    struct ExposeContainer {
        #[input]
        inp: Input,
        #[output]
        outp: Output,
    }

    #[derive(LangNode)]
    pub struct VecNode {
        #[expose]
        exp: ExposeContainer,
        #[allow(unused)]
        payload: String,
    }

    let node = VecNode {
        exp: ExposeContainer {
            inp: Input::default(),
            outp: Output::default(),
        },
        payload: "teddy!".to_owned(),
    };

    assert!(node.inputs().len() == 1);
    assert!(node.outputs().len() == 1);
}

#[test]
fn derive_lang_node_no_input() {
    #[derive(LangNode)]
    struct ExposeContainer {
        #[output]
        outp: Output,
    }

    #[derive(LangNode)]
    pub struct VecNode {
        #[expose]
        exp: ExposeContainer,
        #[allow(unused)]
        payload: String,
    }

    let node = VecNode {
        exp: ExposeContainer {
            outp: Output::default(),
        },
        payload: "teddy!".to_owned(),
    };

    assert!(node.inputs().len() == 0);
    assert!(node.outputs().len() == 1);
}
