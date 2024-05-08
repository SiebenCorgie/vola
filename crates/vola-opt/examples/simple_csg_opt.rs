/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

pub fn main() {
    pretty_env_logger::init();
    /*NOTE: Using the cli intead
    let ast = match vola_ast("crates/vola-ast/examples/csg.vola") {
        Ok(ast) => ast,
        Err((partial_ast, _errors_reporter)) => {
            println!("\nGot {} entries in toplevel!", partial_ast.entries.len());
            return;
        }
    };

    let mut opt = Optimizer::new();
    if opt.add_ast(ast).is_err() {
        println!("Opt did not parse AST successfully :(");
    }

    opt.dump_svg("before_type_resolution.svg", true);

    let tyder_start = Instant::now();
    if let Err(e) = opt.type_derive() {
        println!("Type derive failed: {e}");
    }
    println!(
        "Type derive pass took {}ms / {}ns",
        tyder_start.elapsed().as_millis(),
        tyder_start.elapsed().as_nanos()
    );

    opt.dump_svg("after_type_resolution.svg", true);

    let disp_start = Instant::now();
    if let Err(_e) = opt.dispatch_all_exports() {
        println!("Dispatching exports failed!");
    }
    println!(
        "Dispatch pass took {}ms / {}ns",
        disp_start.elapsed().as_millis(),
        disp_start.elapsed().as_nanos()
    );

    opt.dump_svg("after_dispatch.svg", true);
    */
}
