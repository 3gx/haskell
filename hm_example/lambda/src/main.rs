fn main() {
    use lambda::State as S;
    use lambda::*;
    use std::panic;
    println!("Hello, world!");
    println!("term_id= {x}", x = term_id());
    println!("term_int= {term_int}", term_int = term_int());
    println!("term_id_unit= {}", term_id_unit());
    println!("term_higher= {}", term_higher());
    println!("term_occurs= {}", term_occurs());

    println!("type({})= {}", term_id(), S::run_infer(&term_id()));
    println!("type({})= {}", term_int(), S::run_infer(&term_int()));
    println!(
        "type({})= {}",
        term_id_unit(),
        S::run_infer(&term_id_unit())
    );
    println!("type({})= {}", term_higher(), S::run_infer(&term_higher()));

    let result = panic::catch_unwind(|| {
        println!("type({})= {}", term_occurs(), S::run_infer(&term_occurs()));
    });
    println!("result= {:?}", result);
    println!("OK");
}
