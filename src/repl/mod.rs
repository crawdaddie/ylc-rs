use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

pub fn repl<F>(cb: F) -> Result<()>
where
    F: Fn(String) -> (),
{
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    println!(
        "\x1b[0;31m
YLC LANG REPL       
--------------------
version 0.0.0       
\x1b[0m"
    );

    loop {
        let readline = rl.readline("\x1b[1;31mλ \x1b[1;0m");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                // println!("Line: {}", line);
                cb(line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}
