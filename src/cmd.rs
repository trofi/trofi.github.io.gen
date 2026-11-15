use std::io::Write;

use crate::result::Result;

pub(crate) fn run_cmd(cmd: &str, args: &[&str], input: &str) -> Result<Vec<u8>> {
    
    let mut child = std::process::Command::new(cmd)
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()?;

    // pipe comes from command construction
    let mut stdin = child.stdin.take()
        .ok_or(crate::error::BlogError::Logic("stdin pipe is missing"))?;
    {
        let input: String = input.to_string();
        let handler = std::thread::spawn(move || {
            // TODO: add thread communication error handling
            stdin.write_all(input.as_bytes()).expect("failed to write");
        });
        // TODO: add thread communication error handling
        handler.join().expect("failed to join")
    }
    
    let output = child.wait_with_output()?;

    for l in output
        .stderr
        .split(|c| *c == b'\n')
        .filter(|e| !e.is_empty())
    {
        println!("{cmd} stderr> {}", String::from_utf8_lossy(l));
    }

    if !output.status.success() {
        return Err(crate::error::BlogError::CommandFailed);
    }

    Ok(output.stdout)
}
