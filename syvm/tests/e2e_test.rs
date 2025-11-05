use std::path::Path;
use std::process::Command;

#[test]
fn test_example_test_syi_runs_successfully() {
    let syvm_bin = if cfg!(debug_assertions) {
        "target/debug/syvm"
    } else {
        "target/release/syvm"
    };
    let input_path = Path::new("../example/test.syi");
    assert!(input_path.exists(), "test.syi not found");

    let output = Command::new(syvm_bin)
        .arg(input_path)
        .output()
        .expect("Failed to run syvm");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let all_output = format!("{}\n{}", stdout, stderr);

    // 主要な出力が含まれているか確認
    assert!(all_output.contains("Fib(10) = 55"), "Fib output missing");
    assert!(all_output.contains("Calc(15) = 173"), "Calc output missing");
    assert!(all_output.contains("FizzBuzz"), "FizzBuzz output missing");
    assert!(
        !all_output.contains("Undefined variable"),
        "Unexpected error in output"
    );
    assert!(
        !all_output.contains("Stray statement"),
        "Unexpected parse error in output"
    );
}
