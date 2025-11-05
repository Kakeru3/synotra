use crate::vm::VM;

/// 重い計算タスク: Fibonacci
pub fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

/// フィボナッチ数列の並列計算デモ
pub fn run_fibonacci_demo() {
    let mut vm: VM<u64> = VM::new();

    // タスク定義 (Fibonacci 35~40)
    for i in 35..=40 {
        vm.add_task(i as usize, vec![], move || fibonacci(i));
    }

    // タスク依存例: Fib(41) = Fib(39) + Fib(40)
    vm.add_task(
        41,
        vec![39, 40],
        move || fibonacci(39) + fibonacci(40),
    );

    // --- 並列 ---
    let start = std::time::Instant::now();
    let parallel_res = vm.run_parallel();
    let elapsed_parallel = start.elapsed();

    println!("--- Parallel Results ---");
    for i in 35..=41 {
        println!("Fib({}) = {}", i, parallel_res[&(i as usize)]);
    }
    println!("Parallel elapsed: {:.3?}", elapsed_parallel);

    // --- シングルスレッド ---
    let start = std::time::Instant::now();
    let single_res = vm.run_single();
    let elapsed_single = start.elapsed();

    println!("--- Single-thread Results ---");
    for i in 35..=41 {
        println!("Fib({}) = {}", i, single_res[&(i as usize)]);
    }
    println!("Single-thread elapsed: {:.3?}", elapsed_single);
}
