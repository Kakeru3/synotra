use crate::vm::VM;

/// 素数判定（重い計算）
pub fn is_prime(n: u64) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }

    let limit = (n as f64).sqrt() as u64;
    for i in (3..=limit).step_by(2) {
        if n % i == 0 {
            return false;
        }
    }
    true
}

/// 指定範囲内の素数の個数を数える
pub fn count_primes_in_range(start: u64, end: u64) -> u64 {
    (start..=end).filter(|&n| is_prime(n)).count() as u64
}

/// 素数カウントの並列計算デモ
pub fn run_prime_demo() {
    let mut vm: VM<u64> = VM::new();

    // タスク定義: 各範囲の素数をカウント
    // 1,000,000 から 2,000,000 までを10個の範囲に分割
    let base = 1_000_000;
    let range_size = 100_000;

    for i in 0..10 {
        let start = base + i * range_size;
        let end = start + range_size - 1;
        vm.add_task(i as usize, vec![], move || {
            count_primes_in_range(start, end)
        });
    }

    // 依存タスク: すべての範囲の結果を合計
    vm.add_task(10, vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9], move || {
        // このタスクは依存しているだけで、実際の合計は外で計算
        0
    });

    println!("=== Prime Counting Demo ===");
    println!(
        "Counting primes in range: {}-{}",
        base,
        base + range_size * 10
    );
    println!();

    // --- 並列 ---
    let start = std::time::Instant::now();
    let parallel_res = vm.run_parallel();
    let elapsed_parallel = start.elapsed();

    println!("--- Parallel Results ---");
    let mut total_parallel = 0u64;
    for i in 0..10 {
        let count = parallel_res[&i];
        let range_start = base + i as u64 * range_size;
        let range_end = range_start + range_size - 1;
        println!("Range [{}-{}]: {} primes", range_start, range_end, count);
        total_parallel += count;
    }
    println!("Total primes: {}", total_parallel);
    println!("Parallel elapsed: {:.3?}", elapsed_parallel);
    println!();

    // --- シングルスレッド ---
    let start = std::time::Instant::now();
    let single_res = vm.run_single();
    let elapsed_single = start.elapsed();

    println!("--- Single-thread Results ---");
    let mut total_single = 0u64;
    for i in 0..10 {
        let count = single_res[&i];
        let range_start = base + i as u64 * range_size;
        let range_end = range_start + range_size - 1;
        println!("Range [{}-{}]: {} primes", range_start, range_end, count);
        total_single += count;
    }
    println!("Total primes: {}", total_single);
    println!("Single-thread elapsed: {:.3?}", elapsed_single);
    println!();

    // --- 比較 ---
    println!("--- Performance Comparison ---");
    println!("Parallel:      {:.3?}", elapsed_parallel);
    println!("Single-thread: {:.3?}", elapsed_single);
    let speedup = elapsed_single.as_secs_f64() / elapsed_parallel.as_secs_f64();
    println!("Speedup:       {:.2}x", speedup);
}
