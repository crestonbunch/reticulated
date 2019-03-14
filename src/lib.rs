use std::collections::HashMap;

pub fn fib(n: u32) -> u64 {
    let mut memos = HashMap::new();

    fn step(n: u32, memos: &mut HashMap<u32, u64>) -> u64 {
        if n <= 1 {
            1
        } else {
            let left = {
                match memos.get(&(n - 2)) {
                    Some(v) => *v,
                    None => step(n - 2, memos),
                }
            };
            let right = {
                match memos.get(&(n - 1)) {
                    Some(v) => *v,
                    None => step(n - 1, memos),
                }
            };
            memos.insert(n - 2, left);
            memos.insert(n - 1, right);
            left + right
        }
    }

    step(n, &mut memos)
}

pub fn mean(nums: &Vec<i64>) -> f64 {
    let sum: i64 = nums.iter().sum();
    sum as f64 / nums.len() as f64
}

pub fn mode(nums: &Vec<i64>) -> i64 {
    let mut counts: HashMap<i64, u64> = HashMap::new();
    let mut max: Option<i64> = None;
    for n in nums.iter() {
        let count = counts.entry(*n).or_insert(0);
        *count += 1;

        max = match max {
            Some(m) => {
                if counts[n] > counts[&m] {
                    Some(*n)
                } else {
                    max
                }
            }
            None => Some(*n),
        }
    }

    match max {
        Some(m) => m,
        None => 0,
    }
}

pub fn median(nums: &Vec<i64>) -> f64 {
    let mut nums = nums.clone();
    nums.sort();
    if nums.len() % 2 == 0 {
        let lower = nums.len() / 2 - 1;
        let upper = nums.len() / 2;
        (nums[lower] + nums[upper]) as f64 / 2.0
    } else {
        nums[(nums.len() - 1) / 2] as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_counts_fibonacci() {
        assert_eq!(fib(0), 1);
        assert_eq!(fib(1), 1);
        assert_eq!(fib(2), 2);
        assert_eq!(fib(3), 3);
        assert_eq!(fib(4), 5);
        assert_eq!(fib(5), 8);
    }

    #[test]
    fn it_calculates_mean() {
        let nums = &vec![1, 2, 3, 4, 5];
        assert_eq!(mean(nums), 3.0);
    }

    #[test]
    fn it_calculates_median() {
        let nums = &vec![3, 2, 1, 5, 4];
        assert_eq!(median(nums), 3.0);
        let nums = &vec![3, 2, 1, 6, 5, 4];
        assert_eq!(median(nums), 3.5);
    }

    #[test]
    fn it_calculates_mode() {
        let nums = &vec![3, 2, 1, 5, 4, 4];
        assert_eq!(mode(nums), 4);
    }
}
