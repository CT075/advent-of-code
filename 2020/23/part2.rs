// The Haskell solution is basically correct, but OOMed my box and I don't feel
// up to hunting down all the places I need to add strictness annotations.

fn main() {
    let mut next: [usize; 1000001] = [0; 1000001];

    next[5] = 6;
    next[6] = 2;
    next[2] = 8;
    next[8] = 9;
    next[9] = 3;
    next[3] = 1;
    next[1] = 4;
    next[4] = 7;
    next[7] = 10;

    next[1_000_000] = 5;

    /*
    next[3] = 8;
    next[8] = 9;
    next[9] = 1;
    next[1] = 2;
    next[2] = 5;
    next[5] = 4;
    next[4] = 6;
    next[6] = 7;
    next[7] = 10;

    next[1_000_000] = 3;
    */

    for i in 10..1_000_000 {
        next[i] = i + 1;
    }

    let mut sel = 5;

    for _ in 0..10_000_000 {
        let x1 = next[sel];
        let x2 = next[x1];
        let x3 = next[x2];

        next[sel] = next[x3];

        let mut dst = sel - 1;
        loop {
            if dst < 1 {
                dst = 1_000_000;
            }

            if dst != x1 && dst != x2 && dst != x3 {
                break;
            }

            dst = dst - 1;
        }

        next[x3] = next[dst];
        next[dst] = x1;

        sel = next[sel];
    }

    println!(
        "1 -> {} -> {}; answer is {}",
        next[1],
        next[next[1]],
        next[1] * next[next[1]]
    );
}
