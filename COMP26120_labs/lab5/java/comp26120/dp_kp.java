package comp26120;

import java.util.ArrayList;

public class dp_kp extends KnapSack {
    public dp_kp(String filename) {
	super(filename);
    }

    public void DP(ArrayList<Boolean> solution) {
	ArrayList<Integer> v = item_values;
	ArrayList<Integer> wv = item_weights;
	int n = Nitems;
	int W = Capacity;
	
	// the dynamic programming function for the knapsack problem
	// the code was adapted from p17 of http://www.es.ele.tue.nl/education/5MC10/solutions/knapsack.pdf

	// v array holds the values / profits / benefits of the items
	// wv array holds the sizes / weights of the items
	// n is the total number of items
	// W is the constraint (the weight capacity of the knapsack)
	// solution: a 1 in position n means pack item number n+1. A zero means do not pack it.

	ArrayList<ArrayList<Integer>> V = new ArrayList<ArrayList<Integer>>(n + 1);
	ArrayList<ArrayList<Boolean>> keep = new ArrayList<ArrayList<Boolean>>(n + 1);;// 2d arrays for use in the dynamic programming solution
	// keep[][] and V[][] are both of size (n+1)*(W+1)

	int i, w, K;

	// Initialise V and keep with null objects
	/* ADD CODE HERE */

	for(int a=0; a<V.size(); a++){
		V.add(new ArrayList<Integer>(n + 1));
		for(int b=0; b<V.size(); b++){
			V.get(a).set(b, null);
		}
	}

	for(int c=0; c<V.size(); c++){
		V.add(new ArrayList<Integer>(n + 1));
		for(int d=0; d<V.size(); d++){
			V.get(c).set(d, null);
		}
	}

	//  set the values of the zeroth row of the partial solutions table to zero
	/* ADD CODE HERE */

	for(int e=0; e<V.size(); e++){
		V.get(e).set(0, null);
	}

	// main dynamic programming loops , adding one item at a time and looping through weights from 0 to W
	/* ADD CODE HERE */

	KnapSackDP(int[] v, int[] w, int n, int W){
		for(int x=1; x<=n; x++){
			for(int y=0; y<=W; y++){
				if(w[x] <= w && ){

				}
			}
		}
	}

	// KnapSack(v, w, N, C) {
	// 		for (i = 1 to N)
	// 			for (w = 0 to C)
	// 				if (w[i] <= w) and (v[i] + V[i - 1][w - w[i]] > V[i - 1, w]) {
	// 					V[i][w] = v[i] + V[i - 1][w - w[i]]
	// 					keep[i][w] = 1
	// 				} else {
	// 					V[i][w] = V[i - 1][w]
	// 					keep[i][w] = 0
	// 				}
	// 		K = C
	// 		for (i = N downto 1)
	// 			if (keep[i][K] == 1) {
	// 				output i
	// 				K = K - w[i]
	// 			}
	// 		return V[N, C]
	// }
	// int KnapSack(int[] w, int[] v, int n, int W) {
	// 	if (n <= 0 || W <= 0) {
	// 		return 0;
	// 	}

	// 	int[][] m = new int[n + 1][W + 1];
	// 	for (int j = 0; j <= W; j++) {
	// 		m[0][j] = 0;
	// 	}

	// 	for (int i = 1; i <= n; i++) {
	// 		for (int j = 1; j <= W; j++) { 
	// 			if (w[i - 1] > j) {
	// 				m[i][j] = m[i - 1][j];
	// 			} else {
	// 				m[i][j] = Math.max(
	// 				m[i - 1][j], 
	// 				m[i - 1][j - w[i - 1]] + v[i - 1]);
	// 			}
	// 		}
	// 	}
	// 	return m[n][W];
	// }

	// now discover which items were in the optimal solution
	/* ADD CODE HERE */
	}

    public static void main(String[] args) {
	ArrayList<Boolean> solution;

	dp_kp knapsack = new dp_kp(args[0]);
	solution = new ArrayList<Boolean>(knapsack.Nitems+1);
	solution.add(null); // C implementation has null first object

	knapsack.DP(solution);
	knapsack.check_evaluate_and_print_sol(solution);
    }
}
