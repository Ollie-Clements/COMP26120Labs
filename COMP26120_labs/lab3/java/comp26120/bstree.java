package comp26120;
import java.io.FileWriter;
import java.io.IOException;

public class bstree implements set<String> {
    int verbose;

    String value;
    bstree left;
    bstree right;

    speller_config config;

    // TODO add fields for statistics
    int insert_failures;
    int find_successes;
    int typos;
    int numinserts;
    int numcomparisons;
    float totaltimetaken;

    public bstree(speller_config config) {
	verbose = config.verbose;
	this.config = config;
    }

	
    public int size(){
	// This presumes that if value is not null then (possibly empty) left and right trees exist.
	if(tree()){
	    return 1 + left.size() + right.size();
	}
	return 0;
    } 

    public void insert (String value) 
    {
    float startime = System.nanoTime();
	if(tree()){
	    // TODO if tree is not NULL then insert into the correct sub-tree
        if (value.compareTo(this.value) < 0){
            left.insert(value);
            numinserts = left.numinserts;
            numcomparisons = left.numcomparisons;
            numcomparisons++;
        } else if (value.compareTo(this.value) > 0){
            right.insert(value);
            numinserts = right.numinserts;
            numcomparisons = right.numcomparisons;
            numcomparisons++;
        } 
	}
	else{
	    // TODO otherwise create a new node containing the value and two sub-trees.
        this.value = value;
        left = new bstree(config);
        right = new bstree(config);
        find_successes = 0;
        numinserts++;
	}
    float endtime = System.nanoTime();
    totaltimetaken += (endtime - startime);
    }

    public boolean find (String value)
    {
	if(tree()){
	    //TODO complete the find function
        if (value.compareTo(this.value) < 0){
            if (left.find(value)){
                this.find_successes++;
                return true;
            } else {
                this.typos++;
                return false;
            }
        } else if (value.compareTo(this.value) > 0){
            if (right.find(value)){
                this.find_successes++;
                return true;
            } else {
                this.typos++;
                return false;
            }
        } else {
            this.find_successes++;
            return true;
        }
	}
	// if tree is NULL then it contains no values
    typos++;
	return false;
    }

    private boolean tree() {
	return (value != null);
    }

    // You can update this if you want
    public void print_set_recursive(int depth)
    {
	if(tree()){
	    for(int i=0;i<depth;i++){ System.out.print(" "); }
	    System.out.format("%s\n",value);
	    left.print_set_recursive(depth+1);
	    right.print_set_recursive(depth+1);
	}
    } 

    // You can update this if you want
    public void print_set ()
    {
	System.out.print("Tree:\n");
	print_set_recursive(0);
    }

    public void print_stats ()
    {
	// TODO update code to record and print statistics
        System.out.println("Failed Inserts: " + insert_failures);
        System.out.println("Successful Finds: " + find_successes);
        System.out.println("Typos: " + typos);

        System.out.println("Number of Inserts: " + numinserts);
        System.out.println("number of Comparisons: " + numcomparisons);
        System.out.println("Time Taken: " + totaltimetaken);

        float comparisonsperinsert = numcomparisons / (float)numinserts;
        float timeperinsert = totaltimetaken / (float)numinserts;

        try{
            FileWriter fw = new FileWriter("data2.csv", true);
            fw.write("\n" + size() + ", " + comparisonsperinsert + ", " + timeperinsert + ", " + numinserts);
            fw.close();
        }catch(IOException e) {
            e.printStackTrace();
        }
    }
}
