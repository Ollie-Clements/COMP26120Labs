package comp26120;
import java.io.FileWriter;
import java.io.IOException;

public class hashset implements set<String> {
    int verbose;
    HashingModes mode;

    speller_config config;

    cell[] cells;
    int size;
    int num_entries; // number of cells in_use

    // TODO add any other fields that you need
    int numinserts;
    int numcollisions;
    int numrehashes;
    float totaltimetakeninsert;
    float totaltimetakenfind;


    // This is  a cell structure assuming Open Addressing
    // You wil need alternative data-structures for separate chaining
    public class cell { // hash-table entry
	String element; // only data is the key itself
	state state;
    }

    public static enum state {
        empty,
        in_use,
        deleted;
    }

    public hashset(speller_config config) {
	verbose = config.verbose;
	mode = HashingModes.getHashingMode(config.mode);

	// TODO: create initial hash table
	if(isPrime(config.init_size)){
        size = config.init_size;
    }else{
        size = nextPrime(config.init_size);
    }
    cells = new cell[size];
    for(int i = 0; i<size ; i++){
        cells[i] = new cell();
    }
    num_entries = 0;
    }

    // Helper functions for finding prime numbers 
    public boolean isPrime (int n)
    {
	for (int i = 2; i*i <= n; i++)
	    if (n % i == 0)
		return false;
	return true;
    }

    public int nextPrime(int n)
    {
	int i = n;
	while (!isPrime(i)) {
	    i++;
	}
	return i;
    }

    public void insert (String value) 
    {
	// TODO code for inserting into hash table
        float startime = System.nanoTime();
        int tableindex;
        switch(mode){
            case HASH_2_LINEAR_PROBING:
                tableindex = hash2(value);
                linearInsert(tableindex, value);
                break;
            default:
                tableindex = hash1(value);
                linearInsert(tableindex, value);
                break;
        }
        float endtime = System.nanoTime();
        totaltimetakeninsert += (endtime - startime);
    }

    public boolean find (String value)
    {
	// TODO code for looking up in hash table
        float startime = System.nanoTime();
        int tableindex;
        switch(mode){
            case HASH_2_LINEAR_PROBING:
                tableindex = hash2(value);
                if(linearSearch(tableindex, value)){
                    float endtime = System.nanoTime();
                    totaltimetakenfind += (endtime - startime);
                    return true;
                }break;
            default:
                tableindex = hash1(value);
                if(linearSearch(tableindex, value)){
                    float endtime = System.nanoTime();
                    totaltimetakenfind += (endtime - startime);
                    return true;
                }break;    
        }float endtime = System.nanoTime();
        totaltimetakenfind += (endtime - startime);
        return false;

    }

    public void print_set ()
    {
    // TODO code for printing hash table
        for (cell c : cells) {
            if(c.state == state.in_use){
                System.out.println(c.element);
            }
        }
    }

    public void print_stats ()
    {
	// TODO code for printing statistic
        System.out.println("Number of Inserts: " + numinserts);
        System.out.println("number of Collisions: " + numcollisions);
        System.out.println("Number of Rehashes: " + numrehashes);
        System.out.println("Time Taken: " + totaltimetakeninsert);

        float collisionsperinsert = numcollisions / (float)numinserts;
        float timeperinsert = totaltimetakeninsert  / (float)numinserts;

        try{
            FileWriter fw = new FileWriter("data.csv", true);
            fw.write("\n" + num_entries + ", " + collisionsperinsert + ", " + timeperinsert + ", " + numinserts + ", " + numrehashes);
            fw.close();
        }catch(IOException e) {
            e.printStackTrace();
        }
    }

    // Hashing Modes

    public enum HashingModes {
	HASH_1_LINEAR_PROBING, // =0 in mode flag
        HASH_1_QUADRATIC_PROBING, // =1, 
        HASH_1_DOUBLE_HASHING, //=2, 
        HASH_1_SEPARATE_CHAINING, // =3,
        HASH_2_LINEAR_PROBING, // =4, 
        HASH_2_QUADRATIC_PROBING, // =5, 
        HASH_2_DOUBLE_HASHING, // =6, 
        HASH_2_SEPARATE_CHAINING; // =7

	public static HashingModes getHashingMode(int i) {
	    switch (i) {
	    case 1:
		return HASH_1_QUADRATIC_PROBING;
	    case 2:
		return HASH_1_DOUBLE_HASHING;
	    case 3:
		return HASH_1_SEPARATE_CHAINING;
	    case 4:
		return HASH_2_LINEAR_PROBING;
	    case 5:
		return HASH_2_QUADRATIC_PROBING;
	    case 6:
		return HASH_2_DOUBLE_HASHING;
	    case 7:
		return HASH_2_SEPARATE_CHAINING;
	    default:
		return HASH_1_LINEAR_PROBING;
	    }
	}
    }

    // Your code
    
    private int hash1(String value){
        int hash = 0;
        for (int c=0; c<value.length(); c++){
            hash = hash * 31 + value.charAt(c);
        }
        return hash;
    }

    private int hash2(String value){
        int hash = 0; 
        if(value.length() > 0){
            hash += value.charAt(0);
        }
        if(value.length() > 1){
            hash += value.charAt(1);
        }
        if(value.length() > 4){
            hash += value.charAt(4);
        }
        hash = (hash * hash) - (value.length() * 2);
        return hash;
    }

    private void linearInsert(int tableindex, String value){
        tableindex = tableindex % size;
        tableindex = Math.abs(tableindex);
        boolean insert = false;
        numinserts++;
        while(!insert){
            if(cells[tableindex].state != state.in_use){
                cells[tableindex].element = value;
                cells[tableindex].state = state.in_use;
                insert = true;
                num_entries++;
                if(num_entries > (2*size/3)){
                    rehash();
                    numrehashes++;
                }
            }else if(cells[tableindex].element.equals(value)){
                break;
            }else{
                tableindex++;
                numcollisions++;
                if(tableindex >= size){
                    tableindex = 0;
                }
            }

        }
    }       

    private boolean linearSearch(int tableindex, String value){
        tableindex = tableindex % size;
        tableindex = Math.abs(tableindex);
        boolean search = false;
        while(!search){
            if(cells[tableindex].state == state.in_use){
                if(cells[tableindex].element.equals(value)){
                    return true;
                }else{
                    tableindex++;
                    if(tableindex >= size){
                        tableindex = 0;
                    }          
                }
            }else{
                return false;
            }
        }
        return false;
    }

    private void rehash(){
        String[] copyValues = new String[num_entries];
        int copyValuesCount = 0;
        for(int a=0; a<size; a++){
            if(cells[a].state == state.in_use){
                copyValues[copyValuesCount] = cells[a].element;
                copyValuesCount++;
            }
        }size = nextPrime(size*2);
        cells = new cell[size];
        for (int b=0; b<size ; b++){
            cells[b] = new cell();
        }num_entries = 0;
        for(int c=0; c<copyValuesCount; c++){
            insert(copyValues[c]);
        }
    }
}
