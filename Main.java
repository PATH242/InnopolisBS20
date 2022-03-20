/**
 * @author Mennatullah Awadallah
 * This code creates a simulation of the following scenario:
 * Harry needs to find an important book that's in an unknown location in a room and go to the exit with the known location
 * Harry lost his invisiblity cloak in the library, it can help him hide from inspectors unless he collides with them
 * There are two inspectors looking for him, one has a radius of 1 of perception, and one has a radius of 2 of perception
 * harry can sense perception zones and he has to avoid them or he dies
 * The goal of the code is to benchmark two algorithms in their pursuit of finding the shortest path harry can take to win
 * the two algorithms used are: backtracking and A star
 */
package com.PATH;
import java.util.ArrayList;
import java.util.PriorityQueue;
import java.util.Scanner;
import java.util.concurrent.ThreadLocalRandom;
import static java.lang.Math.abs;

/**
 * a class to make a pair of two data types
 * @param <A> this is a generic type: it can be a class, primary data type, or a wrapper class
 * @param <B>this is a generic type: it can be a class, primary data type, or a wrapper class
 */
class pair<A,B> implements Comparable<pair<Integer,Integer>>{
    public A a;
    public B b;

    /**
     * constructor that creates a pair knowing their intial values
     * @param a
     * @param b
     */
    pair(A a, B b){
        this.a = a;
        this.b = b;
    }

    /**
     * a constructor used to equate two pairs with each other ( variable x = variable y)
     * @param temp
     */
    pair( pair<A,B> temp ) {
        this.a = temp.a;
        this.b = temp.b;
    }

    /**
     * checks if two pairs are equivalent in value
     * @param temp
     * @return
     */
    boolean compare(pair<A,B> temp){
        if(this.a == temp.a && this.b == temp.b)
            return true;
        return false;
    }

    /**
     *compares two pairs with each other based on the first paramater only
     * @param t
     * @return
     */
    @Override
    public int compareTo(pair<Integer,Integer> t) {

        int ans = Integer.compare((Integer) this.a, t.a);
        return ans;
    }
}

/**
 * enum containing type of cell
 */
enum Type{
    NORRIS, FILSH, EXIT, BOOK, PERCEPTION, EMPTY
}

/**
 * Cell represents a cell in a grid containing an actor or an object
 */
class Cell{
    public Type type;
    public boolean visited;
    public boolean path;
    public int dist;
    public boolean hasCloak;

    /**
     * constructor that sets initial values of parameters
     */
    Cell(){
        visited = false;
        hasCloak = false;
        type = Type.EMPTY;
        path = false;
    }

    /**
     * a constructor that sets values of cell equal to another cell
     * @param cell
     */
    Cell(Cell cell){
        type = cell.type;
        path = false;
        visited = cell.visited;
        hasCloak = cell.hasCloak;
    }
}

/**
 * Map class is for the map containing cells that represent our world/room
 */
class Map{
    Cell[][] map;

    /**
     * a constructor that creates cells for the map with initial values
     */
    Map(){
        map = new Cell[9][9];
        for(int i=0;i<9;i++){
            for(int j=0;j<9;j++){
                map[i][j] = new Cell();
            }
        }
    }

    /**
     * a  constructor that sets values of map equal to another map
     * @param someMap
     */
    Map(Map someMap){
        map = new Cell[9][9];
        for(int i=0;i<9;i++){
            for(int j=0;j<9;j++){
                map[i][j] = new Cell(someMap.map[i][j]);
            }
        }
    }

    /**
     * prints full map using certain visuals directly to the console
     */
    void printMap(){
        String RED = "\u001B[31m";
        String RESET = "\u001B[0m";
        String YELLOW = "\u001B[33m";
        String BLUE = "\u001B[34m";

        System.out.println("for mrs Norris: N, Argus Filch: F, Exit: X, Book: B, Perception zone: P, Invisibility cloak: I, empty cell: E");
        String[] output = new String[]{"N", "F", "X", "B", "P", "E"};
        for(int i=0; i<9; i++) {
            for (int j = 0; j < 9; j++) {
                Type type = map[i][j].type;
                if(type.ordinal() < 2)
                    System.out.print(RED + output[type.ordinal()] + RESET);
                else
                if(type.ordinal() < 4) {
                    System.out.print(BLUE + output[type.ordinal()] + RESET);
                }
                else
                if(type.ordinal() == 4){
                    System.out.print(YELLOW + output[type.ordinal()] + RESET);
                }
                else
                if(map[i][j].hasCloak)
                    System.out.print(BLUE + "I" + RESET);
                else
                    System.out.print(output[5]);
                System.out.print('\t');
            }
            System.out.println();
        }

    }

    /**
     * prints full map using certain visuals direcrly to the console
     * in additon to highlighting the path
     */
    void printPath(){
        String PURPLE = "\u001B[35m";
        String RED = "\u001B[31m";
        String RESET = "\u001B[0m";
        String YELLOW = "\u001B[33m";
        String BLUE = "\u001B[34m";

        System.out.println("for mrs Norris: N, Argus Filch: F, Exit: X, Book: B, Perception zone: P, Invisibility cloak: I, empty cell: E");
        System.out.println("Path will be highlighted purple");
        String[] output = new String[]{"N", "F", "X", "B", "P", "E"};
        for(int i=0; i<9; i++) {
            for (int j = 0; j < 9; j++) {
                Type tp = map[i][j].type;
                if(tp.ordinal() < 2)
                    System.out.print(RED + output[tp.ordinal()] + RESET);
                else
                if(tp.ordinal() < 4 && map[i][j].path != true) {
                    System.out.print(BLUE + output[tp.ordinal()] + RESET);
                }
                else
                if(tp.ordinal() == 4){
                    System.out.print(YELLOW + output[tp.ordinal()] + RESET);
                }
                else
                if(map[i][j].hasCloak){
                    if(map[i][j].path){
                        System.out.print(PURPLE + "I" + RESET);
                    }
                    else
                        System.out.print("I");
                }
                else
                if(map[i][j].path == true){
                    System.out.print(PURPLE + output[tp.ordinal()] + RESET);
                }
                else
                    System.out.print(output[5]);
                System.out.print('\t');
            }
            System.out.println();
        }

    }

}

/**
 * Algorithms contains and runs two algorithms: backtracking and A star
 * it also outputs their results
 */
class Algorithms{
    Map currentMap,backtrackingMap,aStarMap;
    int variant;
    pair<Integer,Integer> harry, exit,Book;
    int[] addX={0,0,1,1,1,-1,-1,-1},addY={-1,1,-1,0,1,-1,0,1};
    boolean cloak ;
    int[][][] historyd = new int[9][9][2], historyr = new int[9][9][2];

    /**
     * constructor responsible for running algorithms with certain initial conditions
     * @param currentMap is the map created by our simulation
     * @param variant represents the variant entered by user
     * @param harry is harry's location, which is a known parameter at the start of the simulation
     * @param exit is the exit's location, which is a known parameter at the start of the simulation
     */
    Algorithms(Map currentMap, int variant, pair<Integer,Integer> harry, pair<Integer,Integer> exit){
        this.variant = variant;
        this.harry = new pair<Integer, Integer>(harry);
        this.exit = new pair<Integer, Integer>(exit);
        this.currentMap = currentMap;
        backtrackingMap = new Map(currentMap);
        backtrackingManager();
        aStarMap = new Map(currentMap);
        aStar();
        cloak = false;
    }

    /**
     * This runs A Star algorithm on our current map to get the shortest path from start to exit passing by book
     * It gets the value of shortest path, then gets the path it took, and then outputs the results
     */
    void aStar() {
        long start = System.nanoTime();
        int distance = -1;
        int[][] parent = new int[9][9], vis = new int[9][9];
        boolean[][] bookF = new boolean[9][9], cloakF = new boolean[9][9];
        int temp = getBookForAstar();
        Book = new pair<Integer, Integer>((temp%9),(temp/9));
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                parent[i][j] = -1;
                vis[i][j] = 600;
                bookF[i][j] = false;
                cloakF[i][j] = false;
            }
        }


        PriorityQueue<pair<Integer, Integer>> q = new PriorityQueue<pair<Integer, Integer>>(81);
        q.add(new pair<Integer, Integer>(heurisitic(0, 0, false), 0));
        while (!q.isEmpty()) {
            int x = (q.peek().b % 9), y = (q.peek().b / 9), f = q.peek().a, d = f - heurisitic(x, y, bookF[x][y]);
            q.poll();
            if (f > vis[x][y]) {
                continue;
            }
           // System.out.println(x + " " + y + " " + f + " " + d + parent[x][y]%9 +" " + parent[x][y]/9);
            if (aStarMap.map[x][y].type == Type.BOOK) {
                bookF[x][y] = true;

            }
            if (aStarMap.map[x][y].hasCloak) {
                cloakF[x][y] = true;
            }
            if (x == exit.a && y == exit.b && bookF[x][y]) {
                break;
            }

            int newx, newy;
            for (int i = 0; i < 8; i++) {
                newx = x + addX[i];
                newy = y + addY[i];
                if (isValid(newx, newy, cloakF[x][y]) && vis[newx][newy] > d + 1 + heurisitic(newx, newy, bookF[x][y])) {
                    q.add(new pair<Integer, Integer>((d + 1 + heurisitic(newx, newy, bookF[x][y])), newx + (newy * 9)));
                    parent[newx][newy] = x + (y * 9);
                    vis[newx][newy] = d + 1 + heurisitic(newx, newy, bookF[x][y]);
                    bookF[newx][newy] = bookF[x][y];
                    cloakF[newx][newy] = cloakF[x][y];
                }
            }
        }
        long finish = System.nanoTime();
        double timeElapsed = finish - start;
        timeElapsed/= 1000000;
        ///getting path:
        int x = exit.a ,y = exit.b;
        boolean need = true;
        while(true){
            aStarMap.map[x][y].path = true;
            if(aStarMap.map[x][y].type == Type.BOOK){
                need = false;
            }
            distance ++;
            if(x == 0 && y == 0){
                break;
            }
            temp = parent[x][y];
            x= (temp%9);
            y= (temp/9);
        }
        x= Book.a; y = Book.b;
        int tempDist = 0;

        while(need){
            aStarMap.map[x][y].path = true;
            tempDist ++ ;
            if(parent[x][y] == -1){
                break;
            }
            temp = parent[x][y];
            x= (temp%9);
            y= (temp/9);
            if(x == 0 && y == 0){
                break;
            }
            if(x== exit.a && y == exit.b){
                distance += (tempDist*2-1);
                break;
            }

        }
        if(vis[exit.a][exit.b] < 600 && vis[Book.a][Book.b] < 600){
            System.out.println("A star algorithm solution:");
            System.out.println("It took us " + distance + " steps to reach goal");
            System.out.println("Time taken: " + timeElapsed + " ms");
            System.out.println("We won!");
            aStarMap.printPath();
        }
        else{
            System.out.println("We lost : (");
        }
    }

    /**
     * This is crucial to the aStar algorithm,
     * @param x cell's row
     * @param y cell's column
     * @param bookF represents if book is found in the path we took so far
     * @return diagonal distance from cell to book, and from book to exit
     */
    int heurisitic(int x, int y, boolean bookF){
        int h= Math.max(Math.abs(Book.a-x), Math.abs(Book.b-y));
        if(bookF)
            h=0;
        if(!bookF)
          h+= Math.max(Math.abs(exit.a-Book.a), Math.abs(exit.b-Book.b));
        else
            h+= Math.max(Math.abs(exit.a-x),Math.abs(exit.b-y));
        return h;
    }

    /**
     * searches the map for the book to assist Astar
     * @return book location in the form of row + column*9
     */
    int getBookForAstar(){
        for(int i=0; i < 9; i++){
            for(int j=0; j < 9; j ++){
                if(aStarMap.map[i][j].type == Type.BOOK){
                    return (j*9 + i);
                }
            }
        }
        return 0;
    }

    /**
     * manages the functions associated with the backtracking algorithm run
     * it calls a function to get the cost of shortest path, then another to get the actual path, and then prints collective results
     */
    void backtrackingManager(){
        //initialization:
        for(int i=0;i<9;i++){
            for(int j=0;j<9;j++){
                historyd[i][j][0] = historyd[i][j][1] = 6000000;
                historyr[i][j][0] = historyr[i][j][1] = 6000000;
            }
        }
        long start = System.nanoTime();
        int cost = backtrack(0,0,false,0);
        long finish = System.nanoTime();
        double timeElapsed = finish - start;
        timeElapsed /= 1000000;
        System.out.println("BACKTRACKING SOLUTION:");
        if( cost <= 600){
            System.out.println("It took us " + cost + " steps to reach goal");
            System.out.println("We won!");
            System.out.println("Time taken: " + timeElapsed + " ms");
            backtrackPATH(0,0,false,0);
            backtrackingMap.printPath();
        }
        else{
            System.out.println("We lost :( ");
            System.out.println("Time taken: " + timeElapsed + " ms");
        }
    }

    /**
     * backtracking function to get the shortest path cost
     * @param x cell's row
     * @param y cell's column
     * @param book represents if book is found in the path we took so far
     * @param dist the distance it took us to reach this cell so far
     * @return distance from exit to our cell, while satisfying the goal of finding book
     */
    int backtrack(int x, int y, boolean book, int dist){

        if(book && backtrackingMap.map[x][y].type == Type.EXIT){
            historyr[x][y][1] = 0;
            historyd[x][y][1] = Math.min(dist,historyd[x][y][1]);
            return 0;
        }

        if(historyd[x][y][(book?1:0)] <= dist){
            return 6000000;
        }
        historyd[x][y][(book?1:0)] = dist;

        if(backtrackingMap.map[x][y].type == Type.BOOK){
            book = true;
            historyd[x][y][1] = dist;
        }

        if(backtrackingMap.map[x][y].hasCloak){
            cloak = true;
        }

        int ret = 60000;
        for(int i=0; i < 8; i++){
            if(isValid(x+ addX[i], y+ addY[i], cloak)){
                ret = Math.min(ret, backtrack(x+ addX[i], y + addY[i],book, dist+1)+1);
            }
        }

        if(backtrackingMap.map[x][y].hasCloak){
            cloak = false;
        }

        historyr[x][y][(book?1:0)] = ret;
        if(backtrackingMap.map[x][y].type == Type.BOOK){
            historyr[x][y][0] = ret;
        }
        return ret;
    }

    /**
     * backtracking function to get the shortest path, knowing how much it cost and our history from previous backtracking function
     * @param x cell's row
     * @param y cell's column
     * @param book represents if book is found in the path we took so far
     * @param dist the distance it took us to reach this cell so far
     */
    void backtrackPATH(int x, int y, boolean book, int dist){
        backtrackingMap.map[x][y].path = true;
        if(book && backtrackingMap.map[x][y].type == Type.EXIT){
            return ;
        }
        if(backtrackingMap.map[x][y].hasCloak){
            cloak = true;
        }
        if(backtrackingMap.map[x][y].type == Type.BOOK){
            book = true;
        }
        int ret = historyr[x][y][book?1:0];
        for(int i=0; i < 8; i++){
            if(isValid(x+ addX[i], y+ addY[i], cloak) &&
                    historyd[x+addX[i]][y+addY[i]][book?1:0] == dist+1 &&
                    historyr[x+addX[i]][y+addY[i]][book?1:0] == ret -1){
                backtrackPATH(x+ addX[i], y+ addY[i],book, dist+1);
                return;
            }
        }
        return;
    }

    /**
     * a function to tell us if a cell with location (x,y) is valid to visit or not
     * @param x cell's row
     * @param y cell's column
     * @param c represents if cloak is found in the path we took so far
     * @return a boolean representing the validity of this cell (1- valid, 0- invalid)
     */
    boolean isValid(int x, int y, boolean c){
        if(x < 0 || y < 0 || x > 8 || y > 8)
            return false;
        if(currentMap.map[x][y].type == Type.PERCEPTION
                || currentMap.map[x][y].type == Type.NORRIS
                || currentMap.map[x][y].type == Type.FILSH){
            if(currentMap.map[x][y].type == Type.PERCEPTION && c){
                return true;
            }
            return false;
        }
        return true;
    }


}

/**
 * responsible for creating our simuilation: handling input, setting the map, and calling algorithms
 */
class Simulation{
    Map currentMap = new Map();
    pair< Integer, Integer > harry,exit;

    /**
     * creates a random map
     * @param variant represents the variant user selected
     */
    Simulation(int variant){
        ArrayList< pair<Integer,Integer> > positions = getRandomPositions();
        harry = new pair<Integer,Integer> (positions.get(0));
        currentMap.map[positions.get(1).a][positions.get(1).b].type = Type.FILSH;
        currentMap.map[positions.get(2).a][positions.get(2).b].type = Type.NORRIS;
        currentMap.map[positions.get(3).a][positions.get(3).b].type = Type.BOOK;
        currentMap.map[positions.get(4).a][positions.get(4).b].hasCloak = true;
        currentMap.map[positions.get(5).a][positions.get(5).b].type = Type.EXIT;
        exit = new pair<Integer, Integer>(positions.get(5));
        ///setting up perception for norris
        int[] addX = {0,0,1,1,1,-1,-1,-1,0,0,2,2,2,2,2,-2,-2,-2,-2,-2,-1,-1,1,1}, addY = {1,-1,1,0,-1,1,0,-1,-2,2,-2,-1,0,1,2,-2,-1,0,1,2,-2,2,-2,2};
        int x = positions.get(2).a, y = positions.get(2).b;
        for(int i=0; i < 8; i++){
            if(isValid(x+ addX[i], y + addY[i])  && currentMap.map[x+addX[i]][y+addY[i]].type == Type.EMPTY){
                currentMap.map[x+ addX[i]][y + addY[i]].type = Type.PERCEPTION;
            }
        }
        ///setting up perception for filsch
        x = positions.get(1).a; y = positions.get(1).b;
        for(int i=0; i < 24; i++){
            if(isValid(x+ addX[i], y + addY[i]) && currentMap.map[x+addX[i]][y+addY[i]].type == Type.EMPTY){
                currentMap.map[x+ addX[i]][y + addY[i]].type = Type.PERCEPTION;
            }
        }
        currentMap.printMap();
        Algorithms algorithms = new Algorithms(currentMap, variant, harry, exit);
    }

    /**
     * creates a map using the locations of actors/objects entered by user
     * @param variant represents the variant user selected
     * @param positions locations of actors/objects entered by user
     */
    Simulation(int variant, ArrayList < pair<Integer, Integer>> positions){
        harry = new pair<Integer,Integer> (positions.get(0));
        currentMap.map[positions.get(1).a][positions.get(1).b].type = Type.FILSH;
        currentMap.map[positions.get(2).a][positions.get(2).b].type = Type.NORRIS;
        currentMap.map[positions.get(3).a][positions.get(3).b].type = Type.BOOK;
        currentMap.map[positions.get(4).a][positions.get(4).b].hasCloak = true;
        currentMap.map[positions.get(5).a][positions.get(5).b].type = Type.EXIT;
        exit = new pair<Integer, Integer>(positions.get(5));
        ///setting up perception for norris
        int[] addX = {0,0,1,1,1,-1,-1,-1,0,0,2,2,2,2,2,-2,-2,-2,-2,-2,-1,-1,1,1}, addY = {1,-1,1,0,-1,1,0,-1,-2,2,-2,-1,0,1,2,-2,-1,0,1,2,-2,2,-2,2};
        int x = positions.get(2).a, y = positions.get(2).b;
        for(int i=0; i < 8; i++){
            if(isValid(x+ addX[i], y + addY[i])  && currentMap.map[x+addX[i]][y+addY[i]].type == Type.EMPTY){
                currentMap.map[x+ addX[i]][y + addY[i]].type = Type.PERCEPTION;
            }
        }
        ///setting up perception for filsch
        x = positions.get(1).a; y = positions.get(1).b;
        for(int i=0; i < 24; i++){
            if(isValid(x+ addX[i], y + addY[i]) && currentMap.map[x+addX[i]][y+addY[i]].type == Type.EMPTY){
                currentMap.map[x+ addX[i]][y + addY[i]].type = Type.PERCEPTION;
            }
        }
        currentMap.printMap();
        Algorithms algorithms = new Algorithms(currentMap, variant, harry, exit);
    }

    /**
     * checks if cell at location (x,y) is valid to visit or not
     * @param x cell's row
     * @param y cell's column
     * @return a boolean representing the validity of this cell (1- valid, 0- invalid)
     */
    boolean isValid(int x, int y){
        return (!(x < 0 || y < 0 || y > 8 || x > 8));
    }

    /**
     * a helper function: checks if a certain pair is in a pair of lists
     * it's used by the fucntion that generates random positions to avoid collision
     * @param ind the pair it searches for
     * @param l the list it searches for the pair in
     * @return a boolean representing if pair is found or not (1- found, 0- not found)
     */
    boolean isPairInList(pair<Integer,Integer> ind, ArrayList<pair<Integer,Integer>> l){
        for(int i=0; i < l.size(); i++){
            if( ind.compare(l.get(i)) )
                return true;
        }
        return false;
    }

    /**
     * a helper function: checks if the position of an actor collides with perception cells of previously selected actors
     * @param ind the pair it searches for its validity
     * @param l the list of positions containing actors' positions
     * @return is this position pair valid to select or not (1- valid, 0- invalid)
     */
    boolean isGoodPositionValid(pair<Integer,Integer> ind, ArrayList<pair<Integer,Integer>> l){
        if(abs(ind.a - l.get(1).a) < 2 || abs(ind.b - l.get(1).b) < 2)
            return false;
        if(abs(ind.a - l.get(2).a) < 3 || abs(ind.b - l.get(2).b) < 3)
            return false;
        return true;
    }

    /**
     * aids in generating a random map
     * it creates valid actors/objects positions and sends this list to the random simulation constructor
     * @return
     */
    ArrayList<pair<Integer,Integer>> getRandomPositions(){
        ArrayList<pair<Integer,Integer>> list = new ArrayList<pair<Integer,Integer>>();

        list.add(new pair<Integer, Integer>(0,0));

        pair < Integer, Integer > xy = new pair<Integer, Integer>(0,0);
        while(isPairInList(xy,list)){
            xy.a = ThreadLocalRandom.current().nextInt(0, 9);
            xy.b = ThreadLocalRandom.current().nextInt(0, 9);
        }
        list.add(xy);

        xy = new pair<Integer, Integer>(0,0);
        while(isPairInList(xy,list)){
            xy.a = ThreadLocalRandom.current().nextInt(0, 9);
            xy.b = ThreadLocalRandom.current().nextInt(0, 9);
        }
        list.add(xy);

        xy = new pair<Integer, Integer>(0,0);
        while(!isGoodPositionValid(xy,list)){
            xy.a= ThreadLocalRandom.current().nextInt(0, 9);
            xy.b = ThreadLocalRandom.current().nextInt(0, 9);
        }
        list.add(xy);

        xy = new pair<Integer, Integer>(0,0);
        while(!isGoodPositionValid(xy,list)){
            xy.a = ThreadLocalRandom.current().nextInt(0, 9);
            xy.b = ThreadLocalRandom.current().nextInt(0, 9);
        }
        list.add(xy);

        xy = new pair<Integer, Integer>(0,0);
        while(!isGoodPositionValid(xy, list) || isPairInList(xy,list)){
            xy.a = ThreadLocalRandom.current().nextInt(0, 9);
            xy.b = ThreadLocalRandom.current().nextInt(0, 9);
        }
        list.add(xy);

        return list;
    }


}


/**
 * processes input and starts the simulation
 */
public class Main {
    /**
     * processes input and starts the simulation
     * @param args main
     */
    public static void main(String[] args) {
	    String line1;
	    Scanner scanner = new Scanner(System.in);
	    boolean validInput = false;
	    do {
            line1 = scanner.nextLine();
            if (line1.charAt(0) == '2') {
                Simulation simulation = new Simulation(2);
                validInput = true;
            }
            else
            if(line1.charAt(0) == '1'){
                Simulation simulation = new Simulation(1);
                validInput = true;
            }
            else {
                String line2 = scanner.nextLine();
                ArrayList<pair<Integer, Integer>> list = new ArrayList<pair<Integer, Integer>>();
                Integer zero = 48;
                for (int i = 1; i < line1.length(); i += 6) {
                    list.add(new pair<Integer, Integer>(Integer.valueOf(line1.charAt(i)) - zero, Integer.valueOf(line1.charAt(i + 2)) - zero));
                }
                if(list.size() == 6)
                    validInput = true;
                Simulation results = new Simulation(2, list);
            }
            if(!validInput){
                System.out.println("please try again");
            }
        }while(!validInput);
    }
}
