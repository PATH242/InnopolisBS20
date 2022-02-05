
package com.PATH;

        import java.io.BufferedReader;
        import java.io.File;
        import java.io.FileReader;
        import java.net.URL;
        import java.util.ArrayList;
        import java.util.Scanner;

public class Main {



    public static void main(String[] args) {
        booking bookingSystem = new booking();
        bookingSystem.start();
        return;
    }


}
class booking{
    public ArrayList<String> seatingTime10 = new ArrayList<>();
    public ArrayList <String> seatingTime15 = new ArrayList<>();
    public ArrayList <String> seatingTime20 = new ArrayList<>();
    public void start(){
        for(int i=0;i<10;i++){
            seatingTime10.add("_");
            seatingTime15.add("_");
            seatingTime20.add("_");
        }
        int y;
        do{
            y = menu();
        }while(y == 1);
    }
    public int menu(){
        int x;
        System.out.println("please enter the number of the option you want: ");
        System.out.println("1: book a ticket");
        System.out.println("2: display seating states");
        System.out.println("3: exit");
        Scanner scanner = new Scanner(System.in);
        while(true){
            x= scanner.nextInt();
            if(x>=1 && x<=3){
                break;
            }
            System.out.println("you entered an invalid number, please try again");
        }
        if(x==1){
            int z = pickTime();
            pickSeating(z);
            getUserData();
        }
        else if(x==2){
            showSeating();
        }
        else{
            return 0;
        }
        return 1;
    }
    public void pickSeating(int time){
        int y;
        Scanner scanner = new Scanner(System.in);
        if(time == 10){
            System.out.println("for the time slot of 10 am: ");
            for(int i=0;i<10;i++){
                System.out.print(i+1);
                System.out.print(" ");
            }
            System.out.println();
            for(int i=0;i<10;i++){
                System.out.print(seatingTime10.get(i));
                System.out.print(" ");
            }
            System.out.println();
            System.out.println("please enter the number corrosponding to the seat you want! ");
            System.out.println("not that '_' signals an empty seat");
            while(true){
                y = scanner.nextInt();
                if(y>10 || seatingTime10.get(y) == "X"){
                    System.out.println("you've entered an invalid seat number, please try again");
                }
                else{
                    break;
                }
            }
            seatingTime10.set(y-1,"X");
            getUserData();
        }
        else
        if(time == 15){
            System.out.println("for the time slot of 15 pm: ");
            for(int i=0;i<10;i++){
                System.out.print(i+1);
                System.out.print(" ");
            }
            System.out.println();
            for(int i=0;i<10;i++){
                System.out.print(seatingTime15.get(i));
                System.out.print(" ");
            }
            System.out.println();
            System.out.println("please enter the number corrosponding to the seat you want! ");
            System.out.println("not that '_' signals an empty seat");
            while(true){
                y = scanner.nextInt();
                if(y>10 || seatingTime15.get(y) == "X"){
                    System.out.println("you've entered an invalid seat number, please try again");
                }
                else{
                    break;
                }
            }
            seatingTime15.set(y,"X");
            getUserData();
        }
        else
        if(time == 20){
            System.out.println("for the time slot of 20 am: ");
            for(int i=0;i<10;i++){
                System.out.print(i+1);
                System.out.print(" ");
            }
            System.out.println();
            for(int i=0;i<10;i++){
                System.out.print(seatingTime20.get(i));
                System.out.print(" ");
            }
            System.out.println();
            System.out.println("please enter the number corrosponding to the seat you want! ");
            System.out.println("not that '_' signals an empty seat");
            while(true){
                y = scanner.nextInt();
                if(y>10 || seatingTime20.get(y) == "X"){
                    System.out.println("you've entered an invalid seat number, please try again");
                }
                else{
                    break;
                }
            }
            seatingTime20.set(y,"X");
            getUserData();
        }
    }
    public int pickTime(){
        System.out.println("please pick one of the three time slots available: 10:00, 15:00, 20:00");
        int y;
        while(true){
            Scanner scanner = new Scanner(System.in);
            y= scanner.nextInt();
            if(y!=15 && y!=20 && y!=10){
                System.out.println("you've entered an invalid number, please try again");
            }
            else{
                break;
            }
        }
        return y;
    }
    public void showSeating(){
        System.out.println("for the time slot of 10 am: ");
        for(int i=0;i<10;i++){
            System.out.print(seatingTime10.get(i));
            System.out.print(" ");
        }
        System.out.println("\nfor the time slot of 15 pm: ");
        for(int i=0;i<10;i++){
            System.out.print(seatingTime15.get(i));
            System.out.print(" ");
        }
        System.out.println("\nfor the time slot of 20 pm: ");
        for(int i=0;i<10;i++){
            System.out.print(seatingTime20.get(i));
            System.out.print(" ");
        }
        System.out.println();
    }
    public String getUserData(){
        Scanner scanner = new Scanner(System.in);

        String name, lastName, confirmation;
        while(true) {
            System.out.println("please enter your name in a line, followed by your surname");
            name = scanner.nextLine();
            lastName = scanner.nextLine();
            System.out.println("is this data correct? (Y/N)" + name + " " + lastName);
            confirmation = scanner.next();
            if(confirmation != "N")
                break;
        }
        return name+lastName;
    }
}
