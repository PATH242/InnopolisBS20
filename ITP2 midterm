
/*
 Name: Mennatullah Awadallah
 Group: BS20-06
 email: m.awadallah@innopolis.university
 */

///PS: to look for the required elements to be implemented, search for "element" in the code


#include <bits/stdc++.h>
using namespace std;
static int lastIdTaken= 1000566; ///this includes part of the element i "static typing".
/// we use it because we're going to use this variable to generate unique IDs later so we need one version of it to be seen

string typeOfThingArray[3]={"Student", "Transformer", "Car"}, genderArray[2]={"Female", "Male"};
enum TypeOfThing{
    STUDENT, TRANSFORMER, CAR
};
enum Gender{
    FEMALE, MALE, SOULLESS
}; ///Soulless is for cars


class Thing{ ///This class includes element d, several constructors and a destructor
///we need several constructors here in case we want to initialize the class with the initial values or set values of our own
///and the destructor is for clean code

public:
    ///the main characteristics of a Thing: it's ID, type, and gender

    int uniqueId;
    TypeOfThing kind;
    Gender gender;
    ///constructor
    Thing(){
        uniqueId=0;
        gender= SOULLESS;
    }
    Thing( int Id, Gender g){
        uniqueId= Id;
        gender= g;
    }
    Thing(int Id, string g)= delete; ///element b is included here "use of keyword delete"
    ///we're using this because we have to make sure gender is an enum not a string

    ///Unique ID creator
    int generateUniqueId() {
        if(uniqueId) ///if ID's already been set, don't change it
            return uniqueId;
        lastIdTaken--;
        return lastIdTaken;
    }

    int getId(){
        return uniqueId;
    };
    ///a function to set the type of Thing
    void setType(TypeOfThing kindOfThing){
        kind= kindOfThing;
    }

    TypeOfThing getType(){
        return kind;
    }

    Gender getGender(){
        return gender;
    }
    ///element I is included here, a call to a constant member function
    bool operator<(const Thing& right){ ///element a.2 is included here: "constant types", it's used so that right is not changed withing the function
    ///element f is included here, implementing operator '<'
    ///we needed to implement it so we can use it in our sorting algorithms
        if (this->kind<right.kind)
            return 1;
        if(this->kind==right.kind && this->gender<= right.gender)
            return 1;
        return 0;
    }

    ~Thing(){

    }
};
///the box that will contain every "Thing" in our list
struct box{
    box* next;
    box* before;
    Thing object;
};


///class person fulfills the second part element o, it can be considered an interface since we never create an instance of it
///and we only use the classes that inherit it
class Person: public virtual Thing{ ///a person is a thing
    ///this includes element j, the use of virtual functions. We need to use this so we don't face problems with
    ///the diamond shape inheritance structure
public:
    Person(){
        uniqueId= generateUniqueId();
        if(uniqueId%2)
            gender= FEMALE;
        else
            gender= MALE;
    }
};
///this also includes element j, the use of virtual functions. We need to use this so we don't face problems with
///the diamond shape inheritance structure

class Car: public virtual Thing{ ///a car is also a thing and is soulless so no gender
public:
    Car(): Thing() { ///This includes element k "calling the base constructor..". In my code, I'm not supposed to set the
    ///initial values for "Thing". I want to work within the context of thing being set on its own
        uniqueId=generateUniqueId();
        Thing::setType(CAR);
    }


};

class Transformer: public Person, public Car{ ///a transformer is both a person and a car
///this class includes element h, "multiple inheritance". We need it since transformer is both a person and a car
public:
    int UniqueId;
    Transformer(): Thing() {
        UniqueId= Thing::getId();
        Thing::setType(TRANSFORMER);
    }
};

class Student: public Person{ ///a student is a person
private:
    int UniqueId;
public:
    TypeOfThing myType= STUDENT;
    Student(): Thing(){
        UniqueId= Thing::getId();
        Thing::setType(myType);
    }
};



class ListAbstractClass{ ///This includes half element o, includes an "abstract class"
public:
    virtual void add(Thing item)=0;
    virtual Thing get(int i)=0;
    virtual void swapBefore(int i)=0;
    virtual void swapForQuickSort(int a, int b)=0;
    virtual int& Size()=0;
    virtual bool isEmpty()=0;
};
class List: ListAbstractClass{
    ///this includes part of element d, has a destructor and a constructor
    ///the destructor here frees the pointers of the list

    ///this class includes element e, "at least one private and one public members". we're using this since
    ///the list itself shouldn't be accessed by someone outside the list, but the methods should be
private:
     box* startingBox = new box; ///starting box is the head of the list
     box* endingBox= new box; ///ending box is the tail of the list
     int n; ///n stands for number of items

     /// we don't want our list to be copied so we'll keep this copying constructor private
     List( List& templist){
         this->startingBox= templist.startingBox;
         this->endingBox= templist.endingBox;
         this->n= templist.n;
     }
public:

    List(){ ///initial setters
        startingBox->next= NULL;
        startingBox->before= NULL;
        endingBox->before=NULL;
        endingBox->next= NULL;
        n=0;
    }
    void add( Thing item){

        struct box* x = new struct box; ///make the new box we wanna add
        x->object= item; x->before=NULL; x->next=NULL;
        if(!n){
            ///if we're adding the first item, make the start and end have its value
            startingBox= x;
            endingBox= x;
            n++;
        }
        else{ ///else add at the end
            endingBox->next= x;
            x->before= endingBox;
            endingBox=x;
            n++;
        }
    }

    Thing get(int i) {
        if (i>=n){
            cerr<<"invalid\n";
            ///we use cerr so it's not in iostream
        }
        else{
            i++;
            int c=0;
            box* j= startingBox;
            if(n){
                do{
                    if(c) j= j->next; ///get the next item if the first is evaluated
                    c++; ///counter
                    i--; ///I here indicates the index we're getting
                    ///when I reaches 0, we reach the item we need
                }while( j->next!=NULL&& i!=0 );
            }
            return j->object;
        }
        return startingBox->object; ///if item is not found, return first item
    }
    void swapBefore(int ind){
        if(ind>=n||ind<=0){
            cerr<<"invalid\n";
            ///we use cerr so it's not in iostream
        }
        else{
            ind++;
            int c=0;
            box* j= startingBox;
            if(n){
                do{
                    if(c) j= j->next; ///get the next item if the first is evaluated
                    c++; ///counter
                    ind--; ///Ind here indicates the index we're getting
                    ///when I reaches 0, we reach the item we need
                }while( j->next!=NULL&& ind>0 );
            }
            ///swapping
            ///consider me J

            box* tempNext=j->before; ///hold the value of my future next
            box* tempBefore= (j->before)->before; ///hold the value of my future before

            if(tempBefore!= NULL){
                ///if the one before before me isn't null, connect them to me
                (tempBefore)->next= (j);
            }
            else ///else, I must be swapping with the first item so I'm the new starting node
                startingBox= j;

            if(j->next!=NULL){ ///if my next is not null, connect it to the one before me
                j->next->before= j->before;
            }
            ///give the one before me my connections
            j->before->next= j->next;
            j->before->before= j;
            ///and set me with my desired connections
            j->before= tempBefore;
            j->next= tempNext;

        }
    }
    void swapForQuickSort(int a, int b){
        a++; b++;
        int c=0;
        box* aBox= startingBox;
        do{
            if(c) aBox= aBox->next; ///get the next item if the first is evaluated
            c++; ///counter
            a--; ///a here indicates the index we're getting
            ///when a reaches 0, we reach the item we need
        }while( aBox->next!=NULL&& a!=0 );

        box* bBox= startingBox;
        c=0;
        do{
            if(c) bBox= bBox->next; ///get the next item if the first is evaluated
            c++; ///counter
            b--; ///b here indicates the index we're getting
            ///when b reaches 0, we reach the item we need
        }while( bBox->next!=NULL&& b!=0 );

        ///swapping
        Thing aValue= aBox->object;
        Thing bValue= bBox->object;
        aBox->object= bValue;
        bBox->object= aValue;
        return;

    }
    int& Size() {
         static int SizeOfList=n; ///This includes element i, "static typing"
        return SizeOfList;
    }
    bool isEmpty() {
        return (n?0:1);
    }
    ~List(){
         delete startingBox;
         delete endingBox;
     }

};

///this function is used to limit the number of items allowed from each type
///we add 5 from each type in the beginning so we have 15 to start with, then this controls the additions

constexpr int getRandSize(){ ///this includes element c, the use of constexpr
    int n= 2;
    return n;
}

void randomgen( List* ObjectsList){
    ///add at least 5 elements of each
   for(int i=0;i<5;i++){
        Thing* temp = new Thing();
        temp= new Transformer(); ///This includes element g, "user defined conversion"
        ///we need to use it since our list is a list of things, but we want to classify our things still
        ObjectsList->add(*temp);
    }
    for(int i=0;i<5;i++){
        Thing* temp = new Car();
        ObjectsList->add(*temp);
    }
    for(int i=0;i<5;i++){
        Thing* temp = new Student();
        ObjectsList->add(*temp);
    }

    int n= rand()% getRandSize();

    for(int i=0;i<n;i++){
        Thing* temp = new Transformer(); ///this includes the second part of the element i, "dynamic typing"
        ///we need it so that it creates a new unique object each time. That way we're allocating new memory each time
        ObjectsList->add(*temp);
    }
    n= rand()% getRandSize();
    for(int i=0;i<n;i++){
        Thing* temp = new Car();
        ObjectsList->add(*temp);
    }
    n= rand()% getRandSize();
    for(int i=0;i<n;i++){
        Thing* temp = new Student();
        ObjectsList->add(*temp);
    }
    return;
}

void Merge(List* ObjectsList, int s, int mid, int e){

    int s2 = mid + 1;///the start of the second half
    ///if it's sorted, leave
    if (ObjectsList->get(mid) < ObjectsList->get(s2)) {
        return;
    }

    ///keep track of the starts of the two arrays we're merging
    while (s <= mid && s2 <= e) {

        if (ObjectsList->get(s) < ObjectsList->get(s2)) {
            s++;
        }
        else {
            int tempI= s2;
            /// swap items until the start
            while (tempI != s) {
                ObjectsList->swapBefore(tempI);
                tempI--;
            }
            ///change the ranges
            s++;
            mid++;
            s2++;
        }
    }
}
void MergeSort(List* ObjectsList, int s, int e){
    if (s < e) {
        int mid = (s+e)/2;
        MergeSort(ObjectsList, s, mid);
        MergeSort(ObjectsList, mid + 1, e);

        Merge(ObjectsList, s, mid, e);
    }

}


void Print(List* ObjectsList){
    int n= ObjectsList->Size();
    ObjectsList->Size()=n; ///this includes element n

    cout<<n<<'\n'; ///n stands for number of numbers, so print size followed by the numbers
    for(int i=0;i<n;i++){
        Thing temp= ObjectsList->get(i);
        cout<<temp.getId()<<" "; ///print Id, then type
        TypeOfThing tempType= temp.getType();
        cout<<typeOfThingArray[tempType]<<' ';
        if (tempType<2)
            cout<<genderArray[temp.getGender()]<<'\n';
        else
            cout<<'\n';
    }
}
void Shuffle(List* ObjectsList){
    int n= ObjectsList->Size();
    int randiterator= rand()%100+1,a,b;
    for(int j=0;j<randiterator;j++){
        a= rand()%n; b=rand()%n;
        ObjectsList->swapForQuickSort(a,b);
    }

}


int quickSortHelper (List* ObjectsList, int s, int e){

    Thing pivot = ObjectsList->get(e); // pivot
    int i = (s - 1); // smallest item index

    for (int j=s; j<e; j++) {
        // If current item is smaller than the pivot
        if (ObjectsList->get(j) < pivot ){
            i++; //smallest item index is increased
            ObjectsList->swapForQuickSort(i,j);
        }
    }
    ObjectsList->swapForQuickSort(i+1,e);
    return (i + 1);
}

void quickSort(List* ObjectsList, int s, int e) ///s stands for start, e stands for end
{
    if (s < e){

        int i = quickSortHelper(ObjectsList, s, e); ///i is the index of partition
        quickSort(ObjectsList, s, i - 1);
        quickSort(ObjectsList, i + 1, e);
    }
}


int main(){
    List ObjectsList;
    ///element a.1 is included here whenever we want to pass our list to a function "references"
    ///it's used to save memory and time of copying

    randomgen(&ObjectsList); ///to generate random objects and insert them in our list

    cout<<"Before sorting:\n";
    Print(&ObjectsList);

    cout<<"\nafter sorting with merge sort:\n";
    MergeSort(&ObjectsList, 0, ObjectsList.Size()-1);
    Print(&ObjectsList);

    cout<<"\nafter shuffling:\n";
    Shuffle(&ObjectsList); ///shuffle works randomly
    Print(&ObjectsList);

    cout<<"\nafter sorting with quick sort:\n";
    quickSort(&ObjectsList, 0, ObjectsList.Size()-1);
    Print(&ObjectsList);

    return 0;
}
