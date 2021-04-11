#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <cmath>
using namespace std;
const double epsilon= 1e-5;
///ps: the naming convention used is capitalizing every word after the first
///any index sent to any function is assumed to be in one based form

class matrix{
protected:
    int rows,columns;
    vector < vector<double> > M; ///M is for matrix
public:
    matrix(int n, int m){
        rows = n;
        columns = m;
    }
    void makeInitialMatrix(){
        vector<double> temp;
        for(int i=0;i<columns;i++)
            temp.push_back(0);
        for(int i=0;i<rows;i++)
            M.push_back(temp);
    }
    ///output the matrix
    friend ostream &operator<<(ostream& output, const matrix& Matrix){
        output<< fixed << setprecision(4);
        for(int i=0;i<Matrix.rows;i++){
            for(int j=0;j<Matrix.columns;j++){
                if(abs(Matrix.M[i][j])<=epsilon)
                    output<<"0.0000 ";
                else
                    output<<Matrix.M[i][j]<<" ";
            }
            output<<'\n';
        }
        return output;
    }
    ///input the matrix
    friend istream &operator>>(istream& input, matrix &Matrix){
        double x;

        for(int i=0;i<Matrix.rows;i++){
            vector <double> v;
            for(int j=0;j<Matrix.columns;j++){
                input >> x;
                v.push_back(x);
            }
            Matrix.M.push_back(v);
        }
        return input;
    }
    ///assign one matrix to another
    void operator=(const matrix& b){
        this->M= b.M;
        this->columns= b.columns;
        this->rows= b.rows;
    }
    ///add two matrices together and return the sum
    matrix operator+(const matrix&b){
        matrix sum(this->rows, this->columns);
        vector< vector<double> > sumArray;
        if(this->rows != b.rows || this->columns!= b.columns){
            cout<<"Error: the dimensional problem occurred\n";
            sum.columns= sum.rows =0;
        }
        else{
            for(int i=0;i<this->rows;i++){
                vector<double> temp;
                for(int j=0;j<this->columns;j++)
                    temp.push_back( this->M[i][j]+b.M[i][j] );
                sum.M.push_back(temp);
            }
        }
        return sum;
    }
    ///subtract the second matrix from the first
    matrix operator-(const matrix&b){
        matrix difference(this->rows, this->columns);
        vector< vector<double> > sumArray;
        if(this->rows != b.rows || this->columns!= b.columns){
            cout<<"Error: the dimensional problem occurred\n";
            difference.columns= difference.rows =0;
        }
        else{
            for(int i=0;i<this->rows;i++){
                vector<double> temp;
                for(int j=0;j<this->columns;j++)
                    temp.push_back( this->M[i][j]-b.M[i][j] );
                difference.M.push_back(temp);
            }
        }
        return difference;
    }
    ///get product of two matrices
    matrix operator*(const matrix& b){
        matrix product(this->rows, b.columns);
        if(this->columns != b.rows){
            cout<<"Error: the dimensional problem occurred\n";
            product.columns = product.rows =0;
        }
        else{
            double cellResult;
            for(int i=0;i<this->rows;i++){
                vector <double> temp;
                for(int j=0;j<b.columns;j++){
                    cellResult=0;
                    for(int k=0;k<b.rows;k++){
                        cellResult+= this->M[i][k]*b.M[k][j];
                    }
                    temp.push_back(cellResult);
                }
                product.M.push_back(temp);
            }
        }
        return product;
    }

    ///transpose a matrix
    matrix getTranspose(){
        matrix  aTranspose(this->columns, this->rows);
        for(int i=0;i<this->columns;i++){
            vector <double> temp;
            for(int j=0;j<this->rows;j++)
                temp.push_back(this->M[j][i]);
            aTranspose.M.push_back(temp);
        }
        return aTranspose;
    }
    ///get value of cell [i][j]
    double getCell(int i, int j){
        i--; j--;
        if(i<0 || j<0 || i>=rows || j>= columns )
            return 0;

        return M[i][j];
    }
    ///get number of rows
    int getRowsValue(){
        return rows;
    }
    ///get number of columns
    int getColumnsValue(){
        return columns;
    }
    ///change the value of cell[i][j] to val
    void setCell(int i, int j, double val){
        i--; j--;
        M[i][j]= val;
    }
    ///multiply row i by multiple
    void multiplyRow(int i, double multiple){
        i--;
        for(int j=0;j<getColumnsValue();j++){
            M[i][j]*= multiple;
        }
    }
};
class squareMatrix : public virtual matrix {
public:
    ///upcasting
    squareMatrix(int n) : matrix(n, n) {

    }
    squareMatrix( matrix temp): matrix(temp.getRowsValue(), temp.getRowsValue()){

    }
    ///return size (n*n), return n;
    int size(){
        return rows;
    }

};


class columnVector{
public:
    int n;
    vector<double> values;
    columnVector(int Size){
        n= Size;
        for(int i=0;i<n;i++)
            values.push_back(0);
    }
    void addRowToRow(int i, int j, double multiple){
        i--; j--;
        values[i] += (multiple * values[j]);
    }
    void swapRows(int i, int j){
        i--; j--;
        swap(values[i], values[j]);
    }
    void multiplyRow(int i, double multiple){
        i--;
        values[i] *= multiple;
    }
    friend istream &operator>>(istream& input, columnVector &Vector){
        double x;
        for(int i=0;i<Vector.n;i++){
            input>>x;
            Vector.values[i]=x;
        }
        return input;
    }
    friend ostream &operator<<(ostream& output, columnVector &Vector){
        double x;
        for(int i=0;i<Vector.n;i++){
            if(abs(Vector.values[i])<=epsilon)
                output<<"0.00 "<<'\n';
            else
                output<<Vector.values[i]<<'\n';
        }
        return output;
    }
    double magnitude(){
        double magnitudeValue=0;
        for(int i=0;i<n;i++){
            magnitudeValue+= values[i]*values[i];
        }
        magnitudeValue= sqrt(magnitudeValue);
        return magnitudeValue;
    }
    columnVector operator-(const columnVector& A){
        columnVector difference(A.n);
        for(int i=0;i<difference.n;i++){
            difference.values[i]= this->values[i]-A.values[i];
        }
        return difference;
    }
};


void createAlphaAndBeta(squareMatrix* A, columnVector* V){
    double multiple;
    for(int i=1;i<=A->size();i++){
        multiple= -1/A->getCell(i,i);
        A->multiplyRow(i,multiple);
        V->multiplyRow(i,-multiple);
        A->setCell(i,i,0);
    }
    cout<<"alpha:\n"<<*A;
    cout<<"beta:\n"<<*V;
    return;
}
double calculateError(columnVector A, columnVector B){
    columnVector C = A-B;
    double mag= abs( C.magnitude() );
    cout<<"e: "<<mag<<'\n';
    return mag;
}

void JacobiMethod(squareMatrix* A, columnVector* V, double Error ){
    createAlphaAndBeta(A, V);
    columnVector last(A->size()) , current(A->size());
    double currentError=1000;
    int n= A->size();
    last= *V;
    cout<<"x(0):\n"<<last; ///print first step
    for(int i=1;currentError>= Error ;i++){
        ///create the new values
        for(int j=0;j<n;j++) {
            current.values[j] = 0;
            for (int k = 0; k < n; k++) {
                if (k == j)
                    continue;
                current.values[j] += A->getCell(j+1, k+1) * last.values[k];
            }
            current.values[j] += V->values[j];
        }
        currentError = calculateError(last, current);
        cout<<"x("<<i<<"):\n"<<current;
        last= current;
    }
}
bool checkForJacobiValidity(squareMatrix A){
    for(int i=1;i<=A.size();i++){
        for(int j=1;j<A.size();j++){
            if(i==j)
                continue;
            if(A.getCell(i,i)<= A.getCell(j,i)){
                cout<<"The method is not applicable!\n";
                return 0;
            }

        }
    }
    return 1;
}
int main() {
    int n;
    double Error;
    cin>>n;
    squareMatrix A(n);
    cin>>A>>n;
    columnVector V(n);
    cin>>V>>Error;
    if(checkForJacobiValidity(A))
        JacobiMethod(&A,&V, Error);
    return 0;
}
