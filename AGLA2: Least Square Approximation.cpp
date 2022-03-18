/* input: matrix A
output: inverse matrix with elimination steps */
#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <cmath>
using namespace std;
int order=0;
double epsilon = 1e-5;
class matrix{
protected:
    int rows,columns;
    vector < vector<double> > M; ///M is for matrix
public:
    matrix(int n, int m){
        rows = n;
        columns = m;
    }
    ///output the matrix
    friend ostream &operator<<(ostream& output, const matrix& Matrix){
        output<<fixed<<setprecision(4);
        for(int i=0;i<Matrix.rows;i++) {
            for (int j = 0; j < Matrix.columns; j++) {
                if (abs(0 - Matrix.M[i][j]) < epsilon)
                    cout << "0.0000 ";
                else
                    output << Matrix.M[i][j] << " ";
            }
            cout << '\n';
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
    double getCell(int i, int j){
        i--; j--;
        if(i<0 || j<0 || i>=rows || j>= columns )
            return 1;

        return M[i][j];
    }
    void setCell(int i, int j, double value){
        i--; j--;
        if(i<0 || j<0 || i>=rows || j>= columns )
            return;

        M[i][j]= value;
    }
    int getRowValue(){
        return rows;
    }
    int getColumnsValue(){
        return columns;
    }
    void makeInitialMatrix(){
        vector<double> temp;
        for(int i=0;i<columns;i++)
            temp.push_back(0);
        for(int i=0;i<rows;i++)
            M.push_back(temp);
    }
    void set(int i, int j, double val){
        i--; j--;
        M[i][j]= val;
    }
    void multiplyRow(int i, double multiple){
        i--;
        for(int j=0;j<getColumnsValue();j++){
            M[i][j]*= multiple;
        }
    }

};
class squareMatrix : public virtual matrix {
public:
    squareMatrix(int n) : matrix(n, n) {

    }
    squareMatrix( matrix temp): matrix(temp.getRowValue(), temp.getRowValue()){

    }
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
    friend ostream &operator<<(ostream& output, const columnVector &Vector){
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

class identityMatrix : public squareMatrix{
public:
    identityMatrix(int n): squareMatrix(n) , matrix(n,n){
        ///making the identity matrix
        vector <double> temp;
        for(int i=0;i<n;i++)
            temp.push_back(0);
        for(int i=0;i<n;i++){
            temp[i]=1;
            M.push_back(temp);
            temp[i]=0;
        }
    }
};
///for eliminationMatrix and permutationMatrix, we assume i and j are sent in one based form
class eliminationMatrix: public identityMatrix {
    ///elimination matrix is an identity matrix with only one different value
public:
    eliminationMatrix(int n, int i, int j, double val): identityMatrix(n), matrix(n,n){
        i--; j--;
        M[i][j]=val;
    }
};
class permutationMatrix: public identityMatrix{
public:
    permutationMatrix(int n, int i, int j): identityMatrix(n), matrix(n,n){
        i--;
        j--;
        M[i][j]=1;
        M[i][i]=0;
        ///swapping the two important cells
        M[j][i]=1;
        M[j][j]=0;
    }
};

matrix diagonalNormalization(matrix A, matrix B){
    int n= A.getRowValue();
    for(int i=1;i<=n;i++){
        double multiple= 1/A.getCell(i,i);
        B.multiplyRow(i,multiple);
        A.multiplyRow(i,multiple);
    }
    return B;
}
matrix backwardElimination (matrix X, matrix IM){
    int n= X.getRowValue(), maxInd;
    double maxPivot;
    for(int k=n;k>=1;k--){
        if(X.getCell(k,k)==0){
            maxPivot= X.getCell(k,k);
            maxInd=k;
            for(int i=k-1;i>1;i--){
                if( abs( X.getCell(i,k) ) > abs( maxPivot)  ){
                    maxPivot= X.getCell(i,k);
                    maxInd =i;
                }
            }
            if(maxInd != k){
                order++;
                permutationMatrix P (n,k,maxInd);
                X= (P*X);
                IM= (P*IM);
            }
        }

        for(int i=k-1;i>=1;i--){

            double val = X.getCell(i,k) / X.getCell(k,k);
            if(!val)
                continue;
            eliminationMatrix E(n,i,k,-val);
            order++;
            X= (E*X);
            IM= (E*IM);
        }
    }
    return diagonalNormalization(X,IM);
}
matrix forwardElimination (matrix X){

    int  n= X.getRowValue(), maxInd;
    double maxPivot;
    identityMatrix I(n);
    matrix IM= I; /// IM = Identity Matrix;

    for(int k=1;k<n;k++){

        maxPivot= X.getCell(k,k);
        maxInd=k;
        for(int i=k+1;i<=n;i++){
            if( abs( X.getCell(i,k) ) > abs( maxPivot)  ){
                maxPivot= X.getCell(i,k);
                maxInd =i;
            }
        }
        if(maxInd != k){
            order++;
            permutationMatrix P (n,k,maxInd);
            X= (P*X);
            IM = (P*IM);
        }
        for(int i=k+1;i<=n;i++){

            double val = X.getCell(i,k) / X.getCell(k,k);
            if(val == 0.00)
                continue;
            eliminationMatrix E(n,i,k,-val);
            order++;
            X= (E*X);
            IM= (E*IM);
        }
    }

    return  backwardElimination(X,IM);
}

void leastSquareApproximation( columnVector t, columnVector b, int degree ){
    matrix A(b.n,degree+1),B(b.n,1);
    A.makeInitialMatrix();
    B.makeInitialMatrix();
    for(int i=0;i<t.n;i++){
        for(int j=0;j<=degree;j++){
            A.set(i+1,j+1,pow(t.values[i],j));
        }
    }
    for(int i=0;i<b.n;i++){
        B.set(i+1,1,b.values[i]);
    }
    cout<<"A:\n"<<A;

    matrix At= A.getTranspose();
    matrix A_TA= (At* A);
    cout<<"A_T*A:\n"<<A_TA;

    matrix A_TA_1= forwardElimination(A_TA);
    cout<<"(A_T*A)^-1:\n"<<A_TA_1;

    matrix At_B= (At*B);
    cout<<"A_T*b:\n"<<At_B;

    matrix X= (A_TA_1*At_B);
    cout<<"x~:\n"<<X<<'\n';
}
int main() {
    int n;
    double x;
    cin>>n;
    columnVector t(n),b(n);
    for(int i=0;i<n;i++){
        cin>>x;
        t.values[i]=x;
        cin>>x;
        b.values[i]=x;
    }
    cin>>n;
    leastSquareApproximation(t,b,n);
    return 0;
}
