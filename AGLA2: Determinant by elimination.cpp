/* input: matrix A
output: elimination steps and determinant */
#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>
using namespace std;
const double epsilon= 0.01;
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
        for(int i=0;i<Matrix.rows;i++){
            for(int j=0;j<Matrix.columns;j++){
                if(abs(0-Matrix.M[i][j])<epsilon)
                    cout<<"0.00 ";
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
    double getCell(int i, int j){
        i--; j--;
        if(i<0 || j<0 || i>=rows || j>= columns )
            return 0;

        return M[i][j];
    }
    int getRowValue(){
        return rows;
    }
    int getColumnsValue(){
        return columns;
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

matrix forwardElimination (matrix X){
    int order=0, n= X.getRowValue(), maxInd;
    double maxPivot;
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
            cout<<"step #"<<order<<": permutation\n";
            permutationMatrix P (n,k,maxInd);
            X= (P*X);
            cout<<fixed<<setprecision(2)<<X;
        }
        for(int i=k+1;i<=n;i++){

            double val = X.getCell(i,k) / X.getCell(k,k);
            if(!val)
                continue;
            eliminationMatrix E(n,i,k,-val);
            order++;
            cout<<"step #"<<order<<": elimination\n";
            X= (E*X);
            cout<<fixed<<setprecision(2)<<X;
        }
    }
    return X;
}
double getDeterminant(matrix A){
    double ans=1;
    int n= A.getRowValue();
    for(int i=1;i<=n;i++){
        ans= ( ans* A.getCell(i,i));
    }
    return ans;
}
int main() {
    int n;
    cin>>n;
    squareMatrix A(n);
    cin>>A;
    matrix ans= forwardElimination(A);

    cout<<"result:\n"<<fixed<<setprecision(2)<<getDeterminant(ans)<<'\n';

    return 0;
}
