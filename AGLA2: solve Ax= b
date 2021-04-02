#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>
using namespace std;
int order=0;
const double epsilon= 1e-3;
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
        for(int i=0;i<Matrix.rows;i++){
            for(int j=0;j<Matrix.columns;j++){
                if(abs(Matrix.M[i][j])<=epsilon)
                    output<<"0.00 ";
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

class identityMatrix : public squareMatrix{
public:
    ///upcasting
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
};

///Make a new Augmented matrix with these two matrices and print it
void printAugmentedMatrices( matrix A, matrix B){
    int n1= A.getRowsValue(), m1= A.getColumnsValue();
    int n2= B.getRowsValue(), m2= B.getColumnsValue();

    matrix C(n1, m1+m2);
    C.makeInitialMatrix();

    for(int i=1;i<=n1;i++)
        for(int j=1;j<=m1;j++){
            C.setCell(i,j,A.getCell(i,j));
        }
    for(int i=1;i<=n2;i++)
        for(int j=1;j<=m2;j++){
            C.setCell(i,m1+j,B.getCell(i,j));
        }
    cout<<fixed<<setprecision(2)<<C;
    return;
}

columnVector diagonalNormalization(matrix A, columnVector V){
    cout<<"Diagonal normalization:\n";
    int n= A.getRowsValue();
    for(int i=1;i<=n;i++){
        double multiple= 1/A.getCell(i,i);
        V.multiplyRow(i,multiple);
        A.multiplyRow(i,multiple);
    }
    cout<<fixed<<setprecision(2)<<A<<V;
    return V;
}
columnVector backwardElimination (matrix X, columnVector V){
    int n= X.getRowsValue(), maxInd;
    double maxPivot;
    for(int k=n;k>=1;k--){
        ///you only swap if your current pivot =0
        if(X.getCell(k,k)==0){
            maxPivot= abs(X.getCell(k,k));
            maxInd=k;
            for(int i=k-1;i>=1;i--){
                if( abs( X.getCell(i,k) ) > maxPivot  ){
                    maxPivot= abs(X.getCell(i,k));
                    maxInd =i;
                }
            }
            if(maxInd != k){
                order++;
                cout<<"step #"<<order<<": permutation\n";
                permutationMatrix P (n,k,maxInd);
                X= (P*X);
                V.swapRows(k,maxInd);
                cout<<fixed<<setprecision(2)<<X<<V;
            }
        }
        ///eliminate using [k][k]
        for(int i=k-1;i>=1;i--){

            double val = X.getCell(i,k) / X.getCell(k,k);
            if(abs(val-0.00) <= epsilon)
                continue;
            eliminationMatrix E(n,i,k,-val);
            order++;
            cout<<"step #"<<order<<": elimination\n";
            X= (E*X);
            V.addRowToRow(i,k,-val);
            cout<<fixed<<setprecision(2)<<X<<V;
        }
    }
    ///after you do that, you normalize the diagonals
    return diagonalNormalization(X,V);
}
columnVector forwardElimination (matrix X, columnVector V){

    int  n= X.getRowsValue(), maxInd;
    double maxPivot;

    ///swap with max pivot
    for(int k=1;k<=n;k++){

        maxPivot= abs(X.getCell(k,k));
        maxInd=k;
        for(int i=k+1;i<=n;i++){
            if(abs( X.getCell(i,k) ) > maxPivot){
                maxPivot= abs(X.getCell(i,k));
                maxInd =i;
            }
        }
        if(maxInd != k){
            order++;
            cout<<"step #"<<order<<": permutation\n";
            permutationMatrix P (n,k,maxInd);
            X= (P*X);
            V.swapRows(maxInd,k);
            cout<<fixed<<setprecision(2)<<X<<V;
        }
        for(int i=k+1;i<=n;i++){

            double val = X.getCell(i,k) / X.getCell(k,k);
            if(abs(val-0.00) <= epsilon)
                continue;
            eliminationMatrix E(n,i,k,-val);
            order++;
            cout<<"step #"<<order<<": elimination\n";
            X= (E*X);
            V.addRowToRow(i,k,-val);
            cout<<fixed<<setprecision(2)<<X<<V;
        }
    }
    ///after you do that, you should do backward elimination
    return  backwardElimination(X,V);
}

double getDeterminant(matrix A){
    double ans=1;
    int n= A.getRowsValue();
    for(int i=1;i<=n;i++){
        ans= ( ans* A.getCell(i,i));
    }
    return ans;
}


int main() {
    int n;
    cin>>n;
    squareMatrix A(n);

    cin>>A>>n;
    columnVector V(n);
    cin>>V;
    if( n != A.size() ){
        cout<<"invalid dimensions\n";
        return 0;
    }
    cout<<"step #0:\n";
    cout<<fixed<<setprecision(2)<<A<<V;
    columnVector ans = forwardElimination(A,V);
    cout<<"result:\n";
    cout<<fixed<<setprecision(2)<<ans;
    return 0;
}
