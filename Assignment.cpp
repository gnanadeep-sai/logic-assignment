#include <bits/stdc++.h>

using namespace std;

struct Node {
    int data;
    Node* left = nullptr;
    Node* right = nullptr;

    explicit Node(int val) : data(val) {}
};

class PropositionalTree {
  private:
    static constexpr int OP_NOT = -1;
    static constexpr int OP_OR = -2;
    static constexpr int OP_AND = -3;
    static constexpr int OP_IMP = -4;
    static constexpr int LPAREN = -10;
    static constexpr int RPAREN = -11;
    static constexpr int MAX_TRUTH_TABLE_VARS = 16;

    Node* root = nullptr;
    string infix;

    vector<int> infixTokens;
    vector<int> matchParen;
    vector<int> prefix;
    vector<string> names;
    unordered_map<string, int> nameToId;
    string sep;

    static constexpr bool isOp(int c) { return c < 0 && c != LPAREN && c != RPAREN; }
    static constexpr bool isBinaryOp(int c) { return c == OP_OR || c == OP_AND || c == OP_IMP; }

    static constexpr char opChar(int c) {
      switch (c) {
        case OP_NOT: return '~';
        case OP_OR:  return '+';
        case OP_AND: return '*';
        case OP_IMP: return '>';
      }
      return '?';
    }

    string label(int c) {
      if (c < 0) return string(1, opChar(c));
      return names[c];
    }

    void prepare() {
      infixTokens.clear();
      names.clear();
      nameToId.clear();

      for (size_t i = 0; i < infix.size();) {
        char c = infix[i];
        if (isspace((unsigned char)c)) { i++; }
        else if (c == '(') { infixTokens.push_back(LPAREN); i++; }
        else if (c == ')') { infixTokens.push_back(RPAREN); i++; }
        else if (c == '~') { infixTokens.push_back(OP_NOT); i++; }
        else if (c == '+') { infixTokens.push_back(OP_OR);  i++; }
        else if (c == '*') { infixTokens.push_back(OP_AND); i++; }
        else if (c == '>') { infixTokens.push_back(OP_IMP); i++; }
        else if (isalnum((unsigned char)c) || c == '_') {
          size_t j = i;
          while (j < infix.size() && (isalnum((unsigned char)infix[j]) || infix[j] == '_')) j++;
          string name = infix.substr(i, j - i);
          if (nameToId.count(name) == 0) {
              nameToId[name] = names.size();
              names.push_back(name);
          }
          int id = nameToId[name];
          infixTokens.push_back(id);
          i = j;
        }
        else {
          throw runtime_error(string("unexpected character '") + c + "' in formula");
        }
      }

      matchParen.assign(infixTokens.size(), -1);
      vector<int> st;
      for (int i = 0; i < (int)infixTokens.size(); i++) {
        if (infixTokens[i] == LPAREN) st.push_back(i);
        else if (infixTokens[i] == RPAREN) {
          if (st.empty()) throw runtime_error("unbalanced parentheses in formula");
          matchParen[st.back()] = i;
          matchParen[i] = st.back();
          st.pop_back();
        }
      }
      if (!st.empty()) throw runtime_error("unbalanced parentheses in formula");

      sep = "";
      for (const string& n : names) if (n.size() > 1) sep = " ";
    }

    void processInfix(int l, int r) {
      if (l >= r) return;

      while (r - l >= 2 && infixTokens[l] == LPAREN && matchParen[l] == r - 1) {
        l++;
        r--;
      }

      if (r - l == 1) {
        prefix.push_back(infixTokens[l]);
        return;
      }

      int main_op_idx = -1;
      for (int i = l; i < r; i++) {
        if (infixTokens[i] == LPAREN) { i = matchParen[i]; continue; }
        if (isBinaryOp(infixTokens[i])) {
          main_op_idx = i;
          break;
        }
      }

      if (main_op_idx == -1) {
        if (infixTokens[l] == OP_NOT) {
          prefix.push_back(OP_NOT);
          processInfix(l + 1, r);
        }
        return;
      }

      prefix.push_back(infixTokens[main_op_idx]);
      processInfix(l, main_op_idx);
      processInfix(main_op_idx + 1, r);
    }

    Node* buildTree(vector<int>& exp, int& i){

      if(i >= (int)exp.size()){
        return nullptr;
      }

      int c = exp[i];
      i++;
      Node* root = new Node(c);

      if(isOp(c)){
        if(c == OP_NOT){
          Node* left_tree = buildTree(exp, i);
          root->left = left_tree;
          return root;
        }
        Node* left_tree = buildTree(exp, i);
        Node* right_tree = buildTree(exp, i);
        root->left = left_tree;
        root->right = right_tree;
        return root;
      }

      else{
        return root;
      }

    }
    void inOrderRec(Node* curr){
      if(!curr){
        return;
      }

      if(!(curr->left || curr->right)){
        cout << label(curr->data);
      }
      else{
        cout << "(";
        if(!curr->right){
          cout << "~";
        }
        inOrderRec(curr->left);
        if(curr->right){
          cout << " " << label(curr->data) << " ";
          inOrderRec(curr->right);
        }
        cout << ")";
      }
    }
    void preOrderRec(Node* curr){
      if(!curr){
        return;
      }

      if(!(curr->left || curr->right)){
        cout << label(curr->data) << sep;
      }
      else{
        cout << label(curr->data) << sep;
        preOrderRec(curr->left);
        if(curr->right){
          preOrderRec(curr->right);
        }
      }
    }
    void postOrderRec(Node* curr){
      if(!curr){
        return;
      }

      if(!(curr->left || curr->right)){
        cout << label(curr->data) << sep;
      }
      else{
        postOrderRec(curr->left);
        if(curr->right){
          postOrderRec(curr->right);
        }
        cout << label(curr->data) << sep;

      }
    }

    int heightRec(Node* curr) {
        if (!curr) return 0;
        return 1 + max(heightRec(curr->left), heightRec(curr->right));
    }

    bool evaluateRec(Node* curr, const vector<char>& truthValues) {
        if (!curr) return false;

        if (!(curr->left || curr->right)) {
            return truthValues[curr->data];
        }

        if (curr->data == OP_NOT) {
            return !evaluateRec(curr->left, truthValues);
        }

        bool left_val = evaluateRec(curr->left, truthValues);
        bool right_val = evaluateRec(curr->right, truthValues);

        if (curr->data == OP_OR)  return left_val || right_val;
        if (curr->data == OP_AND) return left_val && right_val;
        if (curr->data == OP_IMP) return (!left_val) || right_val;

        return false;
    }

    void populateVars(Node* curr, set<int>& vars) {
        if (!curr) return;

        if (!(curr->left || curr->right)) {
            vars.insert(curr->data);
        }

        populateVars(curr->left, vars);
        populateVars(curr->right, vars);
    }

    void destroyTree(Node* curr) {
      if (!curr) return;
      destroyTree(curr->left);
      destroyTree(curr->right);
      delete curr;
    }

    string parseCNF(ifstream& file) {
        string line;
        vector<vector<int>> cnf;
        vector<int> current;
        int maxVar = 0;

        while (getline(file, line)) {
            if (line.empty() || line[0] == 'c' || line[0] == 'p' || line[0] == '%') continue;

            stringstream ss(line);
            int lit;

            while (ss >> lit) {
                if (lit == 0) {
                    if (!current.empty()) cnf.push_back(current);
                    current.clear();
                } else {
                    current.push_back(lit);
                    maxVar = max(maxVar, abs(lit));
                }
            }
        }
        if (!current.empty()) cnf.push_back(current);
        if (cnf.empty()) return "";

        auto varName = [&](int v) {
            if (maxVar <= 26) return string(1, (char)('A' + v - 1));
            return "x" + to_string(v);
        };

        string final_formula;
        for (size_t c = 0; c < cnf.size(); c++) {
            string clause;
            for (size_t k = 0; k < cnf[c].size(); k++) {
                int lit = cnf[c][k];
                string atom = (lit < 0 ? "~" : "") + varName(abs(lit));
                if (k == 0) clause = atom;
                else clause = "(" + clause + "+" + atom + ")";
            }
            if (c == 0) final_formula = clause;
            else final_formula = "(" + final_formula + "*" + clause + ")";
        }
        return final_formula;
    }

  public:
    explicit PropositionalTree(string infix_expr) : infix(std::move(infix_expr)) {
      prepare();
    }

    explicit PropositionalTree(ifstream& file) {
      infix = parseCNF(file);
      prepare();
    }

    ~PropositionalTree() {
      destroyTree(root);
    }

    PropositionalTree(const PropositionalTree&) = delete;
    PropositionalTree& operator=(const PropositionalTree&) = delete;

    [[nodiscard]] bool empty() const { return infixTokens.empty(); }

    [[nodiscard]] int countNodes() const {
      if (!root) return 0;
      int count = 0;
      stack<Node*> st;
      st.push(root);
      while (!st.empty()) {
        Node* n = st.top();
        st.pop();
        count++;
        if (n->left) st.push(n->left);
        if (n->right) st.push(n->right);
      }
      return count;
    }

    void infixToPrefix(){
      prefix.clear();
      processInfix(0, infixTokens.size());
    };

    void buildTreeFromPrefix(){
      destroyTree(root);
      int start = 0;
      root = buildTree(prefix, start);
    }

    void printInOrderRecursive() {
      inOrderRec(root);
      cout << '\n';
    }
    void printPreOrderRecursive(){
      preOrderRec(root);
      cout << '\n';
    }
    void printPostOrderRecursive(){
      postOrderRec(root);
      cout << '\n';
    }
    void printInOrderIterative(){
      stack<Node*> nodes;
      nodes.push(nullptr);
      Node* next = root;
      Node* prev = nullptr;
      while(next){
        if(!(next->left || next->right)){
          cout << label(next->data);
          swap(next, prev);
        }

        else{
          if(next->right && prev == next->right){
            cout << ')';
            nodes.pop();
            prev = next;
            next = nodes.top();
          }
          else if(prev == next->left){
            if(next->data == OP_NOT){
              cout << ')';
              nodes.pop();
              prev = next;
              next = nodes.top();
              continue;
            }
            cout << " " << label(next->data) << " ";
            prev = next;
            next = next->right;
          }
          else{
            cout << '(';
            if(next->data == OP_NOT){
              cout << '~';
            }
            nodes.push(next);
            prev = next;
            next = next->left;

          }
        }
      }
      cout << '\n';
    }
    void printPreOrderIterative(){
      stack<Node*> nodes;
      nodes.push(nullptr);
      Node* next = root;
      Node* prev = nullptr;
      while(next){
        if(!(next->left || next->right)){
          cout << label(next->data) << sep;
          swap(next, prev);
        }

        else{
          if(next->right && prev == next->right){
            nodes.pop();
            prev = next;
            next = nodes.top();
          }
          else if(prev == next->left){
            if(next->data == OP_NOT){
              nodes.pop();
              prev = next;
              next = nodes.top();
              continue;
            }
            prev = next;
            next = next->right;
          }
          else{
            cout << label(next->data) << sep;
            nodes.push(next);
            prev = next;
            next = next->left;
          }
        }
      }
      cout << '\n';
    }
    void printPostOrderIterative(){
      stack<Node*> nodes;
      nodes.push(nullptr);
      Node* next = root;
      Node* prev = nullptr;
      while(next){
        if(!(next->left || next->right)){
          cout << label(next->data) << sep;
          swap(next, prev);
        }

        else{
          if(next->right && prev == next->right){
            cout << label(next->data) << sep;
            nodes.pop();
            prev = next;
            next = nodes.top();
          }
          else if(prev == next->left){
            if(next->data == OP_NOT){
              cout << label(next->data) << sep;
              nodes.pop();
              prev = next;
              next = nodes.top();
              continue;
            }
            prev = next;
            next = next->right;
          }
          else{
            nodes.push(next);
            prev = next;
            next = next->left;
          }
        }
      }
      cout << '\n';
    }

    int computeHeightRecursive(){
      return heightRec(root);
    };

    int computeHeightIterative(){
      if (!root) return 0;

      queue<Node*> q;
      q.push(root);
      int height = 0;

      while (!q.empty()) {
        int level_size = q.size();
        height++;

        for (int i = 0; i < level_size; i++) {
          Node* curr = q.front();
          q.pop();

          if (curr->left)  q.push(curr->left);
          if (curr->right) q.push(curr->right);
        }
      }
      return height;
    };

    void generateTruthTable() {
        if (!root) return;

        set<int> unique_vars;
        populateVars(root, unique_vars);
        vector<int> vars(unique_vars.begin(), unique_vars.end());
        std::sort(vars.begin(), vars.end(), [&](int a, int b) {
            return names[a] < names[b];
        });
        int n = vars.size();

        if (n > MAX_TRUTH_TABLE_VARS) {
            cout << "Formula has " << n << " variables, so its truth table would have 2^" << n
                 << " rows.\nSkipping (limit is " << MAX_TRUTH_TABLE_VARS << " variables).\n";
            return;
        }

        string header;
        for (int v : vars) {
            header += names[v] + " | ";
        }
        header += "Result";
        cout << header << "\n";
        cout << string(header.size(), '-') << "\n";

        unsigned int num_rows = 1u << n;
        vector<char> truthValues(names.size(), 0);

        for (unsigned int i = 0; i < num_rows; i++) {
            for (int j = 0; j < n; j++) {
                bool val = (i >> (n - 1 - j)) & 1;
                truthValues[vars[j]] = val;
                cout << (val ? "T" : "F") << string(names[vars[j]].size() > 1 ? names[vars[j]].size() - 1 : 0, ' ') << " | ";
            }

            bool result = evaluateRec(root, truthValues);

            cout << "  " << (result ? "T" : "F") << "\n";
        }
    };
};


void printPreview(const string& label, const string& text) {
    cout << label << ": ";
    if (text.size() > 80) {
        cout << text.substr(0, 80) << "... (" << text.size() << " chars)\n";
    } else {
        cout << text << "\n";
    }
}
int main(void) {
    string input_str;
    unique_ptr<PropositionalTree> tree;

    try {
      cout << "METHOD 1: Enter an infix formula (or press ENTER to skip): ";
      getline(cin, input_str);

      if (!input_str.empty()) {
        tree = make_unique<PropositionalTree>(input_str);
      }

      else {
        cout << "METHOD 2: Enter the path to the DIMACS .cnf file: ";
        getline(cin, input_str);

        auto isJunk = [](char c) { return isspace((unsigned char)c) || c == '"' || c == '\''; };
        while (!input_str.empty() && isJunk(input_str.back())) input_str.pop_back();
        size_t firstGood = 0;
        while (firstGood < input_str.size() && isJunk(input_str[firstGood])) firstGood++;
        input_str = input_str.substr(firstGood);

        ifstream file(input_str);
        if (!file.is_open()) {
          cout << "Error: Could not open file.\n";
          return 1;
        }

        tree = make_unique<PropositionalTree>(file);
      }
    }
    catch (const exception& e) {
      cout << "Error: " << e.what() << "\n";
      return 1;
    }

    if (tree->empty()) {
      cout << "Error: the formula is empty.\n";
      return 1;
    }

  tree->infixToPrefix();

  tree->buildTreeFromPrefix();

  constexpr int LARGE_TREE_NODES = 300;
  int nodeCount = tree->countNodes();
  bool large = nodeCount > LARGE_TREE_NODES;

  cout << "\n--- Tree Traversals ---\n";
  cout << "Nodes in tree          : " << nodeCount << "\n\n";

  if (nodeCount > 300) {
      cout << "Tree is large (" << nodeCount << " nodes). Skipping full traversal display .\n\n";
  } 
  else {
      cout << "Pre-Order (Recursive) : ";
      tree->printPreOrderRecursive();
      cout << "Pre-Order (Iterative) : ";
      tree->printPreOrderIterative();

      cout << "In-Order (Recursive)  : ";
      tree->printInOrderRecursive();
      cout << "In-Order (Iterative)  : ";
      tree->printInOrderIterative();

      cout << "Post-Order (Recursive): ";
      tree->printPostOrderRecursive();
      cout << "Post-Order (Iterative): ";
      tree->printPostOrderIterative();
      cout << "\n";
  }

  cout << "--- Tree Height ---\n";
  cout << "Height (Recursive)     : " << tree->computeHeightRecursive() << "\n";
  cout << "Height (Iterative)     : " << tree->computeHeightIterative() << "\n\n";

  cout << "--- Truth Table ---\n";
  tree->generateTruthTable();

  return 0;
}