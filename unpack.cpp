// unpack.cpp
// Reversing the pallet
// Reconstruct "order file" (boxes with dimensions) from a given pallet placement.
//
// Input placement formats supported:
//  1) CSV with header: SKU,x,y,z,X,Y,Z  (case-insensitive, commas or semicolons)
//     optionally: weight, aisle
//  2) Whitespace-separated lines: sku x y z X Y Z   (optionally + weight + aisle)
//
// Output: reconstructed_boxes.csv
//  columns: SKU,Length,Width,Height,Weight,Aisle,Quantity
//
// Build: g++ -O2 -std=c++17 reverse_pallet.cpp -o reverse_pallet
// Run:   ./reverse_pallet placement.csv reconstructed_boxes.csv --merge

#include <algorithm>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

static constexpr double EPS = 1e-9;

static inline string trim(const string& s) {
    size_t a = 0, b = s.size();
    while (a < b && isspace((unsigned char)s[a])) ++a;
    while (b > a && isspace((unsigned char)s[b - 1])) --b;
    return s.substr(a, b - a);
}

static inline string lower_str(string s) {
    for (char& c : s) c = (char)tolower((unsigned char)c);
    return s;
}

static vector<string> split_any(const string& line) {
    // Split on comma/semicolon/space/tab
    vector<string> out;
    string cur;
    auto flush = [&]() {
        string t = trim(cur);
        if (!t.empty()) out.push_back(t);
        cur.clear();
    };
    for (char c : line) {
        if (c == ',' || c == ';' || c == '\t' || c == ' ') {
            flush();
        } else {
            cur.push_back(c);
        }
    }
    flush();
    return out;
}

static bool is_numberish(const string& s) {
    // minimal check: has digit or '-' and can be stod
    if (s.empty()) return false;
    bool has_digit = false;
    for (char c : s) {
        if (isdigit((unsigned char)c)) has_digit = true;
        else if (c=='-'||c=='+'||c=='.'||c=='e'||c=='E') {}
        else return false;
    }
    if (!has_digit) return false;
    try { (void)stod(s); return true; } catch (...) { return false; }
}

struct PlacementRow {
    int sku = 0;
    double x1=0,y1=0,z1=0,x2=0,y2=0,z2=0;
    double weight = 0;
    int aisle = 0;
    bool has_weight = false;
    bool has_aisle = false;
};

struct BoxOut {
    int sku=0;
    int L=0,W=0,H=0;
    double weight=0;
    int aisle=0;
    int qty=1;
};

static int to_int_round(double v) {
    // your data is usually integer mm; round safely
    return (int)llround(v);
}

static bool nearly_zero_or_pos(double v) {
    return v > -1e-6;
}

static void die(const string& msg) {
    throw runtime_error(msg);
}

static vector<PlacementRow> read_placement(const string& path) {
    ifstream fin(path);
    if (!fin) die("Cannot open input file: " + path);

    string first;
    if (!getline(fin, first)) return {};
    string first_trim = trim(first);
    if (first_trim.empty()) return {};

    vector<PlacementRow> rows;

    // Detect header if contains letters
    bool looks_like_header = false;
    for (char c : first_trim) {
        if (isalpha((unsigned char)c)) { looks_like_header = true; break; }
    }

    // Column indices if header exists
    int idx_sku=-1, idx_x=-1, idx_y=-1, idx_z=-1, idx_X=-1, idx_Y=-1, idx_Z=-1;
    int idx_weight=-1, idx_aisle=-1;

    auto parse_line_as_row_by_indices = [&](const vector<string>& cols) -> PlacementRow {
        auto getd = [&](int idx) -> double {
            if (idx < 0 || idx >= (int)cols.size()) die("Bad/missing column in data row.");
            return stod(cols[idx]);
        };
        auto geti = [&](int idx) -> int {
            if (idx < 0 || idx >= (int)cols.size()) die("Bad/missing column in data row.");
            return (int)llround(stod(cols[idx]));
        };

        PlacementRow r;
        r.sku = geti(idx_sku);
        r.x1 = getd(idx_x);
        r.y1 = getd(idx_y);
        r.z1 = getd(idx_z);
        r.x2 = getd(idx_X);
        r.y2 = getd(idx_Y);
        r.z2 = getd(idx_Z);

        if (idx_weight >= 0 && idx_weight < (int)cols.size() && is_numberish(cols[idx_weight])) {
            r.weight = stod(cols[idx_weight]);
            r.has_weight = true;
        }
        if (idx_aisle >= 0 && idx_aisle < (int)cols.size() && is_numberish(cols[idx_aisle])) {
            r.aisle = (int)llround(stod(cols[idx_aisle]));
            r.has_aisle = true;
        }
        return r;
    };

    auto parse_line_as_simple = [&](const vector<string>& cols) -> PlacementRow {
        // sku x y z X Y Z [weight] [aisle]
        if (cols.size() < 7) die("Expected at least 7 columns: sku x y z X Y Z");
        PlacementRow r;
        r.sku = (int)llround(stod(cols[0]));
        r.x1 = stod(cols[1]);
        r.y1 = stod(cols[2]);
        r.z1 = stod(cols[3]);
        r.x2 = stod(cols[4]);
        r.y2 = stod(cols[5]);
        r.z2 = stod(cols[6]);

        if (cols.size() >= 8 && is_numberish(cols[7])) {
            r.weight = stod(cols[7]);
            r.has_weight = true;
        }
        if (cols.size() >= 9 && is_numberish(cols[8])) {
            r.aisle = (int)llround(stod(cols[8]));
            r.has_aisle = true;
        }
        return r;
    };

    if (looks_like_header) {
        // parse header
        auto hdr = split_any(first_trim);
        vector<string> hlow;
        hlow.reserve(hdr.size());
        for (auto& s : hdr) hlow.push_back(lower_str(trim(s)));

        auto find_idx = [&](const vector<string>& names) -> int {
            for (int i = 0; i < (int)hlow.size(); ++i) {
                for (const auto& n : names) {
                    if (hlow[i] == n) return i;
                }
            }
            return -1;
        };

        idx_sku = find_idx({"sku"});
        idx_x   = find_idx({"x", "x1"});
        idx_y   = find_idx({"y", "y1"});
        idx_z   = find_idx({"z", "z1"});
        idx_X   = find_idx({"x2", "xmax", "X", "X2", "x_", "X1", "x2"}); // but we'll also search exact "x2"
        idx_Y   = find_idx({"y2", "ymax", "Y", "Y2", "y_", "Y1", "y2"});
        idx_Z   = find_idx({"z2", "zmax", "Z", "Z2", "z_", "Z1", "z2"});

        // do stricter fallback for X/Y/Z if weird casing came in
        if (idx_X < 0) idx_X = find_idx({"x2"});
        if (idx_Y < 0) idx_Y = find_idx({"y2"});
        if (idx_Z < 0) idx_Z = find_idx({"z2"});

        idx_weight = find_idx({"weight", "w"});
        idx_aisle  = find_idx({"aisle"});

        if (idx_sku < 0 || idx_x < 0 || idx_y < 0 || idx_z < 0 || idx_X < 0 || idx_Y < 0 || idx_Z < 0) {
            die("Header detected but required columns not found. Need: SKU,x,y,z,X,Y,Z (case-insensitive).");
        }

        // read remaining lines
        string line;
        while (getline(fin, line)) {
            line = trim(line);
            if (line.empty()) continue;
            auto cols = split_any(line);
            if (cols.empty()) continue;
            rows.push_back(parse_line_as_row_by_indices(cols));
        }
    } else {
        // first line is data
        auto cols1 = split_any(first_trim);
        if (!cols1.empty()) rows.push_back(parse_line_as_simple(cols1));

        string line;
        while (getline(fin, line)) {
            line = trim(line);
            if (line.empty()) continue;
            auto cols = split_any(line);
            if (cols.empty()) continue;
            rows.push_back(parse_line_as_simple(cols));
        }
    }

    // sanitize rows
    for (auto& r : rows) {
        // swap if reversed
        if (r.x2 < r.x1) swap(r.x1, r.x2);
        if (r.y2 < r.y1) swap(r.y1, r.y2);
        if (r.z2 < r.z1) swap(r.z1, r.z2);

        double dx = r.x2 - r.x1;
        double dy = r.y2 - r.y1;
        double dz = r.z2 - r.z1;

        if (!nearly_zero_or_pos(dx) || !nearly_zero_or_pos(dy) || !nearly_zero_or_pos(dz)) {
            die("Negative dimension encountered in placement after sanitization.");
        }
        if (dx < EPS || dy < EPS || dz < EPS) {
            // degenerate box - ignore? better to error loudly
            die("Degenerate box with near-zero dimension found (check placement file).");
        }
    }

    return rows;
}

static vector<BoxOut> reconstruct_boxes(const vector<PlacementRow>& rows, bool merge_equal) {
    vector<BoxOut> out;
    out.reserve(rows.size());

    if (!merge_equal) {
        for (const auto& r : rows) {
            BoxOut b;
            b.sku = r.sku;
            b.L = to_int_round(r.x2 - r.x1);
            b.W = to_int_round(r.y2 - r.y1);
            b.H = to_int_round(r.z2 - r.z1);
            b.weight = r.has_weight ? r.weight : 0.0;
            b.aisle  = r.has_aisle  ? r.aisle  : 0;
            b.qty = 1;
            out.push_back(b);
        }
        return out;
    }

    struct Key {
        int sku, L, W, H, aisle;
        long long wq; // weight quantized (to avoid float map issues)
    };

    struct KeyHash {
        size_t operator()(const Key& k) const noexcept {
            size_t h = 1469598103934665603ull;
            auto mix = [&](long long v) {
                h ^= (size_t)v + 0x9e3779b97f4a7c15ull + (h<<6) + (h>>2);
            };
            mix(k.sku); mix(k.L); mix(k.W); mix(k.H); mix(k.aisle); mix(k.wq);
            return h;
        }
    };
    struct KeyEq {
        bool operator()(const Key& a, const Key& b) const noexcept {
            return a.sku==b.sku && a.L==b.L && a.W==b.W && a.H==b.H && a.aisle==b.aisle && a.wq==b.wq;
        }
    };

    unordered_map<Key, int, KeyHash, KeyEq> mp;
    out.clear();

    auto quant_w = [&](double w)->long long {
        // 1 gram granularity if weight is grams; otherwise just quantize to 1e-3
        return (long long) llround(w * 1000.0);
    };

    for (const auto& r : rows) {
        int L = to_int_round(r.x2 - r.x1);
        int W = to_int_round(r.y2 - r.y1);
        int H = to_int_round(r.z2 - r.z1);
        double weight = r.has_weight ? r.weight : 0.0;
        int aisle = r.has_aisle ? r.aisle : 0;

        Key key{r.sku, L, W, H, aisle, quant_w(weight)};
        auto it = mp.find(key);
        if (it == mp.end()) {
            BoxOut b;
            b.sku = r.sku; b.L=L; b.W=W; b.H=H; b.weight=weight; b.aisle=aisle; b.qty=1;
            int idx = (int)out.size();
            out.push_back(b);
            mp.emplace(key, idx);
        } else {
            out[it->second].qty += 1;
        }
    }

    return out;
}

static void write_boxes_csv(const string& path, const vector<BoxOut>& boxes) {
    ofstream fout(path);
    if (!fout) die("Cannot open output file: " + path);

    fout << "SKU,Length,Width,Height,Weight,Aisle,Quantity\n";
    for (const auto& b : boxes) {
        fout << b.sku << ","
             << b.L << ","
             << b.W << ","
             << b.H << ","
             << fixed << setprecision(3) << b.weight << ","
             << b.aisle << ","
             << b.qty << "\n";
    }
}

int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (argc < 3) {
        cerr << "Usage:\n"
             << "  " << argv[0] << " <placement.csv> <reconstructed_boxes.csv> [--merge]\n"
             << "\nExamples:\n"
             << "  " << argv[0] << " order_3_outc++_candH.csv order_3_reconstructed.csv --merge\n";
        return 1;
    }

    string in_path = argv[1];
    string out_path = argv[2];
    bool merge_equal = false;

    for (int i = 3; i < argc; ++i) {
        string a = argv[i];
        if (a == "--merge") merge_equal = true;
        else {
            cerr << "Unknown аргумент: " << a << "\n";
            return 1;
        }
    }

    try {
        auto rows = read_placement(in_path);
        auto boxes = reconstruct_boxes(rows, merge_equal);
        write_boxes_csv(out_path, boxes);

        cerr << "OK: reconstructed " << boxes.size()
             << (merge_equal ? " unique box-types" : " boxes")
             << " -> " << out_path << "\n";
    } catch (const exception& e) {
        cerr << "ERROR: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
