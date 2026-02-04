// pallet_analyzer.cpp
// C++17
// Reads placements CSV: SKU,x1,y1,z1,x2,y2,z2[,weight][,aisle]
// Computes pallet-level metrics + per-box support ratios.
//
// Compile: g++ -O2 -std=c++17 pallet_analyzer.cpp -o pallet_analyzer
// Run:     ./pallet_analyzer placements.csv 1200 800
//
// Notes:
// - EPS is used for z-contact matching and numeric robustness.
// - Support ratio is computed as union area of overlaps between the box base and
//   the top projections of boxes directly underneath (z2 == z1).
// - Floor (z1==0) => support=1.

#include <algorithm>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

using std::cerr;
using std::cout;
using std::string;
using std::vector;

static constexpr double EPS = 1e-9;
static constexpr double MIN_SUPPORT = 0.60;

struct Placed {
    int sku = 0;
    double x1=0, y1=0, z1=0;
    double x2=0, y2=0, z2=0;
    double weight = 1.0; // default if missing
    int aisle = 0;
};

struct Rect {
    double x1=0, y1=0, x2=0, y2=0; // axis-aligned
};

static inline double clamp0(double v) { return v > 0.0 ? v : 0.0; }

static inline double rect_area(const Rect& r) {
    return clamp0(r.x2 - r.x1) * clamp0(r.y2 - r.y1);
}

static inline Rect intersect_rect(const Rect& a, const Rect& b) {
    Rect r;
    r.x1 = std::max(a.x1, b.x1);
    r.y1 = std::max(a.y1, b.y1);
    r.x2 = std::min(a.x2, b.x2);
    r.y2 = std::min(a.y2, b.y2);
    if (r.x2 <= r.x1 || r.y2 <= r.y1) return Rect{0,0,0,0};
    return r;
}

static inline bool nearly_equal(double a, double b, double eps = 1e-7) {
    return std::fabs(a - b) <= eps;
}

static inline Rect base_rect(const Placed& p) {
    return Rect{p.x1, p.y1, p.x2, p.y2};
}

static inline double base_area(const Placed& p) {
    return clamp0(p.x2 - p.x1) * clamp0(p.y2 - p.y1);
}

static inline double volume(const Placed& p) {
    return clamp0(p.x2 - p.x1) * clamp0(p.y2 - p.y1) * clamp0(p.z2 - p.z1);
}

// ---- Union area of axis-aligned rectangles (sweep line by x) ----
static double union_area_rects(const vector<Rect>& rects) {
    if (rects.empty()) return 0.0;

    // Collect unique x coordinates
    vector<double> xs;
    xs.reserve(rects.size() * 2);
    for (const auto& r : rects) {
        if (r.x2 > r.x1 + EPS && r.y2 > r.y1 + EPS) {
            xs.push_back(r.x1);
            xs.push_back(r.x2);
        }
    }
    if (xs.empty()) return 0.0;
    std::sort(xs.begin(), xs.end());
    xs.erase(std::unique(xs.begin(), xs.end(), [](double a, double b){ return nearly_equal(a,b); }), xs.end());
    if (xs.size() < 2) return 0.0;

    double area = 0.0;

    for (size_t i = 0; i + 1 < xs.size(); ++i) {
        double xL = xs[i];
        double xR = xs[i+1];
        double dx = xR - xL;
        if (dx <= EPS) continue;

        // Collect y-intervals of rects that cover [xL,xR]
        vector<std::pair<double,double>> ys;
        ys.reserve(rects.size());
        for (const auto& r : rects) {
            if (r.x1 <= xL + EPS && r.x2 >= xR - EPS) {
                if (r.y2 > r.y1 + EPS) ys.push_back({r.y1, r.y2});
            }
        }
        if (ys.empty()) continue;

        std::sort(ys.begin(), ys.end());
        // Merge y intervals
        double merged = 0.0;
        double curL = ys[0].first, curR = ys[0].second;
        for (size_t k = 1; k < ys.size(); ++k) {
            double a = ys[k].first, b = ys[k].second;
            if (a <= curR + EPS) {
                curR = std::max(curR, b);
            } else {
                merged += (curR - curL);
                curL = a; curR = b;
            }
        }
        merged += (curR - curL);

        area += dx * merged;
    }

    return area;
}

// ---- Support ratio for one box A given all placed boxes ----
static double support_ratio_for(const Placed& A, const vector<Placed>& all) {
    if (A.z1 <= EPS) return 1.0; // on the floor

    const Rect Abase = base_rect(A);
    const double AbaseArea = rect_area(Abase);
    if (AbaseArea <= EPS) return 0.0;

    vector<Rect> overlaps;
    overlaps.reserve(16);

    // Collect rectangles of intersection between A base and top-projections of B directly below
    for (const auto& B : all) {
        if (&B == &A) continue;
        if (!nearly_equal(B.z2, A.z1, 1e-7)) continue; // direct contact
        Rect I = intersect_rect(Abase, base_rect(B));
        if (rect_area(I) > EPS) overlaps.push_back(I);
    }

    double suppArea = union_area_rects(overlaps);
    double ratio = suppArea / AbaseArea;
    // clamp numeric
    if (ratio < 0.0) ratio = 0.0;
    if (ratio > 1.0) ratio = 1.0;
    return ratio;
}

// ---- Parsing helpers ----
static inline string trim(const string& s) {
    size_t b = 0, e = s.size();
    while (b < e && std::isspace((unsigned char)s[b])) ++b;
    while (e > b && std::isspace((unsigned char)s[e-1])) --e;
    return s.substr(b, e-b);
}

static vector<string> split_csv_line(const string& line) {
    // Simple CSV split (no quoted commas). Good enough for numeric files.
    vector<string> out;
    std::stringstream ss(line);
    string tok;
    while (std::getline(ss, tok, ',')) out.push_back(trim(tok));
    return out;
}

static bool looks_like_header(const vector<string>& cols) {
    // Detect if first column is not numeric
    if (cols.empty()) return false;
    const string& c0 = cols[0];
    bool has_digit = false;
    for (char ch : c0) if (std::isdigit((unsigned char)ch)) { has_digit = true; break; }
    // if contains letters, assume header
    bool has_alpha = false;
    for (char ch : c0) if (std::isalpha((unsigned char)ch)) { has_alpha = true; break; }
    return has_alpha && !has_digit;
}

static vector<Placed> read_placements_csv(const string& path) {
    std::ifstream fin(path);
    if (!fin) throw std::runtime_error("Cannot open input file: " + path);

    vector<Placed> v;
    string line;
    bool first = true;

    while (std::getline(fin, line)) {
        line = trim(line);
        if (line.empty()) continue;

        auto cols = split_csv_line(line);
        if (cols.size() < 7) {
            // try whitespace split fallback
            cols.clear();
            std::stringstream ss(line);
            string t;
            while (ss >> t) cols.push_back(t);
        }
        if (cols.size() < 7) continue; // skip garbage

        if (first && looks_like_header(cols)) {
            first = false;
            continue; // skip header
        }
        first = false;

        Placed p;
        try {
            p.sku = std::stoi(cols[0]);
            p.x1  = std::stod(cols[1]);
            p.y1  = std::stod(cols[2]);
            p.z1  = std::stod(cols[3]);
            p.x2  = std::stod(cols[4]);
            p.y2  = std::stod(cols[5]);
            p.z2  = std::stod(cols[6]);
            if (cols.size() >= 8) p.weight = std::stod(cols[7]);
            if (cols.size() >= 9) p.aisle  = std::stoi(cols[8]);
        } catch (...) {
            continue; // skip bad line
        }

        // normalize coords if needed
        if (p.x2 < p.x1) std::swap(p.x1, p.x2);
        if (p.y2 < p.y1) std::swap(p.y1, p.y2);
        if (p.z2 < p.z1) std::swap(p.z1, p.z2);

        v.push_back(p);
    }

    return v;
}

// ---- Overlap checks ----
static inline bool overlap_1d(double a1, double a2, double b1, double b2) {
    return std::min(a2, b2) > std::max(a1, b1) + EPS;
}

static inline bool overlap_3d(const Placed& a, const Placed& b) {
    return overlap_1d(a.x1,a.x2,b.x1,b.x2) &&
           overlap_1d(a.y1,a.y2,b.y1,b.y2) &&
           overlap_1d(a.z1,a.z2,b.z1,b.z2);
}

int main(int argc, char** argv) {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    if (argc < 4) {
        cerr << "Usage: " << argv[0] << " <placements.csv> <pallet_L> <pallet_W>\n";
        cerr << "CSV format: SKU,x1,y1,z1,x2,y2,z2[,weight][,aisle]\n";
        return 1;
    }

    const string in_path = argv[1];
    const double PALLET_L = std::stod(argv[2]);
    const double PALLET_W = std::stod(argv[3]);
    const double PALLET_AREA = PALLET_L * PALLET_W;

    vector<Placed> placed;
    try {
        placed = read_placements_csv(in_path);
    } catch (const std::exception& e) {
        cerr << "ERROR: " << e.what() << "\n";
        return 1;
    }

    if (placed.empty()) {
        cerr << "No placements read.\n";
        return 1;
    }

    // Pallet-level aggregates
    double max_height = 0.0;
    double vol_sum = 0.0;

    // Center of mass in XY (weighted by weight; if weight missing -> 1)
    double w_sum = 0.0;
    double com_x = 0.0, com_y = 0.0;

    // Constraint checks
    int out_of_bounds = 0;
    int non_positive_dims = 0;

    for (const auto& p : placed) {
        max_height = std::max(max_height, p.z2);
        double v = volume(p);
        vol_sum += v;

        double w = (p.weight > 0.0 ? p.weight : 1.0);
        double cx = 0.5 * (p.x1 + p.x2);
        double cy = 0.5 * (p.y1 + p.y2);
        w_sum += w;
        com_x += w * cx;
        com_y += w * cy;

        if (p.x1 < -EPS || p.y1 < -EPS || p.x2 > PALLET_L + EPS || p.y2 > PALLET_W + EPS) {
            out_of_bounds++;
        }
        if ((p.x2 - p.x1) <= EPS || (p.y2 - p.y1) <= EPS || (p.z2 - p.z1) <= EPS) {
            non_positive_dims++;
        }
    }

    if (w_sum > 0.0) {
        com_x /= w_sum;
        com_y /= w_sum;
    }

    // Overlap count (O(n^2) - ok for typical pallet sizes)
    long long overlap_pairs = 0;
    for (size_t i = 0; i < placed.size(); ++i) {
        for (size_t j = i + 1; j < placed.size(); ++j) {
            if (overlap_3d(placed[i], placed[j])) overlap_pairs++;
        }
    }

    // Support ratios
    vector<double> supports(placed.size(), 0.0);
    int floating = 0;
    int good_support = 0;
    double support_sum = 0.0;
    double support_min = 1.0;

    for (size_t i = 0; i < placed.size(); ++i) {
        double s = support_ratio_for(placed[i], placed);
        supports[i] = s;
        support_sum += s;
        support_min = std::min(support_min, s);

        if (placed[i].z1 > EPS && s <= EPS) floating++;
        if (s + 1e-12 >= MIN_SUPPORT) good_support++;
    }

    const double support_avg = support_sum / (double)placed.size();
    const double fill_ratio = (max_height > EPS) ? (vol_sum / (PALLET_AREA * max_height)) : 0.0;

    // Print results
    cout << std::fixed << std::setprecision(6);
    cout << "=== Pallet analysis ===\n";
    cout << "Boxes:                 " << placed.size() << "\n";
    cout << "Max height:            " << max_height << "\n";
    cout << "Total volume:          " << vol_sum << "\n";
    cout << "Volume fill ratio:     " << fill_ratio << "   (vol / (pallet_area * max_height))\n";
    cout << "Support avg:           " << support_avg << "\n";
    cout << "Support min:           " << support_min << "\n";
    cout << "Boxes support>=0.60:   " << good_support << " (" << (100.0 * good_support / (double)placed.size()) << "%)\n";
    cout << "Floating boxes:        " << floating << "\n";
    cout << "Out of bounds:         " << out_of_bounds << "\n";
    cout << "Non-positive dims:     " << non_positive_dims << "\n";
    cout << "Overlap pairs (3D):    " << overlap_pairs << "\n";
    cout << "Center of mass (XY):   (" << com_x << ", " << com_y << ")\n";
    cout << "COM inside base:       "
         << ((com_x >= -EPS && com_x <= PALLET_L + EPS && com_y >= -EPS && com_y <= PALLET_W + EPS) ? "YES" : "NO")
         << "\n";

    // Optional: dump per-box supports
    /*cout << "\n=== Per-box support (first 50) ===\n";
    size_t lim = std::min<size_t>(50, placed.size());
    for (size_t i = 0; i < lim; ++i) {
        cout << "sku=" << placed[i].sku
             << " z1=" << placed[i].z1
             << " support=" << supports[i]
             << ((placed[i].z1 > EPS && supports[i] <= EPS) ? "  [FLOAT]" : "")
             << "\n";
    }*/

    return 0;
}
