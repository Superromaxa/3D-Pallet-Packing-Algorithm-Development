#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

struct BoxSpec {
    std::string sku;
    int qty = 1;
    int L = 0, W = 0, H = 0;
    double weight = 0.0;
    int aisle = 0;
};

struct PlacedBox {
    std::string sku;
    int x = 0, y = 0, z = 0;
    int X = 0, Y = 0, Z = 0;
    double weight = 0.0;
    int aisle = 0;
};

static const int PALLET_L = 1200;
static const int PALLET_W = 800;

static inline std::string trim(const std::string &s) {
    size_t a = 0;
    while (a < s.size() && std::isspace(static_cast<unsigned char>(s[a]))) ++a;
    size_t b = s.size();
    while (b > a && std::isspace(static_cast<unsigned char>(s[b - 1]))) --b;
    return s.substr(a, b - a);
}

static std::vector<std::string> split_csv(const std::string &line) {
    std::vector<std::string> out;
    std::string cur;
    bool in_quotes = false;
    for (char c : line) {
        if (c == '"') {
            in_quotes = !in_quotes;
        } else if (c == ',' && !in_quotes) {
            out.push_back(trim(cur));
            cur.clear();
        } else {
            cur.push_back(c);
        }
    }
    out.push_back(trim(cur));
    return out;
}

static bool parse_line(const std::string &line, BoxSpec &spec) {
    if (line.empty()) return false;
    if (line.find(',') == std::string::npos) return false;
    if (line.find("SKU") != std::string::npos) return false;
    auto cols = split_csv(line);
    if (cols.size() < 8) return false;
    spec.sku = trim(cols[0]);
    if (spec.sku.empty()) return false;
    try {
        spec.qty = std::stoi(cols[1]);
        spec.L = std::stoi(cols[2]);
        spec.W = std::stoi(cols[3]);
        spec.H = std::stoi(cols[4]);
        spec.weight = std::stod(cols[5]);
        spec.aisle = std::stoi(cols[7]);
    } catch (...) {
        return false;
    }
    if (spec.qty < 1 || spec.L <= 0 || spec.W <= 0 || spec.H <= 0) return false;
    return true;
}

static std::vector<BoxSpec> load_boxes(const std::string &path) {
    std::ifstream in(path);
    std::vector<BoxSpec> specs;
    if (!in) return specs;
    std::string line;
    while (std::getline(in, line)) {
        BoxSpec s;
        if (parse_line(line, s)) specs.push_back(s);
    }
    return specs;
}

static bool intersects(const PlacedBox &a, int x, int y, int z, int X, int Y, int Z) {
    if (a.X <= x || X <= a.x) return false;
    if (a.Y <= y || Y <= a.y) return false;
    if (a.Z <= z || Z <= a.z) return false;
    return true;
}

static bool inside_pallet(int x, int y, int X, int Y) {
    return x >= 0 && y >= 0 && X <= PALLET_L && Y <= PALLET_W;
}

static std::pair<double, bool> support_ratio_com(const std::vector<PlacedBox> &placed,
                                                 int x, int y, int z, int X, int Y) {
    if (z == 0) return {1.0, true};
    long long base_area = 1LL * (X - x) * (Y - y);
    if (base_area <= 0) return {0.0, false};

    long long support_area = 0;
    bool com_supported = false;
    double cx = (x + X) * 0.5;
    double cy = (y + Y) * 0.5;

    for (const auto &b : placed) {
        if (b.Z != z) continue;
        int ix0 = std::max(x, b.x);
        int iy0 = std::max(y, b.y);
        int ix1 = std::min(X, b.X);
        int iy1 = std::min(Y, b.Y);
        if (ix1 <= ix0 || iy1 <= iy0) continue;
        support_area += 1LL * (ix1 - ix0) * (iy1 - iy0);
        if (!com_supported) {
            if (cx > ix0 && cx < ix1 && cy > iy0 && cy < iy1) com_supported = true;
        }
    }

    double ratio = static_cast<double>(support_area) / static_cast<double>(base_area);
    return {ratio, com_supported};
}

static bool stable_on_support_60(const std::vector<PlacedBox> &placed, int x, int y, int z, int X, int Y) {
    auto info = support_ratio_com(placed, x, y, z, X, Y);
    if (z == 0) return true;
    return info.second && info.first >= 0.60;
}

static void collect_levels_and_edges(const std::vector<PlacedBox> &placed,
                                     std::vector<int> &xs,
                                     std::vector<int> &ys,
                                     std::vector<int> &zs) {
    xs.clear();
    ys.clear();
    zs.clear();
    xs.push_back(0);
    ys.push_back(0);
    zs.push_back(0);
    for (const auto &p : placed) {
        xs.push_back(p.x);
        xs.push_back(p.X);
        ys.push_back(p.y);
        ys.push_back(p.Y);
        zs.push_back(p.Z);
    }
    auto uniq = [](std::vector<int> &v) {
        std::sort(v.begin(), v.end());
        v.erase(std::unique(v.begin(), v.end()), v.end());
    };
    uniq(xs);
    uniq(ys);
    uniq(zs);
}

int main() {
    for (int k = 153; k < 154; ++k) {
        std::string in_file = "ORDERS436" + std::to_string(k) + ".csv";
        std::string out_file = "order_" + std::to_string(k) + "_out.csv";

        auto specs = load_boxes(in_file);
        std::vector<BoxSpec> boxes;
        for (const auto &s : specs) {
            for (int i = 0; i < s.qty; ++i) boxes.push_back(s);
        }

        std::sort(boxes.begin(), boxes.end(), [](const BoxSpec &a, const BoxSpec &b) {
            long long aa = 1LL * a.L * a.W;
            long long bb = 1LL * b.L * b.W;
            if (aa != bb) return aa > bb;
            if (a.weight != b.weight) return a.weight > b.weight;
            return a.H > b.H;
        });

        std::vector<PlacedBox> placed;
        placed.reserve(boxes.size());

        for (const auto &box : boxes) {
            std::vector<int> xs, ys, zs;
            collect_levels_and_edges(placed, xs, ys, zs);

            PlacedBox best;
            bool found = false;
            double best_score = std::numeric_limits<double>::infinity();

            int dims[2][2] = {{box.L, box.W}, {box.W, box.L}};
            for (int o = 0; o < 2; ++o) {
                int L = dims[o][0];
                int W = dims[o][1];
                int H = box.H;

                for (int z : zs) {
                    for (int y : ys) {
                        for (int x : xs) {
                            int X = x + L, Y = y + W, Z = z + H;
                            if (!inside_pallet(x, y, X, Y)) continue;

                            bool ok = true;
                            for (const auto &p : placed) {
                                if (intersects(p, x, y, z, X, Y, Z)) { ok = false; break; }
                            }
                            if (!ok) continue;
                            if (!stable_on_support_60(placed, x, y, z, X, Y)) continue;

                            auto info = support_ratio_com(placed, x, y, z, X, Y);
                            double support_ratio = info.first;
                            double score = static_cast<double>(z + H) * 1e9
                                           + static_cast<double>(y) * 1e3
                                           + static_cast<double>(x)
                                           - support_ratio * 1e5;
                            if (score < best_score) {
                                best_score = score;
                                best = {box.sku, x, y, z, X, Y, Z, box.weight, box.aisle};
                                found = true;
                            }
                        }
                    }
                }
            }

            if (found) {
                placed.push_back(best);
            }
        }

        long long total_volume = 0;
        double total_weight = 0.0;
        int h_total = 0;
        for (const auto &p : placed) {
            total_volume += 1LL * (p.X - p.x) * (p.Y - p.y) * (p.Z - p.z);
            total_weight += p.weight;
            if (p.Z > h_total) h_total = p.Z;
        }
        double percolation = 0.0;
        if (h_total > 0) {
            percolation = static_cast<double>(total_volume) / (static_cast<double>(PALLET_L) * PALLET_W * h_total);
        }

        std::ofstream out(out_file);
        out.setf(std::ios::fixed);
        out.precision(6);
        out << h_total << "," << percolation << "," << total_weight << "\n";
        for (const auto &p : placed) {
            out << p.sku << ',' << p.x << ',' << p.y << ',' << p.z << ','
                << p.X << ',' << p.Y << ',' << p.Z << ','
                 << "\n";
        }
    }

    return 0;
}
