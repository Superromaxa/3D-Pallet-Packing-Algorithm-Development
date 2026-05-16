#include <algorithm>
#include <chrono>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

struct BoxSpec {
    std::string sku;
    int qty = 1;
    int L = 0, W = 0, H = 0;
    double weight = 0.0;
    int aisle = 0;
    long long volume() const { return 1LL * L * W * H; }
    int base_area() const { return L * W; }
};

struct PlacedBox {
    std::string sku;
    int x = 0, y = 0, z = 0;
    int X = 0, Y = 0, Z = 0;
    double weight = 0.0;
    int aisle = 0;
};

struct Rect {
    int x = 0, y = 0, X = 0, Y = 0;
};

struct PlacementCandidate {
    int box_idx = -1;
    int x = 0, y = 0, z = 0;
    int L = 0, W = 0, H = 0;
    double support = 0.0;
    bool com_supported = false;
    int threshold_level = 0;
    double score = std::numeric_limits<double>::infinity();
};

static constexpr int PALLET_L = 1200;
static constexpr int PALLET_W = 800;
static constexpr double TARGET_SUPPORT = 0.70;
static constexpr double RELAXED_SUPPORT = 0.60;
static constexpr double LAST_RESORT_SUPPORT = 0.50;

using Clock = std::chrono::high_resolution_clock;

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
    if (line.empty() || line.find(',') == std::string::npos) return false;
    if (line.find("SKU") != std::string::npos) return false;
    auto cols = split_csv(line);
    if (cols.size() < 6) return false;
    try {
        spec.sku = trim(cols[0]);
        spec.qty = std::stoi(cols[1]);
        spec.L = std::stoi(cols[2]);
        spec.W = std::stoi(cols[3]);
        spec.H = std::stoi(cols[4]);
        spec.weight = std::stod(cols[5]);
        spec.aisle = (cols.size() >= 8) ? std::stoi(cols[7]) : 0;
    } catch (...) {
        return false;
    }
    return !spec.sku.empty() && spec.qty > 0 && spec.L > 0 && spec.W > 0 && spec.H > 0;
}

static std::vector<BoxSpec> load_boxes(const std::string &path) {
    std::ifstream in(path);
    std::vector<BoxSpec> specs;
    if (!in) return specs;
    std::string line;
    while (std::getline(in, line)) {
        BoxSpec s;
        if (parse_line(line, s)) {
            int qty = s.qty;
            s.qty = 1;
            for (int i = 0; i < qty; ++i) {
                specs.push_back(s);
            }
        }
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

static double union_area(std::vector<Rect> rects) {
    rects.erase(std::remove_if(rects.begin(), rects.end(), [](const Rect &r) {
        return r.X <= r.x || r.Y <= r.y;
    }), rects.end());
    if (rects.empty()) return 0.0;

    std::vector<int> xs;
    xs.reserve(rects.size() * 2);
    for (const auto &r : rects) {
        xs.push_back(r.x);
        xs.push_back(r.X);
    }
    std::sort(xs.begin(), xs.end());
    xs.erase(std::unique(xs.begin(), xs.end()), xs.end());

    double area = 0.0;
    for (size_t i = 0; i + 1 < xs.size(); ++i) {
        int x0 = xs[i];
        int x1 = xs[i + 1];
        if (x1 <= x0) continue;

        std::vector<std::pair<int, int>> ys;
        for (const auto &r : rects) {
            if (r.x <= x0 && x1 <= r.X) ys.push_back({r.y, r.Y});
        }
        if (ys.empty()) continue;
        std::sort(ys.begin(), ys.end());

        int cur0 = ys[0].first;
        int cur1 = ys[0].second;
        int covered_y = 0;
        for (size_t j = 1; j < ys.size(); ++j) {
            if (ys[j].first <= cur1) {
                cur1 = std::max(cur1, ys[j].second);
            } else {
                covered_y += cur1 - cur0;
                cur0 = ys[j].first;
                cur1 = ys[j].second;
            }
        }
        covered_y += cur1 - cur0;
        area += static_cast<double>(x1 - x0) * covered_y;
    }
    return area;
}

static std::pair<double, bool> support_info(const std::vector<PlacedBox> &placed,
                                            int x, int y, int z, int X, int Y) {
    if (z == 0) return {1.0, true};

    const double base_area = static_cast<double>(X - x) * static_cast<double>(Y - y);
    if (base_area <= 0.0) return {0.0, false};

    std::vector<Rect> overlaps;
    const double cx = (x + X) * 0.5;
    const double cy = (y + Y) * 0.5;
    bool center_supported = false;

    for (const auto &b : placed) {
        if (b.Z != z) continue;
        int ix0 = std::max(x, b.x);
        int iy0 = std::max(y, b.y);
        int ix1 = std::min(X, b.X);
        int iy1 = std::min(Y, b.Y);
        if (ix1 <= ix0 || iy1 <= iy0) continue;

        overlaps.push_back({ix0, iy0, ix1, iy1});
        if (cx >= ix0 && cx <= ix1 && cy >= iy0 && cy <= iy1) {
            center_supported = true;
        }
    }

    double ratio = union_area(overlaps) / base_area;
    if (ratio < 0.0) ratio = 0.0;
    if (ratio > 1.0) ratio = 1.0;
    return {ratio, center_supported};
}

static bool can_place_without_overlap(const std::vector<PlacedBox> &placed,
                                      int x, int y, int z, int X, int Y, int Z) {
    if (!inside_pallet(x, y, X, Y)) return false;
    for (const auto &p : placed) {
        if (intersects(p, x, y, z, X, Y, Z)) return false;
    }
    return true;
}

static int current_height(const std::vector<PlacedBox> &placed) {
    int h = 0;
    for (const auto &p : placed) h = std::max(h, p.Z);
    return h;
}

static std::vector<int> unique_sorted(std::vector<int> values) {
    values.erase(std::remove_if(values.begin(), values.end(), [](int v) {
        return v < -5000 || v > 5000;
    }), values.end());
    std::sort(values.begin(), values.end());
    values.erase(std::unique(values.begin(), values.end()), values.end());
    return values;
}

static std::vector<int> candidate_axis_positions(const std::vector<PlacedBox> &placed,
                                                 int box_size, int pallet_size, bool x_axis) {
    std::vector<int> values;
    values.reserve(placed.size() * 4 + 4);
    values.push_back(0);
    values.push_back(pallet_size - box_size);
    for (const auto &p : placed) {
        int a = x_axis ? p.x : p.y;
        int b = x_axis ? p.X : p.Y;
        values.push_back(a);
        values.push_back(b);
        values.push_back(a - box_size);
        values.push_back(b - box_size);
    }
    return unique_sorted(values);
}

static std::vector<int> candidate_z_positions(const std::vector<PlacedBox> &placed) {
    std::vector<int> zs;
    zs.reserve(placed.size() + 1);
    zs.push_back(0);
    for (const auto &p : placed) zs.push_back(p.Z);
    return unique_sorted(zs);
}

static double contact_edge_bonus(const std::vector<PlacedBox> &placed,
                                 int x, int y, int z, int X, int Y, int Z) {
    double bonus = 0.0;
    if (x == 0) bonus += Y - y;
    if (y == 0) bonus += X - x;
    if (X == PALLET_L) bonus += Y - y;
    if (Y == PALLET_W) bonus += X - x;
    for (const auto &p : placed) {
        if (p.Z <= z || Z <= p.z) continue;
        int oy0 = std::max(y, p.y);
        int oy1 = std::min(Y, p.Y);
        if (oy1 > oy0 && (X == p.x || x == p.X)) bonus += oy1 - oy0;
        int ox0 = std::max(x, p.x);
        int ox1 = std::min(X, p.X);
        if (ox1 > ox0 && (Y == p.y || y == p.Y)) bonus += ox1 - ox0;
    }
    return bonus;
}

static double candidate_score(const std::vector<PlacedBox> &placed,
                              const BoxSpec &box,
                              const PlacementCandidate &c,
                              int current_max_height) {
    int new_height = std::max(current_max_height, c.z + c.H);
    double xy_center = std::abs((c.x + c.x + c.L) * 0.5 - PALLET_L * 0.5)
                     + std::abs((c.y + c.y + c.W) * 0.5 - PALLET_W * 0.5);
    double edge_bonus = contact_edge_bonus(placed, c.x, c.y, c.z, c.x + c.L, c.y + c.W, c.z + c.H);

    return static_cast<double>(new_height) * 1e12
         + static_cast<double>(c.z) * 1e9
         + static_cast<double>(c.threshold_level) * 1e8
         - c.support * 5e7
         - static_cast<double>(box.base_area()) * 1e3
         + xy_center * 100.0
         - edge_bonus * 10.0
         + c.y * 0.1
         + c.x * 0.01;
}

static bool evaluate_candidate(const std::vector<PlacedBox> &placed,
                               const BoxSpec &box,
                               int box_idx,
                               int x,
                               int y,
                               int z,
                               int L,
                               int W,
                               int threshold_level,
                               double threshold,
                               int current_max_height,
                               PlacementCandidate &best) {
    int X = x + L;
    int Y = y + W;
    int Z = z + box.H;
    if (!can_place_without_overlap(placed, x, y, z, X, Y, Z)) return false;

    auto info = support_info(placed, x, y, z, X, Y);
    if (!info.second || info.first + 1e-12 < threshold) return false;

    PlacementCandidate c;
    c.box_idx = box_idx;
    c.x = x;
    c.y = y;
    c.z = z;
    c.L = L;
    c.W = W;
    c.H = box.H;
    c.support = info.first;
    c.com_supported = info.second;
    c.threshold_level = threshold_level;
    c.score = candidate_score(placed, box, c, current_max_height);

    if (c.score < best.score) {
        best = c;
        return true;
    }
    return false;
}

static bool find_best_candidate(const std::vector<BoxSpec> &boxes,
                                const std::vector<char> &used,
                                const std::vector<PlacedBox> &placed,
                                int box_limit,
                                int threshold_level,
                                double threshold,
                                PlacementCandidate &best) {
    int considered = 0;
    int current_max_height = current_height(placed);
    auto zs = candidate_z_positions(placed);

    for (int bi = 0; bi < static_cast<int>(boxes.size()); ++bi) {
        if (used[bi]) continue;
        if (box_limit > 0 && considered >= box_limit) break;
        ++considered;

        const auto &box = boxes[bi];
        int dims[2][2] = {{box.L, box.W}, {box.W, box.L}};
        for (int o = 0; o < 2; ++o) {
            int L = dims[o][0];
            int W = dims[o][1];
            auto xs = candidate_axis_positions(placed, L, PALLET_L, true);
            auto ys = candidate_axis_positions(placed, W, PALLET_W, false);

            for (int z : zs) {
                for (int y : ys) {
                    for (int x : xs) {
                        evaluate_candidate(placed, box, bi, x, y, z, L, W,
                                           threshold_level, threshold, current_max_height, best);
                    }
                }
            }
        }
    }
    return best.box_idx >= 0;
}

static bool find_any_legal_candidate(const std::vector<BoxSpec> &boxes,
                                     const std::vector<char> &used,
                                     const std::vector<PlacedBox> &placed,
                                     PlacementCandidate &best) {
    int current_max_height = current_height(placed);
    auto zs = candidate_z_positions(placed);

    for (int bi = 0; bi < static_cast<int>(boxes.size()); ++bi) {
        if (used[bi]) continue;
        const auto &box = boxes[bi];
        int dims[2][2] = {{box.L, box.W}, {box.W, box.L}};
        for (int o = 0; o < 2; ++o) {
            int L = dims[o][0];
            int W = dims[o][1];
            auto xs = candidate_axis_positions(placed, L, PALLET_L, true);
            auto ys = candidate_axis_positions(placed, W, PALLET_W, false);
            for (int z : zs) {
                for (int y : ys) {
                    for (int x : xs) {
                        int X = x + L;
                        int Y = y + W;
                        int Z = z + box.H;
                        if (!can_place_without_overlap(placed, x, y, z, X, Y, Z)) continue;
                        auto info = support_info(placed, x, y, z, X, Y);
                        if (!info.second) continue;

                        PlacementCandidate c;
                        c.box_idx = bi;
                        c.x = x;
                        c.y = y;
                        c.z = z;
                        c.L = L;
                        c.W = W;
                        c.H = box.H;
                        c.support = info.first;
                        c.com_supported = info.second;
                        c.threshold_level = 3;
                        c.score = candidate_score(placed, box, c, current_max_height) - info.first * 1e9;
                        if (c.score < best.score) best = c;
                    }
                }
            }
        }
    }
    return best.box_idx >= 0;
}

static std::vector<PlacedBox> pack_boxes(std::vector<BoxSpec> boxes,
                                         int &relaxed60,
                                         int &relaxed50,
                                         int &below50,
                                         int &skipped) {
    std::sort(boxes.begin(), boxes.end(), [](const BoxSpec &a, const BoxSpec &b) {
        if (a.base_area() != b.base_area()) return a.base_area() > b.base_area();
        if (a.weight != b.weight) return a.weight > b.weight;
        if (a.H != b.H) return a.H > b.H;
        return a.volume() > b.volume();
    });

    std::vector<char> used(boxes.size(), 0);
    std::vector<PlacedBox> placed;
    placed.reserve(boxes.size());

    int remaining = static_cast<int>(boxes.size());
    while (remaining > 0) {
        PlacementCandidate best;

        if (!find_best_candidate(boxes, used, placed, 80, 0, TARGET_SUPPORT, best)) {
            best = PlacementCandidate{};
            find_best_candidate(boxes, used, placed, 0, 0, TARGET_SUPPORT, best);
        }
        if (best.box_idx < 0) {
            best = PlacementCandidate{};
            find_best_candidate(boxes, used, placed, 0, 1, RELAXED_SUPPORT, best);
        }
        if (best.box_idx < 0) {
            best = PlacementCandidate{};
            find_best_candidate(boxes, used, placed, 0, 2, LAST_RESORT_SUPPORT, best);
        }
        if (best.box_idx < 0) {
            best = PlacementCandidate{};
            find_any_legal_candidate(boxes, used, placed, best);
        }
        if (best.box_idx < 0) {
            skipped = remaining;
            break;
        }

        const auto &b = boxes[best.box_idx];
        placed.push_back({b.sku, best.x, best.y, best.z,
                          best.x + best.L, best.y + best.W, best.z + best.H,
                          b.weight, b.aisle});
        used[best.box_idx] = 1;
        --remaining;

        if (best.threshold_level == 1) ++relaxed60;
        else if (best.threshold_level == 2) ++relaxed50;
        else if (best.threshold_level >= 3 && best.support < LAST_RESORT_SUPPORT) ++below50;
    }
    return placed;
}

static void write_result(const std::string &path,
                         const std::vector<PlacedBox> &placed,
                         int relaxed60,
                         int relaxed50,
                         int below50,
                         int skipped) {
    long long total_volume = 0;
    double total_weight = 0.0;
    int h_total = 0;
    for (const auto &p : placed) {
        total_volume += 1LL * (p.X - p.x) * (p.Y - p.y) * (p.Z - p.z);
        total_weight += p.weight;
        h_total = std::max(h_total, p.Z);
    }

    double percolation = 0.0;
    if (h_total > 0) {
        percolation = static_cast<double>(total_volume) /
                      (static_cast<double>(PALLET_L) * PALLET_W * h_total);
    }

    std::ofstream out(path);
    out.setf(std::ios::fixed);
    out.precision(6);
    out << h_total << "," << percolation << "," << total_weight
        << ",relaxed60=" << relaxed60
        << ",relaxed50=" << relaxed50
        << ",below50=" << below50
        << ",skipped=" << skipped << "\n";
    out << "SKU,x1,y1,z1,x2,y2,z2,weight,aisle\n";
    for (const auto &p : placed) {
        out << p.sku << ','
            << p.x << ',' << p.y << ',' << p.z << ','
            << p.X << ',' << p.Y << ',' << p.Z << ','
            << p.weight << ',' << p.aisle << "\n";
    }
}

int main(int argc, char **argv) {
    std::string input_dir = "ORDERS436";
    int start = 153;
    int finish = 153;
    std::string output_prefix = "order_";

    if (argc >= 2) input_dir = argv[1];
    if (argc >= 3) start = std::stoi(argv[2]);
    if (argc >= 4) finish = std::stoi(argv[3]);
    if (argc >= 5) output_prefix = argv[4];

    for (int k = start; k <= finish; ++k) {
        std::string in_file = input_dir + "/" + std::to_string(k) + ".csv";
        std::string out_file = output_prefix + std::to_string(k) + "_out_support70.csv";
        auto boxes = load_boxes(in_file);
        if (boxes.empty()) {
            std::cerr << "[k=" << k << "] Cannot read boxes from " << in_file << "\n";
            continue;
        }

        auto t0 = Clock::now();
        int relaxed60 = 0;
        int relaxed50 = 0;
        int below50 = 0;
        int skipped = 0;
        auto placed = pack_boxes(boxes, relaxed60, relaxed50, below50, skipped);
        auto t1 = Clock::now();

        write_result(out_file, placed, relaxed60, relaxed50, below50, skipped);

        int h_total = current_height(placed);
        long long total_volume = 0;
        for (const auto &p : placed) total_volume += 1LL * (p.X - p.x) * (p.Y - p.y) * (p.Z - p.z);
        double percolation = h_total > 0
            ? static_cast<double>(total_volume) / (static_cast<double>(PALLET_L) * PALLET_W * h_total)
            : 0.0;
        std::chrono::duration<double, std::milli> elapsed = t1 - t0;

        std::cerr << "[k=" << k << "] boxes=" << boxes.size()
                  << " placed=" << placed.size()
                  << " height=" << h_total
                  << " percolation=" << std::fixed << std::setprecision(4) << percolation
                  << " relaxed60=" << relaxed60
                  << " relaxed50=" << relaxed50
                  << " below50=" << below50
                  << " skipped=" << skipped
                  << " time_ms=" << elapsed.count() << "\n";
    }
    return 0;
}
