#include <algorithm>
#include <chrono>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <numeric>
#include <random>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

static constexpr double PALLET_L = 1200.0;
static constexpr double PALLET_W = 800.0;
static constexpr double TARGET_SUPPORT = 0.70;
static constexpr double RELAXED_SUPPORT = 0.60;
static constexpr double LAST_RESORT_SUPPORT = 0.50;
static constexpr double EPS = 1e-9;

struct Box {
    int sku = 0;
    double L = 0.0;
    double W = 0.0;
    double H = 0.0;
    double weight = 0.0;
    int aisle = 0;
};

struct Placed {
    int sku = 0;
    double x1 = 0.0;
    double y1 = 0.0;
    double z1 = 0.0;
    double x2 = 0.0;
    double y2 = 0.0;
    double z2 = 0.0;
    double weight = 0.0;
    int aisle = 0;
    double support = 1.0;
};

struct PalletState {
    std::vector<Placed> placed;
    double total_volume = 0.0;
    double total_weight = 0.0;
    double max_z = 0.0;

    void add(const Placed &p) {
        placed.push_back(p);
        total_volume += (p.x2 - p.x1) * (p.y2 - p.y1) * (p.z2 - p.z1);
        total_weight += p.weight;
        max_z = std::max(max_z, p.z2);
    }
};

struct MultiState {
    std::vector<PalletState> pallets;
    double order_volume = 0.0;
    double total_volume = 0.0;
    double total_weight = 0.0;
    int relaxed60 = 0;
    int relaxed50 = 0;
    int below50 = 0;
    int skipped = 0;

    explicit MultiState(int pallet_count = 1) : pallets(std::max(1, pallet_count)) {}

    double sum_heights() const {
        double s = 0.0;
        for (const auto &p : pallets) s += p.max_z;
        return s;
    }

    int used_pallets() const {
        int used = 0;
        for (const auto &p : pallets) if (!p.placed.empty()) ++used;
        return used;
    }

    double percolation() const {
        double h = sum_heights();
        return h > EPS ? total_volume / (PALLET_L * PALLET_W * h) : 0.0;
    }

    void add(int pallet_id, const Placed &p) {
        pallets[pallet_id].add(p);
        total_volume += (p.x2 - p.x1) * (p.y2 - p.y1) * (p.z2 - p.z1);
        total_weight += p.weight;
        if (p.support + 1e-12 < TARGET_SUPPORT && p.support + 1e-12 >= RELAXED_SUPPORT) ++relaxed60;
        else if (p.support + 1e-12 < RELAXED_SUPPORT && p.support + 1e-12 >= LAST_RESORT_SUPPORT) ++relaxed50;
        else if (p.support + 1e-12 < LAST_RESORT_SUPPORT) ++below50;
    }
};

struct Candidate {
    bool ok = false;
    int pallet_id = 0;
    double x = 0.0;
    double y = 0.0;
    double z = 0.0;
    double dx = 0.0;
    double dy = 0.0;
    double dz = 0.0;
    double support = 0.0;
    double score = std::numeric_limits<double>::infinity();
};

static inline std::string trim(const std::string &s) {
    size_t a = 0;
    while (a < s.size() && std::isspace(static_cast<unsigned char>(s[a]))) ++a;
    size_t b = s.size();
    while (b > a && std::isspace(static_cast<unsigned char>(s[b - 1]))) --b;
    return s.substr(a, b - a);
}

static bool parse_positive_int_line(const std::string &line, int &value) {
    std::string t = trim(line);
    if (t.empty() || t.find(',') != std::string::npos) return false;
    for (char c : t) {
        if (!std::isdigit(static_cast<unsigned char>(c))) return false;
    }
    try {
        value = std::stoi(t);
    } catch (...) {
        return false;
    }
    return value > 0;
}

static std::vector<std::string> split_csv(const std::string &line) {
    std::vector<std::string> out;
    std::string cur;
    bool in_quotes = false;
    for (char c : line) {
        if (c == '"') in_quotes = !in_quotes;
        else if (c == ',' && !in_quotes) {
            out.push_back(trim(cur));
            cur.clear();
        } else {
            cur.push_back(c);
        }
    }
    out.push_back(trim(cur));
    return out;
}

static bool parse_order_line(const std::string &line, std::vector<Box> &boxes) {
    if (line.empty() || line.find(',') == std::string::npos || line.find("SKU") != std::string::npos) return false;
    auto c = split_csv(line);
    if (c.size() < 6 || c[0].empty() || !std::isdigit(static_cast<unsigned char>(c[0][0]))) return false;
    try {
        int sku = std::stoi(c[0]);
        int qty = std::stoi(c[1]);
        double L = std::stod(c[2]);
        double W = std::stod(c[3]);
        double H = std::stod(c[4]);
        double weight = std::stod(c[5]);
        int aisle = (c.size() >= 8) ? std::stoi(c[7]) : 0;
        for (int i = 0; i < qty; ++i) boxes.push_back({sku, L, W, H, weight, aisle});
        return true;
    } catch (...) {
        return false;
    }
}

static std::vector<Box> read_order(const std::string &path, int &pallet_count) {
    std::ifstream in(path);
    std::vector<Box> boxes;
    pallet_count = 1;
    if (!in) return boxes;

    std::string line;
    bool first_nonempty = true;
    while (std::getline(in, line)) {
        line = trim(line);
        if (line.empty()) continue;
        if (first_nonempty) {
            first_nonempty = false;
            int parsed = 1;
            if (parse_positive_int_line(line, parsed)) {
                pallet_count = parsed;
                continue;
            }
        }
        parse_order_line(line, boxes);
    }
    return boxes;
}

static double box_volume(const Box &b) {
    return b.L * b.W * b.H;
}

static inline bool overlap_1d(double a1, double a2, double b1, double b2) {
    return std::min(a2, b2) > std::max(a1, b1) + EPS;
}

static bool overlap_3d(const Placed &p, double x1, double y1, double z1, double x2, double y2, double z2) {
    return overlap_1d(p.x1, p.x2, x1, x2) &&
           overlap_1d(p.y1, p.y2, y1, y2) &&
           overlap_1d(p.z1, p.z2, z1, z2);
}

static bool overlap_xy(const Placed &p, double x1, double y1, double x2, double y2) {
    return overlap_1d(p.x1, p.x2, x1, x2) && overlap_1d(p.y1, p.y2, y1, y2);
}

static bool no_overlap(const PalletState &st, double x, double y, double z, double dx, double dy, double dz) {
    for (const auto &p : st.placed) {
        if (overlap_3d(p, x, y, z, x + dx, y + dy, z + dz)) return false;
    }
    return true;
}

static double clamp(double v, double lo, double hi) {
    return std::max(lo, std::min(v, hi));
}

static double support_ratio(const PalletState &st, double x, double y, double z, double dx, double dy) {
    if (z <= EPS) return 1.0;
    double base = dx * dy;
    if (base <= EPS) return 0.0;
    double area = 0.0;
    for (const auto &p : st.placed) {
        if (std::fabs(p.z2 - z) > 1e-7) continue;
        double ox1 = std::max(x, p.x1);
        double oy1 = std::max(y, p.y1);
        double ox2 = std::min(x + dx, p.x2);
        double oy2 = std::min(y + dy, p.y2);
        if (ox2 > ox1 && oy2 > oy1) area += (ox2 - ox1) * (oy2 - oy1);
    }
    return std::min(1.0, area / base);
}

static bool center_supported(const PalletState &st, double x, double y, double z, double dx, double dy) {
    if (z <= EPS) return true;
    double cx = x + dx * 0.5;
    double cy = y + dy * 0.5;
    for (const auto &p : st.placed) {
        if (std::fabs(p.z2 - z) > 1e-7) continue;
        double ox1 = std::max(x, p.x1);
        double oy1 = std::max(y, p.y1);
        double ox2 = std::min(x + dx, p.x2);
        double oy2 = std::min(y + dy, p.y2);
        if (ox2 <= ox1 || oy2 <= oy1) continue;
        if (cx >= ox1 - EPS && cx <= ox2 + EPS && cy >= oy1 - EPS && cy <= oy2 + EPS) return true;
    }
    return false;
}

static double drop_z(const PalletState &st, double x, double y, double dx, double dy) {
    double z = 0.0;
    for (const auto &p : st.placed) {
        if (overlap_xy(p, x, y, x + dx, y + dy)) z = std::max(z, p.z2);
    }
    return z;
}

static std::vector<std::pair<double, double>> xy_candidates(const PalletState &st, double dx, double dy) {
    std::vector<std::pair<double, double>> pts;
    auto push = [&](double x, double y) {
        if (dx > PALLET_L + EPS || dy > PALLET_W + EPS) return;
        pts.push_back({clamp(x, 0.0, PALLET_L - dx), clamp(y, 0.0, PALLET_W - dy)});
    };

    push(0, 0);
    push(PALLET_L - dx, 0);
    push(0, PALLET_W - dy);
    push(PALLET_L - dx, PALLET_W - dy);
    for (const auto &p : st.placed) {
        push(p.x1, p.y1);
        push(p.x2, p.y1);
        push(p.x1, p.y2);
        push(p.x2, p.y2);
        push(p.x1 - dx, p.y1);
        push(p.x1, p.y1 - dy);
        push(p.x2 - dx, p.y1);
        push(p.x1, p.y2 - dy);
        push(p.x2 - dx, p.y2 - dy);
    }

    std::sort(pts.begin(), pts.end(), [](const auto &a, const auto &b) {
        if (std::fabs(a.second - b.second) > 1e-7) return a.second < b.second;
        return a.first < b.first;
    });
    pts.erase(std::unique(pts.begin(), pts.end(), [](const auto &a, const auto &b) {
        return std::fabs(a.first - b.first) < 1e-7 && std::fabs(a.second - b.second) < 1e-7;
    }), pts.end());
    return pts;
}

static double edge_contact(const PalletState &st, double x, double y, double z, double dx, double dy, double dz) {
    double score = 0.0;
    if (std::fabs(x) < EPS) score += dy;
    if (std::fabs(y) < EPS) score += dx;
    if (std::fabs(x + dx - PALLET_L) < EPS) score += dy;
    if (std::fabs(y + dy - PALLET_W) < EPS) score += dx;
    for (const auto &p : st.placed) {
        if (!overlap_1d(p.z1, p.z2, z, z + dz)) continue;
        if (std::fabs(x + dx - p.x1) < EPS || std::fabs(p.x2 - x) < EPS) {
            double oy = std::min(y + dy, p.y2) - std::max(y, p.y1);
            if (oy > 0) score += oy;
        }
        if (std::fabs(y + dy - p.y1) < EPS || std::fabs(p.y2 - y) < EPS) {
            double ox = std::min(x + dx, p.x2) - std::max(x, p.x1);
            if (ox > 0) score += ox;
        }
    }
    return score;
}

static double candidate_score(const MultiState &mst,
                              const PalletState &pst,
                              const Box &b,
                              const Candidate &c) {
    double old_sum_h = mst.sum_heights();
    double new_pallet_h = std::max(pst.max_z, c.z + c.dz);
    double new_sum_h = old_sum_h - pst.max_z + new_pallet_h;
    double new_total_volume = mst.total_volume + box_volume(b);
    double global_perc = new_total_volume / (PALLET_L * PALLET_W * std::max(new_sum_h, 1e-9));

    double target_pallet_volume = mst.order_volume / std::max<size_t>(1, mst.pallets.size());
    double new_pallet_volume = pst.total_volume + box_volume(b);
    double overfill = target_pallet_volume > EPS
        ? std::max(0.0, new_pallet_volume - 1.12 * target_pallet_volume) / target_pallet_volume
        : 0.0;

    double max_h = 0.0;
    double min_h = std::numeric_limits<double>::infinity();
    for (int i = 0; i < static_cast<int>(mst.pallets.size()); ++i) {
        double h = (i == c.pallet_id) ? new_pallet_h : mst.pallets[i].max_z;
        max_h = std::max(max_h, h);
        min_h = std::min(min_h, h);
    }
    if (!std::isfinite(min_h)) min_h = 0.0;
    double height_spread = max_h - min_h;

    double contact = edge_contact(pst, c.x, c.y, c.z, c.dx, c.dy, c.dz);
    double center_penalty = std::fabs(c.x + c.dx * 0.5 - PALLET_L * 0.5) +
                            std::fabs(c.y + c.dy * 0.5 - PALLET_W * 0.5);
    double empty_bonus = pst.placed.empty() ? 150000.0 : 0.0;

    return new_sum_h * 1e7
         - global_perc * 3e7
         + c.z * 1e5
         - c.support * 2e4
         - contact * 25.0
         + center_penalty * 2.0
         + overfill * 2e7
         + height_spread * 600.0
         - empty_bonus
         + c.pallet_id * 0.001;
}

static Candidate best_for_box_on_pallet(const MultiState &mst,
                                        int pallet_id,
                                        const Box &b,
                                        double required_support) {
    const PalletState &pst = mst.pallets[pallet_id];
    Candidate best;
    double dims[2][2] = {{b.L, b.W}, {b.W, b.L}};
    for (int o = 0; o < 2; ++o) {
        double dx = dims[o][0];
        double dy = dims[o][1];
        double dz = b.H;
        if (dx > PALLET_L + EPS || dy > PALLET_W + EPS) continue;
        auto pts = xy_candidates(pst, dx, dy);
        for (const auto &[x, y] : pts) {
            double z = drop_z(pst, x, y, dx, dy);
            if (!no_overlap(pst, x, y, z, dx, dy, dz)) continue;
            double sr = support_ratio(pst, x, y, z, dx, dy);
            if (sr + 1e-12 < required_support) continue;
            if (!center_supported(pst, x, y, z, dx, dy)) continue;

            Candidate c;
            c.ok = true;
            c.pallet_id = pallet_id;
            c.x = x;
            c.y = y;
            c.z = z;
            c.dx = dx;
            c.dy = dy;
            c.dz = dz;
            c.support = sr;
            c.score = candidate_score(mst, pst, b, c);

            if (!best.ok || c.score < best.score) best = c;
        }
    }
    return best;
}

static Candidate best_for_box(const MultiState &mst, const Box &b, double required_support) {
    Candidate best;
    for (int p = 0; p < static_cast<int>(mst.pallets.size()); ++p) {
        Candidate c = best_for_box_on_pallet(mst, p, b, required_support);
        if (c.ok && (!best.ok || c.score < best.score)) best = c;
    }
    return best;
}

static MultiState decode_order(const std::vector<Box> &boxes,
                               const std::vector<int> &order,
                               int pallet_count,
                               double order_volume) {
    MultiState st(pallet_count);
    st.order_volume = order_volume;
    for (int idx : order) {
        const Box &b = boxes[idx];
        Candidate c = best_for_box(st, b, TARGET_SUPPORT);
        if (!c.ok) c = best_for_box(st, b, RELAXED_SUPPORT);
        if (!c.ok) c = best_for_box(st, b, LAST_RESORT_SUPPORT);
        if (!c.ok) {
            ++st.skipped;
            continue;
        }
        st.add(c.pallet_id, {b.sku, c.x, c.y, c.z, c.x + c.dx, c.y + c.dy, c.z + c.dz,
                             b.weight, b.aisle, c.support});
    }
    return st;
}

static double fitness(const MultiState &st, int total_boxes) {
    double placed_ratio = total_boxes > 0
        ? static_cast<double>(total_boxes - st.skipped) / static_cast<double>(total_boxes)
        : 0.0;
    int unused = static_cast<int>(st.pallets.size()) - st.used_pallets();
    return st.percolation() * 7000.0
         + placed_ratio * 2500.0
         - st.sum_heights() * 0.03
         - st.relaxed60 * 20.0
         - st.relaxed50 * 120.0
         - st.below50 * 600.0
         - st.skipped * 7000.0
         - unused * 30.0;
}

static std::vector<int> sorted_order(const std::vector<Box> &boxes) {
    std::vector<int> order(boxes.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(), [&](int a, int b) {
        double aa = boxes[a].L * boxes[a].W;
        double bb = boxes[b].L * boxes[b].W;
        if (std::fabs(aa - bb) > EPS) return aa > bb;
        if (std::fabs(boxes[a].weight - boxes[b].weight) > EPS) return boxes[a].weight > boxes[b].weight;
        return boxes[a].H > boxes[b].H;
    });
    return order;
}

static std::vector<int> height_order(const std::vector<Box> &boxes) {
    std::vector<int> order(boxes.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(), [&](int a, int b) {
        if (std::fabs(boxes[a].H - boxes[b].H) > EPS) return boxes[a].H > boxes[b].H;
        return boxes[a].L * boxes[a].W > boxes[b].L * boxes[b].W;
    });
    return order;
}

static void mutate(std::vector<int> &order, std::mt19937 &rng) {
    if (order.size() < 2) return;
    std::uniform_int_distribution<int> d(0, static_cast<int>(order.size()) - 1);
    int mode = d(rng) % 3;
    int i = d(rng);
    int j = d(rng);
    if (i > j) std::swap(i, j);
    if (mode == 0) {
        std::swap(order[i], order[j]);
    } else if (mode == 1) {
        std::reverse(order.begin() + i, order.begin() + j + 1);
    } else {
        int v = order[j];
        order.erase(order.begin() + j);
        order.insert(order.begin() + i, v);
    }
}

static std::vector<int> crossover(const std::vector<int> &a, const std::vector<int> &b, std::mt19937 &rng) {
    int n = static_cast<int>(a.size());
    std::uniform_int_distribution<int> d(0, n - 1);
    int l = d(rng), r = d(rng);
    if (l > r) std::swap(l, r);
    std::vector<int> child(n, -1);
    std::vector<char> used(n, 0);
    for (int i = l; i <= r; ++i) {
        child[i] = a[i];
        used[a[i]] = 1;
    }
    int pos = (r + 1) % n;
    for (int k = 0; k < n; ++k) {
        int v = b[(r + 1 + k) % n];
        if (used[v]) continue;
        child[pos] = v;
        used[v] = 1;
        pos = (pos + 1) % n;
    }
    return child;
}

static MultiState run_ga(const std::vector<Box> &boxes,
                         int pallet_count,
                         int population_size,
                         int generations,
                         unsigned seed) {
    double order_volume = 0.0;
    for (const auto &b : boxes) order_volume += box_volume(b);

    std::mt19937 rng(seed);
    std::vector<std::vector<int>> pop;
    pop.reserve(population_size);
    pop.push_back(sorted_order(boxes));
    pop.push_back(height_order(boxes));
    while (static_cast<int>(pop.size()) < population_size) {
        auto o = sorted_order(boxes);
        int mutations = 1 + static_cast<int>(boxes.size() / 10);
        for (int i = 0; i < mutations; ++i) mutate(o, rng);
        pop.push_back(o);
    }

    std::vector<MultiState> states;
    std::vector<double> scores(population_size, -1e100);

    auto evaluate = [&]() {
        states.clear();
        states.reserve(population_size);
        for (int i = 0; i < population_size; ++i) {
            states.push_back(decode_order(boxes, pop[i], pallet_count, order_volume));
            scores[i] = fitness(states.back(), static_cast<int>(boxes.size()));
        }
    };

    evaluate();
    for (int g = 0; g < generations; ++g) {
        std::vector<int> idx(population_size);
        std::iota(idx.begin(), idx.end(), 0);
        std::sort(idx.begin(), idx.end(), [&](int a, int b) { return scores[a] > scores[b]; });

        std::vector<std::vector<int>> next;
        next.reserve(population_size);
        next.push_back(pop[idx[0]]);
        next.push_back(pop[idx[1]]);

        std::uniform_int_distribution<int> parent_dist(0, std::max(1, population_size / 2));
        std::uniform_real_distribution<double> prob(0.0, 1.0);
        while (static_cast<int>(next.size()) < population_size) {
            int p1 = idx[parent_dist(rng)];
            int p2 = idx[parent_dist(rng)];
            auto child = crossover(pop[p1], pop[p2], rng);
            if (prob(rng) < 0.85) mutate(child, rng);
            if (prob(rng) < 0.25) mutate(child, rng);
            next.push_back(child);
        }
        pop.swap(next);
        evaluate();
    }

    int best = static_cast<int>(std::max_element(scores.begin(), scores.end()) - scores.begin());
    return states[best];
}

static void write_result(const std::string &path, const MultiState &st) {
    std::ofstream out(path);
    out.setf(std::ios::fixed);
    out << std::setprecision(6);

    out << st.pallets.size() << "\n";
    out << "global_percolation," << st.percolation()
        << ",sum_heights," << st.sum_heights()
        << ",total_weight," << st.total_weight
        << ",placed_volume," << st.total_volume
        << ",relaxed60," << st.relaxed60
        << ",relaxed50," << st.relaxed50
        << ",below50," << st.below50
        << ",skipped," << st.skipped << "\n";

    out << "Pallet,SKU,x1,y1,z1,x2,y2,z2,weight,aisle,support\n";
    for (int p = 0; p < static_cast<int>(st.pallets.size()); ++p) {
        for (const auto &b : st.pallets[p].placed) {
            out << (p + 1) << ","
                << b.sku << ","
                << b.x1 << "," << b.y1 << "," << b.z1 << ","
                << b.x2 << "," << b.y2 << "," << b.z2 << ","
                << b.weight << "," << b.aisle << "," << b.support << "\n";
        }
    }
}

int main(int argc, char **argv) {
    std::string input_dir = "ORDERS436";
    int start = 153;
    int finish = 153;
    std::string output_prefix = "order_";
    int population = 24;
    int generations = 20;
    int pallet_override = 0;

    if (argc >= 2) input_dir = argv[1];
    if (argc >= 3) start = std::stoi(argv[2]);
    if (argc >= 4) finish = std::stoi(argv[3]);
    if (argc >= 5) output_prefix = argv[4];
    if (argc >= 6) population = std::stoi(argv[5]);
    if (argc >= 7) generations = std::stoi(argv[6]);
    if (argc >= 8) pallet_override = std::stoi(argv[7]);

    for (int k = start; k <= finish; ++k) {
        std::string in_file = input_dir + "/" + std::to_string(k) + ".csv";
        int pallet_count = 1;
        auto boxes = read_order(in_file, pallet_count);
        if (pallet_override > 0) pallet_count = pallet_override;

        if (boxes.empty()) {
            std::cerr << "[k=" << k << "] Cannot read boxes from " << in_file << "\n";
            continue;
        }

        auto t0 = std::chrono::high_resolution_clock::now();
        MultiState best = run_ga(boxes, pallet_count, population, generations, 4242U + static_cast<unsigned>(k));
        auto t1 = std::chrono::high_resolution_clock::now();

        std::string out_file = output_prefix + std::to_string(k) + "_out_multi_ga70.csv";
        write_result(out_file, best);

        std::chrono::duration<double, std::milli> elapsed = t1 - t0;
        std::cerr << "[k=" << k << "] pallets=" << pallet_count
                  << " boxes=" << boxes.size()
                  << " placed=" << (boxes.size() - best.skipped)
                  << " used_pallets=" << best.used_pallets()
                  << " sum_heights=" << best.sum_heights()
                  << " global_percolation=" << std::fixed << std::setprecision(4) << best.percolation()
                  << " relaxed60=" << best.relaxed60
                  << " relaxed50=" << best.relaxed50
                  << " below50=" << best.below50
                  << " skipped=" << best.skipped
                  << " time_ms=" << elapsed.count() << "\n";
    }
    return 0;
}
