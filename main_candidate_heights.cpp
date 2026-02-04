// main_candidate_heights.cpp
// Layer-based palletization with Candidate Heights + MaxRects in each layer
// Reads order_k.csv (k=2..16), writes order_k_outc++_candH.csv

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <chrono>

using Clock = std::chrono::high_resolution_clock;
using ms = std::chrono::duration<double, std::milli>;


struct Box {
    int weight = 0;
    int sku = 0;
    int quantity = 1;
    int length = 0;
    int width = 0;
    int height = 0;
    int base_area = 0;
    bool placed = false;

    Box() = default;
    Box(int weight_, int sku_, int q_, int l_, int w_, int h_)
        : weight(weight_), sku(sku_), quantity(q_), length(l_), width(w_), height(h_) {
        base_area = length * width;
    }

    void turn() {
        std::swap(length, width);
        base_area = length * width;
    }
};

struct Rect {
    int x=0, y=0, w=0, h=0;
};

static inline bool contains(const Rect& a, const Rect& b) {
    return b.x >= a.x && b.y >= a.y &&
           b.x + b.w <= a.x + a.w &&
           b.y + b.h <= a.y + a.h;
}

static inline bool intersects(const Rect& a, const Rect& b) {
    return !(b.x >= a.x + a.w || b.x + b.w <= a.x ||
             b.y >= a.y + a.h || b.y + b.h <= a.y);
}

static inline std::string trim(const std::string& s) {
    size_t i = 0, j = s.size();
    while (i < j && std::isspace((unsigned char)s[i])) ++i;
    while (j > i && std::isspace((unsigned char)s[j - 1])) --j;
    return s.substr(i, j - i);
}

static std::vector<std::string> split_by_comma(const std::string& line) {
    std::vector<std::string> out;
    std::string cur;
    for (char ch : line) {
        if (ch == ',') {
            out.push_back(trim(cur));
            cur.clear();
        } else {
            cur.push_back(ch);
        }
    }
    out.push_back(trim(cur));
    return out;
}

struct Pallet {
    int length = 1200;
    int width  = 800;

    int z_cur = 0;
    int layer_height_fixed = 0;   // выбранная высота слоя H
    long long total_weight = 0;
    long long total_volume = 0;

    std::vector<Rect> free_rects;

    Pallet() { reset_layer(0); }

    void reset_layer(int H) {
        layer_height_fixed = H;
        free_rects.clear();
        free_rects.push_back({0,0,length,width});
    }

    void next_layer(int H) {
        z_cur += layer_height_fixed;
        reset_layer(H);
    }

    struct Placement2D {
        bool ok=false;
        int x=0, y=0, w=0, h=0;
        bool rotated=false;
    };

    Placement2D best_area_fit(const Box& b) const {
        Placement2D best;
        long long best_waste = (1LL<<60);
        int best_short = 1e9;
        int best_y = 1e9, best_x = 1e9;

        auto consider = [&](int bw, int bh, bool rot) {
            for (const auto& fr : free_rects) {
                if (bw <= fr.w && bh <= fr.h) {
                    long long waste = 1LL*fr.w*fr.h - 1LL*bw*bh;
                    int short_side = std::min(fr.w - bw, fr.h - bh);

                    if (waste < best_waste ||
                        (waste == best_waste && short_side < best_short) ||
                        (waste == best_waste && short_side == best_short && fr.y < best_y) ||
                        (waste == best_waste && short_side == best_short && fr.y == best_y && fr.x < best_x)) {
                        best.ok = true;
                        best.x = fr.x; best.y = fr.y;
                        best.w = bw; best.h = bh;
                        best.rotated = rot;
                        best_waste = waste;
                        best_short = short_side;
                        best_y = fr.y; best_x = fr.x;
                    }
                }
            }
        };

        consider(b.length, b.width, false);
        consider(b.width, b.length, true);
        return best;
    }

    void split_free_rects_by_used(const Rect& used) {
        std::vector<Rect> new_free;
        new_free.reserve(free_rects.size()*2);

        for (const auto& fr : free_rects) {
            if (!intersects(fr, used)) {
                new_free.push_back(fr);
                continue;
            }
            if (used.x > fr.x) {
                new_free.push_back({fr.x, fr.y, used.x - fr.x, fr.h});
            }
            if (used.x + used.w < fr.x + fr.w) {
                new_free.push_back({used.x + used.w, fr.y, (fr.x + fr.w) - (used.x + used.w), fr.h});
            }
            if (used.y > fr.y) {
                new_free.push_back({fr.x, fr.y, fr.w, used.y - fr.y});
            }
            if (used.y + used.h < fr.y + fr.h) {
                new_free.push_back({fr.x, used.y + used.h, fr.w, (fr.y + fr.h) - (used.y + used.h)});
            }
        }

        // prune contained rectangles
        std::vector<Rect> pruned;
        pruned.reserve(new_free.size());
        for (int i=0;i<(int)new_free.size();++i) {
            const auto& r = new_free[i];
            if (r.w <= 0 || r.h <= 0) continue;
            bool contained_flag = false;
            for (int j=0;j<(int)new_free.size();++j) {
                if (i==j) continue;
                if (contains(new_free[j], r)) { contained_flag = true; break; }
            }
            if (!contained_flag) pruned.push_back(r);
        }
        free_rects.swap(pruned);
    }

    bool fitable_2d(const Box& b) const {
        return best_area_fit(b).ok;
    }

    // place in current layer (assumes b.height <= layer_height_fixed)
    std::vector<int> place(Box& b) {
        auto pl = best_area_fit(b);
        if (!pl.ok) return {};
        if (pl.rotated) b.turn();

        Rect used{pl.x, pl.y, pl.w, pl.h};

        std::vector<int> ans{
            b.sku,
            used.x, used.y, z_cur,
            used.x + used.w, used.y + used.h, z_cur + b.height
        };

        total_weight += b.weight;
        total_volume += 1LL * b.height * b.length * b.width;
        b.placed = true;

        split_free_rects_by_used(used);
        return ans;
    }
};

// Build up to K candidate heights from remaining boxes (simple frequency-based)
static std::vector<int> candidate_heights_topK(const std::vector<Box>& boxes, int K) {
    std::unordered_map<int,int> freq;
    freq.reserve(boxes.size()*2);

    for (const auto& b : boxes) {
        if (!b.placed) freq[b.height]++;
    }

    std::vector<std::pair<int,int>> arr;
    arr.reserve(freq.size());
    for (auto& [h,c] : freq) arr.push_back({h,c});

    // sort by frequency desc, then height desc
    std::sort(arr.begin(), arr.end(), [](auto& a, auto& b){
        if (a.second != b.second) return a.second > b.second;
        return a.first > b.first;
    });

    std::vector<int> cand;
    for (int i=0; i<(int)arr.size() && (int)cand.size() < K; ++i) {
        cand.push_back(arr[i].first);
    }

    // ensure at least one candidate
    if (cand.empty()) cand.push_back(0);
    return cand;
}

// Quick evaluation of one H: try to pack boxes with height<=H (greedy by same order) and compute "density"
static double evaluate_height_H(const std::vector<Box>& boxes, int H) {
    // lightweight pallet just for 2D packing (no global z, no totals needed)
    Pallet tmp;
    tmp.reset_layer(H);

    long long used_volume = 0;
    for (const auto& b0 : boxes) {
        if (b0.placed) continue;
        if (b0.height > H) continue;

        Box b = b0; // local copy because tmp.place may rotate
        if (!tmp.fitable_2d(b)) continue;
        tmp.place(b);
        used_volume += 1LL * b.height * b.length * b.width;
    }

    if (H <= 0) return 0.0;
    double denom = (double)tmp.length * tmp.width * (double)H;
    return (double)used_volume / denom;  // "volume fill perc" of this layer height
}

int main() {
    double sum_percolation = 0.0;
    double sum_time_ms = 0.0;

    int instances = 0;
    long long sum_boxes = 0;

    for (int k = 153; k < 154; ++k) {
        std::string in_file  = "ORDERS436/" + std::to_string(k) + ".csv";
        std::string out_file = "order_" + std::to_string(k) + "_out.csv";

        std::ifstream fin(in_file);
        if (!fin) {
            std::cerr << "Cannot open input file " << in_file << "\n";
            return 1;
        }
        std::ofstream fout(out_file);
        if (!fout) {
            std::cerr << "Cannot open output file " << out_file << "\n";
            return 1;
        }
        auto t_start = Clock::now();

        std::string line;
        std::getline(fin, line);
        std::getline(fin, line);

        std::vector<Box> boxes;
        boxes.reserve(4096);

        while (std::getline(fin, line)) {
            line = trim(line);
            if (line.empty()) continue;

            auto f = split_by_comma(line);
            if (f.size() < 6) continue;
            if (f[0].empty() || !std::isdigit((unsigned char)f[0][0])) continue;

            int sku = std::stoi(f[0]);
            int qty = std::stoi(f[1]);
            int L   = std::stoi(f[2]);
            int W   = std::stoi(f[3]);
            int H   = std::stoi(f[4]);
            int weight = std::stoi(f[5]);

            for (int i = 0; i < qty; ++i) {
                boxes.emplace_back(weight, sku, 1, L, W, H);
            }
        }

        // keep your usual order (this is your "representation"):
        std::sort(boxes.begin(), boxes.end(), [](const Box& a, const Box& b){
            if (a.base_area != b.base_area) return a.base_area > b.base_area;
            return a.height > b.height;
        });

        Pallet p;
        std::vector<std::vector<int>> placements;
        placements.reserve(boxes.size());

        int remaining = (int)boxes.size();
        int layer_count = 0;

        while (remaining > 0) {
            // 1) choose H among candidates
            const int K = 100; // small constant, keep runtime stable
            auto cands = candidate_heights_topK(boxes, K);

            int bestH = cands[0];
            double bestScore = -1.0;

            for (int H : cands) {
                double score = evaluate_height_H(boxes, H);
                if (score > bestScore) {
                    bestScore = score;
                    bestH = H;
                }
            }

            if (bestH <= 0) {
                // nothing left?
                break;
            }

            if (layer_count == 0) p.reset_layer(bestH);
            else p.next_layer(bestH);

            // 2) pack this layer "for real"
            bool placed_any = false;
            for (auto& b : boxes) {
                if (b.placed) continue;
                if (b.height > bestH) continue;

                if (!p.fitable_2d(b)) continue;

                placements.push_back(p.place(b));
                placed_any = true;
                remaining--;
            }

            layer_count++;

            if (!placed_any) {
                // could happen if bestH chosen but no box fits 2D (very rare)
                // then force choose next layer by taking the tallest remaining as H
                int maxH = 0;
                for (const auto& b : boxes) if (!b.placed) maxH = std::max(maxH, b.height);
                if (maxH == 0) break;

                p.next_layer(maxH);
                bool any2 = false;
                for (auto& b : boxes) {
                    if (b.placed) continue;
                    if (b.height > maxH) continue;
                    if (!p.fitable_2d(b)) continue;
                    placements.push_back(p.place(b));
                    any2 = true;
                    remaining--;
                }
                layer_count++;
                if (!any2) {
                    std::cerr << "[k=" << k << "] Stuck: no remaining box fits even on empty layer.\n";
                    break;
                }
            }
        }

        int total_height = p.z_cur + p.layer_height_fixed;

        double percolation = 0.0;
        if (total_height > 0) {
            percolation = (double)p.total_volume / ((double)p.length * (double)p.width * (double)total_height);
        }

        fout << 1 << "\n";
        fout << total_height << " " << std::fixed << std::setprecision(4)
             << percolation << " " << p.total_weight << "\n";

        for (const auto& v : placements) {
            for (size_t i = 0; i < v.size(); ++i) {
                if (i) fout << ' ';
                fout << v[i];
            }
            fout << "\n";
        }

        auto t_end = Clock::now();
        ms elapsed = t_end - t_start;
        sum_percolation += percolation;
        sum_time_ms += elapsed.count();
        sum_boxes += boxes.size();
        instances++;


        std::cerr
    << "[k=" << k << "] "
    << "boxes=" << boxes.size()
    << " layers=" << layer_count
    << " height=" << total_height
    << " percolation=" << std::fixed << std::setprecision(4) << percolation
    << " time_ms=" << elapsed.count()
    << "\n";

    }

    std::cerr << "\n=== SUMMARY ===\n";
    std::cerr << "Files processed: " << instances << "\n";

    std::cerr << "Average percolation: "
              << std::fixed << std::setprecision(4)
              << (sum_percolation / instances) << "\n";

    std::cerr << "Average time per file (ms): "
              << (sum_time_ms / instances) << "\n";

    std::cerr << "Average time per box (ms): "
              << (sum_time_ms / sum_boxes) << "\n";


    return 0;
}
