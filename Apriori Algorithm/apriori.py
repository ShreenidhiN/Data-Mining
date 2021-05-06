from itertools import combinations
import pandas as pd

def count_singletons(filePath):
    n_buckets = 0
    c1 = {}
    with open(filePath) as f:
        for line in f:
            bucket = line.split(" ")
            if '\n' in bucket: bucket.remove('\n')
            for item in bucket:
                itemset = frozenset({item})
                if itemset in c1.keys():
                    c1[itemset] = c1[itemset] + 1
                else:
                    c1[itemset] = 1
            n_buckets = n_buckets + 1
    return c1, n_buckets


def filter_frequent(c : dict, min_count):
    return {itemset: count for itemset, count in c.items() if count >= min_count}


def construct_candidates(frequent_itemsets, file_path):
    c = {}
    last_l = list(frequent_itemsets[-1].keys()) 
    l1  = list(frequent_itemsets[0].keys()) 
    if last_l:
        k = len(last_l[0]) + 1
    else:
        k = 1

    if len(last_l) < k :
        return {}

    with open(file_path) as f:
        for line in f:
            bucket = line.split(" ")
            if '\n' in bucket: bucket.remove('\n')

            filtered_items =  [item for item in bucket if frozenset({item}) in l1]
            filtered_items = [item for item in filtered_items if not count_presence(item, last_l) < k - 1 ]
            comb = list(combinations(filtered_items, k))
            for itemset in comb:
                if frozenset(itemset) not in c.keys():
                    subsets = list(combinations(itemset, k - 1))
                    if all(frozenset(s) in last_l for s in subsets):
                        c[frozenset(itemset)] = 1
                else:
                    c[frozenset(itemset)] = c[frozenset(itemset)] + 1
    return c


def frequent_itemsets(file_path, s) :
    c1, n_buckets = count_singletons(file_path)
    min_count = s
    frequent_itemsets = []
    c = c1
    while len(c) != 0:
        l = filter_frequent(c, min_count)
        frequent_itemsets.append(l)
        c = construct_candidates(frequent_itemsets, file_path)
    return frequent_itemsets


def count_presence(item, l):
    return sum(1 for i in l if item in i)


def output_itemsets(file_path,freq_itemsets):
    f = open(file_path, "w")
    for itemsets in freq_itemsets:
        for item in sorted(itemsets,  key=itemsets.get, reverse = True) :
            string = ' '.join(item)
            string += "  (" + str(itemsets[item]) + ")"
            f.write(string)
            f.write("\n")
        f.write("\n")
    f.close()



minsupport = int(input("Minimum Support Value: "))
input_path = input("Input file name: ")
output_path = input("Output file name: ")


print("MINIMUM SUPPORT THRESHOLD : ", minsupport)
print("Finding frequent itemsets...")
freq_itemsets = frequent_itemsets(input_path, minsupport)
output_itemsets(output_path,freq_itemsets)
print("Frequent itemsets written in " + output_path)
