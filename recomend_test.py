# -*- coding: utf-8 -*-
"""
Recommendation test 

Created on Mon Mar 27 10:40:21 2017

@author: 116952
"""


# %% module 

import random
from collections import defaultdict
from operator import itemgetter
import pandas as pd
import numpy as np 
import math
# %% precision & recall
def PrecisionRecall(test, N):
    hit = 0
    n_recall = 0
    n_precision = 0
    for user, items in test.items():
        rank = Recommend(user, N)
        hit += len(rank & items)
        n_recall += len(items)
        n_precision += N
    return [hit / (1.0 * n_recall), hit / (1.0 * n_precision)]

# %% Gini index
def GiniIndex(p):
    '''
    p : popularity for a given item / sum of all items popularity
    '''
    j = 1
    n = len(p)
    G = 0
    for item, weight in sorted(p.items(), key=itemgetter(1)):
        G += (2 * j - n - 1) * weight
    return G / float(n - 1)

# %%  test user-based collective filtering on MovieLens small datasets
'''Small: 100,000 ratings and 1,300 tag applications applied to 9,000 movies by 700 users. 
    Last updated 10/2016.'''
## split data    
def SplitData(data, M, k, seed):
    test = []
    train = []
    random.seed(seed)
    for user, item in data:
        if random.randint(0,M) == k:
            test.append([user,item])
        else:
            train.append([user,item])
    return train, test
# %% Recall  
def Recall(train, test, N):
    hit = 0
    all = 0
    for user in train.keys():
        tu = test[user]
        rank = GetRecommendation(user, N)
        for item, pui in rank:
            if item in tu:
                hit += 1
                all += len(tu)
    return hit / (all * 1.0)
# %% Precision

def Precision(train, test, N):
    hit = 0
    all = 0
    for user in train.keys():
        tu = test[user]
        rank = GetRecommendation(user, N)
        for item, pui in rank:
            if item in tu:
                hit += 1
            all += N
    return hit / (all * 1.0)

# %% coverage 
def Coverage(train, test, N):
    recommend_items = set()
    all_items = set()
    for user in train.keys():
        for item in train[user].keys():
            all_items.add(item)
            rank = GetRecommendation(user, N)
        for item, pui in rank:
            recommend_items.add(item)
    return len(recommend_items) / (len(all_items) * 1.0)
        
    
# %% popularity

def Popularity(train, test, N):
    item_popularity = dict()
    for user, items in train.items():
        for item in items.keys():
            if item not in item_popularity:
                item_popularity[item] = 0
            item_popularity[item] += 1
    ret = 0
    n = 0
    for user in train.keys():
        rank = GetRecommendation(user, N)
        for item, pui in rank:
            ret += math.log(1 + item_popularity[item])
            n += 1
    ret /= n * 1.0
    return ret


# %% user-similarity 1. (basic)
def userSimilarity1(train):
    W = defaultdict(dict)
    for u in train.keys():
        for v in train.keys():
            if u==v:continue
            setu = set(train[u]);
            setv = set(train[v]);
            W[u][v] = len(setu & setv)
            W[u][v] /= math.sqrt(len(setu)*len(setv))        
    return W
        

# %% user - similarity 2. ()
def UserSimilarity(train):
    # build inverse table for item_users
    item_users = dict()
    for u, items in train.items():
        for i in items:
            if i not in item_users:
                item_users[i] = set()
            item_users[i].add(u)

            
    # calculate co-rated items between users    
    C = defaultdict(dict)
    N = defaultdict(int)
    for i, users in item_users.items():
        for u in users:
            N[u] += 1
            for v in users:
                if u == v:
                    continue
                try :
                    C[u][v] += 1
                except KeyError:
                    C[u][v] = 1
   
    #calculate finial similarity matrix W
    W = defaultdict(dict)
    for u, related_users in C.items():
        for v, cuv in related_users.items():
            W[u][v] = cuv / math.sqrt(N[u] * N[v])
    return W

# %%
def Recommend(user, train, W, K=3):
    rank = defaultdict(int)
    interacted_items = train[user]
    for v, wuv in list(sorted(W[u].items(), key=itemgetter(1), \
                         reverse=True))[0:K]:
        for i in train[v]:
            if i in interacted_items:
                #we should filter items user interacted before
                continue
            rank[i] += wuv
    return rank

# %% 
