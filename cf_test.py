#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 12 11:44:19 2017

@author: ihong
"""
# %% 
import pandas as pd 

users_interests = [
["Hadoop", "Big Data", "HBase", "Java", "Spark", "Storm", "Cassandra"],
["NoSQL", "MongoDB", "Cassandra", "HBase", "Postgres"],
["Python", "scikit-learn", "scipy", "numpy", "statsmodels", "pandas"],
["R", "Python", "statistics", "regression", "probability"],
["machine learning", "regression", "decision trees", "libsvm"],
["Python", "R", "Java", "C++", "Haskell", "programming languages"],
["statistics", "probability", "mathematics", "theory"],
["machine learning", "scikit-learn", "Mahout", "neural networks"],
["neural networks", "deep learning", "Big Data", "artificial intelligence"],
["Hadoop", "Java", "MapReduce", "Big Data"],
["statistics", "R", "statsmodels"],
["C++", "deep learning", "artificial intelligence", "probability"],
["pandas", "R", "Python"],
["databases", "HBase", "Postgres", "MySQL", "MongoDB"],
["libsvm", "regression", "support vector machines"]
]
# %% recommendation  user based

from __future__ import division
from collections import Counter,defaultdict
import numpy as np
import math

# %% most popular topic

popular_topics = Counter([interest_topic for user in users_interests
                        for interest_topic in user]).most_common()

def most_popular_interests(user_interests,max_result=5):
    suggestions = [interest for interest,freq in popular_topics if \
                     interest not in user_interests
                     ]
    return suggestions[:max_result]

# %% try this with user no3


most_popular_interests(users_interests[2], max_result=5)

###### user-based collabitive filtering


# %% cosine similarity

# 
def cosine_similarity(v,w):
    # v,w is a vector, return the cosine between the two vectors,

    # value between -1~1

    return np.dot(v,w)/(math.sqrt(np.dot(v,v) * np.dot(w,w)))

topics = sorted(Counter([interest_topic for user in users_interests
                        for interest_topic in user]).keys())

# %% construct users interests' vector based on all topic


def user_interest_vector(user_interests):
    vectors = []
    for topic in topics:
        if topic not in user_interests:
            vectors.append(0)
        else:
            vectors.append(1)
    return vectors
# %% #
users_interests_matrix = [user_interest_vector(user_interests) for 
                            user_interests in users_interests]  
                            # same as map(user_interests_vector, users_interests)


# %% for each user construct cosine relation to every others' interests


def users_similarity(users_interests_matrix):
    users_similarity_matrix = []
    for i,user in enumerate(users_interests_matrix):
        similarity_vector = []
        for j in range(len(users_interests_matrix)):
            similarity_val=cosine_similarity(user, users_interests_matrix[j])
            # print similarity_val

            similarity_vector.append(similarity_val)
        users_similarity_matrix.append(similarity_vector)

    return users_similarity_matrix
        
sim_matrix = users_similarity(users_interests_matrix)

def most_similiar_to(userId):
    pairs=[(otherId,e) for otherId,e in enumerate(sim_matrix[userId])
     if (userId!=otherId and e!=1 and e!=0)]
    return sorted(pairs,key=lambda (_,similarity): similarity,
        reverse=True)


def user_based_suggestion(userId,include_current_interest=False):
    suggestions = defaultdict(float)
    for otherId,similarity in most_similiar_to(userId):
        for j in users_interests[otherId]:
            suggestions[j] += similarity
    
    suggestions = sorted(suggestions.items(),
        key=lambda (_,weight):weight,reverse=True)
    # 

    if include_current_interest:
        return suggestions
    else:
        return [(suggestion,weight) for suggestion,weight in 
                suggestions if suggestion not in users_interests[userId]
                ]
        
        
# %% 


