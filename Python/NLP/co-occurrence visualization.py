# -*- coding: utf-8 -*-
"""
Created on Mon Dec  7 19:48:23 2020

@author: W
"""
import re
import numpy as np
import nltk
import matplotlib.pyplot as plt
from scipy.sparse import csr_matrix

from sklearn.decomposition import TruncatedSVD
from sklearn.decomposition import PCA

a ="I am learning Natural Language Processing, which is interesting."
b = "I agree NLP\'s importance and popularity has been increasing"
c = "My colleagues and I are working on Natural Language Processing (NLP)"
d = "The teacher says:\"Learning NLP is important.\""

# remove punctuation marks and split into words
# print(a.replace(",", ""))

test = [a, b, c, d]
print(test)

def cooccur(corpus, win_size=2):

    test2=[]
    for k in range(len(corpus)):
        test2.append(re.sub("[:,.;'\"()]", " ", test[k]).split())
        #print(test2)

    # flatten the list
    fl = [y for x in test2 for y in x]
    #print(fl)

    # remove duplicates
    fl_dis=sorted(list(set(fl)))
    #print(fl_dis)
    num_dis = len(fl_dis)

    # create word indices    
    word_ind = {word: i for i, word in enumerate(fl_dis)}
    print(word_ind)

    #print(list(enumerate(a))[0][1])

    # Initialise co-occurrence matrix to all zeros
    # co_occur[word1][word2]
    co_occur = np.zeros((num_dis, num_dis))

    # Update the matrix
    for k in test2:
        for i in range(len(k)):
            word1 = k[i]
            lwr   = max(0, i-win_size)
            upr   = min(i+win_size+1, len(k))
            for j in range(lwr, upr):
                if j == i:
                    continue
                word2 = k[j]
                m_row = word_ind[word1]
                m_col = word_ind[word2]
                co_occur[m_row][m_col] += 1
    co_occur = np.matrix(co_occur)
    return co_occur, word_ind

co_matrix, word_dic = cooccur(test, 2)


'''   
x = csr_matrix(co_occur)

for i in x:
    print(i)

p_test = np.array([[1, 1], [-1, -1], [1, -1], [-1, 1], [0, 0]])
plt.plot(p_test[:,0], p_test[:,1], 'o')
'''

svd = TruncatedSVD(n_components=2, n_iter=7, random_state=42)
co_red = svd.fit_transform(co_matrix)

x = co_red[:,0]
y = co_red[:,1]

plt.clf
plt.plot(x, y, 'o')

for m, n in word_dic.items():
    plt.annotate(m, (x[n], y[n]), 
                 textcoords="offset points", # how to position the text
                 xytext=(0,10), # distance from text to points (x,y)
                 ha='center',
                 fontsize=8)

#plt.scatter(co_red[:,0], co_red[:,1])
