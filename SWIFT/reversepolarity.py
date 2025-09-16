#!/usr/bin/env python
#Reverses the polarity of the video board integrate stage

import sys
import re
#import gmpy

def switchbits(mo):
    bits = int(mo.groups()[0],2)
    #bits 2 and 3 are the polarity select
    #01 = non-inverting
    #10 = inverting
    #swap the polarity bits
    bits = bits ^ int('0001000',2)
    
    #convert back to string
    #bits = gmpy.digits(bits, 2)
    #Knicked from: http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/219300
    bstr_pos = lambda n: n>0 and bstr_pos(n>>1)+str(n&1) or ''

    bits = bstr_pos(bits)
    

    if len(bits) != 7:
        bits = (7-len(bits))*'0' + bits

    return '%'+bits


if __name__=="__main__":

    TEST = 0

    fname = sys.argv[1]
    
    lines = open(fname).readlines()
    
    out = open(fname+'.reverse', 'w')

    for line in lines:
        #Match video processing bits and substitute in line
        line = line.rstrip('\n')

        if TEST:

            match = re.search('%([0,1]{7})', line)
            if match:
                print line
                line = re.sub('%([0,1]{7})', switchbits, line)
                print line
                print
        else:
            line = re.sub('%([0,1]{7})', switchbits, line)

        out.write(line+'\n')


    out.close()


