default: sg2_50khz

# Original waveforms
sg2:
	./reversepolarity.py sg2.waveforms
	./compile sg2.waveforms.reverse sg2

sg2_50khz:
	./reversepolarity.py sg2_50khz.waveforms
	./compile sg2_50hkz.waveforms.reverse sg2_50khz

# Half the I_DELAY

sg2_70khz:
	./reversepolarity.py sg2_70khz.waveforms
	./compile sg2_70hkz.waveforms.reverse sg2_70khz

# Half the I_DELAY and reduced DELAYS

sg2_87khz:
	./reversepolarity.py sg2_87khz.waveforms
	./compile sg2_87hkz.waveforms.reverse sg2_87khz

# 1/3 the I_DELAY and reduced DELAYS

sg2_100khz:
	./reversepolarity.py sg2_100khz.waveforms
	./compile sg2_100hkz.waveforms.reverse sg2_100khz

# Minimal I_DELAY to keep gain compatibile with default speed. reduced DELAYS

sg2_190khz:
	./reversepolarity.py sg2_190khz.waveforms
	./compile sg2_190hkz.waveforms.reverse sg2_190khz


# Old fast waveform. Gain not properly set.

fast:
	./reversepolarity.py fast.waveforms
	./compile fast.waveforms.reverse fast

test:
	#./reversepolarity.py test.waveforms
	#./compile test.waveforms.reverse test
	./reversepolarity.py sg2.waveforms
	./compile sg2.waveforms.reverse testsg2

