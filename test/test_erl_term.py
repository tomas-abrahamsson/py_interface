#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import unittest

# expecting current dir to be test/
# so that we can find py_interface in ..
sys.path.insert(0, "..")

from py_interface import erl_term

## BF = fun(T) -> io:format("b\"~s\"~n", [[io_lib:format("\\x~2.16.0b",[C]) || <<C>> <= term_to_binary(T)]]) end.

class TestStringUnpacking(unittest.TestCase):
    def test_unpack_abc(self):
        b = b"\x83\x6b\x00\x03\x61\x62\x63" # "abc"
        self.assertEqual(erl_term.BinaryToTerm(b), 'abc')

    def test_unpack_latin1_diacritic_chars(self):
        b = b"\x83\x6b\x00\x03\xe5\xe4\xf6" # "åäö" (as latin1 chars)
        self.assertEqual(erl_term.BinaryToTerm(b), 'åäö')

if __name__ == '__main__':
    unittest.main()
