#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import unittest

# expecting current dir to be test/
# so that we can find py_interface in ..
sys.path.insert(0, "..")

from py_interface.erl_term import BinaryToTerm, TermToBinary, \
    IODataToStr, StrToList

## BF = fun(T) -> io:format("b\"~s\"~n", [[io_lib:format("\\x~2.16.0b",[C]) || <<C>> <= term_to_binary(T)]]) end.

class TestString(unittest.TestCase):
    def test_unpack_abc(self):
        b = b"\x83\x6b\x00\x03\x61\x62\x63" # "abc"
        self.assertEqual(BinaryToTerm(b), 'abc')
        self.assertEqual(TermToBinary('abc'), b)

    def test_unpack_latin1_diacritic_chars(self):
        b = b"\x83\x6b\x00\x03\xe5\xe4\xf6" # "åäö" (as latin1 chars)
        self.assertEqual(BinaryToTerm(b), 'åäö')
        self.assertEqual(TermToBinary('åäö'), b)

    def test_iodata_to_list_and_back(self):
        self.assertEqual(IODataToStr([]), '')
        self.assertEqual(IODataToStr([[[],[]]]), '')
        self.assertEqual(IODataToStr([ord("a"),ord("b"), ord("c")]), 'abc')
        self.assertEqual(IODataToStr(b"abc"), 'abc')
        self.assertEqual(IODataToStr(["a",0xe5]), "aå")
        self.assertEqual(StrToList("abc"), [97,98,99])

class TestBinary(unittest.TestCase):
    def test_unpack_binary(self):
        b = b"\x83\x6d\x00\x00\x00\x03\x61\x62\x63" # <<"abc">>
        self.assertEqual(BinaryToTerm(b), b'abc')
        self.assertEqual(TermToBinary(b'abc'), b)

    def test_unpack_latin1_diacritic_chars(self):
        b = b"\x83\x6d\x00\x00\x00\x03\xe5\xe4\xf6" # <<"åäö">> (latin1 chars)
        self.assertEqual(BinaryToTerm(b), b'\xe5\xe4\xf6')
        self.assertEqual(TermToBinary(b'\xe5\xe4\xf6'), b)

if __name__ == '__main__':
    unittest.main()
