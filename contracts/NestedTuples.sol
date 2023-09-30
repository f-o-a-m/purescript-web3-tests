pragma solidity ^0.8.0;

contract NestedTuples {
    struct A {
        uint a1;
        string a2;
    }

    struct B {
        string[] b1;
        bytes32 b2;
    }

    struct C {
        A c1;
        B c2;
    }

    C public c;

    function update(A calldata a, B memory b) public returns (bool) {
        c.c1 = a;
        c.c2 = b;
        return true;
    }
}
