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

    C[] public cs;

    event Update(A _a, B _b, C[] _cs);

    function update(A calldata a, B memory b) public returns (bool) {
        C memory c;
        c.c1 = a;
        c.c2 = b;
        cs.push(c);
        emit Update(a, b, cs);
        return true;
    }
}
