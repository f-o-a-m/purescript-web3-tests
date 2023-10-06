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
        A a;
        B b;
    }

    C[] public cs;

    event Update(A x, B y, C[] z);

    function update(A calldata a, B memory b) public returns (bool) {
        C memory c;
        c.a = a;
        c.b = b;
        cs.push(c);
        emit Update(a, b, cs);
        return true;
    }
}
