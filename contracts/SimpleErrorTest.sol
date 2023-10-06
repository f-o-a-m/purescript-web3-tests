pragma solidity ^0.8.0;

contract SimpleErrorTest {
    mapping(uint => string) public table;

    string[] public names;

    function testBool(bool _arg) public pure returns (bool) {
        return !_arg;
    }
}
