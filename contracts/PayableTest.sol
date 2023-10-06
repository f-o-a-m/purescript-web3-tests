pragma solidity ^0.8.0;

contract PayableTest {
    event Content(uint _paidContent);

    function seeContent() public payable returns (uint) {
        if (msg.value == 1000000000000000000) {
            emit Content(1);
        } else {
            emit Content(0);
        }
    }
}
