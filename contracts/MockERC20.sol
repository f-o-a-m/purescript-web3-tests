pragma solidity ^0.8.0;

contract MockERC20 {
    event Transfer(address indexed to, address indexed from, uint amount);

    function transfer(address to, uint amount) public {
        emit Transfer(to, msg.sender, amount);
    }
}
