/// 1-4
/// P13
/// Observe that our model of evaluation allows for combinations whose
/// operators are compound expressions. Use this observation to describe the
/// behavior of the following procedure:

const add = (a, b) => a + b;
const sub = (a, b) => a - b;

module.exports.aPlusAbsB = (a, b) => (b < 0? sub: add)(a, b);

