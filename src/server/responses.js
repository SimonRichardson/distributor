'use strict';

function notFound(req, res) {
    console.log('notFound');
}

function internalError(reasons) {
    return (req, res) => {
        // reasons.reverse().toArray().join("\n")
        console.log('internalError');
    };
}

function internalServerError(req, res) {
    console.log('internalServerError');
}

module.exports = {
    // 4xx
    notFound: notFound,

    // 5xx
    internalError      : internalError,
    internalServerError: internalServerError
};
