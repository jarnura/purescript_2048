"use strict";

var matrix = null;

exports.logAny = function (value) {
	console.log(value);
	return;
};

exports.getCachedMatrix = function (just) {
  return function (nothing) {
  	console.log("matrix -> ",matrix);
  	return matrix ? just(matrix) : nothing ;
  };
};

exports.storeMatrix = function(mat){
	matrix = mat;
	return;
};

exports.random = function(highVal){
	var date = new Date();
	return (date.getTime() % highVal);
}