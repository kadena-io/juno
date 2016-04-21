//util.js


export function prettyMoneyPrint(val) {
  if (val) {
	var sign = '';

	if (val < 0) {
	  sign = '-';
	}

	return sign + Math.abs(val).toFixed(2).replace(/\d(?=(\d{3})+\.)/g, '$&,');
  }
}
