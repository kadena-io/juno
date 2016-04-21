//util.js
import React from 'react';



export function prettyMoneyPrint(val) {
  if (val) {
	var sign = '';

	if (val < 0) {
	  sign = '-';
	}

	return sign + Math.abs(val).toFixed(2).replace(/\d(?=(\d{3})+\.)/g, '$&,');
  }
}

export function detailPane(ptitle,deets) {
  return (
           <div className="col-lg-12 col-md-6 col-xs-12">
			<div className="panel panel-default">
			 <div className="panel-heading">
			  <h3 className="panel-title"><span>{ptitle}</span>
			   <ul className="rad-panel-action">
			    <li><i className="fa fa-chevron-down"></i></li>
                <li><i className="fa fa-rotate-right"></i></li>
			    <li><i className="fa fa-close"></i></li>
			   </ul>
			  </h3>
			 </div>
             {deets}
			</div>
		   </div>
  );
}
