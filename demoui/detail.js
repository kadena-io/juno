import React from 'react';
import AddPayments from './add-payments.js';
import BranchDetail from './branch-detail.js';
import NostroDetail from './nostro-detail.js';

export default function Detail(props) {

  var deets, title, ptitle;
  switch (props.currentPane) {
  case "add-payments":
    title = "Add Payments";
    ptitle = "Swift Message Input";
    deets = (<AddPayments {...props} />); break;
  case "london-branch":
    title = "London Branch";
    ptitle = `Branch Statement ${props.londonBranch}`;
    deets = (<BranchDetail branch={props.londonBranch}
             branchAccts={[props.tesla,props.amazon]} {...props} />); break;
  case "tokyo-branch":
    title = "Tokyo Branch";
    ptitle = `Branch Statement ${props.tokyoBranch}`;
    deets = (<BranchDetail branch={props.tokyoBranch}
             branchAccts={[props.nintendo,props.sony]} {...props} />); break;
  case "new-york-branch":
    ptitle = "Nostro Accounts";
    title = "New York Branch";
    deets = (<NostroDetail {...props} />); break;
  default:
    title = "Console"; ptitle = "Console";
    deets = (<div></div>);
 }
  return (
      <main>
	   <section>
	    <div className="rad-body-wrapper">
         <div className="container-fluid">
          <header className="rad-page-title">
           <span>{title}</span>
          </header>
          <div className="row" id="grounds">
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
          </div>
	     </div>
        </div>
	   </section>
	  </main>
  );
}
