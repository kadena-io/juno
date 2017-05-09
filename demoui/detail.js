import React from 'react';
import AddPayments from './add-payments.js';
import BranchDetail from './branch-detail.js';
import NostroDetail from './nostro-detail.js';
import { detailPane } from './util.js';

export default function Detail(props) {

  var title,ui;
  switch (props.currentPane) {
  case "add-payments":
    title = "Add Payments";
    ui = detailPane("Swift Message Input",
                    (<AddPayments {...props} />));
    break;
  case "london-branch":
    title = "London Branch";
    ui = (<BranchDetail branch={props.londonBranch}
          branchAccts={[props.tesla,props.amazon]} {...props} />);
    break;
  case "tokyo-branch":
    title = "Tokyo Branch";
    ui = (<BranchDetail branch={props.tokyoBranch}
          branchAccts={[props.nintendo,props.sony]} {...props} />);
    break;
  case "new-york-branch":
    title = "New York Branch";
    ui = detailPane("Nostro Accounts",
                    (<NostroDetail {...props} />));
    break;
  default:
    title = "Console";
    ui = (<div></div>);
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
           {ui}
          </div>
	     </div>
        </div>
	   </section>
	  </main>
  );
}
