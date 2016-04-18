import React from 'react';

export default function Sidebar(props) {
////  if (props.data == null) {
////    return <div />;
////  }
  return (
    <aside>
		<nav className="rad-sidebar">
			<ul>
      <NavItem navId="add-payments" colorClass="rad-bg-success" navText="Add Payments" iconClass="fa-usd" props={props}/>
      <NavItem navId="london-branch" colorClass="rad-bg-danger" navText="London Branch" iconClass="fa-users" props={props}/>
      <NavItem navId="tokyo-branch" colorClass="rad-bg-primary" navText="Tokyo Branch" iconClass="fa-users" props={props}/>
      <NavItem navId="new-york-branch" colorClass="rad-bg-warning" navText="New York Branch" iconClass="fa-users" props={props}/>
			</ul>
		</nav>
	</aside>

  );
}

function NavItem({navId, navText, colorClass, iconClass, props}): React.Element {
  var activeLi = props.currentPane==navId ? "active" : "";
  return(<li id={navId} className={activeLi}>
         <a href="#" onClick={()=>props.handleChangePane(navId)}>
         <i className={`fa ${iconClass}`}><span className={`icon-bg ${colorClass}`}></span></i>
         <span className="rad-sidebar-item">{navText}</span>
            </a>
		 </li>);
}
