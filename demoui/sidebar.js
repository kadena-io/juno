import React from 'react';

export default function Sidebar(props) {
////  if (props.data == null) {
////    return <div />;
////  }
  return (
    <aside>
		<nav className="rad-sidebar">
			<ul>
      <NavItem navId="add-payments" colorClass="rad-bg-success" navText="Add Payments" iconClass="fa-usd" {...props}/>
      <NavItem navId="london-branch" colorClass="rad-bg-danger" navText="London Branch" iconClass="fa-users" {...props}/>
      <NavItem navId="tokyo-branch" colorClass="rad-bg-primary" navText="Tokyo Branch" iconClass="fa-users" {...props}/>
      <NavItem navId="new-york-branch" colorClass="rad-bg-warning" navText="New York Branch" iconClass="fa-users" {...props}/>
			</ul>
		</nav>
	</aside>

  );
}

function NavItem({navId, navText, colorClass, iconClass, currentPane, handleChangePane}): React.Element {
  var activeLi = currentPane==navId ? "active" : "";
  return(<li id={navId} className={activeLi}>
         <a href="#" onClick={()=>handleChangePane(navId)}>
         <i className={`fa ${iconClass}`}><span className={`icon-bg ${colorClass}`}></span></i>
         <span className="rad-sidebar-item">{navText}</span>
            </a>
		 </li>);
}
