import React from 'react';

export default function HeaderNav(props) {
  return (
	<section>
		<header>
			<nav className="rad-navigation">
				<div className="rad-logo-container">
					<a href="#" className="rad-logo">Payments Ledger</a>
					<a href="#" className="rad-toggle-btn pull-right"><i className="fa fa-bars"></i></a>
				</div>
				<a href="#" className="rad-logo-hidden">Payments Ledger</a>
				<div className="rad-top-nav-container">
					<a href="" className="brand-icon"><i className="fa fa-recycle"></i></a>
					<ul className="pull-right links">
						<li className="rad-dropdown"><a className="rad-menu-item" href="#"><i className="fa fa-cog"></i></a>
							<ul className="rad-dropmenu-item rad-settings">
								<li className="rad-dropmenu-header"><a href="#">Settings</a></li>
								<li className="rad-notification-item text-left">
									<div className="pull-left"><i className="fa fa-link"></i></div>
									<div className="pull-right">
										<label className="rad-chk-pin pull-right">
											<input type="checkbox" />
											<span></span>
										</label>
									</div>
									<div className="rad-notification-body">
										<div className="lg-text">Change to Flat Theme</div>
										<div className="sm-text">Flattify it</div>
									</div>
								</li>
								<li id="rad-color-opts" className="rad-notification-item text-left hide">
									<div className="pull-left"><i className="fa fa-puzzle-piece"></i></div>
									<div className="pull-right">
										<div className="rad-color-swatch">
											<label className="colors rad-bg-crimson rad-option-selected">
												<input type="radio" checked name="color" value="crimson" />
											</label>
											<label className="colors rad-bg-teal">
												<input type="radio" name="color" value="teal" />
											</label>
											<label className="colors rad-bg-purple">
												<input type="radio" name="color" value="purple" />
											</label>
											<label className="colors rad-bg-orange">
												<input type="radio" name="color" value="orange" />
											</label>
											<label className="colors rad-bg-twitter">
												<input type="radio" name="color" value="twitter" />
											</label>
										</div>
									</div>
									<div className="rad-notification-body">
										<div className="lg-text">Choose a color</div>
										<div className="sm-text">Make it colorful</div>
									</div>
								</li>
							</ul>
						</li>
					</ul>
				</div>
			</nav>
		</header>
	</section>
  );
}
