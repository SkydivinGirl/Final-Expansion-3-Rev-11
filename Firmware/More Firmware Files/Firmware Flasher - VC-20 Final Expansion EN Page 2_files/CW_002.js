/**
 * @author	Alexander Ebert
 * @copyright	2001-2013 WoltLab GmbH
 * @license	GNU Lesser General Public License <http://opensource.org/licenses/lgpl-license.php>
 * @modified Christopher Walz
 */
if (!CW) var CW = {};
CW.ExtendedDropdown = {
	/**
	 * list of callbacks
	 * @var	object
	 */
	_callbacks: { },

	/**
	 * initialization state
	 * @var	boolean
	 */
	_didInit: false,

	/**
	 * list of registered dropdowns
	 * @var	object
	 */
	_dropdowns: { },

	/**
	 * container for dropdown menus
	 * @var	object
	 */
	_menuContainer: null,

	/**
	 * list of registered dropdown menus
	 * @var	object
	 */
	_menus: { },

	/**
	 * animation type
	 * @var	string
	 */
	_animation: 'default',

	/**
	 * animation speed
	 * @var	int
	 */
	_animationSpeed: 200,

	/**
	 * Initializes dropdowns.
	 */
	init: function(options) {
		var options = $.extend(true, {
			animation: this._animation,
			animationSpeed: this._animationSpeed,
			enableMobile: 1
		}, options);

		if (options.enableMobile == 0 && $.browser.mobile)
			return;

		this._animation = options.animation;
		this._animationSpeed = options.animationSpeed;

		if (this._menuContainer === null) {
			this._menuContainer = $('<div id="dropdownMenuContainer" />').appendTo(document.body);
		}

		var self = this;
		$('.extendedDropdownToggle:not(.jsExtendedDropdownEnabled)').each(function(index, button) {
			self.initDropdown($(button));
		});

		if (!this._didInit) {
			this._didInit = true;

			WCF.CloseOverlayHandler.addCallback('CW.ExtendedDropdown', $.proxy(this._closeAll, this));
			WCF.DOMNodeInsertedHandler.addCallback('CW.ExtendedDropdown', $.proxy(this.init, this));
			$(document).on('scroll', $.proxy(this._scroll, this));
		}
	},

	/**
	 * Handles dropdown positions in overlays when scrolling in the overlay.
	 *
	 * @param	object		event
	 */
	_dialogScroll: function(event) {
		var $dialogContent = $(event.currentTarget);
		$dialogContent.find('.extendedDropdown.dropdownOpen').each(function(index, element) {
			var $dropdown = $(element);
			var $dropdownID = $dropdown.wcfIdentify();
			var $dropdownOffset = $dropdown.offset();
			var $dialogContentOffset = $dialogContent.offset();

			var $verticalScrollTolerance = $(element).height() / 2;

			// check if dropdown toggle is still (partially) visible
			if ($dropdownOffset.top + $verticalScrollTolerance <= $dialogContentOffset.top) {
				// top check
				CW.ExtendedDropdown.toggleDropdown($dropdownID);
			}
			else if ($dropdownOffset.top >= $dialogContentOffset.top + $dialogContent.height()) {
				// bottom check
				CW.ExtendedDropdown.toggleDropdown($dropdownID);
			}
			else if ($dropdownOffset.left <= $dialogContentOffset.left) {
				// left check
				CW.ExtendedDropdown.toggleDropdown($dropdownID);
			}
			else if ($dropdownOffset.left >= $dialogContentOffset.left + $dialogContent.width()) {
				// right check
				CW.ExtendedDropdown.toggleDropdown($dropdownID);
			}
			else {
				CW.ExtendedDropdown.setAlignmentByID($dropdown.wcfIdentify());
			}
		});
	},

	/**
	 * Handles dropdown positions in overlays when scrolling in the document.
	 *
	 * @param	object		event
	 */
	_scroll: function(event) {
		for (var $containerID in this._dropdowns) {
			var $dropdown = this._dropdowns[$containerID];
			if ($dropdown.data('isOverlayDropdownButton') && $dropdown.hasClass('dropdownOpen')) {
				this.setAlignmentByID($containerID);
			}
		}
	},

	/**
	 * Initializes a dropdown.
	 *
	 * @param	jQuery		button
	 */
	initDropdown: function(button) {
		if (button.hasClass('jsExtendedDropdownEnabled') || button.data('target')) {
			return;
		}

		var $dropdown = button.parents('.extendedDropdown');
		$dropdown.addClass('dropdown');
		if (!$dropdown.length) {
			// broken dropdown, ignore
			console.debug("[CW.ExtendedDropdown] Invalid dropdown passed, button '" + button.wcfIdentify() + "' does not have a parent with .dropdown, aborting.");
			return;
		}

		var $dropdownMenu = button.next('.extendedDropdownMenu');
		$dropdownMenu.addClass('dropdownMenu');
		if (!$dropdownMenu.length) {
			// broken dropdown, ignore
			console.debug("[CW.ExtendedDropdown] Invalid dropdown passed, dropdown '" + $dropdown.wcfIdentify() + "' does not have a dropdown menu, aborting.");
			return;
		}

		$dropdownMenu.detach().appendTo(this._menuContainer);
		var $containerID = $dropdown.wcfIdentify();
		if (!this._dropdowns[$containerID]) {
			if (jQuery.browser.mobile) {
				button.addClass('jsExtendedDropdownEnabled').click($.proxy(this._toggle, this));
			}
			else {
				button.addClass('jsExtendedDropdownEnabled').hover($.proxy(this._hover, this), $.proxy(this._hoverOut, this));
				$dropdownMenu.mouseleave($.proxy(this._hoverOut, this));
			}

			this._dropdowns[$containerID] = $dropdown;
			this._menus[$containerID] = $dropdownMenu;
		}

		button.data('target', $containerID);
		$dropdownMenu.data('target', $containerID);
	},

	/**
	 * Removes the dropdown with the given container id.
	 *
	 * @param	string		containerID
	 */
	removeDropdown: function(containerID) {
		if (this._menus[containerID]) {
			$(this._menus[containerID]).remove();
			delete this._menus[containerID];
			delete this._dropdowns[containerID];
		}
	},

	/**
	 * Initializes a dropdown fragment which behaves like a usual dropdown
	 * but is not controlled by a trigger element.
	 *
	 * @param	jQuery		dropdown
	 * @param	jQuery		dropdownMenu
	 */
	initDropdownFragment: function(dropdown, dropdownMenu) {
		var $containerID = dropdown.wcfIdentify();
		if (this._dropdowns[$containerID]) {
			console.debug("[CW.ExtendedDropdown] Cannot register dropdown identified by '" + $containerID + "' as a fragement.");
			return;
		}

		this._dropdowns[$containerID] = dropdown;
		this._menus[$containerID] = dropdownMenu.detach().appendTo(this._menuContainer);
	},

	/**
	 * Registers a callback notified upon dropdown state change.
	 *
	 * @param	string		identifier
	 * @var		object		callback
	 */
	registerCallback: function(identifier, callback) {
		if (!$.isFunction(callback)) {
			console.debug("[CW.ExtendedDropdown] Callback for '" + identifier + "' is invalid");
			return false;
		}

		if (!this._callbacks[identifier]) {
			this._callbacks[identifier] = [ ];
		}

		this._callbacks[identifier].push(callback);
	},

	/**
	 * Toggles a dropdown.
	 *
	 * @param	object		event
	 * @param	string		targetID
	 */
	_toggle: function(event, targetID) {
		var $targetID = (event === null) ? targetID : $(event.currentTarget).data('target');

		// check if 'isOverlayDropdownButton' is set which indicates if
		// the dropdown toggle is in an overlay
		var $target = this._dropdowns[$targetID];
		if ($target && $target.data('isOverlayDropdownButton') === undefined) {
			var $dialogContent = $target.parents('.dialogContent');
			$target.data('isOverlayDropdownButton', $dialogContent.length > 0);

			if ($dialogContent.length) {
				$dialogContent.on('scroll', this._dialogScroll);
			}
		}

		// close all dropdowns
		for (var $containerID in this._dropdowns) {
			var $dropdown = this._dropdowns[$containerID];
			var $dropdownMenu = this._menus[$containerID];

			if ($dropdown.hasClass('dropdownOpen')) {
				$dropdown.removeClass('dropdownOpen');
				this._menuHide($dropdownMenu);

				this._notifyCallbacks($containerID, 'close');
			}
			else if ($containerID === $targetID) {
				$dropdown.addClass('dropdownOpen');
				this._menuShow($dropdownMenu);

				this._notifyCallbacks($containerID, 'open');

				this.setAlignment($dropdown, $dropdownMenu);
			}
		}

		if (event !== null) {
			event.stopPropagation();
			return false;
		}
	},

	/**
	 * Toggles a dropdown.
	 *
	 * @param	string		containerID
	 */
	toggleDropdown: function(containerID) {
		this._toggle(null, containerID);
	},

	/**
	 * Returns dropdown by container id.
	 *
	 * @param	string		containerID
	 * @return	jQuery
	 */
	getDropdown: function(containerID) {
		if (this._dropdowns[containerID]) {
			return this._dropdowns[containerID];
		}

		return null;
	},

	/**
	 * Returns dropdown menu by container id.
	 *
	 * @param	string		containerID
	 * @return	jQuery
	 */
	getDropdownMenu: function(containerID) {
		if (this._menus[containerID]) {
			return this._menus[containerID];
		}

		return null;
	},

	/**
	 * Sets alignment for given container id.
	 *
	 * @param	string		containerID
	 */
	setAlignmentByID: function(containerID) {
		var $dropdown = this.getDropdown(containerID);
		if ($dropdown === null) {
			console.debug("[CW.ExtendedDropdown] Unable to find dropdown identified by '" + containerID + "'");
		}

		var $dropdownMenu = this.getDropdownMenu(containerID);
		if ($dropdownMenu === null) {
			console.debug("[CW.ExtendedDropdown] Unable to find dropdown menu identified by '" + containerID + "'");
		}

		this.setAlignment($dropdown, $dropdownMenu);
	},

	/**
	 * Sets alignment for dropdown.
	 *
	 * @param	jQuery		dropdown
	 * @param	jQuery		dropdownMenu
	 */
	setAlignment: function(dropdown, dropdownMenu) {
		// force dropdown menu to be placed in the upper left corner, otherwise
		// it might cause the calculations to be a bit off if the page exceeds
		// the window boundaries during getDimensions() making it visible
		if (!dropdownMenu.data('isInitialized')) {
			dropdownMenu.data('isInitialized', true).css({ left: 0, top: 0 });
		}

		// get dropdown position
		var $dropdownDimensions = dropdown.getDimensions('outer');
		var $dropdownOffsets = dropdown.getOffsets('offset');
		var $menuDimensions = dropdownMenu.getDimensions('outer');
		var $windowWidth = $(window).width();

		// check if button belongs to an i18n textarea
		var $button = dropdown.find('.extendedDropdownToggle');
		if ($button.hasClass('dropdownCaptionTextarea')) {
			// use button dimensions instead
			$dropdownDimensions = $button.getDimensions('outer');
		}

		// get alignment
		var $align = 'left';
		if (($dropdownOffsets.left + $menuDimensions.width) > $windowWidth) {
			$align = 'right';
		}

		// calculate offsets
		var $left = 'auto';
		var $right = 'auto';
		if ($align === 'left') {
			dropdownMenu.removeClass('dropdownArrowRight');

			$left = $dropdownOffsets.left + 'px';
		}
		else {
			dropdownMenu.addClass('dropdownArrowRight');

			$right = ($windowWidth - ($dropdownOffsets.left + $dropdownDimensions.width)) + 'px';
		}

		// calculate vertical offset
		var $wasHidden = true;
		if (dropdownMenu.hasClass('dropdownOpen')) {
			$wasHidden = false;
			dropdownMenu.removeClass('dropdownOpen');
		}

		var $bottom = 'auto';
		var $top = $dropdownOffsets.top + $dropdownDimensions.height + 7;
		if ($top + $menuDimensions.height > $(window).height() + $(document).scrollTop()) {
			$bottom = $(window).height() - $dropdownOffsets.top + 10;
			$top = 'auto';

			dropdownMenu.addClass('dropdownArrowBottom');
		}
		else {
			dropdownMenu.removeClass('dropdownArrowBottom');
		}

		if (!$wasHidden) {
			dropdownMenu.addClass('dropdownOpen');
		}

		dropdownMenu.css({
			bottom: $bottom,
			left: $left,
			right: $right,
			top: $top
		});
	},

	/**
	 * Closes all dropdowns.
	 */
	_closeAll: function() {
		for (var $containerID in this._dropdowns) {
			var $dropdown = this._dropdowns[$containerID];

			if ($dropdown.hasClass('dropdownOpen')) {
				$dropdown.removeClass('dropdownOpen');
				this._menuHide(this._menus[$containerID]);

				this._notifyCallbacks($containerID, 'close');
			}
		}
	},

	/**
	 * Closes a dropdown without notifying callbacks.
	 *
	 * @param	string		containerID
	 */
	close: function(containerID) {
		if (!this._dropdowns[containerID]) {
			return;
		}

		this._dropdowns[containerID].removeClass('dropdownMenu');
		this._menuHide(this._menus[containerID]);
	},

	/**
	 * Notifies callbacks.
	 *
	 * @param	string		containerID
	 * @param	string		action
	 */
	_notifyCallbacks: function(containerID, action) {
		if (!this._callbacks[containerID]) {
			return;
		}

		for (var $i = 0, $length = this._callbacks[containerID].length; $i < $length; $i++) {
			this._callbacks[containerID][$i](containerID, action);
		}
	},

	_hover: function(event, targetID) {
		var $targetID = (event === null) ? targetID : $(event.currentTarget).data('target');

		// check if 'isOverlayDropdownButton' is set which indicates if
		// the dropdown toggle is in an overlay
		var $target = this._dropdowns[$targetID];
		if ($target && $target.data('isOverlayDropdownButton') === undefined) {
			var $dialogContent = $target.parents('.dialogContent');
			$target.data('isOverlayDropdownButton', $dialogContent.length > 0);

			if ($dialogContent.length) {
				$dialogContent.on('scroll', this._dialogScroll);
			}
		}

		// close all dropdowns
		for (var $containerID in this._dropdowns) {
			var $dropdown = this._dropdowns[$containerID];
			var $dropdownMenu = this._menus[$containerID];

			if ($containerID === $targetID) {
				$dropdown.addClass('dropdownOpen');
				this._menuShow($dropdownMenu);

				this._notifyCallbacks($containerID, 'open');

				this.setAlignment($dropdown, $dropdownMenu);
			}
			else {
				$dropdown.removeClass('dropdownOpen');
				this._menuHide($dropdownMenu);

				this._notifyCallbacks($containerID, 'close');
			}
		}

		if (event !== null) {
			event.stopPropagation();
			return false;
		}
	},

	_hoverOut: function(event, targetID) {
		setTimeout($.proxy(function() {

			var $targetID = (event === null) ? targetID : $(event.currentTarget).data('target');

			// close all dropdowns
			for (var $containerID in this._dropdowns) {
				var $dropdown = this._dropdowns[$containerID];
				var $dropdownMenu = this._menus[$containerID];

				if ($containerID === $targetID && $dropdown.hasClass('dropdownOpen') && !$dropdownMenu.is(":hover")) {
					$dropdown.removeClass('dropdownOpen');
					this._menuHide($dropdownMenu);

					this._notifyCallbacks($targetID, 'close');
				}
			}
		}, this), 170);
	},

	_menuShow: function(menu) {
		switch (this._animation) {
			case 'slide':
				menu.slideDown(this._animationSpeed, function() {
					$(this).addClass('dropdownOpen');
				});
			break;

			case 'appear':
				menu.addClass('dropdownOpen').css({opacity: 0}).animate({opacity: 1}, this._animationSpeed);
			break;

			default:
				menu.addClass('dropdownOpen');
			break;
		}
	},

	_menuHide: function(menu) {
		switch (this._animation) {
			case 'slide':
				menu.slideUp(this._animationSpeed, function() {
					$(this).removeClass('dropdownOpen');
				});
			break;

			case 'appear':
				menu.removeClass('dropdownOpen').css({opacity: 1}).animate({opacity: 0}, this._animationSpeed);
			break;

			default:
				menu.removeClass('dropdownOpen');
			break;
		}
	}
};