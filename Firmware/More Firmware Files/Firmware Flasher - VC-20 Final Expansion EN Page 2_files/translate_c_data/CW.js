"use strict";if(!CW)var CW={};CW.ExtendedSearch=WCF.User.Panel.Abstract.extend({_triggerLength:2,_searchArea:null,_searchInput:null,_searchString:"",_equalSearchString:!0,_notificationLink:"",_openInfoContainerFlag:!1,init:function(t,e){this._notificationLink=t,this._searchArea=$("#search"),this._searchInput=$("input",this._searchArea),this._super(this._searchArea,"extendedSearch",{pointerOffset:"13px",title:WCF.Language.get("wcf.page.com.woltlab.wcf.SearchResultPage"),showAllLink:e,noItems:WCF.Language.get("wcf.extendedSearch.noItems")}),null===this._dropdown&&(this._dropdown=this._initDropdown()),$(".interactiveDropdownShowAll",this._dropdown._container).text(WCF.Language.get("wcf.search.extended")),this._proxy=new WCF.Action.Proxy({url:this._notificationLink,showLoadingOverlay:!1,success:$.proxy(this._success,this)}),this._searchArea.keyup($.proxy(this._keyUp,this)),this._searchArea.click($.proxy(this._inputClick,this)),$(this._dropdown.getItemList()).on("click","li.extendedNotificationItem:not(.extendedSearchRight, .extendedSearchLeft)",$.proxy(this._click,this))},toggle:function(t,e){return"undefined"===e&&(e=!1),this._searchString.length>=this._triggerLength?(!this._equalSearchString&&e&&(this._loadData=!0),this._super(t)):this._openInfoContainerFlag?(this._loadData=!1,this._super(t)):!1},setNotificationLink:function(t){this._notificationLink=t,this._proxy.options.url=this._notificationLink},_dblClick:function(){},_keyUp:function(t){var e=this._searchString;return this._searchString=$.trim(this._searchInput.val()),this._equalSearchString=e===this._searchString,this._searchString.length>=this._triggerLength&&(this._openInfoContainerFlag=!1),this._equalSearchString?void 0:this._searchString.length<this._triggerLength&&this._dropdown._container.hasClass("open")&&!this._openInfoContainerFlag?void this._dropdown.close():void(this._dropdown._container.hasClass("open")?this._openInfoContainerFlag||this._load():this.toggle(t,!0))},_load:function(){this._dropdown.getItemList().html(""),$('<li class="loading"><span class="icon icon24 fa-spinner" /> <span>'+WCF.Language.get("wcf.global.loading")+"</span></li>").appendTo(this._dropdown.getItemList()),this._proxy.abortPrevious(),this._proxy.setOption("data",{search:this._searchString}),this._proxy.sendRequest()},_success:function(t,e,i){this._super(t,e,i);var n=this._dropdown.getItemList();(!$(".extendedSearchRight",n).length||$.browser.mobile)&&$(".extendedSearchLeft",n).addClass("noFloat"),(!$(".extendedSearchLeft",n).length||$.browser.mobile)&&$(".extendedSearchRight",n).addClass("noFloat");var s=0;$('.interactiveDropdown[data-source="extendedSearch"] ul.interactiveDropdownItems > li').each(function(t,e){s+=$(e).width()});var a=$('.interactiveDropdown[data-source="extendedSearch"]');s<a.width()&&a.css("min-width",s);var r=new RegExp("("+this._searchString+")","gi");$("li h3",n).each($.proxy(function(t,e){var i=$(e).text();i=i.replace(r,'<span class="extendedSearchHighlightString">$1</span>'),$(e).html(i)},this))},_click:function(t){var e=$(t.currentTarget),i=e.data("link");if(i.length)window.location=i;else{var n=$("h3",e).text();this._searchArea.find("input[type=search]").val(n),this._searchArea.find("form").submit()}},_inputClick:function(t){this._searchString.length<=1&&(this._openInfoContainerFlag=!0,this._dropdown._container.hasClass("open")||(this._dropdown.getItemList().html(""),$('<li class="noItems"><span>'+WCF.Language.get("wcf.extendedSearch.info")+"</span></li>").appendTo(this._dropdown.getItemList()),this.toggle(t)))}});