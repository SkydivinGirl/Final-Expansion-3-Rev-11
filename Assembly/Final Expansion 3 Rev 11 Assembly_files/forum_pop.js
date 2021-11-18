function jb_pop(o) {
 var w=340; var h=200;
 var url='http://www.lemon64.com/music/jukebox.php?game='
 var s=o.getAttribute('href').split('=');
 var x=Math.max(parseInt((screen.width-w)/2), 0);
 var y=Math.max(parseInt((screen.height-h)/2), 0);
 var opt='left='+x+',top='+y+',width='+w+',height='+h+',scrollbars';
 var win=window.open(url+s[1],'l64_jukebox',opt);
}
