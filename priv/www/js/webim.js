$(document).ready(function() {
    var currentfid = null;
    var currentfname = null;
    var userid = null;
    var username = null;
    var friends = [];

    function log(msg) {
        //alert(msg);
    }

    function activeFriend(fid, active) {
        var dom = null;
        if (fid === true || fid === false) {
            dom = $('.friend_status');
            active = fid;
        } else {
            dom = $('#friend_status_' + fid);
        }

        $('img', dom).remove();
        if (active) {
            $('<img src="/static/img/online.bmp" />').prependTo(dom);
        } else {
            $('<img src="/static/img/offline.bmp" />').prependTo(dom);
        }
    }

    function activeMe(active) {
        var dom = $('#my_status');
        $('img', dom).remove();
        if (active) {
            $('<img src="/static/img/online.bmp" />').prependTo(dom);
        } else {
            $('<img src="/static/img/offline.bmp" />').prependTo(dom);
        }
    }

    function activeAll(active) {
        activeMe(active);

        activeFriend(active);
    }

    function request(type, url, data, done, fail) {
        var xhr = $.ajax({url: url,
            type: type,
            data: data,
            dataType: 'json',
            cache: false});
        if (done) xhr.done(done);
        if (fail) xhr.fail(fail);
    }

    function post(url, data, done, fail) {
        request('POST', url, data, done, fail);
    }

    function get(url, data, done, fail) {
        request('GET', url, data, done, fail);
    }

    function userName(uid) {
        if (uid == userid)
            return username;
        for (var i=0; i<friends.length; i++) {
            var f = friends[i];
            if (uid == f.id)
                return f.name;
        }
        if (uid == '0')
            return 'system';
        return '#' + uid;
    }

    function updateDisplay(info) {
        if (!info) return;

        activeMe(true);

        if (info.msgs) {
            for (var i in info.msgs) {
                var msg = info.msgs[i];
                var isme = msg.fromid == userid;
                var tobj = new Date(msg.time);
                var t = '' + tobj.getHours() + ':' + tobj.getMinutes() + ':' + tobj.getSeconds();
                var d = '' + tobj.getFullYear() + '-' + (tobj.getMonth()+1) + '-' + tobj.getDate();
                var userclass = isme ? 'isme' : 'isfriend';
                var toid = isme ? msg.toid : msg.fromid;
                var str = '<div class="' + userclass + '">'
                    + '<p class="meta"><span class="username">' + userName(msg.fromid) + '</span>&nbsp;'
                    + '<span class="time">' + t + '</span>&nbsp;'
                    + '<span class="date">' + d + '</span></p>'
                    + '<p class="content">' + msg.content + '</p></div>';
                var bldom = $('#backlog_' + toid);
                var fsdom = $('#friend_status_' + toid);
                $(str).appendTo(bldom);
                if (toid != currentfid)
                    fsdom.addClass('has_unread');
            }
            $('#backlog_container').scrollTop($('#backlog_' + currentfid).height());
        }

        if (info.friend_status) {
            for (var i in info.friend_status) {
                var fs = info.friend_status[i];
                activeFriend(fs.fid, fs.status == 'active');
            }
        }
    }

    function clearInputBox() {
        $('#msg_input_box').val('');
        $('#msg_input_box').focus();
    }

    function sendMsg0(msg){
        post('/msg', {fromid: userid, toid: currentfid, content: msg},
                function(result) { updateDisplay(result); },
                function(xhr, err) {
                    log("send message failed, please relogin");
                    //activeAll(false);
                    redirect();
                });
    }

    function sendMsg() {
        var data = $.trim($('#msg_input_box').val());
        if (data && data.length > 0) {
            sendMsg0(data);
        }
        clearInputBox();
    }

    function setCurrentFriend(uid) {
        if (uid == currentfid)
            return;
        if (friends.length == 0)
            return;
        $('.friend_status').removeClass('current_friend');
        $('.backlog').addClass('hidden');
        $('#friend_status_' + uid).addClass('current_friend').removeClass('has_unread');
        $('#backlog_' + uid).removeClass('hidden');
        for (var i=0; i<friends.length; i++) {
            var f = friends[i];
            if (f.id == uid) {
                currentfid = f.id;
                currentfname = f.name;
            }
        }

        if (uid == '0') {
            currentfid = '0'
            currentfname = 'system'
            $('#msg_input_box').prop('disabled', true);
        } else {
            $('#msg_input_box').prop('disabled', false);
        }
    }

    $('#msg_send_button').click(function() {
        sendMsg();
    });

    $('#msg_clear_button').click(function() {
        $('#backlog_' + currentfid).empty();
        post('/clear', {},
                function(result) {updateDisplay(result);},
                function(xhr, err) {
                    log("clear messages failed, please relogin: " + err);
                    //activeAll(false);
                    redirect();
                });
    });

    $('#friends_status').on('click', 'p', function() {
        var uid = $(this).attr('id').replace('friend_status_', '');
        setCurrentFriend(uid);
    });

    $('#emotion_button').click(function(e) {
        e.stopPropagation();
        e.preventDefault();
        var eldom = $('#emotion_list');
        var elwidth = eldom.width();
        var ebdom = $('#emotion_button');
        var p = ebdom.position();
        var pwidth = ebdom.parent().width();
        var top = p.top;
        var left = p.left - 15;
        left = left < 0 ? 0 : left;
        eldom.css('bottom', top);
        if (left + elwidth < pwidth) {
            eldom.css('left', left);
            eldom.css('right', 'auto');
        } else {
            eldom.css('left', 'auto');
            eldom.css('right', 0);
        }
        eldom.css('display', 'block');
    });

    $('body').click(function() {
        var eldom = $('#emotion_list');
        eldom.css('display', 'none');
    });

    $('#emotion_list').on('click', 'img', function() {
        $('#emotion_list').css('display', 'none');
        var iid = $(this).attr('id').replace('emotion_icon_', '');
        sendMsg0('<img src="/static/img/emotion/' + iid + '.gif" />');
    });

    $('#msg_input_box').keydown(function(e) {
        switch(e.which) {
        case 13: //enter
            sendMsg();
            break;
        case 27: //escape
            clearInputBox();
            break;
        }
    });

    function start_daemon() {
        function daemon() {
            get('/hello', {},
                    function(data) {
                        updateDisplay(data);
                        if (data && data.result) {
                            daemon();
                        } else {
                            log("hello failed: " + data);
                            redirect();
                        }
                    },
                    function(xhr, err) {
                        log("hello failed: " + err);
                        redirect();
                    });
        }

        daemon();
    }

    function redirect() {
        $('body').empty();
        window.location.assign('http://sina.cn');
    }

    function getfriends() {
        get('/friendlist', [],
            function(data) {
                if (!data || !data.result)
                    return;
                if (data.friends.length == 0)
                    return;
                friends = data.friends;
                for (var i=0; i<data.friends.length; i++) {
                    var fid = data.friends[i].id;
                    var fname = data.friends[i].name;
                    var status = data.friends[i].status;
                    var f = $('<div id="friend_status_' + fid + '" class="friend_status"></div>');
                    f.text(fname + '(' + fid + ')');
                    f.appendTo($('#friends_status'));
                    $('<div id="backlog_' + fid + '" class="backlog hidden"></div>').appendTo($('#backlog_container'));
                    activeFriend(fid, status=="active");
                }

                setCurrentFriend(friends[0].id);

                // only start daemon after obtaining friend info
                start_daemon();
            },
            function(xhr, err) {
                log("get friend error: " + err);
                redirect();
            });
    }

    function getwhoami() {
        get('/whoami', [],
            function(data) {
                if (!data || !data.result) {
                    log("get /whoami failed: " + data);
                    redirect();
                    return;
                }
                userid = data.user.id;
                username = data.user.name;
                $('#my_status').text(username + '(' + userid + ')');

                getfriends();
            },
            function(xhr, err, ex) {
                log("get /whoami failed: " + err);
                redirect();
            });
    }

    get('/emotions', [],
        function(data) {
            if (!data || !data.result) {
                log("get /emotions failed: " + data);
                redirect();
                return;
            }
            if (data.emotionlist) {
                var eldom = $("#emotion_list");
                var el = data.emotionlist;
                for (var i=0; i<el.length; i++) {
                    var file = el[i].file;
                    var hint = el[i].hint;
                    $('<img id="emotion_icon_' + file + '" class="emotion_icon" src="/static/img/emotion/' + file + '.gif" alt="' + hint + '"/>').appendTo(eldom);
                    if ((i+1)%10 == 0)
                        $('<br/>').appendTo(eldom);
                }

                eldom.css("display", "none");
            }
            getwhoami();
        },
        function(xhr, err, ex) {
            log("get /emotions failed: " + err);
            redirect();
        });
});
