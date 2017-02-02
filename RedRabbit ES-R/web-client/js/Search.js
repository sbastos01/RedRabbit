//var from = 0;
//var size = 50;
var index = configValues.index,
    search = configValues.search,
    sortColumn = configValues.sortColumn,
    order = configValues.order,
    size = configValues.size,
    from = configValues.from,
    pagingFrom = 0;
$(document).ready(function () {
    initiate(index, search, sortColumn, order, size, from);
});

function initiate(index, searchVal, sortColumn, order, size, from) {
    checkConnection().done(function () {
        getData(index, searchVal, size, from, sortColumn, order);
    })
   .fail(function (obj) {
       handleError(obj);
   });

    $('.fa-search').unbind().click(function () {
        searchVal = $(this).parent().find('input').val();
        searchonvalue(searchVal);
    });

    $('.searchval').unbind().click(function () {
        searchonvalue($(this).attr('value'));
    });

    $('.quicklink').unbind().click(function () {
        searchonindice($(this).attr('value'), $(this).attr('search'));
    });

    $('.msglink').unbind().click(function () {
        $(this).css('color', 'red');
    });
}

function searchonvalue(searchVal) {
    pagingFrom = 0;
    search = searchVal;
    if (searchVal)
        getData(index, searchVal, size, from, sortColumn, order);
}

function searchonindice(indexval, searchVal) {
    pagingFrom = 0;
    search = searchVal;
    index = indexval;
    if (indexval)
        getData(indexval, search, size, from, sortColumn, order);
}

function getData(index, searchVal, size, from, sortColumn, order) {
    if (!size)
        size = 50;
    if (!from)
        from = 0;
    requestData(index, searchVal, size, from, sortColumn, order).done(function (body) {
        parseResult(body);
    }).fail(function (obj) {
        handleError(obj);
    });
}

function parseResult(data) {
    var html = '', medic = 0, naive = 0, svm = 0, tree = 0, emotion = 0, ML = 0;
    var message = '', url = '', messagelink = '', shortstr = '', date = '', sme = 0, formatedDate = '', source = '';

    if (data && data.hits && data.hits.hits) {
        console.log(data.hits.hits.length);
        _.forEach(data.hits.hits, function (value, key) {
            html += '<tr class="rowcls" index="' + value._index + '" type="' + value._type + '" id="' + value._id + '" isedited=' + false + '>';
            medic = value._source.medic;
            naive = convertval(value._source.score_naive);
            svm = convertval(value._source.score_svm);
            tree = convertval(value._source.score_tree);
            if (value._source.score_emotion && value._source.score_emotion[0])
                emotion = parseFloat(value._source.score_emotion[0].toFixed(2));

            ML = parseFloat(value._source.t_ml);
            ML = ML.toFixed(2);
            message = value._source.t_message;
            url = value._source.t_urls;
            date = value._source.t_time;
            sme = value._source.t_sme;
            source = value._source.t_source;
            if (date) {
                if (date.split('T')[0].split('-').length == 3) {
                    date = date.split('T')[0].split('-');
                    formatedDate = date[1] + "-" + date[2] + "-" + date[0].substring(2, 4);
                }
                else
                    formatedDate = date;
            }

            if (message) {
                message = message.split('>').join(' ');
                message = message.split('<').join(' ');
            }

            if (message && message.length > 200)
                shortstr = message.slice(0, 200) + '...';
            else
                shortstr = message;

            if (message)
                messagelink = '<a href="' + url + '"target="_blank" class="msglink" title="' + source + ': ' + message + '">' + shortstr + '</a>';
            else
                messagelink = '';

            html += '<td class="center">' + ML + '</td class="center"><td class="smetd"><input type="number" min="0" max="5" class="smeinp"  value="' + sme + '"/></td><td class="msgtd">' + messagelink + '</td><td class="center">' + medic + '</td><td class="center">' + naive + '</td><td class="center">' + svm + '</td><td class="center">' + tree + '</td><td class="center">' + emotion + '</td></td><td class="center">' + formatedDate + '</td>';

            html += '</tr>'
        });
    }

    $(".img-loader1").remove();
    $('.rowcls').remove();
    $('.error').remove();
    $('.datatable').append(html);
    $('.smeinp').change(function () {
        onSMEchange(this);
    });

    $('.tableheadicon').unbind().click(function () {
        manageSort(this);
    });

    //$('.msgtd').unbind().click(function () {
    //    $(this).css('background-color', 'green');
    //});

    $('.msglink').unbind().mousedown(function (e) {
        console.log(e.which);
        if (e.which == 1) {
            $(this).css('background-color', 'yellow');
            window.open($(this).attr('href'));
        }
        else (e.which == 3)
        {
            var id = $(this).parent().parent().attr('id');
            var index = $(this).parent().parent().attr('index');
            var type = $(this).parent().parent().attr('type');
            deleteNode(index, type, id);
        }
    });
    if(location.href.indexOf('search.html')>0 || location.href.indexOf('Search.html')>0)
    {
        $('.smeinp').attr('disabled', 'disabled');
    }
}

function manageSort(obj) {
    var columnName = $(obj).attr('column');
    var order = '';
    if ($(obj).hasClass("fa fa-sort-desc")) {
        order = 'asc';
        $(obj).removeClass("fa fa-sort-desc").addClass("fa fa-sort-asc");
    }
    else {
        order = 'desc';
        $(obj).removeClass("fa fa-sort-asc").addClass("fa fa-sort-desc");
    }
    sortData(columnName, order);
}

function handleError(error) {
    $(".img-loader1").remove();
    console.log(error);
    $('.rowcls').remove();
    $('.error').remove();
    $('.datatable').append('<div class="error">elasticsearch cluster is down!</div>');
    //$('#maincontainer').html('Something is not working. Please check network connection.');
}

function sortData(columnName, order) {
    getData(index, search, size, from, columnName, order);
}

function getPrevPage() {
    if (pagingFrom >= size)
        pagingFrom = pagingFrom - size;
    getData(index, search, size, pagingFrom, sortColumn, order);
}

function getNextPage() {
    pagingFrom = pagingFrom + 50;
    getData(index, search, size, pagingFrom, sortColumn, order);
}

function onSMEchange(inp) {
    $(inp).parent().parent().attr('isedited', true);
    console.log($(inp));
}

function UpdateSME() {
    var id = '', index = '', type = '', SMEval = '';
    _.forEach($('.rowcls'), function (value, key) {
        if ($(value).attr('isedited') == "true") {
            id = $(value).attr('id');
            index = $(value).attr('index');
            type = $(value).attr('type');
            SMEval = $(value).find('input').val();
            updateData(index, type, id, SMEval);
        }
    });
}

function updateData(index, value, id, SME) {
    client.update({
        index: index,
        type: value,
        id: id,
        body: {
            doc: {
                "t_sme": SME
            }
        }
    }, function (error, response) {
        console.log(error);
        console.log(response);
    });
}