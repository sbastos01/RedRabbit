//var from = 0;
//var size = 50;
var index = configValues.index,
    //search = configValues.search,
    sortColumn = configValues.sortColumn,
    order = configValues.order,
    size = configValues.size,
    from = configValues.from,
    pagingFrom = 0;

var search = {
    "_source": {
        "includes": [
            "pdfDocumentId",
            "title",
            "score01",
            "score02",
            "score03",
            "score04",
            "score05",
            "bodypart",
            "bodypart03"
        ]
    }
};

var searchquery = {
    "_source": {
        "includes": [
            "pdfDocumentId",
            "title",
            "score01",
            "score02",
            "score03",
            "score04",
            "score05",
            "bodypart",
            "bodypart03"
        ]
    },
    "highlight": {
        "fields": {
            "bodypart03": {
                "fragment_size": 300,
                "number_of_fragments": 1
            }
        }
    },
    "query": {
        "bool": {
            "must": {
                "multi_match": {
                    "query": " ", //Psychoanalyse
                    "type": "most_fields",
                    "minimum_should_match": "25%",
                    "fields": [
                        "bodypart03^5",
                        "title^5"
                    ]
                }
            },
            "should": {
                "match": {
                    "bodypart03": " "//Pharmakotherapien und Kombinationstherapien bei Angststörungen
                }
            }
        }
    }
}

$(document).ready(function () {
    initiate(index, search, sortColumn, order, size, from);
    $('a').click(function () { return false; });
});

function initiate(index, searchVal, sortColumn, order, size, from) {
    checkConnection().done(function () {
        getData(index, searchVal, size, from, sortColumn, order);
    }).fail(function (obj) {
        handleError(obj);
    });
    $('.searchtext').keypress(function (e) {
        if (e.which == 13) {
            $('.totalhits').hide();
            $('.datatable').hide();
            searchonvalue(createObject());
        }
    });
    $('.fa-search').unbind().click(function () {
        $('.totalhits').hide();
        $('.datatable').hide();
        searchonvalue(createObject());
    });
    $('.searchval').unbind().click(function () {
        $('.datatable').hide();
        $('.totalhits').hide();
        searchonvalue($(this).attr('value'));
    });
    $('.quicklink').unbind().click(function () {
        $('.datatable').hide();
        $('.totalhits').hide();
        searchonindice($(this).attr('value'), $(this).attr('search'));
    });
    $('.msglink').unbind().click(function () {
        $(this).css('color', 'red');
    });
    $('.projectdropdown').unbind().change(function () {
        $('.datatable').hide();
        $('.totalhits').hide();
        searchonvalue(createObject());
    });
}

function createObject() {
    searchquery["query"]["bool"]["must"]["multi_match"]["query"] = $('#searchquerytext').val();
    searchquery["query"]["bool"]["should"]["match"]["bodypart03"] = $('#searchcontexttext').val();
    searchquery["query"]["bool"]["must"]["multi_match"]["fields"] = _.map($('.projectdropdown').val().split(' '), function (val) {
        return val.split('-').join('^');
    });
    return searchquery;
}

function searchonvalue(searchVal) {
    pagingFrom = 0;
    search = searchVal;
    if (searchVal)
        getData(index, createObject(searchVal), size, from, sortColumn, order);
}

function searchonindice(indexval, searchVal) {
    pagingFrom = 0;
    search = searchVal;;
    index = indexval;
    if (indexval)
        getData(indexval, createObject(searchVal), size, from, sortColumn, order);
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
    var html = '', pdfDocumentId = 0, title = '', highlight = '', score01 = 0, score02 = 0, score03 = 0, score04 = 0, score05 = 0;
    var message = '', shortstr = '', date = '', formatedDate = '';
    $('.totalhits').html("Total Hits: " + data.hits.total);
    $('.totalhits').show();
    if (data && data.hits && data.hits.hits) {
        console.log(data.hits.hits.length);
        _.forEach(data.hits.hits, function (value, key) {
            html += '<tr class="rowcls" index="' + value._index + '" type="' + value._type + '" id="' + value._id + '" isedited=' + false + '>';
            pdfDocumentId = 'pdfDocumentId' in value._source && value._source.pdfDocumentId ? value._source.pdfDocumentId : 0;
            title = 'title' in value._source && value._source.title ? value._source.title : '';
            score01 = '_score' in value && value._score ? value._score.toFixed(2) : 0;
            score02 = 'score02' in value._source && value._source.score02 ? value._source.score02.toFixed(2) : 0;
            score03 = 'score03' in value._source && value._source.score03 ? value._source.score03.toFixed(2) : 0;
            score04 = 'score04' in value._source && value._source.score04 ? value._source.score04.toFixed(2) : 0;
            score05 = 'score05' in value._source && value._source.score05 ? value._source.score05.toFixed(2) : 0;
            highlight = 'highlight' in value && value.highlight.bodypart03[0] ? value.highlight.bodypart03[0] : '';
            message = 'bodypart03' in value._source && value._source.bodypart03 ? value._source.bodypart03 : '';
            date = 'date' in value._source && value._source.date ? value._source.date : new Date();

            // // if (date) {
            // //     if (typeof (date) != 'number' && date.split('T')[0].split('-').length == 3) {
            // //         date = date.split('T')[0].split('-');
            // //         formatedDate = date[1] + "-" + date[2] + "-" + date[0].substring(2, 4);
            // //     }
            // //     else
            // //         formatedDate = date;
            // // }
            let month = date.getMonth() + 1
            formatedDate = date.getFullYear() + '/' +
                (date.getMonth() + 1 < 10 ? '0' : '') + month + '/' +
                (date.getDate() < 10 ? '0' : '') + date.getDate();

            if (message)
                message = message.split('>').join(' ').split('<').join(' ');

            shortstr = message && message.length > 400 ? message.slice(0, 400) + '...' : message;

            messagelink = '<a href="#" target="_blank" class="msglink" title="' + message + '">' + shortstr + '</a>';

            if (title)
                title = title.split('-').join(' ').split('_').join(' ');

            html += '<td class="left">' + pdfDocumentId + '</td class="left"><td class="left">' + title + '</td><td class="center esscore">' + score01 + '</td><td class="left">' + score02 + '</td><td class="left">' + score03 + '</td><td class="left">' + score04 + '</td><td class="left">' + score05 + '</td><td class="left">' + highlight + '</td><td class="left">' + messagelink + '</td></td><td class="left">' + formatedDate + '</td>';

            html += '</tr>'
        });
    }

    $(".img-loader1").remove();
    $('.rowcls').remove();
    $('.error').remove();
    $('.datatable').append(html);
    $('.datatable').show();
    //$("a").removeAttr("onclick")
    $('a').click(function () { return false; });
    $('.smeinp').change(function () {
        onSMEchange(this);
    });

    $('.tableheadicon').unbind().click(function () {
        manageSort(this);
    });

    $('.msglink').unbind().mousedown(function (e) {
        console.log(e.which);
        if (e.which == 1) {
            $(this).css('background-color', 'yellow');
            window.open($(this).attr('href'));
        }
        // else
        // {
        //     var id = $(this).parent().parent().attr('id');
        //     var index = $(this).parent().parent().attr('index');
        //     var type = $(this).parent().parent().attr('type');
        //     deleteNode(index, type, id);
        // }
    });
    if (location.href.indexOf('search.html') > 0 || location.href.indexOf('Search.html') > 0) {
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
}

function sortData(columnName, order) {
    getData(index, search, size, from, columnName, order);
}

function getPrevPage() {
    $('.totalhits').hide();
    $('.datatable').hide();
    if (pagingFrom >= size)
        pagingFrom = pagingFrom - size;
    getData(index, search, size, pagingFrom, sortColumn, order);
}

function getNextPage() {
    $('.totalhits').hide();
    $('.datatable').hide();
    pagingFrom = pagingFrom + 50;
    getData(index, search, size, pagingFrom, sortColumn, order);
}

function savescore() {
    let body = { "doc": {} };
    let selectedscore = $('.scoredropdown').val();
    _.forEach($('.rowcls'), function (value, key) {
        id = $(value).attr('id');
        index = $(value).attr('index');
        type = $(value).attr('type');
        body["doc"][selectedscore] = parseFloat($(value).find('.esscore').text())
        updateData(index, type, id, body);
    });
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

function updateData(index, value, id, body) {
    client.update({
        index: index,
        type: value,
        id: id,
        body: body
    }, function (error, response) {
        console.log(error);
        console.log(response);
    });
}