var from = 0;
var size = 50;

$(document).ready(function () {
    checkConnection().done(function () {
        requestData('ms-s01', 'seeded', 50, 0, "t_ml", "desc").done(function (body) {
            parseResult(body);
        }).fail(function (error) {
            $('#maincontainer').html('elasticsearch cluster is down!');
        });
    })
    .fail(function (obj) {
        $('#maincontainer').html('elasticsearch cluster is down!');
    });
});

function parseResult(data) {
    var html = '';
    var medic = 0;
    var naive = 0;
    var svm = 0;
    var tree = 0;
    var emotion = 0;
    var ML = 0;
    var message = '';
    var url = '';
    var messagelink = '';
    var shortstr = '';
    var date = '';
    var sme = 0;
    var formatedDate = '';
    var source = '';
    if (data && data.hits && data.hits.hits) {
        console.log(data.hits.hits.length);
        _.forEach(data.hits.hits, function (value, key) {
            value._index, value._type, value._id;
            html += '<tr class="rowcls" index="' + value._index + '" type="' + value._type + '" id="' + value._id + '" isedited=' + false + '>';
            medic = value._source.medic;
            naive = convertval(value._source.score_naive);
            svm = convertval(value._source.score_svm);
            tree = convertval(value._source.score_tree);
            if (value._source.score_emotion)
                emotion = parseFloat(value._source.score_emotion[0].toFixed(2));

            ML = parseFloat(value._source.t_ml);
            ML = ML.toFixed(2);
            message = value._source.t_message;
            url = value._source.t_urls;
            date = value._source.t_time;
            sme = value._source.t_sme;
            source = value._source.t_source;
            if (date) {
                date = date.split('T')[0].split('-');
                formatedDate = date[1] + "-" + date[2] + "-" + date[0].substring(2, 4);
            }

            if (message)
                message = message.split('>').join('');

            if (message && message.length > 200)
                shortstr = message.slice(0, 200) + '...';
            else
                shortstr = message;

            messagelink = '<a href="' + url + '"target="_blank" class="msglink" title="' + source + ': ' + message + '">' + shortstr + '</a>';

            html += '<td class="center">' + ML + '</td class="center"><td class="smetd"><input type="number" min="0" max="5" class="smeinp" value="' + sme + '"/></td><td class="msgtd">' + messagelink + '</td><td class="center">' + medic + '</td><td class="center">' + naive + '</td><td class="center">' + svm + '</td><td class="center">' + tree + '</td><td class="center">' + emotion + '</td></td><td class="center">' + formatedDate + '</td>';

            html += '</tr>'
        });
    }
   // html += '</table>';

    // $('#maincontainer').html(data.hits.hits[0]._index);
    $(".img-loader1").remove();
    if (data.hits.hits.length) {
        $('.rowcls').remove();
        $('.datatable').append(html);
    }
    $('.smeinp').change(function () {
        onSMEchange(this);
    });

    $('.tableheadicon').unbind().click(function () {
        console.log(this);
        var columnName = $(this).attr('column');
        var order = '';
        if ($(this).hasClass("fa fa-sort-desc")) {
            order = 'asc';
            $(this).removeClass("fa fa-sort-desc").addClass("fa fa-sort-asc");
        }
        else {
            order = 'desc';
            $(this).removeClass("fa fa-sort-asc").addClass("fa fa-sort-desc");
        }
        sortData(columnName, order);
    });
}

function handleError(error) {
    $(".img-loader1").remove();
    console.log(error);
    $('#maincontainer').html('Something is not working. Please check network connection.');
}

function sortData(columnName, order) {
    requestData('ms-s*', 'seeded', 50, 0, columnName, order).done(function (body) {
        parseResult(body);
    }).fail(function (error) {
        $('#maincontainer').html('elasticsearch cluster is down!');
    });
}

function getPrevPage() {
    if (from >= 50)
        from = from - 50;
    //   $('.img-loader1').length ? '' : $("#maincontainer").append('<div class="img-loader1">' +
    //   '<img class="inner-loader" src="../Image/loading-large.gif"></img>' +
    //'</div>');

    requestData('ms-s*', 'seeded', 50, from, "t_ml", "desc").done(function (body) {
        parseResult(body);
    }).fail(function (error) {
        $('#maincontainer').html('elasticsearch cluster is down!');
    });
}

function getNextPage() {
    UpdateSME();
    from = from + 50;

    requestData('ms-s*', 'seeded', 50, from, "t_ml", "desc").done(function (body) {
        parseResult(body);
    }).fail(function (error) {
        $('#maincontainer').html('elasticsearch cluster is down!');
    });
}

function onSMEchange(inp) {
    $(inp).parent().parent().attr('isedited', true);
    console.log($(inp));
}

function UpdateSME() {
    console.log($('.rowcls').length);
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