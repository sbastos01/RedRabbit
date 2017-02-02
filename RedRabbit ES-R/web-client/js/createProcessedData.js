var counter = 1;
var totalCount;
var greaterquery = rawindex.query + "T00:00:00.000";
var lessquery = rawindex.query + "T23:59:59.000";
$(document).ready(function () {
    checkConnection().done(function () {
        //getData(index, searchVal, size, from, sortColumn, order);
        getCount();
        getdtt();
    })
   .fail(function (obj) {
       handleError(obj);
   });
});

function getCount() {
    client.count({
        index: rawindex.index,
        body: {
            "query": {
                "range": {
                    "@timestamp": { "gte": greaterquery, "lte": lessquery }
                }
            }
        }
    }, function (err, response) {
        totalCount = response.count;
        $('.updatedcount').append("0 of " + totalCount);
    });
}

function getdtt() {
    var dfd = $.Deferred();
    client.search({
        index: rawindex.index,
        body: {
            "query": {
                "range": {
                    "@timestamp": { "gte": greaterquery, "lte": lessquery }
                }
            },
            size: 100,
            from : counter
        }
    }).then(function (body) {
        dfd.resolve(body);
        debugger;
        console.log(body);
        parseJSONresult(body);
    }, function (error) {
        dfd.reject(false);
        console.log(error);
    });
    return dfd.promise();
}

function parseJSONresult(data) {
    var medic = 0, naive = 0, svm = 0, tree = 0, emotion = 0, ML = 0;
    var message = '', url = '', messagelink = '', shortstr = '', date = '', sme = 0, formatedDate = '', source = '';
    var strbody, id, type, status, user;
    if (data.hits.hits.length == 0)
        $('body').html("No data with this date");
    _.forEach(data.hits.hits, function (value, key) {
        index = value._index;
        id = value._id;
        sme = value._source.t_sme;
        type = value._type;
        medic = value._source.medic;
        naive = value._source.score_naive;
        svm = value._source.score_svm;
        tree = value._source.score_tree;
        if (value._source.score_emotion)
            emotion = value._source.score_emotion[0];
        else
            emotion = 0;
        ML = value._source.t_ml
        message = value._source.message;
        if(value._source.urls)
            url = value._source.urls[0];
        date = value._source['@timestamp'];
        sme = value._source.t_sme;
        source = value._source.source;
        user = value._source.user;
        t_id = value._id;
        updateStatus(type, id);
        strbody = convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, user, t_id, source);
        createNode(rawToProcessIndex.index, type, id, strbody).done(function (body) {
            console.log(body);
        }).fail(function (obj) {
            handleError(obj);
        });
    });
}

function updateStatus(type, id) {
    client.update({
        index: rawindex.index,
        type: type,
        id: id,
        body: {
            doc: {
                "t_status": "ETL "+ rawindex.index + "-" + rawindex.query
            }
        }
    }, function (error, response) {
        $('.updatedcount').html(counter++ + " of " + totalCount);
        if (counter % 100 == 0)
        {
            setTimeout(function () {
                getdtt();
            }, 3000);
        }
        console.log(error);
        console.log(response);
    });
}

function convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, user, t_id, source) {
    var objdata = {};

    if (t_id)
        objdata["t_id"] = t_id;
    else
        objdata["t_id"] = 0;

    if (message)
        objdata['t_message'] = message;
    else
        objdata["t_message"] = 0;

    if (date)
        objdata['t_time'] = date;
    else
        objdata["t_time"] = 0;

    if (url)
        objdata['t_urls'] = url;
    else
        objdata["t_urls"] = 0;

    objdata["t_users"] = user;

    if (medic)
        objdata['medic'] = medic;
    else
        objdata["medic"] = 0;

    if (svm)
        objdata['score_svm'] = svm;
    else
        objdata["score_svm"] = 0;

    if (tree)
        objdata['score_tree'] = tree;
    else
        objdata["score_tree"] = 0;

    if (naive)
        objdata['score_naive'] = naive;
    else
        objdata["score_naive"] = 0;

    if (ML)
        objdata['t_ml'] = ML;
    else
        objdata["t_ml"] = 0;

    objdata['score_emotion'] = emotion;

    if (sme)
        objdata['t_sme'] = sme;
    else
        objdata["t_sme"] = 0;

    objdata['t_status'] = status;

    if (source)
        objdata['t_source'] = source;
    else
        objdata["t_source"] = 0;

    return objdata;
}

function handleError(error) {
    $(".img-loader1").remove();
    console.log(error);
    $('.rowcls').remove();
    $('.error').remove();
    //$('.datatable').append('<div class="error">elasticsearch cluster is down!</div>');
}