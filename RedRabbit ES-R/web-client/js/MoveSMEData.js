var index = configValues.index,
search = configValues.search,
sortColumn = configValues.sortColumn,
order = configValues.order,
size = configValues.size,
from = configValues.from,
pagingFrom = 0;
var createdId;
var counter = 1;
var totalCount = 0;
$(document).ready(function () {
    initiate(index, search, sortColumn, order, size, from);
});

function initiate(index, searchVal, sortColumn, order, size, from) {
    checkConnection().done(function () {
        getData(index, searchVal, size, from, sortColumn, order);
        getCount();
    })
   .fail(function (obj) {
       handleError(obj);
   });
}

function getCount() {
    client.count({
        index: index
    }, function (err, response) {
        totalCount = response.count;
        $('.updatedcount').append("0 of " + totalCount);
    });
}

function getData(index, searchVal, size, from, sortColumn, order) {
    if (!size)
        size = 100;
    if (!from)
        from = 0;
    requestData(index, searchVal, size, from, sortColumn, order).done(function (body) {
        console.log(body);
        createJSONtoupdate(body);
    }).fail(function (obj) {
        handleError(obj);
    });
}

function createJSONtoupdate(data) {
    var html = '', medic = 0, naive = 0, svm = 0, tree = 0, emotion = 0, ML = 0;
    var message = '', url = '', messagelink = '', shortstr = '', date = '', sme = 0, formatedDate = '', source = '';
    var strbody, id, type, status, user;
    if (data && data.hits && data.hits.hits) {
        console.log(data.hits.hits.length);
        _.forEach(data.hits.hits, function (value, key) {
            id = value._id;
            sme = value._source.t_sme;
            type = value._type;
            medic = value._source.medic;
            naive = value._source.score_naive;//convertval(value._source.score_naive);
            svm = value._source.score_svm;//convertval(value._source.score_svm);
            tree = value._source.score_tree;//convertval(value._source.score_tree);

            if (value._source.score_emotion)
                emotion = value._source.score_emotion[0];

            ML = value._source.t_ml
            message = value._source.t_message;
            url = value._source.t_urls;
            date = value._source.t_time;
            sme = value._source.t_sme;
            source = value._source.t_source;
            user = value._source.t_user;
            if (sme > 3) {
                status = processValues.status;
                strbody = convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, source, status);
                moveprocessedData(processValues.index, processValues.type, id, strbody);
            }
            else {
                status = archiveValues.status;
                strbody = convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, source, status);
                moveprocessedData(archiveValues.index, archiveValues.type, id, strbody);
            }
        });
    }
}

function moveprocessedData(index, type, id, strbody) {
    createNode(index, type, id, strbody).done(function (body) {
        console.log(body);
        createdId = body._id;
        deleteUpdatedNode(type, createdId);
    }).fail(function (obj) {
        handleError(obj);
        deleteUpdatedNode(type);
    });
}

function deleteUpdatedNode(type, createdId) {
    $('.updatedcount').html(counter++ + " of " + totalCount);
    if (counter % size == 0)
        initiate(configValues.index, configValues.search, sortColumn, order, size, counter);
    if (createdId) {
        deleteNode(index, type, createdId).done(function (body) {
            console.log(body);
        }).fail(function (obj) {
            handleError(obj);
        });
    }
}

function handleError(error) {
    $(".img-loader1").remove();
    console.log(error);
    $('.rowcls').remove();
    $('.error').remove();
   // $('.datatable').append('<div class="error">elasticsearch cluster is down!</div>');
}

function convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, source, status, user) {
    var objdata = {};
    if (sme)
        objdata['t_sme'] = sme;
    else
        objdata['t_sme'] = 0;

    if (medic)
        objdata['medic'] = medic;
    else
        objdata['medic'] = 0;

    if (naive)
        objdata['score_naive'] = naive;
    else
        objdata['score_naive'] = 0;

    if (svm)
        objdata['score_svm'] = svm;
    else
        objdata['score_svm'] = 0;

    if (tree)
        objdata['score_tree'] = tree;
    else
        objdata['score_tree'] = 0;

    if (ML)
        objdata['t_ml'] = ML;
    else
        objdata['t_ml'] = 0;

    if (message)
        objdata['t_message'] = message;
    else
        objdata['t_message'] = 0;

    if (url)
        objdata['t_urls'] = url;
    else
        objdata['t_urls'] = 0;

    if (date)
        objdata['t_time'] = date;
    else
        objdata['t_time'] = 0;

    if (source)
        objdata['t_source'] = source;
    else
        objdata['t_source'] = 0;

    if (status)
        objdata['t_status'] = status;
    else
        objdata['t_status'] = 0;

    if (emotion)
        objdata["score_emotion"] = emotion;
    else
        objdata['score_emotion'] = 0;

    if (user)
        objdata["t_users"] = user
    else
        objdata['t_users'] = 0;

    return objdata;
}


