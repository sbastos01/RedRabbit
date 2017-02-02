var counter = 1;
var indexnames = '';
var totalCount = 0;
$(document).ready(function () {
    indexnames = _.join(processedIndices.indices, ',');
    checkConnection().done(function () {
        getdtt();
        getCount();
    })
   .fail(function (obj) {
       handleError(obj);
   });
});

function getCount() {
    client.count({
        index: indexnames,
        body: {
            query: {
                "bool": {
                    "must_not": [
                    {
                        "prefix": {
                            "t_status": "processed"
                        }
                    }
                    ]
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

    client.msearch({
        body: [
                { index: indexnames },
                {
                    query: {
                        "bool": {
                            "must_not": [
                            {
                                "prefix": {
                                    "t_status": "processed"
                                }
                            }
                            ]
                        }
                    },
                    size: 100
                },
        ]
    }).then(function (body) {
        dfd.resolve(body);
        createJSONtoaggregate(body)
    }, function (error) {
        dfd.reject(false);
        console.log(error);
    });
    return dfd.promise();
}

function createJSONtoaggregate(data) {
    var html = '', medic = 0, naive = 0, svm = 0, tree = 0, emotion = 0, ML = 0;
    var message = '', url = '', messagelink = '', shortstr = '', date = '', sme = 0, formatedDate = '', source = '';
    var strbody, id, type, status, user;
    if (data.responses[0].hits.hits.length == 0)
        $('body').html("All the indices are aggregated");
    _.forEach(data.responses[0].hits.hits, function (value, key) {
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
        message = value._source.t_message;
        url = value._source.t_urls;
        date = value._source.t_time;
        sme = value._source.t_sme;
        source = value._source.t_source;
        user = value._source.t_users;
        t_id = value._source.t_id;

        status = processedIndices.status;
        strbody = convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, source, status, user, t_id);
        updateStatus(index, type, id, status);
        createNode(aggregateIndex, type, id, strbody).done(function (body) {
            console.log(body);
        }).fail(function (obj) {
            handleError(obj);
        });
    });
}

function handleError(error) {
    console.log(error);
}

function convertToJSON(sme, medic, naive, svm, tree, ML, message, url, date, emotion, source, status, user, t_id) {
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

    objdata['t_status'] = status;

    objdata['score_emotion'] = emotion;

    objdata["t_users"] = user;

    if (t_id)
        objdata["t_id"] = t_id;
    else
        objdata['t_id'] = 0;

    return objdata;
}

function updateStatus(index, type, id, status) {
    client.update({
        index: index,
        type: type,
        id: id,
        body: {
            doc: {
                "t_status": status
            }
        }
    }, function (error, response) {
        $('.updatedcount').html(counter++ + " of " + totalCount);
        if (counter % 100 == 0)
            getdtt();
        console.log(error);
        console.log(response);
    });
}