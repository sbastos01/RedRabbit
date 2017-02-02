var counter = 1;
var totalCount;
$(document).ready(function () {
    callRequestedData(0);
    getCount();
});

function getCount() {
    client.count({
        index: MLvalueupdate.index
        
    }, function (err, response) {
        totalCount = response.count;
        $('.updatedcount').append("0 of " + totalCount);
    });
}

function CalculateML(data) {
    var medic = 0;
    var naive = 0;
    var svm = 0;
    var tree = 0;
    var emotion = 0;
    var ML = 0;
    if (data && data.hits && data.hits.hits) {
        _.forEach(data.hits.hits, function (value, key) {
            medic = value._source.medic;
            naive = convertval(value._source.score_naive);
            svm = convertval(value._source.score_svm);
            tree = convertval(value._source.score_tree);
            if (value._source.score_emotion)
                emotion = parseFloat(value._source.score_emotion[0].toFixed(2));
            ML = medic + naive + svm + tree + emotion;
            updateData(value._index, value._type, value._id, ML);
        });
    }
}

function updateData(index, value, id, ML) {
    client.update({
        index: index,
        type: value,
        id: id,
        body: {
            doc: {
                "t_ml": ML
            }
        }
    }, function (error, response) {
        $('.updatedcount').html(counter++ + " of " + totalCount);
        if (counter % 100 == 0)
            callRequestedData(counter);
        console.log(error);
        console.log(response);
    });
}

function callRequestedData(count) {
    checkConnection().done(function () {
        requestData(MLvalueupdate.index, MLvalueupdate.search, 100, count).done(function (body) {
            CalculateML(body)
        }).fail(function (error) {
            $('#maincontainer').html('elasticsearch cluster is down!');
        });
    })
    .fail(function (obj) {
        $(body).html('elasticsearch cluster is down!');
    });
}