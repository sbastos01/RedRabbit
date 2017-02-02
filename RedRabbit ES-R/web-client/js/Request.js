
function requestData(index, search, size, from, sortcolumn, order) {
    if (!index)
        index = '*'
    if (!search)
        search = '*';
    if (!size)
        size = 10;
    if (!from)
        from = 0;

    if (!order)
        order = 'asc';

    var sortformat = '';
    if (sortcolumn)
        sortformat = sortcolumn + ":" + order;
    var dfd = $.Deferred()
    client.search({
        index: index,
        q: search,
        size: size,
        from: from,
        "sort": sortformat
    }).then(function (body) {
        dfd.resolve(body);
    }, function (error) {
        dfd.reject(false);
    });
    return dfd.promise();
}

function createNode(index, type, id, body) {
    var dfd = $.Deferred();
    client.create({
        index: index,
        type: "ms01",
        id: id,
        body: body
    }).then(function (body) {
        dfd.resolve(body);
    }, function (error) {
        dfd.reject(error);
    });
    return dfd.promise();
}

function deleteNode(index, type, id) {
    var dfd = $.Deferred();
    client.delete({
        index: index,
        type: type,
        id: id
    }).then(function (body) {
        dfd.resolve(body);
    }, function (error) {
        dfd.reject(error);
    });
    return dfd.promise();
}