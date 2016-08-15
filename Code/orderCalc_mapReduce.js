// Start mongod
// mongod --dbpath D:/MongodbBin/dataCurrent --setParameter failIndexKeyTooLong=false --noIndexBuildRetry

// ---------------------------------------------------------------------------
// Map reduce to count order (37.25mins)
// ---------------------------------------------------------------------------

var mapper = function() {
	var key = this.Patent;
	var value = {
		order: 1,
		orderExaminer: 0,
		orderOther: 0
	};
	if (this.CitedBy == "cited by examiner") {
		value.orderExaminer = 1;
	} else {
		value.orderOther = 1;
	}
	emit(key, value);
};

var reducer = function(keyPatent, valueCitedby) {
	var citEx = 0;
	var ord = 0;
	var citOth = 0;
	for (var i = valueCitedby.length - 1; i >= 0; i--) {
		citEx += valueCitedby[i].orderExaminer;
		ord += valueCitedby[i].order;
		citOth += valueCitedby[i].orderOther;
	}
	return {order: ord, orderExaminer: citEx, orderOther: citOth};
};

db.citations.mapReduce(
	mapper,
	reducer,
	{
		out: "patentOrders"
	}
);

// ---------------------------------------------------------------------------
// Map reduce to join collections
// ---------------------------------------------------------------------------

var patentMap = function() {
	var key = this.Patent;
	var value  = {
		Date: this.Date, 
		MainClassification: this.MainClassification,
		FurtherClassification: this.FurtherClassification,
		Order: this.Order,
		Date2: this.Date2,
		Order2: 0,
		OrderExaminer: 0,
		OrderOther: 0
	}
	emit(key, value);
};

var ordersMap = function() {
	var key = this._id;
	var value = {
		Date: 0,
		MainClassification: 0,
		FurtherClassification: 0,
		Order: 0,
		Date2: 0,
		Order2: this.value.order,
		OrderExaminer: this.value.orderExaminer,
		OrderOther: this.value.orderOther
	};
	emit(key, value);
};

var reducer = function(key, values) {
	var result = {
		Date: 0,
		MainClassification: "0",
		FurtherClassification: "0",
		Order: 0,
		Date2: 0,
		Order2: 0,
		OrderExaminer: 0,
		OrderOther: 0
	};

	values.forEach(function(value) {
		if (value.Date !== 0) {
			result.Date = value.Date;
			result.MainClassification = value.MainClassification;
			result.FurtherClassification = value.FurtherClassification;
			result.Order = value.Order;
			result.Date2 = value.Date2;
		} else {
			result.Order2 = value.Order2;
			result.OrderExaminer = value.OrderExaminer;
			result.OrderOther = value.OrderOther;
		}
	});

	return result;
};

res = db.patents.mapReduce(patentMap, reducer, {out: {reduce: 'patentsJoined'}})
res = db.patentOrders.mapReduce(ordersMap, reducer, {out: {reduce: 'patentsJoined'}})

// ---------------------------------------------------------------------------
// Map Reduce to group on year and return frequencies of different orders. 
// ---------------------------------------------------------------------------

var mapperOrder = function () {
	var key = { Year: this.value.Date2.substring(0,4),
		Order: this.value.Order
	}; 
	var value = {
		Examiner: 0,
		Other: 0,
		Total: 1,
		Total2: 0
	}
	emit(key, value);}

var mapperOrder2 = function () {
	var key = { Year: this.value.Date2.substring(0,4),
		Order: this.value.Order2
	}; 
	var value = {
		Examiner: 0,
		Other: 0,
		Total: 0,
		Total2: 1
	}
	emit(key, value);}

var mapperExaminer = function () {
	var key = { Year: this.value.Date2.substring(0,4),
		Order: this.value.OrderExaminer
	}; 
	var value = {
		Examiner: 1,
		Other: 0,
		Total: 0,
		Total2: 0
	}
	emit(key, value);
}

var mapperOther = function () {
	var key = { Year: this.value.Date2.substring(0,4),
		Order: this.value.OrderOther
	}; 
	var value = {
		Examiner: 0,
		Other: 1,
		Total: 0,
		Total2: 0
	}
	emit(key, value);
}

var reducer = function(key, values) {
	result = {
		Examiner: 0,
		Other: 0,
		Total: 0,
		Total2: 0,
	};

	for (var i = 0; i < values.length; i++) {
		result.Examiner += values[i].Examiner;
		result.Other += values[i].Other;
		result.Total += values[i].Total;
		result.Total2 += values[i].Total2;
	}
	return(result);
}

res = db.patentsJoined.mapReduce(mapperOrder, reducer, {out: {reduce: 'orderFrequencies'}})
res = db.patentsJoined.mapReduce(mapperOther, reducer, {out: {reduce: 'orderFrequencies'}})
res = db.patentsJoined.mapReduce(mapperExaminer, reducer, {out: {reduce: 'orderFrequencies'}})
res = db.patentsJoined.mapReduce(mapperOrder2, reducer, {out: {reduce: 'orderFrequencies'}})
db.orderFrequencies.find()

// ---------------------------------------------------------------------------
// project only OrderExaminer,OrderOther for each patent to be used in scatterplot
// ---------------------------------------------------------------------------

db.patentsJoined.aggregate([
    	{$project : { "value.OrderExaminer" : 1, "value.OrderOther" : 1, _id : 0}},
    	{$out: "ExaminerOtherScatterplot"}
	])

// ---------------------------------------------------------------------------
// Map reduce to count order of citations (37.25mins)
// ---------------------------------------------------------------------------

var mapper = function() {
	var key = this.Citation;
	var value = {
		order: 1,
		orderExaminer: 0,
		orderOther: 0
	};
	if (this.CitedBy == "cited by examiner") {
		value.orderExaminer = 1;
	} else {
		value.orderOther = 1;
	}
	emit(key, value);
};

var reducer = function(keyPatent, valueCitedby) {
	var citEx = 0;
	var ord = 0;
	var citOth = 0;
	for (var i = valueCitedby.length - 1; i >= 0; i--) {
		citEx += valueCitedby[i].orderExaminer;
		ord += valueCitedby[i].order;
		citOth += valueCitedby[i].orderOther;
	}
	return {order: ord, orderExaminer: citEx, orderOther: citOth};
};

db.citations.mapReduce(
	mapper,
	reducer,
	{
		out: "citationOrders"
	}
);

// ---------------------------------------------------------------------------
// Create CitationsJoined which adds "PatentDate"
// ---------------------------------------------------------------------------
// db.citationsOld.find({ Date: { $gt: 18000000, $lte: 19000000}}).count()
db.citationsOld.find({ Date: { $gt: 18000000, $lte: 19000000}}).noCursorTimeout().forEach(function (citationDoc){
	var patentDoc = db.patentsOld.findOne({Patent: citationDoc.Patent});
	if (patentDoc != null) {
		citationDoc.PatentDate = patentDoc.Date2;
		var msDiff = Date.parse(patentDoc.Date2) - Date.parse(citationDoc.Date2)
		citationDoc.DayLag = msDiff/86400000 
	  	db.citationsOld.save(citationDoc);
	}
});