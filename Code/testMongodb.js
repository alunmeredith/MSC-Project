// --------------------------------------------------------------------------------------------
// Adds Patent Date (and Date2) to each citation
// --------------------------------------------------------------------------------------------

// var totalPatents = db.patentsOld.count();
// var processed = 0;
// var then = new Date();
// db.patents.find().noCursorTimeout().forEach(function (patentDoc){
// 	db.citations.update(
// 		{
// 			Patent: patentDoc.Patent,
// 			PatentDate: {$exists: false}
// 		},
// 		{$set: {PatentDate: patentDoc.Date, PatentDate2: patentDoc.Date2}},
// 		{multi: true});
	
// 	processed += 1;
// 	if (processed % 10000 == 0) {
// 		now = new Date();-
// 		print("Processed: ", processed, "/", totalPatents);
// 		print("Time Remaining: ", (totalPatents - processed) * (now - then)/(10000*1000*60));
// 		then = now;
// 	};
// });
// print("Processed All Patents-----------------------------------")

// --------------------------------------------------------------------------------------------
// Processes Date2 and PatentDate2 as ISOdate formats, computes ms time difference between them
// --------------------------------------------------------------------------------------------

var totalCitations = db.citations.find().count();
var then = new Date();
var processed = 33274687;
db.citationsAll.find({PatentDate3: {$exists: false}}).noCursorTimeout().forEach(function (citationDoc){
	
	citationDoc.PatentDate3 = new Date(citationDoc.PatentDate2);
	citationDoc.Date3 = new Date(citationDoc.Date2);
	citationDoc.msDiff = citationDoc.PatentDate3 - citationDoc.Date3;

	db.citationsAll.save(citationDoc)
	
	processed += 1;
	if (processed % 10000 == 0) {
		now = new Date();-
		print("Processed: ", processed, "/", totalCitations);
		print("Time Remaining: ", (totalCitations - processed) * (now - then)/(10000*1000*60));
		then = now;
	};
});
print("Processed All -------------------------------------------")

// --------------------------------------------------------------------------------------------
// Computes Patent Link Time and Citation Link Time and difference between them for each Citation
// --------------------------------------------------------------------------------------------

//cursor = db.citations.find({PatentLinkTime: {$exists: false}}).sort({PatentDate})
//cursor = db.citations.find({CitationLinkTime: {$exists: false}}).sort({Date})

//var totalPatents = db.patentsOld.count();
//var cursor = db.patentsOld.find({PatentLinkTime: {$exists: true}});
//var PatentLinkTime =  0 //cursor.sort({PatentLinkTime: -1}).limit(1).next().PatentLinkTime;
//var processed = 0; //cursor.count();
//print("Already Processed", processed, "  Max LinkTime: ", PatentLinkTime)
//var then = new Date();
//db.patentsOld.find().noCursorTimeout().forEach(function (patentDoc){ // {PatentLinkTime: {$exists: false}}
//	patentDoc.PatentLinkTime = PatentLinkTime;
//	PatentLinkTime += patentDoc.Order;
//	db.patentsOld.save(patentDoc);
//	
//	processed += 1;
//	if (processed % 10000 == 0) {
//		now = new Date();-
//		print("Processed: ", processed, "/", totalPatents);
//		print("Time Remaining: ", (totalPatents - processed) * (now - then)/(10000*1000*3600));
//		then = now;
//	};
//});
//print("Processed All -------------------------------------------");
