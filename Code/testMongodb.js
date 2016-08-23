// PatentLinkTime
var totalCitations = db.citationsAll.count();
var processedCursor = db.citationsAll.find({PatentLinkTime: {$exists: true}}).sort({PatentLinkTime: -1});
var	PatentLinkTime = 0;
var processed = 0;
if (processedCursor.count() != 0) {
	PatentLinkTime = processedCursor.limit(1).next().PatentLinkTime;
	processed = processedCursor.count();
}
print("Already processed ", processed, "/", totalCitations);
print("LinkTime: ",PatentLinkTime);
var then = new Date();
db.citationsAll.find({PatentLinkTime: {$exists: false}}).sort({PatentDate: 1}).noCursorTimeout().forEach(function(citation) {
	PatentLinkTime += 1;
	citation.PatentLinkTime = PatentLinkTime;
	db.citationsAll.save(citation);

	processed += 1;
	if (processed % 10000 == 0) {
		now = new Date();-
		print("Processed: ", processed, "/", totalCitations);
		print("Time Remaining: ", (totalCitations - processed) * (now - then)/(10000*1000*60));
		then = now;
	};
});

print("Done!")