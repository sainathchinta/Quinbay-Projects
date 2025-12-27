package com.gdn.partners.product.analytics.model;

public interface SubmitBigQueryProcessSteps {

  String DOWNLOAD_FROM_GCS = "downloadFromGCS";

  String SPLITTING_FILE = "splittingFile";

  String WRITE_RECORDS_FROM_FILE = "writeRecordsFromFile";

}
