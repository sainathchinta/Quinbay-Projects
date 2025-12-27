package com.gdn.partners.product.analytics.service.bigQuery;

public interface DownloadSellerInfoBigQueryService {

  /**
   * Download from big query and store it in db
   *
   * @param storeId Store id
   * @param jobProcessType Job type
   * @param fetchHourThreshold Threshold of hour fetch
   * @param suggestedDateFetch Suggested Date Fetch
   * @param attributeName DS Extracted Attribute Name
   */
  void execute(String storeId, String jobProcessType, int fetchHourThreshold,
    int suggestedDateFetch, String attributeName);
}
