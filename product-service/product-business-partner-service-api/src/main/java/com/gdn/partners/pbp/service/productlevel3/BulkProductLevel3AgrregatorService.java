package com.gdn.partners.pbp.service.productlevel3;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;

public interface BulkProductLevel3AgrregatorService {
  /**
   * @param filterRequest
   * @param fetchB2bData
   * @param fetchViewConfigByChannel
   * @return
   * @throws Exception
   */
  BulkDownloadProductLevel3Response aggregateProductLevel3SummaryByDb(ProductLevel3SummaryFilter filterRequest,
      boolean fetchB2bData, String fetchViewConfigByChannel) throws Exception;
}
