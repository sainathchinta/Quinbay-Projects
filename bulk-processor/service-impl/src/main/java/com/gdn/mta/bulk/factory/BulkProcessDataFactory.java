package com.gdn.mta.bulk.factory;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.mta.bulk.BulkDownloadErrorCode;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDataService;

/**
 * Created by keshashah on 25/10/16.
 */
public class BulkProcessDataFactory {
  private Map<BulkProcessEntity, BulkProcessDataService> bulkDataService;
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProcessDataFactory.class);

  public BulkProcessDataFactory() {
  }


  public BulkProcessDataService getRepository(BulkDownloadRequest request)
      throws BulkDownloadException {
    if (bulkDataService.containsKey(request.getBulkProcessEntity())) {
      return bulkDataService.get(request.getBulkProcessEntity());
    }
    LOGGER.error("Bulk Download: No Data Factory Found for Entity {}, requestId {}",
        request.getBulkProcessEntity(), request.getRequestId());
    throw new BulkDownloadException(BulkDownloadErrorCode.ENTITY_NOT_FOUND.toString(),
        BulkDownloadErrorCode.ENTITY_NOT_FOUND.getErrorMessage());
  }

  public void setBulkDataService(Map<BulkProcessEntity, BulkProcessDataService> bulkDataService) {
    this.bulkDataService = bulkDataService;
  }
}
