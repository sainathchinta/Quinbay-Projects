package com.gdn.mta.bulk.factory;

import com.gdn.mta.bulk.BulkDownloadErrorCode;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * Created by keshashah on 25/10/16.
 */
public class BulkProcessHelperFactory {
  private Map<BulkProcessEntity, BulkProcessHelper> bulkProcessHelper;
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProcessHelperFactory.class);

  public BulkProcessHelperFactory() {
  }

  public BulkProcessHelper getHelper(BulkDownloadRequest request) throws Exception {
    if (bulkProcessHelper.containsKey(request.getBulkProcessEntity())) {
      return bulkProcessHelper.get(request.getBulkProcessEntity());
    }
    LOGGER.error("Bulk Download: No Helper Found for Entity {} and request {}",
        request.getBulkProcessEntity(), request.getRequestId());
    throw new BulkDownloadException(BulkDownloadErrorCode.ENTITY_NOT_FOUND.toString(),
        BulkDownloadErrorCode.ENTITY_NOT_FOUND.getErrorMessage());
  }

  public void setBulkProcessHelper(Map<BulkProcessEntity, BulkProcessHelper> bulkProcessHelper) {
    this.bulkProcessHelper = bulkProcessHelper;
  }
}
