package com.gdn.mta.bulk.service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class BulkDownloadServiceWrapperImpl implements BulkDownloadServiceWrapper {

  private static final String BULK_VENDOR_DOWNLOAD_EMAIL_TO = "bulkVendorDownloadEmailTo";
  private static final String BULK_VENDOR_DOWNLOAD_EMAIL_CC = "bulkVendorDownloadEmailCc";
  private static final String EMAIL_TO_MUST_NOT_BE_BLANK = "Email To value must not be blank";

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Async
  @Override
  public void downloadAndSendVendorProductMail(String storeId,
      FilterSummaryRequest filterSummaryRequest, String requestId, String username)
      throws Exception {
    SystemParameterConfig systemParameterConfigEmailTo =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, BULK_VENDOR_DOWNLOAD_EMAIL_TO);
    SystemParameterConfig systemParameterConfigEmailCc =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, BULK_VENDOR_DOWNLOAD_EMAIL_CC);
    String emailTo = systemParameterConfigEmailTo.getValue();
    String emailCc = systemParameterConfigEmailCc.getValue();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(emailTo), EMAIL_TO_MUST_NOT_BE_BLANK);
    bulkProcessDownloadService.downloadAll(bulkDownloadServiceBeanUtil
        .getVendorBulkDownloadRequest(emailTo, emailCc, filterSummaryRequest, requestId, username));
  }
}
