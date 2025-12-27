package com.gdn.mta.bulk.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class StoreCopyServiceWrapperImpl implements StoreCopyServiceWrapper {

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Override
  public String downloadTargetSellerTemplate(String storeId , String sellerCode, String username, String requestId) throws Exception {
    BulkDownloadRequest bulkDownloadRequest =
        bulkDownloadServiceBeanUtil.getTargetSellerTemplateForCopyStore(sellerCode, username, requestId);
    return bulkProcessDownloadService.downloadAndOverwriteExcelFile(bulkDownloadRequest);
  }
}
