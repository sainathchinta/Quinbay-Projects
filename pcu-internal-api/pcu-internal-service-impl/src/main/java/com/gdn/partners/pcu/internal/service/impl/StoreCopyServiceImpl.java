package com.gdn.partners.pcu.internal.service.impl;

import java.util.UUID;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.service.StoreCopyService;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.StoreCopyDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.response.PendingDownloadProcessWebResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class StoreCopyServiceImpl implements StoreCopyService {

  private static final String LANGUAGE = "en";
  private static final String DELIMETER = ".";

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private XBulkOutboundService xBulkOutboundService;

  @Override
  public void downloadAllProductsBySellerCode(String username, String sellerCode) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking of Bulk Products Download for Store copy. requestId: {}, sellerCode : {} ", requestId,
        sellerCode);
    String fileName =
        new StringBuilder().append(requestId).append(DELIMETER).append(FileType.XLSX.name().toLowerCase()).toString();
    StoreCopyDownloadRequest vendorSummaryDownloadRequest =
        StoreCopyDownloadRequest.StoreCopyDownloadBuilder().merchantId(sellerCode).downloadType(DownloadType.ALL)
            .fileType(FileType.XLSX).bulkProcessEntity(BulkProcessEntity.STORE_COPY_PRODUCTS).directDownload(false)
            .filename(fileName).emailTo(username).username(username).language(LANGUAGE).requestId(requestId)
            .archived(false).build();
    this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, sellerCode,vendorSummaryDownloadRequest);
  }

  @Override
  public String downloadUploadTemplate(String sellerCode) {
    return xBulkOutboundService.downloadStoreCopyUploadTemplate(sellerCode);
  }

  @Override
  public PendingDownloadProcessWebResponse getPendingProcesses(String sellerCode, String userName,
      String processType) {
    BulkInternalPendingRequestResponse bulkInternalPendingRequestResponse =
        xBulkOutboundService.getPendingProcesses(sellerCode, userName, processType);
    return ResponseHelper.toPendingDownloadProcessWebResponse(bulkInternalPendingRequestResponse);
  }
}
