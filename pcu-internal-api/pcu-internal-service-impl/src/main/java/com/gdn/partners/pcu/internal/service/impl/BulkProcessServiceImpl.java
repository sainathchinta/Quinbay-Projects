package com.gdn.partners.pcu.internal.service.impl;

import java.util.UUID;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.blibli.oss.kafka.producer.KafkaProducer;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.service.BulkProcessService;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.ReviewProductDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 30/01/2019 AD.
 */

@Slf4j
@Service
public class BulkProcessServiceImpl implements BulkProcessService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public void bulkDownloadScreeningProducts(String username,
      ReviewProductsFilterRequest reviewProductsFilterRequest, String lang) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking preProcessing of Bulk Screening Products Download. requestId: {},", requestId);
    String fileName =
        new StringBuilder().append(requestId).append(".").append(FileType.XLSX.name().toLowerCase())
            .toString();
    ReviewProductDownloadRequest reviewProductDownloadRequest =
        ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder()
        .assignedTo(reviewProductsFilterRequest.getAssignedTo())
        .businessPartnerCode(reviewProductsFilterRequest.getBusinessPartnerCode())
        .categoryCode(reviewProductsFilterRequest.getCategoryCode())
        .sortColumn(reviewProductsFilterRequest.getSortColumn())
        .sortOrder(reviewProductsFilterRequest.getSortColumn())
        .searchKeyword(reviewProductsFilterRequest.getSearchKeyword())
        .statusFilter(reviewProductsFilterRequest.getStatusFilter())
        .timeFilter(reviewProductsFilterRequest.getTimeFilter())
        .downloadType(DownloadType.ALL).fileType(FileType.XLSX).bulkProcessEntity(BulkProcessEntity.REVIEW_PRODUCTS)
        .directDownload(false).filename(fileName).emailTo(username).username(username)
        .language(lang).requestId(requestId).build();
    this.kafkaProducer.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
        reviewProductDownloadRequest);
  }
}
