package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InstantPickupProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkInstantPickupProductResponse;
import com.gdn.mta.bulk.repository.ProductInstantPickupRepository;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkInstantPickupProductDataServiceBean implements BulkProcessDataService {

  @Value("${bulk.process.getSummaryInstantPickup.batch.size:100}")
  private Integer getSummaryInstantPickupBatchSize;

  @Autowired
  public ProductInstantPickupRepository productInstantPickupRepository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    log.info("#bulkInstantPickupProduct-getData - request: {}", request);
    InstantPickupProductDownloadRequest instantPickupProductDownloadRequest =
        (InstantPickupProductDownloadRequest) request;
    List<List<OfflineItemInstantPickupBulkDownloadResponse>> responsesList = new ArrayList<>();
    int pageNumber = 0;
    while (true) {
      List<OfflineItemInstantPickupBulkDownloadResponse> responses =
          productInstantPickupRepository.findSummaryInstantPickupBulkDownload(
              instantPickupProductDownloadRequest, pageNumber, getSummaryInstantPickupBatchSize);
      if (CollectionUtils.isEmpty(responses)) {
        break;
      }
      responsesList.add(responses);
      pageNumber++;
    }
    List<OfflineItemInstantPickupBulkDownloadResponse> flatResponses =
        responsesList.stream().flatMap(List::stream).collect(Collectors.toList());
    return new BulkInstantPickupProductResponse(flatResponses);
  }

}
