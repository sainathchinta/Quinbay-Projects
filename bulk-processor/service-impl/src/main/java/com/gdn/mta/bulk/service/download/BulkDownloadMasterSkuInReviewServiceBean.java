package com.gdn.mta.bulk.service.download;

import com.gdn.mta.bulk.models.download.AnchorMappingModel;
import com.gdn.mta.bulk.models.download.AnchorMappingRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.DownloadInReviewAnchorsWebRequest;
import com.gdn.mta.bulk.models.download.MasterSkuInReviewDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuInReviewDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import com.gdn.mta.bulk.service.MasterSkuReviewOutboundService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Service
public class BulkDownloadMasterSkuInReviewServiceBean implements BulkProcessDataService {

  @Autowired
  private MasterSkuReviewOutboundService masterSkuReviewOutboundService;

  @Value("${master.sku.review.download.in.review.limit}")
  private int masterSkuReviewDownloadInReviewLimit;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    log.info(
      "Calling Get Data for master sku in-review data for requestId {}, " + "BulkProcessEntity "
        + "{}", request.getRequestId(), request.getBulkProcessEntity());
    MasterSkuInReviewDownloadRequest masterSkuInReviewDownloadRequest =
      (MasterSkuInReviewDownloadRequest) request;
    List<InReviewAnchorDownloadResponse> inReviewAnchorDownloadResponseList =
      getMasterSkuInReviewDownloadData(request.getRequestId(), masterSkuInReviewDownloadRequest);
    BulkMasterSkuInReviewDownloadResponse bulkMasterSkuInReviewDownloadResponse =
      new BulkMasterSkuInReviewDownloadResponse();
    bulkMasterSkuInReviewDownloadResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    bulkMasterSkuInReviewDownloadResponse.setAnchorDownloadResponseList(
      inReviewAnchorDownloadResponseList);
    return bulkMasterSkuInReviewDownloadResponse;
  }

  private List<InReviewAnchorDownloadResponse> getMasterSkuInReviewDownloadData(String requestId,
    MasterSkuInReviewDownloadRequest request) {
    DownloadInReviewAnchorsWebRequest downloadInReviewAnchorsWebRequest =
      DownloadInReviewAnchorsWebRequest.builder().keyword(request.getKeyword())
        .assignedTo(request.getAssignedTo()).categoryCode(request.getCategoryCode())
        .startDate(request.getStartDate()).endDate(request.getEndDate())
        .clusterRequestList(convertToAnchorMappingModel(request.getClusterRequestList())).build();
    int numberOfBatchPages;
    int limit = masterSkuReviewDownloadInReviewLimit;
    if (CollectionUtils.isNotEmpty(downloadInReviewAnchorsWebRequest.getClusterRequestList())) {
      // to set the limit as size of selected items in requests for SELECTED DOWNLOAD
      limit = downloadInReviewAnchorsWebRequest.getClusterRequestList().size();
      numberOfBatchPages = 1;
    } else if (request.getSize() <= limit) {
      // to set the limit as size in request if size is less than provided limit
      limit = request.getSize();
      numberOfBatchPages = 1;
    } else {
      // to calculate the total batches for ALL DOWNLOAD if size is large
      numberOfBatchPages = (int) Math.ceil((double) request.getSize() / limit);
    }
    int batchPage = request.getPage() * numberOfBatchPages;
    int batchEndPage = batchPage + numberOfBatchPages;
    List<InReviewAnchorDownloadResponse> inReviewAnchorDownloadResponseList = new ArrayList<>();
    List<InReviewAnchorDownloadResponse> responseList;
    do {
      responseList =
        masterSkuReviewOutboundService.fetchAnchorMappingDownloadList(requestId, batchPage, limit,
          downloadInReviewAnchorsWebRequest);
      inReviewAnchorDownloadResponseList.addAll(
        Optional.ofNullable(responseList).orElse(Collections.emptyList()));
      batchPage++;
    } while (batchPage < batchEndPage && CollectionUtils.isNotEmpty(responseList));
    return inReviewAnchorDownloadResponseList;
  }

  private List<AnchorMappingModel> convertToAnchorMappingModel(
    List<AnchorMappingRequest> requests) {
    return requests.stream().map(this::convertSingleAnchorMapping).collect(Collectors.toList());
  }

  private AnchorMappingModel convertSingleAnchorMapping(AnchorMappingRequest request) {
    return AnchorMappingModel.builder().firstAnchor(request.getFirstAnchorSku())
      .secondAnchor(request.getSecondAnchorSku()).build();
  }
}
