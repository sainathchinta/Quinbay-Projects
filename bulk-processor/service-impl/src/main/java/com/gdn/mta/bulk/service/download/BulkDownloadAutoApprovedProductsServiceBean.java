package com.gdn.mta.bulk.service.download;

import com.gdn.mta.bulk.models.AutoApprovedSelectedDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedWebRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedListWebResponse;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedProductsResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkAutoApprovedProductsDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.ProductAnalyticsOutboundService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
public class BulkDownloadAutoApprovedProductsServiceBean implements BulkProcessDataService {

  @Autowired
  private ProductAnalyticsOutboundService productAnalyticsOutboundService;

  @Value("${auto.approved.products.fetch.batch.size}")
  public int autoApprovedProductsFetchBatchSize;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    log.info("Calling Get Data to fetch auto approved products with requestId {}, "
        + "BulkProcessEntity {}", request.getRequestId(), request.getBulkProcessEntity());
    AutoApprovedProductsDownloadRequest downloadItemsRequest =
        (AutoApprovedProductsDownloadRequest) request;
    BulkAutoApprovedProductsDownloadResponse bulkAutoApprovedProductsDownloadResponse =
        new BulkAutoApprovedProductsDownloadResponse();
    bulkAutoApprovedProductsDownloadResponse.setAutoApprovedProductsResponses(
        getAutoApprovedProductsDownloadData(request.getRequestId(), downloadItemsRequest));
    bulkAutoApprovedProductsDownloadResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    return bulkAutoApprovedProductsDownloadResponse;
  }

  private List<AutoApprovedProductsResponse> getAutoApprovedProductsDownloadData(String requestId,
      AutoApprovedProductsDownloadRequest request) {

    List<AutoApprovedProductsResponse> response = new ArrayList<>();

    AutoApprovedWebRequest downloadAutoApprovedProductsWebRequest =
        AutoApprovedWebRequest.builder().keyword(request.getKeyword())
            .assignedTo(request.getAssignedTo()).categoryCode(request.getCategoryCode())
            .sellerCode(request.getSellerCode()).sortOrder(request.getSortOrder())
            .b2bActivated(request.getB2bActivated()).productCodeList(request.getProductCodeList())
            .build();

    if (CollectionUtils.isEmpty(downloadAutoApprovedProductsWebRequest.getProductCodeList())) {
      int startPage = 0;

      List<AutoApprovedListWebResponse> autoApprovedListWebResponses;
      List<AutoApprovedListWebResponse> responseList = new ArrayList<>();
      do {
        autoApprovedListWebResponses =
            productAnalyticsOutboundService.fetchAutoApprovedProductsDownloadList(requestId,
                startPage, autoApprovedProductsFetchBatchSize, downloadAutoApprovedProductsWebRequest);
        responseList.addAll(autoApprovedListWebResponses);
        startPage++;
      } while (CollectionUtils.isNotEmpty(autoApprovedListWebResponses)
          && autoApprovedListWebResponses.size() <= autoApprovedProductsFetchBatchSize);
      response = addRequiredFields(responseList);
    } else {
      AutoApprovedSelectedDownloadRequest autoApprovedSelectedDownloadRequest =
          new AutoApprovedSelectedDownloadRequest();
      autoApprovedSelectedDownloadRequest.setProductCodes(request.getProductCodeList());
      List<AutoApprovedListWebResponse> autoApprovedSelectedDownload =
          productAnalyticsOutboundService.fetchAutoApprovedProductsSelectedDownloadList(requestId,
              autoApprovedSelectedDownloadRequest);
      response = addRequiredFields(autoApprovedSelectedDownload);
    }
    return response;
  }

  List<AutoApprovedProductsResponse> addRequiredFields(
      List<AutoApprovedListWebResponse> autoApprovedSelectedDownload) {
    List<AutoApprovedProductsResponse> response = new ArrayList<>();
    for (AutoApprovedListWebResponse autoApprovedListWebResponse : autoApprovedSelectedDownload) {
      AutoApprovedProductsResponse autoApprovedProductsResponse =
          getAutoApprovedProductsResponse(autoApprovedListWebResponse);
      response.add(autoApprovedProductsResponse);
    }
    return response;
  }

  private static AutoApprovedProductsResponse getAutoApprovedProductsResponse(
      AutoApprovedListWebResponse autoApprovedListWebResponse) {
    AutoApprovedProductsResponse autoApprovedProductsResponse = new AutoApprovedProductsResponse();
    autoApprovedProductsResponse.setProductCode(autoApprovedListWebResponse.getProductCode());
    autoApprovedProductsResponse.setProductName(autoApprovedListWebResponse.getProductName());
    autoApprovedProductsResponse.setCategory(autoApprovedListWebResponse.getCategoryName());
    autoApprovedProductsResponse.setAssignedTo(autoApprovedListWebResponse.getAssignedTo());
    autoApprovedProductsResponse.setStoreName(
        Objects.nonNull(autoApprovedListWebResponse.getSeller()) ?
            autoApprovedListWebResponse.getSeller().getSellerName() :
            null);
    return autoApprovedProductsResponse;
  }
}
