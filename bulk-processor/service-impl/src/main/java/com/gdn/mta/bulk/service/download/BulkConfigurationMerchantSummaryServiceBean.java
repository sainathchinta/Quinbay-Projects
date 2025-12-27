package com.gdn.mta.bulk.service.download;


import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MerchantConfigurationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkConfigSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.BulkConfigurationService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;

@Service(value = "bulkConfigurationMerchantSummaryServiceBean")
public class BulkConfigurationMerchantSummaryServiceBean implements BulkProcessDataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkConfigurationMerchantSummaryServiceBean.class);

  @Autowired
  private BulkConfigurationService bulkConfigurationService;

  @Value("${bulk.configuration.download.size}")
  public int bulkConfigurationDownloadSize;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling data for merchant configuration summary for requestId {}, BulkProcessEntity {}",
        request.getRequestId(), request.getBulkProcessEntity());
    MerchantConfigurationDownloadRequest merchantConfigurationDownloadRequest =
        (MerchantConfigurationDownloadRequest) request;
    List<BulkConfigDataResponse> bulkConfigDataResponseList = new ArrayList<>();
    if (!CollectionUtils.isEmpty((merchantConfigurationDownloadRequest.getDataList()))) {
      LOGGER.info("Calling PCB for merchant configuration data for configType {} and CodesList {}",
          merchantConfigurationDownloadRequest.getConfigType(), merchantConfigurationDownloadRequest.getDataList());
      bulkConfigDataResponseList = bulkConfigurationService
          .fetchConfigDetailsByCodes(merchantConfigurationDownloadRequest.getStoreId(),
              merchantConfigurationDownloadRequest.getConfigType(), request,
              merchantConfigurationDownloadRequest.getDataList());
      BulkConfigSummaryResponse bulkConfigSummaryResponse = new BulkConfigSummaryResponse();
      bulkConfigSummaryResponse.setBulkConfigDataResponseList(bulkConfigDataResponseList);
      bulkConfigSummaryResponse.setBulkProcessEntity(request.getBulkProcessEntity());
      return bulkConfigSummaryResponse;
    } else {
      int pageNumber = 0;
      long totalCount;
      ConfigurationFilterRequest configurationFilterRequest =
          ConfigurationFilterRequest.builder().categoryCode(merchantConfigurationDownloadRequest.getCategoryCode())
              .reviewConfig(merchantConfigurationDownloadRequest.getReviewConfig())
              .searchKey(merchantConfigurationDownloadRequest.getSearchKey())
              .sortOrder(merchantConfigurationDownloadRequest.getSortOrder()).build();
      do {
        GdnRestListResponse<MerchantConfigurationFilterResponse> pageableResponse = bulkConfigurationService
            .getMerchantConfigurationList(merchantConfigurationDownloadRequest.getStoreId(), Constant.CHANNEL_ID,
                Constant.CLIENT_ID, request.getRequestId(), request.getUsername(), configurationFilterRequest,
                pageNumber, bulkConfigurationDownloadSize);
        totalCount = pageableResponse.getPageMetaData().getTotalRecords();
        bulkConfigDataResponseList
            .addAll(getBulkConfigDataResponseFromMerchantConfigurationFilterResponse(pageableResponse.getContent()));
        ++pageNumber;
      } while (pageNumber * bulkConfigurationDownloadSize < totalCount);
      BulkConfigSummaryResponse bulkConfigSummaryResponse = new BulkConfigSummaryResponse();
      bulkConfigSummaryResponse.setBulkConfigDataResponseList(bulkConfigDataResponseList);
      bulkConfigSummaryResponse.setBulkProcessEntity(request.getBulkProcessEntity());
      return bulkConfigSummaryResponse;
    }
  }

  private List<BulkConfigDataResponse> getBulkConfigDataResponseFromMerchantConfigurationFilterResponse(
      List<MerchantConfigurationFilterResponse> content) {
    ArrayList<BulkConfigDataResponse> bulkConfigDataResponses = new ArrayList<>();
    for (MerchantConfigurationFilterResponse response : content) {
      BulkConfigDataResponse bulkConfigDataResponse =
          BulkConfigDataResponse.builder().code(response.getMerchantCode()).name(response.getMerchantName())
              .reviewConfig(response.getReviewConfig()).build();
      bulkConfigDataResponses.add(bulkConfigDataResponse);
    }
    return bulkConfigDataResponses;
  }
}
