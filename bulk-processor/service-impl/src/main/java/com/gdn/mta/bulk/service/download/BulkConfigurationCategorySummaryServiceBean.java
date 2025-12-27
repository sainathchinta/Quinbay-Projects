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
import com.gdn.mta.bulk.models.download.CategoryConfigurationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkConfigSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.BulkConfigurationService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;

@Service(value = "bulkConfigurationCategorySummaryServiceBean")
public class BulkConfigurationCategorySummaryServiceBean implements BulkProcessDataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkConfigurationCategorySummaryServiceBean.class);

  @Autowired
  private BulkConfigurationService bulkConfigurationService;

  @Value("${bulk.configuration.download.size}")
  public int bulkConfigurationDownloadSize;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling data for category configuration summary for requestId {}, BulkProcessEntity {}",
        request.getRequestId(), request.getBulkProcessEntity());
    CategoryConfigurationDownloadRequest categoryConfigurationDownloadRequest =
        (CategoryConfigurationDownloadRequest) request;
    List<BulkConfigDataResponse> bulkConfigDataResponseList = new ArrayList<>();
    if (!CollectionUtils.isEmpty((categoryConfigurationDownloadRequest.getDataList()))) {
      LOGGER.info("Calling PCB for category configuration data for configType {} and CodesList {}",
          categoryConfigurationDownloadRequest.getConfigType(), categoryConfigurationDownloadRequest.getDataList());
      bulkConfigDataResponseList = bulkConfigurationService
          .fetchConfigDetailsByCodes(categoryConfigurationDownloadRequest.getStoreId(),
              categoryConfigurationDownloadRequest.getConfigType(), request,
              categoryConfigurationDownloadRequest.getDataList());
      BulkConfigSummaryResponse bulkConfigSummaryResponse = new BulkConfigSummaryResponse();
      bulkConfigSummaryResponse.setBulkConfigDataResponseList(bulkConfigDataResponseList);
      bulkConfigSummaryResponse.setBulkProcessEntity(request.getBulkProcessEntity());
      return bulkConfigSummaryResponse;
    } else {
      int pageNumber = 0;
      long totalCount;
      ConfigurationFilterRequest configurationFilterRequest =
          ConfigurationFilterRequest.builder().categoryCode(categoryConfigurationDownloadRequest.getCategoryCode())
              .reviewConfig(categoryConfigurationDownloadRequest.getReviewConfig())
              .searchKey(categoryConfigurationDownloadRequest.getSearchKey())
              .sortOrder(categoryConfigurationDownloadRequest.getSortOrder()).build();
      do {
        GdnRestListResponse<CategoryConfigurationFilterResponse> pageableResponse = bulkConfigurationService
            .getCategoryConfigurationList(categoryConfigurationDownloadRequest.getStoreId(), Constant.CHANNEL_ID,
                Constant.CLIENT_ID, request.getRequestId(), request.getUsername(), configurationFilterRequest,
                pageNumber, bulkConfigurationDownloadSize);
        totalCount = pageableResponse.getPageMetaData().getTotalRecords();
        bulkConfigDataResponseList
            .addAll(getBulkConfigDataResponseFromCategoryConfigurationFilterResponse(pageableResponse.getContent()));
        ++pageNumber;
      } while (pageNumber * bulkConfigurationDownloadSize < totalCount);
      BulkConfigSummaryResponse bulkConfigSummaryResponse = new BulkConfigSummaryResponse();
      bulkConfigSummaryResponse.setBulkConfigDataResponseList(bulkConfigDataResponseList);
      bulkConfigSummaryResponse.setBulkProcessEntity(request.getBulkProcessEntity());
      return bulkConfigSummaryResponse;
    }
  }

  private List<BulkConfigDataResponse> getBulkConfigDataResponseFromCategoryConfigurationFilterResponse(
      List<CategoryConfigurationFilterResponse> content) {
    ArrayList<BulkConfigDataResponse> bulkConfigDataResponses = new ArrayList<>();
    for (CategoryConfigurationFilterResponse response : content) {
      BulkConfigDataResponse bulkConfigDataResponse =
          BulkConfigDataResponse.builder().code(response.getCategoryCode()).name(response.getCategoryName())
              .reviewConfig(response.getReviewConfig()).build();
      bulkConfigDataResponses.add(bulkConfigDataResponse);
    }
    return bulkConfigDataResponses;
  }
}
