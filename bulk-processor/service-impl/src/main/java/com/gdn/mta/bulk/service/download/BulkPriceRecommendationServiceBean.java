package com.gdn.mta.bulk.service.download;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.BulkPriceRecommendationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.response.BulkPriceAnalyticsResponse;
import com.gdn.mta.bulk.service.PriceAnalyticsOutboundService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkPriceRecommendationServiceBean implements BulkProcessDataService{

  @Autowired
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    BulkPriceRecommendationDownloadRequest bulkPriceRecommendationDownloadRequest =
        (BulkPriceRecommendationDownloadRequest) request;
    List<DownloadSkuResponse> downloadSkuResponseGdnRestListResponse =
        priceAnalyticsOutboundService.getDownloadSkus(request.getUsername(), bulkPriceRecommendationDownloadRequest);
    BulkPriceAnalyticsResponse bulkPriceAnalyticsResponse = new BulkPriceAnalyticsResponse();
    bulkPriceAnalyticsResponse.setDownloadSkuResponseList(downloadSkuResponseGdnRestListResponse);
    bulkPriceAnalyticsResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    return bulkPriceAnalyticsResponse;
  }
}
