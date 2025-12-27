package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ReviewProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkReviewProductResponse;
import com.gdn.mta.bulk.repository.ProductLevel1Repository;
import com.gdn.mta.bulk.util.RequestConverterUtil;

/**
 * Created by govind on 29/01/2019 AD.
 */

@Service(value = "bulkReviewProductDataServiceBean")
public class BulkDownloadReviewProductServiceBean implements BulkProcessDataService{


  private static final Logger LOGGER = LoggerFactory.getLogger(BulkDownloadReviewProductServiceBean.class);
  private static final int MAX_SIZE = 1000;

  @Autowired
  private ProductLevel1Repository productLevel1Repository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling Get Data for Review Product data for requestId {}, BulkProcessEntity {}",
        request.getRequestId(), request.getBulkProcessEntity());
    ReviewProductDownloadRequest reviewProductDownloadRequest = (ReviewProductDownloadRequest) request;
    List<ReviewProductResponse> reviewProductResponseList = new ArrayList<>();
    int pageNumber = 0;
    long totalCount;
    do {
      Pageable pageable = PageRequest.of(pageNumber, MAX_SIZE);
      GdnRestListResponse<ReviewProductResponse> pageResponse = productLevel1Repository
          .getReviewProducts(reviewProductDownloadRequest.getUsername(),
              reviewProductDownloadRequest.getRequestId(),
              RequestConverterUtil.toSummaryFilterRequest(reviewProductDownloadRequest), pageable);
      totalCount = pageResponse.getPageMetaData().getTotalRecords();
      reviewProductResponseList.addAll(pageResponse.getContent());
      ++pageNumber;
    } while (pageNumber * MAX_SIZE < totalCount);
    BulkReviewProductResponse bulkReviewProductResponse = new BulkReviewProductResponse();
    bulkReviewProductResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    bulkReviewProductResponse.setResponseList(reviewProductResponseList);
    return bulkReviewProductResponse;
  }
}
