package com.gdn.mta.bulk.service.download;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Pageable;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ReviewProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkReviewProductResponse;
import com.gdn.mta.bulk.repository.ProductLevel1Repository;

/**
 * Created by govind on 06/02/2019 AD.
 */
public class BulkDownloadReviewProductServiceBeanTest {

  @InjectMocks
  private BulkDownloadReviewProductServiceBean bulkDownloadReviewProductServiceBean;

  @Mock
  private ProductLevel1Repository productLevel1Repository;

  private static final String ASSIGNED_TO = "assignedTo";
  private static final String SEARCH_KEYWORD = "searchKeyword";
  private static final String SORT_COLUMN = "sortColumn";
  private static final String SORT_ORDER = "asc";
  private static final String STATUS_FILTER = "statusFilter";
  private static final String TIME_FILTER = "timeFilter";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "category_code";
  private static final String USERNAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String LANGUAGE = "eng";
  private static final String FILENAME = "file-name";

  private static final String PRODUCT_ID = "product-id";
  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";
  private static final String BRAND = "brand";
  private static final String CATEGORY_NAME = "category-name";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String STATE = "state";
  private static final int MAX_SIZE = 1000;

  private BulkDownloadRequest bulkDownloadRequest;
  private GdnRestListResponse<ReviewProductResponse> reviewProductResponses;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    ReviewProductResponse reviewProductResponse = ReviewProductResponse.builder().productCode(PRODUCT_CODE)
        .productId(PRODUCT_ID).productName(PRODUCT_NAME).assignedTo(ASSIGNED_TO).businessPartnerCode(BUSINESS_PARTNER_CODE)
        .businessPartnerName(BUSINESS_PARTNER_NAME).categoryCode(CATEGORY_CODE).categoryName(CATEGORY_NAME).brand(BRAND)
        .submittedDate(new Date()).state(STATE).isSourceDb(false).build();

    List<ReviewProductResponse> reviewProductResponseList = new ArrayList<>();
    reviewProductResponseList.add(reviewProductResponse);
    PageMetaData pageMetaData = new PageMetaData(0, 1000, 7000);
    reviewProductResponses =
        new GdnRestListResponse<ReviewProductResponse>(null, null, true, reviewProductResponseList,
            pageMetaData, null);
    ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder builder =
        new ReviewProductDownloadRequest.ReviewProductDownloadRequestBuilder();
    bulkDownloadRequest = builder.assignedTo(ASSIGNED_TO)
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .categoryCode(CATEGORY_CODE)
        .sortColumn(SORT_COLUMN)
        .sortOrder(SORT_ORDER)
        .searchKeyword(SEARCH_KEYWORD)
        .statusFilter(STATUS_FILTER)
        .timeFilter(TIME_FILTER)
        .downloadType(DownloadType.ALL).fileType(FileType.XLSX).bulkProcessType(BulkProcessEntity.REVIEW_PRODUCTS)
        .directDownload(false).filename(FILENAME).emailCC(USERNAME).emailTo(USERNAME).username(USERNAME)
        .language(LANGUAGE).request(REQUEST_ID).build();
  }

  @Test
  public void getDataTest() throws Exception{
    Mockito.when(this.productLevel1Repository
        .getReviewProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID),
            Mockito.any(SummaryFilterRequest.class), Mockito.any(Pageable.class)))
        .thenReturn(reviewProductResponses);
    BulkReviewProductResponse response =
        (BulkReviewProductResponse) this.bulkDownloadReviewProductServiceBean
            .getData(bulkDownloadRequest);
    Mockito.verify(this.productLevel1Repository, Mockito.times(7))
        .getReviewProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID),
            Mockito.any(SummaryFilterRequest.class), Mockito.any(Pageable.class));
    Assertions.assertEquals(BulkProcessEntity.REVIEW_PRODUCTS, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.productLevel1Repository);
  }
}
