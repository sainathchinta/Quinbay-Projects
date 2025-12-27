package com.gdn.mta.bulk.util;

import static org.mockito.MockitoAnnotations.initMocks;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ReviewProductDownloadRequest;
import com.gdn.mta.bulk.models.download.VendorSummaryDownloadRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;

/**
 * Created by govind on 06/02/2019 AD.
 */
public class RequestConverterUtilTest {

  private static final String ASSIGNED_TO = "assignedTo";
  private static final String SEARCH_KEYWORD = "searchKeyword";
  private static final String SORT_COLUMN = "sortColumn";
  private static final String SORT_ORDER = "asc";
  private static final String STATUS_FILTER = "statusFilter";
  private static final String TIME_FILTER = "timeFilter";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "category_code";
  private static final String USERNAME = "user-name";
  private static final String LANGUAGE = "eng";
  private static final String FILENAME = "file-name";
  private static final String ASSIGNEE_EMAIL = "assignee-email";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String EMPTY_DROPDOWN_LIST_ELEMENT = "NA";
  private static final String FAULTY_IMAGE_TYPE = "faultyType";

  @InjectMocks
  private RequestConverterUtil requestConverterUtil;

  private BulkDownloadRequest bulkDownloadRequest ;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
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
        .language(LANGUAGE).build();
  }

  @Test
  public void toSummaryFilterRequestTest(){
    SummaryFilterRequest response = RequestConverterUtil
        .toSummaryFilterRequest((ReviewProductDownloadRequest) bulkDownloadRequest);
    Assertions.assertEquals(ASSIGNED_TO, response.getAssignedTo());
    Assertions.assertEquals(CATEGORY_CODE, response.getCategoryCode());
  }

  @Test
  public void fromVendorSummaryDownloadRequestToFilterSummaryRequestTest(){
    VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder vendorBuilder =
        new VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder();
    BulkDownloadRequest bulkDownloadRequestVendor = vendorBuilder
        .keyword(SEARCH_KEYWORD)
        .restrictedKeyword(null)
        .unrestrictedDownload(false)
        .timeFilterType(TIME_FILTER)
        .contentPending(Boolean.TRUE)
        .imagePending(Boolean.TRUE)
        .assignment(Boolean.TRUE)
        .categoryCode(CATEGORY_CODE)
        .isCnCategory(Boolean.TRUE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .assigneeEmailId(EMPTY_DROPDOWN_LIST_ELEMENT)
        .vendorCode(VENDOR_CODE)
        .sortOrderByCreatedDate(SORT_ORDER)
        .faultyImageType(FAULTY_IMAGE_TYPE)
        .brandPending(Boolean.TRUE)
        .downloadType(DownloadType.ALL).fileType(FileType.XLSX).bulkProcessType(BulkProcessEntity.VENDOR_FILTERED_PRODUCT)
        .directDownload(false).filename(FILENAME).emailCC(USERNAME).emailTo(USERNAME).username(USERNAME)
        .language(LANGUAGE).build();
    FilterSummaryRequest filterSummaryRequest = RequestConverterUtil
        .fromVendorSummaryDownloadRequestToFilterSummaryRequest(
            (VendorSummaryDownloadRequest) bulkDownloadRequestVendor);
    Assertions.assertEquals(VENDOR_CODE, filterSummaryRequest.getVendorCode());
    Assertions.assertEquals(CATEGORY_CODE, filterSummaryRequest.getCategoryCode());
    Assertions.assertEquals(StringUtils.EMPTY, filterSummaryRequest.getAssigneeEmailId());
    Assertions.assertEquals(FAULTY_IMAGE_TYPE, filterSummaryRequest.getFaultyImageType());
    Assertions.assertTrue(filterSummaryRequest.getBrandPending());
    Assertions.assertNull(filterSummaryRequest.getRestrictedKeyword());
  }
}
