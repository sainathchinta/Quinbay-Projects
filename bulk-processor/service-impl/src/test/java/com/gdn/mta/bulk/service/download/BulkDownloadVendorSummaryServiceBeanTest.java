package com.gdn.mta.bulk.service.download;

import static com.gdn.mta.bulk.SystemParameterConfigNames.VENDOR_BULK_DOWNLOAD_MAX_LIMIT;
import static com.gdn.mta.bulk.SystemParameterConfigNames.VENDOR_BULK_DOWNLOAD_NEW_SIZE;
import static com.gdn.partners.bulk.util.Constant.STORE_ID;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.VendorSummaryDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkVendorSummaryResponse;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;

public class BulkDownloadVendorSummaryServiceBeanTest {

  private static final String PRODUCT_NAME = "product-name";
  private static final String PRODUCT_CODE = "product-code";
  private static final String INITIATOR = "initiator";
  private static final String CONTENT_ASSIGNEE = "content-assignee";
  private static final String IMAGE_ASSIGNEE = "image-Assignee";
  private static final String ASSIGNEE = "assignee";
  private static final String CATEGORY = "category";
  private static final String USERNAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String LANGUAGE = "eng";
  private static final String FILENAME = "file-name";
  private static final String BUSINESS_PARTNER_NAME = "business-partner-name";
  private static final String ASSIGNED_EMAIL_ID = "assignedEmailId";
  private static final String KEYWORD = "keyword";
  private static final String SORT_ORDER = "asc";
  private static final String TIME_FILTER = "timeFilter";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "category_code";
  private static final int MAX_SIZE = 1000;

  @InjectMocks
  private BulkDownloadVendorSummaryServiceBean bulkDownloadVendorSummaryServiceBean;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Captor
  private ArgumentCaptor<FilterSummaryRequest> filterSummaryRequestArgumentCaptor;

  private BulkDownloadRequest bulkDownloadRequest;
  private GdnRestListResponse<DistributionProductResponse> productResponseGdnRestListResponse;
  private SystemParameterConfig newSize;
  private SystemParameterConfig maxLimit;


  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    DistributionProductResponse distributionProduct = new DistributionProductResponse();
    distributionProduct.setProductCode(PRODUCT_CODE);
    distributionProduct.setProductName(PRODUCT_NAME);
    distributionProduct.setCategoryName(CATEGORY);
    distributionProduct.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    distributionProduct.setCreatedBy(INITIATOR);
    distributionProduct.setProductApproved(Boolean.TRUE);
    distributionProduct.setProductApproverAssignee(ASSIGNEE);

    List<DistributionProductResponse> distributionProductResponseList = new ArrayList<>();
    distributionProductResponseList.add(distributionProduct);
    PageMetaData pageMetaData = new PageMetaData(0, MAX_SIZE, 10);

    productResponseGdnRestListResponse =
        new GdnRestListResponse<>(distributionProductResponseList, pageMetaData, null);

    VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder builder =
        new VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder();

    bulkDownloadRequest = builder
        .keyword(KEYWORD)
        .timeFilterType(TIME_FILTER)
        .unrestrictedDownload(false)
        .categoryCode(CATEGORY_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .assigneeEmailId(ASSIGNED_EMAIL_ID)
        .sortOrderByCreatedDate(SORT_ORDER)
        .downloadType(DownloadType.ALL).fileType(FileType.XLSX)
        .bulkProcessType(BulkProcessEntity.VENDOR_FILTERED_PRODUCT).directDownload(false).filename(FILENAME)
        .emailCC(USERNAME).emailTo(USERNAME).username(USERNAME).language(LANGUAGE).request(REQUEST_ID)
        .build();

  }

  @Test
  public void getDataTest() throws Exception{
    PageMetaData pageMetaData = new PageMetaData(0, MAX_SIZE, 10);
    Pageable pageable = PageRequest.of(0, MAX_SIZE);
    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "1000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");

    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT)).thenReturn(maxLimit);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(pageable),
            Mockito.any(FilterSummaryRequest.class)))
        .thenReturn(productResponseGdnRestListResponse);

    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository)
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(pageable),
            Mockito.any(FilterSummaryRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT);

    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @Test
  public void getDataTestLessThanMaxLimit() throws Exception{
    productResponseGdnRestListResponse.setPageMetaData(new PageMetaData(1000, 0, 40000));
    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "1000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");

    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT)).thenReturn(maxLimit);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class)))

        .thenReturn(productResponseGdnRestListResponse);
    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(40))
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT);

    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @Test
  public void getDataTestEqualToMaxLimit() throws Exception{
    productResponseGdnRestListResponse.setPageMetaData(new PageMetaData(1000, 0, 50000));

    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "1000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT)).thenReturn(maxLimit);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class)))
        .thenReturn(productResponseGdnRestListResponse);

    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(50))
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT);

    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @Test
  public void getDataTestGreaterThanMaxLimit() throws Exception{
    productResponseGdnRestListResponse.setPageMetaData(new PageMetaData(1000, 0, 60000));
    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "1000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");

    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT)).thenReturn(maxLimit);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class)))
        .thenReturn(productResponseGdnRestListResponse);

    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(50))
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT);

    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @Test
  public void getDataTesBatchSizeChange() throws Exception{
    productResponseGdnRestListResponse.setPageMetaData(new PageMetaData(1000, 0, 60000));
    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "3000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");

    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT)).thenReturn(maxLimit);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class)))
        .thenReturn(productResponseGdnRestListResponse);

    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(17))
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.any(Pageable.class),
            Mockito.any(FilterSummaryRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_MAX_LIMIT);

    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(this.systemParameterConfigService);
  }

  @Test
  public void getDataTest_unrestrictedDownload() throws Exception{
    Pageable pageable = PageRequest.of(0, MAX_SIZE);
    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "1000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");
    ((VendorSummaryDownloadRequest) bulkDownloadRequest).setUnrestrictedDownload(true);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(pageable),
            Mockito.any(FilterSummaryRequest.class)))
        .thenReturn(productResponseGdnRestListResponse);
    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository)
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(pageable),
            filterSummaryRequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
  }

  @Test
  public void getDataTestEditedDownload() throws Exception{
    Pageable pageable = PageRequest.of(0, MAX_SIZE);
    newSize = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_NEW_SIZE, "1000", "batch_size");
    maxLimit = new SystemParameterConfig(VENDOR_BULK_DOWNLOAD_MAX_LIMIT, "50000", "max_limit");
    ((VendorSummaryDownloadRequest) bulkDownloadRequest).setUnrestrictedDownload(true);
    ((VendorSummaryDownloadRequest) bulkDownloadRequest).setEdited(true);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE)).thenReturn(newSize);
    Mockito.when(this.productDistributionTaskRepository
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(pageable),
            Mockito.any(FilterSummaryRequest.class)))
        .thenReturn(productResponseGdnRestListResponse);

    BulkVendorSummaryResponse response = (BulkVendorSummaryResponse) this.bulkDownloadVendorSummaryServiceBean
        .getData(bulkDownloadRequest);

    Mockito.verify(this.productDistributionTaskRepository)
        .getVendorFilteredProducts(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(pageable),
            filterSummaryRequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    Assertions.assertEquals(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, response.getBulkProcessEntity());
    Assertions.assertNotNull(response.getResponseList());
    Assertions.assertEquals(PRODUCT_CODE, response.getResponseList().get(0).getProductCode());
    Assertions.assertTrue(filterSummaryRequestArgumentCaptor.getValue().getEdited());
  }

}
